unit config_session_fileutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , config_session
  , IniFiles
  ;

type

  { TConfigurationFile }

  TConfigurationFile = class(TIniFile)
  private
    FBlocCount : integer;
    function GetBlocCount : integer;
    function BlocSection(BlocIndex : integer) : string;
    function GetTrialCount(BlocIndex : integer): integer;
    function TrialSection(BlocIndex, TrialIndex : integer) : string;
    function GetBloc(BlocIndex : integer): TCfgBlc;
    function GetTrial(BlocIndex, TrialIndex : integer): TCfgTrial;
    procedure SetBloc(BlocIndex : integer; AValue: TCfgBlc);
    procedure SetTrial(BlocIndex, TrialIndex : integer; AValue: TCfgTrial);
    procedure CopySection(AFrom, ATo : string; AConfigurationFile : TConfigurationFile);
    procedure Invalidate;
  public
    constructor Create(const AConfigurationFile : string; AEscapeLineFeeds:Boolean=False); override;
    procedure AppendBloc(ABloc : integer; ATargetConfigurationFile : TConfigurationFile;
      AlsoAppendTrials : Boolean = True); overload;
    procedure AppendTrial(ABloc,ATrial: integer; ATargetConfigurationFile : TConfigurationFile); overload;
    procedure AppendBloc(ABloc: TCfgBlc; AlsoAppendTrials: Boolean); overload;
    procedure AppendTrial(ATrial : TCfgTrial); overload;
    property BlocCount : integer read GetBlocCount;
    property TrialCount[BlocIndex : integer] : integer read GetTrialCount;
    property Bloc[BlocIndex : integer] : TCfgBlc read GetBloc write SetBloc;
    property Trial[BlocIndex, TrialIndex : integer] : TCfgTrial read GetTrial write SetTrial;
  end;

implementation

uses constants, strutils;

{ TConfigurationFile }

function TConfigurationFile.GetBlocCount: integer;
begin
  if FBlocCount = 0 then
    while SectionExists(BlocSection(FBlocCount)) do
      Inc(FBlocCount);
  Result := FBlocCount;
end;

function TConfigurationFile.BlocSection(BlocIndex: integer): string;
begin
  Result := _Blc + #32 + IntToStr(BlocIndex+1);
end;

function TConfigurationFile.GetTrialCount(BlocIndex : integer): integer;
begin
  Result := 0;
  while SectionExists(TrialSection(BlocIndex,Result)) do
    Inc(Result);
end;

function TConfigurationFile.TrialSection(BlocIndex, TrialIndex: integer
  ): string;
begin
  Result := BlocSection(BlocIndex) + ' - ' + _Trial + IntToStr(TrialIndex+1);
end;

function TConfigurationFile.GetBloc(BlocIndex: integer): TCfgBlc;
var
  LBlcSection , s1: string;
begin
  LBlcSection := BlocSection(BlocIndex);
  with Result do
    begin
      s1 := ReadString(LBlcSection, _NumTrials, '0 0');
      NumTrials:=StrToIntDef(ExtractDelimited(1,s1,[#32]),0);
      VirtualTrialValue:= StrToIntDef(ExtractDelimited(2,s1,[#32]),0);

      Name:= ReadString(LBlcSection, _Name, '');
      BkGnd:= ReadInteger(LBlcSection, _BkGnd, 0);
      ITI:= ReadInteger(LBlcSection, _ITI, 0);

      CrtHitPorcentage := ReadInteger(LBlcSection, _CrtHitPorcentage, -1);
      CrtConsecutiveHit := ReadInteger(LBlcSection, _CrtConsecutiveHit, -1);
      CrtConsecutiveMiss := ReadInteger(LBlcSection, _CrtConsecutiveMiss, -1);
      CrtMaxTrials:= ReadInteger(LBlcSection, _CrtMaxTrials, -1);
      CrtKCsqHit := ReadInteger(LBlcSection, _CsqCriterion, -1);
      NextBlocOnCriteria := ReadInteger(LBlcSection, _NextBlocOnCriteria, -1)-1;
      NextBlocOnNotCriteria := ReadInteger(LBlcSection, _NextBlocOnNotCriteria, -1)-1;
      DefNextBlc:= ReadString(LBlcSection, _DefNextBlc, '');

      Counter:= ReadString(LBlcSection, _Counter, 'NONE');
      MaxCorrection:= ReadInteger(LBlcSection, _MaxCorrection, 0);
      MaxBlcRepetition := ReadInteger(LBlcSection, _MaxBlcRepetition, 0);
    end;
end;

function TConfigurationFile.GetTrial(BlocIndex, TrialIndex: integer): TCfgTrial;
var
  LTrialSection : string;
begin
  LTrialSection := TrialSection(BlocIndex,TrialIndex);
  with Result do
    begin
      Id :=  TrialIndex + 1;
      Name:= ReadString(LTrialSection,_Name, '');
      Kind:= ReadString(LTrialSection, _Kind, '');
      NumComp := ReadInteger(LTrialSection, _NumComp, 0);
      SList := TStringList.Create;
      with SList do
        begin
          //SList.Sorted := False;
          CaseSensitive := False;
          Duplicates := dupIgnore;
          BeginUpdate;
          ReadSectionValues(LTrialSection, SList);
          EndUpdate;
        end;
    end;
end;

procedure TConfigurationFile.SetBloc(BlocIndex: integer; AValue: TCfgBlc);
begin

end;

procedure TConfigurationFile.SetTrial(BlocIndex, TrialIndex: integer;
  AValue: TCfgTrial);
begin

end;

procedure TConfigurationFile.CopySection(AFrom, ATo: string;
  AConfigurationFile: TConfigurationFile);
var
  LSection : TStringList;
  LLine,
  LSelfSectionName,
  LTargetSectionName , LKeyName: string;
begin
  LSection := TStringList.Create;
  try
    LSelfSectionName := ATo;
    LTargetSectionName:= AFrom;
    AConfigurationFile.ReadSectionValues(LTargetSectionName,LSection);
    for LLine in LSection do
      begin
        LKeyName := LSection.ExtractName(LLine);
        WriteString(LSelfSectionName, LKeyName, LSection.Values[LKeyName]);
      end;
  finally
    LSection.Free;
  end;
end;

procedure TConfigurationFile.Invalidate;
begin
  FBlocCount := 0;
  WriteInteger(_Main,_NumBlc,BlocCount);
  WriteInteger(BlocSection(BlocCount-1),_NumTrials,TrialCount[BlocCount-1]);
end;

constructor TConfigurationFile.Create(const AConfigurationFile: string;
  AEscapeLineFeeds: Boolean);
begin
  inherited Create(AConfigurationFile,AEscapeLineFeeds);
  FBlocCount := 0;
  GetBlocCount;
end;

procedure TConfigurationFile.AppendBloc(ABloc: integer;
  ATargetConfigurationFile: TConfigurationFile; AlsoAppendTrials: Boolean);
var
  LSelfSectionName,
  LTargetSectionName : string;
begin
  LSelfSectionName := BlocSection(BlocCount);
  LTargetSectionName:= BlocSection(ABloc-1);
  CopySection(LTargetSectionName,LSelfSectionName, ATargetConfigurationFile);
end;

procedure TConfigurationFile.AppendTrial(ABloc, ATrial: integer;
  ATargetConfigurationFile: TConfigurationFile);
var
  LSelfSectionName,
  LTargetSectionName : string;
begin
  LSelfSectionName := TrialSection(BlocCount-1, TrialCount[BlocCount-1]);
  LTargetSectionName:= TrialSection(ABloc-1,ATrial-1);
  CopySection(LTargetSectionName,LSelfSectionName, ATargetConfigurationFile);
end;

procedure TConfigurationFile.AppendBloc(ABloc: TCfgBlc;
  AlsoAppendTrials: Boolean);
begin

end;

procedure TConfigurationFile.AppendTrial(ATrial: TCfgTrial);
begin

end;

end.


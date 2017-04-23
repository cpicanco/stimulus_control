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
    //procedure SetBloc(BlocIndex : integer; AValue: TCfgBlc);
    //procedure SetTrial(BlocIndex, TrialIndex : integer; AValue: TCfgTrial);
    procedure CopySection(AFrom, ATo : string; AConfigurationFile : TConfigurationFile);
    procedure WriteSection(ASectionName:string;ASection : TStrings);
  public
    constructor Create(const AConfigurationFile: string; AEscapeLineFeeds:Boolean=False); override;
    destructor Destroy; override;
    //procedure Invalidate;
    procedure WriteToBloc(ABloc : integer;AName, AValue: string);
    procedure WriteMain(AMain : TStrings);
    procedure WriteBlocFromTarget(ATargetBloc : integer; ATargetConfigurationFile : TConfigurationFile;
      AlsoAppendTrials : Boolean = True);
    procedure WriteTrialFromTarget(ATargetBloc,ATargetTrial: integer; ATargetConfigurationFile : TConfigurationFile);
    procedure WriteBlocIfEmpty(ABloc : integer; ABlocSenction : TStrings);
    //procedure WriteBloc(ABloc: TCfgBlc; AlsoAppendTrials: Boolean);
    //procedure WriteTrial(ATrial : TCfgTrial);
    property BlocCount : integer read GetBlocCount;
    property TrialCount[BlocIndex : integer] : integer read GetTrialCount;
    property Bloc[BlocIndex : integer] : TCfgBlc read GetBloc {write SetBloc};
    property Trial[BlocIndex, TrialIndex : integer] : TCfgTrial read GetTrial {write SetTrial};
  end;

implementation

uses constants, strutils;

{ TConfigurationFile }

function TConfigurationFile.GetBlocCount: integer;
begin
  FBlocCount := 0;
  while SectionExists(BlocSection(FBlocCount+1)) do
    Inc(FBlocCount);
  Result := FBlocCount;
end;

function TConfigurationFile.BlocSection(BlocIndex: integer): string;
begin
  Result := _Blc + #32 + IntToStr(BlocIndex);
end;

function TConfigurationFile.GetTrialCount(BlocIndex : integer): integer;
begin
  Result := 0;
  while SectionExists(TrialSection(BlocIndex,Result+1)) do
    Inc(Result);
end;

function TConfigurationFile.TrialSection(BlocIndex, TrialIndex: integer
  ): string;
begin
  Result := BlocSection(BlocIndex) + ' - ' + _Trial + IntToStr(TrialIndex);
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
      SList.CaseSensitive := False;
      SList.Duplicates := dupIgnore;
      ReadSectionValues(LTrialSection, SList);
    end;
end;

//procedure TConfigurationFile.SetBloc(BlocIndex: integer; AValue: TCfgBlc);
//begin
//
//end;
//
//procedure TConfigurationFile.SetTrial(BlocIndex, TrialIndex: integer;
//  AValue: TCfgTrial);
//begin
//
//end;

procedure TConfigurationFile.CopySection(AFrom, ATo: string;
  AConfigurationFile: TConfigurationFile);
var
  LSection : TStringList;
  LTargetSectionName,
  LSelfSectionName : string;
begin
  if AConfigurationFile.SectionExists(AFrom) then
    begin
      LSection := TStringList.Create;
      LSection.CaseSensitive := False;
      LSection.Duplicates := dupIgnore;
      try
        LTargetSectionName:= AFrom;
        LSelfSectionName := ATo;
        AConfigurationFile.ReadSectionValues(LTargetSectionName,LSection);
        WriteSection(LSelfSectionName,LSection);
      finally
        LSection.Free;
      end;
    end;
end;

procedure TConfigurationFile.WriteSection(ASectionName: string;
  ASection: TStrings);
var
  LLine, LKeyName: String;
begin
  for LLine in ASection do
    begin
      LKeyName := ASection.ExtractName(LLine);
      WriteString(ASectionName, LKeyName, ASection.Values[LKeyName]);
    end;
end;

//procedure TConfigurationFile.Invalidate;
//begin
//  WriteInteger(_Main,_NumBlc,BlocCount);
//  WriteInteger(BlocSection(BlocCount),_NumTrials,TrialCount[BlocCount]);
//end;

constructor TConfigurationFile.Create(const AConfigurationFile: string;
  AEscapeLineFeeds: Boolean);
begin
  inherited Create(AConfigurationFile,AEscapeLineFeeds);
  FBlocCount := 0;
  GetBlocCount;
end;

destructor TConfigurationFile.Destroy;
begin
  inherited Destroy;
end;

procedure TConfigurationFile.WriteToBloc(ABloc: integer; AName, AValue: string);
begin
  WriteString(BlocSection(ABloc),AName,AValue);
end;

procedure TConfigurationFile.WriteMain(AMain: TStrings);
begin
  WriteSection(_Main,AMain);
end;

procedure TConfigurationFile.WriteBlocFromTarget(ATargetBloc: integer;
  ATargetConfigurationFile: TConfigurationFile; AlsoAppendTrials: Boolean);
var
  LSelfSectionName,
  LTargetSectionName : string;
  i: integer;
begin
  LSelfSectionName := BlocSection(BlocCount+1);
  LTargetSectionName := BlocSection(ATargetBloc);
  CopySection(LTargetSectionName,LSelfSectionName, ATargetConfigurationFile);
  if AlsoAppendTrials then
    if ATargetConfigurationFile.TrialCount[ATargetBloc] > 0 then
      for i := 0 to ATargetConfigurationFile.TrialCount[ATargetBloc]-1 do
        WriteTrialFromTarget(ATargetBloc,i+1,ATargetConfigurationFile);
end;

procedure TConfigurationFile.WriteTrialFromTarget(ATargetBloc,
  ATargetTrial: integer; ATargetConfigurationFile: TConfigurationFile);
var
  LSelfSectionName,
  LTargetSectionName : string;
begin
  LSelfSectionName := TrialSection(BlocCount, TrialCount[BlocCount]+1);
  LTargetSectionName:= TrialSection(ATargetBloc,ATargetTrial);
  CopySection(LTargetSectionName,LSelfSectionName, ATargetConfigurationFile);
end;

procedure TConfigurationFile.WriteBlocIfEmpty(ABloc: integer;
  ABlocSenction: TStrings);
var
  LBlocSection,
  LLine, LKeyName: String;
  function EmptyKey : Boolean;
  var
    S : string;
  begin
    S := ReadString(LBlocSection, LKeyName, '');
    case Length(S) of
      0: Result := True;
      1: Result := not (S[1] in [#0..#32]);
      2..MaxInt : Result := False;
    end;
  end;

begin
  LBlocSection:=BlocSection(ABloc);
  for LLine in ABlocSenction do
    begin
      LKeyName := ABlocSenction.ExtractName(LLine);
      if ValueExists(LBlocSection, LKeyName) then
        begin
          if EmptyKey then
            WriteString(LBlocSection, LKeyName, ABlocSenction.Values[LKeyName])
          else; // do nothing
        end
      else
        WriteString(LBlocSection, LKeyName, ABlocSenction.Values[LKeyName]);
    end;
end;

//procedure TConfigurationFile.WriteBloc(ABloc: TCfgBlc;
//  AlsoAppendTrials: Boolean);
//begin
//
//end;
//
//procedure TConfigurationFile.WriteTrial(ATrial: TCfgTrial);
//begin
//
//end;

end.


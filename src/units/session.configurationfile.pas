{
  Stimulus Control
  Copyright (C) 2014-2020 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Session.ConfigurationFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , Session.Configuration
  , IniFiles
  ;

type

  { TConfigurationFile }

  TConfigurationFile = class(TIniFile)
  private
    FBlocCount : integer;
    class function TrialSection(BlocIndex, TrialIndex : integer) : string;
    class function BlocSection(BlocIndex : integer) : string;
    function CurrentBlocSection : string;
    function GetBlocCount : integer;
    function GetTrialCount(BlocIndex : integer): integer;
    function GetBloc(BlocIndex : integer): TCfgBlc;
    function GetTrial(BlocIndex, TrialIndex : integer): TCfgTrial;
    //procedure SetBloc(BlocIndex : integer; AValue: TCfgBlc);
    //procedure SetTrial(BlocIndex, TrialIndex : integer; AValue: TCfgTrial);
    procedure CopySection(AFrom, ATo : string; AConfigurationFile : TConfigurationFile);
    procedure WriteSection(ASectionName:string; ASection : TStrings);

  public
    constructor Create(const AConfigurationFile: string; AEscapeLineFeeds:Boolean=False); override;
    destructor Destroy; override;
    class function FullTrialSection(ABloc, ATrial : integer) : string;
    function ReadTrialString(ABloc : integer; ATrial : integer; AName:string):string;
    function ReadTrialInteger(ABloc : integer; ATrial : integer; AName:string):LongInt;
    function CurrentBloc: TCfgBlc;
    function CurrentTrial: TCfgTrial;
    function BeginTableName : string;
    function EndTableName : string;
    procedure Invalidate;
    procedure ReadPositionsInBloc(ABloc:integer; APositionsList : TStrings);
    procedure WriteToBloc(ABloc : integer;AName, AValue: string);
    procedure WriteToTrial(ATrial : integer; AStrings : TStrings); overload;
    procedure WriteToTrial(ATrial : integer; AName, AValue: string); overload;
    procedure WriteToTrial(ATrial : integer; ABloc : integer; AName, AValue: string); overload;
    procedure WriteMain(AMain : TStrings);
    procedure WriteBlocFromTarget(ATargetBloc : integer; ATargetConfigurationFile : TConfigurationFile;
      AlsoAppendTrials : Boolean = True);
    procedure WriteTrialFromTarget(ATargetBloc,ATargetTrial: integer; ATargetConfigurationFile : TConfigurationFile);
    procedure WriteBlocIfEmpty(ABloc : integer; ABlocSection : TStrings);
    //procedure WriteBloc(ABloc: TCfgBlc; AlsoAppendTrials: Boolean);
    //procedure WriteTrial(ATrial : TCfgTrial);
    property BlocCount : integer read GetBlocCount;
    property TrialCount[BlocIndex : integer] : integer read GetTrialCount;
    property Bloc[BlocIndex : integer] : TCfgBlc read GetBloc {write SetBloc};
    property Trial[BlocIndex, TrialIndex : integer] : TCfgTrial read GetTrial {write SetTrial};
  end;

var
  ConfigurationFile : TConfigurationFile;

implementation

uses Constants, StrUtils, Session.Configuration.GlobalContainer;

{ TConfigurationFile }

function TConfigurationFile.GetBlocCount: integer;
begin
  FBlocCount := 0;
  while SectionExists(BlocSection(FBlocCount+1)) do
    Inc(FBlocCount);
  Result := FBlocCount;
end;

class function TConfigurationFile.BlocSection(BlocIndex: integer): string;
begin
  Result := _Blc + #32 + IntToStr(BlocIndex);
end;

function TConfigurationFile.GetTrialCount(BlocIndex : integer): integer;
begin
  Result := 0;
  while SectionExists(TrialSection(BlocIndex,Result+1)) do
    Inc(Result);
end;

class function TConfigurationFile.TrialSection(BlocIndex,
  TrialIndex: integer): string;
begin
  Result := BlocSection(BlocIndex) + ' - ' + _Trial + IntToStr(TrialIndex);
end;

function TConfigurationFile.CurrentBloc: TCfgBlc;
begin
  Result := Bloc[Counters.CurrentBlc+1];
end;

function TConfigurationFile.CurrentTrial: TCfgTrial;
begin
  Result := Trial[
    Counters.CurrentBlc+1,
    Counters.CurrentTrial+1];
end;

function TConfigurationFile.BeginTableName: string;
begin
  Result := ReadString(CurrentBlocSection, 'BeginTable', '');
end;

function TConfigurationFile.EndTableName: string;
begin
  Result := ReadString(CurrentBlocSection, 'EndTable', '');
end;

function TConfigurationFile.CurrentBlocSection: string;
begin
  Result := BlocSection(Counters.CurrentBlc+1);
end;

function TConfigurationFile.GetBloc(BlocIndex: integer): TCfgBlc;
var
  LBlcSection , s1: string;
begin
  LBlcSection := BlocSection(BlocIndex);
  with Result do
    begin
      ID := BlocIndex;
      s1 := ReadString(LBlcSection, _NumTrials, '0 0');
      TotalTrials:=StrToIntDef(ExtractDelimited(1,s1,[#32]),0);
      VirtualTrialValue:= StrToIntDef(ExtractDelimited(2,s1,[#32]),0);

      Name:= ReadString(LBlcSection, _Name, '');
      BkGnd:= ReadInteger(LBlcSection, _BkGnd, 0);
      ITI:= ReadInteger(LBlcSection, _ITI, 0);

      CrtHitPorcentage := ReadInteger(LBlcSection, _CrtHitPorcentage, -1);
      CrtConsecutiveHit := ReadInteger(LBlcSection, _CrtConsecutiveHit, -1);
      CrtConsecutiveMiss := ReadInteger(LBlcSection, _CrtConsecutiveMiss, -1);
      CrtConsecutiveHitPerType := ReadInteger(LBlcSection, _CrtConsecutiveHitPerType, -1);
      CrtHitValue := ReadInteger(LBlcSection, _CrtHitValue, -1);
      CrtMaxTrials:= ReadInteger(LBlcSection, _CrtMaxTrials, -1);
      CrtKCsqHit := ReadInteger(LBlcSection, _CsqCriterion, -1);
      NextBlocOnCriteria := ReadInteger(LBlcSection, _NextBlocOnCriteria, -1);
      NextBlocOnNotCriteria := ReadInteger(LBlcSection, _NextBlocOnNotCriteria, -1);
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
      Parameters := TStringList.Create;
      Parameters.CaseSensitive := False;
      Parameters.Duplicates := dupIgnore;
      ReadSectionValues(LTrialSection, Parameters);
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

procedure TConfigurationFile.Invalidate;
var
  i: Integer;
begin
  WriteInteger(_Main,_NumBlc,BlocCount);
  for i := 0 to BlocCount-1 do
    WriteString(BlocSection(i+1),_NumTrials,TrialCount[i+1].ToString+' 1');
end;

procedure TConfigurationFile.ReadPositionsInBloc(ABloc: integer;
  APositionsList: TStrings);
var
  L : TStringList;
  LNumComp: LongInt;
  LTrialSection, LKeyName, S: String;
  j, i: Integer;
begin
  L := TStringList.Create;
  L.Sorted := True;
  L.Duplicates:=dupIgnore;
  try
    for i := 0 to TrialCount[ABloc]-1 do
      begin
        LTrialSection := TrialSection(ABloc,i+1);

        // sample
        if ReadString(LTrialSection,_Kind,'') = T_MTS then
          begin
            LKeyName := _Samp+_cBnd;
            S := ReadString(LTrialSection,LKeyName,'');
            if S <> '' then
              L.Append(S);
          end;

        // comparisons
        LNumComp := ReadInteger(LTrialSection,_NumComp,0);
        if LNumComp > 0 then
          for j := 0 to  LNumComp-1 do
            begin
              LKeyName := _Comp+IntToStr(j+1)+_cBnd;
              S := ReadString(LTrialSection,LKeyName,'');
              if S <> '' then
                L.Append(S);
            end;
      end;

    j := 0;
    for i := L.Count-1 downto 0 do
      begin
        APositionsList.Values[IntToStr(j+1)] := L[i];
        Inc(j);
      end;

  finally
    L.Free;
  end;
end;

function TConfigurationFile.ReadTrialString(ABloc: integer; ATrial: integer;
  AName: string): string;
begin
  Result := ReadString(TrialSection(ABloc, ATrial), AName, '');
end;

function TConfigurationFile.ReadTrialInteger(ABloc: integer; ATrial: integer;
  AName: string): LongInt;
begin
  Result := ReadInteger(TrialSection(ABloc, ATrial), AName, 0);
end;

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

class function TConfigurationFile.FullTrialSection(ABloc,
  ATrial: integer): string;
begin
  Result := '[' + TrialSection(ABloc, ATrial) + ']';
end;

procedure TConfigurationFile.WriteToBloc(ABloc: integer; AName, AValue: string);
begin
  WriteString(BlocSection(ABloc),AName,AValue);
end;

procedure TConfigurationFile.WriteToTrial(ATrial: integer; AStrings: TStrings);
begin
  WriteSection(TrialSection(BlocCount,ATrial),AStrings);
end;

procedure TConfigurationFile.WriteToTrial(ATrial: integer; AName, AValue: string
  );
begin
  WriteString(TrialSection(BlocCount,ATrial),AName,AValue);
end;

procedure TConfigurationFile.WriteToTrial(ATrial: integer; ABloc: integer;
  AName, AValue: string);
begin
  WriteString(TrialSection(ABloc,ATrial),AName,AValue);
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
  ABlocSection: TStrings);
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
  for LLine in ABlocSection do
    begin
      LKeyName := ABlocSection.ExtractName(LLine);
      if ValueExists(LBlocSection, LKeyName) then
        begin
          if EmptyKey then
            WriteString(LBlocSection, LKeyName, ABlocSection.Values[LKeyName])
          else; // do nothing
        end
      else
        WriteString(LBlocSection, LKeyName, ABlocSection.Values[LKeyName]);
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


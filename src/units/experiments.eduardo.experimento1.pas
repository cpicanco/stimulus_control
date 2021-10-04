unit Experiments.Eduardo.Experimento1;

{$mode objfpc}{$H+}

interface

procedure WriteToConfigurationFile(ADesign : string);

const
  FolderChoices =
    'tentativas'+DirectorySeparator;

implementation

uses Classes, SysUtils
   , Constants
   , LazFileUtils
   , Experiments.Eduardo.Comum
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   ;

procedure WriteOperantTask(ABlc : integer; AName: string; AHasDelay : Boolean);
var
  i : integer;
begin
  i := ConfigurationFile.TrialCount[ABlc]+1;
  with ConfigurationFile do
  begin
    WriteToTrial(i, ABlc, _Name, AName);
    WriteToTrial(i, ABlc, _Cursor, '0');
    WriteToTrial(i, ABlc, _Kind, T_EO1);
    WriteToTrial(i, ABlc, _Schedule, 'FR 2');
    WriteToTrial(i, ABlc, _ITI, ITI.ToString);
    if AHasDelay then
      WriteToTrial(i, ABlc, 'Type', 'E1C')
    else
      WriteToTrial(i, ABlc, 'Type', 'E1B');
  end;
end;

procedure WriteB(ABlc : integer);
var
  i : integer;
begin
  for i := 0 to 59 do
    WriteOperantTask(ABlc, 'Operant '+(i+1).ToString, False);
end;

procedure WriteC(ABlc : integer);
var
  i : integer;
begin
  for i := 0 to 59 do
    WriteOperantTask(ABlc, 'Operant '+(i+1).ToString, True);
end;

procedure WriteToConfigurationFile(ADesign : string);
var
  LCondition : char;
  LConditionI : integer = 0;
  LTable : integer = 0;
  procedure WriteBCondition;
  begin
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem B/C');
    ConfigurationFile.WriteToBloc(
      LConditionI, 'EndTable', 'DemandTable'+LTable.ToString);
    WriteMSG(LConditionI, 'M1', MessageE1B);


    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'B');
    LTable := LConditionI;
    ConfigurationFile.WriteToBloc(
      LConditionI, 'BeginTable', 'Experiment1Table'+LTable.ToString);

    WriteB(LConditionI);
  end;

  procedure WriteCCondition;
  begin
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem C/B');
    ConfigurationFile.WriteToBloc(
      LConditionI, 'EndTable', 'DemandTable'+LTable.ToString);
    WriteMSG(LConditionI, 'M1', MessageE1B);

    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'C');
    LTable := LConditionI;
    ConfigurationFile.WriteToBloc(
      LConditionI, 'BeginTable', 'Experiment1Table'+LTable.ToString);

    WriteC(LConditionI);
  end;

begin
  ITI := ITISurvey;
  SetupStimuli;
  if (ADesign = 'ABA') or (ADesign = 'ACA') then
  begin
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem A0');
    WriteMSG(LConditionI, 'M0', MessageA0);
  end;

  for LCondition in ADesign do
  case LCondition of
    'A': WriteACondition(LConditionI, LTable, '1');
    'B': WriteBCondition;
    'C': WriteCCondition;
  end;
end;

end.

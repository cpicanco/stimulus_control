unit Experiments.Vinicius.Experimento2;

{$mode objfpc}{$H+}

interface

procedure WriteToConfigurationFile(ADesign : string);

implementation

uses Classes, SysUtils
   , Constants
   , LazFileUtils
   , Experiments.Vinicius.Comum
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   ;

procedure WriteOperantTask(ABlc : integer; AName: string; AHasDelay : Boolean);
var
  i : integer;
  IsTestTrial: Boolean;
begin
  i := ConfigurationFile.TrialCount[ABlc]+1;
  with ConfigurationFile do
  begin
    WriteToTrial(i, ABlc, _Name, AName);
    WriteToTrial(i, ABlc, _Cursor, '0');
    WriteToTrial(i, ABlc, _Kind, T_EO1);
    WriteToTrial(i, ABlc, _Schedule, 'FR 2');
    WriteToTrial(i, ABlc, _ITI, ITIBase.ToString);
    if AHasDelay then
      WriteToTrial(i, ABlc, 'Type', 'E1C')
    else
      WriteToTrial(i, ABlc, 'Type', 'E1B');
    IsTestTrial := False;
    WriteToTrial(i, ABlc, 'IsTestTrial', IsTestTrial.ToString);
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
    //WriteMSG(LConditionI, 'M1', MessageE1B);


    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'B');
    LTable := LConditionI;

    WriteB(LConditionI);
  end;

  procedure WriteCCondition;
  begin
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem C/B');
    //WriteMSG(LConditionI, 'M1', MessageE1B);

    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'C');
    LTable := LConditionI;

    WriteC(LConditionI);
  end;

begin
  //ITI := ITISurvey;
  //SetupStimuli;
  //if (ADesign = 'ABA') or (ADesign = 'ACA') or (ADesign = 'A') then
  //begin
  //  Inc(LConditionI);
  //  ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem A0');
  //  //WriteMSG(LConditionI, 'M0', MessageA0);
  //end;
  //
  //for LCondition in ADesign do
  //case LCondition of
  //  'A': WriteACondition(LConditionI, LTable, '1', ADesign);
  //  'B': WriteBCondition;
  //  'C': WriteCCondition;
  //end;
end;

end.

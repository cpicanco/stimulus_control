unit Experiments.Eduardo.Experimento1;

{$mode objfpc}{$H+}

interface

procedure WriteToConfigurationFile(ADesign : string);

//procedure WriteChoices;

const
  FolderChoices =
    'tentativas'+DirectorySeparator;

implementation

uses Classes, SysUtils
   , Constants
   , LazFileUtils
   , FileMethods
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
    WriteToTrial(i, ABlc, _ITI, ITI.ToString);
    if AHasDelay then
      WriteToTrial(i, ABlc, 'Type', 'C')
    else
      WriteToTrial(i, ABlc, 'Type', 'B');
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

  procedure WriteBCondition;
  begin
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem B/C');
    WriteMSG(LConditionI, 'M1', MessageE1B);


    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'B');
    WriteB(LConditionI);
  end;

  procedure WriteCCondition;
  begin
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem C/B');
    WriteMSG(LConditionI, 'M1', MessageE1B);

    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'C');
    WriteC(LConditionI);
  end;

begin
  ITI := 500;
  SetupStimuli;
  if (ADesign = 'ABA') or (ADesign = 'ACA') then
  begin
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem A0');
    WriteMSG(LConditionI, 'M0', MessageA0);
  end;

  for LCondition in ADesign do
  case LCondition of
    'A': WriteACondition(LConditionI);
    'B': WriteBCondition;
    'C': WriteCCondition;
  end;
end;

//procedure WriteChoices;
//var
//  i : integer;
//  LStringList : TStringList;
//  LNow : real;
//  LLater : real = 100.0;
//  LDelay : string;
//  LNextTrial : string;
//begin
//  LStringList := TStringList.Create;
//  try
//    for LDelay in Delays do
//      begin
//        LNow := 50.0;
//        for i := 0 to 9 do
//          begin
//            case i of
//            0 : LNextTrial := '10';
//            else
//              LNextTrial := '-1';
//            end;
//
//            LStringList.Append(
//              'Ganhar R$' + FloatToStrF(LNow,ffFixed,0,2)   + ' reais agora' + #9 +     //left
//              'Ganhar R$' + FloatToStrF(LLater,ffFixed,0,2) + ' reais daqui ' + LDelay + #9 + //right
//              LNextTrial);
//            LNow := LNow * 0.75;
//          end;
//
//        LNow := 50.0;
//        for i := 0 to 8 do
//          begin
//            case i of
//            0 : LNextTrial := '-10';
//            8 : LNextTrial := '';
//            else
//              LNextTrial := '-1';
//            end;
//            LNow := LNow * 1.25;
//            LStringList.Append(
//              'Ganhar R$' + FloatToStrF(LNow,ffFixed,0,2)   + ' reais agora' + #9 +
//              'Ganhar R$' + FloatToStrF(LLater,ffFixed,0,2) + ' reais daqui ' + LDelay + #9 +
//              LNextTrial);
//
//          end;
//      end;
//    LStringList.SaveToFile(GlobalContainer.RootMedia+FolderChoices+'desconto.txt');
//  finally
//    LStringList.Free;
//  end;
//end;

end.

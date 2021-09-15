unit Experiments.Fabiane.Experiment1;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Session.ConfigurationFile;

procedure WriteToConfigurationFile(ADesign : string);

implementation

uses
  FileMethods, Constants;

type
  TCondition = (A1, A2, B1, B2, B3, C1);

const
  A1Name = 'Linha de Base (Aquisição)';
  A2Name = 'Linha de Base (Fortalecimento)';
  B1Name = 'Grupo 1';
  B2Name = 'Grupo 2';
  B3Name = 'Grupo 3';
  C1Name = 'Reversão';

var
  Condition : TCondition;
  BlocIndex : integer;
  //ITI : integer = 1000;

procedure SetupStimuli;
begin
  //LoadMessageFromFile(MessageA0, GlobalContainer.RootMedia+FolderMessages+'1-MensagemA0.html');
end;

procedure WriteCondition;
var
  LCondition : string;
begin
  WriteStr(LCondition, Condition);
  with ConfigurationFile do
  begin
    WriteToTrial(1, BlocIndex, _Name, LCondition);
    WriteToTrial(1, BlocIndex, _Cursor, '0');
    WriteToTrial(1, BlocIndex, _Kind, T_EO2);
    WriteToTrial(1, BlocIndex, 'Type', LCondition);
  end;
end;

procedure WriteConditionA1;
begin
  WriteCondition;
end;

procedure WriteConditionA2;
begin
  WriteCondition;
end;

procedure WriteConditionB1;
begin
  WriteCondition;
end;

procedure WriteConditionB2;
begin
  WriteCondition;
end;

procedure WriteConditionB3;
begin
  WriteCondition;
end;

procedure WriteConditionC1;
begin
  WriteCondition;
end;

procedure WriteToConfigurationFile(ADesign : string);
  procedure WriteCondition;
  begin
    Inc(BlocIndex);
    ConfigurationFile.WriteToBloc(BlocIndex, _Name, ADesign);
    case Condition of
      A1 : WriteConditionA1;
      A2 : WriteConditionA2;
      B1 : WriteConditionB1;
      B2 : WriteConditionB2;
      B3 : WriteConditionB3;
      C1 : WriteConditionC1;
    end;
  end;

begin
  case ADesign of
    A1Name : Condition := A1;
    A2Name : Condition := A2;
    B1Name : Condition := B1;
    B2Name : Condition := B2;
    B3Name : Condition := B3;
    C1Name : Condition := C1;
    else
      raise Exception.Create('Unknown condition');
  end;
  WriteCondition;
end;

end.


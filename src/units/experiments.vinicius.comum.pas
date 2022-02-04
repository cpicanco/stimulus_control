unit Experiments.Vinicius.Comum;

{$mode ObjFPC}{$H+}

interface

procedure SetupStimuli;
procedure WriteMSG(ABlc : integer; AName: string; AMessage : string);

var
  ReinforcDuration : Cardinal = 3000;
  BlackoutDuration : Cardinal = 3000;
  MessageE1A1 : string;
  MessageE1A2 : string;
  MessageE1B1 : string;
  MessageE1C1 : string;
  MessageE1C2 : string;
  MessageE1D1 : string;
  MessageE1Final1 : string;
  MessageE1Final2 : string;
  AuditiveVisual : array [0..7] of integer = (1, 2, 5, 6, 9, 10, 13, 14);
  VisualAuditive : array [0..7] of integer = (3, 4, 7, 8, 11, 12, 15, 16);

const
  FolderMessages =
    'mensagens'+DirectorySeparator;


implementation

uses Classes, SysUtils
   , Constants
   , LazFileUtils
   , FileMethods
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   ;

procedure WriteMSG(ABlc : integer; AName: string; AMessage : string);
var
  i : integer;
begin
  //ITI := ITISurvey;
  i := ConfigurationFile.TrialCount[ABlc]+1;
  with ConfigurationFile do
  begin
    WriteToTrial(i, ABlc, _Name, AName);
    WriteToTrial(i, ABlc, _Cursor, '0');
    WriteToTrial(i, ABlc, _Kind, T_HTM);
    WriteToTrial(i, ABlc, _ITI, ITIBase.ToString);
    WriteToTrial(i, ABlc, _Msg, AMessage)
  end;
end;

procedure SetupStimuli;
var
  LRootPath : string;
begin
  LRootPath := GlobalContainer.RootMedia+FolderMessages;
  LoadMessageFromFile(MessageE1A1,
    LRootPath+'1-Mensagem_A_Auditiva.html');
  LoadMessageFromFile(MessageE1A2,
    LRootPath+'1-Mensagem_A_Visual.html');
  LoadMessageFromFile(MessageE1B1,
    LRootPath+'1-Mensagem_B.html');
  LoadMessageFromFile(MessageE1C1,
    LRootPath+'1-Mensagem_C_Auditiva.html');
  LoadMessageFromFile(MessageE1C2,
    LRootPath+'1-Mensagem_C_Visual.html');
  LoadMessageFromFile(MessageE1D1,
    LRootPath+'1-Mensagem_D.html');
  LoadMessageFromFile(MessageE1Final1,
    LRootPath+'1-Mensagem_Final_Primeira_Condicao.html');
  LoadMessageFromFile(MessageE1Final2,
    LRootPath+'1-Mensagem_Final_Segunda_Condicao.html');
end;

end.


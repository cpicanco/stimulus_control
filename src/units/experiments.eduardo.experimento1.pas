unit Experiments.Eduardo.Experimento1;

{$mode objfpc}{$H+}

interface

procedure WriteToConfigurationFile;

const
  FolderPreTrainingBefoAfter =
    'mensagens'+DirectorySeparator;

implementation

uses Classes, SysUtils
   , Constants
   , LazFileUtils
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   ;

const
  ITI = 3000;

procedure SetupStimuli;
begin
  // LoadMessageFromFile(MessageTraining, GlobalContainer.RootMedia+'Mensagem1.txt');
end;


procedure WriteMSG(ABlc : integer; AName: string; AMessage : string);
var
  i : integer;
begin
  i := ConfigurationFile.TrialCount[1]+1;
  with ConfigurationFile do
  begin
    WriteToTrial(i, ABlc, _Name, AName);
    WriteToTrial(i, ABlc, _Cursor, '0');
    WriteToTrial(i, ABlc, _Kind, T_MSG);
    WriteToTrial(i, ABlc, _ITI, ITI.ToString);
    WriteToTrial(i, ABlc, _Msg, AMessage)
  end;
end;


procedure WriteBloc(ABlc : integer);
var
  LMessage1 : string =
   '<p>Este questionário apresenta uma situação com várias escolhas hipotéticas entre diferentes quantias de dinheiro a serem recebidas em momentos distintos. Faça suas escolhas como se fossem reais, mesmo sabendo que não receberá o que for escolhido. Não existem respostas certas ou erradas, apenas sua decisão pessoal.</p>'+
   '<p>Duas alternativas de escolha serão apresentadas lado a lado. Na situação do <b>lado esquerdo</b>, você receberia uma determinada quantia <b>imediatamente</b>; na situação do <b>lado direito</b>, você teria que <b>esperar algum tempo</b> para receber uma outra quantia. Assinale a alternativa escolhida no espaço indicado.</p>';

begin
  WriteMSG(ABlc, 'M1', LMessage1);
  WriteMSG(ABlc, 'M2', LMessage1);
end;

procedure WriteToConfigurationFile;
begin
  // training
  SetupStimuli;
  ConfigurationFile.WriteToBloc(1, _Name, 'Mensagem de treino antes-depois');
  WriteBloc(1);
end;

end.

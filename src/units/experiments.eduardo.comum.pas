{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Experiments.Eduardo.Comum;

{$mode objfpc}{$H+}

interface

procedure SetupStimuli;
procedure WriteMSG(ABlc : integer; AName: string; AMessage : string);
procedure WriteTXTInput(ABlc : integer; AName: string;
  AMessage : string; APrice : string);
procedure WriteACondition(var ACondition : integer;
  var ATable : integer; Experiment:string);

const
  FolderMessages =
    'mensagens'+DirectorySeparator;

var
  ITI : integer = 1000;
  MessageA0 : string;
  MessageE1A1 : string;
  MessageE1A2 : string;
  MessageE1B : string;
  MessageE2B : string;
  MessageE3B : string;

implementation

uses Classes, SysUtils
   , Constants
   , LazFileUtils
   , FileMethods
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   ;

var
  Delays : array [0..4] of string = ('1 mês', '3 meses', '6 meses', '1 ano', '2 anos');
  DelaysIndex : integer = 0;

procedure SetupStimuli;
begin
  LoadMessageFromFile(MessageA0, GlobalContainer.RootMedia+FolderMessages+'1-MensagemA0.html');
  LoadMessageFromFile(MessageE1A1, GlobalContainer.RootMedia+FolderMessages+'1-MensagemA1.html');
  LoadMessageFromFile(MessageE1A2, GlobalContainer.RootMedia+FolderMessages+'1-MensagemA2.html');
  LoadMessageFromFile(MessageE1B,  GlobalContainer.RootMedia+FolderMessages+'1-MensagemB.html');
  LoadMessageFromFile(MessageE2B,  GlobalContainer.RootMedia+FolderMessages+'2-MensagemB.html');
  LoadMessageFromFile(MessageE3B,  GlobalContainer.RootMedia+FolderMessages+'3-MensagemB.html');
end;

procedure WriteTXTInput(ABlc : integer; AName: string;
  AMessage: string; APrice : string);
var
  i : integer;
begin
  i := ConfigurationFile.TrialCount[ABlc]+1;
  with ConfigurationFile do
  begin
    WriteToTrial(i, ABlc, _Name, AName);
    WriteToTrial(i, ABlc, _Cursor, '0');
    WriteToTrial(i, ABlc, _Kind, T_INP);
    WriteToTrial(i, ABlc, _ITI, ITI.ToString);
    WriteToTrial(i, ABlc, _Msg, AMessage);
    WriteToTrial(i, ABlc, 'Price', APrice);
  end;
end;

procedure WriteMSG(ABlc : integer; AName: string; AMessage : string);
var
  i : integer;
begin
  ITI := ITISurvey;
  i := ConfigurationFile.TrialCount[ABlc]+1;
  with ConfigurationFile do
  begin
    WriteToTrial(i, ABlc, _Name, AName);
    WriteToTrial(i, ABlc, _Cursor, '0');
    WriteToTrial(i, ABlc, _Kind, T_HTM);
    WriteToTrial(i, ABlc, _ITI, ITI.ToString);
    WriteToTrial(i, ABlc, _Msg, AMessage)
  end;
end;

procedure WriteChoice(ABlc : integer; AName : string; ALeft : string;
  ARight : string; ADelay : string; ALNextTrial : string; ARNextTrial : string);
var
  i : integer;
begin
  i := ConfigurationFile.TrialCount[ABlc]+1;
  with ConfigurationFile do
  begin
    WriteToTrial(i, ABlc, _Name, AName);
    WriteToTrial(i, ABlc, _Cursor, '0');
    WriteToTrial(i, ABlc, _Kind, T_CHO);
    WriteToTrial(i, ABlc, _ITI, ITI.ToString);
    WriteToTrial(i, ABlc, 'Delay', ADelay);
    WriteToTrial(i, ABlc, 'L', ALeft);
    WriteToTrial(i, ABlc, 'L'+_NextTrial, ALNextTrial);
    WriteToTrial(i, ABlc, 'R', ARight);
    WriteToTrial(i, ABlc, 'R'+_NextTrial, ARNextTrial);
  end;
end;

procedure WriteACondition(var ACondition: integer; var ATable: integer;
  Experiment: string);
var
  i : integer;
  procedure WriteDiscountBloc(ABlc : integer);
  var
    LDelay : string;
  begin
    if DelaysIndex > High(Delays) then DelaysIndex := 0;
    LDelay := Delays[DelaysIndex];

    WriteChoice(ABlc,'50.0'+#32+LDelay,
      '50.0',
      '100.0',
      LDelay,
      '0','0');

    Inc(DelaysIndex);
  end;


  procedure WriteDemandBloc(ABlc : integer);
  var
    i : integer;
  const
    LPrices : array [0..20] of string =
      ('0.50','1.00','1.50','2.00',
      '3.00','4.00','5.00','6.00','7.00',
      '8.00','9.00','10.00','12.00',
      '15.00','20.00','25.00','30.00',
      '50.00','100.00','200.00','400.00');
  begin
    WriteTXTInput(ABlc, 'Demanda 0',
      'Quanto você consumiria se o cigarro fosse gratuito?', '0');
    for i := Low(LPrices) to High(LPrices) do
      WriteTXTInput(ABlc, 'Demanda '+LPrices[i],
      'Quantos cigarros você consumiria se cada um custasse R$'+LPrices[i]+'?',
      LPrices[i]);
  end;
begin
  ITI := ITISurvey;
  Inc(ACondition);
  ConfigurationFile.WriteToBloc(ACondition, _Name, 'Mensagem A1');
  ConfigurationFile.WriteToBloc(
    ACondition,
    'EndTable',
    'Experiment'+Experiment+'Table'+ATable.ToString);
  WriteMSG(ACondition, 'M1', MessageE1A1);

  for i := Low(Delays) to High(Delays)do
  begin
    Inc(ACondition);
    ConfigurationFile.WriteToBloc(ACondition, _Name, Delays[i]);
    ConfigurationFile.WriteToBloc(ACondition, _CrtMaxTrials, '10');
    if i = Low(Delays) then begin
      ATable := ACondition;
      ConfigurationFile.WriteToBloc(
        ACondition, 'BeginTable', 'DiscountTable'+ATable.ToString);
    end;
    WriteDiscountBloc(ACondition);
  end;

  Inc(ACondition);
  ConfigurationFile.WriteToBloc(ACondition, _Name, 'Mensagem A2');
  ConfigurationFile.WriteToBloc(
    ACondition, 'EndTable', 'DiscountTable'+ATable.ToString);
  WriteMSG(ACondition, 'M1', MessageE1A2);

  Inc(ACondition);
  ATable := ACondition;
  ConfigurationFile.WriteToBloc(ACondition, _Name, 'Demanda');
  ConfigurationFile.WriteToBloc(
    ACondition, 'BeginTable', 'DemandTable'+ATable.ToString);
  WriteDemandBloc(ACondition);
end;





end.


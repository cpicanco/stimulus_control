unit Experiments.Eduardo.Experimento1;

{$mode objfpc}{$H+}

interface

procedure WriteToConfigurationFile;

procedure WriteChoices;

const
  FolderMessages =
    'mensagens'+DirectorySeparator;
  FolderChoices =
    'tentativas'+DirectorySeparator;

implementation

uses Classes, SysUtils
   , Constants
   , LazFileUtils
   , FileMethods
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   ;

var
  Message1 : string;
  Delays : array [2..6] of string = ('1 mês', '3 meses', '6 meses', '1 ano', '2 anos');

const
  ITI = 500;

procedure SetupStimuli;
begin
  LoadMessageFromFile(Message1, GlobalContainer.RootMedia+FolderMessages+'Mensagem1.html');
end;


procedure WriteMSG(ABlc : integer; AName: string; AMessage : string);
var
  i : integer;
begin
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

procedure WriteChoice(ABlc : integer; AName: string;
  ALeft: string; ARight: string; ALNextTrial: string; ARNextTrial: string);
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
    WriteToTrial(i, ABlc, 'L', ALeft);
    WriteToTrial(i, ABlc, 'L'+_NextTrial, ALNextTrial);
    WriteToTrial(i, ABlc, 'R', ARight);
    WriteToTrial(i, ABlc, 'R'+_NextTrial, ARNextTrial);
  end;
end;


procedure WriteBloc(ABlc : integer);
var
  i : integer;
  LNow : real;
  LLater : real = 100.0;
  LDelay : string;
  LNextTrial : string;
begin
  LDelay := Delays[ABlc];
  LNow := 50.0;
  for i := 0 to 9 do
    begin
      case i of
      0 : LNextTrial := '10';
      else
        LNextTrial := '-1';
      end;

      WriteChoice(ABlc,FloatToStrF(LNow,ffFixed,0,2)+#32+LDelay,
        'Ganhar '+#13+'R$' + FloatToStrF(LNow,ffFixed,0,2)   + ' reais' + #13 + 'agora',
        'Ganhar '+#13+'R$' + FloatToStrF(LLater,ffFixed,0,2) + ' reais' + #13 + 'daqui ' + LDelay,
        '',LNextTrial);
      LNow := LNow * 0.75;
    end;

  LNow := 50.0;
  for i := 0 to 8 do
    begin
      case i of
      0 : LNextTrial := '-10';
      8 : LNextTrial := '';
      else
        LNextTrial := '-1';
      end;
      LNow := LNow * 1.25;
      WriteChoice(ABlc,FloatToStrF(LNow,ffFixed,0,2)+#32+LDelay,
        'Ganhar '+#13+'R$' + FloatToStrF(LNow,ffFixed,0,2)   + ' reais' + #13 + 'agora',
        'Ganhar '+#13+'R$' + FloatToStrF(LLater,ffFixed,0,2) + ' reais' + #13 + 'daqui ' + LDelay,
        LNextTrial,'');
    end;

end;

procedure WriteToConfigurationFile;
var
  i : integer;
begin
  // training
  SetupStimuli;
  ConfigurationFile.WriteToBloc(1, _Name, 'Mensagem inicial');
  WriteMSG(1, 'M1', Message1);

  for i := Low(Delays) to High(Delays)do
  begin
    ConfigurationFile.WriteToBloc(i, _Name, Delays[i]);
    ConfigurationFile.WriteToBloc(i, _CrtMaxTrials, '10');
    WriteBloc(i);
  end;
end;

procedure WriteChoices;
var
  i : integer;
  LStringList : TStringList;
  LNow : real;
  LLater : real = 100.0;
  LDelay : string;
  LNextTrial : string;
begin
  LStringList := TStringList.Create;
  try
    for LDelay in Delays do
      begin
        LNow := 50.0;
        for i := 0 to 9 do
          begin
            case i of
            0 : LNextTrial := '10';
            else
              LNextTrial := '-1';
            end;

            LStringList.Append(
              'Ganhar R$' + FloatToStrF(LNow,ffFixed,0,2)   + ' reais agora' + #9 +     //left
              'Ganhar R$' + FloatToStrF(LLater,ffFixed,0,2) + ' reais daqui ' + LDelay + #9 + //right
              LNextTrial);
            LNow := LNow * 0.75;
          end;

        LNow := 50.0;
        for i := 0 to 8 do
          begin
            case i of
            0 : LNextTrial := '-10';
            8 : LNextTrial := '';
            else
              LNextTrial := '-1';
            end;
            LNow := LNow * 1.25;
            LStringList.Append(
              'Ganhar R$' + FloatToStrF(LNow,ffFixed,0,2)   + ' reais agora' + #9 +
              'Ganhar R$' + FloatToStrF(LLater,ffFixed,0,2) + ' reais daqui ' + LDelay + #9 +
              LNextTrial);

          end;
      end;
    LStringList.SaveToFile(GlobalContainer.RootMedia+FolderChoices+'desconto.txt');
  finally
    LStringList.Free;
  end;
end;

end.

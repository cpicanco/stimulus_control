unit Experiments.Maues;

{$mode objfpc}{$H+}

interface

procedure MakeConfigurationFile(ACondition : integer);
procedure ShowStimuli;

var
  ConfigurationFilename : string;

implementation

uses Classes, SysUtils, Forms, FileUtil
   , Forms.CheckStimuli
   , Constants
   , Session.ConfigurationFile
   , Session.Configuration.GlobalContainer
   , Controls.Trials.GoNoGo.Maues
   ;

type

  TExperimentalCategory = (InNatura, Processed, Control);

  TGoNoGoTrial = record
    Filename : string;
    ResponseStyle: TResponseStyle;
    ScreenSide : TScreenSide;
    Category : TExperimentalCategory;
  end;

  TGoNoGoTrials = array of TGoNoGoTrial;

var
  TrialsInNatura : TGoNoGoTrials;
  TrialsProcessed : TGoNoGoTrials;
  TrialsControlA : TGoNoGoTrials;
  TrialsControlB : TGoNoGoTrials;
  TrialsControl1 : TGoNoGoTrials;
  TrialsControl2 : TGoNoGoTrials;

const
  FolderInNatura =
   'condicao_experimental_in_natura_go'+DirectorySeparator;

  FolderProcessed =
   'condicao_experimental_ultraprocessados_no_go'+DirectorySeparator;

  FolderControlIntra =
   'condicao_experimental_controle'+DirectorySeparator;

  FolderControlInter =
   'condicao_controle'+DirectorySeparator;

  Folders : array [0..3] of string =
   (FolderInNatura, FolderProcessed, FolderControlIntra, FolderControlInter);

  StmDuration = 1250;

  StmSize = 300;

  ITI = 1250;

procedure SetupStimuli;
var
  i : integer;
  Folder : String;

  procedure FindFilesFor(out AStimuliArray: TGoNoGoTrials; AFolder : string);
  const
    Extensions : string =  '*.png;*.PNG;*.jpg;*.JPG';
  var
    Files : TStringList;
    i : integer;
  begin
    Files := TStringList.Create;
    try
      FindAllFiles(Files, AFolder, Extensions, True);
      SetLength(AStimuliArray, Files.Count);

      for i := Low(AStimuliArray) to High(AStimuliArray) do
      begin
        AStimuliArray[i].Filename := Files[i];
      end;
    finally
      Files.Free;
    end;
  end;

  function RandomScreenSide : TScreenSide;
  begin
    Result := TScreenSide(Random(Succ(Ord(High(TScreenSide)))));
  end;

begin
  for Folder in Folders do
    ForceDirectories(GlobalContainer.RootMedia+Folder);

  FindFilesFor(TrialsInNatura,  GlobalContainer.RootMedia+FolderInNatura);
  FindFilesFor(TrialsProcessed, GlobalContainer.RootMedia+FolderProcessed);
  FindFilesFor(TrialsControlA,  GlobalContainer.RootMedia+FolderControlIntra);
  FindFilesFor(TrialsControlB,  GlobalContainer.RootMedia+FolderControlIntra);
  FindFilesFor(TrialsControl1,  GlobalContainer.RootMedia+FolderControlInter);
  FindFilesFor(TrialsControl2,  GlobalContainer.RootMedia+FolderControlInter);

  for i := Low(TrialsInNatura) to High(TrialsInNatura) do
  begin
    TrialsInNatura[i].Category := InNatura;
    TrialsInNatura[i].ResponseStyle := Go;
    TrialsInNatura[i].ScreenSide := RandomScreenSide;
  end;

  for i := Low(TrialsProcessed) to High(TrialsProcessed) do
  begin
    TrialsProcessed[i].Category := Processed;
    TrialsProcessed[i].ResponseStyle := NoGo;
    TrialsProcessed[i].ScreenSide := RandomScreenSide;
  end;

  for i := Low(TrialsControlA) to High(TrialsControlA) do
  begin
    TrialsControlA[i].Category := Control;
    TrialsControlA[i].ResponseStyle := Go;
    TrialsControlA[i].ScreenSide := RandomScreenSide;
  end;

  for i := Low(TrialsControlB) to High(TrialsControlB) do
  begin
    TrialsControlB[i].Category := Control;
    TrialsControlB[i].ResponseStyle := NoGo;
    TrialsControlB[i].ScreenSide := RandomScreenSide;
  end;

  for i := Low(TrialsControl1) to High(TrialsControl1) do
  begin
    TrialsControl1[i].Category := Control;
    TrialsControl1[i].ResponseStyle := Go;
    TrialsControl1[i].ScreenSide := RandomScreenSide;
  end;

  for i := Low(TrialsControl2) to High(TrialsControl2) do
  begin
    TrialsControl2[i].Category := Control;
    TrialsControl2[i].ResponseStyle := NoGo;
    TrialsControl2[i].ScreenSide := RandomScreenSide;
  end;
end;

procedure NewConfigurationFile;
begin
  ConfigurationFilename := ExtractFilePath(
    Application.ExeName) + DirectorySeparator + 'last_session.ini';
  if FileExists(ConfigurationFilename) then
  begin
    DeleteFile(ConfigurationFilename);
  end;
  ConfigurationFile := TConfigurationFile.Create(ConfigurationFilename);
  ConfigurationFile.CacheUpdates := True;
  ConfigurationFile.WriteString(_Main, _NumBlc, '3');
  ConfigurationFile.WriteToBloc(1, _Name, 'SS');
  ConfigurationFile.Invalidate;
end;

procedure MakeConfigurationFile(ACondition : integer);
{
  bloco 1
  1 mensagem
  36 tentativas 18go/18nogo
  1 contadores de acerto e erro do bloco

  bloco 2..3
  36 tentativas 18go/18nogo
  1 contadores de acerto e erro do bloco

  36 =
  9 InNatura go
  9 Processed no go
  9 control   go
  9 control   no go
}
  procedure WriteMSGTrial(ABlc : integer);
  var
    M1 : string =
    'Serão apresentadas a você algumas imagens na tela do notebook. ' +
    'Para selecionar uma imagem à esquerda, pressione a tecla “c” do teclado. ' +
    'Para selecionar uma à direita, pressione “m”. ' +
    'A tarefa se assemelha a um jogo, você terá acesso a um contador de pontos ' +
    'que lhe dará o total de erros e de acertos. Aperte barra de espaço para começar.';

  i : integer;
  begin
    i := ConfigurationFile.TrialCount[1]+1;
    with ConfigurationFile do
    begin
      WriteToTrial(i, ABlc, _Name, 'Mensagem 1');
      WriteToTrial(i, ABlc, _Kind, T_MSG);
      WriteToTrial(i, ABlc, _ITI, ITI.ToString);
      WriteToTrial(i, ABlc, _Msg, M1)
    end;
  end;

  procedure WriteBloc(Experimental : Boolean; ABlc : integer);
    procedure WriteReviewTrial;
    var
      i : integer;
    begin
      i := ConfigurationFile.TrialCount[ABlc]+1;
      with ConfigurationFile do
      begin
        WriteToTrial(i, ABlc,_Name, 'Review');
        WriteToTrial(i, ABlc, _Cursor, '-1');
        WriteToTrial(i, ABlc, _Kind, T_PFR);
      end;
    end;

    procedure WriteTrial(ATrial : TGoNoGoTrial);
    var
      i : integer;
      Stimulus, Category, ResponseStyle, ScreenSide : string;
      function ExtractMediaName : string;
      begin
        Result := ATrial.Filename;
        Delete(Result,1,Pos('media', Result)+Length('media'));
      end;

    begin
      i := ConfigurationFile.TrialCount[ABlc]+1;
      with ConfigurationFile do
      begin
        WriteStr(Category, ATrial.Category);
        WriteStr(ResponseStyle, ATrial.ResponseStyle);
        WriteStr(ScreenSide, ATrial.ScreenSide);
        Stimulus := ExtractFileName(ATrial.Filename);
        WriteToTrial(i, ABlc, _Name, ('').Join(#32,
          [ExtractFileNameWithoutExt(Stimulus), Category, ResponseStyle, ScreenSide]));
        WriteToTrial(i, ABlc, _Cursor, '-1');
        WriteToTrial(i, ABlc, _Kind, T_GNG);
        WriteToTrial(i, ABlc, _ScreenSide, ScreenSide);
        WriteToTrial(i, ABlc, _ResponseStyle, ResponseStyle);
        WriteToTrial(i, ABlc, _Category, Category);
        WriteToTrial(i, ABlc, _LimitedHold, StmDuration.ToString);
        WriteToTrial(i, ABlc, _ITI, ITI.ToString);
        WriteToTrial(i, ABlc, _Comp+'1'+_cStm, ExtractMediaName);
        WriteToTrial(i, ABlc, _Comp+'1'+_cBnd, StmSize.ToString);
      end;
    end;

    procedure WriteGoNoGoTrials;
    var
      t, r, i, j : integer;
      GoNoGoTrialsMask : array [0..8, 0..3] of integer;
      GoNoGoTrials : array [0..8, 0..3] of TGoNoGoTrial;
    begin
      // initialize mask with ordered indexes
      for i := Low(GoNoGoTrialsMask) to High(GoNoGoTrialsMask) do
        for j := Low(GoNoGoTrialsMask[i]) to High(GoNoGoTrialsMask[i]) do
          GoNoGoTrialsMask[i, j] := j;

      // randomize j mask only
      for i := Low(GoNoGoTrialsMask) to High(GoNoGoTrialsMask) do
        for j := Low(GoNoGoTrialsMask[i]) to High(GoNoGoTrialsMask[i]) do
        begin
          r := Random(Length(GoNoGoTrialsMask[i]));
          t := GoNoGoTrialsMask[i, r];
          GoNoGoTrialsMask[i, r] := GoNoGoTrialsMask[i, j];
          GoNoGoTrialsMask[i, j] := t;
        end;

      if Experimental then
      begin
        // use mask to assign trials
        for i := Low(GoNoGoTrials) to High(GoNoGoTrials) do
        begin
          GoNoGoTrials[i, GoNoGoTrialsMask[i, 0]] := TrialsInNatura[i];
          GoNoGoTrials[i, GoNoGoTrialsMask[i, 1]] := TrialsProcessed[i];
          GoNoGoTrials[i, GoNoGoTrialsMask[i, 2]] := TrialsControlA[i];
          GoNoGoTrials[i, GoNoGoTrialsMask[i, 3]] := TrialsControlB[i];
        end;
      end else
      begin
        // use mask to assign trials
        for i := Low(GoNoGoTrials) to High(GoNoGoTrials) do
        begin
          GoNoGoTrials[i, GoNoGoTrialsMask[i, 0]] := TrialsControl1[i];
          GoNoGoTrials[i, GoNoGoTrialsMask[i, 1]] := TrialsControl2[i];
          GoNoGoTrials[i, GoNoGoTrialsMask[i, 2]] := TrialsControl1[i+9];
          GoNoGoTrials[i, GoNoGoTrialsMask[i, 3]] := TrialsControl2[i+9];
        end;
      end;

      for i := Low(GoNoGoTrials) to High(GoNoGoTrials) do
        for j := Low(GoNoGoTrials[i]) to High(GoNoGoTrials[i]) do
          WriteTrial(GoNoGoTrials[i, j]);
    end;
  begin
    WriteGoNoGoTrials;
    WriteReviewTrial;
  end;

var
  i: Integer;
begin
  NewConfigurationFile;
  case ACondition of
    0 :
      for i := 1 to 3 do
      begin
        ConfigurationFile.WriteToBloc(i, _Name, 'Experimental '+i.ToString);
        if i = 1 then WriteMSGTrial(i);
        WriteBloc(True, i);
      end;
    1 :
      for i := 1 to 3 do
      begin
        ConfigurationFile.WriteToBloc(i, _Name, 'Controle '+i.ToString);
        if i = 1 then WriteMSGTrial(i);
        WriteBloc(False, i);
      end;
  end;
  ConfigurationFile.Invalidate;
  ConfigurationFile.UpdateFile;
end;

procedure ShowStimuli;
var
  Stimuli : array of string;
  i: Integer;
begin
  SetLength(Stimuli, Length(TrialsInNatura));
  for i := Low(TrialsInNatura) to High(TrialsInNatura) do
    Stimuli[i] := TrialsInNatura[i].Filename;
  FormCheckStimuli.ShowStimuli(Stimuli, 0);

  SetLength(Stimuli, Length(TrialsProcessed));
  for i := Low(TrialsProcessed) to High(TrialsProcessed) do
    Stimuli[i] := TrialsProcessed[i].Filename;
  FormCheckStimuli.ShowStimuli(Stimuli, 1);

  SetLength(Stimuli, Length(TrialsControlA));
  for i := Low(TrialsControlA) to High(TrialsControlA) do
    Stimuli[i] := TrialsControlA[i].Filename;
  FormCheckStimuli.ShowStimuli(Stimuli, 2);

  SetLength(Stimuli, Length(TrialsControlB));
  for i := Low(TrialsControlB) to High(TrialsControlB) do
    Stimuli[i] := TrialsControlB[i].Filename;
  FormCheckStimuli.ShowStimuli(Stimuli, 3);

  SetLength(Stimuli, Length(TrialsControl1));
  for i := Low(TrialsControl1) to High(TrialsControl1) do
    Stimuli[i] := TrialsControl1[i].Filename;
  FormCheckStimuli.ShowStimuli(Stimuli, 4);

  SetLength(Stimuli, Length(TrialsControl2));
  for i := Low(TrialsControl2) to High(TrialsControl2) do
    Stimuli[i] := TrialsControl2[i].Filename;
  FormCheckStimuli.ShowStimuli(Stimuli, 5);
  FormCheckStimuli.Show;
end;

initialization
  Randomize;
  SetupStimuli;

end.

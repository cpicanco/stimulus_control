unit Experiments.Maues;

{$mode objfpc}{$H+}

interface

procedure MakeConfigurationFile(ACondition, ASessionBlocs : integer);
procedure ShowStimuli;

var
  ConfigurationFilename : string;
  SessionBlocs : integer = 3;

implementation

uses Classes, SysUtils, Forms, FileUtil
   , Forms.CheckStimuli
   , Constants
   , Session.ConfigurationFile
   , Session.Configuration.GlobalContainer
   ;

type

  TResponseStyle = (Go, NoGo);
  TScreenSide = (ssLeft, ssRight);
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
  TrialsClothesGo : TGoNoGoTrials;
  TrialsClothesNoGo : TGoNoGoTrials;
  TrialsHome : TGoNoGoTrials;
  TrialsTools : TGoNoGoTrials;

const
  FolderInNatura =
   'in_natura_go'+DirectorySeparator;

  FolderProcessed =
   'ultraprocessados_nogo'+DirectorySeparator;

  FolderControlHome =
   'controle_casa_go'+DirectorySeparator;

  FolderControlTools =
   'controle_ferramentas_nogo'+DirectorySeparator;

  FolderControlClothes =
   'controle_vestuario'+DirectorySeparator;

  Folders : array [0..4] of string = (
    FolderInNatura,
    FolderProcessed,
    FolderControlHome,
    FolderControlTools,
    FolderControlClothes);

  StmDuration = 1250;

  StmSize = 300;

  ITI = 1250;

procedure RandomizeStimuli(var Rand : array of integer);
var
  i, t, r : integer;
begin
  for i := Low(Rand) to High(Rand) do
  begin
    r := Random(Length(Rand));
    t := Rand[r];
    Rand[r] := Rand[i];
    Rand[i] := t;
  end;
end;

procedure SetupStimuli;
var
  i : integer;
  Folder : String;
  LTrialsClothes : TGoNoGoTrials;
  R : array [0..17] of integer =
   (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17);

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
  FindFilesFor(TrialsHome,  GlobalContainer.RootMedia+FolderControlHome);
  FindFilesFor(TrialsTools,  GlobalContainer.RootMedia+FolderControlTools);
  FindFilesFor(LTrialsClothes,  GlobalContainer.RootMedia+FolderControlClothes);

  SetLength(TrialsClothesNoGo, 9);
  SetLength(TrialsClothesGo, 9);
  SetLength(TrialsHome, 9);
  SetLength(TrialsTools, 9);
  RandomizeStimuli(R);

  for i:= Low(LTrialsClothes) to High(LTrialsClothes) do
  case i of
    0..8  : TrialsClothesGo[i] := LTrialsClothes[R[i]];
    9..17 : TrialsClothesNoGo[i-9] := LTrialsClothes[R[i]];
  end;

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

  for i := Low(TrialsClothesGo) to High(TrialsClothesGo) do
  begin
    TrialsClothesGo[i].Category := Control;
    TrialsClothesGo[i].ResponseStyle := Go;
    TrialsClothesGo[i].ScreenSide := RandomScreenSide;
  end;

  for i := Low(TrialsClothesNoGo) to High(TrialsClothesNoGo) do
  begin
    TrialsClothesNoGo[i].Category := Control;
    TrialsClothesNoGo[i].ResponseStyle := NoGo;
    TrialsClothesNoGo[i].ScreenSide := RandomScreenSide;
  end;

  for i := Low(TrialsHome) to High(TrialsHome) do
  begin
    TrialsHome[i].Category := Control;
    TrialsHome[i].ResponseStyle := Go;
    TrialsHome[i].ScreenSide := RandomScreenSide;
  end;

  for i := Low(TrialsTools) to High(TrialsTools) do
  begin
    TrialsTools[i].Category := Control;
    TrialsTools[i].ResponseStyle := NoGo;
    TrialsTools[i].ScreenSide := RandomScreenSide;
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

procedure MakeConfigurationFile(ACondition, ASessionBlocs: integer);
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
    'Você deve pressionar as teclas na presença de imagens envoltas na moldura fina, ' +
    'e não pressionar diante das envoltas em moldura grossa. ' +
    'A tarefa se assemelha a um jogo, você terá acesso a um contador de pontos que lhe ' +
    'dará o total de erros e de acertos. Aperte a barra de espaço para começar.';

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
      R1 : array [0..8] of integer = (0, 1, 2, 3, 4, 5, 6, 7, 8);
      R2 : array [0..8] of integer = (0, 1, 2, 3, 4, 5, 6, 7, 8);
      R3 : array [0..8] of integer = (0, 1, 2, 3, 4, 5, 6, 7, 8);
      R4 : array [0..8] of integer = (0, 1, 2, 3, 4, 5, 6, 7, 8);
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

      RandomizeStimuli(R1);
      RandomizeStimuli(R2);
      RandomizeStimuli(R3);
      RandomizeStimuli(R4);
      if Experimental then
      begin
        // use mask to assign trials
        for i := Low(GoNoGoTrials) to High(GoNoGoTrials) do
        begin
          GoNoGoTrials[i, GoNoGoTrialsMask[i, 0]] := TrialsInNatura[R1[i]];
          GoNoGoTrials[i, GoNoGoTrialsMask[i, 1]] := TrialsProcessed[R2[i]];
          GoNoGoTrials[i, GoNoGoTrialsMask[i, 2]] := TrialsClothesGo[R3[i]];
          GoNoGoTrials[i, GoNoGoTrialsMask[i, 3]] := TrialsClothesNoGo[R4[i]];
        end;
      end else
      begin
        // use mask to assign trials
        for i := Low(GoNoGoTrials) to High(GoNoGoTrials) do
        begin
          GoNoGoTrials[i, GoNoGoTrialsMask[i, 0]] := TrialsClothesGo[R1[i]];
          GoNoGoTrials[i, GoNoGoTrialsMask[i, 1]] := TrialsClothesNoGo[R2[i]];
          GoNoGoTrials[i, GoNoGoTrialsMask[i, 2]] := TrialsHome[R3[i]];
          GoNoGoTrials[i, GoNoGoTrialsMask[i, 3]] := TrialsTools[R4[i]];
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
      for i := 1 to ASessionBlocs do
      begin
        ConfigurationFile.WriteToBloc(i, _Name, 'Experimental '+i.ToString);
        if i = 1 then WriteMSGTrial(i);
        WriteBloc(True, i);
      end;
    1 :
      for i := 1 to ASessionBlocs do
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

  SetLength(Stimuli, Length(TrialsHome));
  for i := Low(TrialsHome) to High(TrialsHome) do
    Stimuli[i] := TrialsHome[i].Filename;
  FormCheckStimuli.ShowStimuli(Stimuli, 2);

  SetLength(Stimuli, Length(TrialsTools));
  for i := Low(TrialsTools) to High(TrialsTools) do
    Stimuli[i] := TrialsTools[i].Filename;
  FormCheckStimuli.ShowStimuli(Stimuli, 3);

  SetLength(Stimuli, Length(TrialsClothesGo));
  for i := Low(TrialsClothesGo) to High(TrialsClothesGo) do
    Stimuli[i] := TrialsClothesGo[i].Filename;
  FormCheckStimuli.ShowStimuli(Stimuli, 4);

  SetLength(Stimuli, Length(TrialsClothesNoGo));
  for i := Low(TrialsClothesNoGo) to High(TrialsClothesNoGo) do
    Stimuli[i] := TrialsClothesNoGo[i].Filename;
  FormCheckStimuli.ShowStimuli(Stimuli, 5);
  FormCheckStimuli.Show;
end;

initialization
  Randomize;
  SetupStimuli;

end.

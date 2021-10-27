unit Experiments.Augusto.Derivation;

{$mode objfpc}{$H+}

interface

function MakeConfigurationFile(
  ACondition, ASessionBlocs : integer) : string;

procedure WriteToConfigurationFile;

var
  ConfigurationFilename : string;
  ContextualBefor : string;
  ContextualAfter : string;
  ContextualEqual : String;
  ContextualDiffe : string;

const
  FolderDerivationTest =
    'teste-derivacao'+DirectorySeparator;

implementation

uses Classes, SysUtils
   , Constants
   , FileMethods
   , LazFileUtils
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   , Dialogs
   ;

type

  TResponseStyle = (None, Before, After, Equal, Different);
  TExpectedResponse = (Left, Right);

  TTrial = record
    FilenameA1 : string;
    FilenameA2 : string;
    FilenameA3 : string;
    FilenameA4 : string;

    FilenameB1 : string;
    FilenameB2 : string;
    FilenameB3 : string;
    FilenameB4 : string;

    FilenameC1 : string;
    FilenameC2 : string;
    FilenameC3 : string;
    FilenameC4 : string;

    ContextLabelLeft : string;
    ContextLabelRight : string;
    ContextLabelBottom : string;
    Animated : boolean;
    ResponseStyle: TResponseStyle;
    ExpectedResponse: TExpectedResponse;
    ID : byte;
  end;

  TTrials = array of TTrial;
  TTrialsArray = array of TTrials;

var
  StimuliA : TStringArray;
  StimuliB : TStringArray;
  StimuliC : TStringArray;

  TrialsAnimated : TTrials;   // first 4 trials
  TrialsNotAnimatedA : TTrials; // next 12 trials, all equal
  TrialsNotAnimatedB : TTrials; // next 12 trials, left equal, right diff

  Message1 : string;
  //MessageBefore : string;
  //MessageAfter : string;
  //MessageHIT : string;
  //MessageMISS : string;

const
  LimitedHold = 0;

  ITI = 3000;

procedure RandomizeStimuli(var AStimuli : TStringArray);
var
  i, r, l : integer;
  t : string;
begin
  l := Length(AStimuli);
  for i := Low(AStimuli) to High(AStimuli) do
  begin
    r := Random(l);
    t := AStimuli[r];
    AStimuli[r] := AStimuli[i];
    AStimuli[i] := t;
  end;
end;

procedure SetupStimuli;
var
  LDirectory : String;
  i : integer;
  procedure MountTrialsFor(var ATrials: TTrials;
    AStimuliA, AStimuliB, AStimuliC : TStringArray;
    AID : byte;
    AOffset : integer = 0);
  var
    i, j : integer;
  begin
    j := AOffset;
    for i := Low(ATrials) to High(ATrials) do
    begin
      ATrials[i].FilenameA1 := AStimuliA[0];
      ATrials[i].FilenameA2 := AStimuliA[1];
      ATrials[i].FilenameA3 := AStimuliA[2];
      ATrials[i].FilenameA4 := AStimuliA[3];
      ATrials[i].FilenameB1 := AStimuliB[j];
      ATrials[i].FilenameB2 := AStimuliB[j+1];
      ATrials[i].FilenameB3 := AStimuliB[j+2];
      ATrials[i].FilenameB4 := AStimuliB[j+3];
      ATrials[i].FilenameC1 := AStimuliC[j];
      ATrials[i].FilenameC2 := AStimuliC[j+1];
      ATrials[i].FilenameC3 := AStimuliC[j+2];
      ATrials[i].FilenameC4 := AStimuliC[j+3];
      Inc(j, 4);
      ATrials[i].ID := AID;
    end;
  end;

begin
  LoadMessageFromFile(Message1, GlobalContainer.RootMedia+'MensagemDerivacao.txt');
  //LoadMessageFromFile(MessageBefore, GlobalContainer.RootMedia+'Mensagem-Antes.txt');
  //LoadMessageFromFile(MessageAfter, GlobalContainer.RootMedia+'Mensagem-Depois.txt');
  //LoadMessageFromFile(MessageHIT, GlobalContainer.RootMedia+'Mensagem-acerto.txt');
  //LoadMessageFromFile(MessageMISS, GlobalContainer.RootMedia+'Mensagem-erro.txt');

  ContextualBefor := GlobalContainer.RootMedia+'Antes.jpg';
  ContextualAfter := GlobalContainer.RootMedia+'Depois.jpg';
  ContextualEqual := GlobalContainer.RootMedia+'Igual.jpg';
  ContextualDiffe := GlobalContainer.RootMedia+'Diferente.jpg';

  LDirectory := GlobalContainer.RootMedia+FolderDerivationTest;
  ForceDirectories(LDirectory);

  FindFilesFor(StimuliA, LDirectory, 'A*.bmp;A*.png;A*.jpg;A*.JPG');
  if Length(StimuliA) < 4 then
    Exception.Create('Há estímulos do conjunto A faltando na pasta do teste de derivação');

  FindFilesFor(StimuliB, LDirectory, 'B*.bmp;B*.png;B*.jpg;B*.JPG');
  if Length(StimuliB) < 64 then
    Exception.Create('Há estímulos do conjunto B faltando na pasta do teste de derivação');

  RandomizeStimuli(StimuliB);

  FindFilesFor(StimuliC, LDirectory, 'C*.bmp;C*.png;C*.jpg;C*.JPB');
  if Length(StimuliC) >= 64 then
    Exception.Create('Há estímulos do conjunto C faltando na pasta do teste de derivação');

  RandomizeStimuli(StimuliC);

  SetLength(TrialsAnimated, 4);
  for i:= Low(TrialsAnimated) to High(TrialsAnimated) do
  begin
    TrialsAnimated[i].Animated:=True;
    TrialsAnimated[i].ContextLabelLeft := ContextualEqual;
    TrialsAnimated[i].ContextLabelRight := ContextualEqual;
    TrialsAnimated[i].ContextLabelBottom := ContextualBefor;
  end;

  SetLength(TrialsNotAnimatedA, 12);
  for i:= Low(TrialsNotAnimatedA) to High(TrialsNotAnimatedA) do
  begin
    TrialsNotAnimatedA[i].ContextLabelLeft := ContextualEqual;
    TrialsNotAnimatedA[i].ContextLabelRight := ContextualEqual;
    case i of
    0..5  : TrialsNotAnimatedA[i].ContextLabelBottom := ContextualBefor;
    6..11 : TrialsNotAnimatedA[i].ContextLabelBottom := ContextualAfter;
    end;
    TrialsNotAnimatedA[i].Animated:=False;
  end;

  RandomizeStimuli(StimuliB);
  RandomizeStimuli(StimuliC);
  SetLength(TrialsNotAnimatedB, 12);
  for i:= Low(TrialsNotAnimatedB) to High(TrialsNotAnimatedB) do
  begin
    TrialsNotAnimatedB[i].ContextLabelLeft := ContextualEqual;
    TrialsNotAnimatedB[i].ContextLabelRight := ContextualDiffe;
    case i of
    0..5  : TrialsNotAnimatedB[i].ContextLabelBottom := ContextualBefor;
    6..11 : TrialsNotAnimatedB[i].ContextLabelBottom := ContextualAfter;
    end;
    TrialsNotAnimatedB[i].Animated:=False;
  end;

  MountTrialsFor(TrialsAnimated, StimuliA, StimuliB, StimuliC, 0);
  MountTrialsFor(TrialsNotAnimatedA, StimuliA, StimuliB, StimuliC, 1, 16);
  MountTrialsFor(TrialsNotAnimatedB, StimuliA, StimuliB, StimuliC, 1, 16);
end;

// https://stackoverflow.com/questions/20190110/2d-int-array-shuffle
function RandomizeTrials(var ATrials : TTrialsArray) : TTrials;
var
  r, t, j, i :  integer;
  temp : TPoint;
  LTrialsMask : array of array of TPoint;
begin
  // initialize
  SetLength(LTrialsMask, Length(ATrials));
  for i := Low(LTrialsMask) to High(LTrialsMask) do
  begin
    SetLength(LTrialsMask[i], Length(ATrials[i]));
    for j := Low(LTrialsMask[i]) to High(LTrialsMask[i]) do
      LTrialsMask[i][j] := Point(i,j);
  end;

  for i := Low(LTrialsMask) to High(LTrialsMask) do
  begin
    for j := Low(LTrialsMask[i]) to High(LTrialsMask[i]) do
    begin
        r := Random(i+1);
        t := Random(j+1);
        temp := LTrialsMask[i, j];
        LTrialsMask[i, j] := LTrialsMask[r, t];
        LTrialsMask[r, t] := temp;
    end;
  end;

  SetLength(Result, 0);
  for i := Low(LTrialsMask) to High(LTrialsMask) do
  begin
    for j := Low(LTrialsMask[i]) to High(LTrialsMask[i]) do
    begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)] := ATrials[LTrialsMask[i][j].x][LTrialsMask[i][j].y];
    end;
  end;
  //for i := Low(Result) to High(Result) do
  //  WriteLn(Result[i].ID);
end;


procedure WriteMSG1Trial(ABlc : integer);
var
  i : integer;
begin
  with ConfigurationFile do
  begin
    WriteToTrial(1, ABlc, _Name, 'Mensagem 1');
    WriteToTrial(1, ABlc, _Cursor, '-1');
    WriteToTrial(1, ABlc, _Kind, T_MSG);
    WriteToTrial(1, ABlc, _ITI, ITI.ToString);
    WriteToTrial(1, ABlc, _Msg, Message1)
  end;
end;

procedure WriteBloc(ABlc : integer);
var
  i : integer;
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

  procedure WriteRelationalFrameTrial(ATrial : TTrial);
  var
    aux : string;
    r, i : integer;
    //function ExtractMediaName(AFilename: string): string;
    //begin
    //  Result := AFilename;
    //  Delete(Result,1,Pos('media', Result)+Length('media'));
    //end;
    LKeys : TStringList;

  begin
    LKeys := TStringList.Create;
    LKeys.Append('Z');
    LKeys.Append('C');
    LKeys.Append('B');
    LKeys.Append('M');
    for i := 0 to LKeys.Count-1 do
    begin
      r := Random(LKeys.Count);
      aux := LKeys[i];
      LKeys[i] := LKeys[r];
      LKeys[r] := aux;
    end;

    i := ConfigurationFile.TrialCount[ABlc]+1;
    with ConfigurationFile do
    begin
      WriteToTrial(i, ABlc, _Name, 'Teste');
      WriteToTrial(i, ABlc, _Cursor, '-1');
      WriteToTrial(i, ABlc, _Kind, T_RFT);
      //WriteToTrial(i, ABlc, _Type, '');
      //WriteToTrial(i, ABlc, _ExpectedResponse, ExpectedResponse);
      WriteToTrial(i, ABlc, _LimitedHold, LimitedHold.ToString);
      WriteToTrial(i, ABlc, _ITI, ITI.ToString);
      WriteToTrial(i, ABlc, _Consequence, BoolToStr(False,'1','0'));
      WriteToTrial(i, ABlc, 'ShowType', BoolToStr(ATrial.Animated,'1','0'));

      WriteToTrial(i, ABlc, 'KC1', LKeys[0]);
      WriteToTrial(i, ABlc, 'KC2', LKeys[1]);
      WriteToTrial(i, ABlc, 'KC3', LKeys[2]);
      WriteToTrial(i, ABlc, 'KC4', LKeys[3]);

      {
          Each cue has its own key index.
          KL/KR means Left/Right Key Indexes. (TODO: refactor)
          Each index corresponds to a cue [for example, KL1-Cue1.., KR1-Cue5.., down there]
          the index and the visual position should match,
          counting from left to right on screen.
      }
      WriteToTrial(i, ABlc, 'KL1', '1');
      WriteToTrial(i, ABlc, 'KL2', '2');
      WriteToTrial(i, ABlc, 'KL3', '3');
      WriteToTrial(i, ABlc, 'KL4', '4');
      WriteToTrial(i, ABlc, 'KR1', '1');
      WriteToTrial(i, ABlc, 'KR2', '2');
      WriteToTrial(i, ABlc, 'KR3', '3');
      WriteToTrial(i, ABlc, 'KR4', '4');
      if ATrial.ContextLabelBottom = ContextualBefor then
      begin
        WriteToTrial(i, ABlc, 'BottomOrientation', BoolToStr(True)); // allbefore
      end;

      if ATrial.ContextLabelBottom = ContextualAfter then
      begin
        WriteToTrial(i, ABlc, 'BottomOrientation', BoolToStr(False)); // allafter
      end;

      WriteToTrial(i, ABlc, 'BottomCue', ATrial.ContextLabelBottom);
      if ATrial.ContextLabelRight = ContextualEqual then
      begin
        WriteToTrial(i, ABlc, 'Cue1', 'Equal');
        WriteToTrial(i, ABlc, 'Cue2', 'Equal');
        WriteToTrial(i, ABlc, 'Cue3', 'Equal');
        WriteToTrial(i, ABlc, 'Cue4', 'Equal');
      end;
      if ATrial.ContextLabelRight = ContextualDiffe then
      begin
        WriteToTrial(i, ABlc, 'Cue1', 'Diffe');
        WriteToTrial(i, ABlc, 'Cue2', 'Diffe');
        WriteToTrial(i, ABlc, 'Cue3', 'Diffe');
        WriteToTrial(i, ABlc, 'Cue4', 'Diffe');
      end;
      if ATrial.ContextLabelLeft = ContextualEqual then
      begin
        WriteToTrial(i, ABlc, 'Cue5', 'Equal');
        WriteToTrial(i, ABlc, 'Cue6', 'Equal');
        WriteToTrial(i, ABlc, 'Cue7', 'Equal');
        WriteToTrial(i, ABlc, 'Cue8', 'Equal');
      end;
      if ATrial.ContextLabelLeft = ContextualDiffe then
      begin
        WriteToTrial(i, ABlc, 'Cue5', 'Diffe');
        WriteToTrial(i, ABlc, 'Cue6', 'Diffe');
        WriteToTrial(i, ABlc, 'Cue7', 'Diffe');
        WriteToTrial(i, ABlc, 'Cue8', 'Diffe');
      end;
      WriteToTrial(i, ABlc, 'CueImage1', ATrial.ContextLabelRight);
      WriteToTrial(i, ABlc, 'CueImage2', ATrial.ContextLabelRight);
      WriteToTrial(i, ABlc, 'CueImage3', ATrial.ContextLabelRight);
      WriteToTrial(i, ABlc, 'CueImage4', ATrial.ContextLabelRight);
      WriteToTrial(i, ABlc, 'CueImage5', ATrial.ContextLabelLeft);
      WriteToTrial(i, ABlc, 'CueImage6', ATrial.ContextLabelLeft);
      WriteToTrial(i, ABlc, 'CueImage7', ATrial.ContextLabelLeft);
      WriteToTrial(i, ABlc, 'CueImage8', ATrial.ContextLabelLeft);
      WriteToTrial(i, ABlc, 'A1', ATrial.FilenameA1);
      WriteToTrial(i, ABlc, 'A2', ATrial.FilenameA2);
      WriteToTrial(i, ABlc, 'A3', ATrial.FilenameA3);
      WriteToTrial(i, ABlc, 'A4', ATrial.FilenameA4);
      WriteToTrial(i, ABlc, 'B1', ATrial.FilenameB1);
      WriteToTrial(i, ABlc, 'B2', ATrial.FilenameB2);
      WriteToTrial(i, ABlc, 'B3', ATrial.FilenameB3);
      WriteToTrial(i, ABlc, 'B4', ATrial.FilenameB4);
      WriteToTrial(i, ABlc, 'C1', ATrial.FilenameC1);
      WriteToTrial(i, ABlc, 'C2', ATrial.FilenameC2);
      WriteToTrial(i, ABlc, 'C3', ATrial.FilenameC3);
      WriteToTrial(i, ABlc, 'C4', ATrial.FilenameC4);
    end;
    LKeys.Free
  end;

begin
  case ABlc of
    2, 12 :
      for i := Low(TrialsAnimated) to High(TrialsAnimated) do
        WriteRelationalFrameTrial(TrialsAnimated[i]);

    3, 13 :
      begin
        for i := Low(TrialsNotAnimatedA) to High(TrialsNotAnimatedA) do
          WriteRelationalFrameTrial(TrialsNotAnimatedA[i]);

        for i := Low(TrialsNotAnimatedB) to High(TrialsNotAnimatedB) do
          WriteRelationalFrameTrial(TrialsNotAnimatedB[i]);
      end;
  end;
end;

function MakeConfigurationFile(ACondition, ASessionBlocs: integer): string;
begin
  SetupStimuli;
  Result := NewConfigurationFile;

  ConfigurationFile.WriteToBloc(1, _Name, 'Mensagem do Teste de Derivação');
  WriteMSG1Trial(1);

  ConfigurationFile.WriteToBloc(2, _Name, 'Teste de Derivação (tentativas animadas)');
  WriteBloc(2);

  ConfigurationFile.WriteToBloc(3, _Name, 'Teste de Derivação');
  ConfigurationFile.WriteToBloc(3, _CrtHitValue, '21');
  ConfigurationFile.WriteToBloc(3, _MaxBlcRepetition, '1');
  WriteBloc(3);

  ConfigurationFile.Invalidate;
  ConfigurationFile.UpdateFile;
end;

procedure WriteToConfigurationFile;
begin
  SetupStimuli;
  ConfigurationFile.WriteToBloc(11, _Name, 'Mensagem do Teste de Derivação');
  WriteMSG1Trial(11);

  ConfigurationFile.WriteToBloc(12, _Name, 'Teste de Derivação (tentativas animadas)');
  WriteBloc(12);

  ConfigurationFile.WriteToBloc(13, _Name, 'Teste de Derivação');
  ConfigurationFile.WriteToBloc(13, _CrtHitValue, '21');
  ConfigurationFile.WriteToBloc(13, _MaxBlcRepetition, '1');
  WriteBloc(13);
end;

operator = (A, B: TTrial) : Boolean;
begin
  if (A.ID = B.ID) or (A.ResponseStyle = B.ResponseStyle) then
    Result := True
  else
    Result := False;
end;

initialization
  Randomize;

end.


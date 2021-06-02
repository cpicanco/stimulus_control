unit Experiments.SecondDerivation;

{$mode objfpc}{$H+}

interface

function MakeConfigurationFileHigh(
  ACondition, ASessionBlocs : integer) : string;
function MakeConfigurationFileLow(
  ACondition, ASessionBlocs : integer) : string;

procedure WriteToConfigurationFileHigh;
procedure WriteToConfigurationFileLow;

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

    Key1 : char;
    Key2 : char;
    Key3 : char;
    Key4 : char;

    ContextLabelLeft : string;
    ContextLabelRight : string;
    ContextLabelBottom : string;
    Animated : boolean;
    Consequence : string;
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

  //StimuliKeyB : TStringArray;
  //StimuliKeyC : TStringArray;

  TrialsAnimated : TTrials;   // first 4 trials
  TrialsHighDerivation : TTrials;
  TrialsLowDerivation : TTrials;
  TrialsPunishment : TTrials;
  TrialsReinforcement : TTrials;

  TrialKey : TTrial;
  TrialG01 : TTrial;
  TrialG02 : TTrial;
  TrialG03 : TTrial;
  TrialG04 : TTrial;
  TrialG05 : TTrial;
  TrialG06 : TTrial;
  TrialG07 : TTrial;
  TrialG08 : TTrial;
  TrialG09 : TTrial;
  TrialG10 : TTrial;
  TrialG11 : TTrial;

  Message1 : string;
  //MessageBefore : string;
  //MessageAfter : string;
  //MessageHIT : string;
  //MessageMISS : string;

const
  LimitedHold = 0;

  ITI = 1500;

  MaxTrials = 30;

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
  LStringList : TStringList;
  LHD : array [0..MaxTrials-1] of integer =
    (0,1,2,3,4,5,6,7,8,0,9,5,6,1,2,8,7,4,9,3,1,9,0,7,5,6,8,3,4,2);

  LLD : array [0..MaxTrials-1] of integer =
    (0,1,0,2,0,0,3,0,4,0,9,0,3,0,1,0,4,0,2,0,9,0,2,0,1,0,3,4,0,9);

  LPP : array [0..MaxTrials-1] of integer =
    (0,9,10,11,0,11,10,9,0,9,10,11,0,10,9,0,11,9,0,11,10,9,0,10,11,0,9,0,11,0);

  LRR : array [0..MaxTrials-1] of integer =
    (0,9,0,10,0,9,11,10,0,10,11,9,10,0,11,0,10,9,0,11,9,11,0,10,9,10,11,0,9,0);

  LDirectory : String;
  i : integer;

  procedure ConfigureTrialKeys(var ATrial : TTrial);
  var
    LKeys : TStringList;
    i, r : integer;
  begin
    LKeys := TStringList.Create;
    LKeys.Append('Z');
    LKeys.Append('C');
    LKeys.Append('B');
    LKeys.Append('M');
    for i := 0 to LKeys.Count-1 do
    begin
      r := Random(LKeys.Count);
      LKeys.Exchange(i, r);
    end;
    ATrial.Key1 := LKeys[0][1];
    ATrial.Key2 := LKeys[1][1];
    ATrial.Key3 := LKeys[2][1];
    ATrial.Key4 := LKeys[3][1];
    LKeys.Free;
  end;

  procedure ConfigureTrial(var ATrial : TTrial;
    ATrialType : integer);
  var
    LTrial : TTrial;
  begin
    case ATrialType of
      0 : LTrial := TrialKey;
      1 : LTrial := TrialG01;
      2 : LTrial := TrialG02;
      3 : LTrial := TrialG03;
      4 : LTrial := TrialG04;
      5 : LTrial := TrialG05;
      6 : LTrial := TrialG06;
      7 : LTrial := TrialG07;
      8 : LTrial := TrialG08;
      9 : LTrial := TrialG09;
      10 : LTrial := TrialG10;
      11 : LTrial := TrialG11;
    end;
    ATrial.FilenameA1 := LTrial.FilenameA1;
    ATrial.FilenameA2 := LTrial.FilenameA2;
    ATrial.FilenameA3 := LTrial.FilenameA3;
    ATrial.FilenameA4 := LTrial.FilenameA4;
    ATrial.FilenameB1 := LTrial.FilenameB1;
    ATrial.FilenameB2 := LTrial.FilenameB2;
    ATrial.FilenameB3 := LTrial.FilenameB3;
    ATrial.FilenameB4 := LTrial.FilenameB4;
    ATrial.FilenameC1 := LTrial.FilenameC1;
    ATrial.FilenameC2 := LTrial.FilenameC2;
    ATrial.FilenameC3 := LTrial.FilenameC3;
    ATrial.FilenameC4 := LTrial.FilenameC4;
    ATrial.Key1 := LTrial.Key1;
    ATrial.Key2 := LTrial.Key2;
    ATrial.Key3 := LTrial.Key3;
    ATrial.Key4 := LTrial.Key4;
    ATrial.Animated:=LTrial.Animated;
    ATrial.ContextLabelBottom:=LTrial.ContextLabelBottom;
    ATrial.ContextLabelLeft:=LTrial.ContextLabelLeft;
    ATrial.ContextLabelRight:=LTrial.ContextLabelRight;
    ATrial.ID:=LTrial.ID;
  end;

  procedure MountTrialFor(var ATrial: TTrial; var AOffset : integer;
    AStimuliA, AStimuliB, AStimuliC : TStringArray);
  begin
    ConfigureTrialKeys(ATrial);
    with ATrial do begin
      FilenameA1 := AStimuliA[0];
      FilenameA2 := AStimuliA[1];
      FilenameA3 := AStimuliA[2];
      FilenameA4 := AStimuliA[3];
      FilenameB1 := AStimuliB[AOffset];
      FilenameB2 := AStimuliB[AOffset+1];
      FilenameB3 := AStimuliB[AOffset+2];
      FilenameB4 := AStimuliB[AOffset+3];
      FilenameC1 := AStimuliC[AOffset];
      FilenameC2 := AStimuliC[AOffset+1];
      FilenameC3 := AStimuliC[AOffset+2];
      FilenameC4 := AStimuliC[AOffset+3];
    end;
    Inc(AOffset, 4);
  end;

  procedure MountTrialsFor(var ATrials: TTrials; var AOffset : integer;
    AStimuliA, AStimuliB, AStimuliC : TStringArray);
  var
    i : integer;
  begin
    for i := Low(ATrials) to High(ATrials) do
    begin
      ConfigureTrialKeys(ATrials[i]);
      ATrials[i].FilenameA1 := AStimuliA[0];
      ATrials[i].FilenameA2 := AStimuliA[1];
      ATrials[i].FilenameA3 := AStimuliA[2];
      ATrials[i].FilenameA4 := AStimuliA[3];
      ATrials[i].FilenameB1 := AStimuliB[AOffset];
      ATrials[i].FilenameB2 := AStimuliB[AOffset+1];
      ATrials[i].FilenameB3 := AStimuliB[AOffset+2];
      ATrials[i].FilenameB4 := AStimuliB[AOffset+3];
      ATrials[i].FilenameC1 := AStimuliC[AOffset];
      ATrials[i].FilenameC2 := AStimuliC[AOffset+1];
      ATrials[i].FilenameC3 := AStimuliC[AOffset+2];
      ATrials[i].FilenameC4 := AStimuliC[AOffset+3];
      Inc(AOffset, 4);
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


  //FindFilesFor(StimuliKeyB, LDirectory, 'CHAVE-B*.bmp;CHAVE-B*.png;CHAVE-B*.jpg;CHAVE-B*.JPG');
  //if Length(StimuliKeyB) < 4 then
  //  Exception.Create('Há estímulos do conjunto CHAVE-B faltando na pasta do teste de derivação');
  //
  //RandomizeStimuli(StimuliKeyB);
  //
  //FindFilesFor(StimuliKeyC, LDirectory, 'CHAVE-C*.bmp;CHAVE-C*.png;CHAVE-C*.jpg;CHAVE-C*.JPG');
  //if Length(StimuliKeyC) < 4 then
  //  Exception.Create('Há estímulos do conjunto CHAVE-C faltando na pasta do teste de derivação');
  //
  //RandomizeStimuli(StimuliKeyC);

  FindFilesFor(StimuliB, LDirectory, 'B*.bmp;B*.png;B*.jpg;B*.JPG');
  if Length(StimuliB) < 64 then
    Exception.Create('Há estímulos do conjunto B faltando na pasta do teste de derivação');

  FindFilesFor(StimuliC, LDirectory, 'C*.bmp;C*.png;C*.jpg;C*.JPB');
  if Length(StimuliC) >= 64 then
    Exception.Create('Há estímulos do conjunto C faltando na pasta do teste de derivação');

  SetLength(TrialsAnimated, 4);
  for i:= Low(TrialsAnimated) to High(TrialsAnimated) do
  begin
    TrialsAnimated[i].Animated:=True;
    TrialsAnimated[i].ContextLabelLeft := ContextualEqual;
    TrialsAnimated[i].ContextLabelRight := ContextualEqual;
    TrialsAnimated[i].ContextLabelBottom := ContextualBefor;
    TrialsAnimated[i].ID := 255;
  end;



  //////////////////////////////////////////////////////////////////////////////


  with TrialKey do begin
    Animated:=False;
    ContextLabelLeft := ContextualEqual;
    ContextLabelRight := ContextualEqual;
    ContextLabelBottom := ContextualAfter;
    ID := 0;
  end;

  with TrialG07 do begin
    Animated:=False;
    ContextLabelLeft := ContextualEqual;
    ContextLabelRight := ContextualEqual;
    ContextLabelBottom := ContextualAfter;
    ID := 7;
  end;

  //////////////////////////////////////////////////////////////////////////////

  with TrialG01 do begin
    Animated:=False;
    ContextLabelLeft := ContextualEqual;
    ContextLabelRight := ContextualEqual;
    ContextLabelBottom := ContextualBefor;
    ID := 1;
  end;

  with TrialG06 do begin
    Animated:=False;
    ContextLabelLeft := ContextualEqual;
    ContextLabelRight := ContextualEqual;
    ContextLabelBottom := ContextualBefor;
    ID := 6;
  end;

  with TrialG09 do begin
    Animated:=False;
    ContextLabelLeft := ContextualEqual;
    ContextLabelRight := ContextualEqual;
    ContextLabelBottom := ContextualBefor;
    ID := 9;
  end;

  //////////////////////////////////////////////////////////////////////////////

  with TrialG02 do begin
    Animated:=False;
    ContextLabelLeft := ContextualEqual;
    ContextLabelRight := ContextualDiffe;
    ContextLabelBottom := ContextualBefor;
    ID := 2;
  end;

  with TrialG05 do begin
    Animated:=False;
    ContextLabelLeft := ContextualEqual;
    ContextLabelRight := ContextualDiffe;
    ContextLabelBottom := ContextualBefor;
    ID := 5;
  end;

  with TrialG08 do begin
    Animated:=False;
    ContextLabelLeft := ContextualEqual;
    ContextLabelRight := ContextualDiffe;
    ContextLabelBottom := ContextualBefor;
    ID := 8;
  end;

  with TrialG10 do begin
    Animated:=False;
    ContextLabelLeft := ContextualEqual;
    ContextLabelRight := ContextualDiffe;
    ContextLabelBottom := ContextualBefor;
    ID := 10;
  end;

  //////////////////////////////////////////////////////////////////////////////

  with TrialG03 do begin
    Animated:=False;
    ContextLabelLeft := ContextualEqual;
    ContextLabelRight := ContextualDiffe;
    ContextLabelBottom := ContextualAfter;
    ID := 3;
  end;

  with TrialG04 do begin
    Animated:=False;
    ContextLabelLeft := ContextualEqual;
    ContextLabelRight := ContextualDiffe;
    ContextLabelBottom := ContextualAfter;
    ID := 4;
  end;

  with TrialG11 do begin
    Animated:=False;
    ContextLabelLeft := ContextualEqual;
    ContextLabelRight := ContextualDiffe;
    ContextLabelBottom := ContextualAfter;
    ID := 11;
  end;

  RandomizeStimuli(StimuliB);
  RandomizeStimuli(StimuliC);

  i := 0;
  MountTrialsFor(TrialsAnimated, i,StimuliA, StimuliB, StimuliC);
  MountTrialFor(TrialKey, i, StimuliA, StimuliB, StimuliC);
  MountTrialFor(TrialG01, i, StimuliA, StimuliB, StimuliC);
  MountTrialFor(TrialG02, i, StimuliA, StimuliB, StimuliC);
  MountTrialFor(TrialG03, i, StimuliA, StimuliB, StimuliC);
  MountTrialFor(TrialG04, i, StimuliA, StimuliB, StimuliC);
  MountTrialFor(TrialG05, i, StimuliA, StimuliB, StimuliC);
  MountTrialFor(TrialG06, i, StimuliA, StimuliB, StimuliC);
  MountTrialFor(TrialG07, i, StimuliA, StimuliB, StimuliC);
  MountTrialFor(TrialG08, i, StimuliA, StimuliB, StimuliC);
  MountTrialFor(TrialG09, i, StimuliA, StimuliB, StimuliC);
  MountTrialFor(TrialG10, i, StimuliA, StimuliB, StimuliC);
  MountTrialFor(TrialG11, i, StimuliA, StimuliB, StimuliC);

  SetLength(TrialsHighDerivation, MaxTrials);
  SetLength(TrialsLowDerivation, MaxTrials);
  SetLength(TrialsPunishment, MaxTrials);
  SetLength(TrialsReinforcement, MaxTrials);

  LStringList := TStringList.Create;

  LDirectory := GlobalContainer.RootMedia+'ordembaixaderivacao.txt';
  if FileExistsUTF8(LDirectory) then
  begin
    LStringList.LoadFromFile(LDirectory);
    for i := Low(LLD) to High(LLD) do
      LLD[i] := StrToInt(LStringList[i]);
  end;

  LDirectory := GlobalContainer.RootMedia+'ordemaltaderivacao.txt';
  if FileExistsUTF8(LDirectory) then
  begin
    LStringList.LoadFromFile(LDirectory);
    for i := Low(LHD) to High(LHD) do
      LHD[i] := StrToInt(LStringList[i]);
  end;

  LDirectory := GlobalContainer.RootMedia+'ordempunicao.txt';
  if FileExistsUTF8(LDirectory) then
  begin
    LStringList.LoadFromFile(LDirectory);
    for i := Low(LPP) to High(LPP) do
      LPP[i] := StrToInt(LStringList[i]);
  end;

  LDirectory := GlobalContainer.RootMedia+'ordemreforco.txt';
  if FileExistsUTF8(LDirectory) then
  begin
    LStringList.LoadFromFile(LDirectory);
    for i := Low(LRR) to High(LRR) do
      LRR[i] := StrToInt(LStringList[i]);
  end;

  for i := Low(LHD) to High(LHD) do
    ConfigureTrial(TrialsHighDerivation[i], LHD[i]);

  for i := Low(LLD) to High(LLD) do
    ConfigureTrial(TrialsLowDerivation[i], LLD[i]);

  for i := Low(LPP) to High(LPP) do
  begin
    ConfigureTrial(TrialsPunishment[i], LPP[i]);
    TrialsPunishment[i].Consequence := 'P';
  end;

  for i := Low(LRR) to High(LRR) do
  begin
    ConfigureTrial(TrialsReinforcement[i], LRR[i]);
    TrialsReinforcement[i].Consequence := 'R';
  end;
end;

procedure WriteMSG1Trial(ABlc : integer);
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

procedure WriteBloc(ABlc : integer; IsHighGroup:Boolean = False);
var
  i : integer;

  procedure WriteRelationalFrameTrial(ATrial : TTrial);
  var
    i : integer;
  begin
    i := ConfigurationFile.TrialCount[ABlc]+1;
    with ConfigurationFile do
    begin
      WriteToTrial(i, ABlc, _Name, 'Teste');
      WriteToTrial(i, ABlc, _Cursor, '-1');
      WriteToTrial(i, ABlc, _Kind, T_RFT);
      WriteToTrial(i, ABlc, _LimitedHold, LimitedHold.ToString);
      WriteToTrial(i, ABlc, _ITI, ITI.ToString);
      if ATrial.Consequence = '' then
        WriteToTrial(i, ABlc, _Consequence, BoolToStr(False,'1','0'))
      else
        WriteToTrial(i, ABlc, _Consequence, ATrial.Consequence);

      WriteToTrial(i, ABlc, 'ShowType', BoolToStr(ATrial.Animated,'1','0'));

      WriteToTrial(i, ABlc, 'KC1', ATrial.Key1);
      WriteToTrial(i, ABlc, 'KC2', ATrial.Key2);
      WriteToTrial(i, ABlc, 'KC3', ATrial.Key3);
      WriteToTrial(i, ABlc, 'KC4', ATrial.Key4);

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
      WriteToTrial(i, ABlc, 'ID', ATrial.ID.ToString);
    end;
  end;

begin
  case ABlc of
    2, 12 :
      for i := Low(TrialsAnimated) to High(TrialsAnimated) do
        WriteRelationalFrameTrial(TrialsAnimated[i]);


    3, 13 :
      if IsHighGroup then begin
        for i := Low(TrialsHighDerivation) to High(TrialsHighDerivation) do
          WriteRelationalFrameTrial(TrialsHighDerivation[i]);
      end else begin
        for i := Low(TrialsLowDerivation) to High(TrialsLowDerivation) do
          WriteRelationalFrameTrial(TrialsLowDerivation[i]);
      end;

    4, 14 :
      begin
        for i := Low(TrialsPunishment) to High(TrialsPunishment) do
          WriteRelationalFrameTrial(TrialsPunishment[i]);
      end;

    5, 15 :
      begin
        for i := Low(TrialsReinforcement) to High(TrialsReinforcement) do
          WriteRelationalFrameTrial(TrialsReinforcement[i]);
      end;
  end;
end;

function MakeConfigurationFileHigh(ACondition, ASessionBlocs: integer): string;
begin
  SetupStimuli;
  Result := NewConfigurationFile;

  ConfigurationFile.WriteToBloc(1, _Name, 'Mensagem do Teste de Derivação');
  WriteMSG1Trial(1);

  ConfigurationFile.WriteToBloc(2, _Name, 'Teste de Derivação (tentativas graduais)');
  WriteBloc(2);

  ConfigurationFile.WriteToBloc(3, _Name, 'Alta Derivação');
  WriteBloc(3, True);

  ConfigurationFile.WriteToBloc(4, _Name, 'Punição');
  WriteBloc(4);

  ConfigurationFile.WriteToBloc(5, _Name, 'Reforço');
  WriteBloc(5);

  ConfigurationFile.Invalidate;
  ConfigurationFile.UpdateFile;
end;

function MakeConfigurationFileLow(ACondition, ASessionBlocs: integer): string;
begin
  SetupStimuli;
  Result := NewConfigurationFile;

  ConfigurationFile.WriteToBloc(1, _Name, 'Mensagem do Teste de Derivação');
  WriteMSG1Trial(1);

  ConfigurationFile.WriteToBloc(2, _Name, 'Teste de Derivação (tentativas graduais)');
  WriteBloc(2);

  ConfigurationFile.WriteToBloc(3, _Name, 'Baixa Derivação');
  WriteBloc(3);

  ConfigurationFile.WriteToBloc(4, _Name, 'Punição');
  WriteBloc(4);

  ConfigurationFile.WriteToBloc(5, _Name, 'Reforço');
  WriteBloc(5);

  ConfigurationFile.Invalidate;
  ConfigurationFile.UpdateFile;
end;

procedure WriteToConfigurationFileHigh;
begin
  SetupStimuli;
  ConfigurationFile.WriteToBloc(11, _Name, 'Mensagem do Teste de Derivação');
  WriteMSG1Trial(11);

  ConfigurationFile.WriteToBloc(12, _Name, 'Teste de Derivação (tentativas graduais)');
  WriteBloc(12);

  ConfigurationFile.WriteToBloc(13, _Name, 'Alta Derivação');
  WriteBloc(13, True);

  ConfigurationFile.WriteToBloc(14, _Name, 'Punição');
  WriteBloc(14);

  ConfigurationFile.WriteToBloc(15, _Name, 'Reforço');
  WriteBloc(15);
end;

procedure WriteToConfigurationFileLow;
begin
  SetupStimuli;
  ConfigurationFile.WriteToBloc(11, _Name, 'Mensagem do Teste de Derivação');
  WriteMSG1Trial(11);

  ConfigurationFile.WriteToBloc(12, _Name, 'Teste de Derivação (tentativas graduais)');
  WriteBloc(12);

  ConfigurationFile.WriteToBloc(13, _Name, 'Baixa Derivação');
  WriteBloc(13);

  ConfigurationFile.WriteToBloc(14, _Name, 'Punição');
  WriteBloc(14);

  ConfigurationFile.WriteToBloc(14, _Name, 'Reforço');
  WriteBloc(15);
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


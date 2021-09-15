unit Experiments.Augusto.BeforeAfter;

{$mode objfpc}{$H+}

interface

function MakeConfigurationFile(ACondition, AOperandumDelay : integer) : string;

procedure WriteToConfigurationFile;

const
  SessionBlocs = 1;
  OperandumDelay = 1000;

  FolderPreTrainingBefoAfter =
    'treino-antes-depois'+DirectorySeparator;

  FolderTestBefoAfter =
    'teste-antes-depois'+DirectorySeparator;

implementation

uses Classes, SysUtils
   , Constants
   , FileMethods
   , LazFileUtils
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   , fgl
   ;

type
  TIntegerList = specialize TFPGList<integer>;

  TRandomStyle = (rsNone, BeforOnly, AfterOnly, MixedA, MixedB);
  TResponseStyle = (None, Before, After);
  TExpectedResponse = (Left, Right);
    TArrayOfInteger = array of integer;

  TTrial = record
    FilenameA : string;
    FilenameB : string;
    ContextLabel : string;
    ResponseStyle: TResponseStyle;
    ExpectedResponse: TExpectedResponse;
    ID : byte;
  end;

  TTrials = array of TTrial;
  TTrialsArray = array of TTrials;

var
  AlStimuli : TStringArray;

  TrialsOnlyAfterRight : TTrials;
  TrialsOnlyAfterLeft : TTrials;
  TrialsOnlyBeforeRight : TTrials;
  TrialsOnlyBeforeLeft : TTrials;

  MessageTraining : string;
  MessageTesting : string;
  ContextualBefor : string;
  ContextualAfter : string;
  //MessageBefore   : string;
  //MessageAfter    : string;

const
  LimitedHold = 0;
  ITI = 3000;

operator in (A:integer; B: TArrayOfInteger) : Boolean;
var
  i : integer;
begin
  Result := False;
  for i := Low(B) to High(B) do
    case B[i] of
    -1 : Exit;
    else
      if A = B[i] then
      begin
        Result := True;
        Exit;
      end
    end;
end;

procedure SetupStimuli(Training : Boolean);
var
  LDirectory : string;

  procedure MountTrialsFor(out ATrials: TTrials;
    AStimuli : TStringArray;
    AResponseStyle : TResponseStyle;
    AExpectedResponse : TExpectedResponse;
    AContextLabel : string;
    AID : byte);
  var
    LOldStimuli : TIntegerList;
    i, j : integer;
  begin
    ATrials := nil;
    LOldStimuli := TIntegerList.Create;

    i := Length(AStimuli);
    SetLength(ATrials, i);
    for i := Low(AStimuli) to High(AStimuli) do
      LOldStimuli.Add(i);

    for i := 0 to LOldStimuli.Count-1 do
    begin
      j := Random(Length(AStimuli));
      LOldStimuli.Exchange(i, j);
    end;

    for i := Low(AStimuli) to High(AStimuli) do
    begin
      ATrials[i].FilenameA := AStimuli[i];

      for j := 0 to LOldStimuli.Count-1 do
        if LOldStimuli[j] <> i then
        begin
          ATrials[i].FilenameB := AStimuli[LOldStimuli[j]];
          Break;
        end;
      LOldStimuli.Delete(j);

      //WriteLn(i, ' ', r);
      ATrials[i].ResponseStyle := AResponseStyle;
      ATrials[i].ExpectedResponse := AExpectedResponse;
      ATrials[i].ContextLabel := AContextLabel;
      ATrials[i].ID := AID;
    end;

    LOldStimuli.Free;
  end;


begin
  LoadMessageFromFile(MessageTraining, GlobalContainer.RootMedia+'MensagemTreinoAntesDepois.txt');
  //LoadMessageFromFile(MessageTesting, GlobalContainer.RootMedia+'MensagemTesteAntesDepois.txt');
  //LoadMessageFromFile(MessageBefore, GlobalContainer.RootMedia+'Mensagem-Antes.txt');
  //LoadMessageFromFile(MessageAfter, GlobalContainer.RootMedia+'Mensagem-Depois.txt');
  ContextualBefor := GlobalContainer.RootMedia+'Antes.jpg';
  ContextualAfter := GlobalContainer.RootMedia+'Depois.jpg';
  if not FileExistsUTF8(ContextualBefor) then
    Exception.Create(ContextualBefor + ' não encontrado.');

  if not FileExistsUTF8(ContextualAfter) then
    Exception.Create(ContextualAfter + ' não encontrado.');

  if Training then
    LDirectory := GlobalContainer.RootMedia+FolderPreTrainingBefoAfter
  else
    LDirectory := GlobalContainer.RootMedia+FolderTestBefoAfter;

  ForceDirectories(LDirectory);

  FindFilesFor(AlStimuli, LDirectory, '*.bmp;*.png;*.jpg');

  MountTrialsFor(TrialsOnlyBeforeRight, AlStimuli, Before, Right, ContextualBefor, 0);
  MountTrialsFor(TrialsOnlyBeforeLeft, AlStimuli, Before, Left, ContextualBefor, 1);
  MountTrialsFor(TrialsOnlyAfterRight, AlStimuli, After, Right, ContextualAfter, 2);
  MountTrialsFor(TrialsOnlyAfterLeft, AlStimuli, After, Left, ContextualAfter, 3);

  //ShowMessage(IntToStr(Length(TrialsOnlyAfterLeft)));
end;


procedure WriteTrainingMSGTrial(ABlc : integer);
var
  i : integer;
begin
  i := ConfigurationFile.TrialCount[1]+1;
  with ConfigurationFile do
  begin
    WriteToTrial(i, ABlc, _Name, 'Mensagem 1 - Treino');
    WriteToTrial(i, ABlc, _Cursor, '-1');
    WriteToTrial(i, ABlc, _Kind, T_MSG);
    WriteToTrial(i, ABlc, _ITI, ITI.ToString);
    WriteToTrial(i, ABlc, _Msg, MessageTraining)
  end;
end;
procedure WriteTestingMSGTrial(ABlc : integer);
var
  i : integer;
begin
  i := ConfigurationFile.TrialCount[1]+1;
  with ConfigurationFile do
  begin
    WriteToTrial(i, ABlc, _Name, 'Mensagem 1 - Teste');
    WriteToTrial(i, ABlc, _Cursor, '-1');
    WriteToTrial(i, ABlc, _Kind, T_MSG);
    WriteToTrial(i, ABlc, _ITI, ITI.ToString);
    WriteToTrial(i, ABlc, _Msg, MessageTesting)
  end;
end;

procedure WriteBloc(HasConsequence : Boolean; ABlc : integer);
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

  procedure WriteTrial(ATrial : TTrial);
  var
    i : integer;
    StimulusA, StimulusB,
     StimulusLabel, ResponseStyle, ExpectedResponse : string;
    function ExtractMediaName(AFilename: string): string;
    begin
      Result := AFilename;
      Delete(Result,1,Pos('media', Result)+Length('media'));
    end;

  begin
    i := ConfigurationFile.TrialCount[ABlc]+1;
    with ConfigurationFile do
    begin
      WriteStr(StimulusLabel, ATrial.ContextLabel);
      WriteStr(ResponseStyle, ATrial.ResponseStyle);
      WriteStr(ExpectedResponse, ATrial.ExpectedResponse);
      StimulusA := ExtractFileNameWithoutExt(ExtractFileName(ATrial.FilenameA));
      StimulusB := ExtractFileNameWithoutExt(ExtractFileName(ATrial.FilenameB));
      WriteToTrial(i, ABlc, _Name,
        ('').Join(#32,
          [StimulusA, StimulusLabel, StimulusB, ResponseStyle]));
      WriteToTrial(i, ABlc, _Cursor, '-1');
      WriteToTrial(i, ABlc, _Kind, T_BAT);
      WriteToTrial(i, ABlc, _Type, ResponseStyle);
      WriteToTrial(i, ABlc, _ExpectedResponse, ExpectedResponse);
      WriteToTrial(i, ABlc, _LimitedHold, LimitedHold.ToString);
      WriteToTrial(i, ABlc, 'HasInstruction', BoolToStr(False));
      WriteToTrial(i, ABlc, _Consequence, BoolToStr(HasConsequence,'1','0'));
      //case ResponseStyle of
      //  'Before'     : WriteToTrial(i, ABlc, _Msg, 'Before instruction');
      //  'After'      : WriteToTrial(i, ABlc, _Msg, 'After instruction');
      //end;
      WriteToTrial(i, ABlc, _ITI, ITI.ToString);
      WriteToTrial(i, ABlc, _Comp+'1'+_cStm, ExtractMediaName(ATrial.FilenameA));
      WriteToTrial(i, ABlc, _Comp+'2'+_cStm, ExtractMediaName(ATrial.FilenameB));
      WriteToTrial(i, ABlc, _Comp+'3'+_cStm, ATrial.ContextLabel);
    end;
  end;

  procedure RandomizeTrials(RandomStyle :  TRandomStyle);
  var
    //s : string;
    r, i, j : integer;
    T : TTrial;
    R2 : array [0..1] of TTrial;
    R4 : array [0..3] of TTrial;
  begin
    case RandomStyle of
      BeforOnly :
        begin
          for i := 0 to 3 do
          begin
            R2[0] := TrialsOnlyBeforeRight[i];
            R2[1] := TrialsOnlyBeforeLeft[i];

            // shuffle R2 trials
            for j := Low(R2) to High(R2) do
            begin
              r := Random(Length(R2));
              T := R2[r];
              R2[r] := R2[j];
              R2[j] := T;
            end;

            // write R2  trials
            for j := Low(R2) to High(R2) do
              WriteTrial(R2[j]);
          end;
        end;
      AfterOnly :
        for i := 0 to 3 do
        begin
          R2[0] := TrialsOnlyAfterRight[i];
          R2[1] := TrialsOnlyAfterLeft[i];
          for j := Low(R2) to High(R2) do
          begin
            r := Random(Length(R2));
            T := R2[r];
            R2[r] := R2[j];
            R2[j] := T;
          end;
          for j := Low(R2) to High(R2) do
            WriteTrial(R2[j]);
        end;
      MixedA :
        for i := 4 to 5 do
        begin
          R4[0] := TrialsOnlyBeforeRight[i];
          R4[1] := TrialsOnlyAfterLeft[i];
          R4[2] := TrialsOnlyBeforeLeft[i];
          R4[3] := TrialsOnlyAfterRight[i];

          r := Random(2);
          case r of
            0: begin
              T := R4[0];
              R4[0] := R4[2];
              R4[2] := T;
            end;
            1: begin
              T := R4[1];
              R4[1] := R4[3];
              R4[3] := T;
            end;
          end;
          for j := Low(R4) to High(R4) do
            WriteTrial(R4[j]);
        end;
      MixedB :
        begin
          for i := 0 to 1 do
          begin
            R2[0] := TrialsOnlyBeforeRight[i];
            R2[1] := TrialsOnlyBeforeLeft[i];

            // shuffle R2 trials
            for j := Low(R2) to High(R2) do
            begin
              r := Random(Length(R2));
              T := R2[r];
              R2[r] := R2[j];
              R2[j] := T;
            end;

            for j := Low(R2) to High(R2) do
              WriteTrial(R2[j]);
          end;

          for i := 0 to 1 do
          begin
            R2[0] := TrialsOnlyAfterRight[i];
            R2[1] := TrialsOnlyAfterLeft[i];
            for j := Low(R2) to High(R2) do
            begin
              r := Random(Length(R2));
              T := R2[r];
              R2[r] := R2[j];
              R2[j] := T;
            end;
            for j := Low(R2) to High(R2) do
              WriteTrial(R2[j]);
          end;

        for i := 6 to 7 do
        begin
          R4[0] := TrialsOnlyBeforeRight[i];
          R4[1] := TrialsOnlyAfterLeft[i];
          R4[2] := TrialsOnlyAfterRight[i];
          R4[3] := TrialsOnlyBeforeLeft[i];

          r := Random(2);
          case r of
            0: begin
              T := R4[0];
              R4[0] := R4[2];
              R4[2] := T;
            end;
            1: begin
              T := R4[1];
              R4[1] := R4[3];
              R4[3] := T;
            end;
          end;
          for j := Low(R4) to High(R4) do
            WriteTrial(R4[j]);
        end;

        end;
    end;
  end;
  //////////////////////////// WRITE BLOC /////////////////////////////
var
  r, i  : integer;
  T : TTrial;
begin
  for i := Low(TrialsOnlyBeforeRight) to High(TrialsOnlyBeforeRight) do
  begin
    r := Random(Length(TrialsOnlyBeforeRight));

    T := TrialsOnlyBeforeRight[r];
    TrialsOnlyBeforeRight[r] := TrialsOnlyBeforeRight[i];
    TrialsOnlyBeforeRight[i] := T;

    T := TrialsOnlyBeforeLeft[r];
    TrialsOnlyBeforeLeft[r] := TrialsOnlyBeforeLeft[i];
    TrialsOnlyBeforeLeft[i] := T;
  end;

  for i := Low(TrialsOnlyAfterRight) to High(TrialsOnlyAfterRight) do
  begin
    r := Random(Length(TrialsOnlyAfterRight));

    T := TrialsOnlyAfterRight[r];
    TrialsOnlyAfterRight[r] := TrialsOnlyAfterRight[i];
    TrialsOnlyAfterRight[i] := T;

    T := TrialsOnlyAfterLeft[r];
    TrialsOnlyAfterLeft[r] := TrialsOnlyAfterLeft[i];
    TrialsOnlyAfterLeft[i] := T;
  end;
  if HasConsequence then
  begin
    case ABlc of
      2 : RandomizeTrials(BeforOnly);
      3 : RandomizeTrials(AfterOnly);
      4 : RandomizeTrials(MixedA);
      5 : RandomizeTrials(MixedB);
    end;
  end else
  begin
    RandomizeTrials(MixedB);
    RandomizeTrials(MixedB);
  end;
  //WriteReviewTrial;
end;

function MakeConfigurationFile(ACondition, AOperandumDelay: integer): string;
var
  i: Integer;
begin
  Result := NewConfigurationFile;
  case ACondition of
    0 :
      begin
        SetupStimuli(True);
        ConfigurationFile.WriteToBloc(1, _Name, 'Mensagem de treino antes-depois');
        WriteTrainingMSGTrial(1);
        for i := 2 to 5 do
        begin
          ConfigurationFile.WriteToBloc(i, _Name, 'Treino Antes-Depois'+i.ToString);
          case i of
          2..4 : ConfigurationFile.WriteToBloc(i, _CrtConsecutiveHit, '4');
          5 : ConfigurationFile.WriteToBloc(i, _CrtHitValue, '14');
          end;
          if i = 1 then WriteTrainingMSGTrial(i);
          WriteBloc(True, i);
        end;

      end;
    1 :
      begin
        // testing bloc
        SetupStimuli(False);
        ConfigurationFile.WriteToBloc(1, _Name, 'Teste Antes-Depois');
        ConfigurationFile.WriteToBloc(1, _CrtHitValue, '30');
        ConfigurationFile.WriteToBloc(1, _MaxBlcRepetition, '2');
        WriteBloc(False, 1);
      end;
  end;
  ConfigurationFile.Invalidate;
  ConfigurationFile.UpdateFile;
end;

procedure WriteToConfigurationFile;
var
  i : integer;
begin
  // training
  SetupStimuli(True);
  ConfigurationFile.WriteToBloc(1, _Name, 'Mensagem de treino antes-depois');
  WriteTrainingMSGTrial(1);
  for i := 2 to 5 do
  begin
    ConfigurationFile.WriteToBloc(i, _Name, 'Treino Antes-Depois'+i.ToString);
    case i of
    2..4 : ConfigurationFile.WriteToBloc(i, _CrtConsecutiveHit, '4');
    5 : ConfigurationFile.WriteToBloc(i, _CrtHitValue, '14');
    end;
    WriteBloc(True, i);
  end;

  // testing bloc
  Inc(i);
  SetupStimuli(False);
  ConfigurationFile.WriteToBloc(i, _Name, 'Teste Antes-Depois');
  ConfigurationFile.WriteToBloc(i, _CrtHitValue, '30');
  ConfigurationFile.WriteToBloc(i, _MaxBlcRepetition, '2');
  WriteBloc(False, i);
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

unit Experiments.Augusto.SameDifferent;

{$mode objfpc}{$H+}

interface

function MakeConfigurationFile(ACondition, AOperandumDelay : integer) : string;

procedure WriteToConfigurationFile;

const
  FolderPreTrainingEqualDiff =
   'treino-igual-diferente'+DirectorySeparator;

  FolderTestEqualDiff =
   'teste-igual-diferente'+DirectorySeparator;

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
  TRandomStyle = (rsNone, EqualOnly, DiffeOnly, MixedA, MixedB);
  TArrayOfInteger = array of integer;
  TResponseStyle = (None, Equal, Different);
  TExpectedResponse = (Left, Right);

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

  TrialsOnlyEqualRight : TTrials;
  TrialsOnlyEqualLeft : TTrials;
  TrialsOnlyDifferentRight : TTrials;
  TrialsOnlyDifferentLeft : TTrials;

  MessageTraining : string;
  MessageTesting : string;
  ContextualEqual : string;
  ContextualDiffe : string;

const
  LimitedHold = 0;
  ITI = 3000;

procedure RandomizeStimuli(var AStimuli : array of integer);
var
  i, t, r, l : integer;
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
    i := Length(AStimuli);
    SetLength(ATrials, i);

    LOldStimuli := TIntegerList.Create;
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
      ATrials[i].ResponseStyle := AResponseStyle;
      ATrials[i].ExpectedResponse := AExpectedResponse;
      ATrials[i].ContextLabel := AContextLabel;
      ATrials[i].ID := AID;
    end;
    LOldStimuli.Free;
  end;

begin
  LoadMessageFromFile(MessageTraining, GlobalContainer.RootMedia+'MensagemTreinoIgualDiferente.txt');
  //LoadMessageFromFile(MessageTesting, GlobalContainer.RootMedia+'MensagemTesteIgualDiferente.txt');
  ContextualEqual := GlobalContainer.RootMedia+'Igual.jpg';
  ContextualDiffe := GlobalContainer.RootMedia+'Diferente.jpg';
  if not FileExistsUTF8(ContextualEqual) then
    Exception.Create(ContextualEqual + ' não encontrado.');
  if not FileExistsUTF8(ContextualDiffe) then
    Exception.Create(ContextualDiffe + ' não encontrado.');

  if Training then
    LDirectory := GlobalContainer.RootMedia+FolderPreTrainingEqualDiff
  else
    LDirectory := GlobalContainer.RootMedia+FolderTestEqualDiff;

  ForceDirectories(LDirectory);
  FindFilesFor(AlStimuli, LDirectory, '*.bmp;*.png;*.jpg');

  MountTrialsFor(TrialsOnlyEqualRight, AlStimuli, Equal, Right, ContextualEqual, 0);
  MountTrialsFor(TrialsOnlyEqualLeft, AlStimuli, Equal, Left, ContextualEqual, 1);
  MountTrialsFor(TrialsOnlyDifferentRight, AlStimuli, Different, Right, ContextualDiffe, 2);
  MountTrialsFor(TrialsOnlyDifferentLeft, AlStimuli, Different, Left, ContextualDiffe, 3);
end;

procedure WriteTrainingMSGTrial(ABlc : integer);
var
  i : integer;
begin
  with ConfigurationFile do
  begin
    WriteToTrial(1, ABlc, _Name, 'Mensagem Training');
    WriteToTrial(1, ABlc, _Cursor, '-1');
    WriteToTrial(1, ABlc, _Kind, T_MSG);
    WriteToTrial(1, ABlc, _ITI, ITI.ToString);
    WriteToTrial(1, ABlc, _Msg, MessageTraining)
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

  procedure WritePreTrainingTrial(ATrial : TTrial);
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
      WriteToTrial(i, ABlc, _Kind, T_CTX);
      WriteToTrial(i, ABlc, _Type, ResponseStyle);
      WriteToTrial(i, ABlc, _Consequence, BoolToStr(HasConsequence,'1','0'));
      WriteToTrial(i, ABlc, _ExpectedResponse, ExpectedResponse);
      WriteToTrial(i, ABlc, _LimitedHold, LimitedHold.ToString);
      WriteToTrial(i, ABlc, _ITI, ITI.ToString);
      WriteToTrial(i, ABlc, _Comp+'1'+_cStm, ExtractMediaName(ATrial.FilenameA));
      WriteToTrial(i, ABlc, _Comp+'2'+_cStm, ExtractMediaName(ATrial.FilenameB));
      WriteToTrial(i, ABlc, _Comp+'3'+_cStm, ATrial.ContextLabel);
    end;
  end;

  procedure RandomizeTrials(RandomStyle :  TRandomStyle);
  var
    r, i, j : integer;
    T : TTrial;
    R2 : array [0..1] of TTrial;
    R4 : array [0..3] of TTrial;
  begin
    case RandomStyle of
      EqualOnly :
        begin
          for i := 0 to 1 do
          begin
            R2[0] := TrialsOnlyEqualRight[i];
            R2[1] := TrialsOnlyEqualLeft[i];
            for j := Low(R2) to High(R2) do
            begin
              r := Random(Length(R2));
              T := R2[r];
              R2[r] := R2[j];
              R2[j] := T;
            end;
            for j := Low(R2) to High(R2) do
              WritePreTrainingTrial(R2[j]);
          end;
        end;
      DiffeOnly :
        for i := 2 to 3 do
        begin
          R2[0] := TrialsOnlyDifferentRight[i];
          R2[1] := TrialsOnlyDifferentLeft[i];
          for j := Low(R2) to High(R2) do
          begin
            r := Random(Length(R2));
            T := R2[r];
            R2[r] := R2[j];
            R2[j] := T;
          end;
          for j := Low(R2) to High(R2) do
            WritePreTrainingTrial(R2[j]);
        end;

      MixedA :
        for i := 4 to 5 do
        begin
          R4[0] := TrialsOnlyEqualRight[i];
          R4[1] := TrialsOnlyEqualLeft[i];
          R4[2] := TrialsOnlyDifferentRight[i];
          R4[3] := TrialsOnlyDifferentLeft[i];
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
            WritePreTrainingTrial(R4[j]);

          //for j := Low(R4) to High(R4) do
          //begin
          //  r := Random(Length(R4));
          //  T := R4[r];
          //  R4[r] := R4[j];
          //  R4[j] := T;
          //end;
          //for j := Low(R4) to High(R4) do
          //  WritePreTrainingTrial(R4[j]);
        end;
      MixedB :
        for i := 4 to 7 do
        begin
          R4[0] := TrialsOnlyEqualRight[i];
          R4[1] := TrialsOnlyEqualLeft[i];
          R4[2] := TrialsOnlyDifferentRight[i];
          R4[3] := TrialsOnlyDifferentLeft[i];
          for j := Low(R4) to High(R4) do
          begin
            r := Random(Length(R4));
            T := R4[r];
            R4[r] := R4[j];
            R4[j] := T;
          end;
          for j := Low(R4) to High(R4) do
            WritePreTrainingTrial(R4[j]);
        end;
    end;
  end;
var
  r, i  : integer;
  T : TTrial;
begin
  // randomize all 4 trial types
  for i := Low(TrialsOnlyEqualRight) to High(TrialsOnlyEqualRight) do
  begin
    r := Random(Length(TrialsOnlyEqualRight));

    T := TrialsOnlyEqualRight[r];
    TrialsOnlyEqualRight[r] := TrialsOnlyEqualRight[i];
    TrialsOnlyEqualRight[i] := T;

    T := TrialsOnlyEqualLeft[r];
    TrialsOnlyEqualLeft[r] := TrialsOnlyEqualLeft[i];
    TrialsOnlyEqualLeft[i] := T;
  end;

  for i := Low(TrialsOnlyDifferentRight) to High(TrialsOnlyDifferentRight) do
  begin
    r := Random(Length(TrialsOnlyDifferentRight));

    T := TrialsOnlyDifferentRight[r];
    TrialsOnlyDifferentRight[r] := TrialsOnlyDifferentRight[i];
    TrialsOnlyDifferentRight[i] := T;

    T := TrialsOnlyDifferentLeft[r];
    TrialsOnlyDifferentLeft[r] := TrialsOnlyDifferentLeft[i];
    TrialsOnlyDifferentLeft[i] := T;
  end;
  if HasConsequence then
  begin
    case ABlc of
      1, 8:
        begin
          RandomizeTrials(EqualOnly);
          RandomizeTrials(DiffeOnly);
        end;
      2, 9:  RandomizeTrials(MixedA);
      //3, 10: RandomizeTrials(MixedB);
    end;
  end else
  begin
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
          for i := 1 to 4 do
          begin
            ConfigurationFile.WriteToBloc(i, _Name, 'Treino Igual-Diferente'+i.ToString);
            case i of
            1..3 : ConfigurationFile.WriteToBloc(i, _CrtHitValue, '7');
            4 : ConfigurationFile.WriteToBloc(i, _CrtHitValue, '14');
            end;
            if i = 1 then WriteTrainingMSGTrial(i);
            WriteBloc(True, i);
          end;

        end;
      1 :
        begin
          SetupStimuli(False);
          ConfigurationFile.WriteToBloc(1, _Name, 'Teste Igual-Diferente');
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

  SetupStimuli(True);
  ConfigurationFile.WriteToBloc(7, _Name, 'Mensagem do treino igual-Diferente');
  WriteTrainingMSGTrial(7);

  for i := 8 to 9 do
  begin
    ConfigurationFile.WriteToBloc(i, _Name, 'Treino Igual-Diferente'+i.ToString);
    ConfigurationFile.WriteToBloc(i, _CrtHitValue, '7');
    //ConfigurationFile.WriteToBloc(i, _CrtHitValue, '14');
    WriteBloc(True, i);
  end;

  // testing bloc
  Inc(i);
  SetupStimuli(False);
  ConfigurationFile.WriteToBloc(i, _Name, 'Teste Igual-Diferente');
  ConfigurationFile.WriteToBloc(i, _CrtHitValue, '14');
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

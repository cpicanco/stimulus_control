unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TBackground }

  TBackground = class(TForm)
    ButtonStart: TButton;
    EditParticipant: TEdit;
    OpenDialog: TOpenDialog;
    RadioGroupCondition: TRadioGroup;
    InterTrial: TTimer;
    procedure ButtonStartClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure InterTrialStopTimer(Sender: TObject);
    procedure InterTrialTimer(Sender: TObject);
    procedure RadioGroupConditionSelectionChanged(Sender: TObject);
    procedure TrialEnd(Sender: TObject);
  private
    FHeader : string;
    procedure Play;
    procedure ShowTrialConsole;
  public
    {$IFDEF WINDOWS}
    OriginalBounds: TRect;
    OriginalWindowState: TWindowState;
    ScreenBounds: TRect;
    procedure SwitchFullScreen;
    {$ENDIF}
  end;

resourcestring
  HSUBJECT_NAME      = 'Nome_Sujeito:';
  HSESSION_NAME      = 'Nome_Sessão:';
  HFIRST_TIMESTAMP   = 'Primeira_Timestamp:';
  HBEGIN_TIME        = 'Início:';
  HEND_TIME          = 'Término:';
//  HSESSION_CANCELED  = '----------Sessão Cancelada----------';
  HTEST_MODE         = '(Modo de Teste)';

var
  Background: TBackground;

implementation

{$R *.lfm}

uses FileUtil
   , Constants
   , Timestamps
   , Loggers.Reports
   , Controls.Trials.Abstract
   , Controls.Trials.Likert
   , Controls.Trials.MatchingToSample
   , Controls.Trials.TextMessage
   , Session.Configuration
   , Session.ConfigurationFile
   , Session.Configuration.GlobalContainer
   ;

{ TBackground }

type
  TStmPair = record
    A : string;
    B : string;
  end;

var
  FDataFile : string;
  FSessionFile : string;
  Trial : TTrial;
  ConfigurationFile : TConfigurationFile;
  BndCenter : string = '234 533 300 300';
  BndSample : string = '15 533 300 300';
  BndComparisons : array [0..1] of string = ('400 333 300 300', '400 733 300 300');

function AppExt(Stm : string) : string;
begin
  Result := Stm + '.png';
end;

procedure NewConfigurationFile;
begin
  FSessionFile := ExtractFilePath(
    Application.ExeName) + DirectorySeparator + 'last_session.ini';
  if FileExists(FSessionFile) then
  begin
    DeleteFile(FSessionFile);
  end;
  ConfigurationFile := TConfigurationFile.Create(FSessionFile);
  ConfigurationFile.CacheUpdates := True;
  ConfigurationFile.WriteString(_Main, _NumBlc, '2');
  ConfigurationFile.WriteToBloc(1, _Name, 'Memory Test - SS');
  ConfigurationFile.Invalidate;
end;

procedure ClearConfigurationFile;
begin
  ConfigurationFile.Free;
  NewConfigurationFile;
end;

procedure RandomizeStimuli;
var
  StimuliList : TStringList;
  i : integer;
  SourcePath : string;
  DestinPath : string;
  SrcFile : string;
  DstFile : string;
  Stimuli : array [0..17] of string = (
  '¶', '@', '$', '€', '&', '#', '%', '≡', '∞',
  '∑', '↔', 'ℓ', 'β', 'Δ', 'π', 'Φ', 'Ψ', 'Ω');
  DefaultStimuliNames : array [0..5] of string =
    ('A1', 'A2', 'B1', 'B2', 'C1', 'C2');
begin
  SourcePath := GlobalContainer.RootMedia+'PNG'+DirectorySeparator;
  DestinPath := GlobalContainer.RootMedia;
  StimuliList := TStringList.Create;
  try
    for i := Low(Stimuli) to High(Stimuli) do StimuliList.Append(Stimuli[i]);
    for i := Low(Stimuli) to High(Stimuli) do
      StimuliList.Exchange(Random(Length(Stimuli)), i);

    for i := Low(DefaultStimuliNames) to High(DefaultStimuliNames) do
    begin
      SrcFile := SourcePath + AppExt(StimuliList[i]);
      DstFile := DestinPath + AppExt(DefaultStimuliNames[i]);
      ConfigurationFile.WriteString(
        _Main, DefaultStimuliNames[i], StimuliList[i]);
      CopyFile(SrcFile, DstFile);
    end;
  finally
    StimuliList.Free;
  end;
end;

procedure MakeConfigurationFile(ACondition : integer);
{
  Consistentes: B1A1, B2A2, C1A1, C2A2, B1C1, B2C2, C1B1, C2B2.
  Inconsistentes: B1A2, B2A1, C1A2, C2A1, B1C2, B2C1, C1B2, C2B1.
}
var
  ConsistentPairs : array [0..7] of TStmPair;
  InconsistePairs : array [0..7] of TStmPair;
  AllPairs : array [0..15] of TStmPair;
  r, t : integer;
  StmPair : TStmPair;
  procedure WriteMSGTrial(AMsg : integer);
  var
    M1 : string =
    'Seja bem-vindo(a). ' +
    'Durante esta fase serão apresentados alguns símbolos. ' +
    'VOCÊ DEVERÁ PRESTAR MUITA ATENÇÃO NOS SÍMBOLOS. ' +
    'Esta fase durará alguns minutos. ' +
    'Pressione a BARRA DE ESPAÇO no teclado para começar.';

    M2 : string =
    'Agora, nesta parte, você verá um símbolo na parte superior da tela e dois ' +
    'símbolos na parte inferior da tela. Você DEVERÁ ESCOLHER ENTRE ' +
    'OS DOIS SIMBOLOS QUE ESTÃO NA PARTE INFEIROR DA TELA. ' +
    'PARA ISSO VOCÊ DEVE CLICAR SOBRE UM DOS DOIS. Pressione ' +
    'a BARRA DE ESPAÇO no teclado para começar.';

    M3 : string =
    'Agora, nesta fase, você verá uma figura na tela que junta dois símbolos. ' +
    'Abaixo dessa figura haverá uma escala com os itens: ' + LineEnding + LineEnding +
    '"Tenho certeza que não vi, Acho que não vi, Não sei, Acho que vi, Tenho certeza que vi". ' + LineEnding + LineEnding +
    'Você DEVERÁ CLICAR SOBRE UM desses itens para indicar o quanto você lembra ' +
    'de ter visto os símbolos da figura na fase anterior, ' +
    'independente da ordem em que os símbolos aparecem. ' +
    'Pressione a BARRA DE ESPAÇO no teclado para começar.';
  i : integer;
  begin
    i := ConfigurationFile.TrialCount[1]+1;
    with ConfigurationFile do
    begin
      case AMsg of
        0 : WriteToTrial(i, _Name, 'Pareamento - MSG');
        1 : WriteToTrial(i, _Name, 'Teste MTS - MSG');
        2 : WriteToTrial(i, _Name, 'Teste de Memória - MSG');
      end;
      WriteToTrial(i, _Kind, T_MSG);
      WriteToTrial(i, _ITI, '3000');
      case AMsg of
        0 : WriteToTrial(i, _Msg, M1);
        1 : WriteToTrial(i, _Msg, M2);
        2 : WriteToTrial(i, _Msg, M3);
      end;
    end;
  end;

  procedure WriteSSBloc;
  var
    i, j : integer;
    SSTrials : array [0..5, 0..3] of integer;
    procedure WriteSSTrial(AConPair : TStmPair);
    var
      Comparisons : TStringList;
      i : integer;
    begin
      Comparisons := TStringList.Create;
      Comparisons.Append(BndComparisons[0]);
      Comparisons.Append(BndComparisons[1]);
      for i := 0 to Comparisons.Count -1 do
        Comparisons.Exchange(Random(2), i);

      i := ConfigurationFile.TrialCount[1]+1;
      with ConfigurationFile do
      begin
        WriteToTrial(i, _Name, 'Pareamento - '+AConPair.B+' '+AConPair.A);
        WriteToTrial(i, _Cursor, '-1');
        WriteToTrial(i, _Kind, T_MTS);
        WriteToTrial(i, _ITI, '3000');
        WriteToTrial(i, _SampleType, BoolToStr(True));
        WriteToTrial(i, _Delay, '500');

        WriteToTrial(i, _Samp+_cStm, AppExt(AConPair.B));
        WriteToTrial(i, _Samp+_cBnd, BndCenter);
        WriteToTrial(i, _Samp+_cSch, 'FT 1500');
        WriteToTrial(i, _Samp+_cMsg, AConPair.B + '-' + BndCenter);

        WriteToTrial(i, _NumComp, '1');
        WriteToTrial(i, _Comp+'1'+_cStm, AppExt(AConPair.A));
        WriteToTrial(i, _Comp+'1'+_cBnd, BndCenter);
        WriteToTrial(i, _Comp+'1'+_cSch, 'FT 1500');
        WriteToTrial(i, _Comp+'1'+_cMsg, AConPair.A + '-' + BndCenter);
      end;
    end;
  begin
    // SS trials
    WriteMSGTrial(0);
    for i := Low(SSTrials) to High(SSTrials) do
      for j := Low(SSTrials[i]) to High(SSTrials[i]) do
        SSTrials[i, j] := j;

    for i := Low(SSTrials) to High(SSTrials) do
      for j := Low(SSTrials[i]) to High(SSTrials[i]) do
        begin
          r := Random(Length(SSTrials[i]));
          t := SSTrials[i, r];
          SSTrials[i, r] := SSTrials[i, j];
          SSTrials[i, j] := t;
        end;

    for i := Low(SSTrials) to High(SSTrials) do
      begin
        for j := Low(SSTrials[i]) to High(SSTrials[i]) do
        begin
          WriteSSTrial(ConsistentPairs[SSTrials[i, j]]);
          //WriteLn(ConsistentPairs[SSTrials[i, j]].B + ' - '+
          //ConsistentPairs[SSTrials[i, j]].A);

        end;
        //WriteLn('');
      end;
  end;

  procedure WriteMTSBloc;
  var
    i, j : integer;
    MTSTrials : array [0..2, 0..7] of integer;
    procedure WriteMTSTrial(AConPair, AIncPair : TStmPair);
    var
      Comparisons : TStringList;
      i : integer;
    begin
      Comparisons := TStringList.Create;
      Comparisons.Append(BndComparisons[0]);
      Comparisons.Append(BndComparisons[1]);
      for i := 0 to Comparisons.Count -1 do
        Comparisons.Exchange(Random(2), i);

      i := ConfigurationFile.TrialCount[1]+1;
      with ConfigurationFile do
      begin
        WriteToTrial(i, _Name, 'Teste MTS - '+AConPair.A+' '+AConPair.B);
        WriteToTrial(i, _Kind, T_MTS);
        WriteToTrial(i, _ITI, '1000');
        WriteToTrial(i, _SampleType, BoolToStr(False));
        WriteToTrial(i, _Delay, '0');

        WriteToTrial(i, _Samp+_cStm, AppExt(AConPair.A));
        WriteToTrial(i, _Samp+_cBnd, BndSample);
        WriteToTrial(i, _Samp+_cSch, 'FT 1');
        WriteToTrial(i, _Samp+_cMsg, AConPair.A);

        WriteToTrial(i, _NumComp, '2');
        WriteToTrial(i, _Comp+'1'+_cStm, AppExt(AConPair.B));
        WriteToTrial(i, _Comp+'1'+_cBnd, Comparisons[0]);
        WriteToTrial(i, _Comp+'1'+_cSch, 'CRF');
        WriteToTrial(i, _Comp+'1'+_cMsg, AConPair.B + '-' + Comparisons[0]);

        WriteToTrial(i, _Comp+'2'+_cStm, AppExt(AIncPair.B));
        WriteToTrial(i, _Comp+'2'+_cBnd, Comparisons[1]);
        WriteToTrial(i, _Comp+'2'+_cSch, 'CRF');
        WriteToTrial(i, _Comp+'2'+_cMsg, AIncPair.B + '-' + Comparisons[1]);
      end;
    end;
  begin
    // MTS Trials
    WriteMSGTrial(1);
    for i := Low(MTSTrials) to High(MTSTrials) do
      for j := Low(MTSTrials[i]) to High(MTSTrials[i]) do
        MTSTrials[i, j] := j;

    for i := Low(MTSTrials) to High(MTSTrials) do
      for j := Low(MTSTrials[i]) to High(MTSTrials[i]) do
        begin
          r := Random(Length(MTSTrials[i]));
          t := MTSTrials[i, r];
          MTSTrials[i, r] := MTSTrials[i, j];
          MTSTrials[i, j] := t;
        end;

    for i := Low(MTSTrials) to High(MTSTrials) do
    begin
      for j := Low(MTSTrials[i]) to High(MTSTrials[i]) do
      begin
        WriteMTSTrial(
          ConsistentPairs[MTSTrials[i,j]], InconsistePairs[MTSTrials[i,j]]);
        //WriteLn(
        //  ConsistentPairs[MTSTrials[i,j]].A, ' - ',
        //  ConsistentPairs[MTSTrials[i,j]].B);
      end;
      //WriteLn('');
    end;
  end;

  procedure WriteLikertBloc;
  var
    i : integer;
  begin
    // MemoryTest
    WriteMSGTrial(2);
    for i := Low(AllPairs) to High(AllPairs) do
    case i of
      0..7 : AllPairs[i] := ConsistentPairs[i];
      8..15: AllPairs[i] := InconsistePairs[i-8];
    end;

    for i := Low(AllPairs) to High(AllPairs) do
    begin
      r := Random(16);
      StmPair := AllPairs[r];
      AllPairs[r] := AllPairs[i];
      AllPairs[i] := StmPair;
    end;

    with ConfigurationFile do
      for i := Low(AllPairs) to High(AllPairs) do
      begin
        t := ConfigurationFile.TrialCount[1] + 1;
        WriteToTrial(t, _Name, 'Teste de Memória - '+AllPairs[i].A+' '+AllPairs[i].B);
        WriteToTrial(t, _Kind, T_LIK);
        WriteToTrial(t, _ITI, '3000');
        WriteToTrial(t, _Left, AppExt(AllPairs[i].A));
        WriteToTrial(t, _Right, AppExt(AllPairs[i].B));
      end;
  end;

begin
  ConsistentPairs[0].A := 'B1'; ConsistentPairs[0].B := 'A1';
  ConsistentPairs[1].A := 'B2'; ConsistentPairs[1].B := 'A2';
  ConsistentPairs[2].A := 'C1'; ConsistentPairs[2].B := 'A1';
  ConsistentPairs[3].A := 'C2'; ConsistentPairs[3].B := 'A2';
  ConsistentPairs[4].A := 'B1'; ConsistentPairs[4].B := 'C1';
  ConsistentPairs[5].A := 'B2'; ConsistentPairs[5].B := 'C2';
  ConsistentPairs[6].A := 'C1'; ConsistentPairs[6].B := 'B1';
  ConsistentPairs[7].A := 'C2'; ConsistentPairs[7].B := 'B2';

  InconsistePairs[0].A := 'B1'; InconsistePairs[0].B := 'A2';
  InconsistePairs[1].A := 'B2'; InconsistePairs[1].B := 'A1';
  InconsistePairs[2].A := 'C1'; InconsistePairs[2].B := 'A2';
  InconsistePairs[3].A := 'C2'; InconsistePairs[3].B := 'A1';
  InconsistePairs[4].A := 'B1'; InconsistePairs[4].B := 'C2';
  InconsistePairs[5].A := 'B2'; InconsistePairs[5].B := 'C1';
  InconsistePairs[6].A := 'C1'; InconsistePairs[6].B := 'B2';
  InconsistePairs[7].A := 'C2'; InconsistePairs[7].B := 'B1';

  case ACondition of
    0 :
      begin
        ConfigurationFile.WriteToBloc(1, _Name, 'Avaliação de Memória');
        WriteSSBloc;
        WriteLikertBloc;

        WriteSSBloc;
        WriteSSBloc;
        WriteSSBloc;

        WriteSSBloc;
        WriteLikertBloc;
      end;
    1 :
      begin
        ConfigurationFile.WriteToBloc(1, _Name, 'Avaliação MTS');
        WriteSSBloc;
        WriteLikertBloc;

        WriteSSBloc;
        WriteMTSBloc;

        WriteSSBloc;
        WriteMTSBloc;

        WriteSSBloc;
        WriteMTSBloc;

        WriteSSBloc;
        WriteLikertBloc;
      end;

  end;
  ConfigurationFile.Invalidate;
  ConfigurationFile.UpdateFile;
end;

procedure TBackground.ButtonStartClick(Sender: TObject);
var
  SessionName : string;
begin
  ButtonStart.Hide;
  RadioGroupCondition.Hide;
  EditParticipant.Hide;
  {$IFDEF WINDOWS}SwitchFullScreen;{$ENDIF}

  SessionName := RadioGroupCondition.Items[RadioGroupCondition.ItemIndex];
  FDataFile := '000';
  FHeader := HSUBJECT_NAME + #9 + EditParticipant.Text + LineEnding +
             HSESSION_NAME + #9 + SessionName + LineEnding +
             HBEGIN_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time) + LineEnding;

  FDataFile := GlobalContainer.RootData + DirectorySeparator + FDataFile;
  CreateLogger(LGTimestamps, FDataFile, FHeader);
  GlobalContainer.CounterManager.CurrentTrial:=27;
  Play;
end;

procedure TBackground.FormKeyPress(Sender: TObject; var Key: char);
begin
  case key of
    't' : ShowTrialConsole;
  end;
end;

procedure TBackground.InterTrialStopTimer(Sender: TObject);
var
  CurrentTrial : integer;
  TotalTrials : integer;
  Footer : string;
  MouseOnSet : TPoint = (x:683; y :165);
begin
  TotalTrials := ConfigurationFile.TrialCount[1]-1;
  CurrentTrial := GlobalContainer.CounterManager.CurrentTrial;
  GlobalContainer.CounterManager.CurrentTrial:= CurrentTrial+1;

  if CurrentTrial > TotalTrials-1 then
  begin
    ShowMessage('Fim.');
    Footer := HEND_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time)+ LineEnding;
    FreeLogger(LGTimestamps,Footer);
    CopyFile(FSessionFile, ExtractFileNameWithoutExt(FDataFile)+'.ini');
    WindowState := wsNormal;
    Exit;
  end;
  Mouse.CursorPos := MouseOnSet;
  Play;
end;

procedure TBackground.InterTrialTimer(Sender: TObject);
begin
  InterTrial.Enabled := False;
end;

procedure TBackground.RadioGroupConditionSelectionChanged(Sender: TObject);
begin
  ClearConfigurationFile;
  MakeConfigurationFile(RadioGroupCondition.ItemIndex);
end;

procedure TBackground.TrialEnd(Sender: TObject);
begin
  InterTrial.Enabled := True;
end;

procedure TBackground.Play;
var
  IndTrial: Integer;
  TrialConfig : TCfgTrial;
begin
  if Assigned(Trial) then FreeAndNil(Trial);
  IndTrial := GlobalContainer.CounterManager.CurrentTrial;
  TrialConfig := ConfigurationFile.Trial[1, IndTrial+1];
  try
    InterTrial.Interval := StrToIntDef(TrialConfig.SList.Values[_ITI], 0);
    case TrialConfig.Kind of
      T_MSG : Trial := TMessageTrial.Create(Self);
      T_MTS : Trial := TMTS.Create(Self);
      T_LIK : Trial := TLikert.Create(Self);
    end;

    Trial.SaveData := GetSaveDataProc(LGTimestamps);
    if GlobalContainer.CounterManager.Trials = 0 then
    begin
      Trial.SaveData(HFIRST_TIMESTAMP + #9 + TimestampToStr(TickCount) + LineEnding + LineEnding);
      Trial.SaveData(Trial.HeaderTimestamps + LineEnding);
    end;

    Trial.Configurations := TrialConfig;
    Trial.OnTrialEnd := @TrialEnd;
    Trial.Play;
    Cursor := Trial.Cursor;
  finally
    TrialConfig.SList.Free;
  end;
end;

procedure TBackground.ShowTrialConsole;
var
  LNextTrial : string;
  LNextTrialI : integer;
begin
  LNextTrial := InputBox('Trial Console', 'Insert the Next Trial', '-');
  LNextTrialI := StrToIntDef(LNextTrial, -1);
  if LNextTrialI <> -1 then
  begin
    while Trial = nil do Application.ProcessMessages;
    GlobalContainer.CounterManager.CurrentTrial := LNextTrialI;
  end;
end;

{$IFDEF WINDOWS}
// http://wiki.freepascal.org/Application_full_screen_mode
procedure TBackground.SwitchFullScreen;
begin
  if BorderStyle <> bsNone then begin
    // To full screen
    OriginalWindowState := WindowState;
    OriginalBounds := BoundsRect;

    BorderStyle := bsNone;
    BoundsRect := Screen.MonitorFromWindow(Handle).BoundsRect;
  end else begin
    // From full screen
    BorderStyle := bsSizeable;
    if OriginalWindowState = wsMaximized then
      WindowState := wsMaximized
    else
      BoundsRect := OriginalBounds;
  end;
end;
{$ENDIF}

initialization
  NewConfigurationFile;
  Randomize;
  RandomizeStimuli;
  MakeConfigurationFile(0);

finalization;
  ConfigurationFile.Free;

end.


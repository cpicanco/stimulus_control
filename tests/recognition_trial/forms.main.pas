unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TBackground }

  TBackground = class(TForm)
    ButtonStart: TButton;
    OpenDialog: TOpenDialog;
    RadioGroup1: TRadioGroup;
    InterTrial: TTimer;
    procedure ButtonStartClick(Sender: TObject);
    procedure InterTrialStopTimer(Sender: TObject);
    procedure InterTrialTimer(Sender: TObject);
    procedure TrialEnd(Sender: TObject);
  private
    procedure Play;
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
  Trial : TTrial;
  ConfigurationFile : TConfigurationFile;
  BndCenter : string = '234 533 300 300';
  BndSample : string = '15 533 300 300';
  BndComparisons : array [0..1] of string = ('400 333 300 300', '400 733 300 300');

  function AppExt(Stm : string) : string;
  begin
    Result := Stm + '.png';
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

procedure MakeConfigurationFile;
{
  Consistentes: B1A1, B2A2, C1A1, C2A2, B1C1, B2C2, C1B1, C2B2.
  Inconsistentes: B1A2, B2A1, C1A2, C2A1, B1C2, B2C1, C1B2, C2B1.
}
var
  SSTrials : array [0..5, 0..3] of integer;
  MTSTrials : array [0..3, 0..7] of integer;
  ConsistentPairs : array [0..7] of TStmPair;
  InconsistePairs : array [0..7] of TStmPair;
  AllPairs : array [0..15] of TStmPair;
  i, j : integer;
  r, t : integer;
  StmPair : TStmPair;
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
      WriteToTrial(i, _Kind, T_MTS);
      WriteToTrial(i, _ITI, '6000');
      WriteToTrial(i, _SampleType, BoolToStr(False));
      WriteToTrial(i, _Delay, '0');

      WriteToTrial(i, _Samp+_cStm, AppExt(AConPair.A));
      WriteToTrial(i, _Samp+_cBnd, BndSample);
      WriteToTrial(i, _Samp+_cSch, 'CRF');
      WriteToTrial(i, _Samp+_cMsg, AConPair.A);

      WriteToTrial(i, _NumComp, '2');
      WriteToTrial(i, _Comp+'1'+_cStm, AppExt(AConPair.B));
      WriteToTrial(i, _Comp+'1'+_cBnd, Comparisons[0]);
      WriteToTrial(i, _Comp+'1'+_cSch, 'FR 2');
      WriteToTrial(i, _Comp+'1'+_cMsg, AConPair.B + '-' + Comparisons[0]);

      WriteToTrial(i, _Comp+'2'+_cStm, AppExt(AIncPair.B));
      WriteToTrial(i, _Comp+'2'+_cBnd, Comparisons[1]);
      WriteToTrial(i, _Comp+'2'+_cSch, 'FR 2');
      WriteToTrial(i, _Comp+'2'+_cMsg, AIncPair.B + '-' + Comparisons[1]);
    end;
  end;

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
      WriteToTrial(i, _Kind, T_MTS);
      WriteToTrial(i, _ITI, '9000');
      WriteToTrial(i, _SampleType, BoolToStr(True));
      WriteToTrial(i, _Delay, '500');

      WriteToTrial(i, _Samp+_cStm, AppExt(AConPair.B));
      WriteToTrial(i, _Samp+_cBnd, BndCenter);
      WriteToTrial(i, _Samp+_cSch, 'FT 2000');
      WriteToTrial(i, _Samp+_cMsg, AConPair.B + '-' + BndCenter);

      WriteToTrial(i, _NumComp, '1');
      WriteToTrial(i, _Comp+'1'+_cStm, AppExt(AConPair.A));
      WriteToTrial(i, _Comp+'1'+_cBnd, BndCenter);
      WriteToTrial(i, _Comp+'1'+_cSch, 'FT 2000');
      WriteToTrial(i, _Comp+'1'+_cMsg, AConPair.A + '-' + BndCenter);
    end;
  end;

  procedure WriteMSGTrial(AMsg : integer);
  var
    M1 : string =
    'Seja bem-vindo(a). ' +
    'Durante esta fase serão apresentados alguns símbolos. ' +
    'VOCÊ DEVERÁ PRESTAR MUITA ATENÇÃO NOS SÍMBOLOS. ' +
    'Esta fase durará alguns minutos. ' +
    'Pressione a BARRA DE ESPAÇO no teclado para começar.';

    M2 : string =
    'Agora, nesta fase, você verá uma figura na tela que junta dois símbolos. ' +
    'Abaixo dessa figura haverá uma escala com os itens: ' + LineEnding + LineEnding +
    '"Tenho certeza que não vi, Acho que não vi, Não sei, Acho que vi, Tenho certeza que vi". ' + LineEnding + LineEnding +
    'Você DEVERÁ CLICAR SOBRE UM desses itens para indicar o quanto você lembra ' +
    'de ter visto os símbolos da figura na fase anterior, ' +
    'independente da ordem em que os símbolos aparecem. ' +
    'Pressione a BARRA DE ESPAÇO no teclado para começar.';

    M3 : string =
    'Agora, nesta parte, você verá um símbolo na parte superior da ' +
    'tela e DEVERÁ CLICAR NELE. Em seguida, aparecerão dois símbolos ' +
    'na parte inferior da tela.' +
    'Você DEVERÁ ESCOLHER CLICANDO SOBRE UM DELES. ' +
    'Pressione a BARRA DE ESPAÇO no teclado para começar.';
  i : integer;
  begin
    i := ConfigurationFile.TrialCount[1]+1;
    with ConfigurationFile do
    begin
      WriteToTrial(i, _Kind, T_MSG);
      WriteToTrial(i, _ITI, '3000');
      case AMsg of
        0 : WriteToTrial(i, _Msg, M1);
        1 : WriteToTrial(i, _Msg, M2);
        2 : WriteToTrial(i, _Msg, M3);
      end;
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

  // SS trials
  WriteMSGTrial(0);
  for i := Low(SSTrials) to High(SSTrials) do
    for j := Low(SSTrials[i]) to High(SSTrials[i]) do
      SSTrials[i, j] := j;

  for i := Low(SSTrials) to High(SSTrials) do
    for j := Low(SSTrials[i]) to High(SSTrials[i]) do
      begin
        r := Random(Length(SSTrials[i]));
        t := SSTrials[i, j];
        SSTrials[i, r] := SSTrials[i, j];
        SSTrials[i, j] := t;
      end;

  for i := Low(SSTrials) to High(SSTrials) do
    for j := Low(SSTrials[i]) to High(SSTrials[i]) do
      WriteSSTrial(ConsistentPairs[SSTrials[i, j]]);

  // MTS Trials
  WriteMSGTrial(1);
  for i := Low(MTSTrials) to High(MTSTrials) do
    for j := Low(MTSTrials[i]) to High(MTSTrials[i]) do
      MTSTrials[i, j] := j;

  for i := Low(MTSTrials) to High(MTSTrials) do
    for j := Low(MTSTrials[i]) to High(MTSTrials[i]) do
      begin
        r := Random(Length(MTSTrials[i]));
        t := MTSTrials[i, j];
        MTSTrials[i, r] := MTSTrials[i, j];
        MTSTrials[i, j] := t;
      end;

  for i := Low(MTSTrials) to High(MTSTrials) do
    for j := Low(MTSTrials[i]) to High(MTSTrials[i]) do
      WriteMTSTrial(
        ConsistentPairs[MTSTrials[i,j]], InconsistePairs[MTSTrials[i,j]]);

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
      WriteToTrial(t, _Kind, T_LIK);
      WriteToTrial(t, _ITI, '3000');
      WriteToTrial(t, _Left, AppExt(AllPairs[i].A));
      WriteToTrial(t, _Right, AppExt(AllPairs[i].B));
    end;
  ConfigurationFile.Invalidate;
  ConfigurationFile.UpdateFile;
end;

procedure TBackground.ButtonStartClick(Sender: TObject);
begin
  ButtonStart.Hide;
  {$IFDEF WINDOWS}SwitchFullScreen;{$ENDIF}
  Play;
end;

procedure TBackground.InterTrialStopTimer(Sender: TObject);
var
  CurrentTrial : integer;
  TotalTrials : integer;
begin
  TotalTrials := ConfigurationFile.TrialCount[1]-1;
  CurrentTrial := GlobalContainer.CounterManager.CurrentTrial;
  GlobalContainer.CounterManager.OnNotCorrection(Sender);
  GlobalContainer.CounterManager.OnTrialEnd(Sender);

  if CurrentTrial > TotalTrials-1 then
  begin
    ShowMessage('Fim.');
    WindowState := wsNormal;
    Exit;
  end;
  Play;
end;

procedure TBackground.InterTrialTimer(Sender: TObject);
begin
  InterTrial.Enabled := False;
end;

procedure TBackground.TrialEnd(Sender: TObject);
begin
  if Assigned(Trial) then
    FreeAndNil(Trial);
  InterTrial.Enabled := True;
end;

procedure TBackground.Play;
var
  IndTrial: Integer;
  TrialConfig : TCfgTrial;
begin
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
  finally
    TrialConfig.SList.Free;
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

var
  Footer : string;
  LHeader : string;
  FFilename : string = '';
  FSessionFile : string;

initialization
  FSessionFile := ExtractFilePath(
    Application.ExeName) + DirectorySeparator + 'last_session.ini';
  if FileExists(FSessionFile) then
    DeleteFile(FSessionFile);
  ConfigurationFile := TConfigurationFile.Create(FSessionFile);
  ConfigurationFile.CacheUpdates := True;
  ConfigurationFile.WriteString(_Main, _NumBlc, '2');
  ConfigurationFile.WriteToBloc(1, _Name, 'Memory Test');
  ConfigurationFile.Invalidate;
  Randomize;
  RandomizeStimuli;
  MakeConfigurationFile;

  if (FFilename = #0) or (FFilename = '') then
    FFilename := '000';

  LHeader := HSUBJECT_NAME + #9 + 'name' + LineEnding +
             HSESSION_NAME + #9 + 'name' + LineEnding +
             HBEGIN_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time) + LineEnding;

  FFilename := GlobalContainer.RootData + DirectorySeparator + FFilename;

  CreateLogger(LGTimestamps, FFilename, LHeader);

finalization;
  ConfigurationFile.Free;
  Footer := HEND_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time)+ LineEnding;
  FreeLogger(LGTimestamps,Footer);

end.


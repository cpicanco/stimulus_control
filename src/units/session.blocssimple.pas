unit Session.BlocsSimple;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls
  , CounterManager
  , Controls.Trials.Abstract
  ;

type

  { TBloc }

  TBloc = class(TComponent)
  private
    FCounterManager : TCounterManager;
    FInterTrial : TTimer;
    FITIBegin, FITIEnd : Extended;
    FLastTrialHeader : string;
    FTrial : TTrial;
    procedure PlayTrial(ABloc, ATrial : integer);
    procedure TrialEnd(Sender: TObject);
    procedure WriteTrialData(Sender: TObject);
  private
    FOnEndBloc: TNotifyEvent;
    FOnInterTrialStop: TNotifyEvent;
    procedure InterTrialStopTimer(Sender: TObject);
    procedure InterTrialTimer(Sender: TObject);
    procedure SetOnEndBloc(AValue: TNotifyEvent);
    procedure SetOnInterTrialStop(AValue: TNotifyEvent);
  public
    constructor Create(AOwner : TComponent); override;
    procedure Play;
    property OnEndBloc : TNotifyEvent read FOnEndBloc write SetOnEndBloc;
    property OnInterTrialStop : TNotifyEvent read FOnInterTrialStop write SetOnInterTrialStop;
  end;

implementation

uses Constants
   , Timestamps
   , Loggers.Reports
   , Session.Backgrounds
   , Session.Configuration
   , Session.ConfigurationFile
   , Session.Configuration.GlobalContainer
   , Controls.Trials.Likert
   , Controls.Trials.MatchingToSample
   , Controls.Trials.GoNoGo.Maues
   , Controls.Trials.PerformanceReview
   , Controls.Trials.TextMessage
   ;

{ TBloc }

procedure TBloc.PlayTrial(ABloc, ATrial: integer);
var
  ATrialConfig : TCfgTrial;
begin
  if Assigned(FTrial) then FreeAndNil(FTrial);
  ATrialConfig := ConfigurationFile.Trial[ABloc+1, ATrial+1];
  try
    FInterTrial.Interval := StrToIntDef(ATrialConfig.SList.Values[_ITI], 0);
    case ATrialConfig.Kind of
      T_MSG : FTrial := TMessageTrial.Create(Background);
      T_MTS : FTrial := TMTS.Create(Background);
      T_LIK : FTrial := TLikert.Create(Background);
      T_GNG : FTrial := TGNG.Create(Background);
      T_PFR : FTrial := TPerformanceReview.Create(Background);
    end;

    FTrial.SaveData := GetSaveDataProc(LGTimestamps);
    if FCounterManager.SessionTrials = 0 then
    begin
      FTrial.SaveData(HFIRST_TIMESTAMP + #9 + TimestampToStr(GlobalContainer.TimeStart) + LineEnding + LineEnding);
      FTrial.SaveData(FTrial.HeaderTimestamps + LineEnding);
    end;

    FTrial.Configurations := ATrialConfig;
    FTrial.OnTrialEnd := @TrialEnd;
    FTrial.Play;
    Background.Cursor := FTrial.Cursor;
  finally
    ATrialConfig.SList.Free;
  end;
end;

procedure TBloc.InterTrialStopTimer(Sender: TObject);
begin
  if Assigned(OnIntertrialStop) then OnIntertrialStop(Sender);
  FITIEnd := TickCount - GlobalContainer.TimeStart;
  WriteTrialData(FTrial);
  FCounterManager.CurrentTrial := FCounterManager.CurrentTrial+1;
  Play;
end;

procedure TBloc.InterTrialTimer(Sender: TObject);
begin
  FInterTrial.Enabled := False;
end;

procedure TBloc.SetOnEndBloc(AValue: TNotifyEvent);
begin
  if FOnEndBloc=AValue then Exit;
  FOnEndBloc:=AValue;
end;

procedure TBloc.SetOnInterTrialStop(AValue: TNotifyEvent);
begin
  if FOnInterTrialStop=AValue then Exit;
  FOnInterTrialStop:=AValue;
end;

procedure TBloc.TrialEnd(Sender: TObject);
begin
  if FInterTrial.Interval > 0 then
  begin
    FInterTrial.Enabled := True;
    FITIBegin := TickCount - GlobalContainer.TimeStart;
  end else InterTrialStopTimer(Sender);
end;

procedure TBloc.WriteTrialData(Sender: TObject);
var
  SaveData : TDataProcedure;
  i : integer;
  LTrialID, LTrialName, ITIData, NewData, LReportLn : string;
  LTrial : TTrial;
const
  DoNotApply = #32#32#32#32#32#32 + 'NA';
begin
  LTrial := TTrial(Sender);
  case LTrial.Result of
    'HIT' : FCounterManager.OnHit(Sender);
    'MISS': FCounterManager.OnMiss(Sender);
  end;
  SaveData := GetSaveDataProc(LGData);
  if LTrial.Header <> FLastTrialHeader then
    LReportLn := rsReportTrialID + #9 +
                 rsReportTrialName + #9 +
                 rsReportITIBeg + #9 +
                 rsReportITIEnd + #9 +
                 LTrial.Header + LineEnding;
  FLastTrialHeader := LTrial.Header;
  LTrialID := IntToStr(FCounterManager.CurrentTrial + 1);

  // FTrial Name
  i := FCounterManager.CurrentTrial;
  LTrialName := ConfigurationFile.Trial[1, i+1].Name;
  if LTrialName = '' then
    LTrialName := '--------';

  // pre
  NewData := LTrialID + #9 + LTrialName;

  // iti
  if FCounterManager.SessionTrials = 0 then
    ITIData := DoNotApply + #9 + TimestampToStr(0)
  else
    ITIData :=
      TimestampToStr(FITIBegin) + #9 +
      TimestampToStr(FITIEnd);

  // write data
  LReportLn :=
    LReportLn + NewData + #9 + ITIData + #9 + LTrial.Data + LineEnding;
  SaveData(LReportLn);
end;

constructor TBloc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInterTrial := TTimer.Create(Self);
  FInterTrial.Enabled := False;
  FInterTrial.OnTimer:=@InterTrialTimer;
  FInterTrial.OnStopTimer:=@InterTrialStopTimer;
  FCounterManager := GlobalContainer.CounterManager;
  FLastTrialHeader := '';
  FITIBegin := 0;
  FITIEnd := 0;
end;

procedure TBloc.Play;
var
  LCurrentBloc, LCurrentTrial, LTotalTrials : integer;
begin
  LCurrentBloc := FCounterManager.CurrentBlc;
  LTotalTrials := ConfigurationFile.Bloc[LCurrentBloc+1].NumTrials;
  LCurrentTrial := FCounterManager.CurrentTrial;
  if LCurrentTrial <= LTotalTrials -1 then
    PlayTrial(LCurrentBloc, LCurrentTrial)
  else
    if Assigned(OnEndBloc) then OnEndBloc(Self);
end;

end.

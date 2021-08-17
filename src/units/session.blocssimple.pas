{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Session.BlocsSimple;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls, Controls
  , CounterManager
  , Controls.Trials.Abstract
  , Stimuli.Image
  ;

type

  { TBloc }

  TBloc = class(TComponent)
  private
    FWaitLabel : TLabel;
    FConsequence : TStimulusFigure;
    FCounterManager : TCounterManager;
    FInterTrial : TTimer;
    FTrialConsequence : TTimer;
    FITIBegin, FITIEnd : Extended;
    FLastTrialHeader : string;
    FTrial : TTrial;
    procedure PlayTrial(ABloc, ATrial : integer);
    procedure TrialEnd(Sender: TObject);
    procedure WriteTrialData(Sender: TObject);
  private
    FOnEndBloc: TNotifyEvent;
    FOnInterTrialStop: TNotifyEvent;
    procedure PlayIntertrialInterval(Sender: TObject);
    procedure InterTrialStopTimer(Sender: TObject);
    procedure InterTrialTimer(Sender: TObject);
    procedure SetOnEndBloc(AValue: TNotifyEvent);
    procedure SetOnInterTrialStop(AValue: TNotifyEvent);
  public
    constructor Create(AOwner : TComponent); override;
    procedure BeforePlay;
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
   //, Controls.Trials.Likert
   //, Controls.Trials.MatchingToSample
   //, Controls.Trials.PerformanceReview
   //, Controls.Trials.BeforeAfter
   //, Controls.Trials.ContextualMatchingToSample
   //, Controls.Trials.RelationalFrame
   , Controls.Trials.TextMessage
   , Controls.Trials.HTMLMessage
   , Controls.Trials.BinaryChoice
   , Controls.Trials.TextInput
   , Controls.Trials.FreeSquare
   , Controls.Trials.TemporalBissection
   , Graphics
   , Consequences
   , Dialogs
   ;

{ TBloc }

procedure TBloc.PlayTrial(ABloc, ATrial: integer);
var
  ATrialConfig : TCfgTrial;
  S : string;
begin
  ATrialConfig := ConfigurationFile.Trial[ABloc+1, ATrial+1];

  if (FTrial is TBinaryChoiceTrial) then begin
    if FCounterManager.BlcTrials > 0 then begin
      S := FTrial.Configurations.SList.Values[_LastNow];
      ATrialConfig.SList.Values[_LastNow] := S;

      S := FTrial.Configurations.SList.Values[_Now];
      ATrialConfig.SList.Values[_Now] := S;
    end;
  end;

  if Assigned(FTrial) then
    FreeAndNil(FTrial);

  try
    case ATrialConfig.Kind of
      T_MSG : FTrial := TMessageTrial.Create(Background);
      T_HTM : FTrial := THTMLMessage.Create(Background);
      T_CHO : FTrial := TBinaryChoiceTrial.Create(Background);
      T_INP : FTrial := TTextInput.Create(Background);
      T_EO1 : FTrial := TFreeSquareTrial.Create(Background);
      T_TMB : FTrial := TTBMTS.Create(Background);
      //T_MTS : FTrial := TMTS.Create(Background);
      //T_LIK : FTrial := TLikert.Create(Background);
      //T_GNG : FTrial := TGNG.Create(Background);
      //T_PFR : FTrial := TPerformanceReview.Create(Background);
      //T_BAT : FTrial := TBeforeAfter.Create(Background);
      //T_CTX : FTrial := TCMTS.Create(Background);
      //T_RFT : FTrial := TFrame.Create(Background);
    end;

    FTrial.SaveData := GetSaveDataProc(LGTimestamps);
    if FCounterManager.SessionTrials = 0 then
    begin
      FTrial.SaveData(
        HFIRST_TIMESTAMP + #9 +
        TimestampToStr(GlobalContainer.TimeStart) + LineEnding + LineEnding);
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
var
  LNextTrial : integer;
begin
  FWaitLabel.Hide;
  if Assigned(OnIntertrialStop) then OnIntertrialStop(Sender);
  FITIEnd := TickCount - GlobalContainer.TimeStart;
  WriteTrialData(FTrial);

  LNextTrial := StrToIntDef(FTrial.NextTrial, 1);
  FCounterManager.CurrentTrial := FCounterManager.CurrentTrial+LNextTrial;
  if FCounterManager.CurrentTrial < 0 then
    Exception.Create('Exception. CurrentTrial cannot be less than zero.');
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
var
  LTrial : TTrial;
begin
  LTrial := TTrial(Sender);
  LTrial.Hide;
  Background.Cursor := -1;
  FTrialConsequence.Interval := LTrial.ConsequenceInterval;
  if FTrialConsequence.Interval > 0 then
    begin
      FTrialConsequence.Enabled := True;
      if LTrial.HasVisualConsequence then
        case LTrial.Result of
          'HIT' :
            begin
              Background.Color := clPurple;
              FConsequence.Start;
            end;

          'MISS':
            begin
              Background.Color := clBlack;
            end;
        end;
    end
  else
    PlayInterTrialInterval(Sender);
end;

procedure TBloc.WriteTrialData(Sender: TObject);
var
  SaveData : TDataProcedure;
  i, j : integer;
  LTrialNo, LBlocID, LBlocName,
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
    LReportLn := rsReportTrialNO + #9 +
                 rsReportBlocID + #9 +
                 rsReportBlocName + #9 +
                 rsReportTrialID + #9 +
                 rsReportTrialName + #9 +
                 rsReportITIBeg + #9 +
                 rsReportITIEnd + #9 +
                 LTrial.Header + LineEnding;

  i := FCounterManager.CurrentTrial;
  j := FCounterManager.CurrentBlc;
  FLastTrialHeader := LTrial.Header;
  LTrialNo := (FCounterManager.SessionTrials + 1).ToString;
  LBlocID := (j + 1).ToString;
  LBlocName := ConfigurationFile.Bloc[j+1].Name;
  LTrialID := (i + 1).ToString;

  // FTrial Name

  LTrialName := ConfigurationFile.Trial[j+1, i+1].Name;
  if LTrialName = '' then
    LTrialName := '--------';

  // pre
  NewData :=
    LTrialNo  + #9 +
    LBlocID  + #9 +
    LBlocName + #9 +
    LTrialID + #9 + LTrialName;

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

procedure TBloc.PlayIntertrialInterval(Sender: TObject);
begin
  if Assigned(FTrialConsequence) then
  begin
    FTrialConsequence.Enabled:=False;
    FTrial.StopConsequence;
    //FConsequence.Caption:='';
    //FConsequence.Hide;
    FConsequence.Stop;
    Background.Color := clWhite;
  end;

  {$IFDEF DEBUG}
  FInterTrial.Interval := 100;
  {$ELSE}
  FInterTrial.Interval := Round(StrToFloatDef(FTrial.Configurations.SList.Values[_ITI], 0));
  {$ENDIF}
  if FInterTrial.Interval > 0 then
    begin
      if (FTrial is TFreeSquareTrial) or (FTrial is TTBMTS) then begin
        FWaitLabel.Show;
      end;
      FInterTrial.Enabled := True;
      FITIBegin := TickCount - GlobalContainer.TimeStart;
    end
  else
    InterTrialStopTimer(Sender);
end;

constructor TBloc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInterTrial := TTimer.Create(Self);
  FInterTrial.Enabled := False;
  FInterTrial.OnTimer := @InterTrialTimer;
  FInterTrial.OnStopTimer := @InterTrialStopTimer;

  FTrialConsequence := TTimer.Create(Self);
  FTrialConsequence.Enabled := False;
  FTrialConsequence.OnTimer := @PlayIntertrialInterval;

  FCounterManager := GlobalContainer.CounterManager;
  FLastTrialHeader := '';
  FITIBegin := 0;
  FITIEnd := 0;

  FWaitLabel := TLabel.Create(Self);
  with FWaitLabel do begin
    Visible := False;
    Cursor := -1;
    Align := alClient;
    Alignment := taCenter;
    Anchors := [akLeft,akRight];
    WordWrap := True;
    Font.Name := 'Arial';
    Font.Color := 0;
    Font.Size := 30;
    Layout:=tlCenter;
    Caption:='Aguarde';
    //OnMouseUp := @MessageMouseUp;
  end;

  FConsequence := TStimulusFigure.Create(Self);
  FConsequence.HideCursor;
  FConsequence.LoadFromFile('acerto.png');
end;

procedure TBloc.BeforePlay;
begin
  FWaitLabel.Parent := Background;
  FConsequence.Parent := Background;
  Background.Cursor := -1;
end;

procedure TBloc.Play;
var
  LCurrentBloc, LCurrentTrial, LTotalTrials : integer;
  LBlc : TCfgBlc;
  function EndCriteriaAchieved(ABlc : TCfgBlc) : Boolean;
  begin
    Result := False;
    if ABlc.CrtConsecutiveHit > 0 then
      if FCounterManager.BlcCscHits = ABlc.CrtConsecutiveHit then
      begin

        Exit;
      end;
    if ABlc.CrtMaxTrials > 0 then
      if ABlc.CrtMaxTrials <= FCounterManager.BlcTrials  then
      begin
        Result := True;
        Exit;
      end;
  end;

begin
  LCurrentBloc := FCounterManager.CurrentBlc;
  LBlc := ConfigurationFile.Bloc[LCurrentBloc+1];
  LTotalTrials := LBlc.NumTrials;
  LCurrentTrial := FCounterManager.CurrentTrial;

  // will check every trial
  if EndCriteriaAchieved(LBlc) then
  begin
    if Assigned(OnEndBloc) then OnEndBloc(Self);
    Exit;
  end;

  if (LCurrentTrial <= LTotalTrials -1) then
    PlayTrial(LCurrentBloc, LCurrentTrial)
  else
    begin // will check once per bloc
      if LBlc.CrtHitValue > 0 then
        if (FCounterManager.BlcHits < LBlc.CrtHitValue) then
        begin
          if LBlc.MaxBlcRepetition > 0 then
          begin
            if (FCounterManager.BlcRepetitions < LBlc.MaxBlcRepetition) then
            begin
              FCounterManager.OnRepeatBlc(Self);
              PlayTrial(LCurrentBloc, FCounterManager.CurrentTrial);
              Exit;
            end else begin
              FCounterManager.CurrentBlc := ConfigurationFile.BlocCount;
              //FCounterManager.OnEndSess(Self);
            end;
          end else begin
            FCounterManager.OnRepeatBlc(Self);
            PlayTrial(LCurrentBloc, FCounterManager.CurrentTrial);
            Exit;
          end;
        end;

      if (LBlc.CrtHitPorcentage > 0) and (LBlc.CrtHitPorcentage <= 100) then
      begin
        if (Round((FCounterManager.BlcHits * 100)/LBlc.NumTrials) >= LBlc.CrtHitPorcentage) then
        begin
          if LBlc.NextBlocOnCriteria > 0 then begin
            FCounterManager.CurrentBlc := LBlc.NextBlocOnCriteria-1;
          end else begin
            FCounterManager.CurrentBlc := ConfigurationFile.BlocCount;
          end;
        end;
      end;

      //if LBlc.CrtConsecutiveHit > 0 then
      //  if FCounterManager.BlcHighCscHits < LBlc.CrtConsecutiveHit then
      //  begin
      //    FCounterManager.OnRepeatBlc(Self);
      //    PlayTrial(LCurrentBloc, FCounterManager.CurrentTrial);
      //    if Assigned(OnEndBloc) then OnEndBloc(Self);
      //    Exit;
      //  end;


      if Assigned(OnEndBloc) then OnEndBloc(Self);
    end;
end;

end.

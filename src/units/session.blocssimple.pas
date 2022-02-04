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
  , Controls.Trials.Abstract
  , Stimuli.Image
  , SerialTimer
  ;

type

  { TBloc }

  TBloc = class(TComponent)
  private
    //FTable : TComponent;
    FSerialTimer : TSerialTimer;
    FInterTrial : TTimerItem;
    FDelay : TTimerItem;
    FConsequenceDuration : TTimerItem;
    FWaitLabel : TLabel;
    FConsequence : TStimulusFigure;
    FITIBegin, FITIEnd : Extended;
    FLastTrialHeader : string;
    FTrial : TTrial;
    procedure DelayBegin;
    procedure ConsequenceBegin;
    procedure InterTrialIntervalBegin;
    function HasDelay : Boolean;
    function HasConsequenceDuration : Boolean;
    function HasInterTrialTime : Boolean;
    procedure PlayTrial(ABloc, ATrial : integer);
    procedure TrialEnd(Sender: TObject);
    procedure WriteTrialData(Sender: TObject);
  private
    FOnEndBloc: TNotifyEvent;
    FOnInterTrialStop: TNotifyEvent;
    procedure ConsequenceEnd(Sender: TObject);
    procedure DelayEnd(Sender: TObject);
    procedure InterTrialEnd(Sender: TObject);
    procedure SetOnEndBloc(AValue: TNotifyEvent);
    procedure SetOnInterTrialStop(AValue: TNotifyEvent);
  public
    constructor Create(AOwner : TComponent); override;
    procedure BeforePlay;
    procedure Play;
    property OnEndBloc : TNotifyEvent read FOnEndBloc write SetOnEndBloc;
    property OnInterTrialEnd : TNotifyEvent read FOnInterTrialStop write SetOnInterTrialStop;
  end;

implementation

uses Constants
   , Timestamps
   , Loggers.Reports
   , Session.Backgrounds
   , Session.Configuration
   , Session.ConfigurationFile
   , Session.Configuration.GlobalContainer
   , Session.EndCriteria
   , Controls.Trials.HTMLMessage
   , Controls.Trials.DMTS
   , Graphics
   , Consequences
   , Dialogs
   ;

{ TBloc }

procedure TBloc.PlayTrial(ABloc, ATrial: integer);
var
  LTrialConfig : TCfgTrial;
  S : string;
begin
  LTrialConfig := ConfigurationFile.Trial[ABloc+1, ATrial+1];

  if Assigned(FTrial) then
    FreeAndNil(FTrial);

  try
    case LTrialConfig.Kind of
      T_HTM : FTrial := THTMLMessage.Create(Background);
      T_DMTS : FTrial := TDMTS.Create(Background);
    end;
    LTrialConfig.Parameters.Values[_CrtMaxTrials] :=
      ConfigurationFile.CurrentBloc.CrtMaxTrials.ToString;

    FTrial.SaveData := GetSaveDataProc(LGTimestamps);
    if Counters.SessionTrials = 0 then
    begin
      FTrial.SaveData(
        HFIRST_TIMESTAMP + #9 +
        TimestampToStr(GlobalContainer.TimeStart) + LineEnding + LineEnding);
      FTrial.SaveData(FTrial.HeaderTimestamps + LineEnding);
    end;
    FTrial.Configurations := LTrialConfig;
    FTrial.OnTrialEnd := @TrialEnd;
    FTrial.Play;
    Background.Cursor := FTrial.Cursor;
  finally
    LTrialConfig.Parameters.Free;
  end;
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
  FTrial.Hide;
  Background.Cursor := -1;
  FDelay.Interval := FTrial.ConsequenceDelay;
  FConsequenceDuration.Interval := FTrial.ConsequenceInterval;
  if Counters.CurrentTrial > 0 then begin
    FInterTrial.Interval :=
      StrToInt(FTrial.Configurations.Parameters.Values[_ITI]);
  end else begin
    FInterTrial.Interval := 1;
  end;

  if HasDelay then begin
    FSerialTimer.Append(FDelay);
  end;

  if HasConsequenceDuration then begin
    FSerialTimer.Append(FConsequenceDuration);
  end;

  if HasInterTrialTime then begin
    FSerialTimer.Append(FInterTrial);
  end;

  if HasDelay then begin
    DelayBegin;
    FSerialTimer.Start;
    Exit;
  end;

  if HasConsequenceDuration then begin
    ConsequenceBegin;
    FSerialTimer.Start;
    Exit;
  end;

  if HasInterTrialTime then begin
    InterTrialIntervalBegin;
    FSerialTimer.Start;
    Exit;
  end;

  InterTrialEnd(Sender);
  //if (not HasDelay) and
  //   (not HasConsequenceDuration) and
  //   (not HasInterTrialTime) then begin
  //  FInterTrial.Interval := 1;
  //  FSerialTimer.Append(FInterTrial);
  //  FSerialTimer.Start;
  //end;
end;

procedure TBloc.WriteTrialData(Sender: TObject);
var
  LSaveData : TDataProcedure;
  i, j : integer;
  LTrialNo, LBlocID, LBlocName,
  LTrialID, LTrialName, ITIData, NewData, LReportLn : string;
  LTrial : TTrial;
const
  DoNotApply = #32#32#32#32#32#32 + 'NA';
begin
  LTrial := TTrial(Sender);
  case LTrial.Result of
    'HIT' : Counters.OnHit(Sender);
    'MISS': Counters.OnMiss(Sender);
  end;
  LSaveData := GetSaveDataProc(LGData);
  if LTrial.Header <> FLastTrialHeader then
    LReportLn := rsReportTrialNO + #9 +
                 rsReportBlocID + #9 +
                 rsReportBlocName + #9 +
                 rsReportTrialID + #9 +
                 rsReportTrialName + #9 +
                 rsReportITIBeg + #9 +
                 rsReportITIEnd + #9 +
                 LTrial.Header + LineEnding;

  i := Counters.CurrentTrial;
  j := Counters.CurrentBlc;
  FLastTrialHeader := LTrial.Header;
  LTrialNo := (Counters.SessionTrials + 1).ToString;
  LBlocID := (j + 1).ToString;
  LBlocName := ConfigurationFile.Bloc[j+1].Name;
  LTrialID := (i + 1).ToString;

  // FTrial Name
  LTrialName := LTrial.Configurations.Name;
  if LTrialName = '' then
    LTrialName := '--------';

  // pre
  NewData :=
    LTrialNo  + #9 +
    LBlocID  + #9 +
    LBlocName + #9 +
    LTrialID + #9 + LTrialName;

  // iti
  if Counters.SessionTrials = 0 then
    ITIData := DoNotApply + #9 + TimestampToStr(0)
  else
    ITIData :=
      TimestampToStr(FITIBegin) + #9 +
      TimestampToStr(FITIEnd);

  // write data
  LReportLn :=
    LReportLn + NewData + #9 + ITIData + #9 + LTrial.Data + LineEnding;
  LSaveData(LReportLn);
end;

function TBloc.HasDelay: Boolean;
begin
  Result := FDelay.Interval > 0;
end;

function TBloc.HasConsequenceDuration: Boolean;
begin
  Result := FConsequenceDuration.Interval > 0;
end;

function TBloc.HasInterTrialTime: Boolean;
begin
  Result := FInterTrial.Interval > 0;
end;

procedure TBloc.DelayBegin;
begin
  { do nothing }
end;

procedure TBloc.DelayEnd(Sender: TObject);
begin
  if HasConsequenceDuration then begin
    ConsequenceBegin;
    Exit;
  end;

  if HasInterTrialTime then begin
    InterTrialIntervalBegin;
  end;
end;

procedure TBloc.ConsequenceBegin;
begin
  if FTrial.HasVisualConsequence then
    case FTrial.Result of
      'HIT+BLACKOUT':
        begin
          Background.Color := clBlack;
        end;
      'HIT' :
        begin
          Background.Color := clWhite;
          FConsequence.Start;
        end;

      'MISS':
        begin
          Background.Color := clBlack;
        end;
    end;
end;

procedure TBloc.ConsequenceEnd(Sender: TObject);
begin
  Background.Color := clWhite;

  if FConsequence.Visible then
    FConsequence.Stop;

  if HasInterTrialTime then begin
    InterTrialIntervalBegin;
  end;
end;

procedure TBloc.InterTrialIntervalBegin;
begin
  if (FTrial is TDMTS) then begin
    //FWaitLabel.Show;
  end;
  //FInterTrial.Enabled := True;
  FITIBegin := TickCount - GlobalContainer.TimeStart;
end;

procedure TBloc.InterTrialEnd(Sender: TObject);
var
  LNextTrial : integer;
begin
  if Sender is TSerialTimer then begin
    FSerialTimer.Stop;
    FSerialTimer.Clear;
  end;

  FDelay.Interval := 0;
  FConsequenceDuration.Interval := 0;
  FInterTrial.Interval := 0;

  if FConsequence.Visible then
    FConsequence.Stop;

  if FWaitLabel.Visible then
    FWaitLabel.Hide;

  if Assigned(OnInterTrialEnd) then OnInterTrialEnd(Self);
  FITIEnd := TickCount - GlobalContainer.TimeStart;
  WriteTrialData(FTrial);

  LNextTrial := StrToIntDef(FTrial.NextTrial, 1);
  Counters.CurrentTrial := Counters.CurrentTrial+LNextTrial;
  if Counters.CurrentTrial < 0 then
    Exception.Create('Exception. CurrentTrial cannot be less than zero.');
  Play;
end;

constructor TBloc.Create(AOwner: TComponent);
var
  LParameters : TStringList;
begin
  inherited Create(AOwner);
  //FTable := nil;
  FSerialTimer := TSerialTimer.Create(Self);

  FDelay.OnTimerEvent := @DelayEnd;
  FConsequenceDuration.OnTimerEvent := @ConsequenceEnd;
  FSerialTimer.OnEndTimeSerie := @InterTrialEnd;

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

  LParameters := TStringList.Create;
  LParameters.Values['Consequence'] := 'acerto.png';
  FConsequence := TStimulusFigure.Create(Self);
  FConsequence.Key := 'Consequence';
  FConsequence.HideCursor;
  FConsequence.LoadFromParameters(LParameters);
  LParameters.Free;
end;

procedure TBloc.BeforePlay;
//var
//  LBeginTable : string;
//  LEndTable: string;
begin
  FWaitLabel.Parent := Background;
  FConsequence.Parent := Background;
  Background.Cursor := -1;

  //if Assigned(FTable) then begin
  //  LEndTable := ConfigurationFile.EndTableName;
  //  if LEndTable.IsEmpty then begin
  //    { do nothing }
  //  end else begin
  //    if LEndTable = FTable.Name then begin
  //      FTable.Free;
  //      FTable := nil;
  //    end;
  //  end;
  //end;
  //
  //LBeginTable := ConfigurationFile.BeginTableName;
  //if LBeginTable.IsEmpty then begin
  //  { do nothing }
  //end else begin
  //  if LBeginTable.Contains('DiscountTable') then begin
  //    FTable := TDelayDiscountTable.Create(Self);
  //    FTable.Name := LBeginTable;
  //  end else
  //  if LBeginTable.Contains('DemandTable') then begin
  //    FTable := TDemandTable.Create(Self);
  //    FTable.Name := LBeginTable;
  //  end else
  //  if LBeginTable.Contains('Experiment1Table') then begin
  //    FTable := TExperiment1Table.Create(Self);
  //    FTable.Name := LBeginTable;
  //  end else
  //  if LBeginTable.Contains('Experiment2Table') then begin
  //    FTable := TExperiment2Table.Create(Self);
  //    FTable.Name := LBeginTable;
  //  end else
  //  if LBeginTable.Contains('Experiment3Table') then begin
  //    FTable := TExperiment3Table.Create(Self);
  //    FTable.Name := LBeginTable;
  //  end;
  //end;
end;

procedure TBloc.Play;
begin
  EndCriteria.Invalidate;
  if EndCriteria.OfBloc then begin
    if Assigned(OnEndBloc) then OnEndBloc(Self);
  end else begin
    PlayTrial(Counters.CurrentBlc, Counters.CurrentTrial)
  end;
end;

end.

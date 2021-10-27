{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.Freesquare.MixVTFI;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils, ExtCtrls

  , Controls.Trials.Abstract
  , Stimuli.Game.FreeSquare
  , Stimuli.Image
  , Schedules
  , SerialTimer
  ;

type

  TTrialType = (ttE3C2);

  TReportData = record
    CBegin : Extended;
    CLatency : Extended;
    CEnd     : Extended;
    DelayEnd  : string;
  end;

  { TFreeSquareTrial }

  {
    Implements Mixed schedule with two components:
      Variable Time 7.5 s
      Fixed Interval 15.0 s with Limited Hold 0.5 s
    Square at screen center as operandum
    Square clicks operates in FI
  }

  { TFreeSquareMixVTFI }

  TFreeSquareMixVTFI = class(TTrial)
  private
    FCustomResult : integer;
    FConsequence : TStimulusFigure;
    FSerialTimer : TSerialTimer;
    FFirstResponse : Boolean;
    FReportData : TReportData;
    FStimulus : TFreeSquare;
    procedure GenerateScheduleIntervals;
    procedure VTConsequenceStop(Sender: TObject);
    procedure EnableFIConsequence(Sender: TObject);
    procedure OmissionEnd(Sender: TObject);
    procedure VTConsequenceStart(Sender: TObject);
    procedure Consequence(Sender: TObject);
    procedure Response(Sender: TObject);
    procedure TrialBeforeEnd(Sender: TObject);
    procedure SetCustomResult;
    function GetHeader: string;
    function IsTestTrial : Boolean;
  protected
    procedure TrialStart(Sender: TObject); virtual;
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TCustomControl); override;
    function AsString : string; override;
    function HasVisualConsequence: Boolean; override;
    function ConsequenceInterval: integer; override;
    procedure Play(ACorrection : Boolean); override;
  end;

implementation

uses Constants, Timestamps, Forms, Graphics
  , Cheats, Session.Configuration.GlobalContainer
  , Experiments.Eduardo.Experimento3.Tables;

constructor TFreeSquareMixVTFI.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialStart := @TrialStart;

  if Self.ClassType = TFreeSquareMixVTFI then
    Header := Header + #9 + GetHeader;

  Configurations.Parameters.Values['VTConsequence'] := 'acerto.png';
  FConsequence := TStimulusFigure.Create(Self);
  FConsequence.Key := 'VTConsequence';
  FConsequence.HideCursor;
  FConsequence.LoadFromParameters(Configurations.Parameters);
  FConsequence.Parent := AOwner;
  FSerialTimer := TSerialTimer.Create(Self);
  FFirstResponse := True;
  FResponseEnabled := False;
end;

function TFreeSquareMixVTFI.AsString: string;
begin
  // todo
  Result := '';
end;

function TFreeSquareMixVTFI.HasVisualConsequence: Boolean;
begin
  Result := (Self.Result <> T_NONE);
end;

function TFreeSquareMixVTFI.ConsequenceInterval: integer;
begin
  case Self.Result of
    T_MISS, T_HIT : Result := FreeSquareConsequenceDurationExp3;
    T_NONE : Result := 0;
  end;
end;

procedure TFreeSquareMixVTFI.Play(ACorrection: Boolean);
var
  LParameters : TStringList;
begin
  inherited Play(ACorrection);
  GenerateScheduleIntervals;
  LParameters := Configurations.Parameters;
  FCounterType := ctCustom;

  FStimulus := TFreeSquare.Create(Self);
  FStimulus.Freeze;
  TExperiment3Table(FTable).CreateTrial(Counters.SessionTrials);

  FStimulus.Parent := Self.Parent;
  FStimulus.LoadFromParameters(LParameters);
  FStimulus.OnConsequence := @Consequence;
  FStimulus.OnResponse := @Response;
  FStimulus.FitScreen;
  FShowCounter := True;
  if Self.ClassType = TFreeSquareMixVTFI then Config(Self);
end;

procedure TFreeSquareMixVTFI.TrialStart(Sender: TObject);
begin
  FSerialTimer.Start;
  FStimulus.Start;

  FReportData.CBegin:=LogEvent('Quadrado.Inicio');
  FResponseEnabled:=True;
  Invalidate;

  if CheatsModeOn then begin
    if Random <= 0.8 then
      ParticipantBot.Start(FStimulus.AsInterface);
  end;
end;

procedure TFreeSquareMixVTFI.WriteData(Sender: TObject);
var
  LLatency : Extended = -1;
  LResponsesTime : Extended;
begin
  inherited WriteData(Sender);
  LResponsesTime := FReportData.CEnd-FReportData.CBegin;
  if not FFirstResponse then begin
    LLatency := FReportData.CLatency-FReportData.CBegin;
  end;

  TExperiment3Table(FTable).AddRow(
    Counters.SessionTrials,
    LLatency, LResponsesTime, FCustomResult, IsTestTrial);

  Data := Data +
          TimestampToStr(FReportData.CLatency) + HeaderTabs +
          TimestampToStr(FReportData.CBegin) + HeaderTabs +
          TimestampToStr(FReportData.CEnd);
end;

function TFreeSquareMixVTFI.GetHeader: string;
begin
  Result :=  rsReportRspLat + HeaderTabs +
             rsReportStmBeg + HeaderTabs +
             rsReportStmEnd;
end;

procedure TFreeSquareMixVTFI.SetCustomResult;
begin
  if IsTestTrial then begin
    FCustomResult := -1;
  end else begin
    if Self.Result = T_HIT then begin
      FCustomResult := 1;
    end else
    if (Self.Result = T_MISS) or (Self.Result = T_NONE) then begin
      FCustomResult := 0;
    end;
  end;
end;

function TFreeSquareMixVTFI.IsTestTrial: Boolean;
begin
  Result := Configurations.Parameters.Values['IsTestTrial'].ToBoolean;
end;

procedure TFreeSquareMixVTFI.OmissionEnd(Sender: TObject);
begin
  FResponseEnabled := False;
  Invalidate;
  FReportData.CEnd := LogEvent('Omissao.Fim');
  FStimulus.Stop;
  Result := T_NONE;

  Configurations.Parameters.Values[_ITI] := ITIExperiment3.ToString;

  SetCustomResult;
  EndTrial(Self);
end;

procedure TFreeSquareMixVTFI.VTConsequenceStart(Sender : TObject);
begin
  FResponseEnabled := False;
  Self.Cursor := -1;
  Parent.Cursor := -1;
  FShowCounter := False;
  FStimulus.Hide;
  FConsequence.Start;
  Parent.Color := clDarkGreen;
    Counters.SessionPointsTopLeft :=
      Counters.SessionPointsTopLeft +1;
  TExperiment3Table(FTable).AddDelayBegin(
    Counters.SessionTrials, LogEvent('VT.Inicio'));
  Invalidate;
end;

type
  TVTInterval = record
    Start : Cardinal;
    Stop  : Cardinal;
  end;

procedure TFreeSquareMixVTFI.GenerateScheduleIntervals;
var
  VTSchedule : TSchedule;
  VTInterval  : TVTInterval;
  VTIntervals : array of TVTInterval = nil;
  TotalTime : Cardinal = 0;
  TrialTimeIsFull : Boolean;
  TimerItem : TTimerItem;

  function GetVTInterval : TVTInterval;
  begin
    Result.Start := VTSchedule.GetInterval;
    if CheatsModeOn then begin
      Result.Stop := 100;
    end else begin
      Result.Stop := 1000;
    end;
  end;

  procedure AppendVT(AVTInterval : TVTInterval);
  begin
    SetLength(VTIntervals, Length(VTIntervals)+1);
    VTIntervals[High(VTIntervals)] := AVTInterval;
  end;

  function Valid(AVTInterval : TVTInterval) : Boolean;
  begin
    Inc(TotalTime, AVTInterval.Start);
    Inc(TotalTime, AVTInterval.Stop);
    Result := TotalTime < Experiment3FIValue;
    if Result then
      AppendVT(AVTInterval)
    else
      VTSchedule.Postpone;
  end;

  function GetTotalTime : Cardinal;
  var
    Interval : TVTInterval;
  begin
    Result := 0;
    for Interval in VTIntervals do
      Inc(Result, Interval.Stop);
  end;

begin
  FSerialTimer := TSerialTimer.Create(Self);
  if IsTestTrial then begin
    TimerItem.Interval := FreeSquareOmissionDurationExp3Testing;
    TimerItem.OnTimerEvent := @OmissionEnd;
    FSerialTimer.Append(TimerItem);
  end else begin
    VTSchedule := TSchedule.Create(Self, VT, 1000, 500);
    VTSchedule.UseFleshlerHoffmanIntervals;
    repeat
      VTInterval := GetVTInterval;
      TrialTimeIsFull := not Valid(VTInterval);
    until TrialTimeIsFull;

    for VTInterval in VTIntervals do begin
      TimerItem.Interval := VTInterval.Start;
      TimerItem.OnTimerEvent := @VTConsequenceStart;
      FSerialTimer.Append(TimerItem);

      TimerItem.Interval := VTInterval.Stop;
      TimerItem.OnTimerEvent := @VTConsequenceStop;
      FSerialTimer.Append(TimerItem);
    end;
    TimerItem.Interval :=  Experiment3FIValue-GetTotalTime;
    TimerItem.OnTimerEvent := @EnableFIConsequence;
    FSerialTimer.Append(TimerItem);

    TimerItem.Interval :=  500;
    TimerItem.OnTimerEvent := @OmissionEnd;
    FSerialTimer.Append(TimerItem);
    VTSchedule.Free;
  end;
end;

procedure TFreeSquareMixVTFI.VTConsequenceStop(Sender : TObject);
begin
  FConsequence.Stop;
  Self.Cursor := 1;
  Parent.Cursor := 1;
  FShowCounter := True;
  FStimulus.Start;
  Parent.Color := clWhite;
  Invalidate;
  TExperiment3Table(FTable).AddDelayEnd(
    Counters.SessionTrials, LogEvent('VT.Fim'));
  FResponseEnabled := True;
end;

procedure TFreeSquareMixVTFI.EnableFIConsequence(Sender: TObject);
begin
  FStimulus.Schedule.Load(CRF);
  FStimulus.Start;
end;

procedure TFreeSquareMixVTFI.Consequence(Sender: TObject);
var
  ITI : Extended;
begin
  FSerialTimer.Stop;
  FResponseEnabled := False;
  FStimulus.Stop;
  Counters.SessionPointsTopRight :=
    Counters.SessionPointsTopRight +1;
  Invalidate;
  FReportData.CEnd:=LogEvent('Reforco');
  ITI := ITIExperiment3;
   Configurations.Parameters.Values[_ITI] := ITI.ToString;
  Result := T_HIT;
  SetCustomResult;
  EndTrial(Self);
end;

procedure TFreeSquareMixVTFI.Response(Sender: TObject);
var
  LResponse : Extended;
begin
  if FResponseEnabled then begin
    if FFirstResponse then
    begin
      FFirstResponse := False;
      LResponse := LogEvent('Resposta.Latencia');
      FReportData.CLatency := LResponse
    end else begin
      LResponse := LogEvent('Resposta');
    end;
    TExperiment3Table(FTable).AddResponse(
      Counters.SessionTrials, LResponse);
  end;
end;

procedure TFreeSquareMixVTFI.TrialBeforeEnd(Sender: TObject);
begin
  WriteData(Self);
end;


end.

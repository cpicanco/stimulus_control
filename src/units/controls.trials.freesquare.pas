{
  Stimulus Control
  Copyright (C) 2014-2020 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.FreeSquare;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils, ExtCtrls

  , Controls.Trials.Abstract
  , Stimuli.Game.FreeSquare
  , Stimuli.Image
  , Schedules
  ;

type

  TTrialType = (ttE1B, ttE1C, ttE3B1, ttE3B2, ttE3C1, ttE3C2);

  TReportData = record
    CBegin : Extended;
    CLatency : Extended;
    CTimeLatency : Extended;
    ComparisonBegin   : string;
    ComparisonEnd     : string;
    ComparisonLatency : string;
    DelayEnd  : string;
  end;

  { TFreeSquareTrial }

  {
    Implements Free Operant Square
    Square moves around
    Clicks produces consequences
    LimitedHold produces diferential consequences
  }
  TFreeSquareTrial = class(TTrial)
  private
    FConsequence : TStimulusFigure;
    FSchedule : TSchedule;
    FSquareSchedule : TSchedule;
    FTrialType : TTrialType;
    FFirstResponse : Boolean;
    //FHasConsequence : Boolean;
    FReportData : TReportData;
    FStimulus : TFreeSquare;
    FTimer : TSchedule;
    FConsequenceTimer : TTimer;
    procedure VTConsequenceEnd(Sender: TObject);
    procedure OmissionEnd(Sender: TObject);
    procedure DelayEnd(Sender: TObject);
    procedure VTConsequence(Sender: TObject);
    procedure Consequence(Sender: TObject);
    procedure Response(Sender: TObject);
    procedure TrialBeforeEnd(Sender: TObject);
    function GetHeader: string;
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

uses Constants, Timestamps, Graphics
  , Cheats
  ;

const
  TrialDuration : integer = 20000;

  // Experiment 1
  OmissionDurationExp1 : integer = 5000;

  // Experiment 3
  OmissionDurationExp3Training : integer = 15500;
  OmissionDurationExp3Testing : integer = 30000;

  ConsequenceDelay : integer = 10000;
  ConsequenceDurationExp1 : integer = 2000;
  ConsequenceDurationExp3 : integer = 1000;

  ITIExperiment3 : integer = 500;

constructor TFreeSquareTrial.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialStart := @TrialStart;

  if Self.ClassType = TFreeSquareTrial then
    Header := Header + #9 + GetHeader;

  FConsequence := TStimulusFigure.Create(Self);
  FConsequence.HideCursor;
  FConsequence.LoadFromFile('acerto.png');
  FConsequence.Parent := AOwner;

  FConsequenceTimer := TTimer.Create(Self);
  FConsequenceTimer.Interval := ConsequenceDurationExp3;
  FConsequenceTimer.OnTimer := @VTConsequenceEnd;

  FTimer := TSchedule.Create(Self);
  FFirstResponse := True;
  FResponseEnabled := False;
end;

function TFreeSquareTrial.AsString: string;
begin
  // todo
  Result := '';
end;

function TFreeSquareTrial.HasVisualConsequence: Boolean;
begin
  Result := (Self.Result <> T_NONE);
end;

function TFreeSquareTrial.ConsequenceInterval: integer;
begin
  case FTrialType of
    ttE1B, ttE1C, ttE3B1, ttE3B2 : Result := ConsequenceDurationExp1;
    ttE3C1, ttE3C2 : begin
      case Self.Result of
        T_MISS, T_HIT : Result := ConsequenceDurationExp3;
        T_NONE : Result := 0;
      end;
    end;
  end;
end;

procedure TFreeSquareTrial.Play(ACorrection: Boolean);
var
  LParameters : TStringList;
begin
  inherited Play(ACorrection);
  LParameters := Configurations.SList;
  case LParameters.Values['Type'] of
    'E1B' : begin
      FTrialType := ttE1B;
    end;

    'E1C' : begin
      FTrialType := ttE1C;
    end;

    'E3B1': begin
      FTrialType := ttE3B1;
    end;

    'E3B2': begin
      FTrialType := ttE3B2;
    end;

    'E3C1': begin
      FTrialType := ttE3C1;
    end;

    'E3C2': begin
      FTrialType := ttE3C2;
    end;
  end;

  case FTrialType of
    ttE3C1, ttE3C2 : begin
      FSchedule := TSchedule.Create(Self);
      FSchedule.Load(VT, 1000, 500);
      FSchedule.UseFleshlerHoffmanIntervals;
      FSchedule.OnConsequence := @VTConsequence;
    end
    else { do nothing };
  end;

  case FTrialType of
    ttE1B, ttE1C   : FCounterType := ctBlocPoints;
    ttE3B1, ttE3B2 : FCounterType := ctSessionPoints;
    ttE3C1, ttE3C2 : FCounterType := ctCustom;
  end;

  FStimulus := TFreeSquare.Create(Self);
  case FTrialType of
    ttE1B, ttE1C : { do nothing };
    ttE3B1, ttE3B2, ttE3C1, ttE3C2 : FStimulus.Freeze;
  end;
  FSquareSchedule := FStimulus.Schedule;
  FStimulus.Parent := Self.Parent;
  FStimulus.LoadFromParameters(LParameters);
  FStimulus.OnConsequence := @Consequence;
  FStimulus.OnResponse := @Response;
  FStimulus.FitScreen;
  FShowCounter := True;
  if Self.ClassType = TFreeSquareTrial then Config(Self);
end;

procedure TFreeSquareTrial.TrialStart(Sender: TObject);
begin
  case FTrialType of
    ttE1B,  ttE1C  : FTimer.Load(FT, OmissionDurationExp1);
    ttE3B1, ttE3C1 : FTimer.Load(FT, OmissionDurationExp3Training);
    ttE3B2, ttE3C2 : FTimer.Load(FT, OmissionDurationExp3Testing);
  end;
  FTimer.OnConsequence := @OmissionEnd;
  FTimer.Start;
  FStimulus.Start;
  case FTrialType of
    ttE3C1, ttE3C2 : FSchedule.Start;
    else { do nothing };
  end;

  FReportData.CBegin:=LogEvent(rsReportStmBeg);
  FReportData.ComparisonBegin:=TimestampToStr(FReportData.CBegin);
  FResponseEnabled:=True;
  Invalidate;

  if CheatsModeOn then begin
    ParticipantBot.SetTargetControl(FStimulus.Target);
    ParticipantBot.Start;
  end;
end;

procedure TFreeSquareTrial.WriteData(Sender: TObject);
begin
  inherited WriteData(Sender);
  Data := Data +
          FReportData.ComparisonLatency + HeaderTabs +
          FReportData.ComparisonBegin + HeaderTabs +
          FReportData.ComparisonEnd;
end;

function TFreeSquareTrial.GetHeader: string;
begin
  Result :=  rsReportRspLat + HeaderTabs +
             rsReportStmBeg + HeaderTabs +
             rsReportStmEnd;
end;

procedure TFreeSquareTrial.OmissionEnd(Sender: TObject);
begin
  if CheatsModeOn then begin
    ParticipantBot.Stop;
  end;

  FTimer.Stop;
  FStimulus.Stop;
  case FTrialType of
    ttE3C1, ttE3C2 : FSchedule.Stop;
    else { do nothing };
  end;
  FResponseEnabled := False;
  Invalidate;

  case FTrialType of
    ttE1B, ttE1C, ttE3B1, ttE3B2: begin
      Result := T_MISS;
    end;

    ttE3C1, ttE3C2 : begin
      Result := T_NONE;
    end;
  end;

  case FTrialType of
    ttE1B, ttE1C : begin
      Configurations.SList.Values[_ITI] :=
        (TrialDuration - OmissionDurationExp1).ToString;
    end;

    ttE3B1, ttE3B2, ttE3C1, ttE3C2 : begin
      Configurations.SList.Values[_ITI] := ITIExperiment3.ToString;
    end;
  end;
  FReportData.ComparisonEnd := TimestampToStr(LogEvent(rsReportStmEndO));
  EndTrial(Self);
end;

procedure TFreeSquareTrial.DelayEnd(Sender: TObject);
begin
  FTimer.Stop;
  Parent.Color := clWhite;
  CounterManager.BlcPoints := CounterManager.BlcPoints +1;
  LogEvent(rsReportDelayEnd);
  EndTrial(Self);
end;

procedure TFreeSquareTrial.VTConsequence(Sender : TObject);
begin
  case FTrialType of
    ttE3C1, ttE3C2 : begin
      FResponseEnabled := False;
      FTimer.Pause;
      FSchedule.Stop;
      FSquareSchedule.Pause;
      FShowCounter := False;
      FStimulus.Hide;
      FConsequence.Start;
      Parent.Color := clPurple;
      if Sender = FSchedule then begin
        CounterManager.SessionPointsTopLeft :=
          CounterManager.SessionPointsTopLeft +1;
      end;
      FConsequenceTimer.Enabled := True;
      LogEvent('VT.+.Start');
      Invalidate;
    end

    else { do nothing };
  end;
end;

procedure TFreeSquareTrial.VTConsequenceEnd(Sender : TObject);
begin
  case FTrialType of
    ttE3C1, ttE3C2 : begin
      FConsequenceTimer.Enabled := False;
      FResponseEnabled := False;
      FConsequence.Stop;

      FShowCounter := True;
      FTimer.Start;
      FSchedule.Start;
      FStimulus.Start; // FSquareSchedule.Start;
      Parent.Color := clWhite;
      if Sender = FSchedule then begin
        CounterManager.SessionPointsTopLeft :=
          CounterManager.SessionPointsTopLeft +1;
      end;
      Invalidate;
      LogEvent('VT.+.Stop');
    end

    else { do nothing };
  end;
end;

procedure TFreeSquareTrial.Consequence(Sender: TObject);
var
  ITI : Extended;
begin
  if CheatsModeOn then begin
    ParticipantBot.Stop;
  end;

  FTimer.Stop;
  FStimulus.Stop;
  case FTrialType of
    ttE3C1, ttE3C2 : FSchedule.Stop;
    else { do nothing };
  end;
  FResponseEnabled := False;
  Invalidate;
  Result := T_HIT;

  case FTrialType of
    ttE1B : begin
      ITI := TrialDuration
        -ConsequenceInterval
        -(FReportData.CLatency - FReportData.CBegin);

      Configurations.SList.Values[_ITI] := ITI.ToString;
      CounterManager.BlcPoints := CounterManager.BlcPoints +1;
      FReportData.ComparisonEnd:=TimestampToStr(LogEvent(rsReportStmEndR));
      EndTrial(Self);
    end;

    ttE1C : begin
      ITI := TrialDuration
        -ConsequenceInterval
        -ConsequenceDelay
        -(FReportData.CLatency - FReportData.CBegin);

      Configurations.SList.Values[_ITI] := ITI.ToString;
      Parent.Color := clPurple;
      FTimer.Load(FT, ConsequenceDelay);
      FTimer.OnConsequence := @DelayEnd;
      FTimer.Start;
      FReportData.ComparisonEnd := TimestampToStr(LogEvent(rsReportDelayBeg));
    end;

    ttE3B1: begin
      ITI := ITIExperiment3;
      Configurations.SList.Values[_ITI] := ITI.ToString;
      CounterManager.SessionPointsTopRight :=
        CounterManager.SessionPointsTopRight +1;
      FReportData.ComparisonEnd:=TimestampToStr(LogEvent(rsReportStmEndR));
      EndTrial(Self);
    end;

    ttE3B2 : begin
      ITI := ITIExperiment3;
      Configurations.SList.Values[_ITI] := ITI.ToString;
      CounterManager.SessionPointsTopRight :=
        CounterManager.SessionPointsTopRight +1;
      FReportData.ComparisonEnd:=TimestampToStr(LogEvent(rsReportStmEndR));
      EndTrial(Self);
    end;

    ttE3C1, ttE3C2 : begin
      ITI := ITIExperiment3;
      Configurations.SList.Values[_ITI] := ITI.ToString;

      if Sender = FStimulus.Schedule then begin
        CounterManager.SessionPointsTopRight :=
          CounterManager.SessionPointsTopRight +1;
      end;

      FReportData.ComparisonEnd:=TimestampToStr(LogEvent(rsReportStmEndR));
      EndTrial(Self);
    end;
  end;
end;

procedure TFreeSquareTrial.Response(Sender: TObject);
begin
  if FFirstResponse then
  begin
    FFirstResponse := False;
    FReportData.CLatency := LogEvent(rsReportRspLat);
    FReportData.ComparisonLatency:=TimestampToStr(FReportData.CLatency);
  end;
end;

procedure TFreeSquareTrial.TrialBeforeEnd(Sender: TObject);
begin
  WriteData(Self);
end;


end.

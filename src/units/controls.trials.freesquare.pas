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
    CVTDelayBegin : Extended;
    CVTDelayEnd : Extended;
    CEnd     : Extended;
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
  , Cheats, Session.Configuration.GlobalContainer
  , Experiments.Eduardo.Experimento1.Tables
  , Experiments.Eduardo.Experimento3.Tables;

constructor TFreeSquareTrial.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialStart := @TrialStart;

  if Self.ClassType = TFreeSquareTrial then
    Header := Header + #9 + GetHeader;

  Configurations.Parameters.Values['VTConsequence'] := 'acerto.png';
  FConsequence := TStimulusFigure.Create(Self);
  FConsequence.Key := 'VTConsequence';
  FConsequence.HideCursor;
  FConsequence.LoadFromParameters(Configurations.Parameters);
  FConsequence.Parent := AOwner;

  FConsequenceTimer := TTimer.Create(Self);
  FConsequenceTimer.Interval := FreeSquareConsequenceDurationExp3;
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
    ttE1B, ttE1C, ttE3B1, ttE3B2 : Result := FreeSquareConsequenceDurationExp1;
    ttE3C1, ttE3C2 : begin
      case Self.Result of
        T_MISS, T_HIT : Result := FreeSquareConsequenceDurationExp3;
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
  LParameters := Configurations.Parameters;
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
    ttE1B,  ttE1C  : FTimer.Load(FT, FreeSquareOmissionDurationExp1);
    ttE3B1, ttE3C1 : FTimer.Load(FT, FreeSquareOmissionDurationExp3Training);
    ttE3B2, ttE3C2 : FTimer.Load(FT, FreeSquareOmissionDurationExp3Testing);
  end;
  FTimer.OnConsequence := @OmissionEnd;
  FTimer.Start;
  FStimulus.Start;
  case FTrialType of
    ttE3C1, ttE3C2 : FSchedule.Start;
    else { do nothing };
  end;

  FReportData.CBegin:=LogEvent('Quadrado.Inicio');
  FResponseEnabled:=True;
  Invalidate;

  if CheatsModeOn then begin
    ParticipantBot.Start(FStimulus.AsInterface);
  end;
end;

procedure TFreeSquareTrial.WriteData(Sender: TObject);
var
  LLatency : Extended = -1;
  LResponsesTime : Extended;
  function IsTestTrial : Boolean;
  begin
    Result := Configurations.Parameters.Values['IsTestTrial'].ToBoolean;
  end;
begin
  inherited WriteData(Sender);
  LResponsesTime := FReportData.CEnd-FReportData.CBegin;
  if not FFirstResponse then begin
    LLatency := FReportData.CLatency-FReportData.CBegin;
  end;
  case FTrialType of
    ttE1B, ttE1C : begin
      TExperiment1Table(FTable).AddRow(
        LLatency, ResultAsInteger);
    end;
    ttE3B1, ttE3B2, ttE3C1, ttE3C2 : begin
      TExperiment3Table(FTable).AddRow(
        Counters.CurrentTrial,
        LLatency, LResponsesTime, ResultAsInteger, IsTestTrial);
    end;
  end;

  Data := Data +
          TimestampToStr(FReportData.CLatency) + HeaderTabs +
          TimestampToStr(FReportData.CBegin) + HeaderTabs +
          TimestampToStr(FReportData.CEnd);
end;

function TFreeSquareTrial.GetHeader: string;
begin
  Result :=  rsReportRspLat + HeaderTabs +
             rsReportStmBeg + HeaderTabs +
             rsReportStmEnd;
end;

procedure TFreeSquareTrial.OmissionEnd(Sender: TObject);
begin
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
      Configurations.Parameters.Values[_ITI] :=
        (FreeSquareTrialDuration - FreeSquareOmissionDurationExp1).ToString;
    end;

    ttE3B1, ttE3B2, ttE3C1, ttE3C2 : begin
      Configurations.Parameters.Values[_ITI] := ITIExperiment3.ToString;
    end;
  end;
  FReportData.CEnd := LogEvent('Omissao.Fim');
  EndTrial(Self);
end;

procedure TFreeSquareTrial.DelayEnd(Sender: TObject);
begin
  FTimer.Stop;
  Parent.Color := clWhite;
  Counters.BlcPoints := Counters.BlcPoints +1;
  LogEvent('Atraso.Fim');
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
      Parent.Color := clDarkGreen;
      if Sender = FSchedule then begin
        Counters.SessionPointsTopLeft :=
          Counters.SessionPointsTopLeft +1;
      end;
      FConsequenceTimer.Enabled := True;
      FReportData.CVTDelayBegin := LogEvent('VT.Inicio');
      Invalidate;
    end

    else { do nothing };
  end;
end;

procedure TFreeSquareTrial.VTConsequenceEnd(Sender : TObject);
var
  LDelay: Extended;
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
        Counters.SessionPointsTopLeft :=
          Counters.SessionPointsTopLeft +1;
      end;
      Invalidate;
      FReportData.CVTDelayEnd := LogEvent('VT.Fim');
      LDelay := FReportData.CVTDelayEnd - FReportData.CVTDelayBegin;
      TExperiment3Table(FTable).AddDelay(
        Counters.CurrentTrial, LDelay);
    end

    else { do nothing };
  end;
end;

procedure TFreeSquareTrial.Consequence(Sender: TObject);
var
  ITI : Extended;
begin
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
      ITI := FreeSquareTrialDuration
        -ConsequenceInterval
        -(FReportData.CLatency - FReportData.CBegin);

      Configurations.Parameters.Values[_ITI] := ITI.ToString;
      Counters.BlcPoints := Counters.BlcPoints +1;
      FReportData.CEnd:=LogEvent('Reforco');
      EndTrial(Self);
    end;

    ttE1C : begin
      ITI := FreeSquareTrialDuration
        -ConsequenceInterval
        -FreeSquareConsequenceDelay
        -(FReportData.CLatency - FReportData.CBegin);

      Configurations.Parameters.Values[_ITI] := ITI.ToString;
      Parent.Color := clDarkGreen;
      FTimer.Load(FT, FreeSquareConsequenceDelay);
      FTimer.OnConsequence := @DelayEnd;
      FTimer.Start;
      FReportData.CEnd := LogEvent('Atraso.Inicio');
    end;

    ttE3B1, ttE3B2 : begin
      ITI := ITIExperiment3;
      Configurations.Parameters.Values[_ITI] := ITI.ToString;
      Counters.SessionPointsTopRight :=
        Counters.SessionPointsTopRight +1;
      FReportData.CEnd:=LogEvent('Reforco');
      EndTrial(Self);
    end;

    ttE3C1, ttE3C2 : begin
      ITI := ITIExperiment3;
      Configurations.Parameters.Values[_ITI] := ITI.ToString;

      if Sender = FStimulus then begin
        Counters.SessionPointsTopRight :=
          Counters.SessionPointsTopRight +1;
      end;

      FReportData.CEnd:=LogEvent('Reforco');
      EndTrial(Self);
    end;
  end;
end;

procedure TFreeSquareTrial.Response(Sender: TObject);
var
  LResponse : Extended;
  function IsTestTrial : Boolean;
  begin
    Result := Configurations.Parameters.Values['IsTestTrial'].ToBoolean;
  end;

begin
  LResponse := LogEvent('Resposta.Latencia');
  if FFirstResponse then
  begin
    FFirstResponse := False;
    FReportData.CLatency := LResponse
  end;
  case FTrialType of
    ttE3B1, ttE3C1, ttE3B2, ttE3C2 : begin
      TExperiment3Table(FTable).AddResponse(
        Counters.CurrentTrial, LResponse);
    end;
    else { do nothing };
  end;
end;

procedure TFreeSquareTrial.TrialBeforeEnd(Sender: TObject);
begin
  WriteData(Self);
end;


end.

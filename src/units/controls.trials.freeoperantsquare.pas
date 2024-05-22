{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.FreeOperantSquare;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils, ExtCtrls

  , Controls.Trials.Abstract
  , Stimuli.Game.FreeSquare
  , Stimuli.Image
  , Schedules
  ;

type

  TTrialType = (ttA1, ttA2, ttB1, ttB2, ttB3, ttC1);

  TReportData = record
    CBegin : Extended;
    CLatency : Extended;
    CTimeLatency : Extended;
    ComparisonBegin   : string;
    ComparisonEnd     : string;
    ComparisonLatency : string;
    DelayEnd  : string;
  end;

  { TFreeOperantSquareTrial }

  {
    Implements Free Operant Square
    Square moves around
    Clicks produces consequences
    LimitedHold produces diferential consequences
  }
  TFreeOperantSquareTrial = class(TTrial)
  private
    FSchedule : TSchedule;
    FSquareSchedule : TSchedule;
    FTrialType : TTrialType;
    FResponseReady : Boolean;
    FFirstResponse : Boolean;
    FReportData : TReportData;
    FContainer : TPanel;
    FStimulus : TFreeSquare;
    procedure LoosePoints;
    procedure ConditionalStimulusStarted(Sender: TObject);
    //procedure ConditionalStimulusStopped(Sender: TObject);
    procedure ConditionalStimulusStoppedHighTone(Sender: TObject);
    procedure ConditionalStimulusStopped8Seconds(Sender: TObject);
    //procedure CustomConsequenceStop(Sender: TObject);
    procedure Consequence(Sender: TObject);
    procedure Response(Sender: TObject);
    procedure ResponseReady(Sender: TObject);
    procedure TrialBeforeEnd(Sender: TObject);
    function GetHeader: string;
  protected
    procedure Paint; override;
    procedure TrialStart(Sender: TObject); virtual;
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TCustomControl); override;
    function AsString : string; override;
    function HasVisualConsequence: Boolean; override;
    function ConsequenceInterval: integer; override;
    procedure Play(ACorrection : Boolean); override;
  end;

var
  GainVI: integer = 10;
  LossVT: integer = 7;

implementation

uses Constants, Timestamps, Graphics, Cheats,
  Consequences.Custom, CS.Sound, Session.CastleUtils;

constructor TFreeOperantSquareTrial.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialStart := @TrialStart;
  if Self.ClassType = TFreeOperantSquareTrial then
    Header := Header + #9 + GetHeader;

  Parent.Color := clGreen;
  FContainer := TPanel.Create(Self);
  FContainer.Color := clWhite;
  FContainer.Width := (Self.Width div 3)*2;
  FContainer.Height := (Self.Height div 3)*2;
  FContainer.Left := (Width div 2) -(FContainer.Width div 2);
  FContainer.Top := (Height div 2) -(FContainer.Height div 2);
  FContainer.Parent := Self.Parent;

  FFirstResponse := True;
  FResponseEnabled := False;

  HeaderTimestamps := HeaderTimestamps + #9 +
    'Total.Contador.Centro' + #9 +
    'Total.Contador.Direita' + #9 +
    'VI.Real';
end;

function TFreeOperantSquareTrial.AsString: string;
begin
  // todo
  Result := '';
end;

function TFreeOperantSquareTrial.HasVisualConsequence: Boolean;
begin
  Result := (Self.Result <> T_NONE);
end;

function TFreeOperantSquareTrial.ConsequenceInterval: integer;
begin
  Result := 0;
end;

procedure TFreeOperantSquareTrial.Play(ACorrection: Boolean);
var
  LDuration : integer = (1000*60*15) + 100;
  LParameters : TStringList;
begin
  LParameters := Configurations.Parameters;
  case LParameters.Values['Type'] of
    'A1': FTrialType := ttA1;
    'A2':
      begin
        FTrialType := ttA2;
        LDuration := 1000*60*10;
      end;
    'B1': FTrialType := ttB1;
    'B2': FTrialType := ttB2;
    'B3': FTrialType := ttB3;
    'C1': FTrialType := ttC1;
  end;

  CustomConsequence.Visual.Parent := FContainer;
  Configurations.Parameters.Values[_LimitedHold] := LDuration.ToString;
  inherited Play(ACorrection);

  FStimulus := TFreeSquare.Create(Self);
  case FTrialType of
    ttA1, ttB1, ttB2 : begin
      FStimulus.Schedule.Load(VI);
      FStimulus.Schedule.UseRegisNetoIntervals(True);
    end;

    ttA2, ttB3, ttC1 : begin
      FStimulus.Schedule.Load(VI);
      FStimulus.Schedule.UseRegisNetoIntervals(False);
    end;
  end;

  case FTrialType of
    ttA1: begin
      { do nothing }
    end;

    ttA2 : begin
      { do nothing }
    end;

    ttB1, ttB2: begin
      { do nothing }
    end;

    ttB3, ttC1 :  begin
      SerialSound.OnStart := @ConditionalStimulusStarted;
      //SerialSound.OnStopTone := @ConditionalStimulusStopped;
      //SerialSound.OnStopLaught := @CustomConsequenceStop;
      SerialSound.OnStop8Seconds := @ConditionalStimulusStopped8Seconds;
      SerialSound.OnStopToneHigh := @ConditionalStimulusStoppedHighTone;
    end;
  end;

  case FTrialType of
    ttB3 : SerialSound.PresentationPattern := ppB3;
    ttC1 : SerialSound.PresentationPattern := ppC1;
    else {do nothing};
  end;

  FSchedule := TSchedule.Create(Self);
  FSchedule.Load(FT, 10000);
  FSchedule.OnConsequence := @Consequence;

  FCounterType := ctCenterTopRight;

  FStimulus.Parent := FContainer;
  FStimulus.LoadFromParameters(LParameters);
  FStimulus.OnConsequence := @Consequence;
  FStimulus.OnResponse := @Response;
  FStimulus.Schedule.OnResponseReady := @ResponseReady;
  FSquareSchedule := FStimulus.Schedule;
  FStimulus.FitScreen;
  FShowCounter := True;

  if Self.ClassType = TFreeOperantSquareTrial then Config(Self);
end;

procedure TFreeOperantSquareTrial.TrialStart(Sender: TObject);
begin
  SessionUpdater.Start;
  FStimulus.Start;
  FSchedule.Start;
  case FTrialType of
    ttB3, ttC1 : SerialSound.StartPlayingFromPattern;
    else { do nothing };
  end;
  FReportData.CBegin:=LogEvent('OperanteLivre.Inicio');
  FReportData.ComparisonBegin:=TimestampToStr(FReportData.CBegin);
  FResponseEnabled:=True;
  Invalidate;

  if CheatsModeOn then begin
    ParticipantBot.Start(FStimulus.AsInterface);
  end;
end;

procedure TFreeOperantSquareTrial.WriteData(Sender: TObject);
begin
  inherited WriteData(Sender);
  Data := Data + FReportData.ComparisonLatency + HeaderTabs +
    FReportData.ComparisonBegin + HeaderTabs +
    FReportData.ComparisonEnd + HeaderTabs +
    GainVI.ToString + HeaderTabs +
    LossVT.ToString + HeaderTabs +
    CounterManager.SessionPointsCenter.ToString + HeaderTabs +
    CounterManager.SessionPointsTopRight.ToString;
end;

function TFreeOperantSquareTrial.GetHeader: string;
begin
  Result := rsReportRspLat + HeaderTabs +
            rsReportStmBeg + HeaderTabs +
            rsReportStmEnd + HeaderTabs +
            'Ganho.Contador.Centro' + HeaderTabs +
            'Perda.Contador.Direita' + HeaderTabs +
            'Total.Contador.Centro' + HeaderTabs +
            'Total.Contador.Direita';
end;

procedure TFreeOperantSquareTrial.Paint;
begin
  inherited Paint;
  if CheatsModeOn then begin
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := 5;
    if FResponseReady then begin
      Canvas.Brush.Color := clGreen;
    end else begin
      Canvas.Brush.Color := clRed;
    end;
    Canvas.Ellipse(0, 0, 50, 50);
  end;
end;

procedure TFreeOperantSquareTrial.LoosePoints;
var
  LPoints : integer;
begin
  case FTrialType of
    ttB3 : begin
      LPoints := CounterManager.SessionPointsTopRight - LossVT;
    end;
    else begin
      LPoints := CounterManager.SessionPointsTopRight - 13;
    end;
  end;

  if LPoints < 0 then LPoints := 0;
  CounterManager.SessionPointsTopRight := LPoints;
  Invalidate;
end;

procedure TFreeOperantSquareTrial.ConditionalStimulusStarted(Sender: TObject);
begin
  case FTrialType of
    ttB3, ttC1 : begin
      Parent.Color := clGray;
      LogEvent('TelaCinza.Inicio');
    end;
    else begin
      { do nothing }
    end;
  end;
end;

//procedure TFreeOperantSquareTrial.ConditionalStimulusStopped(Sender: TObject);
//begin
//  case FTrialType of
//    ttA1, ttA2, ttC1 : begin
//      { do nothing }
//    end;
//
//    ttB1 : begin
//      //LoosePoints;
//    end;
//
//    ttB2 : begin
//      CustomConsequence.Start;
//    end;
//
//    ttB3 : begin
//      CustomConsequence.Start;
//    end;
//  end;
//  LogEvent('TomBaixo.Fim');
//end;

procedure TFreeOperantSquareTrial.ConditionalStimulusStoppedHighTone(Sender: TObject);
begin
  Parent.Color := clGreen;
  LogEvent('TelaCinza.Fim');
  LoosePoints;
  LogEvent('TomAlto.Fim' + #9 + CounterManager.SessionPointsTopRight.ToString);
end;

procedure TFreeOperantSquareTrial.ConditionalStimulusStopped8Seconds(
  Sender: TObject);
begin
  case FTrialType of
    ttB3 : begin
      LogEvent('TomAlto30s.Inicio');
    end;
    ttC1 : begin
      LogEvent('TelaCinza.Fim');
      Parent.Color := clGreen;
    end
    else { do nothing };
  end;
end;

//procedure TFreeOperantSquareTrial.CustomConsequenceStop(Sender: TObject);
//begin
//  case FTrialType of
//    ttB2, ttB3 : begin
//      CustomConsequence.Stop;
//      LogEvent('Risada.Fim');
//    end;
//    else { do nothing };
//  end;
//end;

procedure TFreeOperantSquareTrial.Consequence(Sender: TObject);
begin
  if Sender = FSchedule then begin
    CounterManager.SessionPointsTopRight :=
      CounterManager.SessionPointsTopRight +1;
    LogEvent('VT.Consequencia' + #9#9 + CounterManager.SessionPointsTopRight.ToString);
  end;

  if Sender = FStimulus then begin
    FResponseReady := False;
    CounterManager.SessionPointsCenter :=
      CounterManager.SessionPointsCenter + GainVI;
    CoinSound.Start;
    LogEvent('VI.Consequencia' + #9 + CounterManager.SessionPointsCenter.ToString);

    case FTrialType of
      ttA1: begin
        if CounterManager.SessionPointsCenter = 50 then begin
          EndTrial(Self);
          Exit;
        end;
      end;
      else
      { do nothing };
    end;
  end;
  Invalidate;
end;

procedure TFreeOperantSquareTrial.Response(Sender: TObject);
begin
  //CustomConsequence.Play;
  if FFirstResponse then
  begin
    FFirstResponse := False;
    FReportData.CLatency := LogEvent('Quadrado.Resposta.Latencia');
    FReportData.ComparisonLatency:=TimestampToStr(FReportData.CLatency);
  end else begin
    LogEvent('Quadrado.Resposta');
  end;
end;

procedure TFreeOperantSquareTrial.ResponseReady(Sender: TObject);
begin
  if FResponseEnabled then begin
    LogEvent('VI.Pronto' + #9#9#9 + FSquareSchedule.AsString);
    if CheatsModeOn then begin
      FResponseReady := True;
      Invalidate;
    end;
  end;
end;

procedure TFreeOperantSquareTrial.TrialBeforeEnd(Sender: TObject);
begin
  FSchedule.Stop;
  WriteData(Self);
end;


end.

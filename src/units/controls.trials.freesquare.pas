{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

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
  ;

type

  TTrialType = (ttE1B, ttE1C, ttE3B1, ttE3B2);

  TReportData = record
    CBegin : Extended;
    CLatency : Extended;
    CEnd     : Extended;
    DelayEnd  : string;
  end;

  { TFreeSquareTrial }

  {
    Square moves around or fixed
    Clicks produces consequences
  }
  TFreeSquareTrial = class(TTrial)
  private
    FHasDelay : Boolean;
    FCustomResult : integer;
    FTrialType : TTrialType;
    FFirstResponse : Boolean;
    FReportData : TReportData;
    FStimulus : TFreeSquare;
    //FOmissionTimer : TTimer;
    procedure OmissionEnd(Sender: TObject);
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
    function ConsequenceDelay : Cardinal; override;
    function ConsequenceInterval: integer; override;
    procedure Play(ACorrection : Boolean); override;
  end;

implementation

uses Constants, Timestamps, Forms, Graphics
  , Cheats, Session.Configuration.GlobalContainer
  , Experiments.Eduardo.Experimento1.Tables
  , Experiments.Eduardo.Experimento3.Tables;

constructor TFreeSquareTrial.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialStart := @TrialStart;
  OnLimitedHold := @OmissionEnd;

  if Self.ClassType = TFreeSquareTrial then
    Header := Header + #9 + GetHeader;

  //FOmissionTimer := TTimer.Create(Self);
  //FOmissionTimer.Enabled := False;
  FHasDelay := False;
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

function TFreeSquareTrial.ConsequenceDelay: Cardinal;
begin
  if FHasDelay then begin
    Result := FreeSquareConsequenceDelay;
  end else begin
    Result := 0;
  end;
end;

function TFreeSquareTrial.ConsequenceInterval: integer;
begin
  case FTrialType of
    ttE1B, ttE1C, ttE3B1, ttE3B2 : Result := FreeSquareConsequenceDurationExp1;
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
  end;

  case FTrialType of
    ttE1B, ttE1C   : FCounterType := ctBlocPoints;
    ttE3B1, ttE3B2 : FCounterType := ctSessionPoints;
  end;

  FStimulus := TFreeSquare.Create(Self);
  case FTrialType of
    ttE1B, ttE1C : { do nothing };
    ttE3B1, ttE3B2 : begin
      FStimulus.Freeze;
      TExperiment3Table(FTable).CreateTrial(Counters.SessionTrials);
    end;
  end;

  FStimulus.Parent := Self.Parent;
  FStimulus.LoadFromParameters(LParameters);
  FStimulus.OnConsequence := @Consequence;
  FStimulus.OnResponse := @Response;
  FStimulus.FitScreen;
  FShowCounter := True;

  case FTrialType of
    ttE1B,  ttE1C  : LimitedHold := FreeSquareOmissionDurationExp1;
    ttE3B1 : LimitedHold := FreeSquareOmissionDurationExp3Training;
    ttE3B2 : begin
      if IsTestTrial then begin
        LimitedHold := FreeSquareOmissionDurationExp3Testing;
      end else begin
        LimitedHold := FreeSquareOmissionDurationExp3Training;
      end;
    end;
  end;

  if Self.ClassType = TFreeSquareTrial then Config(Self);
end;

procedure TFreeSquareTrial.TrialStart(Sender: TObject);
begin
  FStimulus.Start;
  FReportData.CBegin:=LogEvent('Quadrado.Inicio');

  FResponseEnabled:=True;
  Invalidate;

  if CheatsModeOn then begin
    if Random <= 0.9 then
      ParticipantBot.Start(FStimulus.AsInterface);
  end;
end;

procedure TFreeSquareTrial.WriteData(Sender: TObject);
var
  LLatency : Extended = -1;
  LResponsesTime : Extended;
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
    ttE3B1, ttE3B2 : begin
      TExperiment3Table(FTable).AddRow(
        Counters.SessionTrials,
        LLatency, LResponsesTime, FCustomResult, IsTestTrial);
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

procedure TFreeSquareTrial.SetCustomResult;
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

function TFreeSquareTrial.IsTestTrial: Boolean;
begin
  Result := Configurations.Parameters.Values['IsTestTrial'].ToBoolean;
end;

procedure TFreeSquareTrial.OmissionEnd(Sender: TObject);
begin
  if FResponseEnabled then begin
    Invalidate;
    FReportData.CEnd := LogEvent('Omissao.Fim');
    FStimulus.Stop;

    case FTrialType of
      ttE1B, ttE1C, ttE3B1, ttE3B2: begin
        Result := T_MISS;
      end;
    end;

    case FTrialType of
      ttE1B, ttE1C : begin
        Configurations.Parameters.Values[_ITI] :=
          (FreeSquareTrialDuration - FreeSquareOmissionDurationExp1).ToString;
      end;

      ttE3B1, ttE3B2 : begin
        Configurations.Parameters.Values[_ITI] := ITIExperiment3.ToString;
      end;
    end;
    SetCustomResult;
  end;
end;

procedure TFreeSquareTrial.Consequence(Sender: TObject);
var
  ITI : Extended;
begin
  LimitedHold := 0;
  FStimulus.Stop;
  FResponseEnabled := False;
  Invalidate;

  if CheatsModeOn then begin
    ParticipantBot.Stop;
  end;

  Result := T_HIT;
  SetCustomResult;
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
      Counters.BlcPoints := Counters.BlcPoints +1;
      FReportData.CEnd := LogEvent('Atraso.Inicio');
      FHasDelay := True;
      EndTrial(Self);
    end;

    ttE3B1, ttE3B2 : begin
      ITI := ITIExperiment3;
      Configurations.Parameters.Values[_ITI] := ITI.ToString;
      Counters.SessionPointsTopRight :=
        Counters.SessionPointsTopRight +1;
      FReportData.CEnd:=LogEvent('Reforco');
      EndTrial(Self);
    end;
  end;
end;

procedure TFreeSquareTrial.Response(Sender: TObject);
var
  LResponse : Extended;
begin
  if FFirstResponse then
  begin
    FFirstResponse := False;
    LResponse := LogEvent('Resposta.Latencia');
    FReportData.CLatency := LResponse
  end else begin
    LResponse := LogEvent('Resposta');
  end;
  case FTrialType of
    ttE3B1, ttE3B2 : begin
      TExperiment3Table(FTable).AddResponse(
        Counters.SessionTrials, LResponse);
    end;
    else { do nothing };
  end;
end;

procedure TFreeSquareTrial.TrialBeforeEnd(Sender: TObject);
begin
  WriteData(Self);
end;


end.

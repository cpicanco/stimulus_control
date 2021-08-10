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
  ;

type

  TTrialType = (ttB, ttC);

  TReportData = record
    CBegin : Extended;
    CLatency : Extended;
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
    FTrialType : TTrialType;
    FFirstResponse : Boolean;
    //FHasConsequence : Boolean;
    FReportData : TReportData;
    FStimulus : TFreeSquare;
    FTimer : TTimer;
    procedure OmissionEnd(Sender: TObject);
    procedure DelayEnd(Sender: TObject);
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

uses Constants, Timestamps, Session.ConfigurationFile, Graphics
  , Dialogs
  ;

constructor TFreeSquareTrial.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialStart := @TrialStart;

  if Self.ClassType = TFreeSquareTrial then
    Header := Header + #9 + GetHeader;

  FStimulus := TFreeSquare.Create(Self);
  FStimulus.Parent := Self.Parent;
  FResponseEnabled := False;

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval:= 5000;
  FTimer.OnTimer := @OmissionEnd;
  FFirstResponse := True;
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
  Result := 2000;
end;

procedure TFreeSquareTrial.Play(ACorrection: Boolean);
var
  LParameters : TStringList;
begin
  inherited Play(ACorrection);
  LParameters := Configurations.SList;
  case LParameters.Values['Type'] of
    'B' : FTrialType := ttB;
    'C' : FTrialType := ttC;
  end;
  FStimulus.LoadFromParameters(LParameters);
  FStimulus.OnConsequence := @Consequence;
  FStimulus.OnResponse := @Response;
  FStimulus.FitScreen;
  FShowCounter := True;
  if Self.ClassType = TFreeSquareTrial then Config(Self);
end;

procedure TFreeSquareTrial.TrialStart(Sender: TObject);
begin
  Invalidate;
  FResponseEnabled:=True;
  FStimulus.Start;
  FTimer.Enabled:=True;
  FReportData.CBegin:=LogEvent('C.Start');
  FReportData.ComparisonBegin:=TimestampToStr(FReportData.CBegin);
end;

procedure TFreeSquareTrial.WriteData(Sender: TObject);
const
  HeaderTabs : string = #9;
begin
  inherited WriteData(Sender);
  Data := Data +
          FReportData.ComparisonLatency + HeaderTabs +
          FReportData.ComparisonBegin + HeaderTabs +
          FReportData.ComparisonEnd + HeaderTabs +
          FReportData.DelayEnd;
end;

function TFreeSquareTrial.GetHeader: string;
begin
  Result :=  rsReportRspLat + #9 + // Latency
             'S.Inicio' + #9 +
             'S.Fim' + #9 +
             'Atraso.Fim';
end;

procedure TFreeSquareTrial.OmissionEnd(Sender: TObject);
begin
  FTimer.Enabled:=False;
  FStimulus.Stop;
  FResponseEnabled := False;
  Invalidate;
  Result := T_MISS;
  Configurations.SList.Values[_ITI] := '15000';
  EndTrial(Self);
end;

procedure TFreeSquareTrial.DelayEnd(Sender: TObject);
begin
  FTimer.Enabled:=False;
  Parent.Color := clWhite;
  CounterManager.BlcPoints := CounterManager.BlcPoints +1;
  EndTrial(Self);
end;

procedure TFreeSquareTrial.Consequence(Sender: TObject);
var
  ITI : Extended;
begin
  FTimer.Enabled:=False;
  FStimulus.Stop;
  FShowCounter:=False;
  FResponseEnabled := False;
  Invalidate;

  FReportData.ComparisonEnd:=TimestampToStr(LogEvent('C.End'));
  Result := T_HIT;


  case FTrialType of
    ttB :
      begin
        ITI := 20000-(FReportData.CLatency - FReportData.CBegin);
        Configurations.SList.Values[_ITI] := ITI.ToString;
        CounterManager.BlcPoints := CounterManager.BlcPoints +1;
        EndTrial(Self);
      end;
    ttC :
      begin
        ITI := 10000-(FReportData.CLatency - FReportData.CBegin);
        Configurations.SList.Values[_ITI] := ITI.ToString;
        Parent.Color := clPurple;
        FTimer.OnTimer:=@DelayEnd;
        FTimer.Interval := 10000;
        FTimer.Enabled:=True;
      end;
  end;
end;

procedure TFreeSquareTrial.Response(Sender: TObject);
begin
  if FFirstResponse then
  begin
    FFirstResponse := False;
    FReportData.CLatency := LogEvent('C.End');
    FReportData.ComparisonLatency:=TimestampToStr(FReportData.CLatency);
  end;
end;

procedure TFreeSquareTrial.TrialBeforeEnd(Sender: TObject);
begin
  WriteData(Self);
end;


end.

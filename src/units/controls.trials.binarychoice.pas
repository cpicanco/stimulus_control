{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.BinaryChoice;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils, StdCtrls

  , Controls.Trials.Abstract
  , Stimuli.Choice
  , Consequences
  , Schedules
  // , Dialogs
  ;

type

  TReportData = record
    ComparisonBegin   : string;
    ComparisonEnd     : string;
    ComparisonChosen  : string;
    ComparisonLatency : string;
  end;

  { TBinaryChoiceTrial }

  {
    Simple Discrimination variation
    with mouse events working
  }
  TBinaryChoiceTrial = class(TTrial)
  private
    //FHasConsequence : Boolean;
    FMessage : TBinaryChoiceMessage;
    FLabel : TLabel;
    FReportData : TReportData;
    FStimulus : TBinaryChoice;
    procedure Consequence(Sender: TObject);
    procedure TrialBeforeEnd(Sender: TObject);
    //procedure TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    //procedure TrialMouseDown(Sender: TObject;Button: TMouseButton;
    //  Shift:TShiftState; X,Y:Integer);
    function GetHeader: string;
  protected
    procedure TrialStart(Sender: TObject); virtual;
    {$IFDEF DEBUG}
    procedure TrialPaint;
    {$ENDIF}
    procedure WriteData(Sender: TObject); override;
    procedure TrialResult(Sender: TObject);
  public
    constructor Create(AOwner: TCustomControl); override;
    function AsString : string; override;
    function HasVisualConsequence: Boolean; override;
    procedure Play(ACorrection : Boolean); override;
  end;

implementation

uses Forms, Graphics, Constants, Timestamps;

constructor TBinaryChoiceTrial.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialStart := @TrialStart;
  //OnTrialKeyUp := @TrialKeyUp;
  //OnTrialMouseDown:=@TrialMouseDown;
  {$IFDEF DEBUG}
  OnTrialPaint := @TrialPaint;
  {$ENDIF}

  if Self.ClassType = TBinaryChoiceTrial then
    Header := Header + HeaderTabs + GetHeader;

  FStimulus := TBinaryChoice.Create(Self);
  FStimulus.Parent := Self.Parent;

  FLabel := TLabel.Create(Self);
  with FLabel do begin
    Visible := False;
    Cursor := 0;
    Align := alTop;
    Alignment := taCenter;
    Anchors := [akLeft,akRight];
    BorderSpacing.Top := 50;
    WordWrap := True;
    Font.Name := 'Arial';
    Font.Color := 0;
    Font.Size := 22;
    Layout:=tlCenter;
    Caption:='Clique na alternativa de sua preferência';
    //OnMouseUp := @MessageMouseUp;
  end;
  FLabel.Parent := Self.Parent;
  FResponseEnabled := False;
end;

function TBinaryChoiceTrial.AsString: string;
var
  LTrial : TStringList;
begin
  LTrial := TStringList.Create;
  LTrial.BeginUpdate;

  LTrial.EndUpdate;
  Result := LTrial.Text;
  LTrial.Free;
end;

function TBinaryChoiceTrial.HasVisualConsequence: Boolean;
begin
  Result := (Self.Result <> T_NONE) and StrToBool(IETConsequence);
end;

procedure TBinaryChoiceTrial.Play(ACorrection: Boolean);
var
  LParameters : TStringList;
begin
  inherited Play(ACorrection);
  LParameters := Configurations.SList;
  FStimulus.LoadFromParameters(LParameters);
  FMessage := FStimulus.MessageFromParameters(LParameters);
  FMessage.CurrentTrial := CounterManager.BlcTrials;
  FStimulus.LoadMessage(FMessage);
  FStimulus.SetScheduleConsequence(@Consequence);
  FStimulus.FitScreen;

  if Self.ClassType = TBinaryChoiceTrial then Config(Self);
end;

procedure TBinaryChoiceTrial.TrialResult(Sender: TObject);
begin
  Result := T_NONE;
end;

procedure TBinaryChoiceTrial.TrialStart(Sender: TObject);
begin
  Mouse.CursorPos := Point(Screen.Width div 2, Screen.Height div 2);
  FResponseEnabled:=True;
  FStimulus.Start;
  FReportData.ComparisonBegin:=TimestampToStr(LogEvent(rsReportStmCmpBeg));
end;

procedure TBinaryChoiceTrial.WriteData(Sender: TObject);
begin
  inherited WriteData(Sender);
  Data := Data +
          FReportData.ComparisonChosen + HeaderTabs +
          FReportData.ComparisonBegin + HeaderTabs +
          FReportData.ComparisonLatency
          ;
end;

function TBinaryChoiceTrial.GetHeader: string;
begin
  Result :=
    rsReportRspCmp + HeaderTabs +
    rsReportStmCmpBeg + HeaderTabs +
    rsReportRspCmpLat
    ;
end;

procedure TBinaryChoiceTrial.Consequence(Sender: TObject);
var
  S : string;
begin
  FResponseEnabled := False;
  S := TComponent(Sender).Name;
  FMessage.AComponentName := S;
  NextTrial := FStimulus.NextTrial(S);
  FReportData.ComparisonLatency:=TimestampToStr(LogEvent(S +'.Latency'));
  FReportData.ComparisonChosen:=S;

  FStimulus.NextNow(FMessage);
  S := FloatToStrF(FMessage.Now, ffFixed, 0, 2);
  Configurations.SList.Values[_Now] := S;

  S := FloatToStrF(FMessage.LastNow, ffFixed, 0, 2);
  Configurations.SList.Values[_LastNow] := S;

  FStimulus.Stop;
  EndTrial(Sender);
end;

procedure TBinaryChoiceTrial.TrialBeforeEnd(Sender: TObject);
begin
  TrialResult(Sender);
  WriteData(Self);
end;

end.

{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.BinaryChoice;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils

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
    function HasConsequence: Boolean; override;
    procedure Play(ACorrection : Boolean); override;
  end;

implementation

uses Constants, Timestamps, Dialogs;

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
    Header := Header + #9 + GetHeader;

  FStimulus := TBinaryChoice.Create(Self);
  FStimulus.Parent := Self.Parent;
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

function TBinaryChoiceTrial.HasConsequence: Boolean;
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
  FResponseEnabled:=True;
  FStimulus.Start;
  FReportData.ComparisonBegin:=TimestampToStr(LogEvent('C.Start'));
end;

procedure TBinaryChoiceTrial.WriteData(Sender: TObject);
const
  HeaderTabs : string = #9;
begin
  inherited WriteData(Sender);
  Data := Data +
          FReportData.ComparisonChosen + HeaderTabs +
          FReportData.ComparisonLatency + HeaderTabs +
          FReportData.ComparisonBegin + HeaderTabs +
          FReportData.ComparisonEnd;
end;

//procedure TBinaryChoiceTrial.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
//begin
//  if FResponseEnabled then
//  case Key of
//   32: { ShowMessage(Configurations.SList.Text) } ;
//   77, 109 {M} :
//     begin
//       FResponseEnabled := False;
//       FReportData.ComparisonLatency:=TimestampToStr(LogEvent('C.Latency'));
//       FReportData.ComparisonChosen:='R-M';
//       FStimulus.Stop;
//       EndTrial(Sender);
//     end;
//   66, 98  {B} :
//     begin
//       FResponseEnabled := False;
//       FReportData.ComparisonLatency:=TimestampToStr(LogEvent('C.Latency'));
//       FReportData.ComparisonChosen:='L-B';
//       FStimulus.Stop;
//       EndTrial(Sender);
//     end;
//  end;
//end;

//procedure TBinaryChoiceTrial.TrialMouseDown(Sender: TObject;
//  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//begin
//  // Do something
//end;

function TBinaryChoiceTrial.GetHeader: string;
begin
  Result :=  rsReportRspCmp + #9 +    // Msg of the stimulus that have ended the trial
             rsReportRspCmpLat + #9 + // Latency
             rsReportStmCmpBeg + #9 +
             rsReportStmCmpEnd
             ;
end;

procedure TBinaryChoiceTrial.Consequence(Sender: TObject);
var
  LName : string;
begin
  FResponseEnabled := False;
  LName := TComponent(Sender).Name;
  NextTrial := FStimulus.NextTrial(LName);
  FReportData.ComparisonLatency:=TimestampToStr(LogEvent(LName+'.Latency'));
  FReportData.ComparisonChosen:=LName;
  FStimulus.Stop;
  EndTrial(Sender);
end;

procedure TBinaryChoiceTrial.TrialBeforeEnd(Sender: TObject);
begin
  TrialResult(Sender);
  WriteData(Self);
end;

end.

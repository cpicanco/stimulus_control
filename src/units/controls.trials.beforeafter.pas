{
  Stimulus Control
  Copyright (C) 2014-2020 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.BeforeAfter;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils

  , Controls.Trials.Abstract
  , Stimuli.Sequence.BeforeAfter
  , Consequences
  // , Dialogs
  ;

type

  TReportData = record
    ComparisonBegin   : string;
    ComparisonEnd     : string;
    ComparisonChosen  : string;
    ComparisonLatency : string;
  end;

  { TBeforeAfter }

  {
    Implements Simple Discriminations
    Presents comparisons
  }
  TBeforeAfter = class(TTrial)
  private
    FHasConsequence : Boolean;
    FReportData : TReportData;
    FStimulus : TBeforeAfterSequence;
    FParticipantResponse : TExpectedResponse;
    procedure Consequence(Sender: TObject);
    procedure TrialBeforeEnd(Sender: TObject);
    procedure TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    function GetHeader: string;
  protected
    procedure TrialStart(Sender: TObject); virtual;
    {$IFDEF DEBUG}
    procedure TrialPaint;
    {$ENDIF}
    procedure WriteData(Sender: TObject); override;
    procedure TrialResult(Sender: TObject);
    procedure EnableResponses(Sender : TObject);
  public
    constructor Create(AOwner: TCustomControl); override;
    function AsString : string; override;
    function HasConsequence: Boolean;
    procedure Play(ACorrection : Boolean); override;
  end;

implementation

uses Constants, Timestamps, Session.ConfigurationFile;

constructor TBeforeAfter.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialStart := @TrialStart;
  OnTrialKeyUp := @TrialKeyUp;
  {$IFDEF DEBUG}
  OnTrialPaint := @TrialPaint;
  {$ENDIF}

  if Self.ClassType = TBeforeAfter then
    Header := Header + #9 + GetHeader;

  FStimulus := TBeforeAfterSequence.Create(Self);
  FStimulus.Parent := Self.Parent;
  FResponseEnabled := False;
end;

function TBeforeAfter.AsString: string;
var
  LTrial : TStringList;
  //i : integer;
begin
  LTrial := TStringList.Create;
  LTrial.BeginUpdate;
  //if Self.ClassType = TBeforeAfter then
  //begin
  //  LTrial.Append(TConfigurationFile.FullTrialSection(
  //    CounterManager.CurrentBlc, CounterManager.CurrentTrial));
  //  LTrial.Values[_Kind] := T_Simple;
  //  LTrial.Values[_Cursor] := IntToStr(Cursor);
  //  LTrial.Values[_LimitedHold] := IntToStr(LimitedHold);
  //end;
  //
  //LTrial.Values[_NumComp] := IntToStr(Length(FComparisons));
  //for i := Low(FComparisons) to High(FComparisons) do
  //  with FComparisons[i] do
  //  begin
  //    LTrial.Values[_Comp + IntToStr(i + 1) + _cSch] := Key.Schedule.AsString;
  //    LTrial.Values[_Comp + IntToStr(i + 1) + _cBnd] := Key.BoundsAsString;
  //    LTrial.Values[_Comp + IntToStr(i + 1) + _cStm] := ExtractFileName(Key.Filename);
  //    LTrial.Values[_Comp + IntToStr(i + 1) + _cMsg] := Msg;
  //    LTrial.Values[_Comp + IntToStr(i + 1) + _cRes] := Res;
  //    LTrial.Values[_Comp + IntToStr(i + 1) + _cNxt] := Nxt;
  //    LTrial.Values[_Comp + IntToStr(i + 1) + _cIET] := IET;
  //  end;
  LTrial.EndUpdate;
  Result := LTrial.Text;
  LTrial.Free;
end;

function TBeforeAfter.HasConsequence: Boolean;
begin
  Result := (Self.Result <> T_NONE) and StrToBool(IETConsequence);
end;

procedure TBeforeAfter.Play(ACorrection: Boolean);
var
  LParameters : TStringList;
begin
  inherited Play(ACorrection);
  LParameters := Configurations.Parameters;
  FStimulus.LoadFromParameters(LParameters);
  FStimulus.FitScreen;
  FStimulus.OnEndSerialTimer:=@EnableResponses;

  if Self.ClassType = TBeforeAfter then Config(Self);
end;

procedure TBeforeAfter.TrialResult(Sender: TObject);
begin
  Result := T_NONE;

  if FStimulus.ExpectedResponse = FParticipantResponse then
    Result := T_HIT
  else
    Result:=T_MISS;

  //if HasConsequence then
  //  Consequences.Play(NextConsequence(Result = T_HIT));
end;

procedure TBeforeAfter.EnableResponses(Sender: TObject);
begin
  Invalidate;
  FResponseEnabled:=True;
  FReportData.ComparisonEnd:=TimestampToStr(LogEvent('C.End'));
end;

procedure TBeforeAfter.TrialStart(Sender: TObject);
begin
  FResponseEnabled:=False;
  FStimulus.Start;
  FReportData.ComparisonBegin:=TimestampToStr(LogEvent('C.Start'));
end;

{$IFDEF DEBUG}

procedure TBeforeAfter.TrialPaint;
var
  s : string;
begin
  WriteStr(s, FStimulus.ExpectedResponse);
  Canvas.Pen.Color:=0;
  Canvas.TextOut(
    Canvas.ClipRect.Right div 2,
    Canvas.ClipRect.Bottom div 2,
    s + ',' +
    CounterManager.CurrentBlc.ToString + ',' +
    CounterManager.CurrentTrial.ToString);
end;

{$ENDIF}

procedure TBeforeAfter.WriteData(Sender: TObject);
const
  HeaderTabs : string = #9;
var

  I : Integer;
begin
  inherited WriteData(Sender);
  Data := Data +
          FReportData.ComparisonChosen + HeaderTabs +
          FReportData.ComparisonLatency + HeaderTabs +
          FReportData.ComparisonBegin + HeaderTabs +
          FReportData.ComparisonEnd;
end;

procedure TBeforeAfter.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FResponseEnabled then
  case Key of
   32: { ShowMessage(Configurations.SList.Text) } ;
   77, 109 {M} :
     begin
       FResponseEnabled := False;
       FReportData.ComparisonLatency:=TimestampToStr(LogEvent('C.Latency'));
       FReportData.ComparisonChosen:='R-M';
       FParticipantResponse := TExpectedResponse.Right;
       FStimulus.Stop;
       EndTrial(Sender);
     end;
   66, 98  {B} :
     begin
       FResponseEnabled := False;
       FReportData.ComparisonLatency:=TimestampToStr(LogEvent('C.Latency'));
       FReportData.ComparisonChosen:='L-B';
       FParticipantResponse := TExpectedResponse.Left;
       FStimulus.Stop;
       EndTrial(Sender);
     end;
  end;
end;

function TBeforeAfter.GetHeader: string;
begin
  Result :=  rsReportRspCmp + #9 +    // Msg of the stimulus that have ended the trial
             rsReportRspCmpLat + #9 + // Latency
             rsReportStmCmpBeg + #9 +
             rsReportStmCmpEnd
             ;
end;

procedure TBeforeAfter.Consequence(Sender: TObject);
begin

end;

procedure TBeforeAfter.TrialBeforeEnd(Sender: TObject);
begin
  TrialResult(Sender);
  WriteData(Self);
end;


end.

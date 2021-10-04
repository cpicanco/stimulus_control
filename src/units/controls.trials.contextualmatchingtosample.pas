{
  Stimulus Control
  Copyright (C) 2014-2020 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.ContextualMatchingToSample;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils

  , Controls.Trials.Abstract
  , Stimuli.Sequence.ContextualMatchingToSample
  , Consequences
  ;

type

  { TCMTS }

  {
    Implements a contextual MTS
    contextual stimulus at the top
    sample at the center
    comparisons at the bottom
    presentation from bottom to top
  }
  TCMTS = class(TTrial)
  private
    //FConsequence : TConsequence;
    FLatency : string;
    FStimulus : TCMTSSequence;
    FParticipantResponse : TCMTSExpectedResponse;
    procedure Consequence(Sender: TObject);
    procedure TrialBeforeEnd(Sender: TObject);
    procedure TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    function GetHeader: string;
  protected
    procedure TrialStart(Sender: TObject); virtual;
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

constructor TCMTS.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialStart := @TrialStart;
  OnTrialKeyUp := @TrialKeyUp;

  if Self.ClassType = TCMTS then
    Header := Header + #9 + GetHeader;

  FStimulus := TCMTSSequence.Create(Self);
  FStimulus.Parent := Self.Parent;
  FResponseEnabled := False;
end;

function TCMTS.AsString: string;
var
  LTrial : TStringList;
  //i : integer;
begin
  LTrial := TStringList.Create;
  LTrial.BeginUpdate;
  //if Self.ClassType = TCMTS then
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

function TCMTS.HasConsequence: Boolean;
begin
  Result := (Self.Result <> T_NONE) and StrToBool(IETConsequence);
end;

procedure TCMTS.Play(ACorrection: Boolean);
var
  LParameters : TStringList;
begin
  inherited Play(ACorrection);
  LParameters := Configurations.Parameters;
  FStimulus.LoadFromParameters(LParameters);
  FStimulus.FitScreen;
  FStimulus.OnEndSerialTimer:=@EnableResponses;

  if Self.ClassType = TCMTS then Config(Self);
end;

procedure TCMTS.TrialResult(Sender: TObject);
begin
  Result := T_NONE;

  if FStimulus.ExpectedResponse = FParticipantResponse then
    Result := T_HIT
  else
    Result:=T_MISS;

  //if HasConsequence then
  //  Consequences.Play(NextConsequence(Result = T_HIT));
end;

procedure TCMTS.EnableResponses(Sender: TObject);
begin
  FResponseEnabled:=True;
end;

procedure TCMTS.TrialStart(Sender: TObject);
begin
  FResponseEnabled:=False;
  FStimulus.Start;
end;

procedure TCMTS.WriteData(Sender: TObject);
begin
  inherited WriteData(Sender);
  Data := Data + FLatency;
end;

procedure TCMTS.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FResponseEnabled then
  case Key of
   77, 109 {M} :
     begin
       FLatency := TimestampToStr(LogEvent('KeyUp '+Char(Key)));
       FResponseEnabled := False;
       FParticipantResponse := TCMTSExpectedResponse.Right;
       FStimulus.Stop;
       EndTrial(Sender);
     end;
   66, 98  {B} :
     begin
       FLatency := TimestampToStr(LogEvent('KeyUp '+Char(Key)));
       FResponseEnabled := False;
       FParticipantResponse := TCMTSExpectedResponse.Left;
       FStimulus.Stop;
       EndTrial(Sender);
     end;
  end;
end;

function TCMTS.GetHeader: string;
begin
  Result :=  rsReportRspCmpLat; // Latency
end;

procedure TCMTS.Consequence(Sender: TObject);
begin

end;

procedure TCMTS.TrialBeforeEnd(Sender: TObject);
begin
  TrialResult(Sender);
  WriteData(Self);
end;


end.

{
  Stimulus Control
  Copyright (C) 2014-2020 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.RelationalFrame;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils

  , Controls.Trials.Abstract
  , Stimuli.Sequence.RelationalFrame
  , Keyboard.FrameResponse
  , Keyboard.KeySequence
  , Consequences
  ;

type

  { TFrame }

  {
    Implements a Relational Frame
    using Same-Different and Before-After contextual cues
  }
  TFrame = class(TTrial)
  private
    FKeyCount : ShortInt;
    FLatency : string;
    FStimulus : TRFTSequence;
    FResponse : TKeySequence;
    FExpectedResponse : TFrameResponse;
    procedure TrialBeforeEnd(Sender: TObject);
    procedure TrialEndKeyCapturing(Sender: TObject);
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
    function HasConsequence: Boolean; override;
    procedure Play(ACorrection : Boolean); override;
  end;

implementation

uses Constants, Timestamps, Session.ConfigurationFile, Dialogs;

constructor TFrame.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialStart := @TrialStart;
  OnTrialKeyUp := @TrialKeyUp;

  if Self.ClassType = TFrame then
    Header := Header + #9 + GetHeader;

  FStimulus := TRFTSequence.Create(Self);
  FStimulus.Parent := Self.Parent;
  FExpectedResponse := FStimulus.ExpectedResponse;
  FResponseEnabled := False;
  FKeyCount:= 0;
end;

function TFrame.AsString: string;
var
  LTrial : TStringList;
  //i : integer;
begin
  LTrial := TStringList.Create;
  LTrial.BeginUpdate;
  //if Self.ClassType = TBeforeAfter then
  //begin
  //  LTrial.Append(TConfigurationFile.FullTrialSection(
  //    Counters.CurrentBlc, Counters.CurrentTrial));
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

function TFrame.HasConsequence: Boolean;
begin
  Result := (Self.Result <> T_NONE) and StrToBool(IETConsequence);
end;

procedure TFrame.Play(ACorrection: Boolean);
var
  LParameters : TStringList;
begin
  inherited Play(ACorrection);
  LParameters := Configurations.SList;
  FStimulus.LoadFromParameters(LParameters);
  FStimulus.FitScreen;
  FStimulus.OnEndSerialTimer:=@EnableResponses;

  FExpectedResponse.LoadFromParameters(LParameters);
  //WriteLn(FExpectedResponse.FrameToStr);
  FResponse := FExpectedResponse.KeySequenceFromFrame(Self);
  //ShowMessage(FResponse.SeqToString);
  FResponse.OnEndCapturing:=@TrialEndKeyCapturing;

  if Self.ClassType = TFrame then Config(Self);
end;

procedure TFrame.TrialResult(Sender: TObject);
begin
  Result := T_NONE;

  if FResponse.ContainsRecordedSequence then
    Result := T_HIT
  else
    Result:=T_MISS;

  //if HasConsequence then
  //  Consequences.Play(NextConsequence(Result = T_HIT));
end;

procedure TFrame.EnableResponses(Sender: TObject);
begin
  FResponse.StartNewRecording;
  FResponseEnabled:=True;
end;

procedure TFrame.TrialStart(Sender: TObject);
begin
  FResponseEnabled:=False;
  FStimulus.Start;
  //ShowMessage(FResponse.SeqToString);
end;

procedure TFrame.WriteData(Sender: TObject);
begin
  inherited WriteData(Sender);
  Data := Data + FLatency;
end;

procedure TFrame.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  procedure RecordKey;
  begin
    FLatency := TimestampToStr(LogEvent('KeyUp '+Char(Key)));
    FResponse.RecordKey(Key);
  end;
begin
  if FResponseEnabled then
    if FKeyCount < 4 then
      begin
        case Key of
          90, 122, { z }
          67, 99,  { c }
          66, 98,  { b }
          77, 109: { m }
            begin
              RecordKey;
              Inc(FKeyCount);
            end;
        end;
      end else begin
        case Key of
           13 : { enter }
             begin
               RecordKey;
               FKeyCount := 0;
             end;
        end;
      end;
end;

function TFrame.GetHeader: string;
begin
  Result :=  rsReportRspCmpLat;
end;

procedure TFrame.TrialBeforeEnd(Sender: TObject);
begin
  TrialResult(Sender);
  WriteData(Self);
end;

procedure TFrame.TrialEndKeyCapturing(Sender: TObject);
begin
  FResponseEnabled := False;
  FStimulus.Stop;
  EndTrial(Sender);
end;


end.

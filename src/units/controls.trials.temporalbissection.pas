{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.TemporalBissection;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils, ExtCtrls

  , Controls.Trials.Abstract
  , Stimuli.Sequence.TemporalBissection
  , Consequences
  ;

type

  TReportData = record
    SampleBegin   : string;
    ComparisonBegin   : string;
    ComparisonLatency : string;
    ComparisonChosen  : string;
  end;

  { TTBMTS }

  {
    Implements MTS for temporal bissection
    sample at the center
    comparisons at left right centers
  }
  TTBMTS = class(TTrial)
  private
    FHasConsequenceInterval : Boolean;
    FTimer : TTimer;
    FReportData : TReportData;
    FStimulus : TTBSequence;
    FParticipantResponse : TTBExpectedResponse;
    procedure Consequence(Sender: TObject);
    procedure TrialBeforeEnd(Sender: TObject);
    function GetHeader: string;
    procedure WhiteScreen(Sender: TObject);
  protected
    procedure Paint; override;
    procedure TrialStart(Sender: TObject); virtual;
    procedure WriteData(Sender: TObject); override;
    procedure TrialResult(Sender: TObject);
    procedure EnableResponses(Sender : TObject);
  public
    constructor Create(AOwner: TCustomControl); override;
    function AsString : string; override;
    function HasVisualConsequence: Boolean; override;
    function ConsequenceInterval: integer; override;
    procedure Play(ACorrection : Boolean); override;
  end;

implementation

uses Forms, Graphics, StrUtils, Constants, Timestamps, Cheats;

constructor TTBMTS.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialStart := @TrialStart;

  if Self.ClassType = TTBMTS then
    Header := Header + #9 + GetHeader;

  FStimulus := TTBSequence.Create(Self);
  FStimulus.Parent := Self.Parent;
  FResponseEnabled := False;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval:= ConsequenceDuration;
  FTimer.OnTimer := @WhiteScreen;
end;

function TTBMTS.AsString: string;
var
  LTrial : TStringList;
begin
  LTrial := TStringList.Create;
  LTrial.BeginUpdate;
  { implement me }
  LTrial.EndUpdate;
  Result := LTrial.Text;
  LTrial.Free;
end;

function TTBMTS.HasVisualConsequence: Boolean;
begin
  Result := (Self.Result <> T_NONE);
end;

function TTBMTS.ConsequenceInterval: integer;
begin
  if FHasConsequenceInterval then
    Result := ConsequenceDuration
  else
    Result := 0;
end;

procedure TTBMTS.Play(ACorrection: Boolean);
var
  LParameters : TStringList;
begin
  inherited Play(ACorrection);
  FHasConsequenceInterval := True;
  FStimulus.Cursor := -1;
  FShowCounter := True;
  FCounterType := ctSessionPoints;
  LParameters := Configurations.Parameters;
  FStimulus.LoadFromParameters(LParameters);
  FStimulus.SetScheduleConsequence(@Consequence);
  FStimulus.FitScreen;
  FStimulus.OnEndSerialTimer:=@EnableResponses;

  if Self.ClassType = TTBMTS then Config(Self);
end;

procedure TTBMTS.TrialResult(Sender: TObject);
begin
  //if FStimulus.ExpectedResponse = tbNone then Exit;

end;

procedure TTBMTS.EnableResponses(Sender: TObject);
begin
  FResponseEnabled:=True;
  FStimulus.Cursor := 0;
  Cursor := 0;
  FReportData.ComparisonBegin := TimestampToStr(LogEvent(rsReportStmCmpBeg));
  Mouse.CursorPos := Point(Screen.Width div 2, Screen.Height div 2);
end;

procedure TTBMTS.TrialStart(Sender: TObject);
begin
  FResponseEnabled:=False;
  FStimulus.Start;
  FReportData.SampleBegin := TimestampToStr(LogEvent(rsReportStmModBeg));

  if CheatsModeOn then begin
    ParticipantBot.Start(FStimulus.AsInterface);
  end;
end;

procedure TTBMTS.WriteData(Sender: TObject);
begin
  inherited WriteData(Sender);
  Data := Data +
    FStimulus.SampleDuration.ToString + HeaderTabs +
    FReportData.SampleBegin + HeaderTabs +
    FReportData.ComparisonBegin + HeaderTabs +
    FReportData.ComparisonLatency + HeaderTabs +
    FReportData.ComparisonChosen
    ;
end;

function TTBMTS.GetHeader: string;
begin
  Result :=
    rsReportStmMod + HeaderTabs +
    rsReportStmModBeg + HeaderTabs +
    rsReportStmCmpBeg + HeaderTabs +
    rsReportRspCmpLat + HeaderTabs +
    rsReportRspCmp
    ;
end;

procedure TTBMTS.WhiteScreen(Sender: TObject);
begin
  FTimer.Enabled := False;
  Parent.Color := clWhite;
  Parent.Invalidate;
  EndTrial(Self);
end;

procedure TTBMTS.Paint;
begin
  inherited Paint;
  if CheatsModeOn then
  begin
    case FStimulus.ExpectedResponse of
      tbLeft :
        Canvas.Rectangle(FStimulus.ComparisonLeft.BoundsRect);

      tbRight:
        Canvas.Rectangle(FStimulus.ComparisonRight.BoundsRect);

      tbNone:
        Canvas.TextOut(0, 0, 'Generalization');
    end;

    Canvas.TextRect(FStimulus.Sample.BoundsRect, 0,0, IETConsequence);
  end;
end;

procedure TTBMTS.Consequence(Sender: TObject);
var
  LName  : string;
begin
  FResponseEnabled := False;
  FShowCounter:=False;
  FReportData.ComparisonChosen := TComponent(Sender).Name;
  FReportData.ComparisonLatency:=
    TimestampToStr(LogEvent(FReportData.ComparisonChosen+'.Latency'));
  FStimulus.Stop;
  case FReportData.ComparisonChosen of
    'Left' :  FParticipantResponse:=tbLeft;
    'Right':  FParticipantResponse:=tbRight;
  end;

  if FStimulus.ExpectedResponse = FParticipantResponse then
    begin
      Result := T_HIT;
      // Read Consequence Probability;
      LName := IETConsequence + #32;

      case FParticipantResponse of
        tbLeft :
          FHasConsequenceInterval := StrToBool(ExtractDelimited(1,LName,[#32]));

        tbRight:
          FHasConsequenceInterval := StrToBool(ExtractDelimited(2,LName,[#32]));

        tbNone:
          { do nothing };
      end;

      if FHasConsequenceInterval then begin
        Parent.Cursor := -1;
        Cursor := -1;
        Parent.Invalidate;
        CounterManager.SessionPointsTopRight :=
          CounterManager.SessionPointsTopRight + 1;
        EndTrial(Sender);
      end else begin
        Parent.Cursor := -1;
        Cursor := -1;
        Parent.Color := clBlack;
        Parent.Invalidate;
        FTimer.Enabled := True;
      end;
    end
  else
    begin
      Parent.Cursor := -1;
      Cursor := -1;
      Parent.Invalidate;
      Result := T_MISS;
      EndTrial(Sender);
    end;
end;

procedure TTBMTS.TrialBeforeEnd(Sender: TObject);
begin
  WriteData(Self);
end;


end.

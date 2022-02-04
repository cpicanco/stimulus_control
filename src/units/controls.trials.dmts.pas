{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.DMTS;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils, ExtCtrls

  , Controls.Trials.Abstract
  , Stimuli.Sequence.DMTS
  , Consequences
  , Timestamps
  ;

type

  TReportData = record
    SampleBegin   : string;
    DelayBegin    : string;
    ComparisonBegin   : string;
    ComparisonLatency : string;
    ComparisonChosen  : string;
  end;

  { TDMTS }

  {
    Implements MTS for temporal bissection
    sample at the center
    comparisons at left right centers
  }
  TDMTS = class(TTrial)
  private
    FHasConsequenceInterval : Boolean;
    //FTimer : TTimer;
    FReportData : TReportData;
    FStimulus : TDMTSSequence;
    FParticipantResponse : TTBExpectedResponse;
    procedure Consequence(Sender: TObject);
    procedure TrialBeforeEnd(Sender: TObject);
    function GetHeader: string;
    //procedure WhiteScreen(Sender: TObject);
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
    function IsTestTrial : Boolean;
    function ConsequenceInterval: integer; override;
    procedure Play(ACorrection : Boolean); override;
  end;

implementation

uses Forms, Graphics
  , Constants, Cheats, Experiments.Grids
  , Session.Configuration.GlobalContainer;

constructor TDMTS.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialStart := @TrialStart;

  if Self.ClassType = TDMTS then
    Header := Header + #9 + GetHeader;

  FStimulus := TDMTSSequence.Create(Self);
  FStimulus.Parent := Self.Parent;
  FStimulus.LogEvent := @LogEvent;
  FResponseEnabled := False;
  //FTimer := TTimer.Create(Self);
  //FTimer.Enabled := False;
  //FTimer.Interval:= ConsequenceDuration;
  //FTimer.OnTimer := @WhiteScreen;
end;

function TDMTS.AsString: string;
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

function TDMTS.HasVisualConsequence: Boolean;
begin
  Result := (Self.Result <> T_NONE);
end;

function TDMTS.IsTestTrial: Boolean;
begin
  Result := Configurations.Parameters.Values[_Consequence].ToBoolean;
end;

function TDMTS.ConsequenceInterval: integer;
begin
  if FHasConsequenceInterval then
    Result := ConsequenceDuration
  else
    Result := 0;
end;

procedure TDMTS.Play(ACorrection: Boolean);
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

  if Self.ClassType = TDMTS then Config(Self);
end;

procedure TDMTS.TrialResult(Sender: TObject);
begin
  //if FStimulus.ExpectedResponse = tbNone then Exit;

end;

procedure TDMTS.EnableResponses(Sender: TObject);
begin
  FResponseEnabled:=True;
  FStimulus.Cursor := 0;
  Cursor := 0;
  FReportData.ComparisonBegin := TimestampToStr(LogEvent(rsReportStmCmpBeg));
  Mouse.CursorPos := Point(Screen.Width div 2, Screen.Height div 2);
end;

procedure TDMTS.TrialStart(Sender: TObject);
begin
  FResponseEnabled:=False;
  FStimulus.Start;
  FReportData.SampleBegin := TimestampToStr(LogEvent(rsReportStmModBeg));

  if CheatsModeOn then begin
    ParticipantBot.Start(FStimulus.AsInterface);
  end;
end;

procedure TDMTS.WriteData(Sender: TObject);
  function CustomResult : integer;
  begin
    case FReportData.ComparisonChosen of
      'Left' : CustomResult := 0;
      'Right': CustomResult := 1;
    end;
  end;

  function ShortCompName(N : integer) : string;
  begin
    case N of
      1 : Result :=
        FStimulus.Comparison1.ShortName + HeaderTabs +
        (FStimulus.Comparison1.Position+1).ToString;
      2 : Result :=
        FStimulus.Comparison2.ShortName + HeaderTabs +
        (FStimulus.Comparison2.Position+1).ToString;
      3 : Result :=
        FStimulus.Comparison3.ShortName + HeaderTabs +
        (FStimulus.Comparison3.Position+1).ToString;
    end;
  end;

var
  LSampleDuration: String;
begin
  inherited WriteData(Sender);
  LSampleDuration := FStimulus.SampleDuration.ToString;
  Data := Data +
    LSampleDuration + HeaderTabs +
    FReportData.SampleBegin + HeaderTabs +
    FReportData.DelayBegin + HeaderTabs +
    FReportData.ComparisonBegin + HeaderTabs +
    FReportData.ComparisonLatency + HeaderTabs +
    FReportData.ComparisonChosen  + HeaderTabs +
    ShortCompName(1) + HeaderTabs +
    ShortCompName(2) + HeaderTabs +
    ShortCompName(3);
end;

function TDMTS.GetHeader: string;
begin
  Result :=
    'S.Modelo.DuracaoProgramada' + HeaderTabs +
    rsReportStmModBeg + HeaderTabs +
    'S.Atraso.Inicio' + HeaderTabs +
    rsReportStmCmpBeg + HeaderTabs +
    rsReportRspCmpLat + HeaderTabs +
    rsReportRspCmp    + HeaderTabs +
    'S.C1.Media' + HeaderTabs + 'S.C1.Posicao'    + HeaderTabs +
    'S.C2.Media' + HeaderTabs + 'S.C2.Posicao'    + HeaderTabs +
    'S.C3.Media' + HeaderTabs + 'S.C3.Posicao';
end;

procedure TDMTS.Paint;
begin
  inherited Paint;
  if CheatsModeOn then begin
    case FStimulus.ExpectedResponse of
      tbNone :
        Canvas.TextOut(0, 0, 'BaseLine');

      tbC1 :
        Canvas.Rectangle(RectFromPosition(FStimulus.Comparison1.Position));

      tbC2 :
        Canvas.Rectangle(RectFromPosition(FStimulus.Comparison2.Position));

      tbC3 :
        Canvas.Rectangle(RectFromPosition(FStimulus.Comparison3.Position));
    end;
  end;
end;

procedure TDMTS.Consequence(Sender: TObject);
begin
  FResponseEnabled := False;
  FShowCounter:=False;
  FReportData.DelayBegin := TimestampToStr(FStimulus.DelayBegin);
  FReportData.ComparisonChosen := FStimulus.Comparison(Sender).ShortName;
  FReportData.ComparisonLatency:=
    TimestampToStr(LogEvent(FReportData.ComparisonChosen+'.Latency'));
  FStimulus.Stop;
  case FStimulus.Comparison(Sender).Key of
    'C1' :  FParticipantResponse:=tbC1;
    'C2' :  FParticipantResponse:=tbC2;
    'C3' :  FParticipantResponse:=tbC3;
  end;

  Parent.Cursor := -1;
  Cursor := -1;

  if (FStimulus.ExpectedResponse = FParticipantResponse) then begin
    if IsTestTrial then begin
      Result := T_HIT+'+BLACKOUT';
    end else begin
      Result := T_HIT;

      Counters.SessionPointsTopRight :=
        Counters.SessionPointsTopRight + 1;
    end;
  end else begin
    Result := T_MISS;
  end;
  Parent.Invalidate;
  EndTrial(Sender);
end;

procedure TDMTS.TrialBeforeEnd(Sender: TObject);
begin
  WriteData(Self);
end;


end.


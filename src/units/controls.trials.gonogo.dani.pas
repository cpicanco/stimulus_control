{
  Stimulus Control
  Copyright (C) 2014-2020 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.GoNoGo.Dani;

{$mode objfpc}{$H+}

interface

uses LCLIntf, Controls, Classes, SysUtils, LazFileUtils

  , Controls.Trials.Abstract
  , Controls.Trials.Helpers
  , Controls.Stimuli.Text
  , Controls.GoLeftGoRight
  , Stimuli.Image
  , Schedules
  , Consequences
  ;

type

  TButtonSide = (ssNone, ssLeft, ssRight);

  { TGNG }

  TGNG = Class(TTrial)
  private
    FButtonResponse : TButtonSide;
    FDataSupport : TDataSupport;
    FSample : TLabelStimulus;
    FComparison : TLabelStimulus;
    FOperandum : TGoLeftGoRight;
    FSchedule : TSchedule;
    FButtonSide : TButtonSide;
    FConsequence : TConsequence;
    procedure ButtonLeftClick(Sender: TObject);
    procedure ButtonRightClick(Sender: TObject);
    procedure TrialPaint;
    procedure Consequence(Sender: TObject);
    procedure Response(Sender: TObject);
    procedure TrialBeforeEnd(Sender: TObject);
    procedure TrialStart(Sender: TObject);
    procedure TrialResult(Sender: TObject);
    procedure TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    { TTrial }
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TCustomControl); override;
    destructor Destroy; override;
    procedure Play(ACorrection : Boolean); override;
    procedure StopConsequence; override;
    function StartConsequence : integer;
    function AsString : string; override;
    function HasConsequence: Boolean;
  end;

implementation

uses StdCtrls, strutils, constants, Timestamps
  , Session.Configuration.GlobalContainer;

{ TGNG }

constructor TGNG.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialKeyUp := @TrialKeyUp;
  OnTrialStart := @TrialStart;
  OnTrialPaint := @TrialPaint;
  FButtonResponse := ssNone;
  Header :=  Header + #9 +
             rsReportStmBeg + #9 +
             rsReportRspLat + #9 +
             rsReportStmEnd + #9 +
             rsReportRspStl;

  FDataSupport.Responses:= 0;
end;

destructor TGNG.Destroy;
begin
  {$IFDEF AUDIO} if Assigned(FSound) then FSound.Free; {$ENDIF}
  inherited Destroy;
end;

procedure TGNG.TrialResult(Sender: TObject);
begin
  if Result = T_NONE then
  begin
    case FButtonResponse of
      ssNone: Result := T_NONE;
      ssLeft, ssRight:
        if FButtonResponse = FButtonSide then
          Result := T_HIT
        else
          Result := T_MISS;
    end;
    FConsequence := NextConsequence(Result = T_HIT);
  end;
end;

procedure TGNG.TrialBeforeEnd(Sender: TObject);
begin
  FDataSupport.StmEnd := TickCount;
  TrialResult(Sender);
  LogEvent(Result);
  WriteData(Sender);
end;

procedure TGNG.Consequence(Sender: TObject);
begin
  if Assigned(Counters.OnConsequence) then Counters.OnConsequence(Self);
  EndTrial(Sender);
end;

procedure TGNG.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  //if FButtonResponse = ssNone then
  //begin
  //  case Key of
  //    67,  99 :
  //      begin
  //
  //      end;
  //
  //    77, 109 :
  //      begin
  //
  //      end;
  //  end;
  //end;
end;

procedure TGNG.TrialPaint;
var
  R : TRect;
begin
  if FSample.Visible then
    begin
      R := FSample.BoundsRect;
      if InflateRect(R,50,50) then
      begin
        Canvas.Pen.Color := $2e7aff;
        Canvas.Pen.Width := 15;
      end;

      Canvas.Rectangle(R);
    end;
end;

procedure TGNG.ButtonLeftClick(Sender: TObject);
begin
  FButtonResponse := ssLeft;
  LogEvent('Verdadeiro');
  //if FButtonSide = ssLeft then //force right response
  FSchedule.DoResponse;
end;

procedure TGNG.ButtonRightClick(Sender: TObject);
begin
  FButtonResponse := ssRight;
  LogEvent('Falso');
  //if FButtonSide = ssRight then //force right response
  FSchedule.DoResponse;
end;

procedure TGNG.Play(ACorrection: Boolean);
var
  s1, LName : string;
  Parameters : TStringList;
begin
  inherited Play(ACorrection);
  Parameters := Configurations.Parameters;

  s1:= Parameters.Values[_Samp +_cStm] + #32;
  LName := RootMedia + ExtractDelimited(1,s1,[#32]);
  FSample := TLabelStimulus.Create(Self, Self.Parent);
  with FSample do
    begin
      LoadFromFile(LName);
      CentralizeTopMiddle;
    end;

  s1:= Parameters.Values[_Comp + IntToStr(1) +_cStm] + #32;
  LName := RootMedia + ExtractDelimited(1,s1,[#32]);

  FComparison := TLabelStimulus.Create(Self, Self.Parent);
  with FComparison do
    begin
      LoadFromFile(LName);
      CentralizeBottom;
    end;

  with TLabelStimulus.Create(Self, Self.Parent) do
  begin
    Caption := 'O computador disse:';
    Font.Bold := True;
    CentralizeOnTopOfControl(FComparison);
  end;

  FOperandum := TGoLeftGoRight.Create(Self, FComparison);
  FOperandum.OnButtonRightClick:=@ButtonRightClick;
  FOperandum.OnButtonLeftClick:=@ButtonLeftClick;
  FOperandum.Parent := Self.Parent;
  FSchedule := TSchedule.Create(Self);
  with FSchedule do
    begin
      OnConsequence := @Consequence;
      OnResponse:= @Response;
      Load(CRF);
    end;

  case UpperCase(Parameters.Values[_ResponseStyle]) of
    'GO'   : FButtonSide := ssLeft;
    'NOGO' : FButtonSide := ssRight;
  end;

  if Self.ClassType = TGNG then Config(Self);
end;

procedure TGNG.StopConsequence;
begin
  FConsequence.Visual.Stop;
end;

function TGNG.StartConsequence: integer;
begin
  FConsequence.Visual.Parent := Self.Parent;
  Consequences.Play(FConsequence);
  Result := 1750 + Random(501);
end;

function TGNG.AsString: string;
begin
  Result := '';
end;

function TGNG.HasConsequence: Boolean;
begin
  Result := Self.Result <> T_NONE;
end;

procedure TGNG.TrialStart(Sender: TObject);
begin
  FSample.Show;
  FComparison.Show;
  FDataSupport.Latency := TimeStart;
  FDataSupport.StmBegin := TickCount;
  FSchedule.Start;
end;

procedure TGNG.WriteData(Sender: TObject);
var
  LLatency : string;
  LButtonSide : string;
begin
  inherited WriteData(Sender);

  if FDataSupport.Latency = TimeStart then
    LLatency := 'NA'
  else LLatency := TimestampToStr(FDataSupport.Latency - TimeStart);

  WriteStr(LButtonSide, FButtonSide);

  Data :=  Data +
           TimestampToStr(FDataSupport.StmBegin - TimeStart) + #9 +
           LLatency + #9 +
           TimestampToStr(FDataSupport.StmEnd - TimeStart) + #9 +
           LButtonSide;
end;

procedure TGNG.Response(Sender: TObject);
begin
  Inc(FDataSupport.Responses);
  if FDataSupport.Latency = TimeStart then
    FDataSupport.Latency := TickCount;

  if Assigned(Counters.OnStmResponse) then Counters.OnStmResponse(Sender);
  if Assigned(OnStmResponse) then OnStmResponse (Self);
end;

end.

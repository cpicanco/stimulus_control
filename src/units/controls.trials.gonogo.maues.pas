{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.GoNoGo.Maues;

{$mode objfpc}{$H+}

interface

uses LCLIntf, Controls, Classes, SysUtils, LazFileUtils

  , Controls.Trials.Abstract
  , Controls.Trials.Helpers
  , Schedules
  , Controls.Stimuli.Key
  {$IFDEF AUDIO}, Audio.Bass_nonfree {$ENDIF}
  ;

type

  TResponseStyle = (Go, NoGo);
  TScreenSide = (ssNone, ssLeft, ssRight);

  { TGNG }

  TGNG = Class(TTrial)
  private
    FGoResponseFired : TScreenSide;
    FDataSupport : TDataSupport;
    FStimulus : TKey;
    {$IFDEF AUDIO}FSound : TBassStream;{$ENDIF}
    FSchedule : TSchedule;
    FResponseStyle : TResponseStyle;
    FScreenSide : TScreenSide;
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
    function AsString : string; override;
    //procedure DispenserPlusCall; override;

  end;

implementation

uses strutils, constants, Timestamps;

{ TGNG }

constructor TGNG.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialKeyUp := @TrialKeyUp;
  OnTrialStart := @TrialStart;
  OnTrialPaint := @TrialPaint;
  FGoResponseFired := ssNone;
  Header :=  Header + #9 +
             rsReportStmBeg + #9 +
             rsReportRspLat + #9 +
             rsReportStmEnd + #9 +
             rsReportScrSid + #9 +
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
    case FResponseStyle of
      Go:
        if (FGoResponseFired <> ssNone) and
           (FGoResponseFired = FScreenSide) then
          Result := T_HIT
        else
          Result := T_MISS;

      NoGo:
        if (FGoResponseFired = ssNone) then
          Result := T_HIT
        else
          Result := T_MISS;
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
  TrialResult(Sender);
  if Assigned(CounterManager.OnConsequence) then CounterManager.OnConsequence(Self);
end;

procedure TGNG.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FGoResponseFired = ssNone then
  begin
    case Key of
      67,  99 :
        begin
          FGoResponseFired := ssLeft;
          LogEvent('c');
          if FScreenSide = ssLeft then
            FSchedule.DoResponse;
        end;

      77, 109 :
        begin
          FGoResponseFired := ssRight;
          LogEvent('m');
          if FScreenSide = ssRight then
            FSchedule.DoResponse;
        end;
    end;
  end;
end;

procedure TGNG.TrialPaint;
var
  R : TRect;
begin
  if FStimulus.Visible then
    begin
      R := FStimulus.BoundsRect;
      if InflateRect(R,100,100) then
      case FResponseStyle of
        Go : Canvas.Pen.Width := 5;
        NoGo : Canvas.Pen.Width := 15;
      end;
      Canvas.Rectangle(R);
    end;
end;
procedure TGNG.Play(ACorrection: Boolean);
var
  s1, LName, LWidth, LHeight : string;
  LConfiguration : TStringList;
begin
  inherited Play(ACorrection);
  LConfiguration := Configurations.SList;

  s1:= LConfiguration.Values[_Comp + IntToStr(1) +_cStm] + #32;
  LName := RootMedia + ExtractDelimited(1,s1,[#32]);

  s1:= LConfiguration.Values[_Comp + IntToStr(1) +_cBnd] + #32;
  LWidth := ExtractDelimited(1,s1,[#32]);
  LHeight := ExtractDelimited(2,s1,[#32]);

  FStimulus := TKey.Create(Self);
  with FStimulus do
    begin
      Filename:= LName;
      Width := StrToIntDef(LWidth,300);
      Height:= StrToIntDef(LHeight,Width);
      Parent := TCustomControl(Self.Parent);
    end;

  FSchedule := TSchedule.Create(Self);
  with FSchedule do
    begin
      OnConsequence := @Consequence;
      OnResponse:= @Response;
      Load(CRF);
    end;

  case UpperCase(LConfiguration.Values[_ResponseStyle]) of
    'GO'   : FResponseStyle := Go;
    'NOGO' : FResponseStyle := NoGo;
  end;

  case UpperCase(Configurations.SList.Values[_ScreenSide]) of
    'SSLEFT'  : FScreenSide := ssLeft;
    'SSRIGHT' : FScreenSide := ssRight;
  end;
  if Self.ClassType = TGNG then Config(Self);
end;

function TGNG.AsString: string;
begin
  Result := '';
end;

procedure TGNG.TrialStart(Sender: TObject);
begin
  case FScreenSide of
    ssLeft  : FStimulus.CentralizeLeft;
    ssRight : FStimulus.CentralizeRight;
  end;

  FStimulus.Show;
  FDataSupport.Latency := TimeStart;
  FDataSupport.StmBegin := TickCount;
  FSchedule.Start;
end;

procedure TGNG.WriteData(Sender: TObject);
var
  LLatency : string;
  LScreenSide : string;
  LResponseStyle : string;
begin
  inherited WriteData(Sender);

  if FDataSupport.Latency = TimeStart then
    LLatency := 'NA'
  else LLatency := TimestampToStr(FDataSupport.Latency - TimeStart);

  WriteStr(LScreenSide, FScreenSide);
  WriteStr(LResponseStyle, FResponseStyle);

  Data :=  Data +
           TimestampToStr(FDataSupport.StmBegin - TimeStart) + #9 +
           LLatency + #9 +
           TimestampToStr(FDataSupport.StmEnd - TimeStart) + #9 +
           LScreenSide + #9 +
           LResponseStyle ;
end;

procedure TGNG.Response(Sender: TObject);
begin
  Inc(FDataSupport.Responses);
  if FDataSupport.Latency = TimeStart then
    FDataSupport.Latency := TickCount;

  if Assigned(CounterManager.OnStmResponse) then CounterManager.OnStmResponse(Sender);
  if Assigned(OnStmResponse) then OnStmResponse (Self);
end;

end.

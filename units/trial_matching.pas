{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit trial_matching;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils

    , response_key
    , custom_timer
    , trial_simple
    //, interface_rs232
    //, interface_plp
    ;

type

  { TSampleDataSupport }

  TSampleDataSupport = record
    SampBkndRespCount,
    DelaBkndRespCount : integer;
    SampLatency,
    SampleBegin,
    Delay_Begin : Extended;
    //SampleMsg : string
    //Rs232Code : string;
    //PLPCode : BYTE;
  end;

  { TMTS }

  TMTS = Class(TSimpl)
  private
    FDelayed : Boolean;
    FDelay : TClockThread;
    FSample : TSupportKey;
    FSDataSupport : TSampleDataSupport;
    procedure SampleMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
    procedure DelayEnd(Sender: TObject);
    procedure SampleConsequence(Sender: TObject);
    procedure SampleResponse(Sender: TObject);
    procedure TrialBeforeEnd(Sender: TObject);
    procedure VisibleSample(AValue: Boolean);
  protected
    // TTrial
    procedure TrialStart(Sender: TObject); override;
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Play(ACorrection : Boolean); override;
  end;

implementation

uses constants, timestamps;

constructor TMTS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialStart := @TrialStart;

  Header := 'Pos.Mod.' + #9 +
            'Lat.Mod.' + #9 +
            'Dur.Mod.' + #9 +
            'Atr.Mod.' + #9 +
            'Frq.Mod.' + #9 + #9 + #9 +
            Header;
end;


procedure TMTS.Play(ACorrection : Boolean);
var
  s1: string;
  R: TRect;
begin
  inherited Play(ACorrection);
  with FSample do begin
    Key:= TKey.Create(Parent);
    Key.Cursor:= Self.Cursor;
    Key.Parent:= TCustomControl(Parent);
    Key.OnConsequence:= @SampleConsequence;
    Key.OnResponse:= @SampleResponse;

    // BND
    s1:= CfgTrial.SList.Values[_Samp + _cBnd];
    R.Top:= StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
    NextSpaceDelimitedParameter(s1);
    R.Left:= StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
    NextSpaceDelimitedParameter(s1);
    R.Right:= StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
    NextSpaceDelimitedParameter(s1);
    R.Bottom:= StrToIntDef(s1, 0);
    Key.SetBounds(R.Left, R.Top, R.Right, R.Bottom);

    // STM
    s1:= CfgTrial.SList.Values[_Samp + _cStm] + #32;
    Key.FullPath:= RootMedia + Copy(s1, 0, pos(#32, s1)-1);
    NextSpaceDelimitedParameter(s1);
    Key.Loops:= StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
    NextSpaceDelimitedParameter(s1);
    Key.Color := StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), $0000FF);

    // SCH
    Key.Schedule.Kind:= CfgTrial.SList.Values[_Samp + _cSch];
    AddToClockList(Key.Schedule);

    // MSG
    Msg:= CfgTrial.SList.Values[_Samp + _cMsg];
  end;

  // DELAY
  FDelayed:= StrToBoolDef(CfgTrial.SList.Values[_Delayed], False);
  if FDelayed then
    begin
      FDelay := TClockThread.Create(True);
      FDelay.OnTimer:= @DelayEnd;
      FDelay.Interval:= StrToIntDef(CfgTrial.SList.Values[_Delay], 0);
      FDelay.Enabled := False;
      AddToClockList(@FDelay.Start);
    end;

  if Self.ClassType = TMTS then Config(Self);
end;

procedure TMTS.DelayEnd(Sender: TObject);
begin
  FDelay.Enabled:= False;
  FDelay.Terminate;
  inherited TrialStart(Sender);
end;

procedure TMTS.SampleConsequence(Sender: TObject);
begin
  if FDelayed then
    begin
      VisibleSample(False);
      FDelay.Enabled := True;
      FSDataSupport.Delay_Begin := TickCount;
    end
  else
    inherited TrialStart(Sender);
end;

procedure TMTS.SampleResponse(Sender: TObject);
var
  LTickCount : Extended;
begin
  if Sender is TKey then
    if TKey(Sender).Visible then
      begin
        LTickCount := TickCount;
        LogEvent(FSample.Msg + #9 + TKey(Sender).LastResponseLog);

        if FSDataSupport.SampLatency = TimeStart then
            FSDataSupport.SampLatency := LTickCount;

        if Assigned(CounterManager.OnStmResponse) then CounterManager.OnStmResponse(Sender);
        if Assigned(OnStmResponse) then OnStmResponse (Self);
      end;
end;

procedure TMTS.VisibleSample(AValue: Boolean);
begin
  FSample.Key.Visible := AValue;
end;

procedure TMTS.SampleMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

var
  aStimulus : string;
begin
  if FSample.Key.Visible then
    begin
      aStimulus := 'BK.S';
      Inc(FSDataSupport.SampBkndRespCount);
    end
  else
    begin
      aStimulus := 'BK.D';
      Inc(FSDataSupport.DelaBkndRespCount);
    end;
  LogEvent(aStimulus + #9 + IntToStr(X) + #9 + IntToStr(Y));

  CounterManager.OnBkgndResponse(Self);
  if Assigned(OnBkGndResponse) then OnBkGndResponse(Self);
end;

procedure TMTS.TrialStart(Sender: TObject);
begin
  FSDataSupport.SampLatency := TimeStart;
  FSDataSupport.SampleBegin := TickCount;
  VisibleSample(True);
  OnMouseDown := @SampleMouseDown;
end;


procedure TMTS.WriteData(Sender: TObject);
var
  Pos_Mod,
  Lat_Mod,
  Dur_Mod,
  Atr_Mod,
  Frq_Mod: String;
begin
  Pos_Mod:= FSample.Msg;

  if FSDataSupport.SampLatency = TimeStart then
    Lat_Mod := #32#32#32#32#32#32 + 'NA'
  else Lat_Mod := TimestampToStr(FSDataSupport.SampLatency - TimeStart);

  if FDelayed then
    begin
      Dur_Mod := TimestampToStr(FSDataSupport.Delay_Begin - FSDataSupport.SampleBegin);
      Atr_Mod := TimestampToStr(FSDataSupport.Delay_Begin - TimeStart);
    end
  else
    begin
      Dur_Mod := TimestampToStr(TickCount - FSDataSupport.SampleBegin);
      Atr_Mod := #32#32#32#32#32#32 + 'NA';
    end;

  Frq_Mod := _Samp + '=' +
             IntToStr(FSample.Key.ResponseCount) + #9 +
             'BK.S='+ IntToStr(FSDataSupport.SampBkndRespCount) + #9 +
             'BK.D='+ IntToStr(FSDataSupport.DelaBkndRespCount);

  Data := Pos_Mod + #9 +
          Lat_Mod + #9 +
          Dur_Mod + #9 +
          Atr_Mod + #9 +
          Frq_Mod + #9;

  inherited WriteData(Sender);
end;

procedure TMTS.TrialBeforeEnd(Sender: TObject);
begin
  TrialResult(Sender);
  WriteData(Sender);
end;

end.

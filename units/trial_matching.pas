{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

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
    SampleMsg : string
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
    procedure BackgroundMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
    procedure DelayEnd(Sender: TObject);
    procedure SampleConsequence(Sender: TObject);
    procedure SampleResponse(Sender: TObject);
    procedure TrialStart(Sender: TObject);
    procedure VisibleSample(AValue: Boolean);
  protected
    // TTrial
    procedure WriteData(Sender: TObject); override;
    procedure BeforeEndTrial(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Play(ACorrection : Boolean); override;
  end;

implementation

uses constants, timestamps;

constructor TMTS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnBeforeEndTrial := @BeforeEndTrial;

  Header := 'Pos.Mod.' + #9 +
            'Res.Mod.' + #9 +
            'Lat.Mod.' + #9 +
            'Dur.Mod.' + #9 +
            'Tmp.Mod.' + #9 +
            'Atr.Mod.' + #9 +
            'Frq.Mod.' + #9 +
            Header;
end;


procedure TMTS.Play(ACorrection : Boolean);
var
  s1, LName, LLoop, LColor: string;
  R: TRect;
begin
  inherited Play(ACorrection);
  with FSample do begin
    Key:= TKey.Create(Self);
    Key.Cursor:= Self.Cursor;
    Key.Parent:= Self;
    Key.OnConsequence:= @SampleConsequence;
    Key.OnResponse:= @SampleResponse;

    s1:= CfgTrial.SList.Values[_Samp + _cBnd];
    R.Top:= StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
    NextSpaceDelimitedParameter(s1);
    R.Left:= StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
    NextSpaceDelimitedParameter(s1);
    R.Right:= StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
    NextSpaceDelimitedParameter(s1);
    R.Bottom:= StrToIntDef(s1, 0);
    Key.SetBounds(R.Left, R.Top, R.Right, R.Bottom);

    s1:= CfgTrial.SList.Values[_Samp + _cStm] + #32;
    LName := RootMedia + Copy(s1, 0, pos(#32, s1)-1);
    NextSpaceDelimitedParameter(s1);
    LLoop := Copy(s1, 0, pos(#32, s1)-1);
    NextSpaceDelimitedParameter(s1);
    LColor := Copy(s1, 0, pos(#32, s1)-1);
    Key.Color := StrToIntDef(LColor, $0000FF);

    Key.Loops:= StrToIntDef(LLoop, 0);
    Key.FullPath:= LName;
    Key.Schedule.Kind:= CfgTrial.SList.Values[_Samp + _cSch];
    AddToClockList(Key.Schedule);
    Msg:= CfgTrial.SList.Values[_Samp + _cMsg];
  end;

  FDelayed:= StrToBoolDef(CfgTrial.SList.Values[_Delayed], False);
  if FDelayed then
    begin
      FDelay := TClockThread.Create(True);
      FDelay.OnTimer:= @DelayEnd;
      FDelay.Interval:= StrToIntDef(CfgTrial.SList.Values[_Delay], 0);
      FDelay.Enabled := False;
      AddToClockList(@FDelay.Start);
    end;
  Config(Self);
end;

procedure TMTS.DelayEnd(Sender: TObject);
begin
  FDelay.Enabled:= False;
  FDelay.Terminate;
  OnMouseDown := @TrialMouseDown;
  VisibleComparisons(True);
end;

procedure TMTS.SampleConsequence(Sender: TObject);
begin
  FSDataSupport.Delay_Begin := TickCount;
  VisibleSample(False);
  if FDelayed then
    FDelay.Enabled := True
  else
    VisibleComparisons(True);
end;

procedure TMTS.SampleResponse(Sender: TObject);
var
  LTickCount : Extended;
  LTime : string;
begin
  if Sender is TKey then
    if TKey(Sender).Visible then
      begin
        LTickCount := TickCount;
        LTime := TimestampToStr(LTickCount - TimeStart);

        LogEvent(TKey(Sender).LastResponseLog);

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

procedure TMTS.BackgroundMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

var
  aTime, aStimulus : string;
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
  if FIsCorrection then BeginCorrection(Self);
  FSDataSupport.SampLatency := TimeStart;
  FSDataSupport.SampleBegin := TickCount;
  VisibleSample(True);
  VisibleComparisons(False);
  OnMouseDown := @BackgroundMouseDown;
end;


procedure TMTS.WriteData(Sender: TObject);
var
  Pos_Mod,
  Res_Mod,
  Lat_Mod,
  Dur_Mod,
  Atr_Mod,
  Frq_Mod: String;
begin
  Pos_Mod:= FSample.Msg;

  Res_Mod := FSDataSupport.SampleMsg;

  if FSDataSupport.SampLatency = TimeStart then
    Lat_Mod := #32#32#32#32#32#32 + 'NA'
  else Lat_Mod := TimestampToStr(FSDataSupport.SampLatency - TimeStart);

  Dur_Mod := TimestampToStr(FSDataSupport.Delay_Begin - FSDataSupport.SampleBegin);

  if FDelayed then
    Atr_Mod := TimestampToStr(FSDataSupport.Delay_Begin - TimeStart)
  else Atr_Mod := #32#32#32#32#32#32 + 'NA';

  Frq_Mod := _Samp + '=' +
             IntToStr(FSample.Key.ResponseCount) + #9 +
             'BK.S='+ IntToStr(FSDataSupport.SampBkndRespCount) + #9 +
             'BK.D='+ IntToStr(FSDataSupport.DelaBkndRespCount);

  Data := Pos_Mod + #9 +
          Res_Mod + #9 +
          Lat_Mod + #9 +
          Dur_Mod + #9 +
          Atr_Mod + #9 +
          Frq_Mod + #9;

  inherited WriteData(Sender);
end;

procedure TMTS.BeforeEndTrial(Sender: TObject);
begin
  WriteData(Sender);
end;

end.

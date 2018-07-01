{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.MatchingToSample;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils, ExtCtrls

  , Controls.Trials.SimpleDiscrimination
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
  end;


  TSampleType = (sampSimultaneous, sampSuccessive);

  { TMTS }

  {
    Implements
    Conditional Discriminations
    Successive and Simultaneous
    With or Without delay
  }
  TMTS = Class(TSimpl)
  private
    FSampleType : TSampleType;
    FDelay : TTimer;
    FSample : TKeySupport;
    FSDataSupport : TSampleDataSupport;
    procedure BackgroundMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DelayEnd(Sender: TObject);
    procedure SampleConsequence(Sender: TObject);
    procedure SampleResponse(Sender: TObject);
    procedure TrialBeforeEnd(Sender: TObject);
    procedure VisibleSample(AValue: Boolean);
    function GetHeader : string;
  protected
    // TTrial
    procedure TrialStart(Sender: TObject); override;
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TCustomControl); override;
    function AsString : string; override;
    procedure Play(ACorrection : Boolean); override;
  end;

implementation

uses strutils, constants, timestamps, Controls.Stimuli.Key
  , Session.ConfigurationFile
  ;

constructor TMTS.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialStart := @TrialStart;

  FDelay := nil;
end;

procedure TMTS.Play(ACorrection : Boolean);
var
  s1: string;
  R: TRect;
  LDelayed : Boolean;
  LDelay : integer;
  LParameters : TStringList;
begin
  inherited Play(ACorrection);
  LParameters := Configurations.SList;
  with FSample do
    begin
      // Owner is TGraphicControl
      Key:= TKey.Create(Self);
      Key.Cursor:= Self.Cursor;
      Key.OnConsequence:= @SampleConsequence;
      Key.OnResponse:= @SampleResponse;

      // Parent is TCustomControl/TForm
      Key.Parent:= Self.Parent;

      // BND
      s1:= LParameters.Values[_Samp + _cBnd];
      R.Top:= StrToIntDef(ExtractDelimited(1,s1,[#32]),0);
      R.Left:= StrToIntDef(ExtractDelimited(2,s1,[#32]),0);
      R.Right:= StrToIntDef(ExtractDelimited(3,s1,[#32]),100);
      R.Bottom:= StrToIntDef(ExtractDelimited(4,s1,[#32]),100);
      Key.SetBounds(R.Left, R.Top, R.Right, R.Bottom);

      // STM
      s1:= LParameters.Values[_Samp + _cStm] + #32;
      Key.Loops:= StrToIntDef(ExtractDelimited(2,s1,[#32]), 0);  // must be set before fullpath
      Key.Color := StrToIntDef(ExtractDelimited(3,s1,[#32]),$0000FF);
      Key.Filename:= RootMedia + ExtractDelimited(1,s1,[#32]);

      // SCH
      Key.Schedule.Load(LParameters.Values[_Samp + _cSch]);
      AddToClockList(Key.Schedule);

      // MSG
      Msg:= LParameters.Values[_Samp + _cMsg];
    end;

  // DELAY
  LDelayed := StrToBoolDef(LParameters.Values[_SampleType], False);
  if LDelayed then
    FSampleType := sampSuccessive
  else
    FSampleType := sampSimultaneous;

  LDelay := StrToIntDef(LParameters.Values[_Delay], 0);
  if LDelay > 0 then
  begin
    FDelay := TTimer.Create(Self);
    FDelay.Enabled:=False;
    FDelay.OnTimer := @DelayEnd;
    FDelay.Interval:= LDelay;
  end;

  if Self.ClassType = TMTS then Config(Self);
end;

function TMTS.AsString: string;
var
  LTrial : TStringList;
begin
  LTrial := TStringList.Create;
  LTrial.BeginUpdate;
  if Self.ClassType = TMTS then
  begin
    LTrial.Append(TConfigurationFile.FullTrialSection(
      CounterManager.CurrentBlc, CounterManager.CurrentTrial));
    LTrial.Values[_Kind] := T_Simple;
    LTrial.Values[_Cursor] := IntToStr(Cursor);
    LTrial.Values[_LimitedHold] := IntToStr(LimitedHold);
  end;

  case FSampleType of
    sampSuccessive : LTrial.Values[_SampleType] := BoolToStr(True);
    sampSimultaneous : LTrial.Values[_SampleType] := BoolToStr(False);
  end;
  LTrial.Values[_Delay] := IntToStr(FDelay.Interval);

  with FSample do
  begin
    LTrial.Values[_Samp + _cStm] := ExtractFileName(Key.Filename);
    LTrial.Values[_Samp + _cBnd] := Key.BoundsAsString;
    LTrial.Values[_Samp + _cSch] := Key.Schedule.AsString;
    LTrial.Values[_Samp + _cMsg] := Msg;
  end;
  LTrial.Append(inherited AsString);
  LTrial.EndUpdate;
  Result := LTrial.Text;
  LTrial.Free;
end;

procedure TMTS.DelayEnd(Sender: TObject);
begin
  FDelay.Enabled:= False;
  inherited TrialStart(Sender);
end;

procedure TMTS.SampleConsequence(Sender: TObject);
begin
  case FSampleType of
    sampSimultaneous: { do nothing };
    sampSuccessive:
      begin
        if FSample.Key.Kind.stmAudio then
          FSample.Key.Stop;
        VisibleSample(False);
      end;
  end;

  if Assigned(FSample.Key.OnConsequence) then
  begin
    FSample.Key.OnConsequence := nil;
    if Assigned(FDelay) then
    begin
      FDelay.Enabled := True;
      FSDataSupport.Delay_Begin := TickCount;
    end else
      inherited TrialStart(Sender);
  end;
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
        if Assigned(OnStmResponse) then OnStmResponse(Self);
      end;
end;

procedure TMTS.VisibleSample(AValue: Boolean);
begin
  FSample.Key.Visible := AValue;
  if FSample.Key.Visible then
    LogEvent('Sample.Show,'+FSample.Msg)
  else
    LogEvent('Sample.Hide,'+FSample.Msg);
end;

function TMTS.GetHeader: string;
begin
  Result := rsReportStmMod + #9 +
            rsReportRspModLat + #9 +
            rsReportStmModBeg + #9 +
            rsReportStmModEnd + #9 +
            rsReportStmModDur + #9 +
            rsReportStmModDel + #9 +
            rsReportRspModFrq + #9 + #9 + #9;
end;

procedure TMTS.BackgroundMouseDown(Sender: TObject; Button: TMouseButton;
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
  VisibleComparisons(False);

  if FSample.Key.Kind.stmAudio then
    FSample.Key.Play;

  // todo: video autostart?
  // if FSample.Key.Kind.stmImage = stmVideo then
  //   FSample.Key.Play;

  OnMouseDown := @BackgroundMouseDown;
end;


procedure TMTS.WriteData(Sender: TObject);
var
  Pos_Mod : string = '';
  Lat_Mod : string = '';
  Dur_Mod : string = '';
  Atr_Mod : string = '';
  Frq_Mod : string = '';
  End_Mod : string = '';
  Beg_Mod : string = '';
begin
  inherited WriteData(Sender);
  Header := Header + #9 + GetHeader;
  case FSample.Msg of
    '','AUTO':Pos_Mod:= FSample.Key.ShortName+' - '+'('+IntToStr(FSample.Key.Top)+','+IntToStr(FSample.Key.Left)+')';
    else
      Pos_Mod:= FSample.Msg;
  end;

  if FSDataSupport.SampLatency = TimeStart then
    Lat_Mod := #32#32#32#32#32#32 + 'NA'
  else Lat_Mod := TimestampToStr(FSDataSupport.SampLatency - TimeStart);

  case FSampleType of
    sampSuccessive:
      Dur_Mod := TimestampToStr(FSDataSupport.Delay_Begin - FSDataSupport.SampleBegin);
    sampSimultaneous:
      Dur_Mod := TimestampToStr(TickCount - FSDataSupport.SampleBegin);
  end;

  if Assigned(FDelay) then
    Atr_Mod := TimestampToStr(FSDataSupport.Delay_Begin - TimeStart)
  else
    Atr_Mod := #32#32#32#32#32#32 + 'NA';

  Frq_Mod := _Samp + '=' +
             IntToStr(FSample.Key.ResponseCount) + #9 +
             'BK.S='+ IntToStr(FSDataSupport.SampBkndRespCount) + #9 +
             'BK.D='+ IntToStr(FSDataSupport.DelaBkndRespCount);

  Beg_Mod := TimestampToStr(FDataSupport.StmBegin - TimeStart);
  End_Mod := TimestampToStr(FDataSupport.StmEnd - TimeStart);

  Data := Data + #9 +
          Pos_Mod + #9 +
          Lat_Mod + #9 +
          Beg_Mod + #9 +
          End_Mod + #9 +
          Dur_Mod + #9 +
          Atr_Mod + #9 +
          Frq_Mod;

  if Assigned(OnTrialWriteData) then OnTrialWriteData(Self);
end;

procedure TMTS.TrialBeforeEnd(Sender: TObject);
begin
  TrialResult(Sender);
  WriteData(Sender);
end;

end.

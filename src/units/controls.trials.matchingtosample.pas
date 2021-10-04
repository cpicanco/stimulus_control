{
  Stimulus Control
  Copyright (C) 2014-2020 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.MatchingToSample;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils, ExtCtrls
  , Controls.Trials.Abstract
  , Controls.Trials.Helpers
  , Controls.Stimuli.Key
  //, interface_plp
  ;

type

  { TSampleDataSupport }

  TSampleDataSupport = record
    SampBkndRespCount,
    DelaBkndRespCount : integer;
    SampLatency,
    SampleShow,
    SampleHide : Extended;
    //SampleMsg : string
  end;

  TKeySupport = record
    Key : TKey; // response key
    Csq : BYTE; // PLP consequence
    Usb : string; // USB consequence
    Msg : string; // Report Message
    Nxt : string; // Next Trial
    Res : string; // Trial Result: MISS, NONE or HIT.
    IET : string; // Intertrial interval
    TO_ : Integer; // Timeout consequence
  end;

  TConsequenceSupport = record
    ReportMessage,
    Rs232Code : string;
    PLPCode : BYTE;
  end;


  TSampleType = (sampSimultaneous, sampSuccessive);

  { TMTS }

  {
    Implements
    Conditional Discriminations
    Successive and Simultaneous
    With or Without delay
  }
  TMTS = class sealed(TTrial)
  private
    FSampleType : TSampleType;
    FDelay : TTimer;
    FSample : TKeySupport;
    FSDataSupport : TSampleDataSupport;
    procedure SampleBackgroundMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DelayEnd(Sender: TObject);
    procedure SampleConsequence(Sender: TObject);
    procedure SampleResponse(Sender: TObject);
    procedure VisibleSample(AValue: Boolean);
  private
    FDataSupport : TDataSupport;
    FConsequence : TConsequenceSupport;
    //FConsequenceFired : Boolean;
    FNumComp : Integer;
    FComparisons : array of TKeySupport;
    procedure ComparBackgroundMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ComparisonConsequence(Sender : TObject);
    procedure ComparisonResponse(Sender : TObject);
    procedure ComparisonStart(Sender: TObject);
    procedure VisibleComparisons(AValue: Boolean);
  private
    procedure TrialBeforeEnd(Sender: TObject);
    procedure TrialResult(Sender: TObject);
    function GetHeader : string;
  protected
    // TTrial
    procedure TrialStart(Sender: TObject);
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TCustomControl); override;
    function AsString : string; override;
    procedure Play(ACorrection : Boolean); override;
  end;

implementation

uses StrUtils, Constants, Timestamps
  , Session.ConfigurationFile
  , Session.Configuration.GlobalContainer
  ;

constructor TMTS.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialStart := @TrialStart;
  Header := Header + #9 + GetHeader;
  FDelay := nil;
end;

procedure TMTS.Play(ACorrection : Boolean);
var
  i : integer;
  s1: string;
  R: TRect;
  LDelayed : Boolean;
  LDelay : integer;
  LParameters : TStringList;
begin
  inherited Play(ACorrection);
  LParameters := Configurations.Parameters;
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
    Key.Schedule.Enabled := False;

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

  FNumComp := StrToIntDef(LParameters.Values[_NumComp], 0);
  SetLength(FComparisons, FNumComp);
  for i := Low(FComparisons) to High(FComparisons) do
  with FComparisons[i] do
  begin
    // Owner is TGraphicControl
    Key := TKey.Create(Self);
    Key.Tag := i;
    Key.Cursor := Self.Cursor;

    s1 := LParameters.Values[_Comp + IntToStr(i + 1) + _cBnd] + #32;
    R.Top := StrToIntDef(ExtractDelimited(1,s1,[#32]),0);
    R.Left := StrToIntDef(ExtractDelimited(2,s1,[#32]),0);
    R.Bottom := StrToIntDef(ExtractDelimited(3,s1,[#32]),100);
    R.Right := StrToIntDef(ExtractDelimited(4,s1,[#32]),100);
    Key.SetBounds(R.Left, R.Top, R.Right, R.Bottom);

    s1 := LParameters.Values[_Comp + IntToStr(i + 1) + _cStm] + #32;
    Key.Filename := RootMedia + ExtractDelimited(1,s1,[#32]);

    Key.Schedule.Load(LParameters.Values[_Comp + IntToStr(i + 1) + _cSch]);
    Key.Schedule.Enabled := False;

    Key.Loops:= StrToIntDef(ExtractDelimited(2,s1,[#32]),0);
    Key.Color := StrToIntDef(ExtractDelimited(3,s1,[#32]), $0000FF); //clRed

    Csq := StrToIntDef(LParameters.Values[_Comp + IntToStr(i + 1) + _cCsq], 0);
    Usb := LParameters.Values[_Comp+IntToStr(i+1)+_cUsb];
    Msg := LParameters.Values[_Comp + IntToStr(i + 1) + _cMsg];
    Res := LParameters.Values[_Comp + IntToStr(i + 1) + _cRes];
    Nxt := LParameters.Values[_Comp + IntToStr(i + 1) + _cNxt];
    TO_ := StrToIntDef(LParameters.Values[_Comp + IntToStr(i + 1) + _cTO], 0);
    IET := LParameters.Values[_Comp + IntToStr(I + 1) + _cIET];
  end;

  if Self.ClassType = TMTS then Config(Self);
end;

function TMTS.AsString: string;
var
  i : integer;
  LTrial : TStringList;
begin
  LTrial := TStringList.Create;
  LTrial.BeginUpdate;
  if Self.ClassType = TMTS then
  begin
    LTrial.Append(TConfigurationFile.FullTrialSection(
      Counters.CurrentBlc, Counters.CurrentTrial));
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

  LTrial.Values[_NumComp] := IntToStr(Length(FComparisons));
  for i := Low(FComparisons) to High(FComparisons) do
  with FComparisons[i] do
  begin
    LTrial.Values[_Comp + IntToStr(i + 1) + _cSch] := Key.Schedule.AsString;
    LTrial.Values[_Comp + IntToStr(i + 1) + _cBnd] := Key.BoundsAsString;
    LTrial.Values[_Comp + IntToStr(i + 1) + _cStm] := ExtractFileName(Key.Filename);
    LTrial.Values[_Comp + IntToStr(i + 1) + _cMsg] := Msg;
    LTrial.Values[_Comp + IntToStr(i + 1) + _cRes] := Res;
    LTrial.Values[_Comp + IntToStr(i + 1) + _cNxt] := Nxt;
    LTrial.Values[_Comp + IntToStr(i + 1) + _cIET] := IET;
  end;

  LTrial.EndUpdate;
  Result := LTrial.Text;
  LTrial.Free;
end;

procedure TMTS.DelayEnd(Sender: TObject);
begin
  FDelay.Enabled:= False;
  ComparisonStart(Sender);
end;

procedure TMTS.SampleConsequence(Sender: TObject);
begin
  if Assigned(FSample.Key.OnConsequence) then
  begin
    FSample.Key.OnConsequence := nil;
    FSample.Key.Schedule.Enabled := False;
  end;
  case FSampleType of
    sampSimultaneous:
      if Assigned(FDelay) then
      begin
        FDelay.Enabled := True;
        FSDataSupport.SampleHide := TickCount - TimeStart;
      end;

    sampSuccessive:
      begin
        if FSample.Key.Kind.stmAudio then
          FSample.Key.Stop;
        if Assigned(FDelay) then
        begin
          FDelay.Enabled := True;
          VisibleSample(False);
        end else
        begin
          ComparisonStart(Sender);
        end;
      end;
  end;
end;

procedure TMTS.ComparisonConsequence(Sender: TObject);
var
{$IFDEF RS232}i: PtrInt;{$ENDIF}
  i: Integer;
begin
  //if not FConsequenceFired then
  //  FConsequenceFired := True;

  {$IFDEF RS232}
  if Sender is TKey then
    begin
      i := TKey(Sender).Tag;
      FConsequence.PLPCode:= FComparisons[i].Csq;
      FConsequence.RS232Code := FComparisons[i].Usb;
      Dispenser(FConsequence.PLPCode, FConsequence.RS232Code);
    end;
  {$ENDIF}

  //CounterManager.OnConsequence(Sender);
  //if Assigned(OnConsequence) then OnConsequence(Sender);
  if LimitedHold = 0 then
  begin
    VisibleComparisons(False);
    for i := Low(FComparisons) to High(FComparisons) do
    with FComparisons[i] do
    begin
      Key.OnConsequence := nil;
      Key.OnResponse := nil;
      Key.Schedule.Enabled := False;
    end;
  end;
  EndTrial(Sender);
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

        if Assigned(Counters.OnStmResponse) then Counters.OnStmResponse(Sender);
        if Assigned(OnStmResponse) then OnStmResponse(Self);
      end;
end;

procedure TMTS.ComparisonResponse(Sender: TObject);
var
  LResponseTime : Extended;
begin
  if Sender is TKey then
    if TKey(Sender).Visible then
      begin
        LResponseTime := LogEvent(FComparisons[TKey(Sender).Tag].Msg + #9 +
                                  TKey(Sender).LastResponseLog);

        if FDataSupport.Latency = TimeStart then
          FDataSupport.Latency := LResponseTime;

        if Assigned(OnStmResponse) then OnStmResponse (Self);
      end;
end;

procedure TMTS.VisibleSample(AValue: Boolean);
begin
  FSample.Key.Visible := AValue;
  if FSample.Key.Visible then
    FSDataSupport.SampleShow := LogEvent('Sample.Show,'+FSample.Msg)
  else
    FSDataSupport.SampleHide := LogEvent('Sample.Hide,'+FSample.Msg);
end;

procedure TMTS.VisibleComparisons(AValue: Boolean);
var
  i : integer;
  CmpMsg : string = '';
begin
  for i := Low(FComparisons) to High(FComparisons) do
    CmpMsg := CmpMsg + FComparisons[I].Msg + ',';

  for i := Low(FComparisons) to High(FComparisons) do
    FComparisons[i].Key.Visible := AValue;

  if AValue then
    FDataSupport.StmBegin := LogEvent('Comparisons.Show,'+ CmpMsg)
  else
    FDataSupport.StmEnd := LogEvent('Comparisons.Hide,'+ CmpMsg);
end;

function TMTS.GetHeader: string;
begin
  Result := rsReportStmMod + #9 +
            rsReportRspModLat + #9 +
            rsReportStmModBeg + #9 +
            rsReportStmModEnd + #9 +
            rsReportStmCmp + #9 +
            rsReportRspCmp + #9 +
            rsReportRspCmpLat + #9 +
            rsReportStmCmpBeg + #9 +
            rsReportStmCmpEnd;
end;

procedure TMTS.SampleBackgroundMouseDown(Sender: TObject; Button: TMouseButton;
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

  Counters.OnBkgndResponse(Self);
  if Assigned(OnBkGndResponse) then OnBkGndResponse(Self);
end;

procedure TMTS.ComparBackgroundMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  LogEvent('BK.C' + #9 + IntToStr(X) + #9 + IntToStr(Y));
  //Inc(FDataSupport.BackgroundResponses); // local
  //CounterManager.OnBkgndResponse(Self); // global
  if Assigned(OnBkGndResponse) then OnBkGndResponse(Self);
end;

procedure TMTS.TrialStart(Sender: TObject);
begin
  FSDataSupport.SampLatency := TimeStart;
  FSDataSupport.SampleShow := TickCount;
  VisibleSample(True);

  FSample.Key.Schedule.Enabled := True;
  if FSample.Key.Kind.stmAudio then
    FSample.Key.Play;

  // todo: video autostart?
  // if FSample.Key.Kind.stmImage = stmVideo then
  //   FSample.Key.Play;
  case FSampleType of
    sampSimultaneous :
      if not Assigned(FDelay) then
        ComparisonStart(Sender)
      else
        OnMouseDown := @SampleBackgroundMouseDown;
    sampSuccessive : OnMouseDown := @SampleBackgroundMouseDown;
  end;
end;

procedure TMTS.ComparisonStart(Sender: TObject);
var
  i: Integer;
begin
  with FDataSupport do
  begin
    Latency := TimeStart;
    StmEnd := TimeStart;
  end;
  for i := Low(FComparisons) to High(FComparisons) do
  with FComparisons[i] do
  begin
    Key.OnConsequence := @ComparisonConsequence;
    Key.OnResponse := @ComparisonResponse;
    // Parent is TCustomControl/TForm
    Key.Parent := Self.Parent;
    Key.Schedule.Enabled := True;
  end;
  VisibleComparisons(True);
  OnMouseDown := @ComparBackgroundMouseDown;
end;

procedure TMTS.WriteData(Sender: TObject);
var
  Pos_Mod : string = '';
  Lat_Mod : string = '';
  Atr_Mod : string = '';
  Beg_Mod : string = '';
  Pos_Cmp : string = '';
  Lat_Cmp : string = '';
  Res_Cmp : string = '';
  End_Cmp: string = '';
  Beg_Cmp: string = '';
  i: Integer;
begin
  inherited WriteData(Sender);
  case FSample.Msg of
    '','AUTO':Pos_Mod:= FSample.Key.ShortName+' - '+'('+IntToStr(FSample.Key.Top)+','+IntToStr(FSample.Key.Left)+')';
    else
      Pos_Mod:= FSample.Msg;
  end;

  if FSDataSupport.SampLatency = TimeStart then
    Lat_Mod := #32#32#32#32#32#32 + 'NA'
  else Lat_Mod := TimestampToStr(FSDataSupport.SampLatency - TimeStart);


  if Assigned(FDelay) then
    Atr_Mod := TimestampToStr(FSDataSupport.SampleHide)
  else
    Atr_Mod := #32#32#32#32#32#32 + 'NA';

  //Frq_Mod := _Samp + '=' +
  //           IntToStr(FSample.Key.ResponseCount) + #9 +
  //           'BK.S='+ IntToStr(FSDataSupport.SampBkndRespCount) + #9 +
  //           'BK.D='+ IntToStr(FSDataSupport.DelaBkndRespCount);

  Beg_Mod := TimestampToStr(FSDataSupport.SampleShow);

  // comparisons
  for i:= Low(FComparisons) to High(FComparisons) do
  begin
    if FComparisons[i].Msg = '' then FComparisons[i].Msg:= '-';
    Pos_Cmp:= Pos_Cmp + FComparisons[i].Msg + #32;
  end;

  if FConsequence.ReportMessage = '' then FConsequence.ReportMessage := '-';
  Res_Cmp:= FConsequence.ReportMessage;

  if FDataSupport.Latency = TimeStart then
    Lat_Cmp := #32#32#32#32#32#32 + 'NA'
  else
    Lat_Cmp := TimestampToStr(FDataSupport.Latency);

  //Disp:= RightStr(IntToBin(FConsequence.PLPCode, 9)+#0, 8);
  //uDisp := FConsequence.RS232Code;
  Beg_Cmp := TimestampToStr(FDataSupport.StmBegin);
  End_Cmp := TimestampToStr(FDataSupport.StmEnd);

  Data := Data +
          Pos_Mod + #9 +
          Lat_Mod + #9 +
          Beg_Mod + #9 +
          Atr_Mod + #9 +

          Pos_Cmp + #9 +
          Res_Cmp + #9 +
          Lat_Cmp + #9 +
          Beg_Cmp + #9 +
          End_Cmp;
end;

procedure TMTS.TrialBeforeEnd(Sender: TObject);
begin
  TrialResult(Sender);
  WriteData(Self);
end;

procedure TMTS.TrialResult(Sender: TObject);
var i : integer;
begin
  FDataSupport.StmEnd := TickCount - TimeStart;
  FResponseEnabled:= False;

  if Sender is TKey then
    begin
      i := TKey(Sender).Tag;
      TimeOut := FComparisons[i].TO_;
      IETConsequence := FComparisons[i].IET;

      case FComparisons[i].Res of
        T_HIT : Result := T_HIT;
        T_MISS : Result := T_MISS;
      else
        Result := T_NONE;
      end;

      if FComparisons[i].Nxt = T_CRT then
        NextTrial := T_CRT
      else
        NextTrial := FComparisons[i].Nxt;

      case FComparisons[i].Msg of
        '','AUTO' :
          FConsequence.ReportMessage := TKey(Sender).ShortName+' - ' +
                                       '('+IntToStr(TKey(Sender).Top) +
                                       ','+IntToStr(TKey(Sender).Left)+')';
        else
          FConsequence.ReportMessage:= FComparisons[i].Msg;
      end;
    end;
end;

end.

{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.Abstract;

{$mode objfpc}{$H+}

interface

uses Controls, ExtCtrls, Classes, SysUtils
  , Session.Configuration
  , Loggers.Reports
  , CounterManager
  {$IFDEF RS232}
  , Devices.RS232i
  {$ENDIF}
  //, interface_plp
  ;

type

  { TCounterType }
  TCounterType = (ctNone, ctBlocPoints, ctSessionPoints, ctCustom);

  { TPaintEvent }
  TPaintEvent = procedure of object;

  { TTrial }
  TTrial = class(TGraphicControl)
  private
    FConfigurations: TCfgTrial;
    FClock : TTimer;
    FLogEvent: TDataProcedure;
    FData,
    FFilename,
    FHeader,
    FHeaderTimestamps,
    FResult,
    FIETConsequence : string;
    FStarterLatency : Extended;
    FLimitedHold,
    FTimeOut : Integer;
    FShowStarter : Boolean;
    function GetRootMedia: string;
    function GetTestMode: Boolean;
    function GetTimeStart: Extended;
    procedure BeginStarter;
    procedure SetRootMedia(AValue: string);
    procedure SetTestMode(AValue: Boolean);
    procedure StartTrial(Sender: TObject);
    procedure TrialKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FOnBeginCorrection: TNotifyEvent;
    FOnBkGndResponse: TNotifyEvent;
    FOnConsequence: TNotifyEvent;
    FOnEndCorrection: TNotifyEvent;
    FOnHit: TNotifyEvent;
    FOnMiss: TNotifyEvent;
    FOnNone: TNotifyEvent;
    FOnStmResponse: TNotifyEvent;
    FOnTrialBeforeEnd: TNotifyEvent;
    FOnTrialEnd: TNotifyEvent;
    FOnTrialKeyDown: TKeyEvent;
    FOnTrialKeyUp: TKeyEvent;
    FOnTrialMouseDown: TMouseEvent;
    FOnTrialPaint: TPaintEvent;
    FOnTrialStart: TNotifyEvent;
    FOnTrialWriteData: TNotifyEvent;
    FOldKeyUp : TKeyEvent;
    FOldKeyDown: TKeyEvent;
    procedure EndTrialThread(Sender: TObject);
    procedure SetConfigurations(AValue: TCfgTrial);
    procedure SetLimitedHold(AValue: integer);
    procedure SetOnTrialBeforeEnd(AValue: TNotifyEvent);
    procedure SetOnBeginCorrection(AValue: TNotifyEvent);
    procedure SetOnBkGndResponse(AValue: TNotifyEvent);
    procedure SetOnConsequence(AValue: TNotifyEvent);
    procedure SetOnEndCorrection(AValue: TNotifyEvent);
    procedure SetOnTrialEnd(AValue: TNotifyEvent);
    procedure SetOnHit(AValue: TNotifyEvent);
    procedure SetOnMiss(AValue: TNotifyEvent);
    procedure SetOnNone(AValue: TNotifyEvent);
    procedure SetOnStmResponse(AValue: TNotifyEvent);
    procedure SetOnTrialKeyDown(AValue: TKeyEvent);
    procedure SetOnTrialKeyUp(AValue: TKeyEvent);
    procedure SetOnTrialMouseDown(AValue: TMouseEvent);
    procedure SetOnTrialPaint(AValue: TPaintEvent);
    procedure SetOnTrialStart(AValue: TNotifyEvent);
    procedure SetOnTrialWriteData(AValue: TNotifyEvent);
  strict protected
    FResponseEnabled,
    FIscorrection : Boolean;
    {$ifdef DEBUG}
      procedure ClockStatus(msg : string);
    {$endif}
    function LogEvent(ACode: string) : Extended; overload;
    function LogEvent(ACode: string; ATimestamp: Extended): Extended; overload;
    procedure EndTrial(Sender: TObject);
    procedure Config(Sender: TObject);
    procedure WriteData(Sender: TObject); virtual;
    property OnTrialKeyDown : TKeyEvent read FOnTrialKeyDown write SetOnTrialKeyDown;
    property OnTrialKeyUp : TKeyEvent read FOnTrialKeyUp write SetOnTrialKeyUp;
    property OnTrialPaint: TPaintEvent read FOnTrialPaint write SetOnTrialPaint;
    property OnTrialStart: TNotifyEvent read FOnTrialStart write SetOnTrialStart;
    property OnTrialMouseDown: TMouseEvent read FOnTrialMouseDown write SetOnTrialMouseDown;
  protected
    FShowCounter : Boolean;
    FCounterType : TCounterType;
    FCounterRectTopRight : TRect;
    FCounterRectTopLeft : TRect;
    FNextTrial : string;
    CounterManager : TCounterManager;
    procedure MouseDown(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    {$IFDEF RS232}
    procedure Dispenser(AParallelPort: Byte; ARS232: string);
    {$ENDIF}
  public
    constructor Create(AOwner : TCustomControl); virtual; reintroduce;
    destructor Destroy; override;
    function AsString : string; virtual; abstract;
    function HasVisualConsequence : Boolean; virtual;
    function ConsequenceInterval : integer; virtual;
    procedure StopConsequence; virtual;
    procedure Play(ACorrection: Boolean=False); virtual;
    procedure Hide; virtual;
    procedure Show; virtual;
    procedure SetFocus; virtual;
    procedure HideCounter;
    property Configurations: TCfgTrial read FConfigurations write SetConfigurations;
    property Data: string read FData write FData;
    property FileName : string read FFilename write FFilename;
    property Header: string read FHeader write FHeader;
    property HeaderTimestamps: string read FHeaderTimestamps write FHeaderTimestamps;
    property IETConsequence : string read FIETConsequence write FIETConsequence;
    property NextTrial: string read FNextTrial write FNextTrial;
    property Result: string read FResult write FResult;
    property LimitedHold : Integer read FLimitedHold write SetLimitedHold;
    property RootMedia : string read GetRootMedia write SetRootMedia;
    property SaveData : TDataProcedure read FLogEvent write FLogEvent;
    property TestMode : Boolean read GetTestMode write SetTestMode;
    property TimeOut : Integer read FTimeOut write FTimeOut;
    property TimeStart : Extended read GetTimeStart;
  public
    property OnTrialBeforeEnd: TNotifyEvent read FOnTrialBeforeEnd write SetOnTrialBeforeEnd;
    property OnBeginCorrection : TNotifyEvent read FOnBeginCorrection write SetOnBeginCorrection;
    property OnBkGndResponse: TNotifyEvent read FOnBkGndResponse write SetOnBkGndResponse;
    property OnConsequence: TNotifyEvent read FOnConsequence write SetOnConsequence;
    property OnEndCorrection : TNotifyEvent read FOnEndCorrection write SetOnEndCorrection;
    property OnTrialEnd: TNotifyEvent read FOnTrialEnd write SetOnTrialEnd;
    property OnHit: TNotifyEvent read FOnHit write SetOnHit;
    property OnMiss: TNotifyEvent read FOnMiss write SetOnMiss;
    property OnNone: TNotifyEvent read FOnNone write SetOnNone;
    property OnStmResponse: TNotifyEvent read FOnStmResponse write SetOnStmResponse;
  end;

resourcestring
  SESSION_CANCELED = '(Sessão cancelada)';

implementation


uses Graphics, Constants, Timestamps, Canvas.Helpers
   , Session.Configuration.GlobalContainer
    {$ifdef DEBUG}
    , Loggers.Debug
    {$endif}
    ;

{ TTrial }

{$ifdef DEBUG}
procedure TTrial.ClockStatus(msg: string);
begin
  DebugLn(msg);
end;
{$endif}

procedure TTrial.Config(Sender: TObject);
begin
  FStarterLatency := TimeStart;
  if FShowStarter then
    begin
      LogEvent('S');
      BeginStarter;
      Exit;
    end;
  StartTrial(Sender);
end;

procedure TTrial.WriteData(Sender: TObject);
begin
  Data := Result + #9;
  if FStarterLatency <> TimeStart then
    Data := Data + TimestampToStr(FStarterLatency - TimeStart) + #9;
end;

procedure TTrial.TrialKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  //if FResponseEnabled and (Key = 27 {ESC}) then
  //  begin
  //    FResponseEnabled:= False;
  //    Invalidate;
  //    Exit;
  //  end;

  //if (ssCtrl in shift) and (Key = 113 { q }) then
  //  begin
  //    FResponseEnabled:= False;
  //    Data := Data + LineEnding + SESSION_CANCELED + LineEnding;
  //    Result := T_NONE;
  //    IETConsequence := T_NONE;
  //    NextTrial := T_END;
  //    FClock.Enabled := False;
  //    Exit;
  //  end;

  //if (ssCtrl in Shift) and (Key = 13 { Enter }) then
  //  begin
  //    FResponseEnabled := False;
  //    Result := T_MISS;
  //    //IETConsequence := T_NONE;
  //    NextTrial := '0';
  //    FClock.Enabled := False;  // EndTrial(Self);
  //    Exit;
  //  end;

  if Assigned(OnTrialKeyDown) and FResponseEnabled then OnTrialKeyDown(Sender,Key,Shift);
end;

procedure TTrial.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = 13) then
    begin
      Result:=T_MISS;
      if Assigned(OnTrialEnd) then OnTrialEnd(Self);
    end;

  if (not FResponseEnabled) and (Key = 27) { ESC } then
    begin
      FResponseEnabled:= True;
      Invalidate;
      Exit;
    end;

  if FShowStarter and (Key = 32) { SPACE } then
    begin
      FShowStarter := False;
      FStarterLatency := TickCount;
      StartTrial(Sender);
      Exit;
    end;

  if Assigned(OnTrialKeyUp) and FResponseEnabled then OnTrialKeyUp(Sender,Key,Shift);
end;

procedure TTrial.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  { do something }
  if Assigned(OnTrialMouseDown) then OnTrialMouseDown(Self, Button, Shift, X, Y);
end;

procedure TTrial.EndTrial(Sender: TObject);
begin
  {$ifdef DEBUG}
    DebugLn(mt_Debug + 'TTrial.EndTrial1');
  {$endif}
  if FLimitedHold = 0 then
    EndTrialThread(Sender);
end;

procedure TTrial.EndTrialThread(Sender: TObject);
begin
  if Assigned(FClock) then
    FClock.Enabled := False;
  {$ifdef DEBUG}
    DebugLn(mt_Debug + 'TTrial.EndTrial2');
  {$endif}

  LogEvent('TE');
  FResponseEnabled:= False;
  Hide;
  if Assigned(OnTrialBeforeEnd) then OnTrialBeforeEnd(Sender);
  if Parent is TCustomControl then
    with TCustomControl(Parent) do
      begin
        OnKeyDown := FOldKeyDown;
        OnKeyUp := FOldKeyUp;
      end;
  case Result of
    T_HIT : if Assigned(OnHit) then OnHit(Sender);
    T_MISS: if Assigned(OnMiss) then OnMiss(Sender);
    T_NONE: if Assigned(OnNone) then OnNone(Sender);
  end;

  if FIsCorrection then
    if Assigned(OnEndCorrection) then OnEndCorrection(Sender);

  if Assigned(OnTrialEnd) then OnTrialEnd(Self);
end;

procedure TTrial.SetConfigurations(AValue: TCfgTrial);
begin
  if FConfigurations.Id = AValue.Id then Exit;
  FConfigurations.Id := AValue.Id;
  FConfigurations.Kind := AValue.Kind;
  FConfigurations.Name := AValue.Name;
  FConfigurations.NumComp := AValue.NumComp;
  FConfigurations.SList.Assign(AValue.SList);
end;

procedure TTrial.SetLimitedHold(AValue: integer);
begin
  if FLimitedHold=AValue then Exit;
  FLimitedHold := AValue;
  FClock.Interval := FLimitedHold;
end;

procedure TTrial.Paint;
  procedure DrawCounterTopRight;
  var
    LTextStyle : TTextStyle;
    LPoints : integer;
  begin
    case FCounterType of
      ctBlocPoints : LPoints := CounterManager.BlcPoints;
      ctSessionPoints : LPoints := CounterManager.SessionPointsTopRight;
      ctCustom : LPoints := CounterManager.SessionPointsTopRight;
    end;

    LTextStyle := Canvas.TextStyle;
    LTextStyle.SingleLine:=False;
    LTextStyle.Wordbreak:=True;
    LTextStyle.Clipping:=False;
    LTextStyle.Alignment:=taCenter;
    LTextStyle.Layout:=tlCenter;
    Canvas.TextStyle := LTextStyle;
    with Canvas do
      begin
        Font.Size := 22;
        Font.Color:= 0;
        Pen.Width := 2;
        Pen.Color := 0;
        Rectangle(FCounterRectTopRight);
        TextRect(FCounterRectTopRight, FCounterRectTopRight.Left,
          FCounterRectTopRight.Top, 'Pontos: ' + LPoints.ToString);
      end;
  end;

  procedure DrawCounterTopLeft;
  var
    LTextStyle : TTextStyle;
    LPoints : integer;
  begin
    case FCounterType of
      ctCustom : LPoints := CounterManager.SessionPointsTopLeft;
      else { do nothing };
    end;

    LTextStyle := Canvas.TextStyle;
    LTextStyle.SingleLine:=False;
    LTextStyle.Wordbreak:=True;
    LTextStyle.Clipping:=False;
    LTextStyle.Alignment:=taCenter;
    LTextStyle.Layout:=tlCenter;
    Canvas.TextStyle := LTextStyle;
    with Canvas do
      begin
        Font.Size := 22;
        Font.Color:= 0;
        Pen.Width := 2;
        Pen.Color := 0;
        Rectangle(FCounterRectTopLeft);
        TextRect(FCounterRectTopLeft, FCounterRectTopLeft.Left,
          FCounterRectTopLeft.Top, 'Pontos: ' + LPoints.ToString);
      end;
  end;
begin
  inherited Paint;
  if FShowStarter then begin
    DrawCenteredCircle(Canvas, Width, Height, 6);
    Exit;
  end;

  if FShowCounter then begin
    case FCounterType of
      ctNone : { do nothing };
      ctBlocPoints, ctSessionPoints: DrawCounterTopRight;
      ctCustom :
        begin
          DrawCounterTopRight;
          DrawCounterTopLeft;
        end;
    end;
  end;

  if Assigned(OnTrialPaint) and FResponseEnabled then OnTrialPaint;
end;

{$IFDEF RS232}
procedure TTrial.Dispenser(AParallelPort: Byte; ARS232: string);
begin
    //PLP.OutPortOn(Csq);
    if ARS232 <> '' then
      RS232.Dispenser(ARS232);
end;
{$ENDIF}

constructor TTrial.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  CounterManager := GlobalContainer.CounterManager;
  FConfigurations.Id := -1;
  FConfigurations.NumComp := 0;
  FConfigurations.Name := T_NONE;
  FConfigurations.Kind := 'abstract';
  FConfigurations.SList := TStringList.Create;
  FResponseEnabled := False;
  FShowStarter := False;
  NextTrial := '0';
  Result := T_NONE;
  IETConsequence := T_NONE;
  Color := 0;
  Cursor:= -1;
  Align := alClient;
  with AOwner do
    begin
      FOldKeyUp := OnKeyUp;
      FOldKeyDown := OnKeyDown;
      OnKeyUp := @TrialKeyUp;
      OnKeyDown := @TrialKeyDown;
    end;
  Parent := AOwner;

  //FLimitedhold controls Trial ending.
  FLimitedHold := 0;
  FClock := TTimer.Create(Self);
  FClock.Interval := FLimitedHold;
  FClock.Enabled := False;
  FClock.OnTimer := @EndTrialThread;
  //FClock.OnStopTimer := @EndTrialThread;

  // setup report header
  // descendents should concatenate its own data, if any, to their OnCreate method
  Header := rsReportCsqRes;

  // setup timestamps header
  // descendents should concatenate its own data, if any, to their OnCreate method
  HeaderTimestamps := rsReportTime + #9 +
                      rsReportBlocID + #9 +
                      rsReportTrialID + #9 +
                      rsReportTrialNO + #9 +
                      rsReportTrialName + #9 +
                      rsReportEvent;
end;

destructor TTrial.Destroy;
begin
  {$ifdef DEBUG}
    DebugLn(mt_Debug + 'TTrial.Destroy:FNextTrial:' + FNextTrial);
  {$endif}
  FConfigurations.SList.Free;
  inherited Destroy;
end;

function TTrial.HasVisualConsequence: Boolean;
begin
  Result := IETConsequence <> T_NONE;
end;

function TTrial.ConsequenceInterval: integer;
begin
  // uses ITI by default, overrided as needed
  Result := StrToIntDef(Configurations.SList.Values[_ITI], 0);
end;

procedure TTrial.StopConsequence;
begin

end;

procedure TTrial.Play(ACorrection: Boolean);
var
  LParameters : TStringList;
begin
  LParameters := Configurations.SList;

  // avoid responses while loading configurations
  FResponseEnabled := False;

  // what will be the next trial?
  NextTrial := LParameters.Values[_NextTrial];

  // will the trial count as MISS, HIT or NONE?
  Result := LParameters.Values[_cRes];
  if Result = '' then
    Result := T_NONE;

  // what will happen during the inter trial interval?
  if LParameters.Values[_Consequence] = '' then
    IETConsequence := T_NONE
  else IETConsequence := LParameters.Values[_Consequence];

  // is it a correction trial?
  if ACorrection then
    FIsCorrection := True
  else
    FIsCorrection := False;

  // Trial background color
  Color:= StrToIntDef(LParameters.Values[_BkGnd], Parent.Color);

  // Trial will last LimitedHold ms if LimitedHold > 0
  FLimitedHold := StrToIntDef(LParameters.Values[_LimitedHold], 0);
  FClock.Interval := FLimitedHold;

  // Presents a dot at the screen center. A key response is required before trialstart
  FShowStarter := StrToBoolDef(LParameters.Values[_ShowStarter], False);
  if FShowStarter then
    Header := 'Str.Lat.' + #9 + Header;

  // counters visibility default
  FShowCounter := False;

  // set counter reacts
  FCounterRectTopRight :=
    Rect((ClientRect.Right div 7)*6, 10, ClientRect.Right-10, 70);

  FCounterRectTopLeft :=
    Rect(10, 10, (ClientRect.Right div 7)-10, 70);

  // how counters/points are presented
  case StrToIntDef(LParameters.Values[_CounterType], -1) of
    0 :  FCounterType := ctBlocPoints;
    1 :  FCounterType := ctSessionPoints;
    2 :  FCounterType := ctCustom;
    else FCounterType := ctNone;
  end;

  // image of the mouse cursor
  if TestMode then Cursor:= 0
  else Cursor:= StrToIntDef(LParameters.Values[_Cursor], 0);
end;

procedure TTrial.Hide;
var
  i : integer;
begin
  inherited Hide;
  if ComponentCount > 0 then
    for i := 0 to ComponentCount -1 do
      if Components[i] is TControl then
        TControl(Components[i]).Hide;
end;

procedure TTrial.Show;
var
  i : integer;
begin
  inherited Show;
  if ComponentCount > 0 then
    for i := 0 to ComponentCount -1 do
      if Components[i] is TControl then
        TControl(Components[i]).Show;
end;

procedure TTrial.SetFocus;
begin
  TCustomControl(Parent).SetFocus;
end;

procedure TTrial.HideCounter;
begin
  FShowCounter := False;
end;

procedure TTrial.BeginStarter;
begin
  FResponseEnabled:= True;
  Show;
end;

procedure TTrial.StartTrial(Sender: TObject);
begin
  if FClock.Interval > 0 then
    FClock.Enabled := True;
  LogEvent('TS');
  FResponseEnabled := True;
  Show;
  SetFocus;
  if FIsCorrection then
    if Assigned(OnBeginCorrection) then OnBeginCorrection(Sender);
  if Assigned(OnTrialStart) then OnTrialStart(Sender);
end;

function TTrial.LogEvent(ACode: string): Extended;
begin
  Result := TickCount - TimeStart;
  SaveData(TimestampToStr(Result) + #9 +
           IntToStr(CounterManager.CurrentBlc+1) + #9 +
           IntToStr(CounterManager.CurrentTrial+1) + #9 +
           IntToStr(CounterManager.SessionTrials+1) + #9 + // Current trial cycle
           Configurations.Name + #9 +
           ACode + LineEnding);
end;

function TTrial.LogEvent(ACode : string; ATimestamp : Extended) : Extended;
begin
  Result := ATimestamp - TimeStart;
  SaveData(TimestampToStr(Result) + #9 +
           IntToStr(CounterManager.CurrentBlc+1) + #9 +
           IntToStr(CounterManager.CurrentTrial+1) + #9 +
           IntToStr(CounterManager.SessionTrials+1) + #9 + // Current trial cycle
           Configurations.Name + #9 +
           ACode + LineEnding);
end;

procedure TTrial.SetOnTrialBeforeEnd(AValue: TNotifyEvent);
begin
  if FOnTrialBeforeEnd = AValue then Exit;
  FOnTrialBeforeEnd := AValue;
end;

procedure TTrial.SetOnBeginCorrection(AValue: TNotifyEvent);
begin
  if FOnBeginCorrection = AValue then Exit;
  FOnBeginCorrection := AValue;
end;

procedure TTrial.SetOnBkGndResponse(AValue: TNotifyEvent);
begin
  if FOnBkGndResponse = AValue then Exit;
  FOnBkGndResponse := AValue;
end;

procedure TTrial.SetOnConsequence(AValue: TNotifyEvent);
begin
  if FOnConsequence = AValue then Exit;
  FOnConsequence := AValue;
end;

procedure TTrial.SetOnEndCorrection(AValue: TNotifyEvent);
begin
  if FOnEndCorrection = AValue then Exit;
  FOnEndCorrection := AValue;
end;

procedure TTrial.SetOnTrialEnd(AValue: TNotifyEvent);
begin
  if FOnTrialEnd = AValue then Exit;
  FOnTrialEnd := AValue;
end;

procedure TTrial.SetOnHit(AValue: TNotifyEvent);
begin
  if FOnHit = AValue then Exit;
  FOnHit := AValue;
end;

procedure TTrial.SetOnMiss(AValue: TNotifyEvent);
begin
  if FOnMiss = AValue then Exit;
  FOnMiss := AValue;
end;

procedure TTrial.SetOnNone(AValue: TNotifyEvent);
begin
  if FOnNone = AValue then Exit;
  FOnNone := AValue;
end;

procedure TTrial.SetOnTrialStart(AValue: TNotifyEvent);
begin
  if FOnTrialStart = AValue then Exit;
  FOnTrialStart := AValue;
end;

procedure TTrial.SetOnStmResponse(AValue: TNotifyEvent);
begin
  if FOnStmResponse = AValue then Exit;
  FOnStmResponse := AValue;
end;

procedure TTrial.SetOnTrialKeyDown(AValue: TKeyEvent);
begin
  if FOnTrialKeyDown = AValue then Exit;
  FOnTrialKeyDown := AValue;
end;

procedure TTrial.SetOnTrialKeyUp(AValue: TKeyEvent);
begin
  if FOnTrialKeyUp = AValue then Exit;
  FOnTrialKeyUp := AValue;
end;

procedure TTrial.SetOnTrialMouseDown(AValue: TMouseEvent);
begin
  if FOnTrialMouseDown=AValue then Exit;
  FOnTrialMouseDown:=AValue;
end;

procedure TTrial.SetOnTrialPaint(AValue: TPaintEvent);
begin
  if FOnTrialPaint = AValue then Exit;
  FOnTrialPaint := AValue;
end;

procedure TTrial.SetOnTrialWriteData(AValue: TNotifyEvent);
begin
  if FOnTrialWriteData = AValue then Exit;
  FOnTrialWriteData := AValue;
end;

function TTrial.GetRootMedia: string;
begin
  Result := GlobalContainer.RootMedia;
end;

function TTrial.GetTestMode: Boolean;
begin
  Result := GlobalContainer.TestMode;
end;

function TTrial.GetTimeStart: Extended;
begin
  Result := GlobalContainer.TimeStart;
end;

procedure TTrial.SetRootMedia(AValue: string);
begin
  if GlobalContainer.RootMedia=AValue then Exit;
  GlobalContainer.RootMedia:=AValue;
end;

procedure TTrial.SetTestMode(AValue: Boolean);
begin
  if GlobalContainer.TestMode=AValue then Exit;
  GlobalContainer.TestMode:=AValue;
end;


end.

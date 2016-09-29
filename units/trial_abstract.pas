{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit trial_abstract;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils, LCLProc

  , schedules_main
  , config_session
  , countermanager
  , custom_timer
  ;

type

  { TObjectProcedure }

  TPaintEvent = procedure of object;

  { TTrial }

  TTrial = class(TCustomControl)
  private
    FGlobalContainer: TGlobalContainer;
    FCfgTrial: TCfgTrial;
    FClockThread : TClockThread;
    FCounterManager : TCounterManager;
    FLogEvent: TDataProcedure;
    FClockList : array of TThreadMethod;
    FData,
    FFilename,
    FHeader,
    FHeaderTimestamps,
    FResult,
    FIETConsequence,
    FNextTrial : string;
    FStarterLatency : Extended;
    FLimitedHold,
    FTimeOut : Integer;
    FShowStarter : Boolean;

  { events }

    FOnBeforeEndTrial: TNotifyEvent;
    FOnBeginCorrection: TNotifyEvent;
    FOnBkGndResponse: TNotifyEvent;
    FOnConsequence: TNotifyEvent;
    FOnEndCorrection: TNotifyEvent;
    FOnEndTrial: TNotifyEvent;
    FOnHit: TNotifyEvent;
    FOnMiss: TNotifyEvent;
    FOnNone: TNotifyEvent;
    FOnStmResponse: TNotifyEvent;
    FOnTrialKeyDown: TKeyEvent;
    FOnTrialKeyUp: TKeyEvent;
    FOnTrialPaint: TPaintEvent;
    FOnTrialStart: TNotifyEvent;
    FOnWriteTrialData: TNotifyEvent;
    function GetRootMedia: string;
    function GetTestMode: Boolean;
    function GetTimeStart: Extended;
    procedure BeginStarter;
    procedure EndTrialThread(Sender: TObject);
    procedure SetOnBeforeEndTrial(AValue: TNotifyEvent);
    procedure SetOnBeginCorrection(AValue: TNotifyEvent);
    procedure SetOnBkGndResponse(AValue: TNotifyEvent);
    procedure SetOnConsequence(AValue: TNotifyEvent);
    procedure SetOnEndCorrection(AValue: TNotifyEvent);
    procedure SetOnEndTrial(AValue: TNotifyEvent);
    procedure SetOnHit(AValue: TNotifyEvent);
    procedure SetOnMiss(AValue: TNotifyEvent);
    procedure SetOnNone(AValue: TNotifyEvent);
    procedure SetOnStmResponse(AValue: TNotifyEvent);
    procedure SetOnTrialKeyDown(AValue: TKeyEvent);
    procedure SetOnTrialKeyUp(AValue: TKeyEvent);
    procedure SetOnTrialPaint(AValue: TPaintEvent);
    procedure SetOnTrialStart(AValue: TNotifyEvent);
    procedure SetOnWriteTrialData(AValue: TNotifyEvent);
    procedure SetRootMedia(AValue: string);
    procedure SetTestMode(AValue: Boolean);
    procedure StartClockList;
    procedure StartTrial(Sender: TObject);
    procedure TrialKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  strict protected
    FResponseEnabled,
    FIscorrection : Boolean;
    {$ifdef DEBUG}
      procedure ClockStatus(msg : string);
    {$endif}
    procedure AddToClockList(AClockStart : TThreadMethod); overload;
    procedure AddToClockList(ASchedule: TSchMan); overload;
    procedure BeforeEndTrial(Sender: TObject); virtual; abstract;
    procedure EndTrial(Sender: TObject);
    procedure LogEvent(ACode: string);
    procedure Config(Sender: TObject);
    procedure WriteData(Sender: TObject); virtual;
    property OnTrialKeyDown : TKeyEvent read FOnTrialKeyDown write SetOnTrialKeyDown;
    property OnTrialKeyUp : TKeyEvent read FOnTrialKeyUp write SetOnTrialKeyUp;
    property OnTrialPaint: TPaintEvent read FOnTrialPaint write SetOnTrialPaint;
    property OnTrialStart: TNotifyEvent read FOnTrialStart write SetOnTrialStart;
  protected
    procedure Paint; override;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Play(ACorrection: Boolean); virtual;
    property CfgTrial: TCfgTrial read FCfgTrial write FCfgTrial;
    property CounterManager : TCounterManager read FCounterManager write FCounterManager;
    property Data: string read FData write FData;
    property FileName : string read FFilename write FFilename;
    property GlobalContainer : TGlobalContainer read FGlobalContainer write FGlobalContainer;
    property Header: string read FHeader write FHeader;
    property HeaderTimestamps: string read FHeaderTimestamps write FHeaderTimestamps;
    property IETConsequence : string read FIETConsequence write FIETConsequence;
    property NextTrial: string read FNextTrial write FNextTrial;
    property Result: string read FResult write FResult;
    property RootMedia : string read GetRootMedia write SetRootMedia;
    property SaveTData : TDataProcedure read FLogEvent write FLogEvent;
    property TestMode : Boolean read GetTestMode write SetTestMode;
    property TimeOut : Integer read FTimeOut write FTimeOut;
    property TimeStart : Extended read GetTimeStart;
  public
    property OnBeforeEndTrial: TNotifyEvent read FOnBeforeEndTrial write SetOnBeforeEndTrial;
    property OnBeginCorrection : TNotifyEvent read FOnBeginCorrection write SetOnBeginCorrection;
    property OnBkGndResponse: TNotifyEvent read FOnBkGndResponse write SetOnBkGndResponse;
    property OnConsequence: TNotifyEvent read FOnConsequence write SetOnConsequence;
    property OnEndCorrection : TNotifyEvent read FOnEndCorrection write SetOnEndCorrection;
    property OnEndTrial: TNotifyEvent read FOnEndTrial write SetOnEndTrial;
    property OnHit: TNotifyEvent read FOnHit write SetOnHit;
    property OnMiss: TNotifyEvent read FOnMiss write SetOnMiss;
    property OnNone: TNotifyEvent read FOnNone write SetOnNone;
    property OnStmResponse: TNotifyEvent read FOnStmResponse write SetOnStmResponse;
    property OnWriteTrialData: TNotifyEvent read FOnWriteTrialData write SetOnWriteTrialData;
  end;

implementation


uses constants, timestamps, draw_methods
    {$ifdef DEBUG}
    , debug_logger
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
  if FStarterLatency <> TimeStart then
    Data := TimestampToStr(FStarterLatency - TimeStart) + #9;
end;

procedure TTrial.TrialKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if (Key = 27 {ESC}) then
    begin
      FResponseEnabled:= False;
      Invalidate;
    end;

  if (ssCtrl in shift) and (Key = 81) { q } then
    begin
      FResponseEnabled:= False;
      Data := Data + LineEnding + '(Sessão cancelada)' + #9#9#9#9#9#9#9#9#9 + LineEnding;
      Result := T_NONE;
      IETConsequence := T_NONE;
      NextTrial := T_END;
      EndTrial(Self);
    end;

  if (ssCtrl in Shift) and (Key = 13) { Enter } then
    begin
      FResponseEnabled := False;
      Result := T_NONE;
      IETConsequence := T_NONE;
      NextTrial := '0';
      EndTrial(Self);
    end;

  if Assigned(OnTrialKeyDown) and FResponseEnabled then OnTrialKeyDown(Sender,Key,Shift);
end;

procedure TTrial.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 27 { ESC } then
    begin
      FResponseEnabled:= True;
      Invalidate;
    end;

  if Key = 32 { SPACE }then
    if FShowStarter then
      begin
        FShowStarter := False;
        FStarterLatency := TickCount;
        LogEvent('*R');
        Invalidate;
        StartTrial(Sender);
        if Assigned(OnTrialStart) then OnTrialStart(Sender);
        Exit;
      end;

  if Assigned(OnTrialKeyDown) and FResponseEnabled then OnTrialKeyUp(Sender,Key,Shift);
end;

procedure TTrial.EndTrial(Sender: TObject);
begin
  {$ifdef DEBUG}
    DebugLn(mt_Debug + 'TTrial.EndTrial1');
  {$endif}
  if FLimitedHold = 0 then
    if Assigned(FClockThread) then
      RTLeventSetEvent(FClockThread.RTLEvent);
end;

procedure TTrial.EndTrialThread(Sender: TObject);
begin
  {$ifdef DEBUG}
    DebugLn(mt_Debug + 'TTrial.EndTrial2');
  {$endif}
  Hide;
  if Assigned(OnBeforeEndTrial) then OnBeforeEndTrial(Sender);
  if Assigned(OnEndTrial) then OnEndTrial(Sender);
end;

procedure TTrial.Paint;
begin
  inherited Paint;
  if FShowStarter then
    begin
      DrawCenteredCircle(Canvas, Width, Height, 6);
      Exit;
    end;
  if Assigned(OnTrialPaint) then OnTrialPaint;
end;

constructor TTrial.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnBeforeEndTrial := nil;
  OnEndTrial := nil;
  OnKeyUp := @TrialKeyUp;
  OnKeyDown := @TrialKeyDown;

  //FLimitedhold is controlled by a TTrial descendent. It controls Trial ending.
  FLimitedHold := 0;
  FClockThread := TClockThread.Create(True);
  FClockThread.OnTimer := @EndTrialThread;
  AddToClockList(@FClockThread.Start);

  HeaderTimestamps := 'Time' + #9 +
                      'Bloc__Id' + #9 +
                      'Trial_ID' + #9 +
                      'Trial_No';
end;

destructor TTrial.Destroy;
begin
  {$ifdef DEBUG}
    DebugLn(mt_Debug + 'TTrial.Destroy:FNextTrial:' + FNextTrial);
  {$endif}

  if Assigned(FClockThread) then
    begin
      FClockThread.OnTimer := nil;
      FClockThread.Enabled := False;
      FClockThread.Terminate;
      FClockThread := nil;
    end;
  inherited Destroy;
end;

procedure TTrial.Play(ACorrection: Boolean);
begin
  // avoid responses while loading configurations
  FResponseEnabled := False;

  // full path for loading media files
  RootMedia:= FGlobalContainer.RootMedia;

  // what will be the next trial?
  NextTrial := CfgTrial.SList.Values[_NextTrial];

  // will the trial count as MISS, HIT or NONE?
  Result := 'NONE';

  // what will happen during the inter trial interval?
  IETConsequence := 'NONE';

  // is it a correction trial?
  if ACorrection then
    FIsCorrection := True
  else
    FIsCorrection := False;

  // Trial background color
  Color:= StrToIntDef(CfgTrial.SList.Values[_BkGnd], Parent.Color);

  // Trial will last LimitedHold ms if LimitedHold > 0
  FLimitedHold := StrToIntDef(CfgTrial.SList.Values[_LimitedHold], 0);
  FClockThread.Interval := FLimitedHold;

  // Present a dot at the screen center. A key response is required before trialstart
  FShowStarter := StrToBoolDef(CfgTrial.SList.Values[_ShowStarter], False);
  if FShowStarter then
    Header := 'Str.Lat.' + #9 + Header;

  // image of the mouse cursor
  if TestMode then Cursor:= 0
  else Cursor:= StrToIntDef(CfgTrial.SList.Values[_Cursor], 0);

  // Initialize randomness generator
  Randomize;
end;

procedure TTrial.AddToClockList(AClockStart: TThreadMethod);
begin
  SetLength(FClockList, Length(FClockList) + 1);
  FClockList[Length(FClockList) - 1] := AClockStart;
end;

procedure TTrial.BeginStarter;
begin
  FResponseEnabled:= True;
  Invalidate;
end;

procedure TTrial.StartClockList;
var
  i : integer;
begin
  for i := 0 to Length(FClockList) -1 do
    TThreadMethod(FClockList[i]);
  SetLength(FClockList, 0);
end;

procedure TTrial.StartTrial(Sender: TObject);
begin
  LogEvent('TS');
  StartClockList;
  Invalidate;
  FResponseEnabled := True;
  if Assigned(OnTrialStart) then OnTrialStart(Sender);
end;

procedure TTrial.LogEvent(ACode: string);
begin
  SaveTData(TimestampToStr(TickCount - TimeStart) + #9 +
           IntToStr(CounterManager.CurrentBlc+1) + #9 +
           IntToStr(CounterManager.CurrentTrial+1) + #9 +
           IntToStr(CounterManager.Trials+1) + #9 + // Current trial cycle
           ACode + LineEnding)
end;

procedure TTrial.AddToClockList(ASchedule: TSchMan);
begin
  if ASchedule.Loaded then
    AddToClockList(ASchedule.StartMethod)
  else
    raise Exception.Create(ExceptionNoScheduleFound);
end;


procedure TTrial.SetOnBeforeEndTrial(AValue: TNotifyEvent);
begin
  if FOnBeforeEndTrial = AValue then Exit;
  FOnBeforeEndTrial := AValue;
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

procedure TTrial.SetOnEndTrial(AValue: TNotifyEvent);
begin
  if FOnEndTrial = AValue then Exit;
  FOnEndTrial := AValue;
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

procedure TTrial.SetOnTrialPaint(AValue: TPaintEvent);
begin
  if FOnTrialPaint = AValue then Exit;
  FOnTrialPaint := AValue;
end;

procedure TTrial.SetOnWriteTrialData(AValue: TNotifyEvent);
begin
  if FOnWriteTrialData = AValue then Exit;
  FOnWriteTrialData := AValue;
end;

function TTrial.GetRootMedia: string;
begin
  Result := FGlobalContainer.RootMedia;
end;

function TTrial.GetTestMode: Boolean;
begin
  Result := FGlobalContainer.TestMode;
end;

function TTrial.GetTimeStart: Extended;
begin
  Result := FGlobalContainer.TimeStart;
end;

procedure TTrial.SetRootMedia(AValue: string);
begin
  if FGlobalContainer.RootMedia=AValue then Exit;
  FGlobalContainer.RootMedia:=AValue;
end;

procedure TTrial.SetTestMode(AValue: Boolean);
begin
  if FGlobalContainer.TestMode=AValue then Exit;
  FGlobalContainer.TestMode:=AValue;
end;


end.

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

  , config_session
  , zmq_client
  , countermanager
  , custom_timer
  ;

type

  { TTrial }

  TTrial = class(TCustomControl)
  private
    FCfgTrial: TCfgTrial;
    //FClientThread : TZMQThread;
    FClockThread : TClockThread;
    FCounterManager : TCounterManager;
    FData: string;
    FDataTicks: string;
    FFilename: string;
    FHeader: string;
    FHeaderTicks: string;
    FIETConsequence: string;
    FNextTrial: string;
    FResult: string;
    FRootMedia: string;
    FServerAddress: string;
    FTimeOut: Integer;
    FTimeStart: Extended;
    // events
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
    FOnWriteTrialData: TNotifyEvent;
    procedure EndTrialThread(Sender: TObject);
  strict protected
    FClockList : array of TThreadMethod;
    FLimitedHold : integer;
    FIscorrection : Boolean;
    {$ifdef DEBUG}
      procedure ClockStatus(msg : string);
    {$endif}
    // Log timestamped event:
    // TimestampF, TrialID, ACode
    procedure LogEvent(ACode: string);
    procedure EndTrial(Sender: TObject);
    procedure StartTrial(Sender: TObject); virtual;
    procedure WriteData(Sender: TObject); virtual; abstract;
    procedure BeforeEndTrial(Sender: TObject); virtual; abstract;
    //property Onclick;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
//    procedure DispenserPlusCall; virtual; abstract;
    procedure Play(TestMode: Boolean; Correction : Boolean); virtual; abstract;
    property CfgTrial: TCfgTrial read FCfgTrial write FCfgTrial;
    property CounterManager : TCounterManager read FCounterManager write FCounterManager;
    property Data: string read FData write FData;
    property DataTicks: string read FDataTicks write FDataTicks;
    property FileName : string read FFilename write FFilename;
    property Header: string read FHeader write FHeader;
    property HeaderTicks: string read FHeaderTicks write FHeaderTicks;
    property IETConsequence : string read FIETConsequence write FIETConsequence;
    property NextTrial: string read FNextTrial write FNextTrial;
    property Result: string read FResult write FResult;
    property RootMedia : string read FRootMedia write FRootMedia;
    property ServerAddress : string read FServerAddress write FServerAddress;
    property TimeOut : Integer read FTimeOut write FTimeOut;
    property TimeStart : Extended read FTimeStart write FTimeStart;
  public
    property OnBeforeEndTrial: TNotifyEvent read FOnBeforeEndTrial write FOnBeforeEndTrial;
    property OnBeginCorrection : TNotifyEvent read FOnBeginCorrection write FOnBeginCorrection;
    property OnBkGndResponse: TNotifyEvent read FOnBkGndResponse write FOnBkGndResponse;
    property OnConsequence: TNotifyEvent read FOnConsequence write FOnConsequence;
    property OnEndCorrection : TNotifyEvent read FOnEndCorrection write FOnEndCorrection;
    property OnEndTrial: TNotifyEvent read FOnEndTrial write FOnEndTrial;
    property OnHit: TNotifyEvent read FOnHit write FOnHit;
    property OnMiss: TNotifyEvent read FOnMiss write FOnMiss;
    property OnNone: TNotifyEvent read FOnNone write FOnNone;
    property OnStmResponse: TNotifyEvent read FOnStmResponse write FOnStmResponse;
    property OnWriteTrialData: TNotifyEvent read FOnWriteTrialData write FOnWriteTrialData;

  end;

implementation


uses timestamps
    , timestamps_logger
    {$ifdef DEBUG}
    , debug_logger
    {$endif}
    ;


{ TTrial }

procedure TTrial.LogEvent(ACode: string);  // logger
var Event : string;
begin
  Event := IntToStr(FCfgTrial.Id) + #9 + ACode;
  {$ifdef DEBUG}
    DebugLn(Event);
  {$else}
    TimestampLn(Event);
  {$endif}
end;

{$ifdef DEBUG}
procedure TTrial.ClockStatus(msg: string);
begin
  DebugLn(msg);
end;
{$endif}

{
  FLimitedhold is controlled by a TTrial descendent. It controls Trial ending.
}
procedure TTrial.StartTrial(Sender: TObject);
var
  i : integer;
begin
  FClockThread.Interval := FLimitedHold;
  FClockThread.OnTimer := @EndTrialThread;
  {$ifdef DEBUG}
    FClockThread.OnDebugStatus := @ClockStatus;
  {$endif};

  SetLength(FClockList, Length(FClockList) + 1);
  FClockList[Length(FClockList) - 1] := @FClockThread.Start;

  // TClockThread.Start
  for i := 0 to Length(FClockList) -1 do
    TThreadMethod(FClockList[i]);

  SetLength(FClockList, 0);
  LogEvent('TS');
end;

procedure TTrial.EndTrial(Sender: TObject);
begin
  {$ifdef DEBUG}
    DebugLn(mt_Debug + 'TTrial.EndTrial1');
  {$endif}
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

constructor TTrial.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnBeforeEndTrial := nil;
  OnEndTrial := nil;

  FLimitedHold := 0;
  FClockThread := TClockThread.Create(True);
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



end.

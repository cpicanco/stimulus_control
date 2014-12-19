unit custom_timer;

{$mode objfpc}{$H+}

//{$MODE Delphi}

interface

uses LCLIntf, LCLType, LMessages, Controls, Classes, SysUtils;
type

  { TClockThread }

  TClockThread = class(TThread)
  private
    FTickEvent: PRTLEvent;  //old THandle
    FInterval: longint;     //old cardinal
    FOnTimer:TNotifyEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure FinishThreadExecution;
    procedure Clock;
    property Interval : longint read FInterval write FInterval;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;


implementation

{ TClockThread }

constructor TClockThread.Create(CreateSuspended: Boolean);
begin
  FreeOnTerminate := True;
  FTickEvent := RTLEventCreate; //BasicEventCreate
  FInterval := 100;

  inherited Create(CreateSuspended);
end;

destructor TClockThread.Destroy;
begin
  RTLEventDestroy(FTickEvent); //BasicEventDestroy
  inherited;
end;

procedure TClockThread.Execute;
begin
  while (not Terminated) do
  begin
      Synchronize(@Clock);
      RTLeventWaitFor(FTickEvent, Interval); //BasicEventWaitFor
    end;
end;

procedure TClockThread.FinishThreadExecution;
begin
  Terminate;
  RTLeventSetEvent(FTickEvent);  //BasicSetEvent
end;

procedure TClockThread.Clock;
begin
  if Assigned(OnTimer) then Ontimer(Self);
end;

end.

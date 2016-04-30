//
// Validation Project (PCRF) - Stimulus Control App
// Copyright (C) 2014-2016,  Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.
//
// cpicanco@ufpa.br
//
// This file is part of Validation Project (PCRF).
//
// Validation Project (PCRF) is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Validation Project (PCRF) is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Validation Project (PCRF).  If not, see <http://www.gnu.org/licenses/>.
//
unit custom_timer;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils

    {$ifdef DEBUG}
    , debug_logger
    {$endif}
    ;

type

  { TShowStatusEvent }

  TShowStatusEvent = procedure(Status: String) of object;

  { TClockThread }

  TClockThread = class(TThread)
  private
    FMustReset,
    FEnabled: Boolean;
    FHost: TObject;
    FRTLEvent: PRTLEvent;
    FInterval: longint;
    FOnTimer:TNotifyEvent;
    {$ifdef DEBUG}
      FOnDebugStatus:TShowStatusEvent;
      FDebugStatus : string;
      procedure DebugStatus;
    {$endif}
    procedure Clock;
    procedure SetEnabled(AValue: Boolean);
    procedure SetInterval(AValue: longint);
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure Reset;
    property Enabled : Boolean read FEnabled write SetEnabled;
    property Host: TObject read FHost write FHost;
    property Interval : longint read FInterval write SetInterval;
    {$ifdef DEBUG}
      property OnDebugStatus : TShowStatusEvent read FOnDebugStatus write FOnDebugStatus;
    {$endif}
    property OnTimer : TNotifyEvent read FOnTimer write FOnTimer;
    property RTLEvent : PRTLEvent read FRTLEvent;
  end;


implementation

{ TClockThread }

constructor TClockThread.Create(CreateSuspended: Boolean);
begin
  {$ifdef DEBUG}
    DebugLn(mt_Debug + 'TClockThread.Create at:' + TimeToStr(Time) + ':' + IntToStr(GetTickCount64));
  {$endif}
  FreeOnTerminate := True;
  FInterval := 1000;
  FEnabled := True;
  FMustReset := False;
  FRTLEvent := RTLEventCreate;
  inherited Create(CreateSuspended);
end;

destructor TClockThread.Destroy;
begin
  {$ifdef DEBUG}
    FDebugStatus := mt_Debug + 'TClockThread.Destroy Interval/ThreadId:' + IntToStr(Interval) + '/' + IntToStr(ThreadID);
    DebugStatus;
  {$endif}
  RTLEventDestroy(FRTLEvent);
  inherited Destroy;
end;

procedure TClockThread.Reset;
begin
  if not FMustReset then
  begin
    FMustReset := True;
    RTLeventSetEvent(FRTLEvent);
  end
end;

{$ifdef DEBUG}
procedure TClockThread.DebugStatus;
begin
  if Assigned(FOnDebugStatus) then FOnDebugStatus(FDebugStatus);
end;
{$endif}

procedure TClockThread.Clock;
begin
  {$ifdef DEBUG}
    FDebugStatus := mt_Debug + 'TClockThread.Execute:SyncHead ' + IntToStr(ThreadID);
    DebugStatus;
  {$endif}

  if not Enabled then Exit;
  if FMustReset then
    begin
      FMustReset := False;
      Exit;
    end;
  if Assigned(FOnTimer) then FOnTimer(Self);

  {$ifdef DEBUG}
    FDebugStatus := mt_Debug + 'TClockThread.Execute:Fired ' + IntToStr(ThreadID);
    DebugStatus;
  {$endif}
end;

procedure TClockThread.SetEnabled(AValue: Boolean);
begin
  {$ifdef DEBUG}
    FDebugStatus := mt_Debug + 'TClockThread.SetEnabled:' + BoolToStr(AValue, 'True', 'False') + #32 + IntToStr(ThreadID);
    DebugStatus;
  {$endif}
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
  RTLeventSetEvent(RTLEvent);
end;

procedure TClockThread.SetInterval(AValue: longint);
begin
  {$ifdef DEBUG}
    FDebugStatus := mt_Debug + 'TClockThread.SetInterval:' + IntToStr(AValue) + #32 + IntToStr(ThreadID);
    DebugStatus;
  {$endif}
  if FInterval=AValue then Exit;
  FInterval:=AValue;
end;

procedure TClockThread.Execute;
begin
  {$ifdef DEBUG}
    FDebugStatus := mt_Debug + 'TClockThread.Execute:Start ' +  IntToStr(ThreadID);
    Synchronize(@DebugStatus);
  {$endif}

  while not Terminated do
    if Enabled then
      begin
        RTLeventWaitFor(FRTLEvent, Interval);
        Synchronize(@Clock);
      end
    else
      begin
        RTLeventWaitFor(FRTLEvent);
        {$ifdef DEBUG}
          FDebugStatus := mt_Debug + 'TClockThread.Execute:Wait ' + IntToStr(ThreadID);
          Synchronize(@DebugStatus);
        {$endif}
      end;

  {$ifdef DEBUG}
    FDebugStatus := mt_Debug + 'TClockThread.Execute:Terminated ' + IntToStr(ThreadID);
    Synchronize(@DebugStatus);
  {$endif}
end;

end.

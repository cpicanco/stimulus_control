//
// Validation Project (PCRF) - Stimulus Control App
// Copyright (C) 2014-2015,  Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.
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

uses LCLIntf, LCLType, Controls, Classes, SysUtils;

type

  { TClockThread }

  TClockThread = class(TThread)
  private
    FMustReset,
    FEnabled: Boolean;
    FHost: TObject;
    FRTLEvent: PRTLEvent;
    FInterval: longint;
    FOnTimer:TNotifyEvent;
    procedure Clock;
    procedure SetEnabled(AValue: Boolean);
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure Reset;
    property Enabled : Boolean read FEnabled write SetEnabled;
    property Host: TObject read FHost write FHost;
    property Interval : longint read FInterval write FInterval;
    property OnTimer : TNotifyEvent read FOnTimer write FOnTimer;
    property RTLEvent : PRTLEvent read FRTLEvent;
  end;


implementation

{$ifdef DEBUG}
  uses debug_logger;
{$endif}

{ TClockThread }

constructor TClockThread.Create(CreateSuspended: Boolean);
begin
  {$ifdef DEBUG}
    DebugLn(mt_Debug + 'TClockThread.Create at:' + TimeToStr(Time) + TimeSeparator + IntToStr(GetTickCount));
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
    DebugLn(mt_Debug + 'TClockThread.Destroy:' + IntToStr(ThreadID));
    DebugLn(mt_Debug + 'Thread.Interval:' + IntToStr(Interval));
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

procedure TClockThread.Clock;
begin
  if not Enabled then Exit;
  if FMustReset then
    begin
      FMustReset := False;
      Exit;
    end;
  if Assigned(FOnTimer) then FOnTimer(Self);
end;

procedure TClockThread.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
  RTLeventSetEvent(RTLEvent);
end;

procedure TClockThread.Execute;
begin
  while not Terminated do
    if Enabled then
      begin
        RTLeventWaitFor(FRTLEvent, Interval);
        Synchronize(@Clock);
      end
    else RTLeventWaitFor(FRTLEvent);
end;

end.

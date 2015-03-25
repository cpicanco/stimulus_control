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

//{$MODE Delphi}

interface

uses LCLIntf, LCLType, LMessages, Controls, Classes, SysUtils;
type

  { TClockThread }

  TClockThread = class(TThread)
  private
    FRunning: Boolean;
    FTickEvent: PRTLEvent;  //old THandle
    FInterval: longint;     //old cardinal
    FOnTimer:TNotifyEvent;
    procedure Clock;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;

    property Interval : longint read FInterval write FInterval;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    property Running: Boolean read FRunning write FRunning default True;
  end;


implementation

{ TClockThread }

constructor TClockThread.Create(CreateSuspended: Boolean);
begin
  FreeOnTerminate := True;
  FTickEvent := RTLEventCreate; //BasicEventCreate
  FInterval := 100;
  //RTLeventSetEvent(FTickEvent);  //BasicSetEvent for synchronize threads
  Running := True;
  inherited Create(CreateSuspended);
end;

destructor TClockThread.Destroy;
begin
  RTLEventDestroy(FTickEvent); //BasicEventDestroy
  inherited Destroy;
end;

procedure TClockThread.Clock;
begin
  if Assigned(FOnTimer) then FOnTimer(Self);
end;

procedure TClockThread.Execute;
begin
  while (not Terminated) and Running do
  begin
    Synchronize(@Clock);
    RTLeventWaitFor(FTickEvent, Interval); //BasicEventWaitFor
  end;
end;

end.

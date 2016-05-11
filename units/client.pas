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
unit client;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Process

  , zmqapi
  ;

type

  {
    http://wiki.freepascal.org/Multithreaded_Application_Tutorial
  }

  TShowStatusEvent = procedure(Status: String) of object;

  { TClientThread }

  TClientThread = class(TThread)
  private
    FMsg,
    FTrialIndex,
    FRequest,
    FCode : UTF8string;
    FContext : TZMQContext;
    FRequester : TZMQSocket;

    FRTLEvent: PRTLEvent;
    FOnShowStatus: TShowStatusEvent;
    procedure ShowStatus;
  protected
    procedure Execute; override;
  public
    constructor Create(AHost : string; CreateSuspended: Boolean = True);
    destructor Destroy; override;
    procedure SendRequest(ACode : UTF8string; ATrialIndex : integer; ARequest : UTF8string = 'T');
    property OnShowStatus: TShowStatusEvent read FOnShowStatus write FOnShowStatus;
  end;

implementation

{$ifdef DEBUG}
  uses debug_logger;
{$endif}

constructor TClientThread.Create(AHost: string; CreateSuspended: Boolean);
begin
  FreeOnTerminate := True;

  FRTLEvent := RTLEventCreate;

  FContext := TZMQContext.Create;
  FRequester := FContext.Socket( stReq );
  FRequester.connect( 'tcp://' + AHost );
  inherited Create(CreateSuspended);
end;

destructor TClientThread.Destroy;
begin
  FRequester.Free;
  FContext.Free;
  RTLEventDestroy(FRTLEvent);
  inherited Destroy;
end;

procedure TClientThread.SendRequest(ACode: UTF8string; ATrialIndex: integer;
  ARequest: UTF8string);
begin
  FRequest := ARequest;
  FCode := ACode;
  FTrialIndex := IntToStr(ATrialIndex);

  RTLeventSetEvent(FRTLEvent);
end;


procedure TClientThread.ShowStatus;
// this method is executed by the mainthread and can therefore access all GUI elements.
begin
  if Assigned(FOnShowStatus) then FOnShowStatus(FMsg);
end;


procedure TClientThread.Execute;
var
  ARequest,ACode,ATrialIndex,
  AMessage : UTF8String;
begin
  while not Terminated do
    begin
      AMessage := '';
      RTLeventWaitFor(FRTLEvent);

      // make a local copy to synchronize shared variables
      ARequest := FRequest;
      ACode := FCode;
      ATrialIndex := FTrialIndex;

      FRequester.send( ARequest );
      FRequester.recv( AMessage );

      // trial timestamp event
      FMsg := ATrialIndex + #9 + AMessage + #9 + ACode;
      Synchronize( @Showstatus );

      {$ifdef DEBUG}
        FMsg := mt_Debug + 'TClientThread instance with ThreadID:' + IntToStr(Self.ThreadID);
        Synchronize( @Showstatus );
      {$endif}

    end;
end;

end.

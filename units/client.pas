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
    FServerAddress,
    FMsg,
    FTrialIndex,
    FRequest,
    FCode : string;
    FContext : TZMQContext;
    FRequester : TZMQSocket;
    FRTLEvent: PRTLEvent;

    FOnShowStatus: TShowStatusEvent;

    procedure SetServerAddress(AValue: string);
    procedure ShowStatus;
  protected
    procedure Execute; override;
  public
    constructor Create(AHost : string; CreateSuspended: Boolean = True);
    destructor Destroy; override;
    procedure SendRequest(ACode : string; ATrialIndex : integer; ARequest : string = 'T');
    property OnShowStatus: TShowStatusEvent read FOnShowStatus write FOnShowStatus;
    property ServerAddress : string read FServerAddress write SetServerAddress;

  end;

implementation

{$ifdef DEBUG}
  uses debug_logger;
{$endif}

constructor TClientThread.Create(AHost: string; CreateSuspended: Boolean);
begin
  FreeOnTerminate := True;

  FTrialIndex := '-';
  FCode := '-:-';
  FRequest := '-';
  FRTLEvent := RTLEventCreate;
  FServerAddress := AHost;
  FContext := TZMQContext.Create;
  FRequester := FContext.Socket( stReq );
  FRequester.connect( 'tcp://' + FServerAddress );
  inherited Create(CreateSuspended);
end;

destructor TClientThread.Destroy;
begin
  FRequester.Free;
  FContext.Free;
  RTLEventDestroy(FRTLEvent);

  inherited Destroy;
end;

procedure TClientThread.SendRequest(ACode: string; ATrialIndex: integer;
  ARequest: string);
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

procedure TClientThread.SetServerAddress(AValue: string);
begin
  if FServerAddress = AValue then Exit;
  if Length(AValue) > 0 then FServerAddress := AValue;
end;


procedure TClientThread.Execute;
var
  AMessage : UTF8String;
begin
  while not Terminated do
    begin
      AMessage := '';
      RTLeventWaitFor(FRTLEvent);
      FRequester.send( FRequest );
      FRequester.recv( AMessage );

      // ('trial', 'timestamp', 'event')
      FMsg := #40#39 + FTrialIndex + #39#44#32#39 + AMessage + #39#44#32#39 + FCode + #39#41;
      Synchronize( @Showstatus );

      {$ifdef DEBUG}
        FMsg := mt_Debug + 'TClientThread instance with ThreadID:' + IntToStr(Self.ThreadID);
        Synchronize( @Showstatus );
      {$endif}

    end;
end;

//function TClientThread.GetTimestampFromMessage(aMessage: Utf8String
//  ): Utf8String;
//
//  var aKey, aHeader : Utf8String;
//begin
//  if Pos('timestamp', aMessage) <> 0 then
//  begin
//	  aHeader := 'Pupil';
//	  aKey := 'timestamp:';
//	  Delete(  aMessage, Pos(aHeader, aMessage), Length(aHeader)  );
//	  Delete(  aMessage, Pos(aKey, aMessage), Length(aKey)  );
//
//	  while Pos(#10, aMessage) <> 0 do
//		  Delete(  aMessage, Pos(#10, aMessage), Length(#10)  );
//  end;
//	Result := aMessage;
//
//end;

end.

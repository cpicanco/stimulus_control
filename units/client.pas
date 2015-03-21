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
unit client;

{$mode objfpc}{$H+}

interface

uses
   Classes
 , SysUtils
 , Process
 , FileUtil
 , zmqapi
 ;

type

  TShowStatusEvent = procedure(Status: String) of object;

  { TClientThread }

  TClientThread = class(TThread)
  private
    FAddress: string;
    FContext : TZMQContext;
	  FSubscriber : TZMQSocket;
    FMsg : string;
    FTrialIndex : string;
    FCode : string;
    FTimestampsPath : string;
    FOnShowStatus: TShowStatusEvent;
    FTimestampsFile : TextFile;
    function GetTimestampFromMessage(aMessage : Utf8String) : Utf8String;
    procedure SetAddress(AValue: string);
    procedure ShowStatus;
  protected
    procedure Execute; override;
  public
    constructor Create(TrialIndex : integer; Code, OutputPath : string; CreateSuspended : boolean = True);
    destructor Destroy; override;
    property OnShowStatus: TShowStatusEvent read FOnShowStatus write FOnShowStatus;
    property ServerAddress : string read FAddress write SetAddress;
  end;

implementation

constructor TClientThread.Create(TrialIndex : integer; Code, OutputPath : string; CreateSuspended : boolean = True);
begin
  FreeOnTerminate := True;

  FTrialIndex := IntToStr(TrialIndex);
  FCode := Code;
  FTimestampsPath := OutputPath;
  FAddress := '127.0.0.1:5000';

  ForceDirectoriesUTF8(ExtractFilePath(FTimestampsPath));
	AssignFile(FTimestampsFile, FTimestampsPath);
	if FileExistsUTF8(FTimestampsPath) then
	  Append(FTimestampsFile)
	else Rewrite(FTimestampsFile);

  inherited Create(CreateSuspended);
end;

destructor TClientThread.Destroy;
begin
  inherited Destroy;
end;

procedure TClientThread.ShowStatus;
// this method is executed by the mainthread and can therefore access all GUI elements.
begin
  if Assigned(FOnShowStatus) then
  begin
    FOnShowStatus(FMsg);
  end;
end;

function TClientThread.GetTimestampFromMessage(aMessage: Utf8String
  ): Utf8String;

  var aKey, aHeader : Utf8String;
begin
	aHeader := 'Pupil';
	aKey := 'timestamp:';
	Delete(  aMessage, Pos(aHeader, aMessage), Length(aHeader)  );
	Delete(  aMessage, Pos(aKey, aMessage), Length(aKey)  );

	while Pos(#10, aMessage) <> 0 do
		Delete(  aMessage, Pos(#10, aMessage), Length(#10)  );

	Result := aMessage;

end;

procedure TClientThread.SetAddress(AValue: string);
begin
  if FAddress = AValue then Exit;

  if Length(AValue) > 0 then FAddress := AValue;
end;


procedure TClientThread.Execute;
var
  data : string;
  message : UTF8String;
begin
  try
    FContext := TZMQContext.Create;
	  FSubscriber := FContext.Socket( stSub );
    FSubscriber.connect( 'tcp://' + FAddress );
    FSubscriber.subscribe( '' );

    FSubscriber.recv( message );

    data := '(' + FTrialIndex + ',' + GetTimestampFromMessage(message) + ',' + FCode + ')';

	  Writeln(FTimestampsFile, data);


    FMsg := data + #10#10 +
            '"' + FTimestampsPath + '"'  + #10 +
            '"' + FAddress + '"';

    Synchronize( @Showstatus );

  finally

    CloseFile(FTimestampsFile);
    FSubscriber.Free;
	  FContext.Free;
    Terminate;
  end;
end;

end.

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
program zmq_subscriber;

uses SysUtils
	, Classes
	, FileUtil
	, zmqapi
	;

function GetTimestampFromMessage(aMessage : Utf8String) : Utf8String;
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

const
	UPDATE_COUNT = 5000;
	address = '10.42.0.1:5020';
var
	context : TZMQContext;
	subscriber : TZMQSocket;
	aBegin : TTimeStamp;
	major, minor, patch, i : integer;

	message : Utf8String;
	FTimestampsPath : string;
	FTimestampsFile : TextFile;

begin
	ZmqVersion(major, minor, patch);
	Writeln( 'zmq_version():' + IntToStr(major) + '.' + IntToStr(minor) + '.' + IntToStr(patch) );
	// Socket to talk to server
	
	context := TZMQContext.Create;
	Writeln( 'creating generic context');

	subscriber := Context.Socket( stSub );
	Writeln( 'assigning context stSub' );
	
	subscriber.connect( 'tcp://' + address );
	Writeln( 'connecting to:  tcp://' + address  );
	
	subscriber.subscribe( '' );
	Writeln('subscribing with empty filter');

	FTimestampsPath := '/home/rafael/git/validation_project/Participante1/Data/timestamps_teste';

	ForceDirectoriesUTF8(ExtractFilePath(FTimestampsPath));
	AssignFile(FTimestampsFile, FTimestampsPath);
	if FileExistsUTF8(FTimestampsPath) then
		Append(FTimestampsFile)
	else Rewrite(FTimestampsFile);
    
    aBegin := DateTimeToTimeStamp(Now);
	try
		for i := 0 to UPDATE_COUNT - 1 do
			begin
				subscriber.recv( message );
				Writeln('receiving messsage');
				message := GetTimestampFromMessage(message);
				Writeln(FTimestampsFile, message);
				Writeln( message + #32 + IntToStr(DateTimeToTimeStamp(Now).Time - aBegin.Time));
			end;
	finally
		CloseFile(FTimestampsFile);
		subscriber.Free;
		context.Free;		
	end;
end.

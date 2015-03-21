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

var
	context : TZMQContext;
	subscriber : TZMQSocket;
	aBegin : TTimeStamp;
	i : integer;

	message : Utf8String;
	FTimestampsPath, address, filter : string;
	FTimestampsFile : TextFile;

begin

	// Socket to talk to server
	address := '127.0.0.1:5020';
	Writeln ( 'Connecting to Pupil server...' + #32#40 + 'address:' + #32 + address + #32#41);
	context := TZMQContext.Create;
	subscriber := Context.Socket( stSub );
	subscriber.connect( 'tcp://' + address );
	subscriber.subscribe( '' );

	FTimestampsPath := '/home/rafael/git/validation_project/Participante1/Data/Data_001.timestamp';

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
				message := GetTimestampFromMessage(message);
				Writeln(FTimestampsFile, message);
				Writeln( message + #32 + IntToStr(DateTimeToTimeStamp(Now).Time - aBegin.Time));
			end;
	finally
		CloseFile(FTimestampsFile);
		subscriber.Free;
		context.Free;
		Writeln ('End')		
	end;
end.
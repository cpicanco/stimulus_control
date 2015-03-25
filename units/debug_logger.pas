unit debug_logger;

{$mode objfpc}{$H+}

interface

uses regdata;

procedure DebugLn(msg : string);

var
  Logger : TRegData;

const
    // message types
    mt_Debug : string = '[debug]' + #32;
    mt_Exception : string = '[except]' + #32;
    mt_Information : string = '[information]' + #32;
    mt_Warning : string = '[warning]' + #32;

implementation

uses FileUtil, SysUtils;

procedure DebugLn(msg: string);
begin
  if TextRec(Logger.DataFile).Mode = 55218 then
    begin
      WriteLn(Logger.DataFile, msg);
      Logger.CloseFFile;
    end
  else
    begin
      Logger.AssignFFile;
      Logger.AppendF;
      WriteLn(Logger.DataFile, msg);
      Logger.CloseFFile;
    end;
end;

initialization
begin
  Logger := TRegData.Create(nil, GetCurrentDirUTF8 + '/_Log_001.txt');
end

finalization
begin
  Logger.Free;
end

end.


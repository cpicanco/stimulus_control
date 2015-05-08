unit debug_logger;

{$mode objfpc}{$H+}

interface

uses LCLProc, regdata;

procedure DebugLn(msg : string);

procedure DebuglnThreadLog(const Msg: string);

var
  Logger : TRegData;

const
    // message types
    mt_Debug : string = '[debug]' + #32;
    mt_Exception : string = '[except]' + #32;
    mt_Information : string = '[information]' + #32;
    mt_Warning : string = '[warning]' + #32;

implementation

uses LCLIntf, FileUtil, SysUtils;

var
  FileInfo : TSearchRec;

procedure DebugLn(msg: string);
begin
  DebuglnThreadLog(msg);
end;

procedure DebuglnThreadLog(const Msg: string);
var
  PID: PtrInt;
begin
  PID:=PtrInt(GetThreadID);
  DbgOutThreadLog(IntToStr(GetTickCount) + ' : ' + IntToStr(PtrInt(PID)) + ' : ' + Msg + LineEnding);
end;

initialization
begin
  Logger := TRegData.Create(nil, GetCurrentDirUTF8 + PathDelim + '_Log_001.txt');
end

finalization
begin
  if FindFirst('Log*',faAnyFile, FileInfo) = 0 then
    begin
      repeat
        with FileInfo do
          begin
            if TextRec(Logger.DataFile).Mode <> 55218 then
              begin
                Logger.AssignFFile;
                Logger.AppendF;
              end;
            WriteLn(Logger.DataFile, Name);
            WriteLn(Logger.DataFile, ReadFileToString(Name));
            DeleteFile(Name);
          end;
          WriteLn(Logger.DataFile, #10#10);
      until FindNext(FileInfo) <> 0;
      Logger.CloseFFile;
    end;
  FindClose(FileInfo);
  Logger.Free;
end

end.


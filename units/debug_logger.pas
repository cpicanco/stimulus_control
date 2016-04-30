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

uses LCLIntf, FileUtil, LazFileUtils, SysUtils;

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
  DbgOutThreadLog(IntToStr(GetTickCount64) + ' : ' + IntToStr(PtrInt(PID)) + ' : ' + Msg + LineEnding);
end;

initialization
begin
  Logger := TRegData.Create(nil, GetCurrentDir + PathDelim + '_Log_001.txt');
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


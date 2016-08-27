{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
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

uses LCLIntf, FileUtil, LazFileUtils, SysUtils, timestamp;

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
  DbgOutThreadLog(FloatToStrF(GetCustomTick, ffFixed,0,9) + ' : ' + IntToStr(PtrInt(PID)) + ' : ' + Msg + LineEnding);
end;

initialization
begin
  Logger := TRegData.Create(nil, GetCurrentDir + PathDelim + 'debug_log' + PathDelim + '_Log_001.txt');
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
          WriteLn(Logger.DataFile, LineEnding);
      until FindNext(FileInfo) <> 0;
      Logger.CloseFFile;
    end;
  FindClose(FileInfo);
  Logger.Free;
end

end.


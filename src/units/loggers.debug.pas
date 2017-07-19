{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Loggers.Debug;

{$mode objfpc}{$H+}

interface

uses LCLProc
     , Loggers.DataFile
     ;

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

uses LCLIntf, FileUtil, LazFileUtils, SysUtils
     {$ifdef DEBUG}
     , Loggers.DebugUtils
     {$endif}
     ;

{$ifdef DEBUG}
var
  FileInfo : TSearchRec;
{$endif}

procedure DebugLn(msg: string);
begin
  {$ifdef DEBUG}
    WriteLn(msg);
    DebuglnThreadLog(msg);
  {$endif}
end;


initialization
begin
  {$ifdef DEBUG}
    Logger := TRegData.Create(nil, GetCurrentDir + PathDelim + 'debug_log' + PathDelim + '000.log');
  {$endif}
end

finalization
begin
  {$ifdef DEBUG}
  if FindFirst('Log*',faAnyFile, FileInfo) = 0 then
    begin
      repeat
        with FileInfo do
          begin
            if not Logger.IsOpened then
              begin
                Logger.AssignFile;
                Logger.AppendFile;
              end;
            Logger.SaveLine(Name);
            Logger.SaveLine(ReadFileToString(Name));
            DeleteFile(Name);
          end;
          Logger.SaveLine(LineEnding);
      until FindNext(FileInfo) <> 0;
      Logger.Free;
    end;
  FindClose(FileInfo);
  {$endif}
end

end.


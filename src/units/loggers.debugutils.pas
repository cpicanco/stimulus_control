{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Loggers.DebugUtils;

{$mode objfpc}{$H+}

interface

uses SysUtils;

procedure DebuglnThreadLog(const Msg: string);

implementation

uses LCLProc
     , timestamps
     ;

procedure DebuglnThreadLog(const Msg: string);
var
  PID: PtrInt;
begin
  PID:=PtrInt(GetThreadID);
  DbgOutThreadLog(FloatToStrF(TickCount, ffFixed,0,9) + ' : ' + IntToStr(PtrInt(PID)) + ' : ' + Msg + LineEnding);
end;

end.


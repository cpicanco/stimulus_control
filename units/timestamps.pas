{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit timestamps;

{$mode objfpc}{$H+}

interface

uses  SysUtils;

function TimestampToStr(ATimestamp: Extended) : string;
function GetTimeStampF : string; overload;
function GetTimeStampF (ATimeStart:Extended): string; overload;
function TickCount : Extended;
procedure TimestampLn(msg: string);


implementation

uses timestamps_helpers;

function TimestampToStr(ATimestamp: Extended): string;
begin
  Result:=FloatToStrF(ATimestamp,ffFixed,0,9)
end;

function GetTimeStampF: string;
begin
  Result:=FloatToStrF(GetCustomTick,ffFixed,0,9)
end;

function GetTimeStampF(ATimeStart: Extended): string;
begin
  Result:=FloatToStrF(GetCustomTick-ATimeStart,ffFixed,0,9)
end;

function TickCount: Extended;
begin
  Result := GetCustomTick;
end;

procedure TimestampLn(msg: string);
begin
  TimestampLogger.SaveData(GetTimeStampF + #9 + msg + LineEnding);
end;


end.


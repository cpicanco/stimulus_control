{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Timestamps;

{$mode objfpc}{$H+}

interface

uses  SysUtils;

type
  TLogEvent = function (ACode: string): Extended of object;

procedure StartTimestamp(ATimestamp:Extended);

function GetTimeStampF : string; overload;
function GetTimeStampF(ATimestamp:Extended): string; overload;
function TickCount : Extended;
function TimestampToStr(ATimestamp: Extended) : string;
function TimestampToStrDelta(ATimestamp: Extended) : string;

implementation

uses Timestamps.Helpers;

var
  FirstTimestamp : Extended = 0;

procedure StartTimestamp(ATimestamp : Extended);
begin
  FirstTimestamp := ATimestamp;
end;

function GetTimeStampF: string;
begin
  Result:=FloatToStrF(GetCustomTick,ffFixed,0,9)
end;

function GetTimeStampF(ATimestamp: Extended): string;
begin
  Result:=FloatToStrF(GetCustomTick-ATimestamp,ffFixed,0,9)
end;

function TickCount: Extended;
begin
  Result := GetCustomTick;
end;
  
function TimestampToStr(ATimestamp: Extended): string;
begin
  Result:=FloatToStrF(ATimestamp,ffFixed,0,9)
end;

function TimestampToStrDelta(ATimestamp : Extended) : string;
begin
  Result:=FloatToStrF(FirstTimestamp - ATimestamp,ffFixed,0,9)
end;


end.


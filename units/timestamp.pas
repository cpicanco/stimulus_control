{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}

// Timing
// https://github.com/graemeg/epiktimer/blob/master/epiktimer.pas

unit timestamp;

{$mode objfpc}{$H+}

interface

uses
  { TODO -oRafael -cCrossplatform : Implement clock_gettime() alternative for windows. }
  Linux, UnixType,
  SysUtils;

function GetCustomTick : Extended;
function GetTimeStampRaw : timespec;
function GetTimeStampMono : timespec;
function GetResolution : string;
function GetTimeStampF : string; overload;
function GetTimeStampF (ATimeStart:Extended): string; overload;

implementation



function GetCustomTick: Extended;
var
  tp: timespec;
  a, b : Extended;
begin
  clock_gettime(CLOCK_MONOTONIC, @tp);
  a := tp.tv_sec;
  b := tp.tv_nsec * 1e-9;
  Result := a+b;
  //FloatToStrF(Result, ffFixed, 0, 9);
end;

function GetTimeStampRaw: timespec;
var
  tp: timespec;
begin
  clock_gettime(CLOCK_MONOTONIC_RAW, @tp);
  Result := tp;
end;

function GetTimeStampMono: timespec;
var
  tp: timespec;
begin
  clock_gettime(CLOCK_MONOTONIC, @tp);
  Result := tp;
end;

function GetTimeStampF: string;
begin
  Result:=FloatToStrF(GetCustomTick,ffFixed,0,9)
end;

function GetTimeStampF(ATimeStart: Extended): string;
begin
  Result:=FloatToStrF(GetCustomTick-ATimeStart,ffFixed,0,9)
end;

function GetResolution: string;
var
  tp: timespec;
begin
  clock_getres(CLOCK_MONOTONIC, @tp);
  Result := IntToStr(tp.tv_sec) + '.' + FloatToStr(tp.tv_nsec * 1e-9);
end;

end.


{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Timestamps.Helpers;

{$mode objfpc}{$H+}

interface

{ DONE -oRafael -cCrossplatform : On windows, implement EpikTimer as alternative for clock_gettime. }

uses
  Classes, SysUtils
{$ifdef UNIX}
  , Linux
  , UnixType
{$endif}
{$ifdef WINDOWS}
  , epiktimer
{$endif}
  ;

  procedure StartEpiktimer;
  function GetCustomTick : Extended;
{$ifdef UNIX}
  function GetMonotonicTime : timespec;
  function GetMonotonicTimeRaw : timespec;
  function GetClockResolution : string; // granularity
{$endif}
implementation

{$ifdef WINDOWS}
var
  ET: TEpikTimer;

procedure StartEpiktimer;
begin
  ET.Start;
end;

function GetCustomTick: Extended;
begin
  Result := ET.GetSystemTicks * 1e-7;
end;
{$endif}

{$ifdef UNIX}
function GetCustomTick: Extended;
var
  tp: timespec;
  a, b : Extended;
begin
  clock_gettime(CLOCK_MONOTONIC, @tp);
  a := Extended(tp.tv_sec);
  b := Extended(tp.tv_nsec) * 1e-9;
  Result := a+b;
end;


function GetMonotonicTime: timespec;
var
  tp: timespec;
begin
  clock_gettime(CLOCK_MONOTONIC, @tp);
  Result := tp;
end;

function GetMonotonicTimeRaw: timespec;
var
  tp: timespec;
begin
  clock_gettime(CLOCK_MONOTONIC_RAW, @tp);
  Result := tp;
end;

function GetClockResolution: string;
var
  tp: timespec;
begin
  clock_getres(CLOCK_MONOTONIC, @tp);
  Result := IntToStr(tp.tv_sec) + '.' + FloatToStr(tp.tv_nsec * 1e-9);
end;
{$endif}

{$ifdef WINDOWS}
initialization
  ET := TEpikTimer.Create(nil);
  ET.Clear;

finalization
  ET.Free;
{$endif}
end.


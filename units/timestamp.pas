{
 Timing
 https://github.com/graemeg/epiktimer/blob/master/epiktimer.pas
}
unit timestamp;

{$mode objfpc}{$H+}

{$asmmode intel}

interface

uses
  Linux, UnixType, SysUtils;

function GetTimeStampRaw: timespec;
function GetTimeStampMono: timespec;
function GetResolution: string;

implementation

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

function GetResolution: string;
var
  tp: timespec;
begin
  clock_getres(CLOCK_MONOTONIC, @tp);
  Result := IntToStr(tp.tv_sec) + '.' + FloatToStr(tp.tv_nsec * 1e-9);
end;


end.


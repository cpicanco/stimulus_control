unit debug_logger_thread;

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


unit bass_player_callbacks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , BASS
  , background
  ;

// hello world callback
procedure EndOfFileCallBack(AHandle: HSYNC; Channel, Data: DWORD; User: Pointer);{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

implementation

{$IFDEF MSWINDOWS}
procedure EndOfFileCallBack(AHandle: HSYNC; Channel, Data: DWORD; User: Pointer); stdcall;
begin
  // todo
end;
{$ELSE}
procedure EndOfFileCallBack(AHandle: HSYNC; Channel, Data: DWORD; User: Pointer); cdecl;
begin
  FrmBackground.DrawMask := True;
end;
{$ENDIF}

end.


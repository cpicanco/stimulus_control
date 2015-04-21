//
// Validation Project (PCRF) - Stimulus Control App
// Copyright (C) 2014-2015,  Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.
//
// cpicanco@ufpa.br
//
// This file is part of Validation Project (PCRF).
//
// Validation Project (PCRF) is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Validation Project (PCRF) is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Validation Project (PCRF).  If not, see <http://www.gnu.org/licenses/>.
//
unit interface_plp;

{$mode objfpc}{$H+}

interface

// this unit was not implemented yet

uses
    Classes, SysUtils,

  {$IFDEF Linux}
    Ports,
  {$ENDIF}

  , custom_timer
  ;

type
{
  {$IFDEF WINDOWS}

  TInp32 = function(Address: ShortInt): BYTE; stdcall;
  TOut32 = procedure(Address: ShortInt; Data: BYTE); stdcall;

  {$ENDIF}
}

  { TPLP }

  TPLP = class (TComponent)
  private
    FTimer : TClockThread;
  {
  {$IFDEF WINDOWS}
    FInpout32: THandle;
    FInp32: TInp32;
    FOut32: TOut32;
  {$ENDIF}
  }
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    procedure OutPortOn (Data : BYTE);
    procedure OutPortOff (Sender: TObject);
    procedure OutPortNone (Sender: TObject);
    procedure OnTimerMethod(AutoOff: Boolean);

  end;

 {$IFDEF Linux}
 //program need to be executed under/have root privileges
 function ioperm(from: Cardinal; num: Cardinal; turn_on: Integer): Integer; cdecl; external 'libc';
 {$ENDIF}

 {$IFDEF WINDOWS}
 //function Inp32(EndPorta: ShortInt): BYTE stdcall; external 'inpout32';
 procedure Out32(EndPorta: ShortInt; Valor:BYTE); stdcall; external 'inpout32';
 {$ENDIF}

var FPLP : TPLP;

implementation

{TPLP}

constructor TPLP.Create(AOwner : TComponent);
begin
{
 {
 http://wiki.freepascal.org/Hardware_Access#Using_inpout32.dll_for_Windows
 }

{$IFDEF WINDOWS}
   FInpout32 := LoadLibrary('inpout32.dll');
   if (FInpout32 <> 0) then
   begin
     FInp32 := GetProcAddress(FInpout32, 'Inp32');
     if (@FInp32 = nil) then ShowMessage('Error Inp32');
     FOut32 := GetProcAddress(FInpout32, 'Out32');
     if (@FOut32 = nil) then ShowMessage('Error Out32');
   end
   else ShowMessage ('Error Inpout32');
{$ENDIF}
}

//$378 $278 $3BC
//Usual addresses of the PC Parallel Port
//In linux $3BC for /dev/lp0, $378 for /dev/lp1, and $278 for /dev/lp2
//For now, need to check the bios settings
//of the target machine before compile.

{$IFDEF Linux}
   if ioperm($378, 8, 1) = -1 then Showmessage('Error ioperm $378 on') else;
   //if ioperm($278, 8, 1) = -1 then Showmessage('Error ioperm $278') else;
   //if ioperm($3BC, 8, 1) = -1 then Showmessage('Error ioperm $3BC') else;
{$ENDIF}
end;

destructor TPLP.Destroy;
begin
{
{$IFDEF WINDOWS}
  FreeLibrary(FInpout32);
{$ENDIF}
}

{$IFDEF Linux}
  if ioperm($378, 8, 0) = -1 then Showmessage('Error ioperm $378 off') else;
  //if ioperm($278, 8, 1) = -1 then Showmessage('Error ioperm $278') else;
  //if ioperm($3BC, 8, 1) = -1 then Showmessage('Error ioperm $3BC') else;
{$ENDIF}
  inherited Destroy;
end;

procedure TPLP.OnTimerMethod(AutoOff: Boolean);
begin
  if AutoOff then
    FCsqTimer.OnTimer := OutPortOff
  else
    FCsqTimer.OnTimer := OutPortNone;
end;

procedure TPLP.OutPortNone (Sender: TObject);
begin

end;

procedure TPLP.OutPortOff (Sender: TObject);
begin
  if FTimer.Running then FTimer.Running := False;

  FCsqTimer.Enabled := False;
{$IFDEF WINDOWS}
  Out32($378, 0);
  //FOut32($278, 0);
  //FOut32($3BC, 0);
{$ENDIF}

{$IFDEF Linux}
  port[$378] := 0;
  //port[$278] := 0;
  //port[$3BC] := Data;
{$ENDIF}
  ;
end;

procedure TPLP.OutPortOn(Data: BYTE);
begin
{$IFDEF WINDOWS}
  Out32($378, Data);
  //FOut32($278, Data);
  //FOut32($3BC, Data);
{$ENDIF}

{$IFDEF Linux}
  port[$378] := Data;
  //port[$278] := Data;
  //port[$3BC] := Data;
{$ENDIF}
  FCsqTimer.Enabled := True;

  FTimer := TClockThread.Create(True);
  FTimer.Interval := 50;
  FTimer.OnTimer := @OutPortOff; // descendents can implement the ThreadClock method
  FTimer.Start;
end;



end.

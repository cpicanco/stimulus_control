//
// Stimulus Control App.
// Copyright (C) 2014,  Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.
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
unit interface_library;

{$MODE Delphi}
//{$mode objfpc}{$H+}

interface

uses Classes,

     {$IFDEF WINDOWS}
     Windows, Registry,
     {$ENDIF}

     {$IFDEF Linux}
     Ports,{Serial,}
     {$ENDIF}
     Synaser, {----> Serial port library, windows and linux}
     Dialogs, Forms, ExtCtrls,
     SysUtils, LCLIntf, LCLType, LMessages;

type

  {$IFDEF WINDOWS}

  TInp32 = function(Address: ShortInt): BYTE; stdcall;
  TOut32 = procedure(Address: ShortInt; Data: BYTE); stdcall;

  {$ENDIF}

  { TPLP }

  TPLP = class (TComponent)
  private
    FCsqTimer : TTimer;
  {$IFDEF WINDOWS}
    FInpout32: THandle;
    FInp32: TInp32;
    FOut32: TOut32;
  {$ENDIF}
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    procedure OutPortOn (Data : BYTE);
    procedure OutPortOff (Sender: TObject);
    procedure OutPortNone (Sender: TObject);
    procedure OnTimerMethod(AutoOff: Boolean);

  end;

  { TRS232 }

  TRS232 = class (TObject)
    private
      FBlockSerial : TBlockSerial;
      function GetCommPortNumber : ShortInt;
      function GetCommPortName : string;
    public
      constructor Create; reintroduce;
      destructor Destroy; override;
      procedure Dispenser (Data : String);
      //function GetUsbCsqFromValue (Value : String) : ShortInt;  deprecated
  end;

 {$IFDEF Linux}
 //program need to be executed under/have root privileges
 function ioperm(from: Cardinal; num: Cardinal; turn_on: Integer): Integer; cdecl; external 'libc';
 {$ENDIF}

 {older calls Delphi 7, XE 2, 3}
 //function inportb(EndPorta: Integer): BYTE stdcall; external 'inpout32.DLL' name 'Inp32';
 //procedure outportb(EndPorta: Integer; Valor:BYTE); stdcall; external 'inpout32.DLL' name 'Out32';

var FPLP : TPLP;
    FRS232 : TRS232;


implementation

{TPLP}

constructor TPLP.Create (AOwner : TComponent);
begin
  FCsqTimer := TTimer.Create (Self);
  with FCsqTimer do
    begin
      Enabled := False;
      Interval := 50;
      OnTimer := OutPortOff;
    end;

{$IFDEF WINDOWS}
   FInpout32 := LoadLibrary('inpout32.dll');
   if (FInpout32 <> 0) then
   begin
     @FInp32 := GetProcAddress(FInpout32, 'Inp32');
     if (@FInp32 = nil) then ShowMessage('Error Inp32');
     @FOut32 := GetProcAddress(FInpout32, 'Out32');
     if (@FOut32 = nil) then ShowMessage('Error Out32');
   end
   else ShowMessage ('Error Inpout32');
{$ENDIF}

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
{$IFDEF WINDOWS}
  FreeLibrary(FInpout32);
{$ENDIF}

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

  FCsqTimer.Enabled := False;
{$IFDEF WINDOWS}
  FOut32($378, 0);
  //FOut32($278, 0);
  //FOut32($3BC, 0);
{$ENDIF}

{$IFDEF Linux}
  port[$378] := 0;
  //port[$278] := 0;
  //port[$3BC] := Data;
{$ENDIF}
end;

procedure TPLP.OutPortOn(Data: BYTE);
begin
{$IFDEF WINDOWS}
  FOut32($378, Data);
  //FOut32($278, Data);
  //FOut32($3BC, Data);
{$ENDIF}

{$IFDEF Linux}
  port[$378] := Data;
  //port[$278] := Data;
  //port[$3BC] := Data;
{$ENDIF}
  FCsqTimer.Enabled := True;
end;

{TRS232}

function TRS232.GetCommPortNumber: ShortInt;
var
  {$IFDEF WINDOWS}
  Reg: TRegistry;
  {$ENDIF}
  s1: string;
begin
  {$IFDEF WINDOWS}
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  s1 := 'HARDWARE\DEVICEMAP\SERIALCOMM';

  if Reg.KeyExists(s1) then
    begin
      Reg.OpenKeyReadOnly (s1);
      if Reg.ValueExists('\Device\VCP0') then
        begin
          s1:= Reg.ReadString('\Device\VCP0');
          Delete (s1, 1, 3);
          if (StrToIntDef (s1, 17) <= 16) or (StrToIntDef (s1, 0) >= 1) then
            Result := StrToInt (s1)
          else Result := -1;
        end
      else Result := -1;
    end
  else Result := -1;
  Reg.CloseKey;
  Reg.Free;
  {$ENDIF}

  {$IFDEF LINUX}
  Result := 0;
  {$ENDIF}
end;

function TRS232.GetCommPortName: string;
var PortNumber : ShortInt;
begin
  PortNumber := GetCommPortNumber;
{$IFDEF WINDOWS}
  if PortNumber > 9 then
     Result := Format('\\\\.\\COM%d', [PortNumber])
  else
     Result := Format('COM%d', [PortNumber]);
{$ENDIF}

{$IFDEF Linux}
  Result := Format('/dev/ttyUSB%d', [PortNumber]);
{$ENDIF}

end;

constructor TRS232.Create;
var ComX : shortint;
    PortName : string;
begin
  inherited Create;
  ComX := GetCommPortNumber;
  if ComX = -1 then
    begin
      {ShowMessage('Interface Serial-USB não encontrada ou não suportada.' + #13#10 +
                  'Verifique se a conexão e a instalação do dispositivo foram realizadas corretamente.');}
    end
  else
    begin
      PortName := GetCommPortName; //ex.: Linux: '/dev/ttyUSB0' Windows: 'Com3'
      FBlockSerial := TBlockSerial.Create;
      //FBlockSerial.Connect(PortName);
      FBlockSerial.Connect('/dev/ttyUSB0');
          Sleep(1500);
      FBlockSerial.config(9600, 8, 'N', SB1, false, false);
          Sleep(1500);
    end;
end;

destructor TRS232.Destroy;
begin
  if Assigned(FBlockSerial) then
  begin
   FBlockSerial.Free;
  end;

  inherited Destroy;
end;

procedure TRS232.Dispenser(Data: String);
//var DispenserOutput : AnsiChar;
begin
  if Assigned (FBlockSerial) then
    begin
      FBlockSerial.SendString(Data);
    end;
end;


{deprecated methods}
{
function TRS232.GetUsbCsqFromValue(Value: String): ShortInt;
var s1 : string;
begin

  Value := Value + #32;
  s1:= Copy(Value, 0, Pos(#32, Value) -1);

  Delete(Value, 1, pos(#32, Value));
  if Length(Value)> 0 then while Value[1]= ' ' do Delete(Value, 1, 1);

  if StrToIntDef(s1, -1) = -1 then Result := 0
  else
    begin
      if (s1 = '1') then Result := 1
        else
        if (s1 = '2') then Result := 2
          else
            if (s1 = '3') then Result := 3
              else
              if (s1 = '4') then Result := 4
                else Result := 0;
    end
end;
 }

end.

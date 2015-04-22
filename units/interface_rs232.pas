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
unit interface_rs232;

{$mode objfpc}{$H+}

interface

// this unit was not implemented yet

uses
     Classes, SysUtils,

     {$IFDEF WINDOWS}
     Windows, Registry,
     {$ENDIF}

     Synaser;

type

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

var FRS232 : TRS232;

implementation

{TRS232}

function TRS232.GetCommPortNumber: ShortInt;
{$IFDEF WINDOWS}
var
  Reg: TRegistry;
  s1: string;
{$ENDIF}
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


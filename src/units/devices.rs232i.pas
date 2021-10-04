{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Devices.RS232i;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils
    {$IFDEF WINDOWS}
    , Registry
    {$ENDIF}
    , Synaser
    ;

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
      procedure Dispenser(Data : String);
  end;

var RS232 : TRS232;

implementation

{TRS232}

function TRS232.GetCommPortNumber: ShortInt;
{$IFDEF WINDOWS}
var
  Reg: TRegistry;
  LKey: string;
{$ENDIF}
begin
  Result := -1;
  {$IFDEF WINDOWS}
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  LKey := 'HARDWARE\DEVICEMAP\SERIALCOMM';
  if Reg.KeyExists(LKey) then
    begin
      Reg.OpenKeyReadOnly(LKey);
      if Reg.ValueExists('\Device\VCP0') then
        begin
          LKey := Reg.ReadString('\Device\VCP0');
          Delete(LKey, 1, 3);
          case StrToIntDef(LKey, -1) of
            1..16: Result := StrToInt(LKey);
          end;
        end;
    end;
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
  {$IFDEF DEBUG}
  WriteLn( 'Ports available: ' +  GetSerialPortNames);
  {$ENDIF}
  ComX := GetCommPortNumber;
  if ComX = -1 then
    begin
      {ShowMessage('Interface Serial-USB não encontrada ou não suportada.' + LineEnding +
                  'Verifique se a conexão e a instalação do dispositivo foram realizadas corretamente.');}
    end
  else
    begin
      PortName := GetCommPortName; //ex.: Linux: '/dev/ttyUSB0' Windows: 'Com3'
      FBlockSerial := TBlockSerial.Create;
      //FBlockSerial.Connect(PortName);
      FBlockSerial.RaiseExcept := false;
      {$IFDEF LINUX}
      FBlockSerial.LinuxLock := false;  // user must have full access to /var
      {$ENDIF}
      FBlockSerial.Connect(PortName);
      FBlockSerial.config(9600, 8, 'N', SB1, false, false);
    end;
end;

destructor TRS232.Destroy;
begin
  if Assigned(FBlockSerial) then
    FBlockSerial.Free;

  inherited Destroy;
end;

procedure TRS232.Dispenser(Data: String);
begin
  if Assigned(FBlockSerial) then
    FBlockSerial.SendString(Data);
end;

initialization
  RS232 := TRS232.Create;

finalization
  RS232.Free;

end.


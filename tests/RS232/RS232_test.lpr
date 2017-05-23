program RS232_test;

uses sysutils, interface_rs232;

var RS232 : TRS232;

begin
  RS232 := TRS232.Create;
  RS232.Dispenser('0');
  Sleep(1000);
  RS232.Dispenser('1');
  Sleep(1000);
  RS232.Dispenser('2');
  Sleep(1000);
  RS232.Dispenser('3');
  Sleep(1000);
  RS232.Dispenser('4');
  Sleep(1000);
  RS232.Free;
end.


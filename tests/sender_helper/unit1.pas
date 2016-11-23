unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    SendRequest: TButton;
    procedure SendRequestClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses zmq_network;

{$R *.lfm}

{ TForm1 }

procedure TForm1.SendRequestClick(Sender: TObject);
begin
  TZMQServerThread;
end;

end.


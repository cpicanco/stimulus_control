unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls

  //, zmq
  , zmqapi
  //, fpjson
  //, jsonparser
  , linux
  , unixtype
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnRequestStopRecording: TButton;
    btnRequestTimestamp: TButton;
    btnGetTimestamp: TButton;
    btnRequestRawTimestamp: TButton;
    btnRequestStartRecording: TButton;
    lblDescription: TLabel;
    ltbTimestamps: TListBox;
    procedure btnRequestTimestampClick(Sender: TObject);
    procedure btnGetTimestampClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnRequestTimestampClick(Sender: TObject);
var
  FContext : TZMQContext;
  FRequester : TZMQSocket;
  AMessage,
  FServerAddress : UTF8String;
begin
  AMessage := '';
  FServerAddress := 'localhost:5020';
  FContext := TZMQContext.Create;
  FRequester := FContext.Socket( stReq );
  try
    FRequester.connect( 'tcp://' + FServerAddress );

    if TButton(Sender) = btnRequestRawTimestamp then
      FRequester.send( 'T' );

    if TButton(Sender) = btnRequestTimestamp then
      FRequester.send( 't' );

    if TButton(Sender) = btnRequestStartRecording then
      FRequester.send( 'R' );

    if TButton(Sender) = btnRequestStopRecording then
      FRequester.send( 'r' );

    FRequester.recv( AMessage );
    ltbTimestamps.Items.Append(AMessage);
  finally
    FRequester.Free;
    FContext.Free;
  end;
end;

procedure TForm1.btnGetTimestampClick(Sender: TObject);
var
  tp: timespec;
  a, b : Extended;
begin
  clock_gettime(CLOCK_MONOTONIC, @tp);
  a := tp.tv_sec;
  b := tp.tv_nsec * 1e-9;
  ltbTimestamps.Items.Append(FloatToStrF(a+b, ffFixed, 0, 9));
end;

end.


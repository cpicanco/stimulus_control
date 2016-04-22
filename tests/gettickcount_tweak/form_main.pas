unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls

  , Timestamp
  , Linux
  , UnixType
  ;


type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ListBox1: TListBox;
    ListBox2: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

const
  C_SIZE = 1000;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormActivate(Sender: TObject);
var i : integer;
begin
  for i := 0 to C_SIZE do
    begin
      ListBox1.Items.Append('');
      ListBox2.Items.Append('');
    end;
  WriteLn(GetResolution);
end;

procedure TForm1.Button1Click(Sender: TObject);
var i : integer;
    tp : array [0 .. C_SIZE] of timespec;
    a, b : Extended;
begin
  for i := 0 to C_SIZE do
    clock_gettime(CLOCK_MONOTONIC, @tp[i]);

  for i := 0 to C_SIZE do
    begin
      a := tp[i].tv_sec;
      b := tp[i].tv_nsec * 1e-9;
      ListBox1.Items[i] := FloatToStrF(a+b, ffFixed, 0, 9);
    end;

  for i := 0 to C_SIZE do
    ListBox2.Items[i] := IntToStr(i);

  for i := 0 to C_SIZE-1 do
    begin
      a := tp[i+1].tv_sec + (tp[i+1].tv_nsec * 1e-9);
      b := tp[i].tv_sec + (tp[i].tv_nsec * 1e-9);
      ListBox2.Items[i + 1] := FloatToStrF(a-b, ffFixed, 0, 9);
      WriteLn(ListBox2.Items[i])
    end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var i : integer;
    tp : array [0 .. C_SIZE] of timespec;
    a, b : Extended;
begin
  for i := 0 to C_SIZE do
    clock_gettime(CLOCK_MONOTONIC_RAW, @tp[i]);

  for i := 0 to C_SIZE do
    begin
      a := tp[i].tv_sec;
      b := tp[i].tv_nsec * 1e-9;
      ListBox1.Items[i] := FloatToStrF(a+b, ffFixed, 0, 9);
    end;

  for i := 0 to C_SIZE do
    ListBox2.Items[i] := IntToStr(i);

  for i := 0 to C_SIZE-1 do
    begin
      a := tp[i+1].tv_sec + (tp[i+1].tv_nsec * 1e-9);
      b := tp[i].tv_sec + (tp[i].tv_nsec * 1e-9);
      ListBox2.Items[i + 1] := FloatToStrF(a-b, ffFixed, 0, 9);
    end;
end;

end.


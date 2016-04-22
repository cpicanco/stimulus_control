//
// Validation Project (PCRF) - Stimulus Control App
// Copyright (C) 2014-2016,  Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.
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


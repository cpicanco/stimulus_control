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
unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls
, custom_timer
;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnRestart2: TButton;
    btnStart: TButton;
    btnStart2: TButton;
    btnStop: TButton;
    btnRestart: TButton;
    btnStop2: TButton;
    Button1: TButton;
    GroupBox1: TGroupBox;
    LabelCounter: TLabel;
    btnAssigned: TButton;
    procedure btnRestart2Click(Sender: TObject);
    procedure btnStart2Click(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStop2Click(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnRestartClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAssignedClick(Sender: TObject);
  private
    FN : integer;
    FLabels : array of TLabel;
    FCardinal : cardinal;
    FClock : TClockThread;
    FClocks : array of TClockThread;
    FCardinals : array of cardinal;
    procedure OnClockTimerEx(Sender : TObject);
    procedure OnClockTerminateEX(Sender : TObject);
    procedure OnClockTimer(Sender : TObject);
    procedure OnClockTerminate(Sender: TObject);
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnStartClick(Sender: TObject);
begin
  btnStart.Enabled := False;
  btnStop.Enabled := True;
  btnRestart.Enabled := True;

  FCardinal := 0;

  FClock := TClockThread.Create(True);
  FClock.Interval := 100;
  FClock.OnTimer := @OnClockTimer;
  FClock.OnTerminate := @OnClockTerminate;
  FClock.Start;
end;

procedure TForm1.btnStop2Click(Sender: TObject);
begin

end;

procedure TForm1.btnStart2Click(Sender: TObject);
var
  i : integer;

begin
  btnStart2.Enabled := False;
  btnStop2.Enabled := True;
  btnRestart2.Enabled := True;

  SetLength(FClocks, FN);
  SetLength(FCardinals, FN);

  for i := 0 to FN -1 do
    begin
      FCardinals[i] := 0;
      FClocks[i] := TClockThread.Create(True);
      FClocks[i].Host := FLabels[i];
      //FClocks[i].Interval := 500;
      FClocks[i].OnTimer := @OnClockTimerEx;
      FClocks[i].OnTerminate := @OnClockTerminateEX;
      //FClocks[i].WaitFor;
    end;

  for i := 0 to FN -1 do
    begin
      FClocks[i].Start;
    end;
end;

procedure TForm1.btnRestart2Click(Sender: TObject);
begin

end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  btnStart.Enabled := True;
  btnRestart.Enabled := False;
  btnStop.Enabled := False;

  FClock.Terminate;
  // FClock.WaitFor;
  // FreeAndNil(FClock);
end;

procedure TForm1.btnRestartClick(Sender: TObject);
begin
  FCardinal := 0;
  LabelCounter.Caption := IntToStr(FCardinal);
  RTLeventSetEvent(FClock.ResumeEvent);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FLabels[0].Caption := 'oi';
end;

procedure TForm1.FormCreate(Sender: TObject);
var i : integer;
begin
  FN := 16;

  SetLength(FLabels, FN);

  for i := 0 to FN -1 do
    begin
      FLabels[i] := TLabel.Create(Self);
      FLabels[i].Caption := '0';
      FLabels[i].Tag := i;
      FLabels[i].Left := 380;
      FLabels[i].Top := 64 + (i * 20);
      FLabels[i].Parent := Self;
    end;
end;

procedure TForm1.btnAssignedClick(Sender: TObject);
begin
  // FreeOnTerminate := True does not assign 'nil' to FClock
  // But it will free the thread's resources.
  ShowMessage(BoolToStr(Assigned(FClock), 'True', 'False'));
end;

procedure TForm1.OnClockTimerEx(Sender: TObject);
var
  Host : TObject;
  i : integer;
begin
  Host := TClockThread(Sender).Host;
  i := TLabel(Host).Tag;
  TLabel(Host).Caption := IntToStr(FCardinals[i]);
  Inc(FCardinals[i]);
end;

procedure TForm1.OnClockTerminateEX(Sender: TObject);
var
  Host : TObject;
  i : integer;
begin
  Host := TClockThread(Sender).Host;
  i := TLabel(Host).Tag;
  TLabel(Host).Caption := IntToStr(TClockThread(Sender).ThreadID);

end;

procedure TForm1.OnClockTimer(Sender: TObject);
begin
  LabelCounter.Caption := IntToStr(FCardinal);
  Inc(FCardinal);
end;

procedure TForm1.OnClockTerminate(Sender: TObject);
begin
  LabelCounter.Caption := IntToStr(TClockThread(Sender).ThreadID);
end;

end.


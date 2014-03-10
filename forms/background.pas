//
// Validation Project (PCRF) - Stimulus Control App
// Copyright (C) 2014,  Carlos Rafael Fernandes Picanço, cpicanco@ufpa.br
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
unit background;

{$mode objfpc}{$H+}

interface

uses
  LCLType, LCLIntf, Math,
  {$IFDEF LCLGTK2}
  gtk2, gdk2, //glib2,
  {$ENDIF}
  Classes, SysUtils, FileUtil, Forms
, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls
, client
, draw_methods
, config
;

type

  { Tbkgnd }

  Tbkgnd = class(TForm)
    debugconsole: TMemo;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormPaint(Sender: TObject);
  private
    FTimer : TTimer;
    FSessionConfig : TConfig;
    FVisibleTrial : Boolean;
    FFullScreen : Boolean;
    FOriginalBounds: TRect;
    FOriginalWindowState: TWindowState;
    FScreenBounds: TRect;
    FClientThread : TClientThread;
    //FSimpleClient: TSyncSub;

    procedure CreateClientThread(Code : String);
    procedure DebugStatus(msg : string);
    procedure TimerOnTimer(Sender : TObject);
  public
    { public declarations }
    procedure SetFullScreen(TurnOn : Boolean);
    procedure RunSession(Step, Circle_Size, ITI : integer);
  end;

var
  bkgnd: Tbkgnd;

implementation

{$R background.lfm}

{ Tbkgnd }

procedure Tbkgnd.FormCreate(Sender: TObject);
begin
  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := @TimerOnTimer;
  FFullScreen := False;
  FVisibleTrial := False;
  Randomize;
end;

procedure Tbkgnd.FormActivate(Sender: TObject);
begin

end;

procedure Tbkgnd.FormDestroy(Sender: TObject);
begin
  if Assigned(FClientThread) then FClientThread.Terminate;
  //DeleteCriticalSection(FCriticalSection);
end;

procedure Tbkgnd.FormKeyPress(Sender: TObject; var Key: char);
var msg: string;
begin
  //if key in ['a', 'A'] then SetFullScreen(not FFullScreen);

  if key in [#32] then
    begin
      if FVisibleTrial then
        begin
          CreateClientThread('space_vi');
          if not FSessionConfig.NextTrial then
            begin
              ShowMessage('End.');
              CreateClientThread('end');
              Exit;
            end;
          FVisibleTrial := False;
          Invalidate;
          FTimer.Enabled := True;
        end;
    end;

  if key in ['s', 'S'] then
    begin
      CreateClientThread('press_s');
      FClientThread.Start;
      Exit;
    end;


    if key in ['d', 'D'] then FClientThread.Terminate;

    //
    //if key in ['t', 'T'] then ShowMessage(FSimpleClient.LastLog);
    //
end;

procedure Tbkgnd.FormPaint(Sender: TObject);
begin
  CenteredMarker (Canvas, width, height, 6);

  if FVisibleTrial then
  with FSessionConfig do
    with CT do
    begin
      if (X.axis[i] = 'd1') then
        begin
          with C[0] do DrawCircle(Canvas, o.X, o.Y, size, gap, gap_degree, gap_length);
          with C[1] do DrawCircle(Canvas, o.X - size, o.Y - size, size, gap, gap_degree, gap_length);
        end;
      if (X.axis[i] = 'd2') then
        begin
          with C[0] do DrawCircle(Canvas, o.X - size, o.Y, size, gap, gap_degree, gap_length);
          with C[1] do DrawCircle(Canvas, o.X, o.Y - size, size, gap, gap_degree, gap_length);
        end;
      if (X.axis[i] = 'h') then
        begin
          with C[0] do DrawCircle(Canvas, o.X, o.Y + 13, size, gap, gap_degree, gap_length);
          with C[1] do DrawCircle(Canvas, o.X - size, o.Y + 13, size, gap, gap_degree, gap_length);
        end;
      if (X.axis[i] = 'v') then
        begin
          with C[0] do DrawCircle(Canvas, o.X + 13, o.Y, size, gap, gap_degree, gap_length);
          with C[1] do DrawCircle(Canvas, o.X + 13, o.Y - size, size, gap, gap_degree, gap_length);
        end;
    end;
end;

procedure Tbkgnd.CreateClientThread(Code : string);
begin
  FClientThread := TClientThread.Create( True, FSessionConfig.CT.i, Code );
  FClientThread.OnShowStatus := @DebugStatus;
  FClientThread.Start;
end;

procedure Tbkgnd.DebugStatus(msg: string);
begin
  debugconsole.Lines.Append(msg);
  //Exit;
end;

procedure Tbkgnd.TimerOnTimer(Sender: TObject);
begin
  FTimer.Enabled:= False;
  CreateClientThread('on_timer');
  FVisibleTrial := True;
  Invalidate;
end;

procedure Tbkgnd.SetFullScreen(TurnOn: Boolean);
begin
  if TurnOn then
    begin
      // To full screen
      FOriginalWindowState := WindowState;
      FOriginalBounds := BoundsRect;

      BorderStyle := bsNone;
      FScreenBounds := Screen.MonitorFromWindow(Handle).BoundsRect;
    with FScreenBounds do
      SetBounds(Left, Top, Right - Left, Bottom - Top);
      {$IFDEF WINDOWS}
        Application.ShowInTaskBar := False;
      {$ENDIF}

      {$IFDEF LCLGTK2}
      gdk_window_fullscreen(PGtkWidget(Self.Handle)^.window);
      {$ENDIF}
    end
  else
    begin
      {$IFDEF MSWINDOWS}
      BorderStyle := bsSizeable;
      {$ENDIF}

      if FOriginalWindowState = wsMaximized then
        WindowState := wsMaximized
      else
        with FOriginalBounds do
          SetBounds(Left, Top, Right - Left, Bottom - Top);

      {$IFDEF LINUX}
      BorderStyle := bsSizeable;
      {$ENDIF}

      {$IFDEF LCLGTK2}
      gdk_window_unfullscreen(PGtkWidget(Self.Handle)^.window);
      {$ENDIF}
    end;
  FFullScreen := TurnOn;
end;

procedure Tbkgnd.RunSession(Step, Circle_Size, ITI: integer);
begin
  FSessionConfig := TConfig.Create(Self, Step, Circle_Size);
  FTimer.Interval := 1000;
  FVisibleTrial := True;
  CreateClientThread('begin');
  Invalidate;
end;

end.


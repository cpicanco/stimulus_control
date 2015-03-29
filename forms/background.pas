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
unit background;

{$mode objfpc}{$H+}

interface

uses
  LCLType, LCLIntf, {Math,}
  {$IFDEF LCLGTK2}
  gtk2, gdk2, glib2,
  {$ENDIF}
  Classes, SysUtils, FileUtil, Forms
, Controls, Graphics, Dialogs, {StdCtrls,} ExtCtrls
//, regdata
//, client
//, draw_methods
;

type

  { Tbkgnd }

  Tbkgnd = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FFullScreen : Boolean;
    FOriginalBounds: TRect;
    FOriginalWindowState: TWindowState;
    FScreenBounds: TRect;

  public

    procedure SetFullScreen(TurnOn : Boolean);

  end;

var
  bkgnd: Tbkgnd;

implementation

{$R background.lfm}

{ Tbkgnd }

procedure Tbkgnd.FormCreate(Sender: TObject);
begin
  FFullScreen := False;
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

end.


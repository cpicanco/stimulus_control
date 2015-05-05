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
unit cumulative_record;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls
, custom_timer;

type

  { TCummulativeRecord }

  TCummulativeRecord = class(TCustomControl)
  private
    FClock : TClockThread;
    FOX : integer;
    FOY : integer;
    FNX : integer;
    FNY : integer;
    procedure OnTimer(Sender : TObject);
  public
    constructor Create(AOwner : TWinControl);
    procedure Reset;
    procedure Paint; override;
    procedure IncNX(n : integer);
    procedure DecNY(n : integer);
    procedure DrawEvent(LeftTop : Boolean);
  end;

implementation

{ TCummulativeRecord }

constructor TCummulativeRecord.Create(AOwner: TWinControl);
begin
  inherited Create(AOwner);
  FClock := TClockThread.Create(True);
  FClock.OnTimer := @OnTimer;
  FClock.Interval := 125;

  Parent:= AOwner;
  Align := alClient;
  Color := clTeal;
  Canvas.Pen.Color := RGBToColor(255,128,255);
  Canvas.Pen.Style := psDot;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Mode := pmCopy;

  Reset;
  BringToFront;

  FClock.Start;
end;

procedure TCummulativeRecord.Reset;
begin
  FOX := 2;
  FOY := Height - 10;
  Canvas.MoveTo(FOX, FOY);

  FNX := 2;
  FNY := Height -10;
  Invalidate;
end;

procedure TCummulativeRecord.Paint;
begin
  Canvas.LineTo(FNX, FNY);
end;

procedure TCummulativeRecord.IncNX(n: integer);
begin
  Inc(FNX, n);
  Paint;
end;

procedure TCummulativeRecord.DecNY(n: integer);
begin
  Dec(FNY, n);
  Paint;
end;

procedure TCummulativeRecord.OnTimer(Sender: TObject);
begin
  IncNX(1);
  if FNX > (Width -2) then
    begin
      FNX := 2;
      Canvas.MoveTo(FNX, FNY);
      Reset;
    end;

  if FNY < 2 then
    begin
      FNY := Height - 10;
      Canvas.MoveTo(FNX, FNY);
    end;
  Paint;
end;

procedure TCummulativeRecord.DrawEvent(LeftTop : Boolean);
begin
  if LeftTop then
    begin
      Canvas.MoveTo(Canvas.PenPos.X - 6, Canvas.PenPos.Y - 6);
      Canvas.LineTo(Canvas.PenPos.X + 6, Canvas.PenPos.Y + 6);
    end
  else
    begin
      Canvas.MoveTo(Canvas.PenPos.X + 6, Canvas.PenPos.Y + 6);
      Canvas.LineTo(Canvas.PenPos.X - 6, Canvas.PenPos.Y - 6);
    end;
end;

end.


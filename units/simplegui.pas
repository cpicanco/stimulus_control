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
unit simplegui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
, Controls
, draw_methods
;

type

{ TSimpleGui }

TSimpleGui = class (TGraphicControl)
  private
    FDeltaX,
    FDiameter, //160
    //Left,     //12
    //Top,      //200
    FDegree : integer;
    //Contour: array of TPoint;
    FMouseDown : Boolean;
    FMouseDownPoint : TPoint;
  protected
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner : TComponent);

end;


implementation

{ TSimpleGui }

procedure TSimpleGui.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if mbLeft = Button then
       begin
         FMouseDownPoint := Point(X,Y);
         FMouseDown := True;
         Invalidate;
       end;
end;

procedure TSimpleGui.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
     if FMouseDown then
      begin
        if ssCtrl in Shift then
          begin
            if X > FMouseDownPoint.X then Inc(FDeltaX, 3);
            if X < FMouseDownPoint.X then Dec(FDeltaX, 3);
          end
        else
          begin
            if X > FMouseDownPoint.X then Inc(FDeltaX, 10);
            if X < FMouseDownPoint.X then Dec(FDeltaX, 10);
          end;

        case FDeltaX of
          -20..-10: begin
                      if FDegree > 0 then Dec(FDegree) else
                        if FDegree = 0 then FDegree := 360;
                      FDeltaX := 0;
                      //lblAxis1.Caption := IntToStr(Degree);
                      Invalidate;
                    end;
          -9 ..  9: {do nothing};
          10 .. 20: begin
                      if FDegree < 360 then Inc(FDegree) else
                        if FDegree = 360 then FDegree := 0;
                      FDeltaX := 0;
                      //lblAxis1.Caption := IntToStr(FDegree);
                      Invalidate;
                    end;
        else
          //Degree := 0;
          //DeltaX := 0;
          //lblAxis1.Caption := IntToStr(Degree);
          //Invalidate;
        end;
      end;
end;

procedure TSimpleGui.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FMouseDown then
    begin
      FMouseDown := False;
      Invalidate;
    end;
end;

procedure TSimpleGui.Paint;
begin
  inherited Paint;
  DrawCircle(Canvas, Left, Top, FDiameter, True, FDegree, 1);
  if FMouseDown then DrawMiniCircle(Canvas, FMouseDownPoint, 3);
end;

constructor TSimpleGui.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDeltaX := 0;
  //Left := 12;
  //Top := 200;
  FDiameter := 160;
  Width := FDiameter;
  Height:= FDiameter;
  FDegree := 0;
  FMouseDown := False;
  FMouseDownPoint := Point(-1, -1);
  //SetLength(Contour, 4);
  //Contour[0] := Point(Left,Top);
  //Contour[1] := Point(Left + Diameter, Top + Diameter);
  //Contour[2] := Point(Left, Top + Diameter);
  //Contour[3] := Point(Left + Diameter, Top);
end;


end.


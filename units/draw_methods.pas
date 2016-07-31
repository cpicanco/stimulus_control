{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit draw_methods; //helpers

{$mode objfpc}{$H+}

interface

uses Graphics, SysUtils, Classes, Controls;

type

  TPoints = array of TPoint;

  TArc = record
    p1 : TPoint;
    p2 : TPoint;
  end;

function IsPointInPolygon(AX, AY: Integer; APolygon: array of TPoint): Boolean;

function BresenhamLine(x0, x1, y0, y1 : integer): TPoints;
procedure CenteredMarker(Canvas: TCanvas; Width, Height, size: integer);
procedure DrawCenteredCircle (Canvas : TCanvas; Width, Height, Size : integer);
procedure DrawCircle(Canvas : TCanvas; left, top, size: integer; gap : Boolean; gap_degree, gap_length: integer);
procedure DrawMiniCircle(Canvas: TCanvas; center: TPoint; size : integer);
procedure PlotPixel (Canvas: TCanvas; aPoint : TPoint; clColor : TColor);
procedure TopBottomLine(Canvas:TCanvas; aControl : TControl);

implementation

function IsPointInPolygon(AX, AY: Integer; APolygon: array of TPoint): Boolean;
//  The function will return True if the point x,y is inside the polygon, or
//  False if it is not.
//
//  Original C code: http://www.visibone.com/inpoly/inpoly.c.txt
//
//  Translation from C by Felipe Monteiro de Carvalho
//
//  License: Public Domain

var
  xnew, ynew: Cardinal;
  xold,yold: Cardinal;
  x1,y1: Cardinal;
  x2,y2: Cardinal;
  i, npoints: Integer;
  inside: Integer = 0;
begin
  Result := False;
  npoints := Length(APolygon);
  if (npoints < 3) then Exit;

  xold := APolygon[npoints-1].X;
  yold := APolygon[npoints-1].Y;
  for i := 0 to npoints - 1 do
    begin
      xnew := APolygon[i].X;
      ynew := APolygon[i].Y;
      if (xnew > xold) then
        begin
          x1:=xold;
          x2:=xnew;
          y1:=yold;
          y2:=ynew;
        end
      else
        begin
          x1:=xnew;
          x2:=xold;
          y1:=ynew;
          y2:=yold;
        end;

      // edge "open" at left end
      if (((xnew < AX) = (AX <= xold)) and ((AY-y1)*(x2-x1) < (y2-y1)*(AX-x1))) then
        inside := not inside;

      xold:=xnew;
      yold:=ynew;
    end;
  Result := inside <> 0;
end;

procedure PlotPixel (Canvas: TCanvas; aPoint : TPoint; clColor : TColor);
begin
  Canvas.Pixels[aPoint.X, aPoint.Y] := clColor;
end;

function BresenhamLine(x0, x1, y0, y1: integer): TPoints;
var
    dx, dy, sx, sy, err, err2 : integer;
begin
   dx := abs(x1-x0);
   dy := abs(y1-y0);
   if x0 < x1 then sx := 1 else sx := -1;
   if y0 < y1 then sy := 1 else sy := -1;
   err := dx-dy;

   while True do
   begin
     //Plot(x0,y0);
     SetLength(Result, Length(Result) + 1);
     Result[High(Result)] := Point(x0, y0);
     if (x0 = x1) and (y0 = y1) then Break;

     err2 := 2*err;
     if err2 > -dy then
     begin
       err := err - dy;
       x0 := x0 + sx;
     end;
     if err2 < dx then
     begin
       err := err + dx;
       y0 := y0 + sy;
     end;
   end;
end;

procedure TopBottomLine(Canvas: TCanvas; aControl: TControl);
var p1, p2 : TPoint;
begin
  with Canvas do
    begin
      Pen.Width := 1;
      with aControl do
        begin
          p1 := Point(Left, Top);
          p2 := Point(Left + Width, Top);
        end;
      Line(p1, p2);
      with aControl do
        begin
          p1 := Point(Left, Top + Height);
          p2 := Point(Left + Width, Top + Height);
        end;
      Line(p1, p2);
    end;
end;

procedure CenteredMarker(Canvas: TCanvas; Width, Height, size: integer);
var center : TPoint;
begin
  center.X := Round(Width / 2);
  center.Y := Round(Height / 2);

  with Canvas do
    begin
      Brush.Color := clBlack;
      Pen.Mode := pmCopy;
      Pen.Color := clBlack;
      Pen.Width := 2;
      with center do
        begin
          Line(X - X, Y, X + X, Y);
          Line(X, Y - Y, X, Y + Y);
        end;

      Brush.Color := clGreen;
      Brush.Style:= bsClear;
      Pen.Mode := pmCopy;
      Pen.Style:= psSolid;
      Pen.Color := clGreen;
      Pen.Width := 2;
      with center do
        Ellipse(X - size, Y - size, X + size, Y + size);

    end;
end;

procedure DrawCircle(Canvas: TCanvas; left, top, size: integer; gap: Boolean;
  gap_degree, gap_length: integer);
var
  fix : integer;
  inner_arc : TArc;
begin
  with Canvas do
    begin
      Brush.Style := bsSolid;
      Pen.Style := psSolid;
      Pen.Mode := pmXor;

      Brush.Color := clWhite;
      Pen.Color := clBlack;
      Pen.Width := 1;
      Ellipse(left,top, left + size, top + size);

      Brush.Color := clBlack;
      Brush.Style := bsClear;
      Pen.Color := clWhite;
      Pen.Mode := pmxor;
      Pen.Width := 1;

      fix := (size div 2) div 2;
      with inner_arc do
        begin
          p1.X := left + (size div 2) - fix;
          p1.Y := top + (size div 2) - fix;
          p2.X := left + size - fix;
          p2.Y := top + size - fix;
          Ellipse(p1.X, p1.Y, p2.X, p2.Y);
        end;

      // draw gap on inner circle circumference
      if gap then
        begin
          Pen.Color := clBlack;
          Pen.Mode := pmCopy;
          Pen.Width := 5;
          with inner_arc do
            //Arc(p1.X + 1, p1.Y + 1, p2.X - 1, p2.Y - 1, 16 * gap_degree, 16 * gap_length);
            Arc(p1.X , p1.Y , p2.X , p2.Y , 16 * gap_degree, 16 * gap_length);
        end;
    end;
end;

procedure DrawCenteredCircle (Canvas : TCanvas; Width, Height, Size : integer);
  var center : TPoint;
begin
  center.X := Round(Width / 2);
  center.Y := Round(Height / 2);

  with Canvas do
    begin
      Brush.Color := clBlack;
      Brush.Style:= bsClear;
      Pen.Mode := pmCopy;
      Pen.Style:= psSolid;
      Pen.Color := clBlack;
      Pen.Width := 5;
      with center do
        Ellipse(X - size, Y - size, X + size, Y + size);

   end;
end;


procedure DrawMiniCircle(Canvas: TCanvas; center: TPoint; size : integer);
var plusX : integer;
begin
  with Canvas do
    begin
      Brush.Color := clWhite;
      Brush.Style:= bsSolid;
      Pen.Mode := pmXor;
      Pen.Style:= psSolid;
      Pen.Color := clWhite;
      Pen.Width := 2;
      with center do
        begin
          Ellipse(X - size, Y - size, X + size, Y + size);
          Line(X - (size * 8), Y, X - (size *5), Y);
          Line(X + (size * 9), Y, X + (size *5), Y);
          plusX := Round((((X + (size * 9)) - (X + (size *5)))/2) + (X + (size *5)));
          Line(plusX, Y - (size * 2), plusX, Y + (size * 2));
          //Line(X, Y - size, X, Y + size);
        end;
    end;
end;

end.


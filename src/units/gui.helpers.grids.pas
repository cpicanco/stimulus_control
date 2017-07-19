{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit GUI.Helpers.Grids;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , math
  ;

function GetCentralRect(AWidth,AHeight,ASize:integer):TRect; overload;
function GetCentralRect(AWidth,AHeight,ALeftBorder,ATopBorder,
  ARightBorder,ABottomBorder: integer): TRect; overload;

// 0 degrees = rect right; increments clockwise, squares only
function GetPointFromAngle(AAngle: float; ARect:TRect): TPoint;

implementation

function GetCentralRect(AWidth, AHeight, ASize:integer): TRect;
begin
  Result := GetCentralRect(AWidth,AHeight,ASize,ASize,ASize,ASize);
end;

function GetCentralRect(AWidth, AHeight, ALeftBorder, ATopBorder, ARightBorder,
  ABottomBorder: integer): TRect;
var
  LSide : integer;
begin
  if AHeight > AWidth then
    begin
      LSide := AWidth;
      Result := Rect( (0 + ALeftBorder),
                      (0  + (AHeight div 2) - (LSide div 2) +  ATopBorder),
                      (LSide - ARightBorder),
                      (LSide + (AHeight div 2) - (LSide div 2) +  - ABottomBorder)
                    );
    end
  else if AHeight < AWidth then
    begin
      LSide := AHeight;
      Result := Rect( (0 + (AWidth div 2) - (LSide div 2) + ALeftBorder),
                      (0 + ATopBorder),
                      (LSide + (AWidth div 2) - (LSide div 2) - ARightBorder),
                      (LSide - ABottomBorder)
                    );
    end
  else if AHeight = AWidth then
    begin
      LSide := AHeight;
      Result := Rect( (0 + ALeftBorder),
                      (0 + ATopBorder),
                      (LSide - ARightBorder),
                      (LSide - ABottomBorder)
                    );
    end;
end;

function GetPointFromAngle(AAngle: float; ARect:TRect): TPoint;
var
  LRadius : float;
  LCenterX : float;
  LCenterY : float;
begin
  // http://math.stackexchange.com/questions/143932/calculate-point-given-x-y-angle-and-distance
  LRadius := (ARect.Right - ARect.Left)/ 2;
  LCenterX := LRadius + ARect.Left;
  LCenterY := LRadius + ARect.Top;
  Result.X :=  Round((LRadius * cos(DegtoRad(AAngle))) + LCenterX);
  Result.Y :=  Round((LRadius * sin(DegtoRad(AAngle))) + LCenterY);
end;

end.


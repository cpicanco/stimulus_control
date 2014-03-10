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
unit userconfigs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Spin

, draw_methods;

type

    { SimpleGui }

  { TSimpleGui }

  TSimpleGui = record
    DeltaX,
    Diameter, //160
    Left,     //12
    Top,      //200
    Degree : integer;
    Contour: array of TPoint;
    MouseDown : Boolean;
    MouseDownPoint : TPoint;
  end;

  { TUserConfig }

  TUserConfig = class(TForm)
    btnRun: TButton;
    lblAxis1: TLabel;
    lblmilisec: TLabel;
    lblStep: TLabel;
    lblGeneral: TLabel;
    lblpixels: TLabel;
    lblDiameter: TLabel;
    lblpixels1: TLabel;
    lblITI: TLabel;
    leParticipant: TLabeledEdit;
    seDiameter: TSpinEdit;
    seStep: TSpinEdit;
    seITI: TSpinEdit;
    procedure btnRunClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure lblAxis1Resize(Sender: TObject);
  private
    SimpleGui : TSimpleGui;
    procedure Init;

  public
    { public declarations }
  end;



var
  UserConfig: TUserConfig;


implementation

{$R *.lfm}

uses background;

{ SimpleGui }

procedure TUserConfig.Init;
begin
  with SimpleGui do
    begin
      DeltaX := 0;
      Left := 12;
      Top := 200;
      Diameter := 160;
      Degree := 0;
      MouseDown := False;
      MouseDownPoint := Point(-1, -1);
      SetLength(Contour, 4);
      Contour[0] := Point(Left,Top);
      Contour[1] := Point(Left + Diameter, Top + Diameter);
      Contour[2] := Point(Left, Top + Diameter);
      Contour[3] := Point(Left + Diameter, Top);
    end;
end;

{ TUserConfig }

procedure TUserConfig.FormPaint(Sender: TObject);
  var p1, p2 : TPoint;
begin
  with Canvas do
    begin
      Pen.Width := 1;
      p1 := Point(lblGeneral.Left, lblGeneral.Top);
      p2 := Point(lblGeneral.Left + lblGeneral.Width, lblGeneral.Top);
      Line(p1, p2);
      p1 := Point(lblGeneral.Left, lblGeneral.Top + lblGeneral.Height);
      p2 := Point(lblGeneral.Left + lblGeneral.Width, lblGeneral.Top + lblGeneral.Height);
      Line(p1, p2);

    end;
   with SimpleGui do
    begin
      DrawCircle(Canvas, Left, Top, Diameter, True, Degree, 1);

      if MouseDown then DrawMiniCircle(Canvas, MouseDownPoint,3);
    end;
end;

procedure TUserConfig.lblAxis1Resize(Sender: TObject);
begin
  with lblAxis1 do
    Left := 70 - (Width div 2);
end;


procedure TUserConfig.btnRunClick(Sender: TObject);
begin
  bkgnd := Tbkgnd.Create(Application);
  bkgnd.SetFullScreen(True);
  bkgnd.Show;
  bkgnd.RunSession(seStep.Value, seDiameter.Value, seITI.Value);
end;

procedure TUserConfig.FormActivate(Sender: TObject);
begin
  Invalidate;
end;

procedure TUserConfig.FormCreate(Sender: TObject);
begin
  Init;

end;

procedure TUserConfig.FormDestroy(Sender: TObject);
begin

end;

procedure TUserConfig.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if mbLeft = Button then
    with SimpleGui do
    if IsPointInPolygon(X,Y, Contour) then
      begin
        MouseDownPoint := Point(X,Y);
        MouseDown := True;
        Invalidate;
      end;
end;

procedure TUserConfig.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
//var
  //DeltaX
  //, DeltaY
  //: integer;
begin
  with SimpleGui do
   if MouseDown then
      begin
        if ssCtrl in Shift then
          begin
            if X > MouseDownPoint.X then Inc(DeltaX, 3);
            if X < MouseDownPoint.X then Dec(DeltaX, 3);
          end
        else
          begin
            if X > MouseDownPoint.X then Inc(DeltaX, 10);
            if X < MouseDownPoint.X then Dec(DeltaX, 10);
          end;

        case DeltaX of
          -20..-10: begin
                      if Degree > 0 then Dec(Degree) else
                        if Degree = 0 then Degree := 360;
                      DeltaX := 0;
                      lblAxis1.Caption := IntToStr(Degree);
                      Invalidate;
                    end;
          -9 ..  9: {do nothing};
          10 .. 20: begin
                      if Degree < 360 then Inc(Degree) else
                        if Degree = 360 then Degree := 0;
                      DeltaX := 0;
                      lblAxis1.Caption := IntToStr(Degree);
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

procedure TUserConfig.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  with SimpleGui do if MouseDown then
    begin
      MouseDown := False;
      Invalidate;
    end;
end;


end.


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
unit userconfigs_trial_mirrored;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
     Dialogs, StdCtrls, Spin, Grids, ExtCtrls

     , draw_methods
     , math
     ;

type

  { TTrialsPerNode }

  TTrialsPerNode = array of integer;

  { TCustomAxis }

  TCustomAxis = record
    Trials : integer;
    Angle : string;
    Size : integer;
    BresenhamLine : TPoints;
    Line : TPoints;
    MirroredLine : TPoints;
    TrialsPerNode : TTrialsPerNode;
  end;

  { TAxisList }

  TAxisList = record
    CanRead : Boolean;
    List : array of TCustomAxis;
  end;

  { TBresenhamLineForm }

  TBresenhamLineForm = class(TForm)
    btnAddAxis: TButton;
    btnEditNodes: TButton;
    btnMinimize: TButton;
    btnFinish: TButton;
    btnRmvAxis: TButton;
    btnShow: TButton;
    Button1: TButton;
    cbChooseGrid: TComboBox;
    cbCentralized: TCheckBox;
    cbUseGrid: TCheckBox;
    cbPreview: TCheckBox;
    cbNox0y0: TCheckBox;
    gbAddRmvAxis: TGroupBox;
    gbPointOne: TGroupBox;
    gbPointZero: TGroupBox;
    gbTrials: TGroupBox;
    lbNodes: TLabel;
    lbSize: TLabel;
    lbRepeat: TLabel;
    lbBorder: TLabel;
    Panel1: TPanel;
    seNodes: TSpinEdit;
    seSize: TSpinEdit;
    seRepeat: TSpinEdit;
    seBorder: TSpinEdit;
    seTrials: TSpinEdit;
    StringGrid1: TStringGrid;
    PreviewTimer: TTimer;
    x0: TSpinEdit;
    x1: TSpinEdit;
    y0: TSpinEdit;
    y1: TSpinEdit;
    procedure btnMinimizeClick(Sender: TObject);
    procedure btnShowClick(Sender: TObject);
    procedure cbCentralizedChange(Sender: TObject);
    procedure cbChooseGridChange(Sender: TObject);
    procedure cbNox0y0Change(Sender: TObject);
    procedure cbPreviewChange(Sender: TObject);
    procedure cbUseGridChange(Sender: TObject);
    procedure EditingCDone(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure PreviewTimerStartTimer(Sender: TObject);
    procedure PreviewTimerTimer(Sender: TObject);
    procedure seNodesChange(Sender: TObject);
    procedure SpinChange(Sender: TObject);
    procedure SpinClick(Sender: TObject);
procedure btnAddAxisClick(Sender: TObject);
  private

    FCurrentTrial : integer;
    FGrid : TPoints;
    FGridNames : array of string;
    FCentralRect : TRect;
    FCapturing : Boolean;
    FDrawEllipseC : Boolean;
    FBresenhamLine : TPoints;
    FTrialsPerNode : TTrialsPerNode;
    FAngle : String;
    FLine : TPoints;
    FMirroredLine : TPoints;
    FAxis : TAxisList;
    //fullscreen
    FFullScreen : Boolean;
    FOriginalBounds: TRect;
    FOriginalWindowState: TWindowState;
    FScreenBounds: TRect;
    procedure RefreshLine;
    procedure CentralizeStm;
    function GetLine(aBresenhamLine : TPoints) : TPoints;
    function GetMirroredLine(aLine : TPoints) : TPoints;
    function GetAngleFromPoints(p1, p2 : TPoint) : String;
    function GetPointFromAngle (aAngle : float) : TPoint;//standard or user defined distances?
    function GetCentralRect (aLeftBorderSpace, aTopBorderSpace, aRightBorderSpace, aBottomBorderSpace: integer) : TRect;
  public
    procedure ToggleFullScreen;
    property Axis : TAxisList read FAxis;
  end;

var
  FrmBresenhamLine: TBresenhamLineForm;

implementation

{$R *.lfm}

{ TBresenhamLineForm }

procedure TBresenhamLineForm.FormPaint(Sender: TObject);
var
  i, mX, mY:  integer;
  mP : TPoint;
  OldCanvas : TCanvas;

  procedure SaveOldCanvas;
  begin
    OldCanvas.Brush.Color := Canvas.Brush.Color;
    OldCanvas.Brush.Style := Canvas.Brush.Style;

    OldCanvas.Pen.Color := Canvas.Pen.Color;
    OldCanvas.Pen.Style := Canvas.Pen.Style;
    OldCanvas.Pen.Mode := Canvas.Pen.Mode;
    OldCanvas.Pen.Width := Canvas.Pen.Width;
  end;

  procedure LoadOldCanvas;
  begin
    Canvas.Brush.Color := OldCanvas.Brush.Color;
    Canvas.Brush.Style := OldCanvas.Brush.Style;

    Canvas.Pen.Color := OldCanvas.Pen.Color;
    Canvas.Pen.Style := OldCanvas.Pen.Style;
    Canvas.Pen.Mode := OldCanvas.Pen.Mode;
    Canvas.Pen.Width := OldCanvas.Pen.Width;
  end;
begin
  OldCanvas := TCanvas.Create;
  SaveOldCanvas;

  if cbUseGrid.Checked then
    begin
      Canvas.Pen.Color:= clRed;
      Canvas.Rectangle(FCentralRect);
    end;

  Canvas.Pen.Color := clBlack;

  if FDrawEllipseC then
    begin
     mP := ScreenToClient(Point(Mouse.CursorPos.X, Mouse.CursorPos.Y));
     mX := mP.X;
     mY := mP.Y;

     Canvas.Brush.Color:= clGreen;
     Canvas.Brush.Style:= bsSolid;
     Canvas.Ellipse(mX -5, mY -5, mX +5, mY +5);
     Canvas.Brush.Style:= bsClear;
    end;

  for i := Low(FBresenhamLine) to High(FBresenhamLine) do
    begin
      PlotPixel(Canvas, FBresenhamLine[i], clBlack);
    end;

  Canvas.Font.Color := clBlack;
  for i := Low(FLine) to High(FLine) do
    begin
      mX := FLine[i].X;
      mY := FLine[i].Y;
      Canvas.Ellipse(mX -5, mY -5, mX +5, mY +5);
      Canvas.TextOut(mX, mY -25, IntToStr(FTrialsPerNode[i]))
    end;

  Canvas.TextOut(x0.Value + 10, y0.Value + 10 , FAngle);

  if cbPreview.Checked then
    begin
      DrawCircle(Canvas, FLine[FCurrentTrial].X, FLine[FCurrentTrial].Y, seSize.Value, False, 0, 0);
      DrawCircle(Canvas, FMirroredLine[FCurrentTrial].X, FMirroredLine[FCurrentTrial].Y, seSize.Value, True, 0, 360);
    end;

  LoadOldCanvas;

  case cbChooseGrid.ItemIndex of
    0:{ do nothing };
    1..2:begin

      for i := Low(FGrid) to High(FGrid)do
        begin
          mP := FGrid[i];
          mX := mP.X;
          mY := mP.Y;

          Canvas.Ellipse(mX-10, mY-10, mX +10, mY +10);
          Canvas.TextOut(mX-10, mY-10, FGridNames[i])
      end;
    end;
  end;

  OldCanvas.Free;
end;

procedure TBresenhamLineForm.PreviewTimerStartTimer(Sender: TObject);
begin
  FCurrentTrial := High(Fline);
  Invalidate;
end;

procedure TBresenhamLineForm.PreviewTimerTimer(Sender: TObject);
begin
  if FCurrentTrial > Low(FLine) then
    Dec(FCurrentTrial)
  else FCurrentTrial := High(Fline);
  Invalidate;
end;

procedure TBresenhamLineForm.seNodesChange(Sender: TObject);
begin
  //RefreshLine;
end;

procedure TBresenhamLineForm.SpinChange(Sender: TObject);
begin

end;

procedure TBresenhamLineForm.SpinClick(Sender: TObject);
  var
    Previewing : Boolean;
    i : integer;
begin
  Previewing := cbPreview.Checked;
  cbPreview.Checked := False;

  if (Sender = x0) or (Sender = x1) or (Sender = y0) or (Sender = y1) or (Sender = seNodes) then
    RefreshLine;

  if (Sender = seSize) or (Sender = seBorder) and cbUseGrid.Checked
    and (cbChooseGrid.ItemIndex <> 0) then
      begin
        cbChooseGridChange(Sender);
      end;

  if (Sender = seTrials) or (Sender = seNodes) then
    begin
      SetLength(FTrialsPerNode, Length(FLine));
      for i := Low(FTrialsPerNode) to High(FTrialsPerNode) do
        FTrialsPerNode[i] := seTrials.Value;
    end;

  cbPreview.Checked := Previewing;
  Invalidate;
end;

procedure TBresenhamLineForm.EditingCDone(Sender: TObject);
  var i : integer;
begin
  if (Sender = x0) or (Sender = x1) or (Sender = y0) or (Sender = y1) or (Sender = seNodes) then
    RefreshLine;

  if  (Sender = seSize) or (Sender = seBorder) and cbUseGrid.Checked
    and (cbChooseGrid.ItemIndex <> 0) then
      begin
        cbChooseGridChange(Sender);
      end;

  if (Sender = seTrials) or (Sender = seNodes) then
    begin
      SetLength(FTrialsPerNode, Length(FLine));
      for i := Low(FTrialsPerNode) to High(FTrialsPerNode) do
        FTrialsPerNode[i] := seTrials.Value;
    end;

  Invalidate;
end;

procedure TBresenhamLineForm.RefreshLine;
begin
  FBresenhamLine := BresenhamLine(x0.value, x1.Value, y0.value, y1.value);
  FLine := GetLine(FBresenhamLine);
  if cbCentralized.Checked then CentralizeStm
  else FMirroredLine := GetMirroredLine(FLine);

  FAngle := GetAngleFromPoints(Point(x0.value,y0.value), Point(x1.Value, y1.value));
end;

procedure TBresenhamLineForm.CentralizeStm;
var i, fix : integer;
begin
  fix := seSize.Value div 2;
  for i := Low(FLine) to High(FLine) do
    begin
      FLine[i].X := FLine[i].X - fix;
      FLine[i].Y := FLine[i].Y - fix;
    end;
  FMirroredLine := GetMirroredLine(FLine);
end;

procedure TBresenhamLineForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if key in [#32] then
    begin
      Panel1.Visible := not Panel1.Visible;
      gbAddRmvAxis.Visible := not gbAddRmvAxis.Visible;
    end;
end;

procedure TBresenhamLineForm.FormActivate(Sender: TObject);
var
  aWidth, aHeight,
  i : integer;

begin
  ToggleFullScreen;

  aWidth := Screen.MonitorFromWindow(Handle).Width;
  aHeight := Screen.MonitorFromWindow(Handle).Height;

  x0.value := aWidth div 2;
  y0.value := aHeight div 2;
  RefreshLine;

  SetLength(FTrialsPerNode, Length(FLine));
  for i := Low(FTrialsPerNode) to High(FTrialsPerNode) do
  FTrialsPerNode[i] := seTrials.Value;

  FCentralRect := Rect(0, 0, aWidth, aHeight);
  Canvas.Brush.Style:= bsClear;
end;

procedure TBresenhamLineForm.btnMinimizeClick(Sender: TObject);
begin
  Panel1.Visible := not Panel1.Visible;
  gbAddRmvAxis.Visible := not gbAddRmvAxis.Visible;
end;

procedure TBresenhamLineForm.btnShowClick(Sender: TObject);
begin
  with StringGrid1 do
    if (FAxis.CanRead) and (Cells[0, Row] <> '') then
      begin
        //showmessage(IntToStr(Row));
        FAngle := FAxis.List[Row -1].Angle;
        seSize.value := FAxis.List[Row -1].Size;
        FBresenhamLine := FAxis.List[Row -1].BresenhamLine;
        FLine := FAxis.List[Row -1].Line;
        FMirroredLine := GetMirroredLine(FLine);
        FTrialsPerNode := FAxis.List[Row -1].TrialsPerNode;

        x0.value := FLine[Low(FLine)].X;
        y0.value := FLine[Low(FLine)].Y;
        x1.value := FLine[High(FLine)].X;
        y1.value := FLine[High(FLine)].Y;
      end;
  Invalidate;
end;

procedure TBresenhamLineForm.cbCentralizedChange(Sender: TObject);
begin
  if cbCentralized.Checked then CentralizeStm
  else
    begin
      FLine := GetLine(FBresenhamLine);
      FMirroredLine := GetMirroredLine(FLine);
    end;
  Invalidate;
end;

procedure TBresenhamLineForm.cbChooseGridChange(Sender: TObject);
var
  aWidth, aHeight,
  i: integer;

  aDegree : Float;
begin
  aWidth := Screen.MonitorFromWindow(Handle).Width;
  aHeight := Screen.MonitorFromWindow(Handle).Height;

  i := (seSize.value div 2) + seBorder.Value;
  case cbChooseGrid.ItemIndex of
    0 : FCentralRect := Rect(0, 0, aWidth, aHeight);
    1 : FCentralRect := GetCentralRect(0, 0, 0, 0);
    2 : FCentralRect := GetCentralRect(i, i, i, i);
  end;

  SetLength(FGrid, 360 div 5);
  SetLength(FGridNames, 360 div 5);
  aDegree := 0;
  for i := Low(FGrid) to High (FGrid) do
    begin
      FGridNames[i]:= FloatToStr(aDegree);
      FGrid[i] := GetPointFromAngle(aDegree);
      aDegree := aDegree + 5;
    end;
  RefreshLine;
  Invalidate;
end;

procedure TBresenhamLineForm.cbNox0y0Change(Sender: TObject);
begin
  RefreshLine;
  Invalidate;
end;

procedure TBresenhamLineForm.cbPreviewChange(Sender: TObject);
begin
  PreviewTimer.Enabled:= cbPreview.Checked;
end;

procedure TBresenhamLineForm.cbUseGridChange(Sender: TObject);
begin
  cbChooseGrid.Enabled := not cbChooseGrid.Enabled;
  if not cbChooseGrid.Enabled then cbChooseGrid.ItemIndex := 0;
end;


procedure TBresenhamLineForm.btnAddAxisClick(Sender: TObject);
begin
  FAxis.CanRead := False;
  with StringGrid1 do
    begin
      Row := RowCount;
      SetLength(FAxis.List, RowCount -1);
      with FAxis.List[Row -1] do
        begin
          BresenhamLine := FBresenhamLine;
          Line := FLine;
          MirroredLine := FMirroredLine;
          Angle := FAngle;
          Size := seSize.value;
          Trials := seTrials.Value;
          TrialsPerNode := FTrialsPerNode;
        end;


      if Cells[0, Row] = '' then  //selected row has empty cell on first column
        begin
            case cbChooseGrid.ItemIndex of
              0..2 : begin
                    Cells[0, Row] := FAxis.List[Row -1].Angle;
                  end;
              else
                  begin
                    Cells[0, Row] := 'NA';
                  end;

            end;
          Cells[1, Row] := IntToStr(FAxis.List[Row -1].Trials);
          Cells[2, Row] := IntToStr(Length(FAxis.List[Row -1].Line));
        RowCount := RowCount + 1;
        Row := RowCount;
        end;
    end;
  FAxis.CanRead := True;
end;

procedure TBresenhamLineForm.FormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i : integer;
begin
    if Button = mbLeft then
    begin
      if (x1.value - X)*(x1.value - X) +
         (y1.value - Y)*(y1.value - Y) < 50 then
        begin
          FCapturing := True;
          x1.Value := X;
          Y1.Value := Y;
          RefreshLine;
        end
      else
      if (x0.value - X)*(x0.value - X) +
         (y0.value - Y)*(y0.value - Y) < 50 then
        begin
          FCapturing := True;
          x0.Value := X;
          Y0.Value := Y;
          RefreshLine;
        end;
    for i := Low(FGrid) to High(FGrid) do
      if (FGrid[i].X - X)*(FGrid[i].X - X) +
      (FGrid[i].Y - Y)*(FGrid[i].Y - Y) < 50 then
        begin
          x1.Value := FGrid[i].X;
          y1.Value := FGrid[i].Y;
        end;

    end;
end;

procedure TBresenhamLineForm.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  NeedUpdate : boolean;
begin
  NeedUpdate := False;
  if (x1.value - X)*(x1.value - X) +
     (y1.value - Y)*(y1.value - Y) < 300 then
    begin
      if not FDrawEllipseC then
        begin
          FDrawEllipseC := True;
          Self.Cursor := crDrag;
        end;
    end
  else
    begin
      if FDrawEllipseC then
        begin
          FDrawEllipseC := False;
          Self.Cursor := crDefault;
        end;
    end;

  if FCapturing then
    begin
      X1.Value := X;
      Y1.Value := Y;
      RefreshLine;
      NeedUpdate := True;
    end;

  if NeedUpdate then
    begin
      NeedUpdate := False;
      Invalidate; // need to avoid flickering on windows
    end;
end;

procedure TBresenhamLineForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FCapturing := False;

  if x1.value < FCentralRect.Left then x1.value := FCentralRect.Left;
  if x1.value > FCentralRect.Right then x1.value := FCentralRect.Right;
  if y1.value < FCentralRect.Top then y1.value := FCentralRect.Top;
  if y1.value > FCentralRect.Bottom then y1.value := FCentralRect.Bottom;
  RefreshLine;


  //y0.value
  //y1.value
  Invalidate;
end;

function TBresenhamLineForm.GetLine(aBresenhamLine: TPoints): TPoints;
var
  i, index :  integer; step, aux : float; iaux : TPoint;
begin
  SetLength(Result,seNodes.Value);

  index := High(aBresenhamLine);
  aux := index;
  step := Length(aBresenhamLine) / (seNodes.Value -1);
  for i := High(Result) downto 1 do
    begin
      Result[i] := aBresenhamLine[index];

      aux := aux - step;
      index := Round(aux);
    end;

  Result[Low(Result)] := aBresenhamLine[Low(aBresenhamLine)];

  if cbNox0y0.Checked then
    begin
      for i := Low(Result) to High(Result) - 1 do
        begin
          iaux := Result[i + 1];
          Result[i + 1] := Result[i];
          Result[i] := iaux;
        end;
      SetLength(Result, Length(Result) -1);
    end;
end;

function TBresenhamLineForm.GetMirroredLine(aLine: TPoints): TPoints;
var i, HalfScreenWidth, HalfScreenHeight, Distance : integer;
begin
  SetLength(Result, Length(aLine));
  HalfScreenWidth := Round(Screen.MonitorFromWindow(Handle).Width/2);
  HalfScreenHeight := Round(Screen.MonitorFromWindow(Handle).Height/2);

  if cbCentralized.Checked then
    begin
      HalfScreenWidth := HalfScreenWidth - (seSize.Value div 2);
      HalfScreenHeight := HalfScreenHeight - (seSize.Value div 2);
    end;
  for i := Low(aLine) to High(aLine) do
    begin
     Distance := aLine[i].X;
     if Distance = HalfScreenWidth then Result[i].X := Distance
     else
      if Distance < HalfScreenWidth then Result[i].X := ABS(Distance - HalfScreenWidth) + HalfScreenWidth
      else
       Result[i].X := ABS(ABS(Distance - HalfScreenWidth) - HalfScreenWidth);

     Distance := aLine[i].Y;
     if Distance = HalfScreenHeight then Result[i].Y := Distance
     else
      if Distance < HalfScreenHeight then Result[i].Y := ABS(Distance - HalfScreenHeight) + HalfScreenHeight
        else
         Result[i].Y := ABS(ABS(Distance - HalfScreenHeight) - HalfScreenHeight);
    end;

end;

function TBresenhamLineForm.GetAngleFromPoints(p1, p2: TPoint): String;
  //http://stackoverflow.com/questions/15596217/angle-between-two-vectors
  function AngleOfLine(const P1, P2: TPoint): Double;
  begin
    Result := RadToDeg(ArcTan2((P2.Y - P1.Y),(P2.X - P1.X)));
    if Result < 0 then
      Result := Result + 360;
  end;
begin
  Result := FloatToStrF(AngleOfLine(p1,p2), ffFixed, 8, 2);
end;

function TBresenhamLineForm.GetPointFromAngle(aAngle: float): TPoint;
var Distance : float;
begin
  //http://math.stackexchange.com/questions/143932/calculate-point-given-x-y-angle-and-distance
  //0 on the right, clock increment
  //if aAngle = 0 then
  Distance :=  ((FCentralRect.Right - FCentralRect.Left)/ 2);
  Result.X :=  Round((Distance * cos(DegtoRad(aAngle))) + x0.value);
  Result.Y :=  Round((Distance * sin(DegtoRad(aAngle))) + y0.value);
end;

function TBresenhamLineForm.GetCentralRect(aLeftBorderSpace, aTopBorderSpace,
  aRightBorderSpace, aBottomBorderSpace: integer): TRect;
  var
    aWidth, aHeight,
    side : integer;
begin
  aWidth := Screen.MonitorFromWindow(Handle).Width;
  aHeight := Screen.MonitorFromWindow(Handle).Height;

  if aHeight > aWidth then
    begin
      side := aWidth;
      Result := Rect( (0 + aLeftBorderSpace),
                      (0  + (aHeight div 2) - (side div 2) +  aTopBorderSpace),
                      (side - aRightBorderSpace),
                      (side + (aHeight div 2) - (side div 2) +  - aBottomBorderSpace)
                    );
    end
  else if aHeight < aWidth then
    begin
      side := aHeight;
      Result := Rect( (0 + (aWidth div 2) - (side div 2) + aLeftBorderSpace),
                      (0 + aTopBorderSpace),
                      (side + (aWidth div 2) - (side div 2) - aRightBorderSpace),
                      (side - aBottomBorderSpace)
                    );
    end
  else if aHeight = aWidth then
    begin
      side := aHeight;
      Result := Rect( (0 + aLeftBorderSpace),
                      (0 + aTopBorderSpace),
                      (side - aRightBorderSpace),
                      (side - aBottomBorderSpace)
                    );
    end;
end;


procedure TBresenhamLineForm.ToggleFullScreen;
begin
  { DONE 1 -oRafael -cdev : Update fullscreen behavior }
  //BorderStyle := bsNone;
  WindowState := wsFullScreen;
  BoundsRect := Screen.MonitorFromWindow(Handle).BoundsRect;
end;

end.


unit form_participant;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, DBGrids;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CheckBoxDrawDots: TCheckBox;
    DBGrid1: TDBGrid;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure CheckBoxDrawDotsChange(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses datamodule;

{$R *.lfm}

{ TForm1 }

procedure TForm1.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  OldCanvas : TCanvas;

  procedure SaveOldCanvas;
  begin
    OldCanvas.Brush.Style := StringGrid1.Canvas.Brush.Style;
    OldCanvas.Brush.Color := StringGrid1.Canvas.Brush.Color;
    OldCanvas.Pen.Width := StringGrid1.Canvas.Pen.Width;
    OldCanvas.Pen.Color := StringGrid1.Canvas.Pen.Color;
    OldCanvas.Pen.Mode := StringGrid1.Canvas.Pen.Mode;

  end;

  procedure LoadOldCanvas;
  begin
    StringGrid1.Canvas.Brush.Style := OldCanvas.Brush.Style;
    StringGrid1.Canvas.Brush.Color := OldCanvas.Brush.Color;
    StringGrid1.Canvas.Pen.Width := OldCanvas.Pen.Width;
    StringGrid1.Canvas.Pen.Color := OldCanvas.Pen.Color;
    StringGrid1.Canvas.Pen.Mode := OldCanvas.Pen.Mode;
  end;

  procedure DrawLines(Color : TColor; Selected : Boolean = False);
  var LColor : TColor;

    function HalfDarker(Color : TColor) : TColor;
    begin
      Result := ((Blue(Color) and $7F) shl 16) or ((Green(Color) and $7F) shl 8 ) or (Red(Color) and $7F)
    end;

  begin
    StringGrid1.Canvas.Brush.Style := bsSolid;
    StringGrid1.Canvas.Pen.Width:= 1;
    if Selected then LColor := HalfDarker(Color) else LColor := Color;
    StringGrid1.Canvas.Brush.Color := LColor;
    StringGrid1.Canvas.Pen.Color := LColor;
    StringGrid1.Canvas.Rectangle(aRect);
  end;

  procedure DrawDots;
  var LColor : TColor;
      LFix,
      LLeft,
      LRight,
      LHSize,
      LVSize : longint;
  begin
    LFix := 2;
    LVSize := ((aRect.Bottom - aRect.Top) div 2);
    LHSize := aRect.Left + (aRect.Right - aRect.Left) div 2;
    LLeft := LHSize - LVSize;
    LRight := LHSize + LVSize;

    if gdSelected in aState then
      LColor := clGray
    else
      LColor := clBlack;

    StringGrid1.Canvas.Brush.Style := bsClear;
    StringGrid1.Canvas.Brush.Color := LColor;
    StringGrid1.Canvas.Pen.Width:= 1;
    StringGrid1.Canvas.Pen.Color := LColor;
    StringGrid1.Canvas.Ellipse(LLeft+LFix,aRect.Top+LFix,LRight-LFix,aRect.Bottom-LFix);
  end;

begin
  OldCanvas := TCanvas.Create;
  SaveOldCanvas;
  try
    if (aCol <> 0) and (aRow <> 0) then
    begin
      if (aRow = 1) or (aRow = 8) then DrawLines(clYellow,gdSelected in aState);
      if (aRow = 2) or (aRow = 7) then DrawLines($00FF00,gdSelected in aState);
      if (aRow = 3) or (aRow = 6) then DrawLines(clRed,gdSelected in aState);
      if (aRow = 4) or (aRow = 9) then DrawLines(clBlue,gdSelected in aState);
      if (aRow = 5) or (aRow = 10) then DrawLines($FF00FF,gdSelected in aState);

      if CheckBoxDrawDots.Checked then
        if (Odd(aRow) and Odd(aCol)) or
          (not Odd(aRow) and not Odd(aCol)) then
        DrawDots;

      LoadOldCanvas;
    end;
  finally
    OldCanvas.Free;
  end;
end;

procedure TForm1.CheckBoxDrawDotsChange(Sender: TObject);
begin
  StringGrid1.Invalidate;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Hello world');
end;

end.


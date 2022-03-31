{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Stimuli.Image.Base;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, Graphics, Controls, Schedules;

type

  TImageKind = (ikLetter, ikSquare, ikCircle, ikLetterRect, ikBitmap);

  { TLightImage }

  TLightImage = class(TGraphicControl)
  private
    FOriginalBounds: TRect;
    FFileName: string;
    FBitmap: TBitmap;
    FEdge: TColor;
    FImageKind: TImageKind;
    FOnMouseDown: TMouseEvent;
    FOnResponse: TNotifyEvent;
    FPenWidth: integer;
    function GetRandomPoint : TPoint;
    procedure SetKind(AValue: TImageKind);
    procedure SetOnMouseDown(AValue: TMouseEvent);
    procedure SetOnResponse(AValue: TNotifyEvent);
    procedure SetSchedule(AValue: TSchedule);
  protected
    FSchedule : TSchedule;
    procedure MouseDown(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BoundsAsString : string;
    function ShortName : string;
    procedure DoResponse; virtual;
    procedure LoadFromFile(AFilename : string);
    procedure SetOriginalBounds(aLeft, aTop, aWidth, aHeight: integer);
    procedure SetOriginalSize;
    procedure OriginalBounds;
    procedure Centralize;
    procedure CentralizeLeft;
    procedure CentralizeRight;
    property Schedule : TSchedule read FSchedule write SetSchedule;
    property EdgeColor : TColor read FEdge write FEdge;
    property Kind: TImageKind read FImageKind write SetKind;
    property OnMouseDown : TMouseEvent read FOnMouseDown write SetOnMouseDown;
    property Color;
    property Caption;
    property Font;
  end;

implementation

uses Forms, FileUtil, LazFileUtils, Session.Configuration.GlobalContainer, Dialogs;

{ TLightImage }

procedure TLightImage.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Assigned(FSchedule) then FSchedule.DoResponse;
  if Assigned(OnMouseDown) then OnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TLightImage.SetOnMouseDown(AValue: TMouseEvent);
begin
  if FOnMouseDown=AValue then Exit;
  FOnMouseDown:=AValue;
end;

function TLightImage.GetRandomPoint : TPoint;
begin
  Result.X := BoundsRect.Left + 5 + Random(Width - 5);
  Result.Y := BoundsRect.Top + 5 + Random(Height - 5);
end;

procedure TLightImage.SetKind(AValue: TImageKind);
begin
  if FImageKind=AValue then Exit;
  FImageKind:=AValue
end;

procedure TLightImage.SetOnResponse(AValue: TNotifyEvent);
begin
  if FOnResponse=AValue then Exit;
  FOnResponse:=AValue;
end;

procedure TLightImage.SetSchedule(AValue: TSchedule);
begin
  if FSchedule=AValue then Exit;
  FSchedule:=AValue;
end;

procedure TLightImage.Paint;
var
  LTextStyle : TTextStyle;

  procedure PaintText(Color : TColor);
  begin
    LTextStyle := Canvas.TextStyle;
    LTextStyle.SingleLine:=False;
    LTextStyle.Wordbreak:=True;
    LTextStyle.Clipping:=False;
    LTextStyle.Alignment:=taCenter;
    LTextStyle.Layout:=tlCenter;
    Canvas.TextStyle := LTextStyle;
    with Canvas do
      begin
        Font.Color:= clWhite xor Color;
        Pen.Width := FPenWidth;
        Pen.Color := EdgeColor;
        Brush.Color:= Color;//FBorderColor;
        //FillRect(Rect(0, 0, Width, Height));
        if Caption = '' then Rectangle(ClientRect);
        Rectangle(ClientRect);
        TextRect(ClientRect,
          (((ClientRect.Right-ClientRect.Left) div 2) - (TextWidth(Caption)div 2)),
          (((ClientRect.Bottom-ClientRect.Top) div 2) - (TextHeight(Caption)div 2)),
          Caption);
      end;
  end;

  procedure PaintCircle(Color : TColor);
  var
    LCenter : TPoint;
    LSize : integer;
  begin
    LSize := (Width div 2) - 10;
    LCenter.X := ClientRect.Right - (Width div 2);
    LCenter.Y := ClientRect.Bottom - (Width div 2);

    Canvas.TextStyle := LTextStyle;
    with Canvas do
      begin
        Pen.Width := FPenWidth;
        Pen.Color := Color;
        Brush.Color:= Color;
        with LCenter do
          Ellipse(X - LSize, Y - LSize, X + LSize, Y + LSize);
      end;
  end;

  procedure PaintSquare(Color : TColor);
  begin
    with Canvas do
      begin
        Pen.Width := FPenWidth;
        Pen.Color := Color;
        Brush.Color:= Color;
        Rectangle(ClientRect);
      end;
  end;

  procedure PaintLetterRect(Color : TColor);
  var
    R : TRect;
  begin
    R := Rect(ClientRect.Left,
          ClientRect.Top,
          ClientRect.Right,
          (ClientRect.Bottom - ClientRect.Top) div 2);
    LTextStyle := Canvas.TextStyle;
    LTextStyle.SingleLine:=False;
    LTextStyle.Wordbreak:=True;
    LTextStyle.Clipping:=False;
    LTextStyle.Alignment:=taCenter;
    LTextStyle.Layout:=tlCenter;
    Canvas.TextStyle := LTextStyle;
    Canvas.Font.Size := 15;
    with Canvas do
      begin
        Font.Color:= clWhite xor Color;
        Pen.Width := FPenWidth;
        Pen.Color := EdgeColor;
        Brush.Color := Color; //FBorderColor;

        Rectangle(R);
        TextRect(R,
          (((R.Right-R.Left) div 2) - (TextWidth(Caption)div 2)),
          (((R.Bottom-R.Top) div 2) - (TextHeight(Caption)div 2)),
          Caption);
      end;
  end;

begin
  case FImageKind of
    ikLetterRect : PaintLetterRect(Color);
    ikLetter: PaintText(Color);
    ikSquare: PaintSquare(Color);
    ikCircle: PaintCircle(Color);
    ikBitmap: Canvas.StretchDraw(ClientRect, FBitmap);
  end;
end;

constructor TLightImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPenWidth := 10;
  FImageKind:=ikLetter;
  Visible := False;
  Height:= 200;
  Width:= 300;
  EdgeColor:= clInactiveCaption;
  Canvas.Font.Size := 20;
end;

destructor TLightImage.Destroy;
begin
  if Assigned(FBitmap) then FBitmap.Free;
  inherited Destroy;
end;

function TLightImage.BoundsAsString: string;
begin
  Result :=
    IntToStr(Left) + #32 +
    IntToStr(Top) + #32 +
    IntToStr(Width) + #32 +
    IntToStr(Height);
end;

function TLightImage.ShortName: string;
begin
  if FFilename = '' then
    Result := 'NA'
  else
    Result := ExtractFileNameWithoutExt(ExtractFileNameOnly(FFilename));
end;

procedure TLightImage.LoadFromFile(AFilename: string);
var
  s1 : String;

  procedure Load_PNG;
  var LPNG : TPortableNetworkGraphic;
  begin
    LPNG := TPortableNetworkGraphic.Create;
    LPNG.LoadFromFile(FFileName);
    Width := LPNG.Width;
    Height := LPNG.Height;
    FBitmap.Assign(LPNG);
    FBitmap.Transparent:=True;
    FBitmap.TransparentColor:=clFuchsia;
    LPNG.Free;
  end;

  procedure Load_JPG;
  var LJPG : TJPEGImage;
  begin
    LJPG := TJPEGImage.Create;
    LJPG.LoadFromFile(FFileName);
    FBitmap.Assign(LJPG);
    LJPG.Free;
  end;

begin
  if FFileName = AFilename then Exit;

  if FileExists(AFilename) then
    FFileName := AFilename
  else
    FFileName := GlobalContainer.RootMedia + AFilename;

  if not FileExists(FFileName) then
  begin
    FFilename := '';
    Exit;
  end;

  if Assigned(FBitmap) then
    FBitmap.Free;
  FBitmap := TBitmap.Create;
  with FBitmap do
    begin
      Width := Self.Width;
      Height := Self.Height;
      Canvas.Brush.Color := Self.Color;
      Canvas.Rectangle(ClientRect);
    end;
  s1:= UpperCase(ExtractFileExt(FFileName));
  case s1 of
    // images
    '.BMP' : FBitmap.LoadFromFile(FFileName);
    '.JPG' : Load_JPG;
    '.PNG' : Load_PNG;
  end;
  Invalidate;
  //FImageKind:=ikBitmap;
  //SetOriginalSize;
end;

procedure TLightImage.SetOriginalBounds(aLeft, aTop, aWidth, aHeight: integer);
var
  LRect : TRect;
begin
  LRect.Left := aLeft;
  LRect.Top := aTop;
  LRect.Width := aWidth;
  LRect.Height := aHeight;
  FOriginalBounds := LRect;
  SetBounds(aLeft, aTop, aWidth, aHeight);
end;

procedure TLightImage.DoResponse;
begin
  Mouse.CursorPos := GetRandomPoint;
  MouseDown(mbLeft, [], Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TLightImage.SetOriginalSize;
var
  AspectRatio : Double;
begin
  case FImageKind of
    ikLetterRect, ikLetter, ikSquare, ikCircle : { do nothing };
    ikBitmap:
      begin
        AspectRatio := FBitmap.Width / FBitmap.Height;

        if (FBitmap.Width > Screen.Width) and
           (FBitmap.Height > Screen.Height) then
        begin
           if (FBitmap.Height >= FBitmap.Width) then
             begin
               Height := Screen.Height;
               Width:= Round(Height/AspectRatio);
             end
           else
             begin
               Width := Screen.Width;
               Height := Round(Width/AspectRatio);
             end;
        end;

        if (FBitmap.Width > Screen.Width) and (FBitmap.Height <= Screen.Height) then
        begin
          Width := Screen.Width;
          Height := Round(Width/AspectRatio);
        end;

        if (FBitmap.Height > Screen.Height) and (FBitmap.Width <= Screen.Width)  then
        begin
          Height := Screen.Height;
          Width:= Round(Height/AspectRatio);
        end;

        if (FBitmap.Height <= Screen.Height) and
           (FBitmap.Width <= Screen.Width) then
        begin
          Width := FBitmap.Width;
          Height := FBitmap.Height;
        end;
      end;
  end;
end;

procedure TLightImage.OriginalBounds;
begin
  SetBounds(
    FOriginalBounds.Left,
    FOriginalBounds.Top,
    FOriginalBounds.Width,
    FOriginalBounds.Height);
end;

procedure TLightImage.Centralize;
begin
  Left := (Screen.Width  div 2) - (Width  div 2);
  Top  := (Screen.Height div 2) - (Height div 2);
end;

procedure TLightImage.CentralizeLeft;
begin
  Left := (Screen.Width  div 4) - (Width  div 2);
  Top  := (Screen.Height div 2) - (Height div 2);
end;

procedure TLightImage.CentralizeRight;
begin
  Left := Screen.Width - (Screen.Width  div 4) - (Width  div 2);
  Top  := Screen.Height - (Screen.Height div 2) - (Height div 2);
end;

end.

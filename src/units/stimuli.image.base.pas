{
  Stimulus Control
  Copyright (C) 2014-2019 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Stimuli.Image.Base;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, Graphics, Controls, Schedules;

type

  TImageKind = (ikNone, ikBitmap);

  { TLightImage }

  TLightImage = class(TGraphicControl)
  private
    FSchedule : TSchedule;
    FFileName: string;
    FBitmap: TBitmap;
    FEdge: TColor;
    FImageKind: TImageKind;
    FOnMouseDown: TMouseEvent;
    FOnResponse: TNotifyEvent;
    FPenWidth: integer;
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetOnMouseDown(AValue: TMouseEvent);
    procedure SetOnResponse(AValue: TNotifyEvent);
    procedure SetSchedule(AValue: TSchedule);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BoundsAsString : string;
    function ShortName : string;
    procedure LoadFromFile(AFilename : string);
    procedure SetOriginalSize;
    procedure Centralize;
    property Schedule : TSchedule read FSchedule write SetSchedule;
    property EdgeColor : TColor read FEdge write FEdge;
    property Kind: TImageKind read FImageKind;
    property OnMouseDown : TMouseEvent read FOnMouseDown write SetOnMouseDown;
    property Color;
    property Caption;
    property Font;
  end;

implementation

uses Forms, FileUtil, LazFileUtils;

{ TLightImage }

procedure TLightImage.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMouseDown) then
  begin
    if Assigned(FSchedule) then FSchedule.DoResponse;
    OnMouseDown(Sender,Button, Shift, X, Y);
  end;
end;

procedure TLightImage.SetOnMouseDown(AValue: TMouseEvent);
begin
  if FOnMouseDown=AValue then Exit;
  FOnMouseDown:=AValue;
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
  procedure PaintKey(Color : TColor);
  begin
    with Canvas do
      begin
        Font.Size:= 48;
        Font.Color:= clWhite xor Color;
        Pen.Width := FPenWidth;
        Pen.Color := EdgeColor;
        Brush.Color:= Color;//FBorderColor;
        //FillRect(Rect(0, 0, Width, Height));
        Rectangle(ClientRect);
        TextRect(ClientRect,
          (((ClientRect.Right-ClientRect.Left) div 2) - (TextWidth(Caption)div 2)),
          (((ClientRect.Bottom-ClientRect.Top) div 2) - (TextHeight(Caption)div 2)),
          Caption);
      end;
  end;
begin
  case FImageKind of
    ikNone:   PaintKey(Color);
    ikBitmap: Canvas.StretchDraw(ClientRect, FBitmap);
  end;
end;

constructor TLightImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageKind:=ikNone;
  Visible := False;
  Height:= 45;
  Width:= 45;
  EdgeColor:= clInactiveCaption;
  OnMouseDown := @ImageMouseDown;
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
    LPNG.LoadFromFile(AFilename);
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
    LJPG.LoadFromFile(AFilename);
    FBitmap.Assign(LJPG);
    LJPG.Free;
  end;

begin
  if FFileName = AFilename then Exit;
  FFileName := '';
  if FileExists(AFilename) then
    begin
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
      s1:= UpperCase(ExtractFileExt(AFilename));
      case s1 of
        // images
        '.BMP' : FBitmap.LoadFromFile(AFilename);
        '.JPG' : Load_JPG;
        '.PNG' : Load_PNG;
      end;
      FFileName := AFilename;
      Invalidate;
      FImageKind:=ikBitmap;
      SetOriginalSize;
    end;
end;

procedure TLightImage.SetOriginalSize;
var
  AspectRatio : Double;
begin
  case FImageKind of
    ikNone: { Do nothing };
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

procedure TLightImage.Centralize;
begin
  Left := (Screen.Width  div 2) - (Width  div 2);
  Top  := (Screen.Height div 2) - (Height div 2);
end;

end.

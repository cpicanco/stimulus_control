{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit background;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ExtCtrls, Forms, Controls, Graphics, Dialogs
  , draw_methods
  ;

type

  { TBackground }

  TBackground = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FBitmap : TBitmap;
    FBorder : integer;
    FDrawMask: Boolean;
    FDynamicMask: Boolean;
    FFullScreen : Boolean;
    FTimer : TTimer;
    //FOriginalBounds: TRect;
    //FOriginalWindowState: TWindowState;
    //FScreenBounds: TRect;
    procedure UpdateMask(Sender: TObject); overload;
    procedure SetBorder(AValue: integer);
    procedure SetDrawMask(AValue: Boolean);
    procedure SetDynamicMask(AValue: Boolean);
    //procedure UpdateMask(Sender: TObject);
  published
    procedure Paint;override;
  public
    procedure UpdateMask; overload;
    procedure SetFullScreen(TurnOn : Boolean);
    property DrawMask : Boolean read FDrawMask write SetDrawMask;
    property DynamicMask : Boolean read FDynamicMask write SetDynamicMask;
    property Border : integer read FBorder write SetBorder;
  end;

var
  FrmBackground: TBackground;

implementation

{$R *.lfm}

{ TBackground }

procedure TBackground.Paint;
begin
  inherited Paint;
  if DrawMask then
    Canvas.StretchDraw(Rect(FBorder,FBorder,Width-FBorder,Height-FBorder),FBitmap);
end;

procedure TBackground.UpdateMask;
begin
  UpdateMask(Self);
end;

procedure TBackground.FormCreate(Sender: TObject);
begin
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 100;
  FTimer.OnTimer := @UpdateMask;
  FBitmap := TBitmap.Create;
  RandomMask(FBitmap,0,0);
end;

procedure TBackground.FormDestroy(Sender: TObject);
begin
  FBitmap.Free;
end;

procedure TBackground.SetDrawMask(AValue: Boolean);
begin
  if FDrawMask=AValue then Exit;
  if DynamicMask then
    FTimer.Enabled:=AValue
  else
    FTimer.Enabled:=False;
  FDrawMask:=AValue;
end;

procedure TBackground.SetDynamicMask(AValue: Boolean);
begin
  if FDynamicMask=AValue then Exit;
  FDynamicMask:=AValue;
end;

procedure TBackground.SetBorder(AValue: integer);
begin
  if FBorder=AValue then Exit;
  FBorder:=AValue;
end;

procedure TBackground.UpdateMask(Sender: TObject);
begin
  RandomMask(FBitmap,0,0);
  Invalidate;
end;

procedure TBackground.SetFullScreen(TurnOn: Boolean);
begin
  if TurnOn then
    begin
      //fullscreen true
      {$IFDEF MSWINDOWS}
      BorderStyle:=bsNone;
      WindowState := wsFullScreen;
      {$ENDIF}

      {$IFDEF LINUX}
      WindowState := wsFullScreen;
      {$ENDIF}
    end
  else
    begin
      //fullscreen false
      {$IFDEF MSWINDOWS}
      BorderStyle:=bsDialog;
      WindowState := wsNormal;
      {$ENDIF}

      {$IFDEF LINUX}
      WindowState := wsNormal;
      {$ENDIF}
    end;
  FFullScreen := TurnOn;
end;

end.


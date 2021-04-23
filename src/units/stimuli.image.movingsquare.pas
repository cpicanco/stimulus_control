{
  Stimulus Control
  Copyright (C) 2014-2020 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Stimuli.Image.MovingSquare;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls,  ExtCtrls
  , Stimuli.Image.Base
  , Schedules
  ;

type

  TDirection = (sdTop, sdRight, sdBottom, sdLeft);

  { TMovingSquare }

  TMovingSquare = class sealed(TLightImage)
  private
    FDirection: TDirection;
    FMovementSize: integer;
    FTimer : TTimer;
    FTimerConsequence : TTimer;
    procedure SetDirection(AValue: TDirection);
    procedure Move(Sender : TObject);
    procedure ChangeColor(Sender : TObject);
  protected
    procedure MouseDown(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner : TComponent); override;
    procedure Start;
    procedure Stop;
    property Direction : TDirection read FDirection write SetDirection;
    property MovementSize : integer read FMovementSize write FMovementSize;
  end;

var
  Granularity : Cardinal = 100;
  ScreenInCentimeters : real = 39.624;

implementation

uses Forms;

procedure TMovingSquare.SetDirection(AValue: TDirection);
begin
  if FDirection=AValue then Exit;
  FDirection:=AValue;
end;

constructor TMovingSquare.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := Round(0.8*(Screen.Width/ScreenInCentimeters));
  Height:= Width;
  Color := clGray;
  MovementSize := Round(0.53*(Screen.Width/ScreenInCentimeters));
  Direction := sdBottom;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled:=False;
  FTimer.Interval := Granularity;
  FTimer.OnTimer := @Move;
  FTimerConsequence := TTimer.Create(Self);
  FTimerConsequence.Enabled:=False;
  FTimerConsequence.Interval := Granularity;
  FTimerConsequence.OnTimer := @ChangeColor;
end;

procedure TMovingSquare.Move(Sender: TObject);
var
  R : integer;
begin
  R := Random(4);
  case R of
    0: Direction := sdLeft;
    1: Direction := sdTop;
    2: Direction := sdRight;
    3: Direction := sdBottom;
  end;

  case Direction of
    sdLeft: if Left <= 0 then Direction := sdRight;
    sdTop: if Top <= 0 then Direction := sdBottom;
    sdRight: if Left >= (Parent.Width-Width) then Direction := sdLeft;
    sdBottom: if Top >= (Parent.Height-Height) then Direction := sdTop;
  end;

  case Direction of
    sdLeft: Left := Left-MovementSize;
    sdTop: Top := Top-MovementSize;
    sdRight: Left := Left+MovementSize;
    sdBottom: Top := Top+MovementSize;
  end;
end;

procedure TMovingSquare.ChangeColor(Sender: TObject);
begin
  FTimerConsequence.Enabled := False;
  Color := clGray;
end;

procedure TMovingSquare.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  Color := clYellow;
  FTimerConsequence.Enabled := True;
end;

procedure TMovingSquare.Paint;
begin
  with Canvas do
  begin
    Pen.Width := 0;
    Brush.Color:= Color;
    Rectangle(ClientRect);
  end;
end;

procedure TMovingSquare.Start;
begin
  //FTimer.Enabled := True;
end;

procedure TMovingSquare.Stop;
begin
  FTimer.Enabled := False;
end;

end.


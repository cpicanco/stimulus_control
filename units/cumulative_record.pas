{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit cumulative_record;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Graphics, Controls

     , custom_timer
     ;

type

  { TCummulativeRecord }

  TCummulativeRecord = class(TCustomControl)
  private
    FClock : TClockThread;
    FOnBeforeReset: TNotifyEvent;
    FOX : integer;
    FOY : integer;
    FNX : integer;
    FNY : integer;
    FResetEventEnabled: Boolean;
    procedure OnTimer(Sender : TObject);
    procedure SetOnBeforeReset(AValue: TNotifyEvent);
    procedure SetResetEventEnabled(AValue: Boolean);
  public
    constructor Create(AOwner : TWinControl); reintroduce;
    destructor Destroy; override;
    procedure Reset(EventTrigger : Boolean = False);
    procedure Paint; override;
    procedure IncNX(n : integer);
    procedure DecNY(n : integer);
    procedure DrawEvent(OnLeftTop : Boolean);
    property OnBeforeReset : TNotifyEvent read FOnBeforeReset write SetOnBeforeReset;
    property ResetEventEnabled : Boolean read FResetEventEnabled write SetResetEventEnabled;
  end;

implementation

{ TCummulativeRecord }

constructor TCummulativeRecord.Create(AOwner: TWinControl);
begin
  inherited Create(AOwner);
  FResetEventEnabled := False;
  FClock := TClockThread.Create(True);
  FClock.OnTimer := @OnTimer;
  FClock.Interval := 125;

  Parent:= AOwner;
  Align := alClient;
  Color := clWhite;
  Canvas.Pen.Color := clWhite;
  Canvas.Pen.Style := psDot;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Mode := pmXor;

  Reset;
  BringToFront;

  FClock.Start;
end;

destructor TCummulativeRecord.Destroy;
begin
  FClock.FreeOnTerminate := False;
  FClock.Enabled := False;
  FClock.Terminate;
  FClock.Free;
  inherited Destroy;
end;


procedure TCummulativeRecord.Reset(EventTrigger : Boolean = False);
begin
  if EventTrigger then
    if Assigned(OnBeforeReset) then OnBeforeReset(Self);

  FOX := 2;
  FOY := Height - 10;
  Canvas.MoveTo(FOX, FOY);

  FNX := 2;
  FNY := Height -10;
  Invalidate;
end;

procedure TCummulativeRecord.Paint;
begin
  Canvas.LineTo(FNX, FNY);
end;

procedure TCummulativeRecord.IncNX(n: integer);
begin
  Inc(FNX, n);
  Paint;
end;

procedure TCummulativeRecord.DecNY(n: integer);
begin
  Dec(FNY, n);
  Paint;
end;

procedure TCummulativeRecord.OnTimer(Sender: TObject);
begin
  IncNX(1);
  if FNX > (Width -2) then
    begin
      FNX := 2;
      Canvas.MoveTo(FNX, FNY);
      Reset(FResetEventEnabled);
    end;

  if FNY < 2 then
    begin
      FNY := Height - 10;
      Canvas.MoveTo(FNX, FNY);
    end;
  Paint;
end;

procedure TCummulativeRecord.SetOnBeforeReset(AValue: TNotifyEvent);
begin
  if FOnBeforeReset=AValue then Exit;
  FOnBeforeReset := AValue;
end;

procedure TCummulativeRecord.SetResetEventEnabled(AValue: Boolean);
begin
  if FResetEventEnabled=AValue then Exit;
  FResetEventEnabled := AValue;
end;

procedure TCummulativeRecord.DrawEvent(OnLeftTop : Boolean);
begin
  if OnLeftTop then
    begin
      Canvas.MoveTo(Canvas.PenPos.X - 6, Canvas.PenPos.Y - 6);
      Canvas.LineTo(Canvas.PenPos.X + 6, Canvas.PenPos.Y + 6);
    end
  else
    begin
      Canvas.MoveTo(Canvas.PenPos.X + 6, Canvas.PenPos.Y + 6);
      Canvas.LineTo(Canvas.PenPos.X - 6, Canvas.PenPos.Y - 6);
    end;
end;

end.


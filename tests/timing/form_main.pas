{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs
  , custom_timer
  , draw_methods
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
  private
    FClock : TClockThread;
    FCanDraw : Boolean;
    procedure OnClockTimer(Sender : TObject);
    procedure OnClockTerminate(Sender: TObject);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormActivate(Sender: TObject);
begin
  FCanDraw := False;
  FClock := TClockThread.Create(True);
  FClock.Interval := 1000;
  FClock.OnTimer := @OnClockTimer;
  FClock.OnTerminate := @OnClockTerminate;
  FClock.Start;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if (Key = 32) then
     begin
  	   FCanDraw := not FCanDraw;
       Invalidate;
     end;
end;

procedure TForm1.FormPaint(Sender: TObject);
var i : integer;
begin
  if FCanDraw then
     begin
       for i := 0 to 5 do
     		 DrawCircle(Canvas, 10 + (122*i), 10, 120, True, 90, 2);

       for i := 0 to 5 do
     		 DrawCircle(Canvas, 10 + (122*i), 130, 120, True, 90, 2);

       for i := 0 to 5 do
     		 DrawCircle(Canvas, 10 + (122*i), 250, 120, True, 90, 2);

     end;
end;

procedure TForm1.OnClockTimer(Sender: TObject);
begin
  FCanDraw := not FCanDraw;
  Invalidate;
end;

procedure TForm1.OnClockTerminate(Sender: TObject);
begin
 //
end;

end.


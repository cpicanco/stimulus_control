unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, Graphics, Dialogs
  , Canvas.Helpers
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FBitmap : TBitmap;
    FTimer : TTimer;
    procedure RepaintRandomMask(Sender: TObject);
  published
    procedure Paint; override;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

const
  MIN_INTENSITY : byte = 80;
  MAX_INTENSITY : byte = 180;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  FBitmap := TBitmap.Create;
  RandomMask(FBitmap,MIN_INTENSITY,MAX_INTENSITY);
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 75;
  FTimer.OnTimer:=@RepaintRandomMask;
  FTimer.Enabled:=True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FBitmap.Free;
end;

procedure TForm1.RepaintRandomMask(Sender: TObject);
begin
  RandomMask(FBitmap,MIN_INTENSITY,MAX_INTENSITY);
  Invalidate;
end;

procedure TForm1.Paint;
var
  LTick: QWord;
begin
  LTick := GetTickCount64;
  inherited Paint;
  Canvas.StretchDraw(Rect(0,0,Width,Height),FBitmap);
  WriteLn(GetTickCount64- LTick);
end;

end.


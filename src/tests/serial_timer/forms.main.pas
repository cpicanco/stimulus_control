unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Audio.CastleSound;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FShowingImage : Boolean;
    Timer : TTimer;
    Sound1 : TSound;
    Sound2 : TSound;
    procedure PlaySound1(Sender : TObject);
    procedure PlaySound2(Sender : TObject);
    procedure HideImage(Sender : TObject);
  public

  end;

var
  Form1: TForm1;

implementation

uses Session.CastleUtils;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormActivate(Sender: TObject);
begin
  SessionUpdater.Start;
  Timer.Enabled := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FShowingImage := False;
  Sound1 := TSound.Create(Self);
  Sound1.LoadFromFile('tone.wav');
  Sound1.OnStop := @PlaySound2;

  Sound2 := TSound.Create(Self);
  Sound2.LoadFromFile('laught.wav');
  Sound2.OnStop := @HideImage;

  Timer := TTimer.Create(Self);
  Timer.Interval := 2000;
  Timer.OnTimer := @PlaySound1;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  if FShowingImage then begin
    Canvas.Brush.Color := clPurple;
    Canvas.Rectangle(20, 20, 100, 100);
  end;
end;

procedure TForm1.PlaySound1(Sender: TObject);
begin
  Timer.Enabled := False;
  Sound1.Play;
end;

procedure TForm1.PlaySound2(Sender: TObject);
begin
  Sound2.Play;
  FShowingImage := True;
  Invalidate;
end;

procedure TForm1.HideImage(Sender: TObject);
begin
  { Should happen, what am I doing wrong?? }
  FShowingImage := False;
  Invalidate;
end;

end.


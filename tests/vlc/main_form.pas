unit main_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Controls.Stimuli.Key
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    FKey : TKey;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

const
  CFILE = '../../Participante1/Media/V1.mp4';

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  FKey.Play;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FKey := TKey.Create(Self);
  FKey.Width := 100;
  FKey.Height:= 300;
  FKey.Parent := Self;
  FKey.Filename := CFILE;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  //Timer1.Enabled:=False;
  //LCLVLCPlayer1.Pause;
  //Timer1.OnTimer:=@Timer2Timer;
  //Timer1.Interval:= 2000;
  //Timer1.Enabled:=True;
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
  //Timer1.Enabled:=False;
  //LCLVLCPlayer1.Resume;
  //Timer1.OnTimer:=@Timer1Timer;
  //Timer1.Interval:= 2000;
  //Timer1.Enabled:=True;
end;


end.


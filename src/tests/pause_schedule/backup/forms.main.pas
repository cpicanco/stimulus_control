unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonCreate : TButton;
    ButtonPause : TButton;
    ButtonRestart : TButton;
    Response : TButton;
    ListBox1 : TListBox;
    Timer2 : TTimer;
    procedure ButtonPauseClick(Sender : TObject);
    procedure ButtonRestartClick(Sender : TObject);
    procedure ResponseClick(Sender : TObject);
    procedure ChangeColor(Sender : TObject);
    procedure ButtonCreateClick(Sender : TObject);
    procedure Timer2Timer(Sender : TObject);
  private

  public

  end;

var
  Form1 : TForm1;

implementation

{$R *.lfm}

uses Schedules, Timestamps;

var
  Schedule : TSchedule;
  Start : Extended;

{ TForm1 }

procedure TForm1.Timer2Timer(Sender : TObject);
begin
  Timer2.Enabled := False;
  Color := clWhite;
end;

procedure TForm1.ChangeColor(Sender : TObject);
begin
  Color := clRed;
  Timer2.Enabled := True;
  ListBox1.Items.Append('-'+TimestampToStr(TickCount - Start));
end;

procedure TForm1.ButtonPauseClick(Sender : TObject);
begin
  ListBox1.Items.Append('+'+TimestampToStr(Schedule.Pause - Start));
end;

procedure TForm1.ButtonRestartClick(Sender : TObject);
begin
  Start := Schedule.Start;
end;

procedure TForm1.ResponseClick(Sender : TObject);
begin
  Schedule.DoResponse;
end;

procedure TForm1.ButtonCreateClick(Sender : TObject);
begin
  Schedule := TSchedule.Create(Self);
  Schedule.OnConsequence := @ChangeColor;
  Schedule.Load(FT, 10000);
  Start := Schedule.Start;
  ListBox1.Items.Append('0');
end;

end.


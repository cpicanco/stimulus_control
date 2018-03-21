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
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    FKey : TKey;
    //FKey2 : TKey;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

const

{$IFDEF LINUX}
  CFILE = '../../media/LA.mp4';
{$ENDIF}

{$IFDEF WINDOWS}
  CFILE = '..\..\media\LA.mp4';
{$ENDIF}

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FKey := TKey.Create(Self);
  FKey.Left := 400;
  FKey.Width := 300;
  FKey.Height:= 300;
  FKey.Parent := Self;
  FKey.Filename := CFILE;
  FKey.Schedule.;

  //FKey2 := TKey.Create(Self);
  //FKey2.Width := 300;
  //FKey2.Height:= 300;
  //FKey2.Parent := Self;
  //FKey2.Filename := CFILE;
end;


end.


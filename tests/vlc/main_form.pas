unit main_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Controls.Stimuli.Key, Video.VLC
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FKey : TKey;
    FVlc : TVLCVideoPlayer;
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
  FKey := TKey.Create(Form1);
  FKey.Schedule.Load('FR 2');
  //FKey.Schedule.Start;
  FKey.Left := 400;
  FKey.Width := 300;
  FKey.Height:= 300;
  FKey.Parent := Self;
  FKey.Filename := CFILE;
  Fkey.Schedule.Start;


  //FKey2 := TKey.Create(Self);
  //FKey2.Width := 300;
  //FKey2.Height:= 300;
  //FKey2.Parent := Self;
  //FKey2.Filename := CFILE;
end;


end.


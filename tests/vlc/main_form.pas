unit main_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Video.VLC
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
    FPlayer : TVLCVideoPlayer;
    procedure LCLVLCPlayerBackward(Sender: TObject);
    procedure LCLVLCPlayerBuffering(Sender: TObject);
    procedure LCLVLCPlayerEOF(Sender: TObject);
    procedure LCLVLCPlayerError(Sender: TObject; const AError: string);
    procedure LCLVLCPlayerForward(Sender: TObject);
    procedure LCLVLCPlayerLengthChanged(Sender: TObject; const time: TDateTime);
    procedure LCLVLCPlayerMediaChanged(Sender: TObject);
    procedure LCLVLCPlayerNothingSpecial(Sender: TObject);
    procedure LCLVLCPlayerOpening(Sender: TObject);
    procedure LCLVLCPlayerPausableChanged(Sender: TObject;const AValue: Boolean);
    procedure LCLVLCPlayerPause(Sender: TObject);
    procedure LCLVLCPlayerPlaying(Sender: TObject);
    procedure LCLVLCPlayerPositionChanged(Sender: TObject; const APos: Double);
    procedure LCLVLCPlayerSeekableChanged(Sender: TObject;const AValue: Boolean);
    procedure LCLVLCPlayerSnapshot(Sender: TObject; const AfileName: string);
    procedure LCLVLCPlayerStop(Sender: TObject);
    procedure LCLVLCPlayerTimeChanged(Sender: TObject; const time: TDateTime);
    procedure LCLVLCPlayerTitleChanged(Sender: TObject; const ATitle: Integer);
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
  FPlayer.Play;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FPlayer := TVLCVideoPlayer.Create(Self);
  FPlayer.LoadFromFile(CFILE);
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

procedure TForm1.LCLVLCPlayerBackward(Sender: TObject);
begin
  //WriteLn(Sender.ClassName,#32,'Backward');
end;

procedure TForm1.LCLVLCPlayerBuffering(Sender: TObject);
begin
  //WriteLn(Sender.ClassName,#32,'Buffering');
end;

procedure TForm1.LCLVLCPlayerEOF(Sender: TObject);
begin
  //WriteLn(Sender.ClassName,#32,'End of File');
end;

procedure TForm1.LCLVLCPlayerError(Sender: TObject; const AError: string);
begin
  //WriteLn(Sender.ClassName,#32,'Error', #32,AError);
end;

procedure TForm1.LCLVLCPlayerForward(Sender: TObject);
begin
  //WriteLn(Sender.ClassName,#32,'Forward');
end;

procedure TForm1.LCLVLCPlayerLengthChanged(Sender: TObject;const time: TDateTime);
begin
  //WriteLn(Sender.ClassName,#32,'LengthChanged', #32,time);
end;

procedure TForm1.LCLVLCPlayerMediaChanged(Sender: TObject);
begin
  //WriteLn(Sender.ClassName,#32,'MediaChanged');
end;

procedure TForm1.LCLVLCPlayerNothingSpecial(Sender: TObject);
begin
  //WriteLn(Sender.ClassName,#32,'NothingSpecial');
end;

procedure TForm1.LCLVLCPlayerOpening(Sender: TObject);
begin
  //WriteLn(Sender.ClassName,#32,'Opening');
end;

procedure TForm1.LCLVLCPlayerPausableChanged(Sender: TObject;const AValue: Boolean);
begin
  //WriteLn(Sender.ClassName,#32,'PausableChanged', #32,AValue);
end;

procedure TForm1.LCLVLCPlayerPause(Sender: TObject);
begin
  //WriteLn(Sender.ClassName,#32,'Pause');
end;

procedure TForm1.LCLVLCPlayerPlaying(Sender: TObject);
var
  LVideoDuration : LongInt;
begin
  //// get time in miliseconds;
  //LVideoDuration := DateTimeToTimeStamp(LCLVLCPlayer1.VideoDuration).Time;
  //
  //// set timer interval to one third of the video duration time
  //Timer1.Interval:= LVideoDuration div 3;
  //
  //// start timer
  //Timer1.Enabled := True;
end;

procedure TForm1.LCLVLCPlayerPositionChanged(Sender: TObject; const APos: Double
  );
begin
  //WriteLn(Sender.ClassName,#32,'PositionChanged', #32,APos);
end;

procedure TForm1.LCLVLCPlayerSeekableChanged(Sender: TObject; const AValue: Boolean);
begin
  //WriteLn(Sender.ClassName,#32,'SeekableChanged', #32,AValue);
end;

procedure TForm1.LCLVLCPlayerSnapshot(Sender: TObject; const AfileName: string);
begin
  //WriteLn(Sender.ClassName,#32,'Snapshot', #32,AfileName);
end;

procedure TForm1.LCLVLCPlayerStop(Sender: TObject);
begin
  //WriteLn(Sender.ClassName,#32,'Stop');
end;

procedure TForm1.LCLVLCPlayerTimeChanged(Sender: TObject; const time: TDateTime);
begin
  //WriteLn(Sender.ClassName,#32,'TimeChanged', #32,time);
end;

procedure TForm1.LCLVLCPlayerTitleChanged(Sender: TObject; const ATitle: Integer);
begin
  //WriteLn(Sender.ClassName,#32,'TitleChanged', #32,ATitle);
end;

end.


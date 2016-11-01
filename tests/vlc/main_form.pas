unit main_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls
  , response_key
  , lclvlc
  , vlc
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FForm : TForm;
    ResponseKey : TKey;
    VLCMediaItem : TVLCMediaItem;
    LCLVLCPlayer1 : TLCLVLCPlayer;
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
  CFILE = '/home/rafael/git/stimulus_control/Participante1/Media/V1.avi';

implementation

{$R *.lfm}

{ TForm1 }



procedure TForm1.Button1Click(Sender: TObject);
begin

  LCLVLCPlayer1.Play(VLCMediaItem);
  while not LCLVLCPlayer1.Playing do
    Sleep(10);
  ResponseKey.SetBounds(10, 10, LCLVLCPlayer1.VideoWidth, LCLVLCPlayer1.VideoHeight);

end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  //LCLVLCPlayer1.PlayFile(CFILE); slower
  //LCLVLCPlayer1.Play(VLCMediaItem);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ResponseKey := TKey.Create(Self);
  ResponseKey.SetBounds(10,10,Width -10,300);
  ResponseKey.Parent := Self;
  ResponseKey.Show;

  // hack: LCLWIDGET will fallback to response key handle...
  FForm := TForm.Create(ResponseKey);
  FForm.BorderStyle := bsNone;
  FForm.Color := clBlue;
  FForm.Parent := ResponseKey;
  FForm.Show;

  LCLVLCPlayer1 := TLCLVLCPlayer.Create(FForm);
  LCLVLCPlayer1.FitWindow := True;
  LCLVLCPlayer1.ParentWindow := FForm;
  LCLVLCPlayer1.UseEvents := True;
  with LCLVLCPlayer1 do
    begin
      OnBackward:=@LCLVLCPlayerBackward;
      OnBuffering:=@LCLVLCPlayerBuffering;
      OnEOF:=@LCLVLCPlayerEOF;
      OnError:=@LCLVLCPlayerError;
      OnForward:=@LCLVLCPlayerForward;
      OnLengthChanged:=@LCLVLCPlayerLengthChanged;
      OnMediaChanged:=@LCLVLCPlayerMediaChanged;
      OnNothingSpecial:=@LCLVLCPlayerNothingSpecial;
      OnOpening:=@LCLVLCPlayerOpening;
      OnPausableChanged:=@LCLVLCPlayerPausableChanged;
      OnPause:=@LCLVLCPlayerPause;
      OnPlaying:=@LCLVLCPlayerPlaying;
      OnPositionChanged:=@LCLVLCPlayerPositionChanged;
      OnSeekableChanged:=@LCLVLCPlayerSeekableChanged;
      OnSnapshot:=@LCLVLCPlayerSnapshot;
      OnStop := @LCLVLCPlayerStop;
      OnTimeChanged:=@LCLVLCPlayerTimeChanged;
      OnTitleChanged:=@LCLVLCPlayerTitleChanged;
    end;

  VLCMediaItem := TVLCMediaItem.Create(nil);
  VLCMediaItem.Path := CFILE;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  VLCMediaItem.Free;
end;

procedure TForm1.LCLVLCPlayerBackward(Sender: TObject);
begin
  WriteLn(Sender.ClassName,#32,'Backward');
end;

procedure TForm1.LCLVLCPlayerBuffering(Sender: TObject);
begin
  WriteLn(Sender.ClassName,#32,'Buffering');
end;

procedure TForm1.LCLVLCPlayerEOF(Sender: TObject);
begin
  WriteLn(Sender.ClassName,#32,'End of File');
end;

procedure TForm1.LCLVLCPlayerError(Sender: TObject; const AError: string);
begin
  WriteLn(Sender.ClassName,#32,'Error', #32,AError);
end;

procedure TForm1.LCLVLCPlayerForward(Sender: TObject);
begin
  WriteLn(Sender.ClassName,#32,'Forward');
end;

procedure TForm1.LCLVLCPlayerLengthChanged(Sender: TObject;const time: TDateTime);
begin
  WriteLn(Sender.ClassName,#32,'LengthChanged', #32,time);
end;

procedure TForm1.LCLVLCPlayerMediaChanged(Sender: TObject);
begin
  WriteLn(Sender.ClassName,#32,'MediaChanged');
end;

procedure TForm1.LCLVLCPlayerNothingSpecial(Sender: TObject);
begin
  WriteLn(Sender.ClassName,#32,'NothingSpecial');
end;

procedure TForm1.LCLVLCPlayerOpening(Sender: TObject);
begin
  WriteLn(Sender.ClassName,#32,'Opening');
end;

procedure TForm1.LCLVLCPlayerPausableChanged(Sender: TObject;const AValue: Boolean);
begin
  WriteLn(Sender.ClassName,#32,'PausableChanged', #32,AValue);
end;

procedure TForm1.LCLVLCPlayerPause(Sender: TObject);
begin
  WriteLn(Sender.ClassName,#32,'Pause');
end;

procedure TForm1.LCLVLCPlayerPlaying(Sender: TObject);
begin
  WriteLn('SIZE',#32,LCLVLCPlayer1.VideoWidth,#32, LCLVLCPlayer1.VideoHeight);
  WriteLn(Sender.ClassName,#32,'Playing');
end;

procedure TForm1.LCLVLCPlayerPositionChanged(Sender: TObject; const APos: Double
  );
begin
  WriteLn(Sender.ClassName,#32,'PositionChanged', #32,APos);
end;

procedure TForm1.LCLVLCPlayerSeekableChanged(Sender: TObject; const AValue: Boolean);
begin
  WriteLn(Sender.ClassName,#32,'SeekableChanged', #32,AValue);
end;

procedure TForm1.LCLVLCPlayerSnapshot(Sender: TObject; const AfileName: string);
begin
  WriteLn(Sender.ClassName,#32,'Snapshot', #32,AfileName);
end;

procedure TForm1.LCLVLCPlayerStop(Sender: TObject);
begin
  WriteLn(Sender.ClassName,#32,'Stop');
end;

procedure TForm1.LCLVLCPlayerTimeChanged(Sender: TObject; const time: TDateTime);
begin
  WriteLn(Sender.ClassName,#32,'TimeChanged', #32,time);
end;

procedure TForm1.LCLVLCPlayerTitleChanged(Sender: TObject; const ATitle: Integer);
begin
  WriteLn(Sender.ClassName,#32,'TitleChanged', #32,ATitle);
end;

end.


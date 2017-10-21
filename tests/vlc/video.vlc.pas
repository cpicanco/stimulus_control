unit Video.VLC;

{$mode objfpc}{$H+}

interface

uses
  Classes, Video, StdCtrls, Controls, lclvlc, vlc;

type

  { TVLCVideoPlayer }

  TVLCVideoPlayer = class(TComponent, IVideoPlayer)
  private
    FBounds : TCustomControl;
    FHack : TWinControl;
    FPlayer : TLCLVLCPlayer;
    FCurrentVideoItem : TVLCMediaItem;
    FButton : TButton;
    procedure Click(Sender : TObject);
  public
    constructor Create(AWinControl : TWinControl); reintroduce;
    destructor Destroy; override;
    class function Exist : Boolean;
    procedure FullScreen;
    procedure LoadFromFile(AFilename : string);
    procedure Play;
    procedure Stop;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight : integer);
  end;

implementation

uses SysUtils;
{ TVLCVideoPlayer }

procedure TVLCVideoPlayer.Click(Sender: TObject);
begin
  if FPlayer.Playing then
  begin
    Stop;
    FButton.Caption := '>';
  end else
  begin
    Play;
    FButton.Caption := '#';
  end;
end;

constructor TVLCVideoPlayer.Create(AWinControl : TWinControl);
begin
  inherited Create(AWinControl);
  FBounds := TCustomControl.Create(Self);
  FBounds.Width  := 300;
  FBounds.Height := 300;
  FBounds.Left := 490;
  FBounds.Top := 0;
  FBounds.Parent := AWinControl;
  FBounds.Color:=$00000;

  FButton := TButton.Create(Self);
  FButton.Caption:='>';
  FButton.Width:=100;
  FButton.Height:=30;
  FButton.Parent := AWinControl;
  FButton.AnchorSide[akLeft].Side := asrCenter;
  FButton.AnchorSide[akLeft].Control := FBounds;
  FButton.Anchors := FButton.Anchors + [akLeft];
  FButton.AnchorToNeighbour(akTop, 5, FBounds);
  FButton.OnClick:=@Click;

  FHack := TWinControl.Create(Self);
  FHack.Parent := FBounds;

  FPlayer := TLCLVLCPlayer.Create(Self);

  // for some unknown reason we need
  // one more ParentWindow layer on top
  // to have easy control over
  // video top left
  FPlayer.ParentWindow := FHack;

  //FPlayer.UseEvents := True;
  //with FPlayer do
  //begin
  //  OnBackward:=@PlayerBackward;
  //  OnBuffering:=@PlayerBuffering;
  //  OnEOF:=@PlayerEOF;
  //  OnError:=@PlayerError;
  //  OnForward:=@PlayerForward;
  //  OnLengthChanged:=@PlayerLengthChanged;
  //  OnMediaChanged:=@PlayerMediaChanged;
  //  OnNothingSpecial:=@PlayerNothingSpecial;
  //  OnOpening:=@PlayerOpening;
  //  OnPausableChanged:=@PlayerPausableChanged;
  //  OnPause:=@PlayerPause;
  //  OnPlaying:=@PlayerPlaying;
  //  OnPositionChanged:=@PlayerPositionChanged;
  //  OnSeekableChanged:=@PlayerSeekableChanged;
  //  OnSnapshot:=@PlayerSnapshot;
  //  OnStop := @PlayerStop;
  //  OnTimeChanged:=@PlayerTimeChanged;
  //  OnTitleChanged:=@PlayerTitleChanged;
  //end;

  FCurrentVideoItem := TVLCMediaItem.Create(nil);
end;

destructor TVLCVideoPlayer.Destroy;
begin
  inherited Destroy;
  FCurrentVideoItem.Free;
end;

class function TVLCVideoPlayer.Exist: Boolean;
var
  VLC : TVLCLibrary;
begin
  Result := False;
  VLC := VLCLibrary;
  try
    if not VLC.Initialized then VLC.Initialize;
  except
    on E : Exception do exit;
  end;
  Result := VLC.Initialized;
end;

procedure TVLCVideoPlayer.FullScreen;
begin
  FPlayer.FullScreenMode:=True;
end;

procedure TVLCVideoPlayer.LoadFromFile(AFilename: string);
begin
  FCurrentVideoItem.Path := AFilename;
end;

procedure TVLCVideoPlayer.Play;
begin
  FPlayer.Play(FCurrentVideoItem);
end;

procedure TVLCVideoPlayer.Stop;
begin
  FPlayer.Stop;
end;

procedure TVLCVideoPlayer.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  FBounds.SetBounds(ALeft, ATop, AWidth, AHeight);
end;

end.


unit Video.VLC;

{$mode objfpc}{$H+}

interface

uses
  Classes, Video, Forms, lclvlc, vlc;

type

  { TVLCVideoPlayer }

  TVLCVideoPlayer = class(TComponent, IVideoPlayer)
  private
    FPlayer : TLCLVLCPlayer;
    FCurrentVideoItem : TVLCMediaItem;
  public
    constructor Create(AForm : TForm); reintroduce;
    destructor Destroy; override;
    procedure LoadFromFile(AFilename : string);
    procedure Play;
    procedure Stop;
  end;

implementation

{ TVLCVideoPlayer }

constructor TVLCVideoPlayer.Create(AForm: TForm);
begin
  inherited Create(AForm);
  FPlayer := TLCLVLCPlayer.Create(AForm);
  FPlayer.FitWindow := True;
  FPlayer.ParentWindow := AForm;
  FCurrentVideoItem := TVLCMediaItem.Create(nil);
  //FPlayer.UseEvents := True;
  //with LCLVLCPlayer1 do
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
end;

destructor TVLCVideoPlayer.Destroy;
begin
  inherited Destroy;
  FPlayer.Free;
  FCurrentVideoItem.Free;
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

end.


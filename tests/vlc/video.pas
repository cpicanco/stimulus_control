unit Video;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses Controls;

type

  { IVideoPlayer }

  IVideoPlayer = interface
  ['{C0978598-E70C-4E1B-A430-FD94DDE49594}']
    procedure LoadFromFile(AFilename: string);
    procedure Play;
    procedure Stop;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight : integer);
    procedure FullScreen;
  end;

  function VideoPlayer(AWinControl : TWinControl) : IVideoPlayer; inline;

implementation

uses Video.VLC;

function VideoPlayer(AWinControl: TWinControl): IVideoPlayer;
begin
  Result := nil;
  if TVLCVideoPlayer.Exist then
    begin
      Result := TVLCVideoPlayer.Create(AWinControl);
      exit;
    end;
end;


end.


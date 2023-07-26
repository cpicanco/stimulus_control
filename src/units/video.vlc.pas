unit Video.VLC;

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, Controls, lclvlc, vlc, Video.Contract;

type

  { TVLCVideoPlayer }

  TVLCVideoPlayer = class(TComponent, IVideoPlayer)
  private
    FEndOfFileEvent : TNotifyEvent;
    FBackground : TCustomControl;
    {$IFDEF LINUX}
    FHack : TWinControl;
    {$ENDIF}
    FOnClick: TNotifyEvent;
    FPlayer : TLCLVLCPlayer;
    FCurrentVideoItem : TVLCMediaItem;
    //FButton : TButton;
    procedure Click(Sender : TObject);
    procedure EndOfFile(Sender : TObject);
    procedure SetOnClick(AValue: TNotifyEvent);
  public
    constructor Create(AOwner : TComponent; AParent : TWinControl); reintroduce;
    destructor Destroy; override;
    class function Exist : Boolean;
    procedure FullScreen;
    procedure LoadFromFile(AFilename : string);
    procedure Play;
    procedure Stop;
    procedure Hide;
    procedure Show;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight : integer);
    procedure SetEndOfFileEvent(AEndOfFileEvent : TNotifyEvent);
    property OnClick : TNotifyEvent read FOnClick write SetOnClick;
  end;

implementation

uses SysUtils;

{ TVLCVideoPlayer }

procedure TVLCVideoPlayer.Click(Sender: TObject);
begin
  if FPlayer.Playing then Stop else Play;
  if Assigned(OnClick) then OnClick(Sender);
end;

procedure TVLCVideoPlayer.EndOfFile(Sender: TObject);
begin
  //FButton.Caption := '>';
  //FButton.Enabled := True;
  FBackground.Enabled:=True;
  Sleep(500);
  if Assigned(FEndOfFileEvent) then FEndOfFileEvent(Sender);
end;

procedure TVLCVideoPlayer.SetOnClick(AValue: TNotifyEvent);
begin
  if FOnClick=AValue then Exit;
  FOnClick:=AValue;
end;

constructor TVLCVideoPlayer.Create(AOwner: TComponent; AParent: TWinControl);
begin
  inherited Create(AOwner);
  FBackground := TCustomControl.Create(Self);
  FBackground.Width  := 300;
  FBackground.Height := 300;
  FBackground.Left := 490;
  FBackground.Top := 0;
  FBackground.Parent := AParent;
  FBackground.Color:=$00000;
  FBackground.Cursor:=crHandPoint;
  FBackground.OnClick:=@Click;

  //FButton := TButton.Create(Self);
  //FButton.Caption:='>';
  //FButton.Width:=100;
  //FButton.Height:=30;
  //FButton.Parent := AParent;
  //FButton.AnchorSide[akLeft].Side := asrCenter;
  //FButton.AnchorSide[akLeft].Control := FBackground;
  //FButton.Anchors := FButton.Anchors + [akLeft];
  //FButton.AnchorToNeighbour(akTop, 5, FBackground);
  //FButton.OnClick:=@Click;

  FPlayer := TLCLVLCPlayer.Create(Self);
  {$IFDEF LINUX}
  // for some unknown reason we need
  // one more ParentWindow layer on top
  // to have easy control over
  // video top left
  FHack := TWinControl.Create(Self);
  FHack.Parent := FBackground;
  FPlayer.ParentWindow := FHack;
  {$ENDIF}

  {$IFDEF WINDOWS}
  FPlayer.ParentWindow := FBackground;
  {$ENDIF}
  FPlayer.UseEvents := True;
  FPlayer.OnEOF:=@EndOfFile;

  FCurrentVideoItem := TVLCMediaItem.Create(nil);
end;

destructor TVLCVideoPlayer.Destroy;
begin
  FPlayer.ParentWindow := nil;
  FCurrentVideoItem.Free;
  inherited Destroy;
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
  FBackground.Enabled:= False;
  FPlayer.Play(FCurrentVideoItem);
end;

procedure TVLCVideoPlayer.Stop;
begin
  if FPlayer.Playing then
  begin
     FPlayer.Stop;
     FBackground.Enabled:= True;
  end;
end;

procedure TVLCVideoPlayer.Hide;
begin
  if FPlayer.Playing then FPlayer.Stop;
  FBackground.Hide;
end;

procedure TVLCVideoPlayer.Show;
begin
  FBackground.Show;
end;

procedure TVLCVideoPlayer.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  FBackground.SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TVLCVideoPlayer.SetEndOfFileEvent(AEndOfFileEvent: TNotifyEvent);
begin
  if FEndOfFileEvent = AEndOfFileEvent then Exit;
  FEndOfFileEvent := AEndOfFileEvent;
end;

end.


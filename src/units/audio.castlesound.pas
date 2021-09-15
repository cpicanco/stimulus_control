{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Audio.CastleSound;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, CastleSoundEngine;

type

  { TSound }

  TSound = class(TComponent)
  private
    FOnStartPlaying: TNotifyEvent;
    FOnStop : TNotifyEvent;
    FPlayingSound : TCastlePlayingSound;
    procedure Stop(Sender : TObject);
    procedure SetOnStartPlaying(AValue: TNotifyEvent);
    procedure SetOnStop(AValue: TNotifyEvent);
  public
    constructor Create(AOwner : TComponent); override;
    function Playing : Boolean;
    procedure LoadFromFile(AFilename : string);
    procedure Play;
    property OnStartPlaying : TNotifyEvent read FOnStartPlaying write SetOnStartPlaying;
    property OnStop : TNotifyEvent read FOnStop write SetOnStop;
  end;

implementation

uses Session.Configuration.GlobalContainer;

{ TSound }

procedure TSound.Stop(Sender: TObject);
begin
  if Assigned(OnStop) then OnStop(Self);
end;

procedure TSound.SetOnStartPlaying(AValue: TNotifyEvent);
begin
  if FOnStartPlaying = AValue then Exit;
  FOnStartPlaying := AValue;
end;

procedure TSound.SetOnStop(AValue: TNotifyEvent);
begin
  if FOnStop = AValue then Exit;
  FOnStop := AValue;
end;

constructor TSound.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOnStartPlaying := nil;
  FOnStop := nil;
  FPlayingSound := TCastlePlayingSound.Create(Self);
  FPlayingSound.Sound := TCastleSound.Create(Self);
  FPlayingSound.OnStop := @Stop;
end;

function TSound.Playing: Boolean;
begin
  Result := FPlayingSound.Playing;
end;

procedure TSound.LoadFromFile(AFilename: string);
begin
  FPlayingSound.Sound.URL := GlobalContainer.RootMedia+AFilename;
end;

procedure TSound.Play;
begin
  if Assigned(OnStartPlaying) then OnStartPlaying(Self);
  if FPlayingSound.Playing then FPlayingSound.Stop;
  SoundEngine.Play(FPlayingSound);
end;

end.


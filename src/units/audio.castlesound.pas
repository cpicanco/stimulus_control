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
    FAudio : TCastlePlayingSound;
    procedure SetOnStartPlaying(AValue: TNotifyEvent);
    procedure SetOnStop(AValue: TNotifyEvent);
  public
    constructor Create(AOwner : TComponent); override;
    function Duration : Single;
    function Playing : Boolean;
    function ShortName : string;
    procedure LoadFromFile(AFilename : string);
    procedure Play;
    procedure Stop;
    property OnStartPlaying : TNotifyEvent read FOnStartPlaying write SetOnStartPlaying;
    property OnStop : TNotifyEvent read FOnStop write SetOnStop;
  end;

implementation

uses Session.Configuration.GlobalContainer, FileUtil, LazFileUtils;

{ TSound }

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
  FAudio := TCastlePlayingSound.Create(Self);
  FAudio.Sound := TCastleSound.Create(Self);
end;

function TSound.Duration: Single;
begin
  Result := FAudio.Sound.Duration;
end;

function TSound.Playing: Boolean;
begin
  Result := FAudio.Playing;
end;

function TSound.ShortName: string;
begin
  Result := ExtractFileNameWithoutExt(ExtractFileNameOnly(FAudio.Sound.URL));
end;

procedure TSound.LoadFromFile(AFilename: string);
begin
  FAudio.Sound.URL := GlobalContainer.RootMedia+AFilename;
end;

procedure TSound.Play;
begin
  if Assigned(OnStartPlaying) then OnStartPlaying(Self);
  if FAudio.Playing then FAudio.Stop;
  SoundEngine.Play(FAudio);
end;

procedure TSound.Stop;
begin
  FAudio.Stop;
  if Assigned(OnStop) then OnStop(Self);
end;

end.


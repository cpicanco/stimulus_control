{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Consequences.Custom;

{$mode objfpc}{$H+}

// linux dependencies: sudo apt install zlib1g-dev libopenal-dev libvorbis-dev

interface

uses
  Classes, SysUtils, Stimuli.Image, Audio.CastleSound;

type

  { TConsequence }

  TConsequence = class(TComponent)
  private
    FAuditive: TSound;
    FVisual: TStimulusFigure;
    procedure SoundStopped(Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load(AImage, ASound : string);
    procedure Play;
    property Visual : TStimulusFigure read FVisual write FVisual;
    property Auditive : TSound read FAuditive write FAuditive;
  end;

var
  CustomConsequence : TConsequence;

implementation

uses
  Session.Configuration.GlobalContainer;

{ TConsequence }

procedure TConsequence.SoundStopped(Sender: TObject);
begin
  FVisual.Stop;
  FAuditive.Play;
end;

constructor TConsequence.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVisual := TStimulusFigure.Create(Self);
  FAuditive := TSound.Create(Self);
  FAuditive.OnStop := @SoundStopped;
end;

destructor TConsequence.Destroy;
begin
  { do stuff here }
  inherited Destroy;
end;

procedure TConsequence.Load(AImage, ASound: string);
begin
  FVisual.LoadFromFile(AImage);
  FAuditive.LoadFromFile(ASound);
end;

procedure TConsequence.Play;
begin
  FVisual.Start;
  FAuditive.Play;
end;

initialization
  CustomConsequence := TConsequence.Create(nil);
  CustomConsequence.Name := 'CustomConsequence';
  CustomConsequence.Load('imagem-jocosa.png', 'risada-jocosa.wav');

finalization
  CustomConsequence.Free;

end.


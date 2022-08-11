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
    //procedure SoundStopped(Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadImage(AImage : string);
    procedure LoadSound(ASound : string);
    procedure Start;
    procedure Stop;
    property Visual : TStimulusFigure read FVisual write FVisual;
    property Auditive : TSound read FAuditive write FAuditive;
  end;

var
  CustomConsequence : TConsequence;
  CoinSound : TConsequence;

implementation

uses
  Session.Configuration.GlobalContainer;

{ TConsequence }

//procedure TConsequence.SoundStopped(Sender: TObject);
//begin
//  FVisual.Stop;
//  FAuditive.Play;
//end;

constructor TConsequence.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVisual := nil;
  FAuditive := nil;
  //FAuditive.OnStop := @SoundStopped;
end;

destructor TConsequence.Destroy;
begin
  { do stuff here }
  inherited Destroy;
end;

procedure TConsequence.LoadImage(AImage: string);
begin
  FVisual := TStimulusFigure.Create(Self);
  FVisual.LoadFromFile(AImage);
end;

procedure TConsequence.LoadSound(ASound: string);
begin
  FAuditive := TSound.Create(Self);
  FAuditive.LoadFromFile(ASound);
end;

procedure TConsequence.Start;
begin
  if Assigned(FVisual) then begin
    FVisual.Start;
  end;

  if Assigned(FAuditive) then begin
    FAuditive.Play;
  end;
end;

procedure TConsequence.Stop;
begin
  if Assigned(FVisual) then begin
    FVisual.Stop;
  end;

  if Assigned(FAuditive) then begin
    FAuditive.Stop;
  end;
end;

initialization
  CustomConsequence := TConsequence.Create(nil);
  CustomConsequence.Name := 'CustomConsequence';
  CustomConsequence.LoadImage('imagem-jocosa.png');

  CoinSound := TConsequence.Create(nil);
  CoinSound.Name := 'CoinSound';
  CoinSound.LoadSound('som-de-moeda.wav');

finalization
  CustomConsequence.Free;
  CoinSound.Free;

end.


{
  Stimulus Control
  Copyright (C) 2014-2020 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Consequences;

{$mode objfpc}{$H+}

// linux dependencies: sudo apt install zlib1g-dev libopenal-dev libvorbis-dev

interface

uses
  Classes, SysUtils
  , CastleSoundEngine
  , Stimuli.Image
  ;

type

  TConsequence = record
    Visual : TStimulusFigure;
    Auditive : TSoundBuffer;
    //Interval : integer;
  end;

function NextConsequence(AHit : Boolean) : TConsequence;
procedure Play(AConsequence: TConsequence);

implementation

uses
  FileMethods
  , Session.Configuration.GlobalContainer
  ;

const
  ImageFilterHit  = 'acerto*.bmp;acerto*.BMP;acerto*.jpg;acerto*.JPG;acerto*.png;acerto*.PNG;';
  ImageFilterMiss = 'erro*.bmp;erro*.BMP;erro*.jpg;erro*.JPG;erro*.png;erro*.PNG;';
  AudioFilterHit  = 'acerto*.wav;acerto*.WAV;acerto*.ogg;acerto*.OGG;';
  AudioFilterMiss = 'erro*.wav;erro*.WAV;erro*.ogg;erro*.OGG;';
  FolderConsequences = 'consequencias';

var
  ImageFilesHit  : TStringArray;
  ImageFilesMiss : TStringArray;
  AudioFilesHit : TStringArray;
  AudioFilesMiss : TStringArray;
  VisualsHit : array of TStimulusFigure;
  AudiblesHit : array of TSoundBuffer;
  VisualsMiss : array of TStimulusFigure;
  AudiblesMiss : array of TSoundBuffer;

function NextConsequence(AHit : Boolean) : TConsequence;
begin
  if AHit then
    begin
      if Length(VisualsHit) > 0 then
        Result.Visual := VisualsHit[Random(Length(VisualsHit))];

      if Length(AudiblesHit) > 0 then
        Result.Auditive := AudiblesHit[Random(Length(AudiblesHit))];
    end
  else
    begin
      if Length(VisualsMiss) > 0 then
        Result.Visual := VisualsMiss[Random(Length(VisualsMiss))];

      if Length(AudiblesMiss) > 0 then
        Result.Auditive := AudiblesMiss[Random(Length(AudiblesMiss))];
    end;
end;

procedure Play(AConsequence: TConsequence);
begin
  AConsequence.Visual.Start;
  SoundEngine.PlaySound(AConsequence.Auditive);
end;

procedure LoadBuffers(ImagesHit, ImagesMiss,
  AudiosHit, AudiosMiss : TStringArray);
var
  i : integer;
  LParameters : TStringList;
  LKey : string;
begin
  LParameters := TStringList.Create;
  SetLength(VisualsHit, Length(ImagesHit));
  for i := Low(ImagesHit) to High(ImagesHit) do
  begin
    LKey := 'S';
    LParameters.Values[LKey] := ImagesHit[i];
    VisualsHit[i] := TStimulusFigure.Create(nil);
    VisualsHit[i].Key := LKey;
    VisualsHit[i].LoadFromParameters(LParameters);
  end;

  SetLength(VisualsMiss, Length(ImagesMiss));
  for i := Low(ImagesMiss) to High(ImagesMiss) do
  begin
    LKey := 'S';
    LParameters.Values[LKey] := ImagesHit[i];
    VisualsMiss[i] := TStimulusFigure.Create(nil);
    VisualsMiss[i].Key := LKey;
    VisualsMiss[i].LoadFromParameters(LParameters);
  end;

  SetLength(AudiblesHit, Length(AudiosHit));
  for i := Low(AudiosHit) to High(AudiosHit) do
  begin
    AudiblesHit[i] := SoundEngine.LoadBuffer(AudiosHit[i]);
  end;

  SetLength(AudiblesMiss, Length(AudiosMiss));
  for i := Low(AudiosMiss) to High(AudiosMiss) do
  begin
    AudiblesMiss[i] := SoundEngine.LoadBuffer(AudiosMiss[i]);
  end;

  LParameters.Free;
end;

procedure FreeBuffers;
var
  i : integer;
begin
  for i := Low(VisualsHit) to High(VisualsHit) do
    VisualsHit[i].Free;
  for i := Low(VisualsMiss) to High(VisualsMiss) do
    VisualsMiss[i].Free;
end;

initialization
  FindFilesFor(ImageFilesHit,
    GlobalContainer.RootMedia+FolderConsequences, ImageFilterHit);
  FindFilesFor(AudioFilesHit,
    GlobalContainer.RootMedia+FolderConsequences, AudioFilterHit);
  FindFilesFor(ImageFilesMiss,
    GlobalContainer.RootMedia+FolderConsequences, ImageFilterMiss);
  FindFilesFor(AudioFilesMiss,
    GlobalContainer.RootMedia+FolderConsequences, AudioFilterMiss);
  LoadBuffers(ImageFilesHit, ImageFilesMiss,
    AudioFilesHit, AudioFilesMiss);

finalization
  FreeBuffers;

end.


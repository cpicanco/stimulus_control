{
  Stimulus Control
  Copyright (C) 2014-2020 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  IniPropStorage, ComCtrls;

type

  { TBackground }

  TBackground = class(TForm)
    ButtonStartAll: TButton;
    EditParticipant: TEdit;
    IniPropStorage: TIniPropStorage;
    PanelConfigurations: TPanel;
    RadioGroupMode: TRadioGroup;
    ValidateStimuli: TButton;
    procedure ValidateStimuliClick(Sender: TObject);
    procedure ButtonStartAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EndSession(Sender: TObject);
    procedure BeforeStartSession(Sender: TObject);
  private

  public

  end;

var
  Background: TBackground;

implementation

{$R *.lfm}

uses
   FileUtil
   , Session.Backgrounds
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   , SessionSimple
   , Experiments.Eduardo
   , Stimuli.Image.Base
   , FileMethods
   ;

{ TBackground }

var
  LSession : TSession;
  ConfigurationFilename : string;

//procedure TBackground.ButtonStartClick(Sender: TObject);
//begin
//  PanelConfigurations.Hide;
//
//  Session.Backgrounds.Background := Self;
//  case RadioGroupBehaviour.ItemIndex of
//    0 :
//        ConfigurationFilename := Experiments.BeforeAfter.MakeConfigurationFile(
//            RadioGroupMode.ItemIndex, 2000);
//    1 :
//        ConfigurationFilename := Experiments.SameDifferent.MakeConfigurationFile(
//            RadioGroupMode.ItemIndex, 2000);
//    2 :
//        ConfigurationFilename := Experiments.Derivation.MakeConfigurationFile(
//            RadioGroupMode.ItemIndex, 2000);
//  end;
//  LSession.Play(RadioGroupMode.Items[RadioGroupMode.ItemIndex], EditParticipant.Text);
//end;

procedure TBackground.ButtonStartAllClick(Sender: TObject);
begin
  PanelConfigurations.Hide;
  Session.Backgrounds.Background := Self;
  ConfigurationFilename := Experiments.Eduardo.MakeConfigurationFile;
  LSession.Play(RadioGroupMode.Items[RadioGroupMode.ItemIndex], EditParticipant.Text);
end;

procedure TBackground.ValidateStimuliClick(Sender: TObject);
var
  LDirectory : string;
  LStimuli : TStringArray;
  LS : TLightImage;
  i : integer;

begin
  LDirectory := GlobalContainer.RootMedia+Experiments.BeforeAfter.FolderTestBefoAfter;
  FindFilesFor(LStimuli, LDirectory, '*.bmp;*.png;*.jpg');

  LDirectory := GlobalContainer.RootMedia+Experiments.BeforeAfter.FolderPreTrainingBefoAfter;
  AppendFilesTo(LStimuli, LDirectory, '*.bmp;*.png;*.jpg');

  LDirectory := GlobalContainer.RootMedia+Experiments.SameDifferent.FolderTestEqualDiff;
  AppendFilesTo(LStimuli, LDirectory, '*.bmp;*.png;*.jpg');

  LDirectory := GlobalContainer.RootMedia+Experiments.SameDifferent.FolderPreTrainingEqualDiff;
  AppendFilesTo(LStimuli, LDirectory, '*.bmp;*.png;*.jpg');

  LDirectory := GlobalContainer.RootMedia+Experiments.Derivation.FolderDerivationTest;
  AppendFilesTo(LStimuli, LDirectory, '*.bmp;*.png;*.jpg');

  i := Length(LStimuli);

  for i := Low(LStimuli) to High(LStimuli) do
  begin
    LS := TLightImage.Create(nil);
    try
      LS.LoadFromFile(LStimuli[i]);
    except
      on E : exception do
        ShowMessage(LStimuli[i]);
    end;
    LS.Left := 0;
    LS.Top := 0;
    LS.Free;
  end;
end;

procedure TBackground.FormCreate(Sender: TObject);
begin
  LSession := TSession.Create(Self);
  LSession.OnEndSession:=@EndSession;
  LSession.OnBeforeStart:=@BeforeStartSession;
end;

procedure TBackground.EndSession(Sender: TObject);
begin
  ShowMessage('Fim.');
  Close;
end;

procedure TBackground.BeforeStartSession(Sender: TObject);
begin
  CopyFile(ConfigurationFilename, LSession.BaseFilename+'.ini');
end;

end.


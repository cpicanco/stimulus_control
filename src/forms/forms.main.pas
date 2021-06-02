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
  IniPropStorage;

type

  { TBackground }

  TBackground = class(TForm)
    ButtonReplication: TButton;
    ButtonB: TButton;
    ButtonStart: TButton;
    ButtonA: TButton;
    EditParticipant: TEdit;
    IniPropStorage: TIniPropStorage;
    PanelConfigurations: TPanel;
    RadioGroupCondition: TRadioGroup;
    RadioGroupBehaviour: TRadioGroup;
    procedure Button1Click(Sender: TObject);
    procedure ButtonAClick(Sender: TObject);
    procedure ButtonBClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    //procedure InterTrialStop(Sender: TObject);
    procedure EndSession(Sender: TObject);
    procedure BeforeStartSession(Sender: TObject);
  private

  public
    {$IFDEF WINDOWS}
    OriginalBounds: TRect;
    OriginalWindowState: TWindowState;
    ScreenBounds: TRect;
    procedure SwitchFullScreen;
    {$ENDIF}
  end;

var
  Background: TBackground;

implementation

{$R *.lfm}

uses
   FileUtil
   , Session.Backgrounds
   , Session.Configuration.GlobalContainer
   , SessionSimple
   , Experiments.BeforeAfter
   , Experiments.SameDifferent
   , Experiments.Derivation
   , Experiments.SecondDerivation
   , Experiments.Augusto
   , Stimuli.Image.Base
   , FileMethods
   ;

{ TBackground }

var
  LSession : TSession;
  ConfigurationFilename : string;

procedure TBackground.ButtonStartClick(Sender: TObject);
begin
  PanelConfigurations.Hide;
  //if FormCheckStimuli.Visible then
  //  FormCheckStimuli.Hide;

  Session.Backgrounds.Background := Self;
  //{$IFDEF WINDOWS}SwitchFullScreen;{$ENDIF}
  case RadioGroupBehaviour.ItemIndex of
    0 :
        ConfigurationFilename := Experiments.BeforeAfter.MakeConfigurationFile(
            RadioGroupCondition.ItemIndex, 2000);
    1 :
        ConfigurationFilename := Experiments.SameDifferent.MakeConfigurationFile(
            RadioGroupCondition.ItemIndex, 2000);
    2 :
        ConfigurationFilename := Experiments.Derivation.MakeConfigurationFile(
            RadioGroupCondition.ItemIndex, 2000);
    3 :
        ConfigurationFilename := Experiments.SecondDerivation.MakeConfigurationFileHigh(
            RadioGroupCondition.ItemIndex, 2000);
    4 :
        ConfigurationFilename := Experiments.SecondDerivation.MakeConfigurationFileLow(
            RadioGroupCondition.ItemIndex, 2000);
  end;
  LSession.Play(RadioGroupCondition.Items[RadioGroupCondition.ItemIndex], EditParticipant.Text);
end;

procedure TBackground.ButtonAClick(Sender: TObject);
begin
  PanelConfigurations.Hide;
  Session.Backgrounds.Background := Self;
  //{$IFDEF WINDOWS}SwitchFullScreen;{$ENDIF}
  ConfigurationFilename := Experiments.Augusto.MakeSecondConfigurationFileHigh;
  LSession.Play(RadioGroupCondition.Items[RadioGroupCondition.ItemIndex], EditParticipant.Text);
end;

procedure TBackground.ButtonBClick(Sender: TObject);
begin
  PanelConfigurations.Hide;
  Session.Backgrounds.Background := Self;
  //{$IFDEF WINDOWS}SwitchFullScreen;{$ENDIF}
  ConfigurationFilename := Experiments.Augusto.MakeSecondConfigurationFileLow;
  LSession.Play(RadioGroupCondition.Items[RadioGroupCondition.ItemIndex], EditParticipant.Text);
end;

procedure TBackground.Button1Click(Sender: TObject);
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

{$IFDEF WINDOWS}
// http://wiki.freepascal.org/Application_full_screen_mode
procedure TBackground.SwitchFullScreen;
begin
  if BorderStyle <> bsNone then begin
    // To full screen
    OriginalWindowState := WindowState;
    OriginalBounds := BoundsRect;

    BorderStyle := bsNone;
    BoundsRect := Screen.MonitorFromWindow(Handle).BoundsRect;
  end else begin
    // From full screen
    BorderStyle := bsSizeable;
    if OriginalWindowState = wsMaximized then
      WindowState := wsMaximized
    else
      BoundsRect := OriginalBounds;
  end;
end;
{$ENDIF}

end.


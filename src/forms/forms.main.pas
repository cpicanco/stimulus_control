{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, IniPropStorage, ComCtrls, Spin;

type

  { TBackground }

  TBackground = class(TForm)
    ButtonStartAll: TButton;
    CheckBoxCheatsMode : TCheckBox;
    EditParticipant: TEdit;
    FloatSpinEditScreen: TFloatSpinEdit;
    FloatSpinEditSquare: TFloatSpinEdit;
    FloatSpinEditSquareMove: TFloatSpinEdit;
    IniPropStorage: TIniPropStorage;
    LabelLossVT: TLabel;
    LabelGainVI: TLabel;
    LabelScreenSize: TLabel;
    LabelSquareMove: TLabel;
    LabelSquareSize: TLabel;
    LabelTimeSize: TLabel;
    PanelConfigurations: TPanel;
    RadioGroupDesign1: TRadioGroup;
    SpinEditLossVT: TSpinEdit;
    SpinEditGainVI: TSpinEdit;
    SpinEditTimeSize: TSpinEdit;
    procedure ButtonStartAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EndSession(Sender: TObject);
    procedure BeforeStartSession(Sender: TObject);
  private

  public
    //FImage : TStimulusLabeledFigure;
  end;

var
  Background: TBackground;

implementation

{$R *.lfm}

uses
   FileUtil
   , Constants
   , Session.Backgrounds
   , Session.Configuration.GlobalContainer
   , SessionSimple
   , Experiments.Fabiane
   , Stimuli.Image.MovingSquare
   , Controls.Trials.FreeOperantSquare
   , Cheats
   ;

{ TBackground }

var
  GSession : TSession;
  ConfigurationFilename : string;

procedure TBackground.ButtonStartAllClick(Sender: TObject);
var
  LName : string;
  LDesign : string;
begin
  CheatsModeOn := CheckBoxCheatsMode.Checked;
  PanelConfigurations.Hide;
  Session.Backgrounds.Background := Self;
  ScreenInCentimeters := FloatSpinEditScreen.Value;

  LDesign := RadioGroupDesign1.Items[RadioGroupDesign1.ItemIndex];
  SquareSize := FloatSpinEditSquare.Value;
  SquareMovementSize := FloatSpinEditSquareMove.Value;
  Granularity := SpinEditTimeSize.Value;
  GainVI := SpinEditGainVI.Value;
  LossVT := SpinEditLossVT.Value;
  ConfigurationFilename := Experiments.Fabiane.MakeConfigurationFile(LDesign, 0);
  LName := 'Experimento 1'+ #32 + LDesign;
  GSession.Play(LName, EditParticipant.Text);
end;

procedure TBackground.FormCreate(Sender: TObject);
begin
  GSession := TSession.Create(Self);
  GSession.OnEndSession:=@EndSession;
  GSession.OnBeforeStart:=@BeforeStartSession;
end;

procedure TBackground.EndSession(Sender: TObject);
begin
  ShowMessage('Fim.');
  Close;
end;

procedure TBackground.BeforeStartSession(Sender: TObject);
begin
  CopyFile(ConfigurationFilename, GSession.BaseFilename+'.ini');
end;


end.


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
  ExtCtrls, StdCtrls, IniPropStorage, ComCtrls, Spin
  //, Stimuli.Image.Labeled
  ;

type

  { TBackground }

  TBackground = class(TForm)
    ButtonStartAll: TButton;
    CheckBoxCheatsMode : TCheckBox;
    EditParticipant: TEdit;
    FloatSpinEditScreen: TFloatSpinEdit;
    FloatSpinEditSquare: TFloatSpinEdit;
    FloatSpinEditSquareExp3 : TFloatSpinEdit;
    FloatSpinEditSquareMove: TFloatSpinEdit;
    IniPropStorage: TIniPropStorage;
    LabelScreenSize: TLabel;
    LabelSquareMove: TLabel;
    LabelSquareSize: TLabel;
    LabelSquareSizeExp3 : TLabel;
    LabelTimeSize: TLabel;
    PageControl1: TPageControl;
    PanelConfigurations: TPanel;
    RadioGroupDesign1: TRadioGroup;
    RadioGroupDesign2: TRadioGroup;
    RadioGroupDesign3: TRadioGroup;
    RadioGroupDesign4: TRadioGroup;
    SpinEditTimeSize: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
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
   , Experiments.Eduardo
   , Stimuli.Image.MovingSquare
   , Cheats
   ;

{ TBackground }

var
  LSession : TSession;
  ConfigurationFilename : string;

procedure TBackground.ButtonStartAllClick(Sender: TObject);
var
  LName : string;
begin
  CheatsModeOn := CheckBoxCheatsMode.Checked;
  PanelConfigurations.Hide;
  Session.Backgrounds.Background := Self;
  ScreenInCentimeters := FloatSpinEditScreen.Value;
  case PageControl1.TabIndex of
    0:begin
      SquareSize := FloatSpinEditSquare.Value;
      SquareMovementSize := FloatSpinEditSquareMove.Value;
      Granularity := SpinEditTimeSize.Value;
      ConfigurationFilename :=
        Experiments.Eduardo.MakeConfigurationFile(
          RadioGroupDesign1.Items[RadioGroupDesign1.ItemIndex],
          PageControl1.TabIndex);
    end;

    1:begin
      { code }
      ConfigurationFilename :=
        Experiments.Eduardo.MakeConfigurationFile(
          RadioGroupDesign2.Items[RadioGroupDesign2.ItemIndex],
          PageControl1.TabIndex);
    end;

    2:begin
      { code }
      SquareSize := FloatSpinEditSquareExp3.Value;
      ConfigurationFilename :=
        Experiments.Eduardo.MakeConfigurationFile(
          RadioGroupDesign3.Items[RadioGroupDesign3.ItemIndex],
          PageControl1.TabIndex);
    end;

    3:begin
      { code }
      ConfigurationFilename :=
        Experiments.Eduardo.MakeConfigurationFile(
          RadioGroupDesign4.Items[RadioGroupDesign4.ItemIndex],
          PageControl1.TabIndex);

      Exit;
    end;
  end;
  LName := 'Experimento '+(PageControl1.TabIndex+1).ToString;
  LSession.Play(LName, EditParticipant.Text);
end;


procedure TBackground.FormCreate(Sender: TObject);
begin
  LSession := TSession.Create(Self);
  LSession.OnEndSession:=@EndSession;
  LSession.OnBeforeStart:=@BeforeStartSession;

  //FImage := TStimulusLabeledFigure.Create(Self);
  //FImage.Parent := Self;
  //FImage.LoadFromFile('acerto.png');
  //FImage.Start;
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


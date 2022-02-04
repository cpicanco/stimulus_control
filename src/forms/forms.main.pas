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
    FloatSpinEditScreenWidth: TFloatSpinEdit;
    IniPropStorage: TIniPropStorage;
    LabelScreenWidth: TLabel;
    PageControl1: TPageControl;
    PanelConfigurations: TPanel;
    RadioGroupCondition: TRadioGroup;
    RadioGroupDesign1: TRadioGroup;
    RadioGroupDesign2: TRadioGroup;
    SpinEditParticipant: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
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
   , Experiments.Grids
   , Experiments.Vinicius.Comum
   , Experiments.Vinicius
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
  LVisual : Boolean;
  LFirstCondition  : Boolean;
begin
  GlobalContainer.RootData :=
    GlobalContainer.RootData +
    SpinEditParticipant.Value.ToString + '_' +
    EditParticipant.Text +
    DirectorySeparator;
  ForceDirectories(GlobalContainer.RootData);
  CheatsModeOn := CheckBoxCheatsMode.Checked;
  PanelConfigurations.Hide;
  Session.Backgrounds.Background := Self;

  case PageControl1.TabIndex of
    0:begin
      LDesign := RadioGroupDesign1.Items[RadioGroupDesign1.ItemIndex];
    end;

    1:begin
      LDesign := RadioGroupDesign2.Items[RadioGroupDesign2.ItemIndex];
    end;
  end;

  case SpinEditParticipant.Value of
    1, 2, 5, 6, 9, 10, 13, 14 :
      LVisual := False;
    3, 4, 7, 8, 11, 12, 15, 16 :
      LVisual := True;
  end;

  case RadioGroupCondition.ItemIndex of
    0 : begin
      LFirstCondition := True;
    end;
    1 : begin
      LFirstCondition := False;
      LVisual := not LVisual;
    end;
  end;
  ConfigurationFilename :=
    Experiments.Vinicius.MakeConfigurationFile(LDesign, PageControl1.TabIndex,
      LVisual, LFirstCondition, SpinEditParticipant.Value);

  LName := 'Experimento ' + (PageControl1.TabIndex+1).ToString +
           ' Delineamento:' + LDesign;

  GSession.Play(LName, EditParticipant.Text);
end;

//procedure TBackground.Button1Click(Sender: TObject);
//begin
//  PanelConfigurations.Hide;
//  Grid := GetCentralGrid(3, 3.0, False);
//  DoDraw := True;
//end;

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

//procedure TBackground.FormPaint(Sender: TObject);
//var
//  j, i: Integer;
//  R : TRect;
//begin
//  Canvas.Pen.Color := clBlack;
//  if DoDraw then
//  for j := Low(Grid) to High(Grid) do begin
//    for i := Low(Grid[j]) to High(Grid[j]) do begin
//      R := Rect(
//        Grid[j][i].Left,
//        Grid[j][i].Top,
//        Grid[j][i].Left+Grid[j][i].SquareSide,
//        Grid[j][i].Top +Grid[j][i].SquareSide);
//      Canvas.Rectangle(R);
//      Canvas.TextOut(R.Left, R.Top, Grid[j][i].Index.ToString);
//    end;
//  end;
//end;


end.


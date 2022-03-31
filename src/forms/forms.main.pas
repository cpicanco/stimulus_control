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
    Button1: TButton;
    ButtonStartAll: TButton;
    CheckBoxCheatsMode : TCheckBox;
    EditParticipant: TEdit;
    FloatSpinEditScreenWidth: TFloatSpinEdit;
    IniPropStorage: TIniPropStorage;
    LabelScreenWidth: TLabel;
    PageControl1: TPageControl;
    PanelConfigurations: TPanel;
    RadioGroupDesign1: TRadioGroup;
    TabSheet1: TTabSheet;
    procedure FormClick(Sender: TObject);
    procedure SampleDblClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ButtonStartAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EndSession(Sender: TObject);
    procedure BeforeStartSession(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private

  public

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
   , Experiments.Arrasta
   , Stimuli.Image.DragDropable
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
  GlobalContainer.RootData :=
    GlobalContainer.RootData +
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
  end;

  ConfigurationFilename :=
    Experiments.Arrasta.MakeConfigurationFile(LDesign, PageControl1.TabIndex);

  LName := 'Experimento ' + (PageControl1.TabIndex+1).ToString +
           ' Delineamento:' + LDesign;

  GSession.Play(LName, EditParticipant.Text);
end;

var
  LSample : TDragDropableItem;

procedure TBackground.SampleDblClick(Sender: TObject);
begin

end;

procedure TBackground.FormClick(Sender: TObject);
begin
  LSample.OriginalBounds;
  LSample.Color := clWhite;
end;

procedure TBackground.Button1Click(Sender: TObject);
var
  LComparison : TDragDropableItem;
  LComparison1 : TDragDropableItem;
  LComparison2 : TDragDropableItem;
  LComparison3 : TDragDropableItem;
  LComparison4 : TDragDropableItem;
  LComparison5 : TDragDropableItem;
  LComparison6 : TDragDropableItem;
  LComparison7 : TDragDropableItem;
  LComparison8 : TDragDropableItem;
begin
  LComparison := TDragDropableItem.Create(Self);
  LComparison.Caption := 'A';
  LComparison.Parent := Self;
  LComparison.SetOriginalBounds(
    Grid[0, 0].Left, Grid[0, 0].Top, Grid[0, 0].SquareSide, Grid[0, 0].SquareSide);
  LComparison.Show;

  LComparison1 := TDragDropableItem.Create(Self);
  LComparison1.Caption := 'B';
  LComparison1.Parent := Self;
  LComparison1.SetOriginalBounds(
    Grid[0, 1].Left, Grid[0, 1].Top, Grid[0, 1].SquareSide, Grid[0, 1].SquareSide);
  LComparison1.Show;

  LComparison2 := TDragDropableItem.Create(Self);
  LComparison2.Caption := 'B';
  LComparison2.Parent := Self;
  LComparison2.SetOriginalBounds(
    Grid[0, 2].Left, Grid[0, 2].Top, Grid[0, 2].SquareSide, Grid[0, 2].SquareSide);
  LComparison2.Show;


  LComparison3 := TDragDropableItem.Create(Self);
  LComparison3.Caption := 'B';
  LComparison3.Parent := Self;
  LComparison3.SetOriginalBounds(
    Grid[0, 3].Left, Grid[0, 3].Top, Grid[0, 3].SquareSide, Grid[0, 3].SquareSide);
  LComparison3.Show;

    LComparison4 := TDragDropableItem.Create(Self);
  LComparison4.Caption := 'B';
  LComparison4.Parent := Self;
  LComparison4.SetOriginalBounds(
    Grid[0, 4].Left, Grid[0, 4].Top, Grid[0, 4].SquareSide, Grid[0, 4].SquareSide);
  LComparison4.Show;


    LComparison5 := TDragDropableItem.Create(Self);
  LComparison5.Caption := 'B';
  LComparison5.Parent := Self;
  LComparison5.SetOriginalBounds(
    Grid[0, 5].Left, Grid[0, 5].Top, Grid[0, 5].SquareSide, Grid[0, 5].SquareSide);
  LComparison5.Show;

    LComparison6 := TDragDropableItem.Create(Self);
  LComparison6.Caption := 'B';
  LComparison6.Parent := Self;
  LComparison6.SetOriginalBounds(
    Grid[0, 6].Left, Grid[0, 6].Top, Grid[0, 6].SquareSide, Grid[0, 6].SquareSide);
  LComparison6.Show;


    LComparison7 := TDragDropableItem.Create(Self);
  LComparison7.Caption := 'B';
  LComparison7.Parent := Self;
  LComparison7.SetOriginalBounds(
    Grid[0, 7].Left, Grid[0, 7].Top, Grid[0, 7].SquareSide, Grid[0, 7].SquareSide);
  LComparison7.Show;

    LComparison8 := TDragDropableItem.Create(Self);
  LComparison8.Caption := 'B';
  LComparison8.Parent := Self;
  LComparison8.SetOriginalBounds(
    Grid[0, 8].Left, Grid[0, 8].Top, Grid[0, 8].SquareSide, Grid[0, 8].SquareSide);
  LComparison8.Show;


  LSample := TDragDropableItem.Create(Self);
  LSample.Caption := 'A';
  LSample.Parent := Self;
  LSample.SetOriginalBounds(
    Grid[1, 0].Left, Grid[1, 0].Top, Grid[1, 0].SquareSide, Grid[1, 0].SquareSide);
  LSample.Target := LComparison;
  LSample.Show;
  PanelConfigurations.Hide;
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

procedure TBackground.FormPaint(Sender: TObject);
//var
//  j, i: Integer;
//  R : TRect;
begin
  //Canvas.Pen.Color := clBlack;
  //if DoDraw then
  //for j := Low(Grid) to High(Grid) do begin
  //  for i := Low(Grid[j]) to High(Grid[j]) do begin
  //    R := Rect(
  //      Grid[j][i].Left,
  //      Grid[j][i].Top,
  //      Grid[j][i].Left+Grid[j][i].SquareSide,
  //      Grid[j][i].Top +Grid[j][i].SquareSide);
  //    Canvas.Rectangle(R);
  //    Canvas.TextOut(R.Left, R.Top, Grid[j][i].Index.ToString);
  //  end;
  //end;
end;


end.


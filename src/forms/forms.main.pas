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
    RadioGroupDesign: TRadioGroup;
    procedure Button1Click(Sender: TObject);
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
   , SessionSimple
   , Experiments.Eduardo
   , Experiments.Eduardo.Experimento1
   ;

{ TBackground }

var
  LSession : TSession;
  ConfigurationFilename : string;

procedure TBackground.ButtonStartAllClick(Sender: TObject);
begin
  PanelConfigurations.Hide;
  Session.Backgrounds.Background := Self;
  ConfigurationFilename :=
    Experiments.Eduardo.MakeConfigurationFile(
      RadioGroupDesign.Items[RadioGroupDesign.ItemIndex]
    );
  LSession.Play('Experimento 1', EditParticipant.Text);
end;

procedure TBackground.Button1Click(Sender: TObject);
begin
  Experiments.Eduardo.Experimento1.WriteChoices;
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


{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls
  , Audio.Bass_nonfree
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonStop: TButton;
    ButtonPlayLoop: TButton;
    ButtonPlay: TButton;
    procedure ButtonPlayClick(Sender: TObject);
    procedure ButtonPlayLoopClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    F : TBassStream;
    FAudioDevice : TBassAudioDevice;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

const
  CSOUND_PATH = '../../docs/config_examples/Media/CSQ1.wav';

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FAudioDevice := TBassAudioDevice.Create(WindowHandle);
end;

procedure TForm1.ButtonPlayClick(Sender: TObject);
begin
  F := TBassStream.Create(CSOUND_PATH);
  F.Play;
end;

procedure TForm1.ButtonPlayLoopClick(Sender: TObject);
begin
  F := TBassStream.Create(CSOUND_PATH,1);
  F.Play;
end;

procedure TForm1.ButtonStopClick(Sender: TObject);
begin
  F.Stop;
end;

end.


{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit background;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type

  { TBackground }

  TBackground = class(TForm)
  private
    FFullScreen : Boolean;
    //FOriginalBounds: TRect;
    //FOriginalWindowState: TWindowState;
    //FScreenBounds: TRect;
  public
    procedure SetFullScreen(TurnOn : Boolean);
  end;

var
  FrmBackground: TBackground;

implementation

{$R *.lfm}

{ TBackground }

procedure TBackground.SetFullScreen(TurnOn: Boolean);
begin
  if TurnOn then
    begin
      //fullscreen true
      {$IFDEF MSWINDOWS}
      // to do
      {$ENDIF}

      {$IFDEF LINUX}
      WindowState := wsFullScreen;
      {$ENDIF}
    end
  else
    begin
      //fullscreen false
      {$IFDEF MSWINDOWS}
      // to do
      {$ENDIF}

      {$IFDEF LINUX}
      WindowState := wsNormal;
      {$ENDIF}
    end;
  FFullScreen := TurnOn;
end;

end.


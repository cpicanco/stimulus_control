{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
program stimulus_control;

{$mode objfpc}{$H+}

uses

  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
      cthreads
      , cmem
    {$ENDIF}
  //, heaptrc
  ,
  {$ENDIF}
  Interfaces // this includes the LCL widgetset
  , Forms

  //Forms
  , userconfigs
  ;

{$R *.res}

begin
  Application.Initialize;
  {$ifdef DEBUG}
    DebugLn(mt_Information + 'Application Title:' + ApplicationTitle);
    DebugLn(mt_Debug + 'Application ThreadID:' + IntToStr(ThreadID));
  {$endif}
  Application.CreateForm(TFormUserConfig, FormUserConfig);
  Application.Run;
end.


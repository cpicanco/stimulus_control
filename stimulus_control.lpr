{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

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
  {$ENDIF}
  , Interfaces // this includes the LCL widgetset
  , Forms

  //Forms
  , userconfigs
  //, userconfigs_trial_mirrored
  //, userconfigs_get_matrix
  //, userconfigs_simple_discrimination_matrix
  //, background

  //units
  //, client
  //, criatore
  //, escriba
  //, custom_timer
  //, regdata
  //, constants
  //, countermanager

  // Responses, Schedules of Reinforcement, Stimuli
  //, schedules
  //, schedules_main
  //, response_key
  //, bass_player

  // session, blocs, trials
  //, trial_abstract
  //, trial_simple
  //, trial_matching
  //, trial_message

  //, trial_mirrored_stm
  //, trial_feature_positive
  //, trial_calibration
  //, trial_dizzy_timers

  //, blocs
  //, session
  //, config_session

  //PLP, RS232
  //, interface_rs232
  //, interface_plp

  // helpers
  //, draw_methods
  //, timestamps_logger
  //, timestamps_helpers
  //, timestamps
  //, git_vertioning

  {$ifdef DEBUG}
  , debug_logger
  , SysUtils
  , FileUtil, debug_logger_thread
  {$endif}

  ;

{$R *.res}

{$ifdef DEBUG}
  resourcestring
    ApplicationTitle = 'Controle de Estímulos';
{$endif}

begin

  {$ifdef DEBUG}
    DebugLn(mt_Information + 'Debug Logger initialized');
    {$ifdef WINDOWS}
      DebugLn(mt_Information + 'Inp32(0):' + IntToStr(Inp32(0)));
    {$endif}
  {$endif}

  Application.Initialize;
  {$ifdef DEBUG}
    DebugLn(mt_Information + 'Application Title:' + ApplicationTitle);
    DebugLn(mt_Debug + 'Application ThreadID:' + IntToStr(ThreadID));
  {$endif}
  Application.CreateForm(TUserConfig, UserConfig);
  Application.Run;
end.


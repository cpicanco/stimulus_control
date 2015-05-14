//
// Validation Project (PCRF) - Stimulus Control App
// Copyright (C) 2014-2015,  Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.
//
// cpicanco@ufpa.br
//
// This file is part of Validation Project (PCRF).
//
// Validation Project (PCRF) is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Validation Project (PCRF) is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Validation Project (PCRF).  If not, see <http://www.gnu.org/licenses/>.
//
program stimulus_control;

{$mode objfpc}{$H+}

uses

  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
      cthreads ,
      cmem ,
    {$ENDIF}
  //, heaptrc
  {$ENDIF}
  Interfaces // this includes the LCL widgetset
  , Forms

  //Forms
  , userconfigs
  , userconfigs_trial_mirrored
  , userconfigs_get_matrix
  , userconfigs_simple_discrimination_matrix
  , background

  //units
  , client
  , criatore
  , escriba
  , custom_timer
  , regdata
  , constants
  , counter
  , countermanager

  // Responses, Schedules of Reinforcement, Stimuli
  , schedules
  , schedules_main
  , response_key
  , bass_player

  // session, blocs, trials
  , trial_abstract
  , trial_simple
  , trial_matching
  , trial_message
  , trial_mirrored_stm
  
  , trial_feature_positive
  , trial_calibration
  , blocs

  , session
  , config_session

  //PLP, RS232
  , interface_rs232
  , interface_plp

  // helpers
  , draw_methods
  , timestamps_logger, trial_dizzy_timers

  {$ifdef DEBUG}
  , debug_logger
  , SysUtils
  , FileUtil
  {$endif}

  ;

{$R *.res}

resourcestring
  ApplicationTitle = 'Controle de Estímulos';

begin

  {$ifdef DEBUG}
    DebugLn(mt_Information + 'Debug Logger initialized');
    {$ifdef WINDOWS}
      DebugLn(mt_Information + 'Inp32(0):' + IntToStr(Inp32(0)));
    {$endif}
  {$endif}

  Application.Title := ApplicationTitle;
  Application.Initialize;
  {$ifdef DEBUG}
    DebugLn(mt_Information + 'Application Title:' + ApplicationTitle);
    DebugLn(mt_Debug + 'Application ThreadID:' + IntToStr(ThreadID));
  {$endif}
  Application.CreateForm(TUserConfig, UserConfig);
  Application.Run;
end.


//
// Validation Project (PCRF) - Eye Tracking Set Up Validation
// Copyright (C) 2014,  Carlos Rafael Fernandes Picanço, cpicanco@ufpa.br
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
program validation_study;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
      cthreads,
      cmem,
    {$ENDIF}
  //heaptrc,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,

  //Forms
  userconfigs,
  userconfigs_trial_mirrored,
  background,

  //units
  client,
  session_config, custom_timer,
  trial_mirrored_config,
  //simplegui,
  regdata,
  trial_mirrored_stm,
  response_key,
  schedules,
  schedules_abstract,
  counter,
  countermanager,
  blocs,
  session,
  constants,

  //helpers
  draw_methods, trial_simple, trial_matching, trial_message
  ;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TUserConfig, UserConfig);
  Application.Run;
end.


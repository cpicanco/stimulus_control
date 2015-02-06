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
unit constants;

{$mode objfpc}{$H+}

interface

const
  // Posições
  _Positions = 'Positions';
  _Cols = 'Cols';
  _Rows = 'Rows';
  _NumPos = 'NumPos';

  // Main
  _Main = 'Main';
  _Name = 'Name';
  _Subject = 'Subject';
  _Media  = 'Media';
  _Data  = 'Data';
  _RootData   = 'RootData';
  _RootMedia  = 'RootMedia';
  _NumBlc = 'NumBlc';
  _Type = 'Type';

  // Blc
  _NumTrials = 'NumTrials';
  _CrtConsecutiveHit  = 'ConsecutiveHitCriterion';
  _CrtConsecutiveMiss = 'ConsecutiveMissCriterion';
  _CrtMaxTrials = 'MaxTrialCriterion';
  _CsqCriterion = 'CsqCriterion';
  _DefNextBlc = 'DefNextBlc';
  _MaxCorrection = 'MaxCorrection';
  _ITI = 'ITI';

  // Blc Trial
  _BkGnd = 'BkGnd';

  // Trial
  _Cursor = 'Cursor';
  _AutoNxt = 'AutoNxt';
  _CustomNxtValue = 'CustomNxtValue';
  _Kind = 'Kind';

  // MRD
  _UseMedia = 'UseMedia';
  _ShowStarter = 'Starter';
  _Angle = 'Angle';
  _ExpectedResponse = 'Response';
  _Schedule = 'Schedule';
  _NextTrial = 'NextTrial';
  _cGap = 'Gap';                 //boolean
  _cGap_Degree = 'GapDegree';
  _cGap_Length = 'GapLength';
  //some constants are shared with 'SIMPLE' when 'usemedia' is true

  // MSG
  _Msg = 'Msg';
  _MsgWidth = 'MsgWidth';
  _MsgFontSize = 'MsgFontSize';
  _MsgFontColor = 'MsgFontColor';
  _Prompt = 'Prompt';

  // MTS
  _Delayed = 'Delayed';
  _Delay = 'Delay';

  // SIMPLE
  _NumComp = 'NumComp';   // number of stimuli on screen
  _cBnd = 'Bnd';          // top left width height
  _cStm = 'Stm';          // stimulus path
  _cSch = 'Sch';          // reinforcement schedule
  _cMsg = 'Msg';          // message sent to the report {REGDATA}
  _cCsq = 'Csq';          // PLP code,see the unit 'library'
  _cUsb = 'Usb';          // RS232 code, see the unit 'library'
  _cRes = 'Res';          // Kind of the response, hit, miss or none
  _cNxt = 'Nxt';          // Next trial {if = '0', same as '1', if > 0, go to the specified trial}
  _cIET = 'IET';          // Inter Trial Interval, in miliseconds
  _cTO  = 'TO';           // Time-Out Interval, in miliseconds

  // Identificadores
  _Kplus = 'K+';
  _Kminus = 'K-';
  _Pos = 'P';
  _Blc = 'Blc';
  _Trial = 'T';
  _Comp = 'C';
  _Samp = 'S';


const
  { escriba }
  KEnter = #13#10;
  KPositions = '[Positions]' + #13#10;
  KMain = '[Main]' + #13#10;
  KName = 'Name=' + #9;
  KSubject = 'Subject=' + #9;
  KType = 'Type=' + #9;
  KMedia = 'Media=' + #9;
  KRootMedia = 'RootMedia=' + #9;
  KData = 'Data=' + #9;
  KRootData = 'RootData=' + #9;
  KNumBlc = 'NumBlc=' + #9;
  KNumTrial = 'NumTrials=' + #9;
  KMaxCorrection = 'MaxCorrection=' + #9;
  KCrtConsecutiveHit = 'ConsecutiveHitCriterion=' + #9;
  KCrtConsecutiveMiss = 'ConsecutiveMissCriterion=' + #9;
  KCrtMaxTrials = 'MaxTrialCriterion=' + #9;
  KCrtKCsqHit = 'CsqCriterion=' + #9;
  KNumComp = 'NumComp=' + #9;
  KRows = 'Rows=' + #9;
  KCols = 'Cols=' + #9;
  KNumPos = 'NumPos=' + #9;
  KBackGround = 'BkGnd=' + #9;
  KITInterval = 'ITI=' + #9;
  KCursor = 'Cursor=' + #9;
  KKind = 'Kind=' + #9;
  KComAtraso = 'Delayed=' + #9;
  KAtraso = 'Delay=' + #9;
  KAutoNext  = 'AutoNxt=' + #9;
  KCustomNext = 'CustomNxtValue=' + #9;
  KWidth = 'MsgWidth=' + #9;
  KFontSize = 'MsgFontSize=' + #9;
  KFontColor = 'MsgFontColor=' + #9;
  KPrompt = 'Prompt=' + #9;

  KUseMedia = 'UseMedia=' + #9;  //boolean
  KShowStarter = 'Starter=' + #9; //boolean
  KAngle = 'Angle=' + #9;
  KExpectedResponse = 'Response=' + #9;
  KSchedule = 'Schedule=' + #9;
  KNextTrial = 'NextTrial=' + #9;
  KcGap = 'Gap=' + #9;                 //boolean
  KcGap_Degree = 'GapDegree=' + #9;
  KcGap_Length = 'GapLength=' + #9;

  KBnd = 'Bnd=' + #9;
  KStm = 'Stm=' + #9;
  KIET = 'IET=' + #9;
  KSch = 'Sch=' + #9;
  KMsg = 'Msg=' + #9;
  KCsq = 'Csq=' + #9;
  KUsb = 'Usb=' + #9;
  KRes = 'Res=' + #9;
  KNxt = 'Nxt=' + #9;
  KTO = 'TO='  + #9;

  { Session Parameters }
  T_CIC = 'CIC';
  T_CRT = 'CRT';

  { General Parameters }
  T_END = 'END';

  { Trial Kinds }
  T_FPE = 'FPE';
  T_MRD = 'MRD';
  T_MSG = 'MSG';
  T_MTS = 'MTS';
  T_Simple = 'SIMPLE';

  { Schedules }
  T_RR  = 'RR';
  T_FR  = 'FR';
  T_VR = 'VR';
  T_RI = 'RI';
  T_FI = 'FI';
  T_VI  = 'VI';
  T_RT = 'RT';
  T_VT = 'VT';
  T_FT  = 'FT';
  T_CRF = 'CRF';
  T_EXT  = 'EXT';
  T_DRL = 'DRL';
  T_DRH = 'DRH';

  { Res }
  T_HIT = 'HIT';
  T_MISS = 'MISS';
  T_NONE = 'NONE';

  { Nxt }
  T_REP = 'CRT';

const
  //report  messages
  rmKeyPlus = 'Tecla +';
  rmKeyMinus = 'Tecla -';

implementation

end.


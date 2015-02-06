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
unit trial_abstract;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, LMessages, Controls, Classes, SysUtils,
     session_config, countermanager;
type

  { TTrial }

  TTrial = class(TCustomControl)
  protected
    FTimeStart: cardinal;
    FFileName : string;
    FRootMedia: string;
    FIETConsequence: string;
    FTimeOut : Integer;
    FManager : TCounterManager;
    FResult: String;
    FHeader: String;
    FHeaderTicks: String;
    FData: String;
    FDataTicks: String;
    FNextTrial: String;
    FIsCorrection : Boolean;
    FCfgTrial: TCfgTrial;
    FOnWriteTrialData: TNotifyEvent;
    FOnBeginCorrection : TNotifyEvent;
    FOnEndCorrection : TNotifyEvent;
    FOnBkGndResponse: TNotifyEvent;
    FOnHit: TNotifyEvent;
    FOnMiss: TNotifyEvent;
    FOnNone: TNotifyEvent;
    FOnStmResponse: TNotifyEvent;
    FOnConsequence: TNotifyEvent;
    FOnEndTrial: TNotifyEvent;
    procedure WriteData(Sender: TObject); virtual; abstract;
  public
    procedure Play(Manager : TCounterManager; TestMode: Boolean; Correction : Boolean); virtual; abstract;
    procedure DispenserPlusCall; virtual; abstract;
    property CfgTrial: TCfgTrial read FCfgTrial write FCfgTrial;
    property RootMedia : string read FRootMedia;
    property FileName : string read FFilename write FFilename;
    property IETConsequence : string read FIETConsequence;
    property Result: String read FResult;
    property Header: String read FHeader;
    property HeaderTicks: String read FHeaderTicks;
    property Data: String read FData;
    property DataTicks: String read FDataTicks;
    property NextTrial: String read FNextTrial;
    property OnEndTrial: TNotifyEvent read FOnEndTrial write FOnEndTrial;
    property OnConsequence: TNotifyEvent read FOnConsequence write FOnConsequence;
    property OnWriteTrialData: TNotifyEvent read FOnWriteTrialData write FOnWriteTrialData;
    property OnStmResponse: TNotifyEvent read FOnStmResponse write FOnStmResponse;
    property OnBkGndResponse: TNotifyEvent read FOnBkGndResponse write FOnBkGndResponse;
    property OnHit: TNotifyEvent read FOnHit write FOnHit;
    property OnMiss: TNotifyEvent read FOnMiss write FOnMiss;
    property OnNone: TNotifyEvent read FOnNone write FOnNone;
    property OnBeginCorrection : TNotifyEvent read FOnBeginCorrection write FOnBeginCorrection;
    property OnEndCorrection : TNotifyEvent read FOnEndCorrection write FOnEndCorrection;
    property TimeOut : Integer read FTimeOut;
    property TimeStart : cardinal read FTimeStart write FTimeStart;
  end;

implementation

end.

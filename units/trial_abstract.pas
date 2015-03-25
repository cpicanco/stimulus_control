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

{$I stimulus_control.inc}

interface

uses LCLIntf, LCLType, LMessages, Controls, Classes, SysUtils, LCLProc

  , session_config
  , client
  , regdata
  , countermanager
  , custom_timer
  ;

type

  { TTrial }

  TTrial = class(TCustomControl)
  private
    FCfgTrial: TCfgTrial;
    FClientThread : TClientThread;
    FData: string;
    FDataTicks: string;
    FFilename: string;
    FHeader: string;
    FHeaderTicks: string;
    FIETConsequence: string;
    FCounterManager : TCounterManager;
    FNextTrial: string;
    FResult: string;
    FRootMedia: string;
    FServerAddress: string;
    FTimeOut: Integer;
    FTimestampsData: TRegData;
    FTimeStart: cardinal;
    // events
    FOnBeginCorrection: TNotifyEvent;
    FOnBkGndResponse: TNotifyEvent;
    FOnConsequence: TNotifyEvent;
    FOnEndCorrection: TNotifyEvent;
    FOnEndTrial: TNotifyEvent;
    FOnHit: TNotifyEvent;
    FOnMiss: TNotifyEvent;
    FOnNone: TNotifyEvent;
    FOnStmResponse: TNotifyEvent;
    FOnWriteTrialData: TNotifyEvent;
    FClockThread : TClockThread;
  strict protected
    FIscorrection : Boolean;
    procedure DebugStatus(msg : string);
    procedure StartTrial(Sender: TObject); virtual;
    procedure ThreadClock(Sender: TObject); virtual; abstract;
    procedure WriteData(Sender: TObject); virtual; abstract;
  public
    destructor Destroy; override;
    procedure DispenserPlusCall; virtual; abstract;
    procedure Play(TestMode: Boolean; Correction : Boolean); virtual; abstract;
    procedure CreateClientThread(Code : string);
    property CounterManager : TCounterManager read FCounterManager write FCounterManager;
    property CfgTrial: TCfgTrial read FCfgTrial write FCfgTrial;
    property Data: string read FData write FData;
    property DataTicks: string read FDataTicks write FDataTicks;
    property FileName : string read FFilename write FFilename;
    property Header: string read FHeader write FHeader;
    property HeaderTicks: string read FHeaderTicks write FHeaderTicks;
    property IETConsequence : string read FIETConsequence write FIETConsequence;
    property NextTrial: string read FNextTrial write FNextTrial;
    property Result: string read FResult write FResult;
    property RootMedia : string read FRootMedia write FRootMedia;
    property ServerAddress : string read FServerAddress write FServerAddress;
    property TimeOut : Integer read FTimeOut write FTimeOut;
    property TimestampsData : TRegdata read FTimestampsData write FTimestampsData;
    property TimeStart : cardinal read FTimeStart write FTimeStart;
    property OnBeginCorrection : TNotifyEvent read FOnBeginCorrection write FOnBeginCorrection;
    property OnBkGndResponse: TNotifyEvent read FOnBkGndResponse write FOnBkGndResponse;
    property OnConsequence: TNotifyEvent read FOnConsequence write FOnConsequence;
    property OnEndCorrection : TNotifyEvent read FOnEndCorrection write FOnEndCorrection;
    property OnEndTrial: TNotifyEvent read FOnEndTrial write FOnEndTrial;
    property OnHit: TNotifyEvent read FOnHit write FOnHit;
    property OnMiss: TNotifyEvent read FOnMiss write FOnMiss;
    property OnNone: TNotifyEvent read FOnNone write FOnNone;
    property OnStmResponse: TNotifyEvent read FOnStmResponse write FOnStmResponse;
    property OnWriteTrialData: TNotifyEvent read FOnWriteTrialData write FOnWriteTrialData;
  end;

implementation

{ TTrial }

procedure TTrial.CreateClientThread(Code: string);
begin
  FClientThread := TClientThread.Create( FCfgTrial.Id, Code);
  FClientThread.OnShowStatus := @DebugStatus;
  FClientThread.TimestampsData := FTimestampsData;
  FClientThread.ServerAddress := FServerAddress;
  FClientThread.Start;
end;

procedure TTrial.DebugStatus(msg: string);
begin
//
end;

procedure TTrial.StartTrial(Sender: TObject);
begin
  FClockThread := TClockThread.Create(False);
  FClockThread.OnTimer := @ThreadClock; // descendents can implement the ThreadClock method
end;

destructor TTrial.Destroy;
begin
  if Assigned(FClockThread) then
    FClockThread.Running := False;
  inherited Destroy;
end;



end.

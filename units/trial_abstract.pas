//
// Validation Project (PCRF) - Stimulus Control App
// Copyright (C) 2014-2016,  Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.
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

uses LCLIntf, LCLType, Controls, Classes, SysUtils, LCLProc

  , config_session
  , client
  , countermanager
  , custom_timer
  , timestamps_logger
  ;

type

  { TTrial }

  TTrial = class(TCustomControl)
  private
    FCfgTrial: TCfgTrial;
    FClientThread : TClientThread;
    FClockThread : TClockThread;
    FCounterManager : TCounterManager;
    FData: string;
    FDataTicks: string;
    FFilename: string;
    FHeader: string;
    FHeaderTicks: string;
    FIETConsequence: string;
    FNextTrial: string;
    FResult: string;
    FRootMedia: string;
    FServerAddress: string;
    FTimeOut: Integer;
    FTimeStart: Extended;
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
  strict protected
    FClockList : array of TThreadMethod;
    FLimitedHold : integer;
    FIscorrection : Boolean;
    procedure ClientStatus(msg : string);
    {$ifdef DEBUG}
      procedure ClockStatus(msg : string);
    {$endif}
    procedure StartTrial(Sender: TObject); virtual;
    procedure ThreadClock(Sender: TObject); virtual; abstract;
    procedure WriteData(Sender: TObject); virtual; abstract;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    procedure DispenserPlusCall; virtual; abstract;
    procedure Play(TestMode: Boolean; Correction : Boolean); virtual; abstract;
    procedure SendRequest(ACode : string);
    procedure SetClientThread(AClientThread:TClientThread);
    property CfgTrial: TCfgTrial read FCfgTrial write FCfgTrial;
    property CounterManager : TCounterManager read FCounterManager write FCounterManager;
    property Data: string read FData write FData;
    property DataTicks: string read FDataTicks write FDataTicks;
    property FileName : string read FFilename write FFilename;
    property Header: string read FHeader write FHeader;
    property HeaderTicks: string read FHeaderTicks write FHeaderTicks;
    property IETConsequence : string read FIETConsequence write FIETConsequence;
    property NextTrial: string read FNextTrial write FNextTrial;
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
    property Result: string read FResult write FResult;
    property RootMedia : string read FRootMedia write FRootMedia;
    property ServerAddress : string read FServerAddress write FServerAddress;
    property TimeOut : Integer read FTimeOut write FTimeOut;
    property TimeStart : Extended read FTimeStart write FTimeStart;

  end;

implementation

{$ifdef DEBUG}
uses debug_logger;
{$endif}

{ TTrial }

procedure TTrial.SendRequest(ACode: string);
begin
  FClientThread.SendRequest(ACode,FCfgTrial.Id);
end;

procedure TTrial.SetClientThread(AClientThread: TClientThread);
begin
  FClientThread := AClientThread;
  FClientThread.OnShowStatus := @ClientStatus;
end;

procedure TTrial.ClientStatus(msg: string);
 begin
  {$ifdef DEBUG}
    DebugLn(msg);
  {$else}
    TimestampLn(msg);
  {$endif}
end;

{$ifdef DEBUG}
procedure TTrial.ClockStatus(msg: string);
begin
  DebugLn(msg);
end;
{$endif}

{
  FLimitedhold is controlled by a TTrial descendent.
}
procedure TTrial.StartTrial(Sender: TObject);
var
  i : integer;
begin
  if FLimitedHold > 0 then
    begin
      FClockThread.Interval := FLimitedHold;
      FClockThread.OnTimer := @ThreadClock;
      {$ifdef DEBUG}
        FClockThread.OnDebugStatus := @ClockStatus;
      {$endif};

      SetLength(FClockList, Length(FClockList) + 1);
      FClockList[Length(FClockList) - 1] := @FClockThread.Start;
    end;

  // TClockThread.Start
  if Length(FClockList) > 0 then
    for i := 0 to Length(FClockList) -1 do
      TThreadMethod(FClockList[i]);

  SetLength(FClockList, 0);
end;

constructor TTrial.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClockThread := TClockThread.Create(True);
end;

destructor TTrial.Destroy;
begin
  {$ifdef DEBUG}
    DebugLn(mt_Debug + 'TTrial.Destroy');
    DebugLn(mt_Debug + 'FNextTrial:' + FNextTrial);
  {$endif}

  if Assigned(FClockThread) then
    begin
      FClockThread.Enabled := False;
      FClockThread.Terminate;
    end;
  inherited Destroy;
end;



end.

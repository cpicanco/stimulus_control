{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit pupil_communication;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils
     , zmq_client
     ;

type
  { TNotifyPupilEvent }

  TNotifyPupilEvent = procedure(Sender: TObject; AResponse: String) of object;

  { TPupilCommunication }

  TPupilCommunication = class(TZMQThread)
    private
      FOnAfterCalibrationStart: TNotifyPupilEvent;
      FOnAfterRecordingStart: TNotifyPupilEvent;
      FOnAfterCalibrationStop: TNotifyPupilEvent;
      FOnAfterRecordingStop: TNotifyPupilEvent;
      FOnAfterSynchronizeTime: TNotifyPupilEvent;
      FOnReceiveRecordingPath: TNotifyPupilEvent;
      FOnReceiveTimestamp: TNotifyPupilEvent;
      //procedure AfterCalibrationStart(Sender : TObject; AResponse : string);
      //procedure AfterCalibrationStop(Sender : TObject; AResponse : string);
      //procedure AfterRecordingStart(Sender : TObject; AResponse : string);
      //procedure AfterRecordingStop(Sender : TObject; AResponse : string);
      //procedure AfterSynchronizeTime(Sender : TObject; AResponse : string);

      procedure ReceiveResponse(ARequest, AResponse: String);
      procedure SetOnAfterCalibrationStart(AValue: TNotifyPupilEvent);
      procedure SetOnAfterCalibrationStop(AValue: TNotifyPupilEvent);
      procedure SetOnAfterRecordingStart(AValue: TNotifyPupilEvent);
      procedure SetOnAfterRecordingStop(AValue: TNotifyPupilEvent);
      procedure SetOnAfterSynchronizeTime(AValue: TNotifyPupilEvent);
      procedure SetOnReceiveRecordingPath(AValue: TNotifyPupilEvent);
      procedure SetOnReceiveTimestamp(AValue: TNotifyPupilEvent);
    public
      constructor Create(AHost : string; CreateSuspended: Boolean = True);
      destructor Destroy; override;
      procedure RequestRecordingPath;
      procedure RequestTimestamp;
      procedure StartRecording;
      procedure StopRecording;
      procedure StartCalibration;
      procedure StopCalibration;
      procedure SynchronizeTime(ATime: Extended);
      property OnAfterCalibrationStart : TNotifyPupilEvent read FOnAfterCalibrationStart write SetOnAfterCalibrationStart ;
      property OnAfterCalibrationStop : TNotifyPupilEvent read FOnAfterCalibrationStop write SetOnAfterCalibrationStop  ;
      property OnAfterRecordingStart : TNotifyPupilEvent read FOnAfterRecordingStart write SetOnAfterRecordingStart  ;
      property OnAfterRecordingStop : TNotifyPupilEvent read FOnAfterRecordingStop write SetOnAfterRecordingStop  ;
      property OnAfterSynchronizeTime : TNotifyPupilEvent read FOnAfterSynchronizeTime write SetOnAfterSynchronizeTime  ;
      property OnReceiveRecordingPath : TNotifyPupilEvent read FOnReceiveRecordingPath write SetOnReceiveRecordingPath  ;
      property OnReceiveTimestamp : TNotifyPupilEvent read FOnReceiveTimestamp write SetOnReceiveTimestamp   ;
  end;


implementation

{$ifdef DEBUG}
uses debug_logger;
{$endif}

//# IPC Backbone communication
//'PUB_PORT' return the current pub port of the IPC Backbone
//'SUB_PORT' return the current sub port of the IPC Backbone

resourcestring
  ERROR_UNKNOWN_COMMAND = 'Commando Pupil desconhecido: ';
  ERROR_NOT_IMPLEMENTED = 'Commando Pupil não implementado:';

const
  // start recording with auto generated session name
  // note: may append a string to session name, 'R [session name]'
  SHOULD_START_RECORDING = 'R';

  // stop recording
  SHOULD_STOP_RECORDING = 'r';

  // start currently selected calibration
  SHOULD_START_CALIBRATION = 'C';

  // stop currently selected calibration
  SHOULD_STOP_CALIBRATION = 'c';

  // 'T [time]' make pupil timestamps count from [time] on.
  SYNCHRONIZE_TIME = 'T';

  // get pupil capture timestamp returns a float as string.
  REQUEST_TIMESTAMP = 't';

  // request recording path
  REQUEST_RECORDING_PATH = 'P';

{ TPupilCommunication }

//procedure TPupilCommunication.AfterCalibrationStart(Sender: TObject;
//  AResponse: string);
//begin
//
//end;
//
//procedure TPupilCommunication.AfterCalibrationStop(Sender: TObject;
//  AResponse: string);
//begin
//
//end;
//
//procedure TPupilCommunication.AfterRecordingStart(Sender: TObject;
//  AResponse: string);
//begin
//
//end;
//
//procedure TPupilCommunication.AfterRecordingStop(Sender: TObject;
//  AResponse: string);
//begin
//
//end;
//
//procedure TPupilCommunication.AfterSynchronizeTime(Sender: TObject;
//  AResponse: string);
//begin
//
//end;

procedure TPupilCommunication.ReceiveResponse(ARequest, AResponse: String);
begin
  case ARequest of
    SHOULD_START_RECORDING : if Assigned(OnAfterRecordingStart) then OnAfterRecordingStart(Self, AResponse);
    SHOULD_STOP_RECORDING : if Assigned(OnAfterRecordingStop) then OnAfterRecordingStop(Self, AResponse);
    SHOULD_START_CALIBRATION : if Assigned(OnAfterCalibrationStart) then OnAfterCalibrationStart(Self, AResponse);
    SHOULD_STOP_CALIBRATION : if Assigned(OnAfterCalibrationStop) then OnAfterCalibrationStop(Self, AResponse);
    SYNCHRONIZE_TIME : if Assigned(OnAfterSynchronizeTime) then OnAfterSynchronizeTime(Self, AResponse);
    REQUEST_TIMESTAMP : if Assigned(OnReceiveTimestamp) then OnReceiveTimestamp(Self, AResponse);
    REQUEST_RECORDING_PATH : if Assigned(OnReceiveRecordingPath) then OnReceiveRecordingPath(Self, AResponse);
    else raise Exception.Create( ERROR_UNKNOWN_COMMAND + ARequest + #32 + Self.ClassName );
  end;
end;

procedure TPupilCommunication.SetOnAfterCalibrationStart(
  AValue: TNotifyPupilEvent);
begin
  if FOnAfterCalibrationStart=AValue then Exit;
  FOnAfterCalibrationStart:=AValue;
end;

procedure TPupilCommunication.SetOnAfterCalibrationStop(
  AValue: TNotifyPupilEvent);
begin
  if FOnAfterCalibrationStop=AValue then Exit;
  FOnAfterCalibrationStop:=AValue;
end;

procedure TPupilCommunication.SetOnAfterRecordingStart(AValue: TNotifyPupilEvent
  );
begin
  if FOnAfterRecordingStart=AValue then Exit;
  FOnAfterRecordingStart:=AValue;
end;

procedure TPupilCommunication.SetOnAfterRecordingStop(AValue: TNotifyPupilEvent
  );
begin
  if FOnAfterRecordingStop=AValue then Exit;
  FOnAfterRecordingStop:=AValue;
end;

procedure TPupilCommunication.SetOnAfterSynchronizeTime(
  AValue: TNotifyPupilEvent);
begin
  if FOnAfterSynchronizeTime=AValue then Exit;
  FOnAfterSynchronizeTime:=AValue;
end;

procedure TPupilCommunication.SetOnReceiveRecordingPath(
  AValue: TNotifyPupilEvent);
begin
  if FOnReceiveRecordingPath=AValue then Exit;
  FOnReceiveRecordingPath:=AValue;
end;

procedure TPupilCommunication.SetOnReceiveTimestamp(AValue: TNotifyPupilEvent);
begin
  if FOnReceiveTimestamp=AValue then Exit;
  FOnReceiveTimestamp:=AValue;
end;

constructor TPupilCommunication.Create(AHost: string; CreateSuspended: Boolean);
begin
  inherited Create(AHost, CreateSuspended);
  OnResponseReceived := @ReceiveResponse;
end;

destructor TPupilCommunication.Destroy;
begin
  OnResponseReceived := nil;
  inherited Destroy;
end;

procedure TPupilCommunication.StartRecording;
begin
  SendRequest(SHOULD_START_RECORDING);
end;

procedure TPupilCommunication.StopRecording;
begin
  SendRequest(SHOULD_STOP_RECORDING);
end;

procedure TPupilCommunication.StartCalibration;
begin
  SendRequest(SHOULD_START_CALIBRATION);
end;

procedure TPupilCommunication.StopCalibration;
begin
  SendRequest(SHOULD_STOP_CALIBRATION);
end;

procedure TPupilCommunication.RequestRecordingPath;
begin
  raise Exception.Create( ERROR_NOT_IMPLEMENTED + REQUEST_RECORDING_PATH + #32 + Self.ClassName );
end;

procedure TPupilCommunication.RequestTimestamp;
begin
  SendRequest(REQUEST_TIMESTAMP);
end;

procedure TPupilCommunication.SynchronizeTime(ATime: Extended);
begin
  SendRequest(SYNCHRONIZE_TIME + #32 + FloatToStrF(ATime,ffFixed,0,9));
end;

end.


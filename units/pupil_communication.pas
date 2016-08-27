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
     , SimpleMsgPack
     ;

type

  { TNotifyPupilEvent }

  TNotifyPupilEvent = procedure(Sender: TObject; AResponse: String) of object;

  { TPupilCommunication }

  TPupilCommunication = class(TZMQThread)
    private
      FSubPort : string;
      FLocalIP : string;
      FZMQSubThread : TZMQSubThread;
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

      procedure ReceiveDictionary(UncodedMessagePackage : TSimpleMsgPack);
      procedure ReceiveSubPort(AResponse: String);
      procedure ReceivePubPort(AResponse: String);
      procedure ReceiveResponse(ARequest, AResponse: String);
      procedure ReceiveMultipartMessage(AMultipartMessage : TPupilMultiPartMessage);
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
      procedure RequestSubscribePort;
      procedure RequestPublisherPort;
      procedure RequestTimestamp;
      procedure StartRecording;
      procedure StopRecording;
      procedure StartCalibration;
      procedure StopCalibration;
      procedure SubscribeAllNotification;
      procedure SubscribeEyeCameraZ;
      procedure SubscribeLoggingInfo;
      procedure SubscribeLoggingError;
      procedure UnSubscribeAllNotification;
      procedure UnSubscribeEyeCameraZ;
      procedure UnSubscribeLoggingInfo;
      procedure UnSubscribeLoggingError;
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

resourcestring
  ERROR_UNKNOWN_COMMAND = 'Commando Pupil desconhecido: ';
  ERROR_NOT_IMPLEMENTED = 'Commando Pupil não implementado:';

const
  // return the current publisher's port of the IPC Backbone
  REQUEST_PUB_PORT = 'PUB_PORT';

  // return the current subscriber's port of the IPC Backbone
  REQUEST_SUB_PORT = 'SUB_PORT';

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

const
  SUB_ALL_NOTIFICATIONS = 'notify.';
  SUB_EYE_CAMERA_0 = 'pupil.0';
  SUB_LOGGING_INFO = 'logging.info';
  SUB_LOGGING_ERROR = 'logging.error';

{ TPupilCommunication }

constructor TPupilCommunication.Create(AHost: string; CreateSuspended: Boolean);
begin
  FLocalIP := Copy(AHost,1, pos(':', AHost)); // WriteLn('DEGUG',#32, FLocalIP);
  inherited Create(AHost, CreateSuspended);
  OnResponseReceived := @ReceiveResponse;
end;

destructor TPupilCommunication.Destroy;
begin
  OnResponseReceived := nil;
  if Assigned(FZMQSubThread) then FZMQSubThread.Terminate;
  inherited Destroy;
end;

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

procedure TPupilCommunication.ReceiveDictionary(UncodedMessagePackage: TSimpleMsgPack);
begin

end;

procedure TPupilCommunication.ReceiveSubPort(AResponse: String);
var SubHost : string;
begin
  FSubPort := AResponse;
  SubHost := FLocalIP + FSubPort; WriteLn('SubHost', #32, SubHost);
  FZMQSubThread := TZMQSubThread.Create(SubHost);
  FZMQSubThread.OnMultiPartMessageReceived := @ReceiveMultipartMessage;
  FZMQSubThread.Subscribe('nothing.at_all');
  FZMQSubThread.Start;
end;

procedure TPupilCommunication.ReceivePubPort(AResponse: String);
begin
  raise Exception.Create( ERROR_NOT_IMPLEMENTED + REQUEST_PUB_PORT + #32 + Self.ClassName );
  //SendRequest(REQUEST_PUB_PORT);
  { TODO 1 -oRafael -c-enhencement : publish to the pupil ipc backbone }
end;

procedure TPupilCommunication.RequestSubscribePort;
begin
  SendRequest(REQUEST_SUB_PORT);
end;

procedure TPupilCommunication.RequestPublisherPort;
begin
  SendRequest(REQUEST_PUB_PORT);
end;

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
    REQUEST_SUB_PORT : ReceiveSubPort(AResponse);
    REQUEST_PUB_PORT : ReceivePubPort(AResponse);
    else raise Exception.Create( ERROR_UNKNOWN_COMMAND + ARequest + #32 + Self.ClassName );
  end;
end;

procedure TPupilCommunication.ReceiveMultipartMessage(AMultipartMessage: TPupilMultiPartMessage);
var //i : integer;
    Serializer :  TSimpleMsgPack;
const
  NOTIFY_RECORDING_SHOULD_START = 'notify.recording.should_start';
//  NOTIFY_RECORDING_SHOULD_STOP = 'notify.recording.should_stop';
//  NOTIFY_RECORDING_STOPPED = 'notify.recording.stopped';
//  NOTIFY_RECORDING_STARTED = 'notify.recording.started';
//  NOTIFY_CALIBRATION_SHOULD_START = 'notify.calibration.should_start';
//  NOTIFY_CALIBRATION_STARTED = 'notify.calibration.started';
//  NOTIFY_CALIBRATION_FAILED = 'notify.calibration.failed';

begin
  AMultipartMessage.MsgPackage.Position := 0;
  WriteLn(AMultipartMessage.MsgTopic);
  Serializer := TSimpleMsgPack.Create;
  try
    Serializer.Clear;
    Serializer.DecodeFromStream(AMultipartMessage.MsgPackage);

    case AMultipartMessage.MsgTopic of
      SUB_EYE_CAMERA_0 : ReceiveDictionary(Serializer);
      SUB_LOGGING_INFO : ReceiveDictionary(Serializer);
      NOTIFY_RECORDING_SHOULD_START : WriteLn(Serializer.S['session_name']);
    end;
    WriteLn(IntToStr(Serializer.Count));
  finally
    Serializer.Free;
    AMultipartMessage.MsgPackage.Free;
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

procedure TPupilCommunication.SubscribeAllNotification;
begin
  if Assigned(FZMQSubThread) then FZMQSubThread.Subscribe(SUB_ALL_NOTIFICATIONS);
end;

procedure TPupilCommunication.SubscribeEyeCameraZ;
begin
  if Assigned(FZMQSubThread) then FZMQSubThread.Subscribe(SUB_EYE_CAMERA_0);
end;

procedure TPupilCommunication.SubscribeLoggingInfo;
begin
  if Assigned(FZMQSubThread) then FZMQSubThread.Subscribe(SUB_LOGGING_INFO);
end;

procedure TPupilCommunication.SubscribeLoggingError;
begin
  if Assigned(FZMQSubThread) then FZMQSubThread.Subscribe(SUB_LOGGING_ERROR);
end;

procedure TPupilCommunication.UnSubscribeAllNotification;
begin
  if Assigned(FZMQSubThread) then FZMQSubThread.UnSubscribe(SUB_ALL_NOTIFICATIONS);
end;

procedure TPupilCommunication.UnSubscribeEyeCameraZ;
begin
  if Assigned(FZMQSubThread) then FZMQSubThread.UnSubscribe(SUB_EYE_CAMERA_0);
end;

procedure TPupilCommunication.UnSubscribeLoggingInfo;
begin
  if Assigned(FZMQSubThread) then FZMQSubThread.UnSubscribe(SUB_LOGGING_INFO);
end;

procedure TPupilCommunication.UnSubscribeLoggingError;
begin
  if Assigned(FZMQSubThread) then FZMQSubThread.UnSubscribe(SUB_LOGGING_ERROR);
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


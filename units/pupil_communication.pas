{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit pupil_communication;

{$mode objfpc}{$H+}

{$DEFINE DEBUG}

interface

uses Classes, SysUtils
     , zmq_client
     , SimpleMsgPack
     ;

type

  { TMPMessage }

  TMPMessage = record
    Topic : string;
    Message : TSimpleMsgPack;
  end;

  { TNotifyMultipartMessage }

  TNotifyMultipartMessage = procedure(Sender: TObject; AMultiPartMessage : TMPMessage) of object;

  { TNotifyRequest }

  TNotifyRequest = procedure(Sender: TObject; ARequest, AResponse: String) of object;

  { TPupilCommunication }

  TPupilCommunication = class(TZMQThread)
    private
      FSubPort : string;
      FLocalIP : string;
      FZMQSubThread : TZMQSubThread;
      procedure ReceiveDictionary(DecodedMessagePackage : TSimpleMsgPack);
      procedure ReceiveSubPort(AResponse: string);
      procedure ReceivePubPort(AResponse: string);
      procedure ReceiveResponse(ARequest, AResponse: string);
      procedure ReceiveMultipartMessage(AMultipartMessage : TMultiPartMessage);
      procedure SubscriberTerminated(Sender : TObject);
    private
      FOnCalibrationStopped: TNotifyMultipartMessage;
      FOnRecordingStarted: TNotifyMultipartMessage;
      FOnRequestReceived : TNotifyRequest;
      FOnMultipartMessageReceived : TNotifyMultipartMessage;
      function GetSubscribed: Boolean;
      procedure SetOnCalibrationStopped(AValue: TNotifyMultipartMessage);
      procedure SetOnMultiPartMessageReceived(AValue: TNotifyMultipartMessage);
      procedure SetOnRecordingStarted(AValue: TNotifyMultipartMessage);
      procedure SetOnRequestReceived(AValue: TNotifyRequest);
    public
      constructor Create(AHost : string; CreateSuspended: Boolean = True);
      destructor Destroy; override;
      procedure Request(AReq : UTF8String; Blocking : Boolean = False);

      // Must call StartSubscriber first
      procedure Subscribe(ASub : UTF8String);
      procedure StartSubscriber; overload;
      procedure StartSubscriber(Blocking : Boolean); overload;
      procedure UnSubscribe(ASub : UTF8String);
      property Subscribed : Boolean read GetSubscribed;
    public
      property OnCalibrationStopped : TNotifyMultipartMessage read FOnCalibrationStopped write SetOnCalibrationStopped;
      property OnRecordingStarted : TNotifyMultipartMessage read FOnRecordingStarted write SetOnRecordingStarted;
      property OnRequestReceived : TNotifyRequest read FOnRequestReceived write SetOnRequestReceived;
      property OnMultiPartMessageReceived : TNotifyMultipartMessage read FOnMultiPartMessageReceived write SetOnMultiPartMessageReceived;
  end;

const
  // start recording with auto generated session name
  // note: may append a string to session name, 'R [session name]'
  REQ_SHOULD_START_RECORDING  = 'R';

  // stop recording
  REQ_SHOULD_STOP_RECORDING = 'r';

  // start currently selected calibration
  REQ_SHOULD_START_CALIBRATION = 'C';

  // stop currently selected calibration
  REQ_SHOULD_STOP_CALIBRATION = 'c';

  // '[T][#32][time]' make pupil timestamps count from [time] on.
  REQ_SYNCHRONIZE_TIME = 'T';

  // get pupil capture timestamp returns a float as string.
  REQ_TIMESTAMP = 't';

  // request recording path
  REQ_RECORDING_PATH = 'P';

const
  SUB_ALL_NOTIFICATIONS = 'notify.';

  SUB_GAZE_DATA = 'gaze';
  SUB_EYE_CAMERA_0 = 'pupil.0';
  SUB_EYE_CAMERA_1 = 'pupil.1';

  SUB_LOGGING_INFO = 'logging.info';
  SUB_LOGGING_ERROR = 'logging.error';
  SUB_LOGGING_WARNING = 'logging.warning';

  SUB_TIME_SYNC = 'time_sync.';

const
  NOTIFY_RECORDING_SHOULD_START = 'notify.recording.should_start';
  NOTIFY_RECORDING_SHOULD_STOP = 'notify.recording.should_stop';
  NOTIFY_RECORDING_STARTED = 'notify.recording.started';
  NOTIFY_RECORDING_STOPPED = 'notify.recording.stopped';

  NOTIFY_CALIBRATION_SHOULD_START = 'notify.calibration.should_start';
  NOTIFY_CALIBRATION_SHOULD_STOP = 'notify.calibration.should_stop';
  NOTIFY_CALIBRATION_STARTED = 'notify.calibration.started';
  NOTIFY_CALIBRATION_STOPPED = 'notify.calibration.stopped';
  NOTIFY_CALIBRATION_FAILED = 'notify.calibration.failed';
  NOTIFY_CALIBRATION_SUCCESSFUL = 'notify.calibration.successful';

  // 'notify.eye_process.stopped';
  // 'notify.eye_process.should_start.0'
  // 'notify.eye_process.should_start.1'

const
  KEY_SUBJECT = 'subject';
  KEY_RECORDING_PATH = 'rec_path';
  KEY_SESSION_NAME = 'session_name';
  KEY_RECORD_EYE = 'record_eye';
  KEY_COMPRESSION = 'compression';



implementation

{$ifdef DEBUG}
uses debug_logger;
{$endif}

resourcestring
  ERROR_UNKNOWN_COMMAND = 'Commando Pupil desconhecido: ';
  ERROR_NOT_IMPLEMENTED = 'Commando Pupil não implementado:';

const
  // return the current publisher's port of the IPC Backbone
  REQ_PUB_PORT = 'PUB_PORT';

  // return the current subscriber's port of the IPC Backbone
  REQ_SUB_PORT = 'SUB_PORT';


{ TPupilCommunication }

constructor TPupilCommunication.Create(AHost: string; CreateSuspended: Boolean);
begin
  FLocalIP := Copy(AHost,1, pos(':', AHost));
  FSubPort := '';
  inherited Create(AHost, CreateSuspended);
  OnResponseReceived := @ReceiveResponse;
end;

destructor TPupilCommunication.Destroy;
begin
  OnResponseReceived := nil;
  if Assigned(FZMQSubThread) then FZMQSubThread.Terminate;
  inherited Destroy;
end;

procedure TPupilCommunication.Request(AReq: UTF8String; Blocking: Boolean);
begin
  SendRequest(AReq,Blocking);
end;

procedure TPupilCommunication.Subscribe(ASub: UTF8String);
begin
  if Assigned(FZMQSubThread) then FZMQSubThread.Subscribe(ASub);
end;

procedure TPupilCommunication.StartSubscriber;
begin
  Request(REQ_SUB_PORT);
end;

procedure TPupilCommunication.StartSubscriber(Blocking: Boolean);
begin
  Request(REQ_SUB_PORT,Blocking);
end;

procedure TPupilCommunication.UnSubscribe(ASub: UTF8String);
begin
  if Assigned(FZMQSubThread) then FZMQSubThread.UnSubscribe(ASub);
end;


procedure TPupilCommunication.ReceiveDictionary(DecodedMessagePackage: TSimpleMsgPack);
var j : integer;
begin
  with DecodedMessagePackage do
    for j := 0 to Count -1 do
      begin;
        {$ifdef DEBUG}
          DebugLn(mt_Debug + Items[j].Key + ':' + Items[j].Value);
        {$endif};
      end;
end;

procedure TPupilCommunication.ReceiveSubPort(AResponse: string);
var SubHost : string;
begin
  if FSubPort = '' then
    begin
      SubHost := FLocalIP + AResponse;
      {$ifdef DEBUG}
        DebugLn(mt_Debug + 'SubHost:' + #32 + SubHost);
      {$endif};
      FZMQSubThread := TZMQSubThread.Create(SubHost);
      with FZMQSubThread do
        begin;
          OnTerminate := @SubscriberTerminated;
          OnMultiPartMessageReceived := @ReceiveMultipartMessage;
          Start;
        end;
      FSubPort := AResponse;
    end;
end;

procedure TPupilCommunication.ReceivePubPort(AResponse: string);
begin
  raise Exception.Create( ERROR_NOT_IMPLEMENTED + REQ_PUB_PORT + #32 + Self.ClassName + '#32' + AResponse);
  //SendRequest(REQUEST_PUB_PORT);
  { TODO 1 -oRafael -c-enhencement : publish to the pupil ipc backbone }
end;

procedure TPupilCommunication.ReceiveResponse(ARequest, AResponse: string);
begin
  case ARequest of
    REQ_SHOULD_START_RECORDING, REQ_SHOULD_STOP_RECORDING,
    REQ_SHOULD_START_CALIBRATION, REQ_SHOULD_STOP_CALIBRATION,
    REQ_TIMESTAMP : if Assigned(OnRequestReceived) then OnRequestReceived(Self, ARequest, AResponse);
    REQ_SUB_PORT : ReceiveSubPort(AResponse);
    REQ_PUB_PORT : ReceivePubPort(AResponse);
    else
      if Pos(REQ_SYNCHRONIZE_TIME,ARequest) <> 0 then
        begin
          if Assigned(OnRequestReceived) then OnRequestReceived(Self, ARequest, AResponse);
        end
      else raise Exception.Create( ERROR_UNKNOWN_COMMAND + ARequest + #32 + Self.ClassName );
  end;
end;

procedure TPupilCommunication.ReceiveMultipartMessage(AMultipartMessage: TMultiPartMessage);
var Serializer :  TSimpleMsgPack;
    MPMessage : TMPMessage;
begin
  MPMessage.Topic := AMultipartMessage.MsgTopic;
  {$ifdef DEBUG}
    DebugLn(mt_Debug + MPMessage.Topic)
  {$endif};

  AMultipartMessage.MsgPackage.Position := 0;
  Serializer := TSimpleMsgPack.Create;
  try
    Serializer.Clear;
    Serializer.DecodeFromStream(AMultipartMessage.MsgPackage);
    {$ifdef DEBUG}
      DebugLn(mt_Debug + 'TopicCount:'+ IntToStr(Serializer.Count));
    {$endif};
    ReceiveDictionary(Serializer);
    MPMessage.Message := Serializer;
    case AMultipartMessage.MsgTopic of
      NOTIFY_RECORDING_STARTED : if Assigned(OnRecordingStarted) then OnRecordingStarted(Self,MPMessage);
      NOTIFY_CALIBRATION_STOPPED : if Assigned(OnCalibrationStopped) then OnCalibrationStopped(Self, MPMessage);
      else if Assigned(OnMultiPartMessageReceived) then OnMultiPartMessageReceived(Self,MPMessage);
    end;

  finally
    Serializer.Free;
    AMultipartMessage.MsgPackage.Free;
  end;
end;

procedure TPupilCommunication.SubscriberTerminated(Sender: TObject);
begin
  FSubPort := '';
end;

procedure TPupilCommunication.SetOnMultiPartMessageReceived(
  AValue: TNotifyMultipartMessage);
begin
  if FOnMultiPartMessageReceived = AValue then Exit;
  FOnMultiPartMessageReceived := AValue;
end;

procedure TPupilCommunication.SetOnCalibrationStopped(
  AValue: TNotifyMultipartMessage);
begin
  if FOnCalibrationStopped = AValue then Exit;
  FOnCalibrationStopped := AValue;
end;

function TPupilCommunication.GetSubscribed: Boolean;
begin
  Result := FSubPort <> '';
end;

procedure TPupilCommunication.SetOnRecordingStarted(
  AValue: TNotifyMultipartMessage);
begin
  if FOnRecordingStarted = AValue then Exit;
  FOnRecordingStarted := AValue;
end;

procedure TPupilCommunication.SetOnRequestReceived(AValue: TNotifyRequest);
begin
  if FOnRequestReceived = AValue then Exit;
  FOnRequestReceived := AValue;
end;


end.


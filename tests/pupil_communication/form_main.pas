unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls
  , pupil_communication
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnReceiveTimestamp: TButton;
    btnStartRecording: TButton;
    btnReceiveRecordingPath: TButton;
    btnStopRecording: TButton;
    btnStartCalibration: TButton;
    btnStopCalibration: TButton;
    btnUnsubscribe: TButton;
    btnReqTimeSync: TButton;
    gbxSubscribe: TGroupBox;
    LabelHost: TLabel;
    rbtnGazeData: TRadioButton;
    rbtnTimeSync: TRadioButton;
    rbtnNotifications: TRadioButton;
    rbtnLoggingInfo: TRadioButton;
    rbtnEyeCamera: TRadioButton;
    Subscribe: TButton;
    procedure btnReceiveTimestampClick(Sender: TObject);
    procedure btnReceiveRecordingPathClick(Sender: TObject);
    procedure btnReqTimeSyncClick(Sender: TObject);
    procedure btnStartRecordingClick(Sender: TObject);
    procedure btnStopRecordingClick(Sender: TObject);
    procedure btnStartCalibrationClick(Sender: TObject);
    procedure btnStopCalibrationClick(Sender: TObject);
    procedure btnUnsubscribeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SubscribeClick(Sender: TObject);
  private
    FPupilClient : TPupilCommunication;
    procedure ReceiveRequest(Sender: TObject; ARequest, AResponse: String);
    procedure MultipartMessage(Sender: TObject; AMultipartMessage : TMPMessage);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnReceiveTimestampClick(Sender: TObject);
begin
  FPupilClient.Request(REQ_TIMESTAMP);
end;

procedure TForm1.btnReceiveRecordingPathClick(Sender: TObject);
begin
  FPupilClient.Request(REQ_RECORDING_PATH);
end;

procedure TForm1.btnReqTimeSyncClick(Sender: TObject);
begin
  FPupilClient.Request(REQ_SYNCHRONIZE_TIME + #32 + '0');
end;

procedure TForm1.btnStartRecordingClick(Sender: TObject);
begin
  FPupilClient.Request(REQ_SHOULD_START_RECORDING);
end;

procedure TForm1.btnStopRecordingClick(Sender: TObject);
begin
  FPupilClient.Request(REQ_SHOULD_STOP_RECORDING);
end;

procedure TForm1.btnStartCalibrationClick(Sender: TObject);
begin
  FPupilClient.Request(REQ_SHOULD_START_CALIBRATION);
end;

procedure TForm1.btnStopCalibrationClick(Sender: TObject);
begin
  FPupilClient.Request(REQ_SHOULD_STOP_CALIBRATION);
end;

procedure TForm1.btnUnsubscribeClick(Sender: TObject);
begin
  if rbtnNotifications.Checked then
    FPupilClient.UnSubscribe(SUB_ALL_NOTIFICATIONS);

  if rbtnLoggingInfo.Checked then
    FPupilClient.UnSubscribe(SUB_LOGGING_INFO);

  if rbtnEyeCamera.Checked then
    FPupilClient.UnSubscribe(SUB_EYE_CAMERA_0);

  if rbtnTimeSync.Checked then
    FPupilClient.UnSubscribe(SUB_TIME_SYNC);

  if rbtnGazeData.Checked then
    FPupilClient.UnSubscribe(SUB_GAZE_DATA);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FPupilClient := TPupilCommunication.Create('localhost:5020');
  with FPupilClient do
    begin
      OnRequestReceived := @ReceiveRequest;
      OnMultiPartMessageReceived := @MultipartMessage;
      Start;
      StartSubscriber;
    end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FPupilClient.Terminate;
end;

procedure TForm1.SubscribeClick(Sender: TObject);
begin
  if rbtnNotifications.Checked then
    FPupilClient.Subscribe(SUB_ALL_NOTIFICATIONS);

  if rbtnLoggingInfo.Checked then
    FPupilClient.Subscribe(SUB_LOGGING_INFO);

  if rbtnEyeCamera.Checked then
    FPupilClient.Subscribe(SUB_EYE_CAMERA_0);

  if rbtnTimeSync.Checked then
    FPupilClient.Subscribe(SUB_TIME_SYNC);

  if rbtnGazeData.Checked then
    FPupilClient.Subscribe(SUB_GAZE_DATA);

end;

procedure TForm1.ReceiveRequest(Sender: TObject; ARequest, AResponse: String);
begin
  case ARequest of
    REQ_RECORDING_PATH: ShowMessage('Not implemented');
    else ShowMessage(Sender.ClassName + #32 + ARequest + #32 + AResponse);
  end;
end;

procedure TForm1.MultipartMessage(Sender: TObject; AMultipartMessage: TMPMessage
  );
begin
  case AMultipartMessage.MsgTopic of
    NOTIFY_CALIBRATION_FAILED : ShowMessage('Calibration Failed!');
  end;

end;


end.


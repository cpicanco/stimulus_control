unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls
  , pupil_communication
  , timestamp
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
    gbxSubscribe: TGroupBox;
    LabelHost: TLabel;
    rbtnNotifications: TRadioButton;
    rbtnLoggingInfo: TRadioButton;
    rbtnEyeCamera: TRadioButton;
    Subscribe: TButton;
    procedure btnReceiveTimestampClick(Sender: TObject);
    procedure btnReceiveRecordingPathClick(Sender: TObject);
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
    procedure ReceiveResponse(Sender: TObject; AResponse: String);
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
  FPupilClient.RequestTimestamp;
end;

procedure TForm1.btnReceiveRecordingPathClick(Sender: TObject);
begin
  FPupilClient.RequestRecordingPath;
end;

procedure TForm1.btnStartRecordingClick(Sender: TObject);
begin
  FPupilClient.StartRecording;
end;

procedure TForm1.btnStopRecordingClick(Sender: TObject);
begin
  FPupilClient.StopRecording;
end;

procedure TForm1.btnStartCalibrationClick(Sender: TObject);
begin
  FPupilClient.StartCalibration;
end;

procedure TForm1.btnStopCalibrationClick(Sender: TObject);
begin
  FPupilClient.StopCalibration;
end;

procedure TForm1.btnUnsubscribeClick(Sender: TObject);
begin
  if rbtnNotifications.Checked then
    FPupilClient.UnSubscribeAllNotification;

  if rbtnLoggingInfo.Checked then
    FPupilClient.UnSubscribeLoggingInfo;

  if rbtnEyeCamera.Checked then
    FPupilClient.UnSubscribeEyeCameraZ;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FPupilClient := TPupilCommunication.Create('localhost:5020');
  with FPupilClient do
    begin
      OnAfterCalibrationStart := @ReceiveResponse;
      OnAfterCalibrationStop := @ReceiveResponse;
      OnAfterRecordingStart := @ReceiveResponse;
      OnAfterRecordingStop := @ReceiveResponse;
      OnReceiveTimestamp := @ReceiveResponse;
      OnReceiveRecordingPath := @ReceiveResponse;
      Start;
      RequestSubscribePort;
    end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FPupilClient.Terminate;
end;

procedure TForm1.SubscribeClick(Sender: TObject);
begin
  if rbtnNotifications.Checked then
    FPupilClient.SubscribeAllNotification;

  if rbtnLoggingInfo.Checked then
    FPupilClient.SubscribeLoggingInfo;

  if rbtnEyeCamera.Checked then
    FPupilClient.SubscribeEyeCameraZ;
end;

procedure TForm1.ReceiveResponse(Sender: TObject; AResponse: String);
begin
  ShowMessage(Sender.ClassName + #32 + AResponse)
end;


end.


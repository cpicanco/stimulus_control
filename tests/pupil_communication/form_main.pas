{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, pupil_communication
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
    LabelHost: TLabel;
    rgrpSubscribe: TRadioGroup;
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
    procedure RecordingStarted(Sender: TObject; AMultipartMessage : TMPMessage);
    procedure CalibrationStopped(Sender: TObject; AMultipartMessage : TMPMessage);
    procedure MultipartMessage(Sender: TObject; AMultipartMessage : TMPMessage);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

const CLOCAL_HOST = '127.0.1.1:5020';

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
  case rgrpSubscribe.ItemIndex of
    0 : FPupilClient.UnSubscribe(SUB_ALL_NOTIFICATIONS);
    1 : FPupilClient.UnSubscribe(SUB_LOGGING_INFO);
    2 : FPupilClient.UnSubscribe(SUB_EYE_CAMERA_0);
    3 : FPupilClient.UnSubscribe(SUB_TIME_SYNC);
    4 : FPupilClient.UnSubscribe(SUB_GAZE_DATA);
    5 : FPupilClient.UnSubscribe(SUB_GROUPS);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LabelHost.Caption:='Address:'+CLOCAL_HOST+', Pupil Capture Version:v0.8.5';
  FPupilClient := TPupilCommunication.Create(CLOCAL_HOST);
  with FPupilClient do
    begin
      OnRequestReceived := @ReceiveRequest;
      OnMultiPartMessageReceived := @MultipartMessage;
      OnCalibrationStopped := @CalibrationStopped;
      OnRecordingStarted := @RecordingStarted;
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
  case rgrpSubscribe.ItemIndex of
    0 : FPupilClient.Subscribe(SUB_ALL_NOTIFICATIONS);
    1 : FPupilClient.Subscribe(SUB_LOGGING_INFO);
    2 : FPupilClient.Subscribe(SUB_EYE_CAMERA_0);
    3 : FPupilClient.Subscribe(SUB_TIME_SYNC);
    4 : FPupilClient.Subscribe(SUB_GAZE_DATA);
    5 : FPupilClient.Subscribe(SUB_GROUPS);
  end;
end;

procedure TForm1.ReceiveRequest(Sender: TObject; ARequest, AResponse: String);
begin
  case ARequest of
    REQ_RECORDING_PATH: ShowMessage('Not implemented.');
    else ShowMessage(Sender.ClassName + #32 + ARequest + #32 + AResponse);
  end;
end;

procedure TForm1.RecordingStarted(Sender: TObject; AMultipartMessage: TMPMessage
  );
begin
  ShowMessage(AMultipartMessage.Message.S[KEY_RECORDING_PATH] + 'stimulus_control' + PathDelim);
end;

procedure TForm1.CalibrationStopped(Sender: TObject;
  AMultipartMessage: TMPMessage);
begin
  BringToFront;
  ShowMessage('Calibration Stopped.');
end;

procedure TForm1.MultipartMessage(Sender: TObject; AMultipartMessage: TMPMessage
  );
begin
  case AMultipartMessage.Topic of
    NOTIFY_CALIBRATION_FAILED : ShowMessage('Calibration Failed!');
  end;
end;


end.


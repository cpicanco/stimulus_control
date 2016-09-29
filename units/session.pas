{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit session;

{$mode objfpc}{$H+}

{$DEFINE DEBUG}

interface

uses Classes, Controls, SysUtils, LCLIntf
     //,dialogs
     , constants
     , bass_player
     , config_session
     , countermanager
     , FileUtil
     , blocs
     , pupil_communication
     , timestamps
     ;

type

  { TSession }

  TSession = class(TComponent)
  private
    FAudioDevice: TBassAudioDevice;
    FBackGround: TWinControl;
    FBlc: TBlc;
    FConfigs: TCfgSes;
    FCrtReached : Boolean; // blc criteria achieved
    FGlobalContainer : TGlobalContainer;
    FManager : TCounterManager;
    FPupilClientEnabled: Boolean;
    FServerAddress: string;
    FSessName: string;
    FShowCounter : Boolean;
    FSubjName: string;
    FFilename : string;
    function GetTestMode: Boolean;
    procedure BkGndResponse(Sender: TObject);
    procedure EndBlc(Sender: TObject);
    procedure Consequence(Sender: TObject);
    procedure Criteria(Sender: TObject);
    procedure EndSess(Sender: TObject);
    procedure EndTrial(Sender: TObject);
    procedure Hit(Sender: TObject);
    procedure Miss(Sender: TObject);
    procedure Play; overload;
    procedure PlayBlc(Sender: TObject);
    procedure PupilMultipartReceived(Sender: TObject; AMultipart : TMPMessage);
    procedure PupilRecordingStarted(Sender: TObject; AMultipart : TMPMessage);
    procedure PupilRequestReceived(Sender: TObject; ARequest, AResponse : string);
    procedure SetBackGround(BackGround: TWinControl);
    procedure SetPupilClient(AValue: Boolean);
    procedure StmResponse(Sender:TObject);
    procedure SetTestMode(AValue: Boolean);
    procedure SetConfigs(AValue: TCfgSes);
    procedure SetManager(AValue: TCounterManager);
  private
    //FOnBeginSess: TNotifyEvent;
    //FOnBeginTrial: TNotifyEvent;
    //FOnCriteria: TNotifyEvent;
    FOnBkGndResponse: TNotifyEvent;
    FOnConsequence: TNotifyEvent;
    FOnEndBlc: TNotifyEvent;
    FOnEndSess: TNotifyEvent;
    FOnEndTrial: TNotifyEvent;
    FOnHit: TNotifyEvent;
    FOnMiss: TNotifyEvent;
    FOnStmResponse: TNotifyEvent;
    property PupilClientEnabled : Boolean read FPupilClientEnabled write SetPupilClient;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Play(AFilename: String); overload;
    property AudioDevice : TBassAudioDevice read FAudioDevice write FAudioDevice;
    property BackGround : TWinControl read FBackGround write SetBackGround;
    property SessName : String  read FSessName write FSessName;
    property ShowCounter : Boolean read FShowCounter write FShowCounter;
    property SubjName : String  read FSubjName write FSubjName;
    property TestMode : Boolean  read GetTestMode write SetTestMode;
    property Configs : TCfgSes read FConfigs write SetConfigs;
    property Manager : TCounterManager read FManager write SetManager;
  public
    property OnBkGndResponse : TNotifyEvent read FOnBkGndResponse write FOnBkGndResponse;
    property OnConsequence : TNotifyEvent read FOnConsequence write FOnConsequence;
    property OnEndBlc: TNotifyEvent read FOnEndBlc write FOnEndBlc;
    property OnEndSess: TNotifyEvent read FOnEndSess write FOnEndSess;
    property OnEndTrial : TNotifyEvent read FOnEndTrial write FOnEndTrial;
    property OnHit: TNotifyEvent read FOnHit write FOnHit;
    property OnMiss: TNotifyEvent read FOnMiss write FOnMiss;
    property OnStmResponse : TNotifyEvent read FOnStmResponse write FOnStmResponse;
  end;

implementation

uses
  data_logger
{$ifdef DEBUG}
  , debug_logger
{$endif}
  ;

resourcestring
  HSUBJECT_NAME      = 'Nome_Sujeito:';
  HSESSION_NAME      = 'Nome_Sessão:';
  HBEGIN_TIME        = 'Início:';
  HEND_TIME          = 'Término:';
  HSESSION_CANCELED  = '----------Sessão Cancelada----------';
  HTEST_MODE         = '(Modo de Teste)';

procedure TSession.BkGndResponse(Sender: TObject);
begin
  if Assigned(OnBkGndResponse) then FOnBkGndResponse(Sender);
end;

procedure TSession.EndBlc(Sender: TObject);
begin
  case Configs.SessionType of
    T_CIC : if FBlc.NextBlc = T_END then
              Manager.CurrentBlc := Configs.NumBlc
            else
              Manager.OnEndBlc(Sender);

    T_CRT:  if FCrtReached then
              Manager.OnEndBlc(Sender)
            else
              Manager.CurrentBlc := Configs.NumBlc;
  end;

  if Assigned(OnEndBlc) then FOnEndBlc(Sender);
  PlayBlc(Sender);
end;

procedure TSession.EndSess(Sender: TObject);
var Footer : string;
begin
  Footer := HEND_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time)+ LineEnding;
  if PupilClientEnabled then
    begin
      FGlobalContainer.PupilClient.Request(REQ_SHOULD_STOP_RECORDING);
      CopyFile(FConfigs.Filename,FGlobalContainer.RootData + ExtractFileName(FConfigs.Filename));
    end;

  Sleep(100);
  FreeLogger(LGData, Footer);
  FreeLogger(LGTimestamps,Footer);
  if Assigned(OnEndSess) then FOnEndSess(Sender);
end;

procedure TSession.EndTrial(Sender: TObject);
begin
   if Assigned(OnEndTrial) then FOnEndTrial (Sender);
end;

procedure TSession.Hit(Sender: TObject);
begin
  if Assigned(OnHit) then FOnHit (Sender);
end;

procedure TSession.Miss(Sender: TObject);
begin
  if Assigned(OnMiss) then FOnMiss (Sender);
end;

procedure TSession.Play;
var
  LHeader : string;
begin
  if (FFilename = #0) or (FFilename = '') then
    FFilename := '000';

  LHeader := HSUBJECT_NAME + #9 + FSubjName + LineEnding +
             HSESSION_NAME + #9 + FSessName + LineEnding +
             HBEGIN_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time)+ LineEnding + LineEnding;

  FFilename := FGlobalContainer.RootData + FFilename;
  CreateLogger(LGData, FFilename, LHeader);
  CreateLogger(LGTimestamps, FFilename, LHeader);

  { TODO -oRafael -cdebug : if test mode then }
  // if TestMode then
  //

  FBlc.SaveData := GetSaveDataProc(LGData);
  FBlc.SaveTData := GetSaveDataProc(LGTimestamps);
  FBlc.BackGround:= FBackGround;
  FBlc.ShowCounter := FShowCounter;
  FManager.OnBeginSess(Self);
  PlayBlc(Self);
end;

procedure TSession.PupilMultipartReceived(Sender: TObject;
  AMultipart: TMPMessage);
begin
  {$ifdef DEBUG}
    DebugLn(mt_Information + AMultipart.Topic );
  {$endif}
end;

procedure TSession.PupilRecordingStarted(Sender: TObject;
  AMultipart: TMPMessage);
begin
  {$ifdef DEBUG}
    DebugLn(mt_Debug +  AMultipart.Message.S[KEY_RECORDING_PATH] + 'stimulus_control' + PathDelim);
  {$endif}
  FGlobalContainer.RootData := AMultipart.Message.S[KEY_RECORDING_PATH] + 'stimulus_control' + PathDelim;
  Play;
end;

procedure TSession.PupilRequestReceived(Sender: TObject; ARequest,
  AResponse: string);
begin
  {$ifdef DEBUG}
    DebugLn(mt_Information + ARequest + #32 + AResponse);
  {$endif}
end;

procedure TSession.SetBackGround(BackGround: TWinControl);
begin
  FBackGround:= BackGround;
end;

procedure TSession.SetPupilClient(AValue: Boolean);
begin
  if FPupilClientEnabled=AValue then Exit;
  if AValue then
    begin
      FGlobalContainer.PupilClient := TPupilCommunication.Create(FServerAddress);
    end
  else FGlobalContainer.PupilClient.Terminate;

  FPupilClientEnabled:=AValue;
end;

procedure TSession.StmResponse(Sender: TObject);
begin
  if Assigned(OnStmResponse) then FOnStmResponse (Sender);
end;

procedure TSession.SetTestMode(AValue: Boolean);
begin
  if FGlobalContainer.TestMode=AValue then Exit;
  FGlobalContainer.TestMode:=AValue;
end;

procedure TSession.SetConfigs(AValue: TCfgSes);
begin
  if FConfigs=AValue then Exit;
  FConfigs:=AValue;
  FSessName := FConfigs.Name;
  FSubjName := FConfigs.SessionSubject;
  FServerAddress := FConfigs.SessionServer;
  FGlobalContainer := FConfigs.GlobalContainer;
  PupilClientEnabled := FGlobalContainer.PupilEnabled;
end;

procedure TSession.SetManager(AValue: TCounterManager);
begin
  if FManager=AValue then Exit;
  FManager:=AValue;
end;

function TSession.GetTestMode: Boolean;
begin
  Result := FGlobalContainer.TestMode;
end;

procedure TSession.Consequence(Sender: TObject);
begin
  if Assigned(OnConsequence) then FOnConsequence (Sender);
end;

constructor TSession.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FManager := TCounterManager.Create(Self);
  FPupilClientEnabled := False;

  FBlc:= TBlc.Create(Self);
  FBlc.OnStmResponse:= @StmResponse;
  FBlc.OnConsequence:= @Consequence;
  FBlc.OnBkGndResponse:= @BkGndResponse;
  FBlc.OnEndBlc:= @EndBlc;
  FBlc.OnEndTrial:= @EndTrial;
  FBlc.OnHit:= @Hit;
  FBlc.OnMiss:= @Miss;
  FBlc.OnCriteria := @Criteria;
end;

procedure TSession.Criteria(Sender: TObject);
begin
  FCrtReached := True;
end;

destructor TSession.Destroy;
begin
  //external objects
  FBackGround := nil;
  FAudioDevice := nil;

  //internal objects
  PupilClientEnabled := False;
  inherited;
end;

procedure TSession.Play(AFilename: String);
begin
  FFilename := AFilename;
  FGlobalContainer.TimeStart := TickCount;

  if PupilClientEnabled then
    begin
      with FGlobalContainer.PupilClient do
        begin
          OnRequestReceived := @PupilRequestReceived;
          OnMultiPartMessageReceived := @PupilMultipartReceived;
          OnRecordingStarted := @PupilRecordingStarted;
          //OnCalibrationSuccessful := @PupilCalibrationSuccessful;
          Start;
          StartSubscriber(True);
          Subscribe(SUB_ALL_NOTIFICATIONS);
          Request(REQ_SYNCHRONIZE_TIME+#32+TimestampToStr(FGlobalContainer.TimeStart));
          Request(REQ_SHOULD_START_RECORDING); // must be non-blocking
        end
    end
  else Play;
end;

procedure TSession.PlayBlc(Sender: TObject);
begin
  if Manager.CurrentBlc < Configs.NumBlc then
    begin
      Manager.SetVirtualTrialValue(Configs.Blcs[Manager.CurrentBlc].VirtualTrialValue);
      FBlc.Play(Configs.Blc[Manager.CurrentBlc], FManager, FGlobalContainer)
    end
  else EndSess(Sender);
end;



end.

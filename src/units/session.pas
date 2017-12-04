{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Session;

{$mode objfpc}{$H+}

interface

uses Classes, Controls, SysUtils, LCLIntf, FileUtil
     //,dialogs
     , constants
     , Audio.Bass_nonfree
     , Session.Configuration
     , countermanager
     , Session.Blocs
     {$IFNDEF NO_LIBZMQ}
     , Pupil.Client
     {$ENDIF}
     , Timestamps
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
    {$IFNDEF NO_LIBZMQ}
    procedure PupilMultipartReceived(Sender: TObject; APupilMessage : TPupilMessage);
    procedure PupilRecordingStarted(Sender: TObject; APupilMessage : TPupilMessage);
    {$ENDIF}
    procedure PupilReplyReceived(Sender: TObject; ARequest, AReply : string);
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
    FOnTrialEnd: TNotifyEvent;
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
    property OnTrialEnd : TNotifyEvent read FOnTrialEnd write FOnTrialEnd;
    property OnHit: TNotifyEvent read FOnHit write FOnHit;
    property OnMiss: TNotifyEvent read FOnMiss write FOnMiss;
    property OnStmResponse : TNotifyEvent read FOnStmResponse write FOnStmResponse;
  end;

implementation

uses
  Loggers.Reports
{$ifdef DEBUG}
  , Loggers.Debug
{$endif}
  ;

resourcestring
  HSUBJECT_NAME      = 'Nome_Sujeito:';
  HSESSION_NAME      = 'Nome_Sessão:';
  HFIRST_TIMESTAMP   = 'Primeira_Timestamp:';
  HBEGIN_TIME        = 'Início:';
  HEND_TIME          = 'Término:';
//  HSESSION_CANCELED  = '----------Sessão Cancelada----------';
  HTEST_MODE         = '(Modo de Teste)';

procedure TSession.BkGndResponse(Sender: TObject);
begin
  if Assigned(OnBkGndResponse) then FOnBkGndResponse(Sender);
end;

procedure TSession.EndBlc(Sender: TObject);
var
  LLastBlcIndex : integer;
  LLastBlc : TCfgBlc;
begin
  if Assigned(OnEndBlc) then FOnEndBlc(Sender);
  LLastBlcIndex := Manager.CurrentBlc;
  LLastBlc := Configs.CurrentBlc;
  Manager.OnEndBlc(Sender); // go to the next blc

  case Configs.SessionType of
    T_CIC : // go to next blc always, except if user defined an end alias
      if FBlc.NextBlc = T_END then
        Manager.CurrentBlc := Configs.NumBlc;

    T_CRT: // go to next blc if criteria else end session
      if not FCrtReached then
        Manager.CurrentBlc := Configs.NumBlc;

    T_CND: // if not defined go to the next bloc, if defined go to user defined blc
      begin
        if FCrtReached then
          begin
            if LLastBlc.NextBlocOnCriteria > 0 then
              Manager.CurrentBlc := LLastBlc.NextBlocOnCriteria-1
            else // use negative numbers to count backwards
              if LLastBlc.NextBlocOnCriteria < 0 then
                Manager.CurrentBlc :=  (Configs.NumBlc+1) - LLastBlc.NextBlocOnCriteria;
          end
        else
          begin
            if LLastBlc.NextBlocOnNotCriteria > 0 then
              Manager.CurrentBlc := LLastBlc.NextBlocOnNotCriteria-1
            else // use negative numbers to count backwards
              if LLastBlc.NextBlocOnNotCriteria < 0 then
                Manager.CurrentBlc :=  (Configs.NumBlc+1) - LLastBlc.NextBlocOnNotCriteria;
          end;
      end;
  end;

  // increment blc (successive) repetitions
  if Manager.CurrentBlc < Configs.NumBlc then
    if LLastBlcIndex = Manager.CurrentBlc then
      begin
        Manager.OnBlcRepetition(Sender);
        if Manager.BlcRepetitions >= Configs.CurrentBlc.MaxBlcRepetition then
          Manager.CurrentBlc := Configs.NumBlc;
      end
    else
      Manager.BlcRepetitions := 0;

  if FCrtReached then
    FCrtReached := False;

  PlayBlc(Sender);
end;

procedure TSession.EndSess(Sender: TObject);
var
  Footer : string;
  UserCalibrationData : string = 'pupil_capture_settings'+PathDelim+'user_calibration_data';
begin
  Footer := HEND_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time)+ LineEnding;
  {$IFNDEF NO_LIBZMQ}
  if PupilClientEnabled then
    begin
      FGlobalContainer.PupilClient.Request(REQ_SHOULD_STOP_RECORDING);
      CopyFile(FConfigs.Filename,FGlobalContainer.RootData + ExtractFileName(FConfigs.Filename));
      {$IFDEF LINUX}
      UserCalibrationData := GetEnvironmentVariable('HOME')+PathDelim+UserCalibrationData;
      if FileExists(UserCalibrationData) then
        CopyFile(UserCalibrationData, FGlobalContainer.RootData + ExtractFileName(UserCalibrationData));
      {$ENDIF}
    end;
  {$ENDIF}

  Sleep(100);
  FreeLogger(LGData, Footer);
  FreeLogger(LGTimestamps,Footer);
  if Assigned(OnEndSess) then FOnEndSess(Sender);
end;

procedure TSession.EndTrial(Sender: TObject);
begin
   if Assigned(OnTrialEnd) then FOnTrialEnd (Sender);
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
             HFIRST_TIMESTAMP + #9 + TimestampToStr(FGlobalContainer.TimeStart) + LineEnding +
             HBEGIN_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time) + LineEnding + LineEnding;

  FFilename := FGlobalContainer.RootData + FFilename;
  CreateLogger(LGData, FFilename, LHeader);
  CreateLogger(LGTimestamps, FFilename, LHeader);

  FBlc.SaveData := GetSaveDataProc(LGData);
  FBlc.SaveTData := GetSaveDataProc(LGTimestamps);
  FBlc.BackGround:= FBackGround;
  FBlc.ShowCounter := FShowCounter;

  if TestMode then
    begin
     FBlc.SaveData(HTEST_MODE);
     FBlc.SaveTData(HTEST_MODE);
    end;

  FManager.OnBeginSess(Self);
  PlayBlc(Self);
end;
{$IFNDEF NO_LIBZMQ}
procedure TSession.PupilMultipartReceived(Sender: TObject;
  APupilMessage: TPupilMessage);
begin
  {$ifdef DEBUG}
    DebugLn(mt_Information + APupilMessage.Topic );
  {$endif}
end;

procedure TSession.PupilRecordingStarted(Sender: TObject;
  APupilMessage: TPupilMessage);
begin
  FGlobalContainer.RootData := APupilMessage.Payload.S[KEY_RECORDING_PATH] + 'stimulus_control' + PathDelim;
  Play;
end;
{$ENDIF}

procedure TSession.PupilReplyReceived(Sender: TObject; ARequest,
  AReply: string);
begin
  {$ifdef DEBUG}
    DebugLn(mt_Information + ARequest + #32 + AReply);
  {$endif}
end;

procedure TSession.SetBackGround(BackGround: TWinControl);
begin
  FBackGround:= BackGround;
end;

procedure TSession.SetPupilClient(AValue: Boolean);
begin
  if FPupilClientEnabled=AValue then Exit;
  {$IFNDEF NO_LIBZMQ}
  if AValue then
    begin
      FGlobalContainer.PupilClient := TPupilClient.Create(FServerAddress);
    end
  else FGlobalContainer.PupilClient.Terminate;

  FPupilClientEnabled:=AValue;
  {$ELSE}
  FPupilClientEnabled:=False;
  {$ENDIF}
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
  FGlobalContainer.CounterManager := FManager;
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
  FBlc.OnTrialEnd:= @EndTrial;
  FBlc.OnHit:= @Hit;
  FBlc.OnMiss:= @Miss;
  FBlc.OnCriteria := @Criteria;
end;

procedure TSession.Criteria(Sender: TObject);
begin
  FCrtReached := True;
  case Configs.SessionType of
    T_CIC, T_CRT:
      Manager.CurrentTrial := Configs.CurrentBlc.NumTrials;
    T_CND: ; // for now, do nothing
  end;
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
  FGlobalContainer.TimeStart := 0;
  //FGlobalContainer.TimeStart := TickCount;

  if PupilClientEnabled then
    begin
      {$IFNDEF NO_LIBZMQ}
      with FGlobalContainer.PupilClient do
        begin
          OnReplyReceived := @PupilReplyReceived;
          OnMultiPartMessageReceived := @PupilMultipartReceived;
          OnRecordingStarted := @PupilRecordingStarted;
          //OnCalibrationSuccessful := @PupilCalibrationSuccessful;
          Start;
          StartSubscriber(True);
          Subscribe(SUB_ALL_NOTIFICATIONS);
          Request(REQ_SHOULD_START_RECORDING);
        end
      {$ENDIF}
    end
  else Play;
end;

procedure TSession.PlayBlc(Sender: TObject);
begin
  if Manager.CurrentBlc < Configs.NumBlc then
    begin
      Manager.SetVirtualTrialValue(Configs.CurrentBlc.VirtualTrialValue);
      FBlc.Play(Configs.CurrentBlc, FManager, FGlobalContainer)
    end
  else EndSess(Sender);
end;



end.

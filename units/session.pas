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
     , regdata
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
    FCrtReached : Boolean;
    FDataTicks: Boolean;
    FGlobalContainer : TGlobalContainer;
    FManager : TCounterManager;
    FPupilClientEnabled: Boolean;
    FRegData: TRegData;
    FRegDataTicks: TRegData;
    FServerAddress: string;
    FSessName: string;
    FShowCounter : Boolean;
    FSubjName: string;
    FFileData : string;
    function GetTestMode: Boolean;
    procedure BkGndResponse(Sender: TObject);
    procedure BlcEndBlc(Sender: TObject);
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
    procedure SetManager(AValue: TCounterManager);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoEndSess(Sender: TObject);
    procedure Play(AFileData: String); overload;
    property PupilClientEnabled : Boolean read FPupilClientEnabled write SetPupilClient;
    property AudioDevice : TBassAudioDevice read FAudioDevice write FAudioDevice;
    property BackGround : TWinControl read FBackGround write SetBackGround;
    property ServerAddress : string read FServerAddress write FServerAddress;
    property SessName : String  read FSessName write FSessName;
    property ShowCounter : Boolean read FShowCounter write FShowCounter;
    property SubjName : String  read FSubjName write FSubjName;
    property TestMode : Boolean  read GetTestMode write SetTestMode;
    property DataTicks : Boolean read FDataTicks write FDataTicks;
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
  timestamps_logger
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

procedure TSession.BlcEndBlc(Sender: TObject);
begin
  case Configs.SessionType of
  T_CIC:
    begin
      if FBlc.NextBlc = 'END' then Manager.CurrentBlc := Configs.NumBlc //ir para último bloco apenas se END constar
      else  //próximo bloco sob qualquer outra condição
        begin
          Manager.OnEndBlc(Sender)
        end;
    end;

  T_CRT:
    begin
      if not FCrtReached then Manager.CurrentBlc := Configs.NumBlc
      else  //próximo bloco apenas se o critério foi alcançado
        begin
          Manager.OnEndBlc(Sender)
        end;
    end;

  end;

  if Assigned(OnEndBlc) then FOnEndBlc(Sender);
  PlayBlc(Sender);
end;

procedure TSession.EndSess(Sender: TObject);
var Footer : string;
begin
  Footer := LineEnding + LineEnding+ HEND_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time)+ LineEnding;
  if PupilClientEnabled then
     FGlobalContainer.PupilClient.Request(REQ_SHOULD_STOP_RECORDING);

  FRegData.SaveData(Footer);
  SetLogger(Footer);
  if DataTicks then
    FRegDataTicks.SaveData(Footer);

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
var Header : string;
begin
  if FFileData = #0 then FFileData := '000.data';
    if TestMode then
      begin
        FSessName:= FSessName + #9 + HTEST_MODE;
        FFileData:= '000.test';
      end;

  Header := HSUBJECT_NAME + #9 + FSubjName + LineEnding +
            HSESSION_NAME + #9 + FSessName + LineEnding +
            HBEGIN_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time)+ LineEnding+LineEnding;

  FRegData := TRegData.Create(Self, FGlobalContainer.RootData + FFileData);

  FRegData.SaveData(Header);
  SetLogger(ExtractFileNameWithoutExt(FRegData.FileName) + '.timestamps', Header);

  if DataTicks then
    begin
      FRegDataTicks:= TRegData.Create(Self, ExtractFileNameWithoutExt(FRegData.FileName) + '.ticks');
      FBlc.RegDataTicks := FRegDataTicks;
      FRegDataTicks.SaveData(Header);
    end;

  FBlc.ShowCounter := ShowCounter;
  FBlc.RegData:= FRegData;
  FBlc.BackGround:= FBackGround;

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
  //case ARequest of
  //  REQ_SHOULD_START_RECORDING : Play;
  //end;
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
  FGlobalContainer := FConfigs.GlobalContainer;
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
  FBlc.OnEndTrial:= @EndTrial;
  FBlc.OnHit:= @Hit;
  FBlc.OnMiss:= @Miss;
  FBlc.OnEndBlc:= @BlcEndBlc;
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

procedure TSession.DoEndSess(Sender: TObject);
var Footer : string;
begin
  Footer := LineEnding + LineEnding + HSESSION_CANCELED + LineEnding + HEND_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time)+ LineEnding;
  if DataTicks then
    FRegDataTicks.SaveData(Footer);

  FRegData.SaveData(Footer);
  EndSess(Sender);
end;

procedure TSession.Play(AFileData: String);
begin
  FFileData := AFileData;
  FGlobalContainer.TimeStart := TickCount;

  if PupilClientEnabled then
    begin
      with FGlobalContainer.PupilClient do
        begin
          OnRequestReceived := @PupilRequestReceived;
          OnMultiPartMessageReceived := @PupilMultipartReceived;
          OnRecordingStarted := @PupilRecordingStarted;
          //OnCalibrationStopped := @PupilCalibrationStopped;
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

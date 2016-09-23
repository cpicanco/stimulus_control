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
     , bass_player
     , config_session
     , countermanager
     , FileUtil
     , regdata
     , blocs
     , pupil_communication
     ;

type

  { TSession }

  TSession = class(TComponent)
  private
    //FOnBeginSess: TNotifyEvent;
    //FOnBeginTrial: TNotifyEvent;
    //FOnCriteria: TNotifyEvent;
    FAudioDevice: TBassAudioDevice;
    FBackGround: TWinControl;
    FBlc: TBlc;
    FCfgSes: TCfgSes;
    FCrtReached : Boolean;
    FDataTicks: Boolean;
    FManager : TCounterManager;
    FPupilClient : TPupilCommunication;
    FPupilClientEnabled: Boolean;
    FRegData: TRegData;
    FRegDataTicks: TRegData;
    FServerAddress: string;
    FSessName: string;
    FShowCounter : Boolean;
    FSubjName: string;
    FTestMode: Boolean;
    FFileData : string;
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
    procedure PupilCalibrationStopped(Sender: TObject; AMultipart : TMPMessage);
    procedure SetBackGround(BackGround: TWinControl);
    procedure SetPupilClient(AValue: Boolean);
    procedure StmResponse(Sender:TObject);
  private
    FOnBkGndResponse: TNotifyEvent;
    FOnConsequence: TNotifyEvent;
    FOnEndBlc: TNotifyEvent;
    FOnEndSess: TNotifyEvent;
    FOnEndTrial: TNotifyEvent;
    FOnHit: TNotifyEvent;
    FOnMiss: TNotifyEvent;
    FOnStmResponse: TNotifyEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoEndSess(Sender: TObject);
    procedure Play(CfgSes: TCfgSes; Manager : TCounterManager; FileData: String); overload;
    property PupilClientEnabled : Boolean read FPupilClientEnabled write SetPupilClient;
    property AudioDevice : TBassAudioDevice read FAudioDevice write FAudioDevice;
    property BackGround : TWinControl read FBackGround write SetBackGround;
    property ServerAddress : string read FServerAddress write FServerAddress;
    property SessName : String  read FSessName write FSessName;
    property ShowCounter : Boolean read FShowCounter write FShowCounter;
    property SubjName : String  read FSubjName write FSubjName;
    property TestMode : Boolean  read FTestMode write FTestMode;
    property DataTicks : Boolean read FDataTicks write FDataTicks;
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
  if FCfgSes.SesType = 'CIC' then
    begin
      if FBlc.NextBlc = 'END' then FManager.CurrentBlc.Counter := FCfgSes.NumBlc //ir para último bloco apenas se END constar
      else  //próximo bloco sob qualquer outra condição
        begin
          FManager.OnEndBlc(Sender)
        end;
    end;
  if FCfgSes.SesType = 'CRT' then
    begin
      if not FCrtReached then FManager.CurrentBlc.Counter := FCfgSes.NumBlc
      else  //próximo bloco apenas se o critério foi alcançado
        begin
          FManager.OnEndBlc(Sender)
        end;
    end;

  if Assigned(OnEndBlc) then FOnEndBlc(Sender);
  PlayBlc(Sender);
end;

procedure TSession.EndSess(Sender: TObject);
var Footer : string;
begin
  Footer := LineEnding + LineEnding+ HEND_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time)+ LineEnding;
  FPupilClient.Request(REQ_SHOULD_STOP_RECORDING);

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
  if FFileData = #0 then FFileData:= '000.data';
    if FTestMode then
      begin
        FSessName:= FSessName + #9 + HTEST_MODE;
        FFileData:= '000.test';
      end;

  Header := HSUBJECT_NAME + #9 + FSubjName + LineEnding +
            HSESSION_NAME + #9 + FSessName + LineEnding +
            HBEGIN_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time)+ LineEnding+LineEnding;

  FRegData := TRegData.Create(nil, FCfgSes.RootData + FFileData);

  FRegData.SaveData(Header);
  SetLogger(ExtractFileNameWithoutExt(FRegData.FileName) + '.timestamps', Header);

  if DataTicks then
    begin
      FRegDataTicks:= TRegData.Create(nil, ExtractFileNameWithoutExt(FRegData.FileName) + '.ticks');
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
  FCfgSes.RootData := AMultipart.Message.S[KEY_RECORDING_PATH] + 'stimulus_control' + PathDelim;
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

procedure TSession.PupilCalibrationStopped(Sender: TObject;
  AMultipart: TMPMessage);
begin
  if Assigned(FBackGround) then FBackGround.BringToFront;
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
      FPupilClient := TPupilCommunication.Create(FServerAddress);
      FBlc.PupilClient := FPupilClient;
    end
  else FPupilClient.Terminate;

  FPupilClientEnabled:=AValue;
end;

procedure TSession.StmResponse(Sender: TObject);
begin
  if Assigned(OnStmResponse) then FOnStmResponse (Sender);
end;

procedure TSession.Consequence(Sender: TObject);
begin
  if Assigned(OnConsequence) then FOnConsequence (Sender);
end;

constructor TSession.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPupilClientEnabled := False;

  FBlc:= TBlc.Create(nil);
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
  //events
  FBlc.OnStmResponse := nil;
  FBlc.OnConsequence := nil;
  FBlc.OnBkGndResponse := nil;
  FBlc.OnEndTrial := nil;
  FBlc.OnHit := nil;
  FBlc.OnMiss := nil;
  FBlc.OnEndBlc := nil;
  FBlc.OnCriteria := nil;

  //external objects
  FBackGround := nil;
  FAudioDevice := nil;
  FManager := nil;
  FCfgSes := nil;

  //internal objects
  if PupilClientEnabled then PupilClientEnabled := False;
  if Assigned(FRegData) then FreeAndNil(FRegData);
  if Assigned(FRegDataTicks) then FreeAndNil(FRegDataTicks);
  if Assigned(FBlc) then FreeAndNil(FBlc);
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

procedure TSession.Play(CfgSes: TCfgSes; Manager : TCounterManager; FileData: String);
begin
  FCfgSes:= CfgSes;
  FManager := Manager;
  FFileData := FileData;

  if PupilClientEnabled then
  begin
    with FPupilClient do
      begin
        OnRequestReceived := @PupilRequestReceived;
        OnMultiPartMessageReceived := @PupilMultipartReceived;
        OnRecordingStarted := @PupilRecordingStarted;
        OnCalibrationStopped := @PupilCalibrationStopped;
        Start;
        StartSubscriber(True);
        Subscribe(SUB_ALL_NOTIFICATIONS);
        Request(REQ_SHOULD_START_RECORDING); // must be non-blocking
      end;
  end
    else Play;
end;

procedure TSession.PlayBlc(Sender: TObject);
var IndBlc, IndTrial : integer;
begin
  IndBlc := FManager.CurrentBlc.Counter;
  IndTrial := FManager.CurrentTrial.Counter;
  if IndBlc < FCfgSes.NumBlc then
    begin
      FManager.SetVirtualTrialValue(FCfgSes.Blcs[IndBlc].VirtualTrialValue);
      FBlc.Play(FCfgSes.CfgBlc[IndBlc], FManager, IndTrial, FTestMode)
    end
  else EndSess(Sender);
end;



end.

unit session;

{$mode objfpc}{$H+}

interface

uses Classes, Controls, SysUtils, LCLIntf,//dialogs,
     session_config,
     trial_mirrored_config, countermanager, regdata, blocs;

type

  TSession = class(TComponent)
  private
    FTimeStart : cardinal;
    FManager : TCounterManager;
    FCrtReached : Boolean;
    FBlc: TBlc;
    FRegData: TRegData;
    //FRegDataTicks: TRegData;
    FCfgSes: TCfgSes;
    FOnEndSess: TNotifyEvent;
    FSubjName: String;
    FSessName: String;
    FBackGround: TWinControl;
    FTestMode: Boolean;
    FShowCounter : Boolean;
    FOnStmResponse: TNotifyEvent;
    FOnEndTrial: TNotifyEvent;
    FOnBkGndResponse: TNotifyEvent;
    //FOnBeginTrial: TNotifyEvent;
    FOnEndBlc: TNotifyEvent;
    FOnConsequence: TNotifyEvent;
    //FOnBeginSess: TNotifyEvent;
    FOnHit: TNotifyEvent;
    FOnMiss: TNotifyEvent;
    //FOnCriteria: TNotifyEvent;
    procedure EndSess(Sender: TObject);
    procedure SetBackGround(BackGround: TWinControl);
    procedure BlcEndBlc(Sender: TObject);
    procedure StmResponse(Sender:TObject);
    procedure Consequence(Sender: TObject);
    procedure Hit(Sender: TObject);
    procedure Miss(Sender: TObject);
    procedure BkGndResponse(Sender: TObject);
    procedure EndTrial(Sender: TObject);
    procedure Criteria(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoEndSess(Sender: TObject);
    procedure Play(CfgSes: TCfgSes; Manager : TCounterManager; FileData: String);
    procedure PlayBlc(Sender: TObject);
    property BackGround: TWinControl read FBackGround write SetBackGround;
    property SessName: String  read FSessName write FSessName;
    property SubjName: String  read FSubjName write FSubjName;
    property TestMode: Boolean  read FTestMode write FTestMode;
    property ShowCounter : Boolean read FShowCounter write FShowCounter;
    property OnEndSess: TNotifyEvent read FOnEndSess write FOnEndSess;
    property OnStmResponse : TNotifyEvent read FOnStmResponse write FOnStmResponse;
    property OnBkGndResponse : TNotifyEvent read FOnBkGndResponse write FOnBkGndResponse;
    property OnConsequence : TNotifyEvent read FOnConsequence write FOnConsequence;
    property OnEndTrial : TNotifyEvent read FOnEndTrial write FOnEndTrial;
    property OnEndBlc: TNotifyEvent read FOnEndBlc write FOnEndBlc;
    property OnHit: TNotifyEvent read FOnHit write FOnHit;
    property OnMiss: TNotifyEvent read FOnMiss write FOnMiss;
  end;

implementation

procedure TSession.BkGndResponse(Sender: TObject);
begin
  If Assigned(OnBkGndResponse) then FOnBkGndResponse(Sender);
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

  If Assigned(OnEndBlc) then FOnEndBlc(Sender);
  PlayBlc(Sender);
end;

procedure TSession.EndSess(Sender: TObject);
begin
  FRegData.SaveData('Hora de Término:' + #9 + TimeToStr(Time) + #13#10);
  //FRegDataTicks.SaveData('Hora de Término:' + #9 + TimeToStr(Time) + #13#10);
  FRegData.Free;
  //FRegDataTicks.Free;
  If Assigned(OnEndSess) then FOnEndSess(Sender);
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

procedure TSession.SetBackGround(BackGround: TWinControl);
begin
  FBackGround:= BackGround;
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
  inherited Destroy;
end;

procedure TSession.DoEndSess(Sender: TObject);
begin
  //FRegDataTicks.SaveData(#13#10 + 'Sessão Cancelada' + #13#10);
  FRegData.SaveData(#13#10 + 'Sessão Cancelada' + #13#10);
  EndSess(Sender);
end;

procedure TSession.Play(CfgSes: TCfgSes; Manager : TCounterManager; FileData: String);
begin
  FCfgSes:= CfgSes;
  FManager := Manager;

  if FileData = #0 then FileData:= 'Dados_000.txt';
  if FTestMode then FSessName:= FSessName + #9 + '(Modo de Teste)';

  FRegData:= TRegData.Create(Self, FCfgSes.RootData + FileData);
  //FRegDataTicks:= TRegData.Create(Self, FCfgSes.RootData + 'Ticks_001.txt');

  FBlc.ShowCounter := ShowCounter;
  FBlc.RegData:= FRegData;
  //FBlc.RegDataTicks := FRegDataTicks;
  FBlc.BackGround:= FBackGround;

  FRegData.SaveData('Sujeito:' + #9 + FSubjName + #13#10 +
                    'Sessão:' + #9+ FSessName + #13#10 +
                    'Data:' + #9 + DateTimeToStr(Date)+ #13#10 +
                    'Hora de Início:' + #9 + TimeToStr(Time)+ #13#10 + #13#10);
  FTimeStart := GetTickCount;
  FBlc.TimeStart := FTimeStart;
  //FRegDataTicks.SaveData('Sujeito:' + #9 + FSubjName + #13#10 +
  //                  'Sessão:' + #9+ FSessName + #13#10 +
  //                  'Data:' + #9 + DateTimeToStr(Date)+ #13#10 +
  //                  'Hora de Início:' + #9 + TimeToStr(Time)+ #13#10 + #13#10);
  FManager.OnBeginSess(Self);
  PlayBlc(Self);
end;

procedure TSession.PlayBlc(Sender: TObject);
var IndBlc, IndTrial : integer;
begin
  IndBlc := FManager.CurrentBlc.Counter;
  IndTrial := FManager.CurrentTrial.Counter;
  FManager.SetVirtualTrialValue(FCfgSes.Blcs[IndBlc].VirtualTrialValue);

  if IndBlc < FCfgSes.NumBlc then FBlc.Play(FCfgSes.CfgBlc[IndBlc], FManager, IndTrial, FTestMode)
  else EndSess(Sender);
end;

end.



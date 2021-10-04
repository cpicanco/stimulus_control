{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit CounterManager;

{$mode objfpc}{$H+}

interface

uses Classes, Dialogs, SysUtils;

type

  { TCounterManager }

  {

    Requires refactoring

  }
  TCounterManager = class(TComponent)
  private
    FBlcCscHitsType1: integer;
    FBlcCscHitsType2: integer;
    FBlcPoints: integer;
    FOnRepeatBlc: TNotifyEvent;
  {Current Session}
    FSessionBackGroundResponses : integer;
    FSessionConsequences : integer;
    FSessionCorrections : integer;
    FSessionCscCorrections : integer;
    FSessionCscHits : integer;
    FSessionCscMisses : integer;
    FSessionCscNones : integer;
    FSessionHighCscCorrections : integer;
    FSessionHighCscHits : integer;
    FSessionHighCscMisses : integer;
    FSessionHighCscNones : integer;
    FSessionHits : integer;
    FSessionMisses : integer;
    FSessionNones : integer;
    FSessionPoints : integer;
    FSessionPoints2 : integer;
    FSessionStmResponses : integer;
    FSessionTrials : integer;
    FSessionVirtualHits : integer;
    FCurrentBlc : integer;
  {Current Blc}
    FBlcTrials : integer;
    FBlcRepetitions : integer;
    FBlcBkGndResponses : integer;
    FBlcConsequences : integer;
    FBlcCorrections : integer;
    FBlcCscCorrections : integer;
    FBlcCscHits : integer;
    FBlcCscMisses : integer;
    FBlcCscNones : integer;
    FBlcCsqHits : integer;
    FBlcHighCscCorrections : integer;
    FBlcHighCscHits : integer;
    FBlcHighCscMisses : integer;
    FBlcHighCscNones : integer;
    FBlcHits : integer;
    FBlcMisses : integer;
    FBlcNones : integer;
    FBlcStmResponses : integer;
    FBlcVirtualHitLoop : integer;
    FBlcVirtualTrial : integer;
    FBlcVirtualTrialLoop : integer;
    FBlcVirtualTrialValue : Integer;
    FCurrentTrial : integer;
  {Current Trial}
    FTrialBkGndResponses : integer;
    FTrialStmResponses : integer;
  {Current Stm}
    FStmCounter : integer;
  {Events}
    //FOnBlcRepetition: TNotifyEvent;
    FOnBeginBlc: TNotifyEvent;
    FOnBeginSess: TNotifyEvent;
    FOnBeginTrial: TNotifyEvent;
    FOnBkGndResponse: TNotifyEvent;
    FOnConsequence: TNotifyEvent;
    FOnCorrection: TNotifyEvent;
    FOnCsqCriterion: TNotifyEvent;
    FOnEndBlc: TNotifyEvent;
    FOnEndSess: TNotifyEvent;
    FOnTrialEnd: TNotifyEvent;
    FOnHit: TNotifyEvent;
    FOnHitResult: integer;
    FOnMiss: TNotifyEvent;
    FOnNotCorrection: TNotifyEvent;
    FOnNxtTrial: TNotifyEvent;
    FOnStmResponse: TNotifyEvent;
    procedure BeginBlc(Sender : TObject);
    procedure BeginSess(Sender : TObject);
    procedure BeginTrial(Sender : TObject);
    procedure BkGndResponse(Sender : TObject);
    procedure Consequence(Sender : TObject);
    procedure Correction(Sender : TObject);
    procedure CsqCriterion(Sender : TObject);
    procedure CustomNxtTrial(Sender : TObject);
    procedure EndBlc(Sender : TObject);
    procedure RepeatBlc(Sender : TObject);
    procedure EndSess(Sender : TObject);
    procedure EndTrial(Sender : TObject);
    procedure Hit(Sender : TObject);
    procedure Miss(Sender : TObject);
    procedure NotCorrection(Sender : TObject);
    procedure StmResponse(Sender : TObject);
    //procedure BlcRepeated(Sender : TObject);
    //procedure SetOnBlcRepetition(AValue: TNotifyEvent);
  public
    constructor Create (AOwner : TComponent); override;
    function HitPorcentage(AGlobal : Boolean = False) : integer;
    procedure SetCurrentTrial(ATrial : integer);
    procedure _VirtualTrialFix;
    procedure SetVirtualTrialValue(Value : integer);
    property CurrentBlc : integer read FCurrentBlc write FCurrentBlc;
    property CurrentTrial : integer read FCurrentTrial write SetCurrentTrial;
  {Current Trial}
    property TrialStmResponses  : integer read FTrialStmResponses;
    property TrialBkGndResponses : integer read FTrialBkGndResponses;
  {Current Session}
    property SessionBkGndResponses : integer read FSessionBackGroundResponses;
    property SessionConsequences : integer read FSessionConsequences;
    property SessionCorrections : integer read FSessionCorrections;
    property SessionCscCorrections : integer read FSessionCscCorrections;
    property SessionCscHits : integer read FSessionCscHits;
    property SessionCscMisses : integer read FSessionCscMisses;
    property SessionCscNones : integer read FSessionCscNones;
    property SessionHighCscCorrections : integer read FSessionHighCscCorrections;
    property SessionHighCscHits : integer read FSessionHighCscHits;
    property SessionHighCscMisses : integer read FSessionHighCscMisses;
    property SessionHighCscNones : integer read FSessionHighCscNones;
    property SessionHits : integer read FSessionHits;
    property SessionMisses : integer read FSessionMisses;
    property SessionNones : integer read FSessionNones;
    property SessionStmResponses : integer read FSessionStmResponses;
    property SessionTrials : integer read FSessionTrials;
    property SessionPointsTopRight : integer read FSessionPoints write FSessionPoints;
    property SessionPointsTopLeft : integer read FSessionPoints2 write FSessionPoints2;
    property BlcTrials : integer read FBlcTrials;
    property SessionVirtualHits : integer read FSessionVirtualHits;
  {Current Blc}
    property BlcVirtualTrialValue : Integer read FBlcVirtualTrialValue;
    property BlcVirtualHitLoop : integer read FBlcVirtualHitLoop write FBlcVirtualHitLoop;
    property BlcVirtualTrialLoop : integer read FBlcVirtualTrialLoop write FBlcVirtualTrialLoop;
    property BlcVirtualTrial : integer read FBlcVirtualTrial;
    property BlcBkGndResponses : integer read FBlcBkGndResponses;
    property BlcStmResponses : integer read FBlcStmResponses;
    property BlcConsequences : integer read FBlcConsequences;
    property BlcCorrections : integer read FBlcCorrections;
    property BlcCscCorrections : integer read FBlcCscCorrections;
    property BlcHighCscCorrections : integer read FBlcHighCscCorrections;
    property BlcCsqHits : integer read FBlcCsqHits write FBlcCsqHits;
    property BlcHits : integer read FBlcHits;
    property BlcCscHits : integer read FBlcCscHits;
    property BlcHighCscHits : integer read FBlcHighCscHits;
    property BlcMisses : integer read FBlcMisses;
    property BlcCscMisses : integer read FBlcCscMisses;
    property BlcHighCscMisses : integer read FBlcHighCscMisses;
    property BlcNones : integer read FBlcNones;
    property BlcCscNones : integer read FBlcCscNones;
    property BlcHighCscNones : integer read FBlcHighCscNones;
    property BlcRepetitions : integer read FBlcRepetitions write FBlcRepetitions;
    property BlcPoints : integer read FBlcPoints write FBlcPoints;
  {Each Stm}
    property StmCounter : integer read FStmCounter;
  {Events}
    property OnBeginSess: TNotifyEvent read FOnBeginSess write FOnBeginSess;
    property OnBeginBlc: TNotifyEvent read FOnBeginBlc write FOnBeginBlc;
    property OnBeginTrial : TNotifyEvent read FOnBeginTrial write FOnBeginTrial;
    property OnCorrection : TNotifyEvent read FOnCorrection write FOnCorrection;
    property OnNotCorrection : TNotifyEvent read FOnNotCorrection write FOnNotCorrection;
    property OnBkGndResponse : TNotifyEvent read FOnBkGndResponse write FOnBkGndResponse;
    property OnConsequence : TNotifyEvent read FOnConsequence write FOnConsequence;
    property OnEndBlc: TNotifyEvent read FOnEndBlc write FOnEndBlc;
    property OnRepeatBlc: TNotifyEvent read FOnRepeatBlc write FOnRepeatBlc;
    property OnEndSess: TNotifyEvent read FOnEndSess write FOnEndSess;
    property OnTrialEnd : TNotifyEvent read FOnTrialEnd write FOnTrialEnd;
    property OnNxtTrial : TNotifyEvent read FOnNxtTrial write FOnNxtTrial;
    property OnHit: TNotifyEvent read FOnHit write FOnHit;
    property OnHitResult : integer read FOnHitResult;
    property OnMiss: TNotifyEvent read FOnMiss write FOnMiss;
    property OnStmResponse : TNotifyEvent read FOnStmResponse write FOnStmResponse;
    property OnCsqCriterion : TNotifyEvent read FOnCsqCriterion write FOnCsqCriterion;
    //property OnBlcRepetition : TNotifyEvent read FOnBlcRepetition write SetOnBlcRepetition;
  end;

implementation

{ TCounterManager }

constructor TCounterManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { Session - 17 }
  FSessionBackGroundResponses := 0;
  FSessionStmResponses := 0;
  FSessionConsequences := 0;
  FSessionCorrections := 0;
  FSessionCscCorrections := 0;
  FSessionHighCscCorrections := 0;
  FSessionHits := 0;
  FSessionCscHits := 0;
  FSessionHighCscHits := 0;
  FSessionMisses := 0;
  FSessionCscMisses := 0;
  FSessionHighCscMisses := 0;
  FSessionNones := 0;
  FSessionCscNones := 0;
  FSessionHighCscNones := 0;
  FSessionTrials := 0;
  FCurrentBlc := 0;
  FSessionVirtualHits := 0;
  FSessionPoints := 0;
  { Blocs - 20 }
  FBlcVirtualHitLoop := 0;
  FBlcVirtualTrial := 0;
  FBlcVirtualTrialLoop := 0;
  FBlcBkGndResponses := 0;
  FBlcStmResponses := 0;
  FBlcConsequences := 0;
  FBlcCorrections := 0;
  FBlcCscCorrections := 0;
  FBlcHighCscCorrections := 0;
  FBlcCsqHits := 0;
  FBlcHits := 0;
  FBlcCscHits := 0;
  FBlcCscHitsType1 := 0;
  FBlcCscHitsType2 := 0;
  FBlcHighCscHits := 0;
  FBlcMisses := 0;
  FBlcCscMisses := 0;
  FBlcHighCscMisses := 0;
  FBlcNones := 0;
  FBlcCscNones := 0;
  FBlcHighCscNones := 0;
  FCurrentTrial := 0;
  FBlcPoints := 0;
{ SessionTrials }
  FTrialStmResponses := 0;

  { events }

  OnBeginBlc := @BeginBlc;
  OnBeginSess := @BeginSess;
  OnBeginTrial := @BeginTrial;
  OnBkGndResponse := @BkGndResponse;
  OnConsequence := @Consequence;
  OnCorrection := @Correction;
  OnEndBlc := @EndBlc;
  OnRepeatBlc := @RepeatBlc;
  OnEndSess := @EndSess;
  OnTrialEnd := @EndTrial;
  OnNxtTrial := @CustomNxtTrial;
  OnHit := @Hit;
  OnMiss := @Miss;
  OnStmResponse := @StmResponse;
  OnCsqCriterion := @CsqCriterion;
  OnNotCorrection := @NotCorrection;
  //OnBlcRepetition := @BlcRepeated;
end;

function TCounterManager.HitPorcentage(AGlobal: Boolean): integer;
var
  LHits : integer;
  LBaseTrials : integer;
begin
  if AGlobal then
    begin
      LHits := SessionHits;
      LBaseTrials := SessionTrials;
    end
  else
    begin
      LHits := BlcHits;
      LBaseTrials := BlcTrials;
    end;
  Result := (LHits*100) div LBaseTrials;
end;

procedure TCounterManager.SetCurrentTrial(ATrial: integer);
begin
  OnNotCorrection(Self);
  Inc(FSessionTrials);
  Inc(FBlcTrials);
  FTrialBkGndResponses := 0;
  FTrialStmResponses := 0;

  FCurrentTrial := ATrial;
end;

procedure TCounterManager.BeginBlc(Sender: TObject);
begin

end;

procedure TCounterManager.BeginSess(Sender: TObject);
begin
  FCurrentBlc := 0;
  FCurrentTrial := 0;
end;

procedure TCounterManager.BeginTrial(Sender: TObject);
begin

end;

procedure TCounterManager.BkGndResponse(Sender: TObject);
begin
  Inc(FSessionBackGroundResponses);
  Inc(FTrialBkGndResponses);
end;

procedure TCounterManager.Consequence(Sender: TObject);
begin
  Inc(FSessionConsequences);
  Inc(FBlcConsequences);
end;

procedure TCounterManager.Correction(Sender: TObject);
begin
  Inc(FSessionTrials);
  Inc(FBlcTrials);
  Inc(FSessionCorrections);
  Inc(FSessionCscCorrections);
  Inc(FBlcCorrections);
  Inc(FBlcCscCorrections);
end;

procedure TCounterManager.CsqCriterion(Sender: TObject);
begin
  FBlcCsqHits := 0;
end;

procedure TCounterManager.EndBlc(Sender: TObject);
begin
  FBlcVirtualHitLoop := 0;
  FBlcVirtualTrial := 0;
  FBlcBkGndResponses := 0;
  FBlcStmResponses := 0;
  FBlcConsequences := 0;
  FBlcCorrections := 0;
  FBlcCscCorrections := 0;
  FBlcHighCscCorrections := 0;
  FBlcCsqHits := 0;
  FBlcHits := 0;
  FBlcCscHits := 0;
  FBlcCscHitsType1 := 0;
  FBlcCscHitsType2 := 0;
  FBlcHighCscHits := 0;
  FBlcMisses := 0;
  FBlcCscMisses := 0;
  FBlcHighCscMisses := 0;
  FBlcNones := 0;
  FBlcCscNones := 0;
  FBlcHighCscNones := 0;
  FCurrentTrial := 0;
  FBlcTrials:= 0;
  FBlcRepetitions := 0;
  FBlcPoints:=0;

  Inc(FCurrentBlc);
end;

procedure TCounterManager.RepeatBlc(Sender: TObject);
begin
  FBlcVirtualHitLoop := 0;
  FBlcVirtualTrial := 0;
  FBlcBkGndResponses := 0;
  FBlcStmResponses := 0;
  FBlcConsequences := 0;
  FBlcCorrections := 0;
  FBlcCscCorrections := 0;
  FBlcHighCscCorrections := 0;
  FBlcCsqHits := 0;
  FBlcHits := 0;
  FBlcCscHits := 0;
  FBlcCscHitsType1 := 0;
  FBlcCscHitsType2 := 0;
  FBlcHighCscHits := 0;
  FBlcMisses := 0;
  FBlcCscMisses := 0;
  FBlcHighCscMisses := 0;
  FBlcNones := 0;
  FBlcCscNones := 0;
  FBlcHighCscNones := 0;
  FCurrentTrial := 0;
  FBlcTrials:= 0;
  FBlcPoints := 0;
  Inc(FBlcRepetitions);
end;

procedure TCounterManager.EndSess(Sender: TObject);
begin

end;

procedure TCounterManager.EndTrial(Sender: TObject);
begin
  Inc(FSessionTrials);
  Inc(FBlcTrials);
  Inc(FCurrentTrial);
  FTrialBkGndResponses := 0;
  FTrialStmResponses := 0;
end;

//TBlc.TrialTerminate, TCounterManager.Hit, TCounterManager.NotCorrection,
//permitem definir quando incrementar um acerto tendo como base os valores dos contadores:
//FVirtualTrialValue, FVirtualTrialLoop, FVirtualHitLoop e FVirtualTrial

//FVirtualTrialValue é definido pelo usuário e determina o valor máximo do FVirtualTrialLoop
procedure TCounterManager.Hit(Sender: TObject);
begin
  Inc(FSessionVirtualHits);
  Inc(FBlcVirtualHitLoop);
  FOnHitResult := FBlcVirtualHitLoop;

    if FBlcVirtualTrialLoop = FBlcVirtualTrialValue then
    begin
      ///bloco onhit
      if FBlcVirtualHitLoop = FBlcVirtualTrialValue + 1 then
        begin
          FBlcVirtualHitLoop := 0;
          Inc(FSessionHits);              //Contador de corretas na sessão
          Inc(FSessionCscHits);           //Contador de corretas consecutivas na sessão
          Inc(FBlcHits);           //Contador de corretas no bloco
          Inc(FBlcCscHits);        //Contador de corretas consecutivas do bloco
          Inc(FBlcCscHitsType1);        //Contador de corretas consecutivas t1
          Inc(FBlcCscHitsType2);        //Contador de corretas consecutivas t2
          Inc(FBlcCsqHits);        //Contador de corretas consecutivas para liberação de consequências
        end else FBlcVirtualHitLoop := 0;
       ///
    end;
  //

  if FBlcCscMisses > FBlcHighCscMisses then FBlcHighCscMisses := FBlcCscMisses;
  FBlcCscMisses := 0;
  if FBlcCscHits > FBlcHighCscHits then FBlcHighCscHits := FBlcCscHits;

  if FSessionCscMisses > FSessionHighCscMisses then FSessionHighCscMisses := FSessionCscMisses;
  FSessionCscMisses := 0;
  if FSessionCscHits > FSessionHighCscHits then FSessionHighCscHits := FSessionCscHits;
end;

procedure TCounterManager.Miss(Sender: TObject);
begin
  FBlcCsqHits := 0; //Para a liberação de consequências
  FBlcVirtualHitLoop := 0;
  Inc(FSessionMisses);
  Inc(FSessionCscMisses);
  Inc(FBlcMisses);
  Inc(FBlcCscMisses);

  if FBlcCscHits > FBlcHighCscHits then FBlcHighCscHits := FBlcCscHits;
  FBlcCscHits := 0;
  FBlcCscHitsType1 := 0;
  FBlcCscHitsType2 := 0;
  if FBlcCscMisses > FBlcHighCscMisses then FBlcHighCscMisses := FBlcCscMisses;

  if FSessionCscHits > FSessionHighCscHits then FSessionHighCscHits := FSessionCscHits;
  FSessionCscHits := 0;
  if FSessionCscMisses > FSessionHighCscMisses then FSessionHighCscMisses := FSessionCscMisses;
end;

procedure TCounterManager.NotCorrection(Sender : TObject);
begin
  if FBlcCscCorrections > FBlcHighCscCorrections  then FBlcHighCscCorrections := FBlcCscCorrections;
  FBlcCscCorrections := 0;

  if FSessionCscCorrections > FSessionHighCscCorrections  then FSessionHighCscCorrections := FSessionCscCorrections;
  FSessionCscCorrections := 0;

  if FBlcVirtualTrialLoop = FBlcVirtualTrialValue then
    begin
      FBlcVirtualTrialLoop := 0;
      Inc(FBlcVirtualTrial);

      //Bloco onhit
    end
  else
    begin
      Inc(FBlcVirtualTrialLoop);
    end;
end;

//procedure TCounterManager.SetOnBlcRepetition(AValue: TNotifyEvent);
//begin
//  if FOnBlcRepetition=AValue then Exit;
//  FOnBlcRepetition:=AValue;
//end;

procedure TCounterManager.CustomNxtTrial(Sender: TObject);
begin
  Inc(FSessionTrials);
end;

procedure TCounterManager.SetVirtualTrialValue(Value: integer);
begin
  if Value <= 0 then
    FBlcVirtualTrialValue := 0
  else FBlcVirtualTrialValue := Value - 1;
end;

procedure TCounterManager.StmResponse(Sender: TObject);
begin
  Inc(FSessionStmResponses);
  Inc(FBlcStmResponses);
  Inc(FTrialStmResponses);
end;

//procedure TCounterManager.BlcRepeated(Sender: TObject);
//begin
//  Inc(FBlcRepetitions);
//end;


procedure TCounterManager._VirtualTrialFix;
begin
  if FBlcVirtualTrialValue <= 0 then
  else
    begin
     // ShowMessage('A=' + IntToStr(FBlcVirtualTrialLoop) + #32 + 'B=' + IntToStr(FBlcVirtualTrialValue));
      Inc(FCurrentTrial,FBlcVirtualTrialValue - FBlcVirtualTrialLoop);
      FBlcVirtualTrialLoop := FBlcVirtualTrialValue;
    end;
end;

end.

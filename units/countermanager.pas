{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit countermanager;

{$mode objfpc}{$H+}

interface

uses Classes, Dialogs, SysUtils;

type

  TCounterManager = class (TComponent)
  private
  {Current Session}
    FBkGndResponses : integer;
    FConsequences : integer;
    FCorrections : integer;
    FCscCorrections : integer;
    FCscHits : integer;
    FCscMisses : integer;
    FCscNones : integer;
    FCurrentBlc : integer;
    FHighCscCorrections : integer;
    FHighCscHits : integer;
    FHighCscMisses : integer;
    FHighCscNones : integer;
    FHits : integer;
    FMisses : integer;
    FNones : integer;
    FStmResponses : integer;
    FTrials : integer;
    FVirtualHits : integer;
  {Current Blc}
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
    FCurrentTrial : integer;
    FVirtualHitLoop : integer;
    FVirtualTrial : integer;
    FVirtualTrialLoop : integer;
    FVirtualTrialValue : Integer;
  {Current Trial}
    FTrialBkGndResponses : integer;
    FTrialStmResponses : integer;
  {Current Stm}
    FStmCounter : integer;
  {Events}
    FOnBeginBlc: TNotifyEvent;
    FOnBeginSess: TNotifyEvent;
    FOnBeginTrial: TNotifyEvent;
    FOnBkGndResponse: TNotifyEvent;
    FOnConsequence: TNotifyEvent;
    FOnCorrection: TNotifyEvent;
    FOnCsqCriterion: TNotifyEvent;
    FOnEndBlc: TNotifyEvent;
    FOnEndSess: TNotifyEvent;
    FOnEndTrial: TNotifyEvent;
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
    procedure EndSess(Sender : TObject);
    procedure EndTrial(Sender : TObject);
    procedure Hit(Sender : TObject);
    procedure Miss(Sender : TObject);
    procedure NotCorrection(Sender : TObject);
    procedure StmResponse(Sender : TObject);
  public
    constructor Create (AOwner : TComponent); override;
    procedure _VirtualTrialFix;
    procedure SetVirtualTrialValue(Value : integer);
  {Current Trial}
    property TrialStmResponses  : integer read FTrialStmResponses;
    property TrialBkGndResponses : integer read FTrialBkGndResponses;
  {Current Session}
    property BkGndResponses : integer read FBkGndResponses;
    property Consequences : integer read FConsequences;
    property Corrections : integer read FCorrections;
    property CscCorrections : integer read FCscCorrections;
    property CscHits : integer read FCscHits;
    property CscMisses : integer read FCscMisses;
    property CscNones : integer read FCscNones;
    property CurrentBlc : integer read FCurrentBlc write FCurrentBlc;
    property HighCscCorrections : integer read FHighCscCorrections;
    property HighCscHits : integer read FHighCscHits;
    property HighCscMisses : integer read FHighCscMisses;
    property HighCscNones : integer read FHighCscNones;
    property Hits : integer read FHits;
    property Misses : integer read FMisses;
    property Nones : integer read FNones;
    property StmResponses : integer read FStmResponses;
    property Trials : integer read FTrials;
    property VirtualHits : integer read FVirtualHits;
  {Current Blc}
    property VirtualTrialValue : Integer read FVirtualTrialValue;
    property VirtualHitLoop : integer read FVirtualHitLoop write FVirtualHitLoop;
    property VirtualTrialLoop : integer read FVirtualTrialLoop write FVirtualTrialLoop;
    property VirtualTrial : integer read FVirtualTrial;
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
    property CurrentTrial : integer read FCurrentTrial write FCurrentTrial;
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
    property OnEndSess: TNotifyEvent read FOnEndSess write FOnEndSess;
    property OnEndTrial : TNotifyEvent read FOnEndTrial write FOnEndTrial;
    property OnNxtTrial : TNotifyEvent read FOnNxtTrial write FOnNxtTrial;
    property OnHit: TNotifyEvent read FOnHit write FOnHit;
    property OnHitResult : integer read FOnHitResult;
    property OnMiss: TNotifyEvent read FOnMiss write FOnMiss;
    property OnStmResponse : TNotifyEvent read FOnStmResponse write FOnStmResponse;
    property OnCsqCriterion : TNotifyEvent read FOnCsqCriterion write FOnCsqCriterion;
  end;
implementation

{ TCounterManager }

constructor TCounterManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { Session - 17 }
  FBkGndResponses := 0;
  FStmResponses := 0;
  FConsequences := 0;
  FCorrections := 0;
  FCscCorrections := 0;
  FHighCscCorrections := 0;
  FHits := 0;
  FCscHits := 0;
  FHighCscHits := 0;
  FMisses := 0;
  FCscMisses := 0;
  FHighCscMisses := 0;
  FNones := 0;
  FCscNones := 0;
  FHighCscNones := 0;
  FTrials := 0;
  FCurrentBlc := 0;
  FVirtualHits := 0;
  { Blocs - 20 }
  FVirtualHitLoop := 0;
  FVirtualTrial := 0;
  FVirtualTrialLoop := 0;
  FBlcBkGndResponses := 0;
  FBlcStmResponses := 0;
  FBlcConsequences := 0;
  FBlcCorrections := 0;
  FBlcCscCorrections := 0;
  FBlcHighCscCorrections := 0;
  FBlcCsqHits := 0;
  FBlcHits := 0;
  FBlcCscHits := 0;
  FBlcHighCscHits := 0;
  FBlcMisses := 0;
  FBlcCscMisses := 0;
  FBlcHighCscMisses := 0;
  FBlcNones := 0;
  FBlcCscNones := 0;
  FBlcHighCscNones := 0;
  FCurrentTrial := 0;
{ Trials }
  FTrialStmResponses := 0;

  { events }

  OnBeginBlc := @BeginBlc;
  OnBeginSess := @BeginSess;
  OnBeginTrial := @BeginTrial;
  OnBkGndResponse := @BkGndResponse;
  OnConsequence := @Consequence;
  OnCorrection := @Correction;
  OnEndBlc := @EndBlc;
  OnEndSess := @EndSess;
  OnEndTrial := @EndTrial;
  OnNxtTrial := @CustomNxtTrial;
  OnHit := @Hit;
  OnMiss := @Miss;
  OnStmResponse := @StmResponse;
  OnCsqCriterion := @CsqCriterion;
  OnNotCorrection := @NotCorrection;
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
  Inc(FBkGndResponses);
  Inc(FTrialBkGndResponses);
end;

procedure TCounterManager.Consequence(Sender: TObject);
begin
  Inc(FConsequences);
  Inc(FBlcConsequences);
end;

procedure TCounterManager.Correction(Sender: TObject);
begin
  Inc(FTrials);
  Inc(FCorrections);
  Inc(FCscCorrections);
  Inc(FBlcCorrections);
  Inc(FBlcCscCorrections);
end;

procedure TCounterManager.CsqCriterion(Sender: TObject);
begin
  FBlcCsqHits := 0;
end;

procedure TCounterManager.EndBlc(Sender: TObject);
begin
  FVirtualHitLoop := 0;
  FVirtualTrial := 0;
  FBlcBkGndResponses := 0;
  FBlcStmResponses := 0;
  FBlcConsequences := 0;
  FBlcCorrections := 0;
  FBlcCscCorrections := 0;
  FBlcHighCscCorrections := 0;
  FBlcCsqHits := 0;
  FBlcHits := 0;
  FBlcCscHits := 0;
  FBlcHighCscHits := 0;
  FBlcMisses := 0;
  FBlcCscMisses := 0;
  FBlcHighCscMisses := 0;
  FBlcNones := 0;
  FBlcCscNones := 0;
  FBlcHighCscNones := 0;
  FCurrentTrial := 0;

  Inc(FCurrentBlc);
end;

procedure TCounterManager.EndSess(Sender: TObject);
begin

end;

procedure TCounterManager.EndTrial(Sender: TObject);
begin
  Inc(FTrials);
  Inc(FCurrentTrial);
  FTrialBkGndResponses := 0;
  FTrialStmResponses := 0;
end;

//cada Trial.Consequence pode produzir um 'acerto' ou um 'erro' (uTrialMTS e uTrialSimpl);
//TBlc.TrialTerminate, TCounterManager.Hit, TCounterManager.NotCorrection,
//permitem definir quando incrementar um acerto tendo como base os valores dos contadores:
//FVirtualTrialValue, FVirtualTrialLoop, FVirtualHitLoop e FVirtualTrial

//FVirtualTrialValue é definido pelo usuário e determina o valor máximo do FVirtualTrialLoop
procedure TCounterManager.Hit(Sender: TObject);
begin
  Inc(FVirtualHits);
  Inc(FVirtualHitLoop);
  FOnHitResult := FVirtualHitLoop;

    if FVirtualTrialLoop = FVirtualTrialValue then
    begin
      ///bloco onhit
      if FVirtualHitLoop = FVirtualTrialValue + 1 then
        begin
          FVirtualHitLoop := 0;
          Inc(FHits);              //Contador de corretas na sessão
          Inc(FCscHits);           //Contador de corretas consecutivas na sessão
          Inc(FBlcHits);           //Contador de corretas no bloco
          Inc(FBlcCscHits);        //Contador de corretas consecutivas do bloco
          Inc(FBlcCsqHits);        //Contador de corretas consecutivas para liberação de consequências
        end else FVirtualHitLoop := 0;
       ///
    end;
  //

  if FBlcCscMisses > FBlcHighCscMisses then FBlcHighCscMisses := FBlcCscMisses;
  FBlcCscMisses := 0;
  if FBlcCscHits > FBlcHighCscHits then FBlcHighCscHits := FBlcCscHits;

  if FCscMisses > FHighCscMisses then FHighCscMisses := FCscMisses;
  FCscMisses := 0;
  if FCscHits > FHighCscHits then FHighCscHits := FCscHits;
end;

procedure TCounterManager.Miss(Sender: TObject);
begin
  FBlcCsqHits := 0; //Para a liberação de consequências
  FVirtualHitLoop := 0;
  Inc(FMisses);
  Inc(FCscMisses);
  Inc(FBlcMisses);
  Inc(FBlcCscMisses);

  if FBlcCscHits > FBlcHighCscHits then FBlcHighCscHits := FBlcCscHits;
  FBlcCscHits := 0;
  if FBlcCscMisses > FBlcHighCscMisses then FBlcHighCscMisses := FBlcCscMisses;

  if FCscHits > FHighCscHits then FHighCscHits := FCscHits;
  FCscHits := 0;
  if FCscmisses > FHighCscMisses then FHighCscMisses := FCscmisses;
end;

procedure TCounterManager.NotCorrection(Sender : TObject);
begin
  if FBlcCscCorrections > FBlcHighCscCorrections  then FBlcHighCscCorrections := FBlcCscCorrections;
  FBlcCscCorrections := 0;

  if FCscCorrections > FHighCscCorrections  then FHighCscCorrections := FCscCorrections;
  FCscCorrections := 0;

  if FVirtualTrialLoop = FVirtualTrialValue then
    begin
      FVirtualTrialLoop := 0;
      Inc(FVirtualTrial);

      //Bloco onhit
    end
  else
    begin
      Inc(FVirtualTrialLoop);
    end;
end;

procedure TCounterManager.CustomNxtTrial(Sender: TObject);
begin
  Inc(FTrials);
end;

procedure TCounterManager.SetVirtualTrialValue(Value: integer);
begin
  if Value <= 0 then
    FVirtualTrialValue := 0
  else FVirtualTrialValue := Value - 1;
end;

procedure TCounterManager.StmResponse(Sender: TObject);
begin
  Inc(FStmResponses);
  Inc(FBlcStmResponses);
  Inc(FTrialStmResponses);
end;


procedure TCounterManager._VirtualTrialFix;
begin
  if FVirtualTrialValue <= 0 then
  else
    begin
     // ShowMessage('A=' + IntToStr(FVirtualTrialLoop) + #32 + 'B=' + IntToStr(FVirtualTrialValue));
      Inc(FCurrentTrial,FVirtualTrialValue - FVirtualTrialLoop);
      FVirtualTrialLoop := FVirtualTrialValue;
    end;
end;

end.

//
// Validation Project (PCRF) - Stimulus Control App
// Copyright (C) 2014-2015,  Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.
//
// cpicanco@ufpa.br
//
// This file is part of Validation Project (PCRF).
//
// Validation Project (PCRF) is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Validation Project (PCRF) is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Validation Project (PCRF).  If not, see <http://www.gnu.org/licenses/>.
//
unit countermanager;

{$mode objfpc}{$H+}

interface

uses Classes, Dialogs, SysUtils, Counter;

type

  TCounterManager = class (TComponent)
  private
  {Current Session}
    FBkGndResponses : TCounter;
    FStmResponses : TCounter;
    FConsequences : TCounter;
    FCorrections : TCounter;
    FCscCorrections : TCounter;
    FHighCscCorrections : TCounter;
    FHits : TCounter;
    FVirtualHits : TCounter;
    FCscHits : TCounter;
    FHighCscHits : TCounter;
    FMisses : TCounter;
    FCscMisses : TCounter;
    FHighCscMisses : TCounter;
    FNones : TCounter;
    FCscNones : TCounter;
    FHighCscNones : TCounter;
    FTrials : TCounter;
    FCurrentBlc : TCounter;
  {Current Blc}
    FVirtualHitLoop : TCounter;
    FVirtualTrialValue : Integer;
    FVirtualTrial : TCounter;
    FVirtualTrialLoop : TCounter;
    FBlcBkGndResponses : TCounter;
    FBlcStmResponses : TCounter;
    FBlcConsequences : TCounter;
    FBlcCorrections : TCounter;
    FBlcCscCorrections : TCounter;
    FBlcHighCscCorrections : TCounter;
    FBlcCsqHits : TCounter;
    FBlcHits : TCounter;
    FBlcCscHits : TCounter;
    FBlcHighCscHits : TCounter;
    FBlcMisses : TCounter;
    FBlcCscMisses : TCounter;
    FBlcHighCscMisses : TCounter;
    FBlcNones : TCounter;
    FBlcCscNones : TCounter;
    FBlcHighCscNones : TCounter;
    FCurrentTrial : TCounter;
  {Current Trial}
    FTrialBkGndResponses : TCounter;
    FTrialStmResponses : TCounter;
  {Current Stm}
    FStmCounter : TCounter;
  {Events}
    FOnBeginSess: TNotifyEvent;
    FOnStmResponse: TNotifyEvent;
    FOnHit: TNotifyEvent;
    FOnEndTrial: TNotifyEvent;
    FOnBkGndResponse: TNotifyEvent;
    FOnBeginTrial: TNotifyEvent;
    FOnEndBlc: TNotifyEvent;
    FOnEndSess: TNotifyEvent;
    FOnConsequence: TNotifyEvent;
    FOnMiss: TNotifyEvent;
    FOnBeginBlc: TNotifyEvent;
    FOnCorrection: TNotifyEvent;
    FOnCsqCriterion: TNotifyEvent;
    FOnNotCorrection: TNotifyEvent;
    FOnNxtTrial: TNotifyEvent;
    FOnHitResult: integer;
    procedure Hit(Sender : TObject);
    procedure CustomNxtTrial(Sender : TObject);
    procedure NotCorrection(Sender : TObject);
    procedure CsqCriterion(Sender : TObject);
    procedure Correction(Sender : TObject);
    procedure BeginBlc(Sender : TObject);
    procedure BeginSess(Sender : TObject);
    procedure StmResponse(Sender : TObject);
    procedure EndTrial(Sender : TObject);
    procedure BkGndResponse(Sender : TObject);
    procedure BeginTrial(Sender : TObject);
    procedure EndBlc(Sender : TObject);
    procedure EndSess(Sender : TObject);
    procedure Consequence(Sender : TObject);
    procedure Miss(Sender : TObject);
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    procedure _VirtualTrialFix;
    procedure SetVirtualTrialValue(Value : integer);
  {Current Trial}
    property TrialStmResponses  : TCounter read FTrialStmResponses;
    property TrialBkGndResponses : TCounter read FTrialBkGndResponses;
  {Current Session}
    property BkGndResponses : TCounter read FBkGndResponses;
    property StmResponses : TCounter read FStmResponses;
    property Consequences : TCounter read FConsequences;
    property Corrections : TCounter read FCorrections;
    property CscCorrections : TCounter read FCscCorrections;
    property HighCscCorrections : TCounter read FHighCscCorrections;
    property Hits : TCounter read FHits;
    property CscHits : TCounter read FCscHits;
    property HighCscHits : TCounter read FHighCscHits;
    property Misses : TCounter read FMisses;
    property CscMisses : TCounter read FCscMisses;
    property HighCscMisses : TCounter read FHighCscMisses;
    property Nones : TCounter read FNones;
    property CscNones : TCounter read FCscNones;
    property HighCscNones : TCounter read FHighCscNones;
    property Trials : TCounter read FTrials;
    property CurrentBlc : TCounter read FCurrentBlc write FCurrentBlc;
    property VirtualHits : TCounter read FVirtualHits;
  {Current Blc}

    property VirtualTrialValue : Integer read FVirtualTrialValue;
    property VirtualHitLoop : TCounter read FVirtualHitLoop;
    property VirtualTrialLoop : TCounter read FVirtualTrialLoop;
    property VirtualTrial : TCounter read FVirtualTrial;
    property BlcBkGndResponses : TCounter read FBlcBkGndResponses;
    property BlcStmResponses : TCounter read FBlcStmResponses;
    property BlcConsequences : TCounter read FBlcConsequences;
    property BlcCorrections : TCounter read FBlcCorrections;
    property BlcCscCorrections : TCounter read FBlcCscCorrections;
    property BlcHighCscCorrections : TCounter read FBlcHighCscCorrections;
    property BlcCsqHits : TCounter read FBlcCsqHits write FBlcCsqHits;
    property BlcHits : TCounter read FBlcHits;
    property BlcCscHits : TCounter read FBlcCscHits;
    property BlcHighCscHits : TCounter read FBlcHighCscHits;
    property BlcMisses : TCounter read FBlcMisses;
    property BlcCscMisses : TCounter read FBlcCscMisses;
    property BlcHighCscMisses : TCounter read FBlcHighCscMisses;
    property BlcNones : TCounter read FBlcNones;
    property BlcCscNones : TCounter read FBlcCscNones;
    property BlcHighCscNones : TCounter read FBlcHighCscNones;
    property CurrentTrial : TCounter read FCurrentTrial write FCurrentTrial;
  {Each Stm}
    property StmCounter : TCounter read FStmCounter;
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
  {Current Session - 17}
    FBkGndResponses := TCounter.Create;
    FStmResponses := TCounter.Create;
    FConsequences := TCounter.Create;
    FCorrections := TCounter.Create;
    FCscCorrections := TCounter.Create;
    FHighCscCorrections := TCounter.Create;
    FHits := TCounter.Create;
    FCscHits := TCounter.Create;
    FHighCscHits := TCounter.Create;
    FMisses := TCounter.Create;
    FCscMisses := TCounter.Create;
    FHighCscMisses := TCounter.Create;
    FNones := TCounter.Create;
    FCscNones := TCounter.Create;
    FHighCscNones := TCounter.Create;
    FTrials := TCounter.Create;
    FCurrentBlc := TCounter.Create;
    FVirtualHits := TCounter.Create;
  {Current Blc - 20}
    FVirtualHitLoop := TCounter.Create;
    FVirtualTrial := TCounter.Create;
    FVirtualTrialLoop := TCounter.Create;
    FBlcBkGndResponses := TCounter.Create;
    FBlcStmResponses := TCounter.Create;
    FBlcConsequences := TCounter.Create;
    FBlcCorrections := TCounter.Create;
    FBlcCscCorrections := TCounter.Create;
    FBlcHighCscCorrections := TCounter.Create;
    FBlcCsqHits := TCounter.Create;
    FBlcHits := TCounter.Create;
    FBlcCscHits := TCounter.Create;
    FBlcHighCscHits := TCounter.Create;
    FBlcMisses := TCounter.Create;
    FBlcCscMisses := TCounter.Create;
    FBlcHighCscMisses := TCounter.Create;
    FBlcNones := TCounter.Create;
    FBlcCscNones := TCounter.Create;
    FBlcHighCscNones := TCounter.Create;
    FCurrentTrial := TCounter.Create;
  {Current Trial}
    FTrialStmResponses := TCounter.Create;

 ///////////////
  {Current Session - 17}
    FBkGndResponses.Clear;
    FStmResponses.Clear;
    FConsequences.Clear;
    FCorrections.Clear;
    FCscCorrections.Clear;
    FHighCscCorrections.Clear;
    FHits.Clear;
    FCscHits.Clear;
    FHighCscHits.Clear;
    FMisses.Clear;
    FCscMisses.Clear;
    FHighCscMisses.Clear;
    FNones.Clear;
    FCscNones.Clear;
    FHighCscNones.Clear;
    FTrials.Clear;
    FCurrentBlc.Clear;
    FVirtualHits.Clear;
  {Current Blc - 18}
    FVirtualHitLoop.Clear;
    FVirtualTrial.Clear;
    FVirtualTrialLoop.Clear;
    FBlcBkGndResponses.Clear;
    FBlcStmResponses.Clear;
    FBlcConsequences.Clear;
    FBlcCorrections.Clear;
    FBlcCscCorrections.Clear;
    FBlcHighCscCorrections.Clear;
    FBlcCsqHits.Clear;
    FBlcHits.Clear;
    FBlcCscHits.Clear;
    FBlcHighCscHits.Clear;
    FBlcMisses.Clear;
    FBlcCscMisses.Clear;
    FBlcHighCscMisses.Clear;
    FBlcNones.Clear;
    FBlcCscNones.Clear;
    FBlcHighCscNones.Clear;
    FCurrentTrial.Clear;
  {Current Trial}
    FTrialStmResponses.Clear;
  //////////////////

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

destructor TCounterManager.Destroy;
begin
  {Current Session - 17}
    FBkGndResponses.Free;
    FStmResponses.Free;
    FConsequences.Free;
    FCorrections.Free;
    FCscCorrections.Free;
    FHighCscCorrections.Free;
    FHits.Free;
    FCscHits.Free;
    FHighCscHits.Free;
    FMisses.Free;
    FCscMisses.Free;
    FHighCscMisses.Free;
    FNones.Free;
    FCscNones.Free;
    FHighCscNones.Free;
    FTrials.Free;
    FCurrentBlc.Free;
    FVirtualHits.Free;
  {Current Blc - 18}
    FVirtualTrial.Free;
    FVirtualTrialLoop.Free;
    FBlcBkGndResponses.Free;
    FBlcStmResponses.Free;
    FBlcConsequences.Free;
    FBlcCorrections.Free;
    FBlcCscCorrections.Free;
    FBlcHighCscCorrections.Free;
    FBlcCsqHits.Free;
    FBlcHits.Free;
    FBlcCscHits.Free;
    FBlcHighCscHits.Free;
    FBlcMisses.Free;
    FBlcCscMisses.Free;
    FBlcHighCscMisses.Free;
    FBlcNones.Free;
    FBlcCscNones.Free;
    FBlcHighCscNones.Free;
    FCurrentTrial.Free;
  {Current Trial}
    FTrialStmResponses.Free;
  {Current Stm}

  inherited Destroy;
end;

procedure TCounterManager.BeginBlc(Sender: TObject);
begin

end;

procedure TCounterManager.BeginSess(Sender: TObject);
begin
  FCurrentBlc.Clear;
  FCurrentTrial.Clear;
end;

procedure TCounterManager.BeginTrial(Sender: TObject);
begin

end;

procedure TCounterManager.BkGndResponse(Sender: TObject);
begin
  FTrialBkGndResponses := TCounter(Sender);
end;

procedure TCounterManager.Consequence(Sender: TObject);
begin
  FConsequences.Plus(1);
  FBlcConsequences.Plus(1);
end;

procedure TCounterManager.Correction(Sender: TObject);
begin
  FTrials.Plus(1);
  FCorrections.Plus(1);
  FCscCorrections.Plus(1);
  FBlcCorrections.Plus(1);
  FBlcCscCorrections.Plus(1);
end;

procedure TCounterManager.CsqCriterion(Sender: TObject);
begin
  FBlcCsqHits.Clear;
end;

procedure TCounterManager.EndBlc(Sender: TObject);
begin
  FVirtualHitLoop.Clear;
  FVirtualTrial.Clear;
  FBlcBkGndResponses.Clear;
  FBlcStmResponses.Clear;
  FBlcConsequences.Clear;
  FBlcCorrections.Clear;
  FBlcCscCorrections.Clear;
  FBlcHighCscCorrections.Clear;
  FBlcCsqHits.Clear;
  FBlcHits.Clear;
  FBlcCscHits.Clear;
  FBlcHighCscHits.Clear;
  FBlcMisses.Clear;
  FBlcCscMisses.Clear;
  FBlcHighCscMisses.Clear;
  FBlcNones.Clear;
  FBlcCscNones.Clear;
  FBlcHighCscNones.Clear;
  FCurrentTrial.Clear;

  FCurrentBlc.Plus(1);
end;

procedure TCounterManager.EndSess(Sender: TObject);
begin

end;

procedure TCounterManager.EndTrial(Sender: TObject);
begin
  FTrials.Plus(1);
  FCurrentTrial.Plus(1);
end;

//cada Trial.Consequence pode produzir um 'acerto' ou um 'erro' (uTrialMTS e uTrialSimpl);
//TBlc.TrialTerminate, TCounterManager.Hit, TCounterManager.NotCorrection,
//permitem definir quando incrementar um acerto tendo como base os valores dos contadores:
//FVirtualTrialValue, FVirtualTrialLoop, FVirtualHitLoop e FVirtualTrial

//FVirtualTrialValue é definido pelo usuário e determina o valor máximo do FVirtualTrialLoop
procedure TCounterManager.Hit(Sender: TObject);
begin
  FVirtualHits.Plus(1);    //Para o prodimento do Ryan, gerando contagem diferente do FVirtualHitLoop, não sei porque ainda
  FVirtualHitLoop.Plus(1);
  FOnHitResult := FVirtualHitLoop.Counter;

    if FVirtualTrialLoop.Counter = FVirtualTrialValue then
    begin
      ///bloco onhit
      if FVirtualHitLoop.Counter = FVirtualTrialValue + 1 then   //+1   para 0
        begin
          FVirtualHitLoop.Clear;
          FHits.Plus(1);              //Contador de corretas na sessão
          FCscHits.Plus(1);           //Contador de corretas consecutivas na sessão
          FBlcHits.Plus(1);           //Contador de corretas no bloco
          FBlcCscHits.Plus(1);        //Contador de corretas consecutivas do bloco
          FBlcCsqHits.Plus(1);        //Contador de corretas consecutivas para liberação de consequências
        end else FVirtualHitLoop.Clear;
       ///
    end;
  //

  if FBlcCscMisses.Counter > FBlcHighCscMisses.Counter then FBlcHighCscMisses.Counter := FBlcCscMisses.Counter;
  FBlcCscMisses.Clear;
  if FBlcCscHits.Counter > FBlcHighCscHits.Counter then FBlcHighCscHits.Counter := FBlcCscHits.Counter;

  if FCscMisses.Counter > FHighCscMisses.Counter then FHighCscMisses.Counter := FCscMisses.Counter;
  FCscMisses.Clear;
  if FCscHits.Counter > FHighCscHits.Counter then FHighCscHits.Counter := FCscHits.Counter;
end;

procedure TCounterManager.Miss(Sender: TObject);
begin
  FBlcCsqHits.Clear; //Para a liberação de consequências
  FVirtualHitLoop.Clear;
  FMisses.Plus(1);
  FCscMisses.Plus(1);
  FBlcMisses.Plus(1);
  FBlcCscMisses.Plus(1);

  if FBlcCscHits.Counter > FBlcHighCscHits.Counter then FBlcHighCscHits.Counter := FBlcCscHits.Counter;
  FBlcCscHits.Clear;
  if FBlcCscMisses.Counter > FBlcHighCscMisses.Counter then FBlcHighCscMisses.Counter := FBlcCscMisses.Counter;

  if FCscHits.Counter > FHighCscHits.Counter then FHighCscHits.Counter := FCscHits.Counter;
  FCscHits.Clear;
  if FCscmisses.Counter > FHighCscMisses.Counter then FHighCscMisses.Counter := FCscmisses.Counter;
end;

procedure TCounterManager.NotCorrection(Sender : TObject);
begin
  if FBlcCscCorrections.Counter > FBlcHighCscCorrections.Counter  then FBlcHighCscCorrections.Counter := FBlcCscCorrections.Counter;
  FBlcCscCorrections.Clear;

  if FCscCorrections.Counter > FHighCscCorrections.Counter  then FHighCscCorrections.Counter := FCscCorrections.Counter;
  FCscCorrections.Clear;

  if FVirtualTrialLoop.Counter = FVirtualTrialValue then
    begin
      FVirtualTrialLoop.Clear;
      FVirtualTrial.Plus(1);

      //Bloco onhit
    end
  else
    begin
      FVirtualTrialLoop.Plus(1);
    end;
end;

procedure TCounterManager.CustomNxtTrial(Sender: TObject);
begin
  FTrials.Plus(1);
end;

procedure TCounterManager.SetVirtualTrialValue(Value: integer);
begin
  if Value <= 0 then
    FVirtualTrialValue := 0
  else FVirtualTrialValue := Value - 1;
end;

procedure TCounterManager.StmResponse(Sender: TObject);
begin
  FStmResponses.Plus(1);
  FBlcStmResponses.Plus(1);
  FTrialStmResponses.Plus(1);
  //FStmCounter := TCounter(Sender);
end;


procedure TCounterManager._VirtualTrialFix;
begin
  if FVirtualTrialValue <= 0 then //= para <=
  else
    begin
     // SHOWMESSAGE ('A=' + INTTOSTR(FVirtualTrialLoop.Counter) + #32 + 'B=' + INTTOSTR(FVirtualTrialValue));
      FCurrentTrial.Plus (FVirtualTrialValue - FVirtualTrialLoop.Counter);
      FVirtualTrialLoop.Counter := FVirtualTrialValue;
    end;
end;

end.

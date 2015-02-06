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
unit schedules;

{$MODE Delphi}

interface

uses Classes, SysUtils;

type
  TAbsSch = class(TComponent)
  private
    FOnConsequence  : TNotifyEvent;
    FOnConsequence2  : TNotifyEvent;
    FOnResponse  : TNotifyEvent;
    FValue  : Integer;
    FValue2 : Integer;
    FVariation  : Integer;
    FVariation2 : Integer;
    FTimeCount: integer;
    procedure Consequence;
    procedure Consequence2;
    procedure Response;
  public
    procedure Clock; virtual; abstract;
    procedure DoResponse; virtual; abstract;
    procedure Reset; virtual; abstract;
    procedure Reset2; virtual; abstract;
    property OnConsequence: TNotifyEvent read FOnConsequence write FOnConsequence;
    property OnConsequence2: TNotifyEvent read FOnConsequence2 write FOnConsequence2;
    property OnResponse: TNotifyEvent read FOnResponse write FOnResponse;
    property Value: Integer read FValue write FValue;
    property Value2: Integer read FValue2 write FValue2;
    property Variation: Integer read FVariation write FVariation;
    property Variation2: Integer read FVariation2 write FVariation2;
    property Time : integer read FTimeCount default 0;
  end;

  TSchRR = class(TAbsSch)
  private
    FCountResp: Integer;
    FNumResp: Integer;
  public
    procedure Clock; override;
    procedure DoResponse; override;
    procedure Reset; override;
    procedure Reset2; override;
  end;

  TSchRI = class(TAbsSch)
  private
    FFlagReinf: Boolean;
    FInterval: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clock; override;
    procedure DoResponse; override;
    procedure Reset; override;
    procedure Reset2; override;
  end;

  TSchRT = class(TAbsSch)
  private
    FInterval: Integer;
  public
    procedure Clock; override;
    procedure DoResponse; override;
    procedure Reset; override;
    procedure Reset2; override;
  end;

  TSchDRL = class(TAbsSch)
  private
    FFlagTime: Boolean;
    FInterval: Integer;
  public
    constructor Create (AOwner : TComponent); override;
    procedure Clock; override;
    procedure DoResponse; override;
    procedure Reset; override;
    procedure Reset2; override;
  end;

  TSchDRH = class(TAbsSch)
  private
    FFlagFirst : Boolean;
    FTotalResp : Integer;
    FCountResp : Integer;
    FNumResp: Integer;
    FFlagTime: Boolean;
    FInterval: Integer;
  public
    constructor Create (AOwner : TComponent); override;
    procedure Clock; override;
    procedure DoResponse; override;
    procedure Reset; override;
    procedure Reset2; override;
  end;

  TSchRRRT = class(TAbsSch)    //Tempo controla a passagem da tentativa, Razão controla ativação da interface, A tentativa passa mais rápido se a razão não for atingida e dura mais se for;
  private
    FFlagFirst : Boolean;      //Somente a primeira ativação de interface incrementa o tempo (FInterval)
    FCountResp: Integer;       //Contador da razão (respostas)
    FNumResp: Integer;         //Razão (variável ou fixa) produz ativação de interface (USB ou PPP)
    // FInterval = Contador de tempo
    FInterval: Integer;        //A variação 2 é o incremento possível do tempo;
  public
    constructor Create (AOwner : TComponent); override;
    procedure Clock; override;
    procedure DoResponse; override;
    procedure Reset; override;
    procedure Reset2; override;
  end;

implementation

{ TSchRR }

procedure TSchRR.Clock;
begin
  //do nothing
end;

procedure TSchRR.DoResponse;
begin
  Response;
  Inc(FCountResp);
  if FCountResp = FNumResp then Consequence;
end;

procedure TSchRR.Reset;
begin
  FCountResp:= 0;
  FNumResp:= FValue - FVariation + Random((2 * Variation) + 1);
  if FNumResp < 1 then FNumResp := 1;
end;

procedure TSchRR.Reset2;
begin

end;

{ TSchRI }

constructor TSchRI.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FFlagReinf:= False;
end;

procedure TSchRI.DoResponse;
begin
  Response;
  if FFlagReinf then Consequence;
end;

procedure TSchRI.Reset;
begin
  FTimeCount:= 0;
  FFlagReinf:= False;
  FInterval:= FValue-FVariation+(Random((2*FVariation)+1));
  if FInterval < 1 then FInterval := 1;
end;

procedure TSchRI.Reset2;
begin

end;

procedure TSchRI.Clock;
begin
  Inc(FTimeCount);
  FFlagReinf:= FTimeCount >= FInterval;
end;

{ TSchRT }

procedure TSchRT.Clock;
begin
  Inc(FTimeCount);
  if FTimeCount >= FInterval then Consequence;
end;

procedure TSchRT.DoResponse;
begin
  Response;
end;

procedure TSchRT.Reset;
begin
  FTimeCount:= 0;
  FInterval:= FValue-FVariation+Random((2*Variation)+1);
  if FInterval < 1 then FInterval := 1;
end;

procedure TSchRT.Reset2;
begin

end;

{ TSchDRH }

procedure TSchDRH.Clock;
begin
  if FFlagTime then
    begin
      Inc(FTimeCount);
      If FTimeCount >= FInterval then Reset;
    end;
end;

constructor TSchDRH.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  FFlagFirst := False;
end;

procedure TSchDRH.DoResponse;
begin
  Response;
  if not FFlagTime then FFlagTime:= True;
  Inc(FTotalResp);
  Inc(FCountResp);
  if FCountResp = FNumResp then Consequence;
end;

procedure TSchDRH.Reset;
begin
  FFlagTime := False;
  FCountResp:= 0;
  FTimeCount := 0;
  FNumResp  := FValue - FVariation + Random((2 * Variation) + 1);
  FInterval := FValue2 - FVariation2 + Random((2 * Variation2) + 1);
  if FNumResp < 1 then FNumResp := 1;
  if FInterval < 1 then FInterval := 1;
end;

procedure TSchDRH.Reset2;
begin

end;

{ TSchDRL }

procedure TSchDRL.Clock;
begin
  If FFlagTime then Inc(FTimeCount);
end;

constructor TSchDRL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFlagTime := False;
end;

procedure TSchDRL.DoResponse;
begin
  Response;
  if FFlagTime = False then FFlagTime:= True;
  if FInterval <= FTimeCount  then Consequence else FTimeCount:= 0;
end;

procedure TSchDRL.Reset;
begin
  FTimeCount:= 0;
  FInterval:= FValue-FVariation+Random((2*Variation)+1);
  if FInterval < 1 then FInterval := 1;
end;

procedure TSchDRL.Reset2;
begin

end;

{ TSchRRRT }

procedure TSchRRRT.Clock;
begin
  Inc(FTimeCount);
  if FTimeCount >= FInterval then Consequence;
end;

constructor TSchRRRT.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFlagFirst := True;
end;

procedure TSchRRRT.DoResponse;
begin
  Response;
  Inc(FCountResp);
  if FCountResp = FNumResp then
    begin
      if FFlagFirst then
        begin
          FFlagFirst := False;
          if FVariation2 > 0 then Inc(Finterval,FVariation2);
        end;
      Consequence2;
    end;
end;

procedure TSchRRRT.Reset;
begin
  FFlagFirst:= True;
  FTimeCount:= 0;
  FCountResp:= 0;
  FInterval := FValue2;
  if FInterval < 1 then FInterval := 1;
  //FInterval := FValue2 - FVariation2 + Random((2 * Variation2) + 1); //Variável e Fixo
end;

procedure TSchRRRT.Reset2;
begin
  FCountResp:= 0;
  FNumResp  := FValue - FVariation + Random((2 * Variation) + 1);
  if FNumResp = 0 then FNumResp := 1;
end;

{ TAbsSch }

procedure TAbsSch.Consequence;      //Ativação das interfaces e/ou Passagem de tentativa
begin
  Reset;
  If Assigned(OnConsequence) then
    begin
      FOnConsequence(Self);
    end;
end;

//Consequence2 serve Apenas para a ativação das interfaces
procedure TAbsSch.Consequence2;
begin
  Reset2;
  If Assigned(OnConsequence2) then
    begin
      FOnConsequence2(Self);
    end;
end;

procedure TAbsSch.Response;
begin
  if Assigned (OnResponse) then FOnResponse(Self);
end;

end.



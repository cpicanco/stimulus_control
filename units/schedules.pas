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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils

, custom_timer
;

type

  { TAbsSch }

  TAbsSch = class(TComponent)
  private
    FClockThread : TClockThread;
    FOnConsequence  : TNotifyEvent;
    FOnResponse  : TNotifyEvent;
    FParameter1  : integer;
    FParameter2 : integer;
    FParameter3  : integer;
    FParameter4 : integer;
    FResponseCounter : integer;
    procedure Consequence;
    procedure Response;
    procedure Pass;
    function GetStartMethod : TThreadMethod;
  public
    destructor Destroy; override;
    procedure AssignParameters; virtual; abstract;
    procedure DoResponse; virtual; abstract;
    procedure Reset; virtual; abstract;
    property StartMethod : TThreadMethod read GetStartMethod;
    property OnConsequence : TNotifyEvent read FOnConsequence write FOnConsequence;
    property OnResponse : TNotifyEvent read FOnResponse write FOnResponse;
    property Parameter1 : Integer read FParameter1 write FParameter1;
    property Parameter2 : Integer read FParameter2 write FParameter2;
    property Parameter3 : Integer read FParameter3 write FParameter3;
    property Parameter4 : Integer read FParameter4 write FParameter4;
  end;

  { TSchRR }

  TSchRR = class(TAbsSch)
  private
    FResponseRatio,
    FRatioVariation,
    FRatio : Integer;
  public
    procedure AssignParameters; override;
    procedure DoResponse; override;
    procedure Reset; override;
  end;

  { TSchRI }

  TSchRI = class(TAbsSch)
  private
    FFlagClock: Boolean;
    FTimeInterval,
    FIntervalVariation,
    FInterval: Integer;
    procedure UpdateInterval;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AssignParameters; override;
    procedure Clock(Sender : TObject);
    procedure DoResponse; override;
    procedure Reset; override;
  end;

  { TSchRT }

  TSchRT = class(TAbsSch)
  private
    FTimeInterval,
    FIntervalVariation,
    FInterval: Integer;
    procedure UpdateInterval;
  public
    constructor Create(AOwner : TComponent); override;
    procedure AssignParameters; override;
    procedure Clock(Sender : TObject);
    procedure DoResponse; override;
    procedure Reset; override;
  end;

  { TSchDRH }

  TSchDRH = class(TAbsSch)
  private
    //FResponseRatio,
    //FRatioVariation,
    //FTimeInterval,
    //FIntervalVariation,
    FRatio,
    FInterval : Integer;
  public
    constructor Create(AOwner : TComponent); override;
    procedure AssignParameters; override;
    procedure Clock(Sender : TObject);
    procedure DoResponse; override;
    procedure Reset; override;
  end;

  { TSchDRL }

  TSchDRL = class(TAbsSch)
  private
    FInterval : Integer;
    //FIntervalVariation : integer;
    //FTimeInterval : integer;
    FFlagClock : Boolean;
    //procedure DestroyClock;
    //procedure CreateClock;
  public
    constructor Create(AOwner : TComponent); override;
    procedure AssignParameters; override;
    procedure Clock(Sender : TObject);
    procedure DoResponse; override;
    procedure Reset; override;

  end;

implementation

{ TSchRR }

procedure TSchRR.AssignParameters;
begin
  FResponseRatio := FParameter1;
  FRatioVariation := FParameter2;
end;


procedure TSchRR.DoResponse;
begin
  Response;
  if FResponseCounter = FRatio then Consequence;
end;

procedure TSchRR.Reset;
begin
  FResponseCounter := 0;
  FRatio := FResponseRatio - FRatioVariation + Random((2 * FRatioVariation) + 1);
  if FRatio < 1 then FRatio := FResponseRatio;
end;

{ TSchRI }

constructor TSchRI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClockThread := TClockThread.Create(True);
  FClockThread.OnTimer := @Clock;
  FFlagClock := False;
end;

procedure TSchRI.AssignParameters;
begin
  FTimeInterval := FParameter1;
  FIntervalVariation := FParameter2;
end;

procedure TSchRI.Clock(Sender : TObject);
begin
  FClockThread.Enabled := False;
  FFlagClock := True;
end;

procedure TSchRI.DoResponse;
begin
  Response;
  if FFlagClock then Consequence;
end;

procedure TSchRI.Reset;
begin
  FFlagClock:= False;
  UpdateInterval;
end;

procedure TSchRI.UpdateInterval;
var NewInterval : integer;
begin
  NewInterval := FTimeInterval - FIntervalVariation + Random((2 * FIntervalVariation) + 1);
  if NewInterval < 1 then FInterval := FTimeInterval
  else FInterval := NewInterval;

  FClockThread.Interval := FInterval;
  FClockThread.Enabled := True;
end;


{ TSchRT }

procedure TSchRT.AssignParameters;
begin
  FTimeInterval := FParameter1;
  FIntervalVariation := FParameter2;
end;

procedure TSchRT.Clock(Sender : TObject);
begin
  Consequence;
end;

procedure TSchRT.DoResponse;
begin
  Response;
end;

procedure TSchRT.Reset;
begin
  UpdateInterval;
end;

procedure TSchRT.UpdateInterval;
var NewInterval : integer;
begin
  NewInterval := FTimeInterval - FIntervalVariation + Random((2 * FIntervalVariation) + 1);
  if NewInterval < 1 then FInterval := FTimeInterval
  else FInterval := NewInterval;

  FClockThread.Interval := FInterval;
end;

constructor TSchRT.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClockThread := TClockThread.Create(True);
  FClockThread.OnTimer := @Clock;
end;

{ TSchDRH }

constructor TSchDRH.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  FClockThread := TClockThread.Create(True);
  FClockThread.OnTimer := @Clock;
end;

procedure TSchDRH.AssignParameters;
begin
  FRatio := Parameter1;
  FInterval := Parameter2;
  //FRatioVariation := Parameter3;
  //FIntervalVariation := Parameter4;

  FClockThread.Interval := FInterval;
end;

procedure TSchDRH.Clock(Sender : TObject);
begin
  FResponseCounter := 0;
end;

procedure TSchDRH.DoResponse;
begin
  Response;
  if FResponseCounter = FRatio then Consequence;
end;

procedure TSchDRH.Reset;
begin
  FResponseCounter := 0;
end;

{ TSchDRL }

constructor TSchDRL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClockThread := TClockThread.Create(True);
  FClockThread.OnTimer := @Clock;
  FFlagClock := False;
end;

procedure TSchDRL.AssignParameters;
begin
  FInterval := Parameter1;
  FClockThread.Interval := FInterval;
end;

procedure TSchDRL.Clock(Sender : TObject);
begin
  FFlagClock := True;
end;

procedure TSchDRL.DoResponse;
begin
  Response;
  if FFlagClock then Consequence
  else Reset;
end;

procedure TSchDRL.Reset;
begin
  FClockThread.Reset;
  FFlagClock:= False;
end;

{ TAbsSch }

procedure TAbsSch.Consequence;
begin
  Reset;
  if Assigned(OnConsequence) then
    begin
      FOnConsequence(Self);
    end;
end;

procedure TAbsSch.Response;
begin
  Inc(FResponseCounter);
  if Assigned(OnResponse) then OnResponse(Self);
end;

procedure TAbsSch.Pass;
begin
  // do nothing
end;

function TAbsSch.GetStartMethod: TThreadMethod;
begin
  if Assigned(FClockThread) then Result := @FClockThread.Start
  else Result := @Self.Pass;
end;

destructor TAbsSch.Destroy;
begin
  FClockThread.Enabled := False;
  if Assigned(FClockThread) then FClockThread.Terminate;
  inherited Destroy;
end;



end.

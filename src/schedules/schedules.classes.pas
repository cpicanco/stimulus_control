{
  Schedules
  Copyright (C) 2007 Drausio Capobianco, Universidade Federal de São Carlos.
  Copyright (C) 2010-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Schedules.Classes;

{$mode objfpc}{$H+}

interface

uses Schedules.Abstract;

type

  { TUnknownSchedule }

  TUnknownSchedule = class sealed (TSchedules)
  protected
    function GetParameter1: Cardinal; override;
    function GetParameter2: Cardinal; override;
    procedure SetParameter1(AValue: Cardinal); override;
    procedure SetParameter2(AValue: Cardinal); override;
  end;

  { TRatioSchedule }

  // Allow fixed and pseudo-random ratios.
  TRatioSchedule = class sealed (TSchedules)
  private
    FBaseResponseRatio,
    FRatioVariation,
    FCurrentRatio : Cardinal;
  protected
    function GetParameter1: Cardinal; override;
    function GetParameter2: Cardinal; override;
    procedure SetParameter1(AValue: Cardinal); override;
    procedure SetParameter2(AValue: Cardinal); override;
  public
    constructor Create; override;
    procedure DoResponse; override;
    procedure Reset; override;
  end;

  { TIntervalSchedule }

  // Allow fixed and pseudo-random intervals.
  TIntervalSchedule = class sealed (TSchedules)
  private
    FFlagClock: Boolean;
    FBaseTimeInterval,
    FIntervalVariation : Cardinal;
  protected
    function GetParameter1: Cardinal; override;
    function GetParameter2: Cardinal; override;
    procedure SetParameter1(AValue: Cardinal); override;
    procedure SetParameter2(AValue: Cardinal); override;
  public
    constructor Create; override;
    procedure Clock(Sender : TObject);
    procedure DoResponse; override;
    procedure Reset; override;
  end;

  { TTimeSchedule }

  // Allow fixed and pseudo-random times.
  TTimeSchedule = class sealed (TSchedules)
  private
    FBaseTimeInterval,
    FIntervalVariation : Cardinal;
  protected
    function GetParameter1: Cardinal; override;
    function GetParameter2: Cardinal; override;
    procedure SetParameter1(AValue: Cardinal); override;
    procedure SetParameter2(AValue: Cardinal); override;
  public
    constructor Create; override;
    procedure Clock(Sender : TObject);
    procedure DoResponse; override;
    procedure Reset; override;
  end;

  { TDRHSchedule }

  // Differential Reinforcement of High Rates.
  TDRHSchedule = class sealed (TSchedules)
  private
    FRatio : Cardinal;
  protected
    function GetParameter1: Cardinal; override;
    function GetParameter2: Cardinal; override;
    procedure SetParameter1(AValue: Cardinal); override;
    procedure SetParameter2(AValue: Cardinal); override;
  public
    constructor Create; override;
    procedure Clock(Sender : TObject);
    procedure DoResponse; override;
    procedure Reset; override;
  end;

  { TDRLSchedule }

  // Differential Reinforcement of Low Rates.
  TDRLSchedule = class sealed (TSchedules)
  private
    FRatio : Cardinal;
    FFlagClock : Boolean;
  protected
    function GetParameter1: Cardinal; override;
    function GetParameter2: Cardinal; override;
    procedure SetParameter1(AValue: Cardinal); override;
    procedure SetParameter2(AValue: Cardinal); override;
  public
    constructor Create; override;
    procedure Clock(Sender : TObject);
    procedure DoResponse; override;
    procedure Reset; override;
  end;

implementation

{ TUnknownSchedule }

function TUnknownSchedule.GetParameter1: Cardinal;
begin
  Result := 0;
end;

function TUnknownSchedule.GetParameter2: Cardinal;
begin
  Result := 0;
end;

procedure TUnknownSchedule.SetParameter1(AValue: Cardinal);
begin
  { do nothing }
end;

procedure TUnknownSchedule.SetParameter2(AValue: Cardinal);
begin
  { do nothing }
end;

{ TRatioSchedule }

function TRatioSchedule.GetParameter1: Cardinal;
begin
  Result := FBaseResponseRatio;
end;

function TRatioSchedule.GetParameter2: Cardinal;
begin
  Result := FRatioVariation;
end;

procedure TRatioSchedule.SetParameter1(AValue: Cardinal);
begin
  FBaseResponseRatio := AValue;
end;

procedure TRatioSchedule.SetParameter2(AValue: Cardinal);
begin
  FRatioVariation := AValue;
end;

constructor TRatioSchedule.Create;
begin
  inherited Create;
  FBaseResponseRatio:=0;
  FCurrentRatio:=0;
  FRatioVariation:=0;
end;

procedure TRatioSchedule.DoResponse;
begin
  Response;
  if Responses = FCurrentRatio then
    Consequence;
end;

procedure TRatioSchedule.Reset;
begin
  ResetResponses;
  UpdateRatio(FCurrentRatio,FBaseResponseRatio,FRatioVariation);
end;

{ TIntervalSchedule }

constructor TIntervalSchedule.Create;
begin
  inherited Create(@Clock);
  FFlagClock := False;
  FBaseTimeInterval:= 0;
  FIntervalVariation:= 0;
end;

procedure TIntervalSchedule.Clock(Sender : TObject);
begin
  StopClock;
  FFlagClock := True;
end;

procedure TIntervalSchedule.DoResponse;
begin
  Response;
  if FFlagClock then
    Consequence;
end;

procedure TIntervalSchedule.Reset;
begin
  UpdateInterval(FBaseTimeInterval, FIntervalVariation);
  FFlagClock:= False;
  StartClock;
end;

function TIntervalSchedule.GetParameter1: Cardinal;
begin
  Result := FBaseTimeInterval;
end;

function TIntervalSchedule.GetParameter2: Cardinal;
begin
  Result := FIntervalVariation;
end;

procedure TIntervalSchedule.SetParameter1(AValue: Cardinal);
begin
  FBaseTimeInterval := AValue;
end;

procedure TIntervalSchedule.SetParameter2(AValue: Cardinal);
begin
  FIntervalVariation := AValue;
end;


{ TTimeSchedule }

constructor TTimeSchedule.Create;
begin
  inherited Create(@Clock);
  FBaseTimeInterval := 0;
  FIntervalVariation:= 0;
end;

procedure TTimeSchedule.Clock(Sender : TObject);
begin
  Consequence;
end;

procedure TTimeSchedule.DoResponse;
begin
  Response;
end;

procedure TTimeSchedule.Reset;
begin
  if (FIntervalVariation > 0) or MustUpdate then begin
    StopClock;
    UpdateInterval(FBaseTimeInterval, FIntervalVariation);
    StartClock;
    if MustUpdate then MustUpdate := False;
  end;
end;

function TTimeSchedule.GetParameter1: Cardinal;
begin
  Result := FBaseTimeInterval;
end;

function TTimeSchedule.GetParameter2: Cardinal;
begin
  Result := FIntervalVariation;
end;

procedure TTimeSchedule.SetParameter1(AValue: Cardinal);
begin
  FBaseTimeInterval := AValue;
  UpdateInterval(FBaseTimeInterval, FIntervalVariation);
end;

procedure TTimeSchedule.SetParameter2(AValue: Cardinal);
begin
  FIntervalVariation := AValue;
end;

{ TDRHSchedule }

function TDRHSchedule.GetParameter1: Cardinal;
begin
  Result := FRatio;
end;

function TDRHSchedule.GetParameter2: Cardinal;
begin
  Result := GetInterval;
end;

procedure TDRHSchedule.SetParameter1(AValue: Cardinal);
begin
  FRatio := AValue;
end;

procedure TDRHSchedule.SetParameter2(AValue: Cardinal);
begin
  UpdateInterval(AValue);
end;

constructor TDRHSchedule.Create;
begin
  inherited Create(@Clock);
  FRatio := 0;
end;

procedure TDRHSchedule.Clock(Sender : TObject);
begin
  ResetResponses;
end;

procedure TDRHSchedule.DoResponse;
begin
  Response;
  if Responses = FRatio then Consequence;
end;

procedure TDRHSchedule.Reset;
begin
  ResetResponses;
  ResetClock;
end;

{ TDRLSchedule }

function TDRLSchedule.GetParameter1: Cardinal;
begin
  Result := FRatio;
end;

function TDRLSchedule.GetParameter2: Cardinal;
begin
  Result := GetInterval;
end;

procedure TDRLSchedule.SetParameter1(AValue: Cardinal);
begin
  FRatio := AValue;
end;

procedure TDRLSchedule.SetParameter2(AValue: Cardinal);
begin
  UpdateInterval(AValue);
end;

constructor TDRLSchedule.Create;
begin
  inherited Create(@Clock);
  FFlagClock := False;
end;

procedure TDRLSchedule.Clock(Sender : TObject);
begin
  FFlagClock := True;
end;

procedure TDRLSchedule.DoResponse;
begin
  Response;
  if FFlagClock then Consequence else
    if Responses = FRatio then Reset;
end;

procedure TDRLSchedule.Reset;
begin
  FFlagClock:= False;
  ResetClock;
  ResetResponses;
end;


end.

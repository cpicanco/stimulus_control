{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Schedules.Classes;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils
    , Schedules.Abstract
    {$ifdef DEBUG}
    , Loggers.Debug
    {$endif}
    ;

type

  { TRatioSchedule }

  // Allow fixed and pseudo-random ratios.
  TRatioSchedule = class(TSchedules)
  private
    FResponseRatio,
    FRatioVariation,
    FRatio : Integer;
  protected
    procedure SetParameter1(AValue:integer); override;
    procedure SetParameter2(AValue:integer); override;
  public
    procedure DoResponse; override;
    procedure Reset; override;
  end;

  { TIntervalSchedule }

  // Allow fixed and pseudo-random intervals.
  TIntervalSchedule = class(TSchedules)
  private
    FFlagClock: Boolean;
    FTimeInterval,
    FIntervalVariation,
    FInterval: Integer;
    procedure UpdateInterval;
  protected
    procedure SetParameter1(AValue:integer); override;
    procedure SetParameter2(AValue:integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clock(Sender : TObject);
    procedure DoResponse; override;
    procedure Reset; override;
  end;

  { TTimeSchedule }

  // Allow fixed and pseudo-random times.
  TTimeSchedule = class(TSchedules)
  private
    FTimeInterval,
    FIntervalVariation,
    FInterval: Integer;
    procedure UpdateInterval;
  protected
    procedure SetParameter1(AValue:integer); override;
    procedure SetParameter2(AValue:integer); override;
  public
    constructor Create(AOwner : TComponent); override;
    procedure Clock(Sender : TObject);
    procedure DoResponse; override;
    procedure Reset; override;
  end;

  { TDRHSchedule }

  // Differential Reinforcement of High Rates.
  TDRHSchedule = class(TSchedules)
  private
    //FResponseRatio,
    //FRatioVariation,
    //FTimeInterval,
    //FIntervalVariation,
    FRatio,
    FInterval : Integer;
  protected
    procedure SetParameter1(AValue:integer); override;
    procedure SetParameter2(AValue:integer); override;
  public
    constructor Create(AOwner : TComponent); override;
    procedure Clock(Sender : TObject);
    procedure DoResponse; override;
    procedure Reset; override;
  end;

  { TDRLSchedule }

  // Differential Reinforcement of Low Rates.
  TDRLSchedule = class(TSchedules)
  private
    FInterval : Integer;
    //FIntervalVariation : integer;
    //FTimeInterval : integer;
    FFlagClock : Boolean;
  protected
    procedure SetParameter1(AValue:integer); override;
  public
    constructor Create(AOwner : TComponent); override;
    procedure Clock(Sender : TObject);
    procedure DoResponse; override;
    procedure Reset; override;

  end;

implementation

{ TRatioSchedule }

procedure TRatioSchedule.SetParameter1(AValue: integer);
begin
  FResponseRatio := AValue;
end;

procedure TRatioSchedule.SetParameter2(AValue: integer);
begin
  FRatioVariation := AValue;
end;

procedure TRatioSchedule.DoResponse;
begin
  Response;
  if Responses = FRatio then
    Consequence;
end;

procedure TRatioSchedule.Reset;
begin
  ResetResponses;
  FRatio := RandomAmplitude(FResponseRatio,FRatioVariation);
  if FRatio < 1 then
    FRatio := FResponseRatio;
end;

{ TIntervalSchedule }

constructor TIntervalSchedule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateClock;
  FClockThread.OnTimer := @Clock;
  {$ifdef DEBUG}
    FClockThread.OnDebugStatus := @DebugStatus;
  {$endif}
  FFlagClock := False;
end;

procedure TIntervalSchedule.Clock(Sender : TObject);
begin
  FClockThread.Enabled := False;
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
  FFlagClock:= False;
  UpdateInterval;
end;

procedure TIntervalSchedule.UpdateInterval;
var LNewInterval : integer;
begin
  LNewInterval:=RandomAmplitude(FTimeInterval,FIntervalVariation);
  if LNewInterval < 1 then
    FInterval := FTimeInterval
  else FInterval := LNewInterval;

  FClockThread.Interval := FInterval;
  FClockThread.Enabled := True;
end;

procedure TIntervalSchedule.SetParameter1(AValue: integer);
begin
  FTimeInterval := AValue;
end;

procedure TIntervalSchedule.SetParameter2(AValue: integer);
begin
  FIntervalVariation := AValue;
end;


{ TTimeSchedule }

constructor TTimeSchedule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateClock;
  FClockThread.OnTimer := @Clock;
  {$ifdef DEBUG}
    FClockThread.OnDebugStatus := @DebugStatus;
  {$endif}
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
  UpdateInterval;
end;

procedure TTimeSchedule.UpdateInterval;
var LNewInterval : integer;
begin
  LNewInterval:=RandomAmplitude(FTimeInterval,FIntervalVariation);
  if LNewInterval < 1 then
    FInterval := FTimeInterval
  else FInterval := LNewInterval;

  FClockThread.Interval := FInterval;
end;

procedure TTimeSchedule.SetParameter1(AValue: integer);
begin
  FTimeInterval := AValue;
end;

procedure TTimeSchedule.SetParameter2(AValue: integer);
begin
  FIntervalVariation := AValue;
end;

{ TDRHSchedule }

procedure TDRHSchedule.SetParameter1(AValue: integer);
begin
  FRatio := AValue;
  //FRatioVariation := Parameter3;
end;

procedure TDRHSchedule.SetParameter2(AValue: integer);
begin
  FInterval := AValue;
  FClockThread.Interval := FInterval;
  //FIntervalVariation := Parameter4;
end;

constructor TDRHSchedule.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  CreateClock;
  FClockThread.OnTimer := @Clock;
  {$ifdef DEBUG}
    FClockThread.OnDebugStatus := @DebugStatus;
  {$endif}
end;

procedure TDRHSchedule.Clock(Sender : TObject);
begin
  ResetResponses;
end;

procedure TDRHSchedule.DoResponse;
begin
  Response;
  if Responses = FRatio then
    Consequence;
end;

procedure TDRHSchedule.Reset;
begin
  ResetResponses;
end;

{ TDRLSchedule }

procedure TDRLSchedule.SetParameter1(AValue: integer);
begin
  FInterval := AValue;
  FClockThread.Interval := FInterval;
end;

constructor TDRLSchedule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateClock;
  FClockThread.OnTimer := @Clock;
  {$ifdef DEBUG}
    FClockThread.OnDebugStatus := @DebugStatus;
  {$endif}
  FFlagClock := False;
end;

procedure TDRLSchedule.Clock(Sender : TObject);
begin
  FFlagClock := True;
end;

procedure TDRLSchedule.DoResponse;
begin
  Response;
  if FFlagClock then
    Consequence
  else Reset;
end;

procedure TDRLSchedule.Reset;
begin
  FClockThread.Reset;
  FFlagClock:= False;
end;


end.

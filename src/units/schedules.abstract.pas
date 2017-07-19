unit Schedules.Abstract;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , custom_timer
  ;

type
  { TSchedules }

  // Base class for all schedules. Do not create it directly, use TSchedule instead.
  TSchedules = class(TComponent)
  private
    FOnConsequence : TNotifyEvent;
    FOnResponse : TNotifyEvent;
    FResponseCounter : integer;
    function GetStartMethod : TThreadMethod;
    function GetTimeEnabled : Boolean;
    procedure Pass;
    procedure SetTimeEnabled(AValue: Boolean);
  {$ifdef DEBUG}
  strict protected
    procedure DebugStatus(DebugMessage : string);
  {$endif}
  protected
    FClockThread : TClockThread;
    function RandomAmplitude(AValue, AAmplitude:Integer) : Integer;
    procedure ResetResponses;
    procedure CreateClock;
    procedure Consequence;
    procedure Response;
    procedure SetParameter1(AValue: Integer); virtual; abstract;
    procedure SetParameter2(AValue: Integer); virtual; abstract;
    //procedure SetParameter3(AValue: Integer); virtual; abstract;
    //procedure SetParameter4(AValue: Integer); virtual; abstract;
  public
    destructor Destroy; override;
    procedure DoResponse; virtual; abstract;
    procedure Reset; virtual; abstract;
    property TimeEnabled : Boolean read GetTimeEnabled write SetTimeEnabled;
    property StartMethod : TThreadMethod read GetStartMethod;
    property OnConsequence : TNotifyEvent read FOnConsequence write FOnConsequence;
    property OnResponse : TNotifyEvent read FOnResponse write FOnResponse;
    property Parameter1 : Integer write SetParameter1;
    property Parameter2 : Integer write SetParameter2;
    //property Parameter3 : Integer write SetParameter3;
    //property Parameter4 : Integer write SetParameter4;
    property Responses : Integer read FResponseCounter;
  end;

implementation

{$ifdef DEBUG}
uses Loggers.Debug;
{$endif}

{ TSchedules }

procedure TSchedules.Consequence;
begin
  Reset;
  if Assigned(OnConsequence) then
    begin
      FOnConsequence(Self);
    end;
end;

function TSchedules.GetTimeEnabled: Boolean;
begin
  if Assigned(FClockThread) then
    Result := FClockThread.Enabled
  else Result := False;
end;

procedure TSchedules.Response;
begin
  Inc(FResponseCounter);
  if Assigned(OnResponse) then
    OnResponse(Self);
end;

procedure TSchedules.Pass;
begin
  // do nothing
end;

function TSchedules.GetStartMethod: TThreadMethod;
begin
  if Assigned(FClockThread) then
    Result := @FClockThread.Start
  else Result := @Self.Pass;
end;

procedure TSchedules.SetTimeEnabled(AValue: Boolean);
begin
  if Assigned(FClockThread) then
    begin
      if FClockThread.Enabled = AValue then Exit;
      FClockThread.Enabled := AValue;
    end;
end;

function TSchedules.RandomAmplitude(AValue, AAmplitude:Integer): integer;
begin
  Result := AValue - AAmplitude + Random((2 * AAmplitude) + 1);
end;

procedure TSchedules.ResetResponses;
begin
  FResponseCounter:=0;
end;

procedure TSchedules.CreateClock;
begin
  FClockThread := TClockThread.Create(True);
end;

{$ifdef DEBUG}
procedure TSchedules.DebugStatus(DebugMessage : string);
begin
  DebugLn(DebugMessage);
end;
{$endif}

destructor TSchedules.Destroy;
begin
  if Assigned(FClockThread) then
    begin
      FClockThread.Enabled := False;
      FClockThread.Terminate;
    end;
  inherited Destroy;
end;

end.


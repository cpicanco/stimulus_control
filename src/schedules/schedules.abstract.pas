{
  Schedules
  Copyright (C) 2007 Drausio Capobianco, Universidade Federal de São Carlos.
  Copyright (C) 2010-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Schedules.Abstract;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls;

type

  TPausedState = record
    MustUpdate   : Boolean;
    Enabled      : Boolean;
    TimerEnabled : Boolean;
    Interval     : Extended;
    Elapsed      : Extended;
    Started      : Extended;
    Paused       : Extended;
  end;

  TRandomIntervalType = (
    ritRandomAmplitude,
    ritFleshlerHoffman,
    ritRegisNetoProgression,
    ritRegisNetoNoProgression
  );

  TClockStartMethod = procedure of object;

  { TSchedules }

  // Base class for all schedules. Do not create it directly, use TSchedule instead.
  TSchedules = class
  private
    FRandomValuesList : TStringList;
    FOnResponseReady: TNotifyEvent;
    FPausedState : TPausedState;
    FRandomIntervalType : TRandomIntervalType;
    FOnConsequence : TNotifyEvent;
    FOnResponse : TNotifyEvent;
    FResponseCounter : Cardinal;
    FTimer : TTimer;
    function GetEnabled: Boolean;
    function GetMustUpdate : Boolean;
    function GetPaused : Boolean;
    function GetStartMethod : TClockStartMethod;
    function RandomAmplitude(AValue, AAmplitude: Cardinal) : Cardinal;
    function FleshlerHoffman : Cardinal;
    function RegisNeto(NoProgression: Boolean = False) : Cardinal;
    procedure Pass;
    procedure UpdateRandomValuesList;
    procedure SetEnabled(AValue: Boolean);
    procedure SetMustUpdate(AValue : Boolean);
    procedure SetOnConsequence(AValue: TNotifyEvent);
    procedure SetOnResponse(AValue: TNotifyEvent);
    procedure SetOnResponseReady(AValue: TNotifyEvent);
    procedure SetPaused(AValue : Boolean);
  protected
    function GetInterval : Cardinal;
    function GetParameter1: Cardinal; virtual; abstract;
    function GetParameter2: Cardinal; virtual; abstract;
    procedure SetParameter1(AValue: Cardinal); virtual; abstract;
    procedure SetParameter2(AValue: Cardinal); virtual; abstract;
  protected
    procedure ResetResponses;inline;
    procedure StartClock;inline;
    procedure StopClock;inline;
    procedure ResetClock;inline;
    procedure UpdateInterval(ABase: Cardinal; AVariation: Cardinal = 0); inline;
    procedure UpdateRatio(out ATarget : Cardinal; ABase: Cardinal; AVariation: Cardinal = 0); inline;
    procedure Consequence; inline;
    procedure Response; inline;
    procedure ResponseReady; inline;
  public
    constructor Create; virtual; overload;
    constructor Create(OnTimer : TNotifyEvent); overload;
    destructor Destroy; override;
    procedure DoResponse; virtual; abstract;
    procedure Reset; virtual; abstract;
    function Start : Extended;
    function Stop : Extended;
    function Pause : Extended;
    function Resume : Extended;
    property OnConsequence : TNotifyEvent read FOnConsequence write SetOnConsequence;
    property OnResponse : TNotifyEvent read FOnResponse write SetOnResponse;
    property OnResponseReady : TNotifyEvent read FOnResponseReady write SetOnResponseReady;
    property Responses : Cardinal read FResponseCounter;
    property RandomIntervalType : TRandomIntervalType read FRandomIntervalType write FRandomIntervalType;
    property MustUpdate : Boolean read GetMustUpdate write SetMustUpdate;
  published
    property Parameter1 : Cardinal read GetParameter1 write SetParameter1;
    property Parameter2 : Cardinal read GetParameter2 write SetParameter2;
    property Enabled : Boolean read GetEnabled write SetEnabled;
    property Paused : Boolean read GetPaused write SetPaused;
  end;

resourcestring
  RSErrorCreatingUnknownSchedule = 'Unknown Schedule cannot be created.';
  RSErrorGettingUnknownScheduleParameters = 'Unknown Schedule have no parameters to get.';
  RSErrorSettingUnknownScheduleParameters = 'Unknown Schedule does not have setable parameters.';
  RSErrorInvalidValue = 'Value must be higher than zero.';
  RSErrorInvalidVariation = 'Variation must be lower than value.';
  RSErrorParameterDoesNotExist = 'Requested parameter does not exist.';
  RSErrorUnknownScheduleAction = 'Unknow schedule. Load a schedule first.';

implementation

uses Timestamps, Math;

type
  TArrayOfInteger = array of integer;
  TRegisNetoIndex = 0..9;
  TFleshlerHoffmanIndex = 0..9;

var

  RegisNetoIndex : TRegisNetoIndex = 0;
  RegisNetoValues : array [TRegisNetoIndex] of integer =
    (1000, 2000, 3000, 4000, 5000, 10000, 20000, 30000, 40000, 60000);

  FleshlerHoffmanValues : array [TFleshlerHoffmanIndex] of integer =
    (207, 652, 1154, 1727, 2397, 3202, 4213, 5572, 7665, 13210);

procedure TSchedules.UpdateRandomValuesList;
var
  Value, R, i : integer;
  RandomVIS : TArrayOfInteger;
  function Append(AArray : TArrayOfInteger; AItem : integer) : TArrayOfInteger;
  begin
    SetLength(AArray, Length(AArray)+1);
    AArray[High(AArray)] := AItem;
    Result := AArray;
  end;
begin
  FRandomValuesList.Clear;
  case FRandomIntervalType of
    ritRandomAmplitude: begin
      { do nothing }
    end;

    ritFleshlerHoffman: begin
      for Value := Low(TFleshlerHoffmanIndex) to High(TFleshlerHoffmanIndex) do begin
        FRandomValuesList.Append(Value.ToString);
      end;
    end;

    ritRegisNetoProgression, ritRegisNetoNoProgression: begin
      Value := RegisNetoValues[RegisNetoIndex];
      repeat
        RandomVIS := Default(TArrayOfInteger);
        for i := 0 to 4 do begin
          R := RandomAmplitude(Value, (Value*30) div 100);
          RandomVIS := Append(RandomVIS, R);
        end;
      until Mean(RandomVIS) = Value;
      for Value in RandomVIS do begin
        FRandomValuesList.Append(Value.ToString);
      end;
    end;
  end;
end;

{ TSchedules }

procedure TSchedules.Consequence;
begin
  if not Paused then begin
    Reset;
    if Assigned(OnConsequence) then
      OnConsequence(Self);
  end;
end;

procedure TSchedules.Response;
begin
  if not Paused then begin
    Inc(FResponseCounter);
    if Assigned(OnResponse) then
      OnResponse(Self);
  end;
end;

procedure TSchedules.ResponseReady;
begin
  if Assigned(OnResponseReady) then
    OnResponseReady(Self);
end;

constructor TSchedules.Create;
begin
  FTimer := nil;
  FPausedState.Enabled := False;
end;

constructor TSchedules.Create(OnTimer: TNotifyEvent);
begin
  FTimer := TTimer.Create(nil);
  FTimer.OnTimer := OnTimer;
  FRandomIntervalType := ritRandomAmplitude;
  FRandomValuesList := TStringList.Create;
  FRandomValuesList.Sorted := False;
  UpdateRandomValuesList;
end;

procedure TSchedules.Pass;
begin
  // do nothing
end;

procedure TSchedules.SetEnabled(AValue: Boolean);
begin
  if Assigned(FTimer) then
  begin
    if AValue then
    begin
      Reset;
      StartClock;
    end else
      StopClock;
  end else
  begin
    if AValue then Reset;
  end;
end;

procedure TSchedules.SetMustUpdate(AValue : Boolean);
begin
  if AValue = FPausedState.MustUpdate then Exit;
  FPausedState.MustUpdate := AValue;
end;

procedure TSchedules.SetOnConsequence(AValue: TNotifyEvent);
begin
  if FOnConsequence=AValue then Exit;
  FOnConsequence:=AValue;
end;

procedure TSchedules.SetOnResponse(AValue: TNotifyEvent);
begin
  if FOnResponse=AValue then Exit;
  FOnResponse:=AValue;
end;

procedure TSchedules.SetOnResponseReady(AValue: TNotifyEvent);
begin
  if FOnResponseReady=AValue then Exit;
  FOnResponseReady:=AValue;
end;

procedure TSchedules.SetPaused(AValue : Boolean);
begin
  FPausedState.Enabled := AValue;
  if Assigned(FTimer) then begin
    if FTimer.Enabled then begin
      if FPausedState.Enabled then begin
        StopClock;
        FPausedState.TimerEnabled := True;
        FPausedState.Paused := TickCount;
      end else begin
        { should be unreachable }
      end;
    end else begin
      if FPausedState.Enabled then begin
        FPausedState.TimerEnabled := False;
        FPausedState.Paused := TickCount;
      end else begin
        if FPausedState.TimerEnabled then begin
          FPausedState.Elapsed := FPausedState.Elapsed +
            (FPausedState.Paused - FPausedState.Started);
          FTimer.Interval :=
            Round(FPausedState.Interval - (FPausedState.Elapsed*1000));
          FPausedState.MustUpdate := True;
          StartClock;
        end else begin
          FPausedState.Started := TickCount;
        end;
      end;
    end;
  end else begin
    { for now, do nothing }
  end;
end;

function TSchedules.GetInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TSchedules.StartClock;
begin
  FTimer.Enabled := True;
  FPausedState.Started := TickCount;
end;

procedure TSchedules.StopClock;
begin
  FTimer.Enabled := False;
end;

function TSchedules.GetStartMethod: TClockStartMethod;
begin
  if Assigned(FTimer) then Result := @Self.StartClock else Result := @Self.Pass;
end;

function TSchedules.GetEnabled: Boolean;
begin
  if Assigned(FTimer) then
    Result := FTimer.Enabled   // time/ interval
  else
    Result := True;            // ratio
end;

function TSchedules.GetMustUpdate : Boolean;
begin
  Result := FPausedState.MustUpdate;
end;

function TSchedules.GetPaused : Boolean;
begin
  Result := FPausedState.Enabled;
end;

function TSchedules.RandomAmplitude(AValue, AAmplitude: Cardinal): Cardinal;
begin
  Result := AValue;
  if AAmplitude > 0 then
    Result := AValue - AAmplitude + Random((2 * AAmplitude) + 1);
end;

function TSchedules.FleshlerHoffman : Cardinal;
var
  R : integer;
begin
  if FRandomValuesList.Count = 0 then UpdateRandomValuesList;
  R := Random(FRandomValuesList.Count);
  Result := FleshlerHoffmanValues[FRandomValuesList[R].ToInteger];
  FRandomValuesList.Delete(R);
end;

function TSchedules.RegisNeto(NoProgression: Boolean): Cardinal;
begin
  if FRandomValuesList.Count = 0 then begin
    if NoProgression then begin
      RegisNetoIndex := High(TRegisNetoIndex);
    end else begin
      if RegisNetoIndex < High(TRegisNetoIndex) then
        RegisNetoIndex := RegisNetoIndex + 1;
    end;
    UpdateRandomValuesList;
  end;
  Result := FRandomValuesList.Pop.ToInteger;
end;

procedure TSchedules.ResetResponses;
begin
  FResponseCounter:=0;
end;

procedure TSchedules.UpdateInterval(ABase: Cardinal; AVariation: Cardinal);
begin
  case FRandomIntervalType of
    ritRandomAmplitude: begin
      FTimer.Interval := RandomAmplitude(ABase, AVariation);
    end;

    ritFleshlerHoffman: begin
      FTimer.Interval := FleshlerHoffman;
    end;

    ritRegisNetoProgression: begin
      FTimer.Interval := RegisNeto;
    end;

    ritRegisNetoNoProgression: begin
      FTimer.Interval := RegisNeto(True);
    end;
  end;
  FPausedState.Interval := FTimer.Interval;
  FPausedState.Elapsed  := 0;
end;

procedure TSchedules.UpdateRatio(out ATarget: Cardinal; ABase: Cardinal;
  AVariation: Cardinal);
begin
  ATarget := RandomAmplitude(ABase, AVariation);
end;

destructor TSchedules.Destroy;
begin
  if Assigned(FRandomValuesList) then
    FRandomValuesList.Free;
  OnConsequence := nil;
  OnResponse := nil;
  if Assigned(FTimer) then
    FTimer.Free;
  inherited Destroy;
end;

function TSchedules.Start : Extended;
begin
  if Paused then begin
    Result := Resume;
  end else begin
    Reset;
    if Assigned(FTimer) then begin
      StartClock;
      Result := FPausedState.Started;
    end else begin
      Result := TickCount;
    end;
  end;
end;

function TSchedules.Stop : Extended;
begin
  with FPausedState do begin
    Enabled := False;
    Paused := 0;
    Started := 0;
    Interval := 0;
    Elapsed := 0;
    MustUpdate := False;
  end;

  Enabled := False;
  Result := TickCount;
end;

function TSchedules.Pause : Extended;
begin
  if Paused then Exit;
  Paused := True;
  Result := FPausedState.Paused;
end;

function TSchedules.Resume : Extended;
begin
  if Paused then begin
    Paused := False;
    Result := FPausedState.Started;
  end;
end;

procedure TSchedules.ResetClock;
begin
  FTimer.Enabled:=False;
  FTimer.Enabled:=True;
end;

end.


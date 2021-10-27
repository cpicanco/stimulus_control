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

  TRandomIntervalType = (ritRandomAmplitude, ritFleshlerHoffman);

  TClockStartMethod = procedure of object;

  { TSchedules }

  // Base class for all schedules. Do not create it directly, use TSchedule instead.
  TSchedules = class
  private
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
    procedure Pass;
    procedure SetEnabled(AValue: Boolean);
    procedure SetMustUpdate(AValue : Boolean);
    procedure SetOnConsequence(AValue: TNotifyEvent);
    procedure SetOnResponse(AValue: TNotifyEvent);
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
    function NextInterval : Cardinal;
    procedure PostponeLastInterval;
    property OnConsequence : TNotifyEvent read FOnConsequence write SetOnConsequence;
    property OnResponse : TNotifyEvent read FOnResponse write SetOnResponse;
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

uses Cheats, Timestamps, fgl;

type
  TIntegerList = specialize TFPGList<integer>;

var
  FleshlerHoffmanLastIndex : integer = -1;
  FleshlerHoffmanList : TIntegerList;
  FleshlerHoffmanValues : array [0..9] of Cardinal =
    (207, 652, 1154, 1727, 2397, 3202, 4213, 5572, 7665, 13210);

procedure UpdateFleshlerHoffmanList;
var
  i : integer;
begin
  FleshlerHoffmanList.Clear;
  for i := Low(FleshlerHoffmanValues) to High(FleshlerHoffmanValues) do begin
    FleshlerHoffmanList.Add(i);
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

procedure TSchedules.SetPaused(AValue : Boolean);
var
  LElapsed      : Extended;
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
          LElapsed := FPausedState.Interval - (FPausedState.Elapsed*1000);
          if LElapsed > 0 then begin
            FTimer.Interval := Round(LElapsed);
          end else begin
            FTimer.Interval := 1;
          end;
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
  if FleshlerHoffmanList.Count = 0 then UpdateFleshlerHoffmanList;
  R := Random(FleshlerHoffmanList.Count);
  Result := FleshlerHoffmanValues[FleshlerHoffmanList[R]];
  FleshlerHoffmanList.Delete(R);
  FleshlerHoffmanLastIndex := R;
  if CheatsModeOn then begin
    Result := Result div 10;
  end;
end;

procedure TSchedules.ResetResponses;
begin
  FResponseCounter:=0;
end;

procedure TSchedules.UpdateInterval(ABase: Cardinal; AVariation: Cardinal);
begin
  case FRandomIntervalType of
    ritRandomAmplitude:
      FTimer.Interval := RandomAmplitude(ABase, AVariation);

    ritFleshlerHoffman:
      FTimer.Interval := FleshlerHoffman;
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

function TSchedules.NextInterval: Cardinal;
begin
  case FRandomIntervalType of
    ritRandomAmplitude:
      Result := RandomAmplitude(Parameter1, Parameter2);

    ritFleshlerHoffman:
      Result := FleshlerHoffman;
  end;
end;

procedure TSchedules.PostponeLastInterval;
begin
  if FleshlerHoffmanLastIndex > -1 then begin
    FleshlerHoffmanList.Add(FleshlerHoffmanLastIndex);
    FleshlerHoffmanLastIndex := -1;
  end;
end;

procedure TSchedules.ResetClock;
begin
  FTimer.Enabled:=False;
  FTimer.Enabled:=True;
end;

initialization
  FleshlerHoffmanList := TIntegerList.Create;
  UpdateFleshlerHoffmanList;

finalization
  FleshlerHoffmanList.Free;

end.


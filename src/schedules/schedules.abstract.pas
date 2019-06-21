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
  Classes, ExtCtrls;

type

  TClockStartMethod = procedure of object;

  { TSchedules }

  // Base class for all schedules. Do not create it directly, use TSchedule instead.
  TSchedules = class
  private
    FOnConsequence : TNotifyEvent;
    FOnResponse : TNotifyEvent;
    FResponseCounter : Cardinal;
    FTimer : TTimer;
    function GetEnabled: Boolean;
    function GetStartMethod : TClockStartMethod;
    function RandomAmplitude(AValue, AAmplitude: Cardinal) : Cardinal;
    procedure Pass;
    procedure SetEnabled(AValue: Boolean);
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
    procedure Start; virtual;
    property OnConsequence : TNotifyEvent read FOnConsequence write FOnConsequence;
    property OnResponse : TNotifyEvent read FOnResponse write FOnResponse;
    property Responses : Cardinal read FResponseCounter;
  published
    property Parameter1 : Cardinal read GetParameter1 write SetParameter1;
    property Parameter2 : Cardinal read GetParameter2 write SetParameter2;
    property Enabled : Boolean read GetEnabled write SetEnabled;
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

{ TSchedules }

procedure TSchedules.Consequence;
begin
  Reset;
  if Assigned(OnConsequence) then
    FOnConsequence(Self);
end;

procedure TSchedules.Response;
begin
  Inc(FResponseCounter);
  if Assigned(OnResponse) then
    OnResponse(Self);
end;

constructor TSchedules.Create;
begin
  FTimer := nil;
end;

constructor TSchedules.Create(OnTimer: TNotifyEvent);
begin
  FTimer := TTimer.Create(nil);
  FTimer.OnTimer := OnTimer;
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

function TSchedules.GetInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TSchedules.StartClock;
begin
  FTimer.Enabled := True;
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

function TSchedules.RandomAmplitude(AValue, AAmplitude: Cardinal): Cardinal;
begin
  Result := AValue;
  if AAmplitude > 0 then
    Result := AValue - AAmplitude + Random((2 * AAmplitude) + 1);
end;

procedure TSchedules.ResetResponses;
begin
  FResponseCounter:=0;
end;

procedure TSchedules.UpdateInterval(ABase: Cardinal; AVariation: Cardinal);
begin
  FTimer.Interval := RandomAmplitude(ABase, AVariation);
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

procedure TSchedules.Start;
begin
  Reset;
  if Assigned(FTimer) then
    StartClock;
end;

procedure TSchedules.ResetClock;
begin
  FTimer.Enabled:=False;
  FTimer.Enabled:=True;
end;

end.


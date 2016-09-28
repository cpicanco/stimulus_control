{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit schedules;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils

    , custom_timer
    {$ifdef DEBUG}
    , debug_logger
    {$endif}
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
    function GetTimeEnabled: Boolean;
    function GetStartMethod : TThreadMethod;
    procedure Consequence;
    procedure Response;
    procedure Pass;
    procedure SetTimeEnabled(AValue: Boolean);
  {$ifdef DEBUG}
  strict protected
    procedure DebugStatus(DebugMessage : string);
  {$endif}
  public
    destructor Destroy; override;
    procedure AssignParameters; virtual; abstract;
    procedure DoResponse; virtual; abstract;
    procedure Reset; virtual; abstract;
    property TimeEnabled : Boolean read GetTimeEnabled write SetTimeEnabled;
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
  {$ifdef DEBUG}
    FClockThread.OnDebugStatus := @DebugStatus;
  {$endif}
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

constructor TSchRT.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClockThread := TClockThread.Create(True);
  FClockThread.OnTimer := @Clock;
  {$ifdef DEBUG}
    FClockThread.OnDebugStatus := @DebugStatus;
  {$endif}
end;

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

{ TSchDRH }

constructor TSchDRH.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  FClockThread := TClockThread.Create(True);
  FClockThread.OnTimer := @Clock;
  {$ifdef DEBUG}
    FClockThread.OnDebugStatus := @DebugStatus;
  {$endif}
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
  {$ifdef DEBUG}
    FClockThread.OnDebugStatus := @DebugStatus;
  {$endif}
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

function TAbsSch.GetTimeEnabled: Boolean;
begin
  if Assigned(FClockThread) then Result := FClockThread.Enabled
  else Result := False;
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

procedure TAbsSch.SetTimeEnabled(AValue: Boolean);
begin
  if Assigned(FClockThread) then
    begin
      if FClockThread.Enabled = AValue then Exit;
      FClockThread.Enabled := AValue;
    end;
end;

{$ifdef DEBUG}
procedure TAbsSch.DebugStatus(DebugMessage : string);
begin
  DebugLn(DebugMessage);
end;
{$endif}

destructor TAbsSch.Destroy;
begin
  if Assigned(FClockThread) then
    begin
      FClockThread.Enabled := False;
      FClockThread.Terminate;
    end;
  inherited Destroy;
end;



end.

{
  Schedules
  Copyright (C) 2007 Drausio Capobianco, Universidade Federal de São Carlos.
  Copyright (C) 2010-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Schedules;

{$mode objfpc}{$H+}

interface

uses Classes, Schedules.Abstract;

type

  TScheduleName = (UnknownSchedule,
    EXT, // Extintion = FR MaxtInt
    CRF, // Continuous reinforcement = FR 1
    FR,  // Fixed ratio
    FI,  // Fixed interval
    FT,  // Fixed time
    VR,  // Variable ratio
    VI,  // Variable interval
    VT,  // Variable time
    DRL, // Differential reinforcement of low rates
    DRH  // Differential reinforcement of high rates
  );


  { TSchedule }

  TSchedule = class sealed (TComponent)
  private
    FName : TScheduleName;
    FSchedule: TSchedules;
    function GetLoaded: Boolean;
    function GetParameter(i : integer): Cardinal;
    function GetParametersAsString: string; overload;
    function GetParameters(ASchedule: string): string; overload;
    function GetScheduleName: TScheduleName;
    function GetScheduleString: string;
    function GetScheduleNameAsString: string; overload;
    function GetScheduleName(ASchedule: string): string; overload;
    procedure LoadScheduleName(AScheduleName : TScheduleName); overload;
    procedure SetScheduleName(AName : string); overload;
    procedure SetOnConsequence(AValue: TNotifyEvent);
    procedure SetOnResponse(AValue: TNotifyEvent);
    procedure SetParameter(i : integer; AValue: Cardinal);
    procedure SetParameters(ASchedule: string); overload;
    procedure SetParameters(AParameter1, AParameter2 : Cardinal); overload; inline;
  private
    FOnConsequence: TNotifyEvent;
    FOnResponse: TNotifyEvent;
    function GetEnabled: Boolean;
    function GetParameter1: Cardinal;
    function GetParameter2: Cardinal;
    procedure Response(Sender : TObject);
    procedure Consequence(Sender : TObject);
    procedure SetEnabled(AValue: Boolean);
    procedure SetParameter1(AValue: Cardinal);
    procedure SetParameter2(AValue: Cardinal);
    procedure SetScheduleName(AValue: TScheduleName);
  public
    constructor Create(AOwner : TComponent); override; overload;
    constructor Create(AOwner : TComponent; ASchedule : string); overload;
    constructor Create(AOwner : TComponent; AScheduleName : TScheduleName;
      AParameter1: Cardinal = 0; AParameter2: Cardinal = 0); overload;
    destructor Destroy; override;
    procedure DoResponse;
    procedure Load(ASchedule : string); overload;
    procedure Load(AScheduleName : TScheduleName;
      AParameter1: Cardinal = 0; AParameter2: Cardinal = 0); overload;
    function Start : Extended;
    function Stop : Extended;
    function Pause : Extended;
    procedure UseFleshlerHoffmanIntervals;
    property AsString : string read GetScheduleString write Load;
    property Loaded : Boolean read GetLoaded;
    property ScheduleNameAsString : string read GetScheduleNameAsString;
    property Parameter[i : integer] : Cardinal read GetParameter write SetParameter;
    property ParametersAsString : string read GetParametersAsString;
  published
    property ScheduleName : TScheduleName read GetScheduleName write SetScheduleName;
    property OnConsequence: TNotifyEvent read FOnConsequence write SetOnConsequence;
    property OnResponse: TNotifyEvent read FOnResponse write SetOnResponse;
    property Parameter1: Cardinal read GetParameter1 write SetParameter1;
    property Parameter2: Cardinal read GetParameter2 write SetParameter2;
    property Enabled : Boolean read GetEnabled write SetEnabled;
  end;

implementation

uses SysUtils, StrUtils, Schedules.Classes;


procedure TSchedule.LoadScheduleName(AScheduleName: TScheduleName);
begin
  if FName = AScheduleName then Exit;
  if Assigned(FSchedule) then FreeAndNil(FSchedule);
  case AScheduleName of
    EXT, CRF, FR, VR : FSchedule := TRatioSchedule.Create;
              FI, VI : FSchedule := TIntervalSchedule.Create;
              FT, VT : FSchedule := TTimeSchedule.Create;
                 DRL : FSchedule := TDRLSchedule.Create;
                 DRH : FSchedule := TDRHSchedule.Create;
    UnknownSchedule  : FSchedule := TUnknownSchedule.Create;
  end;
  FName := AScheduleName;
  FSchedule.OnConsequence:=@Consequence;
  FSchedule.OnResponse:=@Response;
end;

procedure TSchedule.SetScheduleName(AName: string);
var
  LScheduleName : TScheduleName;
  LName : string;
  ValidScheduleNameFound : Boolean = False;
begin
  for LScheduleName := Low(TScheduleName) to High(TScheduleName) do
    begin
      WriteStr(LName,LScheduleName);
      if LName = GetScheduleName(AName) then
        begin
          ValidScheduleNameFound:=True;
          Break;
        end;
    end;
  if ValidScheduleNameFound then
      LoadScheduleName(LScheduleName)
  else
    raise Exception.Create(RSErrorCreatingUnknownSchedule) at
      get_caller_addr(get_frame),
      get_caller_frame(get_frame);
end;

procedure TSchedule.SetParameter1(AValue: Cardinal);
begin
  if Parameter[0] = AValue then Exit;
  Parameter[0] := AValue;
end;

procedure TSchedule.SetParameter2(AValue: Cardinal);
begin
  if Parameter[1] = AValue then Exit;
  Parameter[1] := AValue;
end;

procedure TSchedule.SetScheduleName(AValue: TScheduleName);
begin
  LoadScheduleName(AValue);
  case FName of
    UnknownSchedule : SetParameters(0, 0);
    CRF, EXT: SetParameters(0, 0);
    FR : SetParameters(5, 0);
    FI, FT: SetParameters(5000, 0);
    VR : SetParameters(5, 3);
    VI, VT: SetParameters(5000, 2500);
    DRH: SetParameters(5, 4000);
    DRL: SetParameters(1, 4000);
  end;
end;

procedure TSchedule.Response(Sender: TObject);
begin
  if Assigned(OnResponse) then OnResponse(Self);
end;

function TSchedule.GetParameter1: Cardinal;
begin
  Result := Parameter[0];
end;

function TSchedule.GetEnabled: Boolean;
begin
  Result := FSchedule.Enabled and Loaded;
end;

function TSchedule.GetParameter2: Cardinal;
begin
  Result := Parameter[1];
end;

procedure TSchedule.Consequence(Sender: TObject);
begin
  if Assigned(OnConsequence) then OnConsequence(Self);
end;

procedure TSchedule.SetEnabled(AValue: Boolean);
begin
  FSchedule.Enabled := AValue;
end;

constructor TSchedule.Create(AOwner: TComponent);
begin
  Inherited;
  FSchedule := TUnknownSchedule.Create;
  FName := UnknownSchedule;
end;

procedure TSchedule.SetParameters(AParameter1, AParameter2: Cardinal);
var
  LExceptionMessage : string;
begin
  case FName of
    CRF:
      begin
        FSchedule.Parameter1 := 1; // ratio
        FSchedule.Parameter2 := 0; // no variation
        Exit;
      end;

    EXT:
      begin
        FSchedule.Parameter1 := MaxInt; // ratio
        FSchedule.Parameter2 := 0;      // no variation
        Exit;
      end;

    FR, FI, FT:
      if AParameter1 > 0 then
        begin
          FSchedule.Parameter1 := AParameter1; // ratio or time
          FSchedule.Parameter2 := 0;           // no variation
          Exit;
        end
      else
        LExceptionMessage:=RSErrorInvalidValue;

    VR, VI, VT:
      if AParameter1 > 0 then
        begin
          FSchedule.Parameter1 := AParameter1; // ratio or time
          if AParameter2 < AParameter1 then
            begin
              FSchedule.Parameter2 := AParameter2; // variation
              Exit;
            end
          else
           LExceptionMessage := RSErrorInvalidVariation;
        end
      else
        LExceptionMessage := RSErrorInvalidValue;

    DRH, DRL:
      if (AParameter1 > 0) and (AParameter2 > 0) then
        begin
         FSchedule.Parameter1 := AParameter1; // ratio
         FSchedule.Parameter2 := AParameter2; // time
         Exit;
        end
      else
        LExceptionMessage:=RSErrorInvalidValue;

    UnknownSchedule:
      LExceptionMessage := RSErrorSettingUnknownScheduleParameters;
  end;

  raise Exception.Create(LExceptionMessage) at
    get_caller_addr(get_frame),
    get_caller_frame(get_frame);
end;

procedure TSchedule.SetParameter(i : integer; AValue: Cardinal);
begin
  case i of
    0 : if FSchedule.Parameter1 = AValue then Exit;
    1 : if FSchedule.Parameter2 = AValue then Exit;
  end;
  case FName of
    UnknownSchedule: SetParameters(0,0);
    EXT, CRF: SetParameters(FSchedule.Parameter1,FSchedule.Parameter2);
    FR, FI, FT:
      case i of
        0 : SetParameters(AValue, FSchedule.Parameter2);
        1 : Exit;
      end;

    VR, VI, VT, DRH, DRL:
      case i of
        0 : SetParameters(AValue,FSchedule.Parameter2);
        1 : SetParameters(FSchedule.Parameter1,AValue);
      end;
  end;
end;

procedure TSchedule.SetParameters(ASchedule: string);
begin
  if ParametersAsString = GetParameters(ASchedule) then Exit;
  SetParameters(StrToIntDef(ExtractDelimited(2,ASchedule,[#32]), 0),
                StrToIntDef(ExtractDelimited(3,ASchedule,[#32]), 0));
end;

constructor TSchedule.Create(AOwner: TComponent; ASchedule: string);
begin
  inherited Create(AOwner);
  SetScheduleName(ASchedule);
end;

constructor TSchedule.Create(AOwner: TComponent; AScheduleName: TScheduleName;
  AParameter1: Cardinal; AParameter2: Cardinal);
begin
  inherited Create(AOwner);
  Load(AScheduleName, AParameter1, AParameter2);
end;

destructor TSchedule.Destroy;
begin
  if Assigned(FSchedule) then
    FreeAndNil(FSchedule);
  inherited Destroy;
end;

function TSchedule.GetLoaded: Boolean;
begin
  Result := FName <> UnknownSchedule;
end;

function TSchedule.GetParameter(i : integer): Cardinal;
begin
  case i of
    0 : Result := FSchedule.Parameter1;
    1 : Result := FSchedule.Parameter2;
  end;
end;

function TSchedule.GetParametersAsString: string;
begin
  case FName of
    EXT, CRF: Result := '';                              // no parameter required
    FR, FI, FT: Result:= IntToStr(FSchedule.Parameter1); // one parameter required
    VR, VI, VT, DRH, DRL:                                // two parameters required
      Result:= IntToStr(FSchedule.Parameter1)+#32+IntToStr(FSchedule.Parameter2);

    UnknownSchedule: Result := RSErrorGettingUnknownScheduleParameters;
       //raise Exception.Create(RSErrorGettingUnknownScheduleParameters) at
       //   get_caller_addr(get_frame),
       //   get_caller_frame(get_frame);
  end;
end;

function TSchedule.GetParameters(ASchedule: string): string;
begin
  Result := ExtractDelimited(2,ASchedule,[#32])+#32+ExtractDelimited(3,ASchedule,[#32]);
end;

function TSchedule.GetScheduleName: TScheduleName;
begin
  Result := FName;
end;

function TSchedule.GetScheduleString: string;
begin
  Result := GetScheduleNameAsString+#32+GetParametersAsString;
end;

function TSchedule.GetScheduleNameAsString: string;
var
  LName : string;
begin
  WriteStr(LName, FName);
  Result := LName;
end;

function TSchedule.GetScheduleName(ASchedule: string): string;
begin
  ASchedule := ASchedule+#32;
  Result := ExtractDelimited(1, ASchedule, [#32]);
end;

procedure TSchedule.SetOnConsequence(AValue: TNotifyEvent);
begin
  if FOnConsequence = AValue then Exit;
  FOnConsequence:=AValue;
end;

procedure TSchedule.SetOnResponse(AValue: TNotifyEvent);
begin
  if FOnResponse = AValue then Exit;
  FOnResponse:=AValue;
end;

procedure TSchedule.DoResponse;
begin
  if FName = UnknownSchedule then
     raise Exception.Create(RSErrorUnknownScheduleAction) at
        get_caller_addr(get_frame),
        get_caller_frame(get_frame)
  else
    FSchedule.DoResponse;
end;

procedure TSchedule.Load(ASchedule: string);
begin
  SetScheduleName(GetScheduleName(ASchedule));
  SetParameters(ASchedule);
end;

procedure TSchedule.Load(AScheduleName: TScheduleName; AParameter1: Cardinal;
  AParameter2: Cardinal);
begin
  LoadScheduleName(AScheduleName);
  SetParameters(AParameter1, AParameter2);
end;

function TSchedule.Start : Extended;
begin
  if FName = UnknownSchedule then begin
     raise Exception.Create(RSErrorUnknownScheduleAction) at
        get_caller_addr(get_frame),
        get_caller_frame(get_frame)
  end else begin
    Result := FSchedule.Start;
  end;
end;

function TSchedule.Stop : Extended;
begin
  if FName = UnknownSchedule then begin
     raise Exception.Create(RSErrorUnknownScheduleAction) at
        get_caller_addr(get_frame),
        get_caller_frame(get_frame)
  end else begin
    Result := FSchedule.Stop;
  end;
end;

function TSchedule.Pause : Extended;
begin
  if FName = UnknownSchedule then begin
     raise Exception.Create(RSErrorUnknownScheduleAction) at
        get_caller_addr(get_frame),
        get_caller_frame(get_frame)
  end else begin
    Result := FSchedule.Pause;
  end;
end;

procedure TSchedule.UseFleshlerHoffmanIntervals;
begin
  FSchedule.RandomIntervalType := ritFleshlerHoffman;
end;

end.


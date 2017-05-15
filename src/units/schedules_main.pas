{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit schedules_main;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils
     , schedules
     , schedules_abstract
     ;

type

  { TSchedule }

  TSchedule = class(TComponent)
  private
    FSchedule: TSchedules;
    FScheduleLoaded: Boolean;
    FKind: string;
    FOnConsequence: TNotifyEvent;
    FOnResponse: TNotifyEvent;
    procedure Consequence(Sender: TObject);
    function GetTimeEnabled: Boolean;
    procedure Response(Sender: TObject);
    procedure SetTimeEnabled(AValue: Boolean);
    procedure SetKind(Kind: String);
  public
    procedure DoResponse;
    procedure StartClock;
    function StartMethod : TThreadMethod;
    property Enabled : Boolean read GetTimeEnabled write SetTimeEnabled;
    property Kind: string read FKind write SetKind;
    property Loaded : Boolean read FScheduleLoaded;
    property OnConsequence: TNotifyEvent read FOnConsequence write FOnConsequence;
    property OnResponse: TNotifyEvent read FOnResponse write FOnResponse;
  end;

implementation

uses
  strutils, constants
  {$ifdef DEBUG}
  debug_logger
  {$endif}
  ;
procedure TSchedule.SetKind(Kind: String);
var
  LKind, LParameter1, LParameter2
  {, LParameter3, LParameter4} : String;

begin
  if FScheduleLoaded then
    begin
      FreeAndNil(FSchedule);
      FScheduleLoaded := Assigned(FSchedule);
    end;

  LKind := ExtractDelimited(1,Kind,[#32]);
  LParameter1 := ExtractDelimited(2,Kind,[#32]);
  LParameter2 := ExtractDelimited(3,Kind,[#32]); ;
  //LParameter3 := ExtractDelimited(4,Kind,[#32]);
  //LParameter4 := ExtractDelimited(5,Kind,[#32]);

  LKind:=UpperCase(LKind);
  case LKind of
    T_CRF:
      begin
        FSchedule:= TRatioSchedule.Create(Self);
        FSchedule.Parameter1 := 1;
        FSchedule.Parameter2 := 0;
      end;

    T_EXT:
      begin
        FSchedule:= TRatioSchedule.Create(Self);
        FSchedule.Parameter1 := MaxInt;
        FSchedule.Parameter2 := 0;
      end;

    T_FR,T_FI,T_FT:
      begin
        case LKind of
          T_FR: FSchedule:= TRatioSchedule.Create(Self);
          T_FI: FSchedule:= TIntervalSchedule.Create(Self);
          T_FT: FSchedule:= TTimeSchedule.Create(Self);
        end;
        FSchedule.Parameter1 := StrToIntDef(LParameter1 , 0);
        FSchedule.Parameter2 := 0;
      end;

    T_RR,T_RI,T_RT,
    T_VR,T_VI,T_VT:
      begin
        case LKind of
          T_VR, T_RR : FSchedule:= TRatioSchedule.Create(Self);
          T_VI, T_RI : FSchedule:= TIntervalSchedule.Create(Self);
          T_VT, T_RT : FSchedule:= TTimeSchedule.Create(Self);
        end;
        FSchedule.Parameter1 := StrToIntDef(LParameter1, 0);
        FSchedule.Parameter2 := StrToIntDef(LParameter2, 0);
      end;

    T_DRL:
      begin
        FSchedule:= TDRLSchedule.Create(Self);
        FSchedule.Parameter1 := StrToIntDef(LParameter1, 0);
        //FSchedule.Parameter2 := StrToIntDef(LParameter2, 0);
      end;

    T_DRH:
      begin
        FSchedule:= TDRHSchedule.Create(Self);
        FSchedule.Parameter1 := StrToIntDef(LParameter1, 0);
        FSchedule.Parameter2 := StrToIntDef(LParameter2, 0);
        //FSchedule.Parameter3 := StrToIntDef(LParameter3, 0);
        //FSchedule.Parameter4 := StrToIntDef(LParameter4, 0);
      end;
    else
      begin
        // use CRF as default if invalid
        FSchedule:= TRatioSchedule.Create(Self);
        FSchedule.Parameter1 := 1;
        FSchedule.Parameter2 := 0;
      end;
  end;

  FScheduleLoaded := Assigned(FSchedule);
  {$ifdef DEBUG}
    DebugLn(mt_Debug + 'FAbsSchLoaded:'+ BoolToStr(FAbsSchLoaded, 'True', 'False') );
  {$endif}

  if FScheduleLoaded then
    begin
      {$ifdef DEBUG}
        DebugLn(mt_Debug + 'Schedule:' + LKind + #32 +
          Parameter1  + #32 + Parameter2  + #32
          //+ Parameter3 + #32 + Parameter4
          );
      {$endif}
      FSchedule.OnConsequence := @Consequence;
      FSchedule.OnResponse := @Response;
      FSchedule.Reset;
      FKind := LKind;
    end;
end;

procedure TSchedule.Consequence(Sender: TObject);
begin
  if Assigned(OnConsequence) then
    FOnConsequence(Self);
end;

function TSchedule.GetTimeEnabled: Boolean;
begin
  Result := FSchedule.TimeEnabled;
end;

procedure TSchedule.Response(Sender: TObject);
begin
  if Assigned(OnResponse) then
    FOnResponse(Self);
end;

procedure TSchedule.SetTimeEnabled(AValue: Boolean);
begin
  if FSchedule.TimeEnabled = AValue then Exit;
  FSchedule.TimeEnabled := AValue;
end;

procedure TSchedule.DoResponse;
begin
  if FScheduleLoaded then
    FSchedule.DoResponse;
end;

procedure TSchedule.StartClock;
var
  LStart : TThreadMethod;
begin
  LStart := StartMethod;
  TThreadMethod(LStart);
end;

function TSchedule.StartMethod : TThreadMethod;
begin
  Result := FSchedule.StartMethod;
end;

end.


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
     ;

type

  { TSchMan }

  TSchMan = class(TComponent)
  private
    FAbsSch: TAbsSch;
    FAbsSchLoaded: Boolean;
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
    function StartMethod : TThreadMethod;
    property Enabled : Boolean read GetTimeEnabled write SetTimeEnabled;
    property Kind: string read FKind write SetKind;
    property Loaded : Boolean read FAbsSchLoaded;
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
procedure TSchMan.SetKind(Kind: String);
var
  LKind, LParameter1, LParameter2
  {, LParameter3, LParameter4} : String;

begin
  if FAbsSchLoaded then
    begin
      FreeAndNil(FAbsSch);
      FAbsSchLoaded := Assigned(FAbsSch);
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
        FAbsSch:= TSchRR.Create(Self);
        FAbsSch.Parameter1 := 1;
        FAbsSch.Parameter2 := 0;
      end;

    T_EXT:
      begin
        FAbsSch:= TSchRR.Create(Self);
        FAbsSch.Parameter1 := MaxInt;
        FAbsSch.Parameter2 := 0;
      end;

    T_FR,T_FI,T_FT:
      begin
        case LKind of
          T_FR: FAbsSch:= TSchRR.Create(Self);
          T_FI: FAbsSch:= TSchRI.Create(Self);
          T_FT: FAbsSch:= TSchRT.Create(Self);
        end;
        FAbsSch.Parameter1 := StrToIntDef(LParameter1 , 0);
        FAbsSch.Parameter2 := 0;
      end;

    T_RR,T_RI,T_RT,
    T_VR,T_VI,T_VT:
      begin
        case LKind of
          T_VR, T_RR : FAbsSch:= TSchRR.Create(Self);
          T_VI, T_RI : FAbsSch:= TSchRI.Create(Self);
          T_VT, T_RT : FAbsSch:= TSchRT.Create(Self);
        end;
        FAbsSch.Parameter1 := StrToIntDef(LParameter1, 0);
        FAbsSch.Parameter2 := StrToIntDef(LParameter2, 0);
      end;

    T_DRL:
      begin
        FAbsSch:= TSchDRL.Create(Self);
        FAbsSch.Parameter1 := StrToIntDef(LParameter1, 0);
        //FAbsSch.Parameter2 := StrToIntDef(LParameter2, 0);
      end;

    T_DRH:
      begin
        FAbsSch:= TSchDRH.Create(Self);
        FAbsSch.Parameter1 := StrToIntDef(LParameter1, 0);
        FAbsSch.Parameter2 := StrToIntDef(LParameter2, 0);
        //FAbsSch.Parameter3 := StrToIntDef(LParameter3, 0);
        //FAbsSch.Parameter4 := StrToIntDef(LParameter4, 0);
      end;
    else
      raise Exception.Create('Esquema desconhecido.');
  end;

  FAbsSchLoaded := Assigned(FAbsSch);
  {$ifdef DEBUG}
    DebugLn(mt_Debug + 'FAbsSchLoaded:'+ BoolToStr(FAbsSchLoaded, 'True', 'False') );
  {$endif}

  if FAbsSchLoaded then
    begin
      {$ifdef DEBUG}
        DebugLn(mt_Debug + 'Schedule:' + aSchedule + #32 +
          Parameter1  + #32 + Parameter2  + #32
          //+ Parameter3 + #32 + Parameter4
          );
      {$endif}
      FAbsSch.OnConsequence := @Consequence;
      FAbsSch.OnResponse := @Response;
      FAbsSch.AssignParameters;
      FAbsSch.Reset;
      FKind := LKind;
    end;
end;

procedure TSchMan.Consequence(Sender: TObject);
begin
  if Assigned(OnConsequence) then
    FOnConsequence(Self);
end;

function TSchMan.GetTimeEnabled: Boolean;
begin
  Result := FAbsSch.TimeEnabled;
end;

procedure TSchMan.Response(Sender: TObject);
begin
  if Assigned(OnResponse) then
    FOnResponse(Self);
end;

procedure TSchMan.SetTimeEnabled(AValue: Boolean);
begin
  if FAbsSch.TimeEnabled = AValue then Exit;
  FAbsSch.TimeEnabled := AValue;
end;

procedure TSchMan.DoResponse;
begin
  if FAbsSchLoaded then
    FAbsSch.DoResponse;
end;

function TSchMan.StartMethod : TThreadMethod;
begin
  Result := FAbsSch.StartMethod;
end;

end.


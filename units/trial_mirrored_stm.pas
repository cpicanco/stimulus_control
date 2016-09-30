{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit trial_mirrored_stm;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils

    , config_session
    , trial_abstract
    , constants
    , draw_methods
    , schedules_main
    , response_key
    ;

type

  { TMRD }
  TDataSupport = record
    Responses : integer;
    Latency,
    StmBegin,
    StmEnd : Extended;
  end;

  TMRD = Class(TTrial)
  private
    FDataSupport : TDataSupport;
    FCircles: TCurrentTrial;
    FSchedule : TSchMan;
    FKey1 : TKey;
    FKey2 : TKey;
    FFirstResp : Boolean;
    FUseMedia : Boolean;
    procedure Consequence(Sender: TObject);
    procedure Response(Sender: TObject);
    procedure TrialBeforeEnd(Sender: TObject);
    procedure TrialStart(Sender: TObject);
    procedure TrialResult(Sender : TObject);
    procedure TrialPaint;
    procedure TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure Paint; override;
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Play(ACorrection : Boolean); override;
    //procedure DispenserPlusCall; override;

  end;

implementation

uses
  timestamps
{$ifdef DEBUG}
  , debug_logger
  , dialogs
{$endif}
  ;

constructor TMRD.Create(AOwner: TComponent);
begin
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialKeyUp := @TrialKeyUp;
  OnTrialPaint := @TrialPaint;
  OnTrialStart := @TrialStart;

  inherited Create(AOwner);

  Header :=  Header + #9 +
             'StmBegin' + #9 +
             '_Latency' + #9 +
             '__StmEnd' + #9 +
             '___Angle' + #9 +
             '______X1' + #9 +
             '______Y1' + #9 +
             '______X2' + #9 +
             '______Y2' + #9 +
             'ExpcResp' + #9 +
             'RespFreq';

  HeaderTimestamps := HeaderTimestamps + #9 + 'Event';
  FDataSupport.Responses:= 0;
end;

procedure TMRD.Consequence(Sender: TObject);
begin
  LogEvent('C');
  if Assigned(CounterManager.OnConsequence) then CounterManager.OnConsequence(Self);

  TrialResult(Sender);
end;

procedure TMRD.TrialResult(Sender: TObject);
begin
  Result := 'HIT';
  IETConsequence := T_NONE;

  if FCircles.NextTrial = T_CRT then NextTrial := T_CRT
  else NextTrial := FCircles.NextTrial;
end;

procedure TMRD.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FResponseEnabled then
    begin
      LogEvent('R');
    end;
end;

procedure TMRD.TrialPaint;
var i : integer;
begin
  if not FUseMedia then
    for i := 0 to 1 do
      with FCircles.C[i] do
        DrawCircle(Canvas, o.X, o.Y, size, gap, gap_degree, gap_length);
end;

procedure TMRD.Play(ACorrection: Boolean);
var
  s1, sName, sLoop, sColor{, sGap, sGapDegree, sGapLength} : string;
  R : TRect;
  a1 : Integer;

  procedure KeyConfig(var aKey : TKey);
  begin
    with aKey do
      begin
        BoundsRect := R;
        Color := StrToIntDef(sColor, $0000FF); // clRed
        Loops := StrToIntDef(sLoop, 0);
        FullPath := sName;
        Schedule.Kind := CfgTrial.SList.Values[_Schedule];
        // Visible := False;
      end;
  end;

begin
  inherited Play(ACorrection);
  FUseMedia := StrToBoolDef(CfgTrial.SList.Values[_UseMedia], False);

  if not FUseMedia then
    SetLength(FCircles.C, 2);

  FSchedule := TSchMan.Create(self);
  with FSchedule do
    begin
      OnConsequence := @Consequence;
      OnResponse:= @Response;
      Kind := CfgTrial.SList.Values[_Schedule];
    end;
  AddToClockList(FSchedule);
  FCircles.angle := StrToFloatDef(CfgTrial.SList.Values[_Angle], 0.0);
  FCircles.response := CfgTrial.SList.Values[_Comp + '1' + _cGap];

  for a1 := 0 to 1 do
    begin
        s1:= CfgTrial.SList.Values[_Comp + IntToStr(a1+1) + _cBnd];

        R.Left:= StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
        NextSpaceDelimitedParameter(s1);

        R.Top:= StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
        NextSpaceDelimitedParameter(s1);

        R.Right := StrToIntDef(s1, 100);
        R.Bottom := R.Right;

       if FUseMedia then
         begin
           s1:= CfgTrial.SList.Values[_Comp + IntToStr(a1+1)+_cStm] + #32;

           sName := RootMedia + Copy(s1, 0, pos(#32, s1)-1);
           NextSpaceDelimitedParameter(s1);

           sLoop := Copy(s1, 0, pos(#32, s1)-1);
           NextSpaceDelimitedParameter(s1);

           sColor := s1;

           if a1 = 0 then KeyConfig(FKey1) else KeyConfig(FKey2)

         end
       else
          with FCircles.C[a1] do
            begin
              o := Point(R.Left, R.Top);
              size := R.Right;
              gap := StrToBoolDef( CfgTrial.SList.Values[_Comp + IntToStr(a1+1) + _cGap], False );
              if gap then
                  begin
                    gap_degree := StrToIntDef( CfgTrial.SList.Values[_Comp + IntToStr(a1+1) + _cGap_Degree], Round(Random(360)));
                    gap_length := StrToIntDef( CfgTrial.SList.Values[_Comp + IntToStr(a1+1) + _cGap_Length], 5 );
                  end;
            end;
      end;

  if Self.ClassType = TMRD then Config(Self);
end;

procedure TMRD.TrialStart(Sender: TObject);
begin
  {
  if FUseMedia then
    begin
      KeyStart(FKey1);
      KeyStart(FKey2);
    end;
  }
  FFirstResp := True;
  FDataSupport.StmBegin := TickCount;
end;

procedure TMRD.WriteData(Sender: TObject);  //
begin
  Data := Data +
          TimestampToStr(FDataSupport.StmBegin - TimeStart) + #9 +
          TimestampToStr(FDataSupport.Latency - TimeStart) + #9 +
          TimestampToStr(FDataSupport.StmEnd - TimeStart) + #9 +
          #32#32#32#32#32 + FormatFloat('000;;00', FCircles.angle) + #9 +
          Format('%-*.*d', [4,8,FCircles.C[0].o.X]) + #9 +
          Format('%-*.*d', [4,8,FCircles.C[0].o.Y]) + #9 +
          Format('%-*.*d', [4,8,FCircles.C[1].o.X]) + #9 +
          Format('%-*.*d', [4,8,FCircles.C[1].o.Y]) + #9 +
          #32#32#32#32#32#32#32 + FCircles.response + #9 +
          Format('%-*.*d', [4,8,FDataSupport.Responses]);
  if Assigned(OnTrialWriteData) then OnTrialWriteData(Self);
end;

procedure TMRD.TrialBeforeEnd(Sender: TObject);
begin
  FDataSupport.StmEnd := TickCount;
  FCircles.Result := Result;
  WriteData(Sender);
end;

procedure TMRD.Paint;
begin
  inherited Paint;
end;


procedure TMRD.Response(Sender: TObject);
var LTickCount : Extended;
begin
  LTickCount := TickCount;
  Inc(FDataSupport.Responses);

  if FFirstResp then
    begin
      FFirstResp := False;
      FDataSupport.Latency := LTickCount;
    end;

  //Invalidate;
  if Assigned(CounterManager.OnStmResponse) then CounterManager.OnStmResponse (Sender);
  if Assigned(OnStmResponse) then OnStmResponse (Self);
end;

end.

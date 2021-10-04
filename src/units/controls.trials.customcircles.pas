{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.CustomCircles;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils

  , Controls.Trials.Abstract
  , Controls.Trials.Helpers
  , Schedules
  ;

type

  TCurrentTrial = record
    C : array of TCircle; //circles
    i : integer; //trial index
    NextTrial : string;
    angle : Extended; // angle from "userconfigs_trial_mirrored" form,
    response : string;
    Contingency : string;
    Result : string;
  end;

  { TMRD }

  TMRD = Class(TTrial)
  private
    FDataSupport : TDataSupport;
    FCircles: TCurrentTrial;
    FSchedule : TSchedule;
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
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner : TCustomControl); override;
    procedure Play(ACorrection : Boolean); override;
  end;

implementation

uses strutils, constants, Timestamps, Canvas.Helpers
  , Session.Configuration.GlobalContainer
{$ifdef DEBUG}
  , Loggers.Debug
  , Dialogs
{$endif}
  ;

constructor TMRD.Create(AOwner: TCustomControl);
begin
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialKeyUp := @TrialKeyUp;
  OnTrialPaint := @TrialPaint;
  OnTrialStart := @TrialStart;

  inherited Create(AOwner);

  Header :=  Header + #9 +
             rsReportStmBeg + #9 +
             rsReportRspLat + #9 +
             rsReportStmEnd + #9 +
             '___Angle' + #9 +
             '______X1' + #9 +
             '______Y1' + #9 +
             '______X2' + #9 +
             '______Y2' + #9 +
             rsReportRspExp + #9 +
             rsReportRspFrq;

  FDataSupport.Responses:= 0;
end;

procedure TMRD.Consequence(Sender: TObject);
begin
  LogEvent('C');
  if Assigned(Counters.OnConsequence) then Counters.OnConsequence(Self);

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
  for i := 0 to 1 do
    with FCircles.C[i] do
      DrawCustomEllipse(Canvas,OuterRect,InnerRect, gap, gap_degree, gap_length);
end;

procedure TMRD.Play(ACorrection: Boolean);
var
  s1 : string;
  LOuterR : TRect;
  a1, LWidth, LHeight : Integer;
begin
  inherited Play(ACorrection);
  //FUseMedia := StrToBoolDef(CfgTrial.SList.Values[_UseMedia], False);
  FUseMedia := False;

  if not FUseMedia then
    SetLength(FCircles.C, 2);

  FSchedule := TSchedule.Create(self);
  with FSchedule do
    begin
      OnConsequence := @Consequence;
      OnResponse:= @Response;
      Load(Configurations.Parameters.Values[_Schedule]);
    end;
  //AddToClockList(FSchedule);
  FCircles.angle := StrToFloatDef(Configurations.Parameters.Values[_Angle], 0.0);
  FCircles.response := Configurations.Parameters.Values[_Comp + '1' + _cGap];

  for a1 := 0 to 1 do
    begin
      s1:= Configurations.Parameters.Values[_Comp + IntToStr(a1+1) + _cBnd];

      LOuterR.Left:= StrToIntDef(ExtractDelimited(1,s1,[#32]), 0);
      LOuterR.Top:= StrToIntDef(ExtractDelimited(2,s1,[#32]), 0);
      LWidth := StrToIntDef(ExtractDelimited(3,s1,[#32]), 100);
      LOuterR.Right := LOuterR.Left + LWidth;
      LHeight := StrToIntDef(ExtractDelimited(4,s1,[#32]), 100);
      LOuterR.Bottom := LOuterR.Top + LHeight;

      with FCircles.C[a1] do
        begin
          OuterRect := LOuterR;
          InnerRect := GetInnerRect(LOuterR, LWidth, LHeight);
          gap := StrToBoolDef(Configurations.Parameters.Values[_Comp + IntToStr(a1+1) + _cGap], False );
          gap_degree := 16 * StrToIntDef(Configurations.Parameters.Values[_Comp + IntToStr(a1+1) + _cGap_Degree], Round(Random(360)));
          gap_length := 16 * StrToIntDef(Configurations.Parameters.Values[_Comp + IntToStr(a1+1) + _cGap_Length], 5 );
        end;
  end;

  if Self.ClassType = TMRD then Config(Self);
end;

procedure TMRD.TrialStart(Sender: TObject);
begin
  FFirstResp := True;
  FDataSupport.StmBegin := TickCount;
end;

procedure TMRD.WriteData(Sender: TObject);  //
begin
  inherited WriteData(Sender);
  Data := Data +
          TimestampToStr(FDataSupport.StmBegin - TimeStart) + #9 +
          TimestampToStr(FDataSupport.Latency - TimeStart) + #9 +
          TimestampToStr(FDataSupport.StmEnd - TimeStart) + #9 +
          #32#32#32#32#32 + FormatFloat('000;;00', FCircles.angle) + #9 +
          Format('%-*.*d', [4,8,FCircles.C[0].OuterRect.Left]) + #9 +
          Format('%-*.*d', [4,8,FCircles.C[0].OuterRect.Top]) + #9 +
          Format('%-*.*d', [4,8,FCircles.C[1].OuterRect.Left]) + #9 +
          Format('%-*.*d', [4,8,FCircles.C[1].OuterRect.Top]) + #9 +
          #32#32#32#32#32#32#32 + FCircles.response + #9 +
          Format('%-*.*d', [4,8,FDataSupport.Responses]);
  //if Assigned(OnTrialWriteData) then OnTrialWriteData(Self);
end;

procedure TMRD.TrialBeforeEnd(Sender: TObject);
begin
  FDataSupport.StmEnd := TickCount;
  FCircles.Result := Result;
  WriteData(Sender);
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

  if Assigned(Counters.OnStmResponse) then Counters.OnStmResponse (Sender);
  if Assigned(OnStmResponse) then OnStmResponse (Self);
end;

end.

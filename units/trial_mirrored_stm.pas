//
// Validation Project (PCRF) - Stimulus Control App
// Copyright (C) 2014-2015,  Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.
//
// cpicanco@ufpa.br
//
// This file is part of Validation Project (PCRF).
//
// Validation Project (PCRF) is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Validation Project (PCRF) is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Validation Project (PCRF).  If not, see <http://www.gnu.org/licenses/>.
//
unit trial_mirrored_stm;

{$mode objfpc}{$H+}

interface

uses

  LCLIntf, LCLType, Controls, Classes, SysUtils

  //, counter
  //, countermanager
  //, client

  {$ifdef DEBUG}
  , debug_logger
  , dialogs
  {$endif}
  , session_config
  , trial_abstract
  , constants
  , draw_methods
  , schedules_abstract
  , response_key
  ;

type

  { TMRD }
  TDataSupport = record
    Latency : cardinal;
    Responses : integer;
    StmBegin : cardinal;
    StmEnd : cardinal;
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
    FShowStarter : Boolean;
    FCanResponse : Boolean;
    //FList : TStringList;
  protected
    procedure BeginCorrection (Sender : TObject);
    procedure EndCorrection (Sender : TObject);
    procedure Consequence(Sender: TObject);
    procedure Response(Sender: TObject);
    procedure Hit(Sender: TObject);
    procedure Miss(Sender: TObject);
    procedure None(Sender: TObject);
    procedure ThreadClock(Sender: TObject); override;
    procedure StartTrial(Sender: TObject); override;
    procedure BeginStarter;
    procedure EndTrial(Sender: TObject);
    procedure WriteData(Sender: TObject); override;
    procedure SetTimerCsq;

    //TCustomControl
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Play(TestMode: Boolean; Correction : Boolean); override;

  end;

implementation


constructor TMRD.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Header := '___Angle' + #9 +
             '______X1' + #9 +
             '______Y1' + #9 +
             '______X2' + #9 +
             '______Y2' + #9 +
             'ExpcResp' + #9 +
             'RespFreq' + #9 +
             'StmBegin' + #9 +
             '_Latency' + #9 +
             '__StmEnd'
             //'__DBegin' + #9 +
             //'DLatency' + #9 +
             //'ITIBEGIN' + #9 +
             //'_ITI_END' + #9 +
             //'' + #9 +

             ;
  FDataSupport.Responses:= 0;
end;


procedure TMRD.BeginCorrection(Sender: TObject);
begin
  if Assigned (OnBeginCorrection) then OnBeginCorrection (Sender);
end;

procedure TMRD.EndCorrection(Sender: TObject);
begin
  if Assigned (OnEndCorrection) then OnEndCorrection (Sender);
end;

procedure TMRD.Consequence(Sender: TObject);
begin
  if FCanResponse then
    begin
      CreateClientThread('C:' + FormatFloat('00000000;;00000000', GetTickCount - TimeStart));
      FCanResponse:= False;
      //FDataSupport.StmDuration := GetTickCount;

      Result := 'HIT';
      //Result := 'MISS';
      //Result := 'NONE';

      FCircles.Result := Result;
      IETConsequence := 'NONE';

      if FCircles.NextTrial = T_CRT then NextTrial := T_CRT
      else NextTrial := FCircles.NextTrial;

      //Dispenser(FDataCsq, FDataUsb);
      if Result = 'HIT' then  Hit(Sender);
      //if Result = 'MISS' then  Miss(Sender);
      //if Result = 'NONE' then  None(Sender);

    EndTrial(Sender);
    end;
end;

procedure TMRD.ThreadClock(Sender: TObject);
begin
  if FUseMedia then
    begin
      //FKey1.SchMan.Clock;
      //FKey2.SchMan.Clock
    end
  else FSchedule.Clock;
end;

procedure TMRD.KeyDown(var Key: Word; Shift: TShiftState);

    function GetShiftEnum : TShiftStateEnum;
     begin
       if ssCtrl in Shift then Result := ssCtrl;
       if ssAlt in Shift then Result := ssAlt;
     end;

begin
  inherited KeyDown (Key, Shift);

  if Key = 27 {ESC} then
    begin
      FCanResponse:= False;
      Invalidate;
    end;

   // This should be the last one.
  case GetShiftEnum of
   ssCtrl :
     begin
       if Key = 81 {q} then
         begin
           Data := Data + '<>################## Sessão cancelada ##################<>' + #13#10;

           Result := 'NONE';
           IETConsequence := 'NONE';
           NextTrial := 'END';
           None(Self);
           EndTrial(Self);
         end;
     end;

   ssAlt :
     begin
       if Key = 244 {f4} then
         begin
           Data := Data + '<>################## Sessão cancelada ##################<>' + #13#10;

           Result := 'NONE';
           IETConsequence := 'NONE';
           NextTrial := 'END';
           None(Self);
           EndTrial(Self);
         end;
     end;
end;
end;

procedure TMRD.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if Key = 27 {ESC} then
    begin
      FCanResponse:= True;
      Invalidate;
    end;

  if FCanResponse then
    begin
      if FShowStarter then
        begin
          if key = 32 then
            begin
              //FSchedule.DoResponse; need to fix that, this line does not apply when no TResponseKey is used
              FDataSupport.Latency := GetTickCount;
              CreateClientThread('*R:' + FormatFloat('00000000;;00000000', GetTickCount - TimeStart));
              FShowStarter := False;
              Invalidate;
              StartTrial(Self);
            end;
        end
      else CreateClientThread('R:' + FormatFloat('00000000;;00000000', GetTickCount - TimeStart));
    end;
end;

procedure TMRD.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  // do nothing

end;

procedure TMRD.Paint;
//var i : integer;
begin
  inherited Paint;

  if FCanResponse then
    begin
      if FShowStarter then
        begin
          DrawCenteredCircle (Canvas, Width, Height, 6);
        end
      else
        if FUseMedia then
          begin
        //do nothing, TKey draws itself
          end
        else
          begin
            with FCircles.C[0] do DrawCircle(Canvas, o.X, o.Y, size, gap, gap_degree, gap_length);
            with FCircles.C[1] do DrawCircle(Canvas, o.X, o.Y, size, gap, gap_degree, gap_length);
          end;
    end;
end;

procedure TMRD.Play(TestMode: Boolean; Correction : Boolean);
var
  s1, sName, sLoop, sColor{, sGap, sGapDegree, sGapLength} : string;
  R : TRect;
  a1 : Integer;

  procedure NextSpaceDelimitedParameter;
  begin
    Delete(s1, 1, pos(#32, s1));
    if Length(s1) > 0 then while s1[1] = #32 do Delete(s1, 1, 1);
  end;

  procedure KeyConfig(var aKey : TKey);
  begin
    with aKey do
      begin
        BoundsRect := R;
        Color := StrToIntDef(sColor, $0000FF {clRed} );
        HowManyLoops:= StrToIntDef(sLoop, 0);
        FullPath:= sName;
        SchMan.Kind:= CfgTrial.SList.Values[_Schedule];
        //Visible := False;
      end;
  end;

begin
  FCanResponse:= False;
  NextTrial := '-1';
  Randomize;


  FUseMedia := StrToBoolDef(CfgTrial.SList.Values[_UseMedia], False);

  FShowStarter := StrToBoolDef(CfgTrial.SList.Values[_ShowStarter], False);
  //FStarterSchedule :=  necessary?

  if FUseMedia then
    RootMedia := CfgTrial.SList.Values[_RootMedia]
  else
    begin
      SetLength(FCircles.C, 2);
    end;

  if Correction then FIsCorrection := True
  else FIsCorrection := False;

  //self descends TCustomControl
  Color:= StrToIntDef(CfgTrial.SList.Values[_BkGnd], 0);
  if TestMode then Cursor:= 0
  else Cursor:= StrToIntDef(CfgTrial.SList.Values[_Cursor], 0);

  FSchedule := TSchMan.Create(self);
  with FSchedule do
    begin
      OnConsequence:= @Consequence;
      OnResponse:= @Response;
    end;

  FCircles.angle := StrToFloatDef(CfgTrial.SList.Values[_Angle], 0.0);
  FCircles.response := CfgTrial.SList.Values[_Comp + '1' + _cGap];
  FCircles.NextTrial := CfgTrial.SList.Values[_NextTrial];
  FSchedule.Kind:= CfgTrial.SList.Values[_Schedule];

  for a1 := 0 to 1 do
    begin
        s1:= CfgTrial.SList.Values[_Comp + IntToStr(a1+1) + _cBnd];

        R.Left:= StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
        NextSpaceDelimitedParameter;

        R.Top:= StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
        NextSpaceDelimitedParameter;

        R.Right := StrToIntDef(s1, 100);
        R.Bottom := R.Right;

       if FUseMedia then
         begin
           s1:= CfgTrial.SList.Values[_Comp + IntToStr(a1+1)+_cStm] + #32;

           sName := RootMedia + Copy(s1, 0, pos(#32, s1)-1);
           NextSpaceDelimitedParameter;

           sLoop := Copy(s1, 0, pos(#32, s1)-1);
           NextSpaceDelimitedParameter;

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
  if FShowStarter then BeginStarter else StartTrial(Self);

  {$ifdef DEBUG}
  DebugLn(mt_Information + 'Starter:' + BoolToStr(FShowStarter));
  {$endif}
end;

procedure TMRD.StartTrial(Sender: TObject);

  procedure KeyStart(var aKey : TKey);
  begin
    aKey.Play;
    aKey.Visible:= True;
  end;

begin
  inherited StartTrial(Sender);

  if FIsCorrection then
    begin
      BeginCorrection (Self);
    end;

  if FUseMedia then
    begin
      KeyStart(FKey1);
      KeyStart(FKey2);
    end;

  FFirstResp := True;
  FCanResponse:= True;
  FDataSupport.StmBegin := GetTickCount;
end;

procedure TMRD.BeginStarter;
begin
  CreateClientThread('S:' + FormatFloat('00000000;;00000000', GetTickCount - TimeStart));
  FCanResponse:= True;
  Invalidate;
end;

procedure TMRD.WriteData(Sender: TObject);  //
begin

  Data := //Format('%-*.*d', [4,8,CfgTrial.Id + 1]) + #9 +
           #32#32#32#32#32 + FormatFloat('000;;00', FCircles.angle) + #9 +
           Format('%-*.*d', [4,8,FCircles.C[0].o.X]) + #9 +
           Format('%-*.*d', [4,8,FCircles.C[0].o.Y]) + #9 +
           Format('%-*.*d', [4,8,FCircles.C[1].o.X]) + #9 +
           Format('%-*.*d', [4,8,FCircles.C[1].o.Y]) + #9 +
           #32#32#32#32#32#32#32 + FCircles.response + #9 +
           Format('%-*.*d', [4,8,FDataSupport.Responses]) + #9 +
           FormatFloat('00000000;;00000000',FDataSupport.StmBegin - TimeStart) + #9 +
           FormatFloat('00000000;;00000000',FDataSupport.Latency - TimeStart) + #9 +
           FormatFloat('00000000;;00000000',FDataSupport.StmEnd - TimeStart)
           //FormatFloat('00000000;;00000000',FData.LatencyStmResponse) + #9 +
           //FormatFloat('00000000;;00000000',FData.ITIBEGIN) + #9 +
           //FormatFloat('00000000;;00000000',FData.ITIEND) + #9 +
           //'' + #9 +
           ;
end;

procedure TMRD.SetTimerCsq;
begin

end;

procedure TMRD.EndTrial(Sender: TObject);
begin
  FDataSupport.StmEnd := GetTickCount;
  WriteData(Sender);
  if Assigned(CounterManager.OnConsequence) then CounterManager.OnConsequence(Self);
  if Assigned(OnWriteTrialData) then OnWriteTrialData (Self);
  if Assigned(OnEndTrial) then OnEndTrial(Sender);
end;

procedure TMRD.Hit(Sender: TObject);
begin
  if Assigned(OnHit) then OnHit(Sender);
end;

procedure TMRD.Miss(Sender: TObject);
begin
  if Assigned(OnMiss) then OnMiss (Sender);
end;


procedure TMRD.None(Sender: TObject);
begin
  if Assigned(OnNone) then OnNone (Sender);
end;

procedure TMRD.Response(Sender: TObject);
begin
  Inc(FDataSupport.Responses);
  if FFirstResp then
    begin
      FDataSupport.Latency := GetTickCount;
      FFirstResp := False;
    end
  else;

  if FUseMedia then
    if Sender is TKey then TKey(Sender).IncCounterResponse
      else;

  Invalidate;
  if Assigned(CounterManager.OnStmResponse) then CounterManager.OnStmResponse (Sender);
  if Assigned(OnStmResponse) then OnStmResponse (Self);
end;

end.

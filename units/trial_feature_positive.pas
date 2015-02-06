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
unit trial_feature_positive;

{$mode objfpc}{$H+}

//{$MODE Delphi}

interface

uses LCLIntf, LCLType, LMessages, Controls, Classes, SysUtils,
     counter,
     dialogs,
     session_config,
     countermanager,
     trial_abstract, custom_timer, constants, client,
     draw_methods, schedules_abstract, response_key;

type

  { TMRD }
  TDataSupport = record
    StarterLatency : cardinal;
    Latency : cardinal;
    Responses : integer;
    StmBegin : cardinal;
    StmEnd : cardinal;
  end;

  TFPE = Class(TTrial)
  private
    FIETCode : string;
    FNumComp : integer;
    FDataSupport : TDataSupport;
    FCurrTrial: TCurrentTrial;
    FSchedule : TSchMan;
    FFirstResp : Boolean;
    FFlagCsq2Fired : Boolean;
    FUseMedia : Boolean;
    FShowStarter : Boolean;
    FCanResponse : Boolean;
    FClockThread : TClockThread;
    FClientThread : TClientThread;
    FList : TStringList;
    procedure CreateClientThread(Code : string);
    procedure DebugStatus(msg : string);
  protected
    procedure BeginCorrection (Sender : TObject);
    procedure EndCorrection (Sender : TObject);
    procedure Consequence(Sender: TObject);
    procedure Consequence2(Sender: TObject);
    procedure Response(Sender: TObject);
    procedure Hit(Sender: TObject);
    procedure Miss(Sender: TObject);
    procedure None(Sender: TObject);
    procedure TimerClockTimer(Sender: TObject);
    procedure StartTrial;
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
    destructor Destroy;override;

    procedure Play(Manager : TCounterManager; TestMode: Boolean; Correction : Boolean); override;

  end;

implementation


constructor TFPE.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeader := 'ExpcResp' + #9 +
             '__Result' + #9 +
             'RespFreq' + #9 +
             'StartLat' + #9 +
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

destructor TFPE.Destroy;
begin
  //do something
  FClockThread.FinishThreadExecution;
  inherited Destroy;
end;

procedure TFPE.BeginCorrection(Sender: TObject);
begin
  if Assigned (OnBeginCorrection) then FOnBeginCorrection (Sender);
end;

procedure TFPE.EndCorrection(Sender: TObject);
begin
  if Assigned (OnEndCorrection) then FOnEndCorrection (Sender);
end;

procedure TFPE.Consequence(Sender: TObject);
begin
  if FCanResponse then
    begin
      CreateClientThread('Consequence');
      FCanResponse:= False;
      //FDataSupport.StmDuration := GetTickCount;

      if FCurrTrial.response = 'Positiva' then
        begin
          if FFlagCsq2Fired then
            begin
              FResult := T_HIT;
              //FIETConsequence := V2.avi 1 255 18000
              FIETConsequence := 'CSQ+';
              FIETConsequence := FIETCode;
            end
          else
            begin
              FResult := T_MISS;
              // within trial user defined differential consequences is not implemented yet
              FIETConsequence := 'CSQ-';
            end;

        end
      else if FCurrTrial.response = 'Negativa' then
        begin
          if FFlagCsq2Fired then
            begin
              FResult := T_MISS;
              // within trial user defined differential consequences is not implemented yet
              FIETConsequence := 'CSQ-';
            end
          else
            begin
              FResult := T_HIT;
              //FIETConsequence := V2.avi 1 255 18000
              FIETConsequence := 'CSQ+';
              FIETConsequence := FIETCode;
            end;
        end;

      FCurrTrial.Result := FResult;

      if FCurrTrial.NextTrial = T_CRT then FNextTrial := T_CRT
      else FNextTrial := FCurrTrial.NextTrial;

      //Dispenser(FDataCsq, FDataUsb);
      if FResult = T_HIT then  Hit(Sender)
      else
      if FResult = T_MISS then  Miss(Sender)
      else
      if FResult = T_NONE then  None(Sender);

    EndTrial(Sender);
    end;
end;

procedure TFPE.Consequence2(Sender: TObject);
begin
    if FFlagCsq2Fired = False then FFlagCsq2Fired := True;

end;

procedure TFPE.TimerClockTimer(Sender: TObject);
begin
  FSchedule.Clock;
end;

procedure TFPE.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown (Key, Shift);
  if Key = 27 {ESC} then FCanResponse:= False;
  Invalidate;
end;

procedure TFPE.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if Key = 27 {ESC} then FCanResponse:= True;
  Invalidate;
end;

procedure TFPE.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if FCanResponse then
    begin
      if FShowStarter then
        begin
          if (key in [#32]) then
            begin
              //if FUseMedia then ... not implemented yet

              FDataSupport.StarterLatency := GetTickCount;
              CreateClientThread('*R:' + IntToStr(TTimeStamp(DateTimeToTimeStamp(Now)).Date) + #32 +
                                  IntToStr(TTimeStamp(DateTimeToTimeStamp(Now)).Time));
              FShowStarter := False;
              Invalidate;
              StartTrial;
            end;
        end
      else
        begin
          //if FUseMedia then ... not implemented yet
          CreateClientThread('R:' + IntToStr(TTimeStamp(DateTimeToTimeStamp(Now)).Date) + #32 +
                                  IntToStr(TTimeStamp(DateTimeToTimeStamp(Now)).Time));
          FSchedule.DoResponse;
        end;
    end;

end;

procedure TFPE.Paint;
var i : integer;
begin
  if FCanResponse then
    begin
      if FShowStarter then
        begin
          DrawCenteredCircle (Canvas, Width, Height, 6);
        end
      else
        if FUseMedia then
          begin
            // do nothing, TKey draws itself
            // use of custom TKey was not implemented yet
          end
        else
          begin
            for i := Low(FCurrTrial.C) to High(FCurrTrial.C) do
              with FCurrTrial.C[i] do DrawCircle(Canvas, o.X, o.Y, size, gap, gap_degree, gap_length);
          end;
    end;
end;

procedure TFPE.Play(Manager : TCounterManager; TestMode: Boolean; Correction : Boolean);
var
  s1, sName, sLoop, sColor, sGap, sGapDegree, sGapLength : string;
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
        SchMan.Kind:= FCfgTrial.SList.Values[_Schedule];
        //Visible := False;
      end;
  end;

begin
  FCanResponse:= False;
  FNextTrial:= '-1';
  FManager := Manager;
  Randomize;

  FUseMedia := StrToBoolDef(FCfgTrial.SList.Values[_UseMedia], False);

  FShowStarter := StrToBoolDef(FCfgTrial.SList.Values[_ShowStarter], False);
  //FStarterSchedule :=  not implemented yet
  FNumComp := StrToIntDef(FCfgTrial.SList.Values[_NumComp], 1);
  FRootMedia := FCfgTrial.SList.Values[_RootMedia];

  if FUseMedia then
    // not implemented yet
  else
    begin
      SetLength(FCurrTrial.C, FNumComp);
    end;

  if Correction then FIsCorrection := True
  else FIsCorrection := False;


  Color:= StrToIntDef(FCfgTrial.SList.Values[_BkGnd], 0);
  if TestMode then Cursor:= 0
  else Cursor:= StrToIntDef(FCfgTrial.SList.Values[_Cursor], 0);

  //self descends from TCustomControl
  FSchedule := TSchMan.Create(self);
  with FSchedule do
    begin
      OnConsequence2 := @Consequence2;
      OnConsequence:= @Consequence;
      OnResponse:= @Response;
    end;

  FCurrTrial.response := FCfgTrial.SList.Values[_ExpectedResponse];
  FSchedule.Kind:= FCfgTrial.SList.Values[_Schedule];
  FIETCode := FCfgTrial.SList.Values[_Trial + _cIET];
  FCurrTrial.Result := T_NONE;
  FCurrTrial.NextTrial := FCfgTrial.SList.Values[_NextTrial];

  for a1 := 0 to FNumComp -1 do
    begin
        s1:= FCfgTrial.SList.Values[_Comp + IntToStr(a1 + 1) + _cBnd];

        R.Left:= StrToIntDef(Copy(s1, 0, pos(#32, s1) - 1), 0);
        NextSpaceDelimitedParameter;

        R.Top:= StrToIntDef(Copy(s1, 0, pos(#32, s1) - 1), 0);
        NextSpaceDelimitedParameter;

        R.Right := StrToIntDef(s1, 100);
        R.Bottom := R.Right;

       if FUseMedia then
         begin
           //not implemented yet
           {s1:= FCfgTrial.SList.Values[_Comp + IntToStr(a1+1)+_cStm] + #32;

           sName := RootMedia + Copy(s1, 0, pos(#32, s1)-1);
           NextSpaceDelimitedParameter;

           sLoop := Copy(s1, 0, pos(#32, s1)-1);
           NextSpaceDelimitedParameter;

           sColor := s1;

           if a1 = 0 then KeyConfig(FKey1) else KeyConfig(FKey2)}

         end
       else
          with FCurrTrial.C[a1] do
            begin
              o := Point( R.Top, R.Left );
              size := R.Right;
              gap := StrToBoolDef( FCfgTrial.SList.Values[_Comp + IntToStr(a1+1) + _cGap], False );
              if gap then
                  begin
                    gap_degree := StrToIntDef( FCfgTrial.SList.Values[_Comp + IntToStr(a1 + 1) + _cGap_Degree], Random(360));
                    gap_length := StrToIntDef( FCfgTrial.SList.Values[_Comp + IntToStr(a1 + 1) + _cGap_Length], 5 );
                  end;
            end;
      end;

  if FShowStarter then BeginStarter else StartTrial;
  //showmessage(BoolToStr(FShowStarter));
end;

procedure TFPE.StartTrial;
var a1 : integer;

  procedure KeyStart(var aKey : TKey);
  begin
    aKey.Play;
    aKey.Visible:= True;
  end;

begin
  if FIsCorrection then
    begin
      BeginCorrection(Self);
    end;

  if FUseMedia then
    begin
      // not implemented yet
    end;

  FFlagCsq2Fired := False;
  FFirstResp := True;
  FCanResponse:= True;
  FDataSupport.StmBegin := GetTickCount;
  FClockThread := TClockThread.Create(False);
  FClockThread.OnTimer := @TimerClockTimer;
end;

procedure TFPE.BeginStarter;
begin
  CreateClientThread('BeginStarter');
  FCanResponse:= True;
  Invalidate;
end;

procedure TFPE.WriteData(Sender: TObject);  //
var Latency : string;
begin
  if not FFirstResp then
    Latency := FormatFloat('00000000;;00000000',FDataSupport.Latency - TimeStart)
  else Latency := #32#32#32#32#32#32#32 + 'NA';

  FData := //Format('%-*.*d', [4,8,FCfgTrial.Id + 1]) + #9 +
           #32#32#32#32#32#32#32 + FCurrTrial.response + #9 +
           #32#32#32#32#32#32#32 + FCurrTrial.Result + #9 +
           Format('%-*.*d', [4,8, FDataSupport.Responses]) + #9 +
           FormatFloat('00000000;;00000000',FDataSupport.StarterLatency - TimeStart) + #9 +
           FormatFloat('00000000;;00000000',FDataSupport.StmBegin - TimeStart) + #9 +
           Latency + #9 +
           FormatFloat('00000000;;00000000',FDataSupport.StmEnd - TimeStart)
           //FormatFloat('00000000;;00000000',FData.LatencyStmResponse) + #9 +
           //FormatFloat('00000000;;00000000',FData.ITIBEGIN) + #9 +
           //FormatFloat('00000000;;00000000',FData.ITIEND) + #9 +
           //'' + #9 +
           ;
end;

procedure TFPE.SetTimerCsq;
begin

end;

procedure TFPE.EndTrial(Sender: TObject);
begin
  FDataSupport.StmEnd := GetTickCount;
  WriteData(Sender);
  FManager.OnConsequence(Self);

  if Assigned(OnWriteTrialData) then FOnWriteTrialData(Self);
  if Assigned(OnEndTrial) then FOnEndTrial(sender);
end;

procedure TFPE.Hit(Sender: TObject);
begin
  if Assigned(OnHit) then FOnHit(Sender);
end;

procedure TFPE.Miss(Sender: TObject);
begin
  if Assigned(OnMiss) then FOnMiss (Sender);
end;


procedure TFPE.None(Sender: TObject);
begin
  if Assigned(OnNone) then FOnNone (Sender);
end;

procedure TFPE.CreateClientThread(Code: string);
begin
  FClientThread := TClientThread.Create( True, FCfgTrial.Id, Code, FileName );
  FClientThread.OnShowStatus := @DebugStatus;
  FClientThread.Start;
end;

procedure TFPE.DebugStatus(msg: string);
begin
  // do nothing
end;

procedure TFPE.Response(Sender: TObject);
begin
  Inc(FDataSupport.Responses);
  if FFirstResp then
    begin
      FDataSupport.Latency := GetTickCount;
      FFirstResp := False;
    end
  else;

  // not implemented yet
  //if FUseMedia then
  //  if Sender is TKey then TKey(Sender).IncCounterResponse
  //    else;

  Invalidate;
  if Assigned(FManager.OnStmResponse) then FManager.OnStmResponse (Sender);
  if Assigned(OnStmResponse) then FOnStmResponse (Self);
end;

end.

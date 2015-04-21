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

interface

uses LCLIntf, LCLType, {LMessages,} Controls, Classes, SysUtils

  //, counter
  , dialogs
  , session_config
  //, countermanager
  , trial_abstract
  //, custom_timer
  , constants
  //, client
  , draw_methods
  , schedules_abstract
  , response_key
  ;

type

  { TMRD }

  TDataSupport = record
    StarterLatency : cardinal;
    Latency : cardinal;
    Responses : integer;
    StmBegin : cardinal;
    StmEnd : cardinal;

    {
      CSQHIT occurs at the trial ending, after the trial is destroyed, see units/blocs.pas IETconsequence,
      so it is not always contingent to the subject's response
    }
    CSQHIT : string;

    {
      CSQMISS occurs at the trial ending, after the trial is destroyed, see units/blocs.pas IETconsequence,
      so it is not always contingent to the subject's response
    }
    CSQMISS : string;

    {
      CSQ2 occurs as soon as the subject's response meets the response schedule, i.e., always contingent.
      CSQ2 is only available for TSchRRRT instances, see units/schedules_abstract.
    }
    CSQ2 : string;

  end;

  { TFPE }

  TFPE = Class(TTrial)
  private
    //FIETCode : string;
    FNumComp : integer;
    FDataSupport : TDataSupport;
    FCurrTrial: TCurrentTrial;
    FSchedule : TSchMan;
    FFirstResp : Boolean;
    FFlagCsq2Fired : Boolean;
    FUseMedia : Boolean;
    FShowStarter : Boolean;
    FCanResponse : Boolean;
    //FClientThread : TClientThread;
    //FList : TStringList;
  protected
    procedure BeginCorrection (Sender : TObject);
    procedure EndCorrection (Sender : TObject);
    procedure Consequence(Sender: TObject);
    procedure Consequence2(Sender: TObject);
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


constructor TFPE.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Header :=  'StartLat' + #9 +
             'StmBegin' + #9 +
             '_Latency' + #9 +
             '__StmEnd' + #9 +
             'RespFreq' + #9 +
             'ExpcResp' + #9 +
             '__Result'
             //'__DBegin' + #9 +
             //'DLatency' + #9 +
             ;

  FDataSupport.Responses:= 0;
end;

procedure TFPE.BeginCorrection(Sender: TObject);
begin
  if Assigned (OnBeginCorrection) then OnBeginCorrection (Sender);
end;

procedure TFPE.EndCorrection(Sender: TObject);
begin
  if Assigned (OnEndCorrection) then OnEndCorrection (Sender);
end;

procedure TFPE.Consequence(Sender: TObject);
begin
  if FCanResponse then
    begin
      CreateClientThread('C:'+ FormatFloat('00000000;;00000000', GetTickCount - TimeStart));

      FCanResponse:= False;
      //FDataSupport.StmDuration := GetTickCount;

      if FFlagCsq2Fired then
        begin
          if FCurrTrial.response = 'Positiva' then  Result := T_HIT else Result := T_MISS;
          IETConsequence := FDataSupport.CSQHIT;
        end
      else
        begin
          if FCurrTrial.response = 'Negativa' then  Result := T_HIT else Result := T_MISS;
          // within trial user defined differential consequences is not implemented yet
          IETConsequence := FDataSupport.CSQMISS;
        end;

      FCurrTrial.Result := Result;

      if FCurrTrial.NextTrial = T_CRT then NextTrial := T_CRT
      else NextTrial := FCurrTrial.NextTrial;

      //Dispenser(FDataCsq, FDataUsb);
      if Result = T_HIT then  Hit(Sender)
      else
      if Result = T_MISS then  Miss(Sender)
      else
      if Result = T_NONE then  None(Sender);

    EndTrial(Sender);
    end;
end;

procedure TFPE.Consequence2(Sender: TObject);
var
  aConsequence : TKey;

begin
    if FFlagCsq2Fired = False then FFlagCsq2Fired := True;
    aConsequence := TKey.Create(Self);
    aConsequence.Cursor:= Self.Cursor;
    aConsequence.Parent:= Self;
    //aConsequence.OnConsequence2:=@Consequence2;
    //aConsequence.OnConsequence:= @Consequence;
    //aConsequence.OnResponse:= @Response;
    aConsequence.HowManyLoops := 0;
    aConsequence.Color := 255;
    aConsequence.FullPath := RootMedia + FDataSupport.CSQ2;
    aConsequence.Play;
    //aConsequence.FullScreen;
end;

procedure TFPE.ThreadClock(Sender: TObject);
begin
  if Visible then FSchedule.Clock;
end;

procedure TFPE.KeyDown(var Key: Word; Shift: TShiftState);
begin
  //inherited KeyDown (Key, Shift);

  if (Key = 27 {ESC}) and (FCanResponse = True) then
    begin
      FCanResponse:= False;
      Invalidate;
    end;

  // This should be the last one.
   if ssCtrl in shift then
      begin
        if Key = 81 {q} then
          begin
            Data := Data + #13#10 + '(Sessão cancelada)' + #9#9#9#9#9#9#9#9#9 + #13#10;
            Result := 'NONE';
            IETConsequence := 'NONE';
            NextTrial := 'END';
            None(Self);
            EndTrial(Self);
          end;
      end;
end;


procedure TFPE.KeyUp(var Key: Word; Shift: TShiftState);
var TickCount : cardinal;
begin
  TickCount := GetTickCount;
  inherited KeyUp(Key, Shift);
  if Key = 27 {ESC} then FCanResponse:= True;
  Invalidate;

  if FCanResponse then
  begin
    if FShowStarter then
      begin
        if Key = 32 then
          begin
            //if FUseMedia then ... not implemented yet
            FDataSupport.StarterLatency := TickCount;
            CreateClientThread('*R:' + FormatFloat('00000000;;00000000', TickCount - TimeStart));
            FShowStarter := False;
            Invalidate;
            StartTrial(Self);
          end;
      end
    else
      begin
        //if FUseMedia then ... not implemented yet
        CreateClientThread('R:' + FormatFloat('00000000;;00000000', TickCount - TimeStart));
        FSchedule.DoResponse;
      end;
  end;

end;

procedure TFPE.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  //do nothing

end;

procedure TFPE.Paint;
var i : integer;
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

procedure TFPE.Play(TestMode: Boolean; Correction : Boolean);
var
  s1{, sName, sLoop, sColor, sGap, sGapDegree, sGapLength} : string;
  R : TRect;
  a1 : Integer;


  procedure NextCommaDelimitedParameter;
  begin
    Delete(s1, 1, pos(#44, s1));
    if Length(s1) > 0 then while s1[1] = #44 do Delete(s1, 1, 1);
  end;

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
        //Color := StrToIntDef(sColor, $0000FF {clRed} );
        //HowManyLoops:= StrToIntDef(sLoop, 0);
        //FullPath:= sName;
        SchMan.Kind:= CfgTrial.SList.Values[_Schedule];
        //Visible := False;
      end;
  end;

begin
  FCanResponse:= False;
  NextTrial:= '-1';
  Randomize;

  FUseMedia := StrToBoolDef(CfgTrial.SList.Values[_UseMedia], False);

  FShowStarter := StrToBoolDef(CfgTrial.SList.Values[_ShowStarter], False);
  //FStarterSchedule :=  not implemented
  FNumComp := StrToIntDef(CfgTrial.SList.Values[_NumComp], 1);
  RootMedia := CfgTrial.SList.Values[_RootMedia];

  if FUseMedia then
    // not implemented yet
  else
    begin
      SetLength(FCurrTrial.C, FNumComp);
    end;

  if Correction then FIsCorrection := True
  else FIsCorrection := False;


  Color:= StrToIntDef(CfgTrial.SList.Values[_BkGnd], 0);
  if TestMode then Cursor:= 0
  else Cursor:= StrToIntDef(CfgTrial.SList.Values[_Cursor], 0);

  //self descends from TCustomControl
  FSchedule := TSchMan.Create(self);
  with FSchedule do
    begin
      OnConsequence2 := @Consequence2;
      OnConsequence:= @Consequence;
      OnResponse:= @Response;
    end;

  FSchedule.Kind:= CfgTrial.SList.Values[_Schedule];

  // allow user defined differential consequences
  // we expect somthing like:

  // PositiveHIT, PositiveMISS, PositiveCSQ2
  // NegativeHIT, NegativeMISS, NegativeCSQ2

  // Positive
  // NONE,MISS,HIT

  // Negative
  // NONE,HIT,MISS

  // Custom
  // S1.wav 0 -1 1000,S2.wav 0 -1 1000, S3.wav 0 -1 1000,

  s1 := CfgTrial.SList.Values[_Trial + _cIET] + #44;

  FDataSupport.CSQHIT := Copy(s1, 0, pos(#44, s1) - 1);
  NextCommaDelimitedParameter;

  FDataSupport.CSQMISS := Copy(s1, 0, pos(#44, s1) - 1);
  NextCommaDelimitedParameter;

  FDataSupport.CSQ2 := Copy(s1, 0, pos(#44, s1) - 1);

  // Alias to a default media name.ext
  FCurrTrial.response := CfgTrial.SList.Values[_ExpectedResponse];
  if FCurrTrial.response = 'Positiva' then
    if FDataSupport.CSQ2 = T_HIT then FDataSupport.CSQ2 := 'CSQ1.wav';

  if FCurrTrial.response = 'Negativa' then
    if FDataSupport.CSQ2 = T_MISS then FDataSupport.CSQ2 := 'CSQ2.wav';


  FCurrTrial.Result := T_NONE;
  FCurrTrial.NextTrial := CfgTrial.SList.Values[_NextTrial];

  for a1 := 0 to FNumComp -1 do
    begin
        s1:= CfgTrial.SList.Values[_Comp + IntToStr(a1 + 1) + _cBnd];

        R.Left:= StrToIntDef(Copy(s1, 0, pos(#32, s1) - 1), 0);
        NextSpaceDelimitedParameter;

        R.Top:= StrToIntDef(Copy(s1, 0, pos(#32, s1) - 1), 0);
        NextSpaceDelimitedParameter;

        R.Right := StrToIntDef(s1, 100);
        R.Bottom := R.Right;

       if FUseMedia then
         begin
           //not implemented yet
           {s1:= CfgTrial.SList.Values[_Comp + IntToStr(a1+1)+_cStm] + #32;

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
              gap := StrToBoolDef( CfgTrial.SList.Values[_Comp + IntToStr(a1+1) + _cGap], False );
              if gap then
                  begin
                    gap_degree := StrToIntDef( CfgTrial.SList.Values[_Comp + IntToStr(a1 + 1) + _cGap_Degree], Random(360));
                    gap_length := StrToIntDef( CfgTrial.SList.Values[_Comp + IntToStr(a1 + 1) + _cGap_Length], 5 );
                  end;
            end;
      end;

  if FShowStarter then BeginStarter else StartTrial(Self);
  //showmessage(BoolToStr(FShowStarter));
end;

procedure TFPE.StartTrial(Sender: TObject);
//var a1 : integer;

  procedure KeyStart(var aKey : TKey);
  begin
    aKey.Play;
    aKey.Visible:= True;
  end;

begin
  inherited StartTrial(Sender);

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
end;

procedure TFPE.BeginStarter;
begin
  CreateClientThread('S:' + FormatFloat('00000000;;00000000', GetTickCount - TimeStart));
  FCanResponse:= True;
  Invalidate;
end;

procedure TFPE.WriteData(Sender: TObject);  //
var Latency : string;
begin
  if not FFirstResp then
    Latency := FormatFloat('00000000;;00000000',FDataSupport.Latency - TimeStart)
  else Latency := #32#32#32#32#32#32#32 + 'NA';

  {
  Header :=  'StartLat' + #9 +
             'StmBegin' + #9 +
             '_Latency' + #9 +
             '__StmEnd' + #9 +
             'RespFreq' + #9 +
             'ExpcResp' + #9 +
             '__Result'
  }

  Data :=  FormatFloat('00000000;;00000000',FDataSupport.StarterLatency - TimeStart) + #9 +
           FormatFloat('00000000;;00000000',FDataSupport.StmBegin - TimeStart) + #9 +
           Latency + #9 +
           FormatFloat('00000000;;00000000',FDataSupport.StmEnd - TimeStart) + #9 +
           Format('%-*.*d', [4,8, FDataSupport.Responses]) + #9 +
           #32#32#32#32#32#32#32 + FCurrTrial.response + #9 +
           #32#32#32#32#32#32#32 + FCurrTrial.Result + #9 +
           Data // forced session end data
           ;
end;

procedure TFPE.SetTimerCsq;
begin

end;

procedure TFPE.EndTrial(Sender: TObject);
begin
  FDataSupport.StmEnd := GetTickCount;
  WriteData(Sender);

  if Assigned(CounterManager.OnConsequence) then CounterManager.OnConsequence(Self);
  if Assigned(OnWriteTrialData) then OnWriteTrialData(Self);
  if Assigned(OnEndTrial) then OnEndTrial(sender);
end;

procedure TFPE.Hit(Sender: TObject);
begin
  if Assigned(OnHit) then OnHit(Sender);
end;

procedure TFPE.Miss(Sender: TObject);
begin
  if Assigned(OnMiss) then OnMiss(Sender);
end;


procedure TFPE.None(Sender: TObject);
begin
  if Assigned(OnNone) then OnNone(Sender);
end;

procedure TFPE.Response(Sender: TObject);
var TickCount : cardinal;
begin
  TickCount := GetTickCount;
  Inc(FDataSupport.Responses);
  if FFirstResp then
    begin
      FDataSupport.Latency := TickCount;
      FFirstResp := False;
    end
  else;

  // not implemented yet
  //if FUseMedia then
  //  if Sender is TKey then TKey(Sender).IncCounterResponse
  //    else;

  Invalidate;
  if Assigned(CounterManager.OnStmResponse) then CounterManager.OnStmResponse(Sender);
  if Assigned(OnStmResponse) then OnStmResponse (Self);
end;

end.

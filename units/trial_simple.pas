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
unit trial_simple;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils

    // TTimer
    , ExtCtrls

    , response_key
    , trial_abstract
    //, countermanager
    //, config_session
    , counter
    , constants
    , interface_rs232
    , interface_plp
    ;

type

  { Need testing, refactoring and implementation of new timing schedules }

  TSupportKey = record
    Key : TKey;
    Csq : BYTE;
    Usb : string;
    Msg : string;
    Nxt : string;
    Res : string;
    IET : string;
    TO_ : Integer;
  end;

  TDataSupport = record
    Latency,
    StmBegin,
    StmEnf : cardinal;
  end;

  { TSimpl }

  TSimpl = Class(TTrial)
  protected
    FCanPassTrial : Boolean;
    FDataBkGndI : TCounter;
    FDataBkGndS : string;
    FDataCMsg : string;
    FDataCsq : Byte;
    FDataSupport : TDataSupport;
    FDataUsb : string;
    FFirstResp : Boolean;
    FKMinus : TSupportKey;
    FKPlus : TSupportKey;
    FNumComp : Integer;
    FPLP : TPLP;
    FResponseEnabled : Boolean;
    FTimerCsq : TTimer;
    FTrialInterval : Integer;
    FVetSupportC : array of TSupportKey;
    procedure BeginCorrection (Sender: TObject);
    procedure Consequence(Sender: TObject);
    procedure Dispenser(Csq: Byte; Usb: string);
    procedure EndCorrection(Sender: TObject);
    procedure EndTrial(Sender: TObject);
    procedure Hit(Sender: TObject);
    procedure LimitedHoldOnTimer(Sender: TObject);
    procedure Miss(Sender: TObject);
    procedure None(Sender: TObject);
    procedure Response(Sender: TObject);
    procedure SetTimerCsq;
    procedure TrialResult(Sender: TObject);
    // TTrial
    procedure ThreadClock(Sender: TObject); override;
    procedure StartTrial(Sender: TObject); override;
    procedure WriteData(Sender: TObject); override;
    // TCustomControl
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Play(TestMode: Boolean; Correction : Boolean); override;
    procedure DispenserPlusCall; override;
  end;

implementation

constructor TSimpl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Header :=  #9 +
             'Pos.Cmp.' + #9 +        //header do Relatório
             'Res.Cmp.' + #9 +
             'Lat.Cmp.' + #9 + #9 +
             'Dur.Cmp.' + #9 + #9 +
             'Tmp.Cmp.' + #9 + #9 +
             'Disp.   ' + #9 +
             'UDsp.   ' + #9 +
             'Res.Frq.';

  HeaderTicks := 'Tempo_ms' + #9 +
                 'Cmp.Tipo' + #9 +
                 'FileName' + #9 +
                 'Left....' + #9 +
                 'Top.....';

  FDataBkGndI := TCounter.Create;
end;

destructor TSimpl.Destroy;
begin
  FDataBkGndI.Free;
  inherited Destroy;
end;

procedure TSimpl.Dispenser(Csq: Byte; Usb: string);
begin
  FPLP.OutPortOn (Csq);
  //FRS232.Dispenser(Usb);
end;

procedure TSimpl.DispenserPlusCall;
begin
  Dispenser(FKPlus.Csq, FKPlus.Usb);
end;

procedure TSimpl.Play(TestMode: Boolean; Correction : Boolean);
var
  s1, sName, sLoop, sColor : string;
  R : TRect;
  a1 : Integer;

  procedure NextSpaceDelimitedParameter;
  begin
    Delete(s1, 1, pos(#32, s1));
    if Length(s1) > 0 then while s1[1] = #32 do Delete(s1, 1, 1);
  end;

begin
  Randomize;

  // TCustomControl
  Color := StrToIntDef(CfgTrial.SList.Values[_BkGnd], 0);

  if TestMode then Cursor := 0
  else Cursor := StrToIntDef(CfgTrial.SList.Values[_Cursor], 0);

  // TTrial
  RootMedia := CfgTrial.SList.Values[_RootMedia];
  NextTrial := '-1';

  if Correction then FIsCorrection := True
  else FIsCorrection := False;

  // Self
  FResponseEnabled := False;
  FLimitedHold := StrToIntDef(CfgTrial.SList.Values[_LimitedHold], 0);

  FNumComp := StrToIntDef(CfgTrial.SList.Values[_NumComp], 0);
  SetLength(FVetSupportC, FNumComp);
  for a1:= 0 to FNumComp-1 do
    begin
      with FVetSupportC[a1] do
        begin
          Key := TKey.Create(Self);
          Key.Tag := a1;
          Key.Cursor := Self.Cursor;
          Key.Visible := False;
          Key.OnConsequence := @Consequence;
          Key.OnResponse := @Response;

          with Key.Schedule do
            begin
              Kind := CfgTrial.SList.Values[_Comp + IntToStr(a1 + 1) + _cSch];
              if Loaded then
                begin
                  SetLength(FClockList, Length(FClockList) + 1);
                  FClockList[Length(FClockList) -1] := StartMethod;
                end
              else raise Exception.Create(ExceptionNoScheduleFound);
            end;
          Key.Parent := Self;

          s1 := CfgTrial.SList.Values[_Comp + IntToStr(a1 + 1) + _cBnd];
          R.Top := StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
          NextSpaceDelimitedParameter;

          R.Left := StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
          NextSpaceDelimitedParameter;

          R.Bottom := StrToIntDef(s1, 0);
          NextSpaceDelimitedParameter;

          R.Right := StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
          Key.SetBounds(R.Left, R.Top, R.Right, R.Bottom);

          s1 := CfgTrial.SList.Values[_Comp + IntToStr(a1 + 1) + _cStm] + #32;
          sName := RootMedia + Copy(s1, 0, pos(#32, s1)-1);
          Key.FullPath := sName;
          NextSpaceDelimitedParameter;

          sLoop := Copy(s1, 0, pos(#32, s1)-1);
          Key.HowManyLoops:= StrToIntDef(sLoop, 0);
          NextSpaceDelimitedParameter;

          sColor := Copy(s1, 0, pos(#32, s1)-1);
          Key.Color := StrToIntDef(sColor, $0000FF); //clRed

          Csq := StrToIntDef(CfgTrial.SList.Values[_Comp + IntToStr(a1 + 1) + _cCsq], 0);
          //Usb := CfgTrial.SList.Values[_Comp+IntToStr(a1+1)+_cUsb];
          Msg := CfgTrial.SList.Values[_Comp + IntToStr(a1 + 1) + _cMsg];
          Res := CfgTrial.SList.Values[_Comp + IntToStr(a1 + 1) + _cRes];
          Nxt := CfgTrial.SList.Values[_Comp + IntToStr(a1 + 1) + _cNxt];
          TO_ := StrToIntDef(CfgTrial.SList.Values[_Comp + IntToStr(a1 + 1) + _cTO], 0);
          IET := CfgTrial.SList.Values[_Comp + IntToStr(a1 + 1) + _cIET];
        end;
    end;

  with FKPlus do begin
    Csq := StrToIntDef(CfgTrial.SList.Values[_Kplus + _cCsq], 0);
    //Usb := StrToIntDef(CfgTrial.SList.Values[_Kplus + _cUsb], 0);
    Msg := CfgTrial.SList.Values[_Kplus + _cMsg];
    Res := CfgTrial.SList.Values[_Kplus + _cRes];
    Nxt := CfgTrial.SList.Values[_Kplus + _cNxt];
    IET := CfgTrial.SList.Values[_Kplus + _cIET];
    TO_ := StrToIntDef(CfgTrial.SList.Values[_Kplus + _cTO],0);
  end;

  with FKMinus do begin
    Csq := StrToIntDef(CfgTrial.SList.Values[_Kminus + _cCsq], 0);
    //Usb := StrToIntDef(CfgTrial.SList.Values[_Kminus + _cUsb], 0);
    Msg := CfgTrial.SList.Values[_Kminus + _cMsg];
    Res := CfgTrial.SList.Values[_Kminus + _cRes];
    Nxt := CfgTrial.SList.Values[_Kminus + _cNxt];
    IET := CfgTrial.SList.Values[_Kminus + _cIET];
    TO_ := StrToIntDef(CfgTrial.SList.Values[_Kminus + _cTO],0);
  end;

  StartTrial(Self);
end;

procedure TSimpl.TrialResult(Sender: TObject);
begin
  if FResponseEnabled then
    begin
      FResponseEnabled:= False;
      FDataSuport.StmEnd := GetTickCount;

      if (FVetSupportC[TKey(Sender).Tag].Res = T_HIT) or
         (FVetSupportC[TKey(Sender).Tag].Res = T_MISS)  then
        begin
          if FVetSupportC[TKey(Sender).Tag].Res = T_HIT then Result := 'HIT';

          if FVetSupportC[TKey(Sender).Tag].Res = T_MISS then Result := 'MISS';

        end
      else Result := 'NONE';

      //FDataCsq:= FVetSupportC[TKey(Sender).Tag].Csq;
      //FDataUsb := FVetSupportC[TKey(Sender).Tag].Usb;
      TimeOut := FVetSupportC[TKey(Sender).Tag].TO_;
      IETConsequence := FVetSupportC[TKey(Sender).Tag].IET;
      if FVetSupportC[TKey(Sender).Tag].Nxt = T_CRT then NextTrial := T_CRT
      else NextTrial := FVetSupportC[TKey(Sender).Tag].Nxt;

      if FVetSupportC[TKey(Sender).Tag].Msg = 'GONOGO' then
        begin       //GONOGO serve apenas para a diferenciação entre Go e NoGo nos esquemas RRRT
           if FConsequenceFired then FDataCMsg := 'GO'
           else  FDataCMsg := 'NOGO';

           if (FConsequenceFired = False) and (FVetSupportC[TKey(Sender).Tag].Res = T_HIT) then
             begin
               Result := 'MISS';
             end;
           if (FConsequenceFired = False) and (FVetSupportC[TKey(Sender).Tag].Res = T_MISS) then
             begin
               FDataCsq := 0;
               FDataUSB := '0';
               TimeOut := 0;
               Result := T_HIT;
             end;

           if FConsequenceFired and (FVetSupportC[TKey(Sender).Tag].Res = T_HIT) then TimeOut := 0;
           if FConsequenceFired and (FVetSupportC[TKey(Sender).Tag].Res = T_MISS) then
             begin
               FDataCsq := 0;
               FDataUSB := '0';
             end;
        end
      else FDataCMsg:= FVetSupportC[TKey(Sender).Tag].Msg;

      if Result = T_HIT then  Hit(Sender);
      if Result = T_MISS then  Miss(Sender);
      if Result = T_NONE then  None(Sender);

      if not (FLimitedHold > 0) then EndTrial(Sender);
    end;
end;

procedure TSimpl.ThreadClock(Sender: TObject);
begin
  TrialResult(Sender);
  EndTrial(Sender);
end;

procedure TSimpl.StartTrial(Sender: TObject);
var a1 : integer;
begin
  if FIsCorrection then
    begin
      BeginCorrection(Self);
    end;

  for a1:= 0 to FNumComp-1 do
    begin
      FVetSupportC[a1].Key.Play;
      FVetSupportC[a1].Key.Visible := True;
    end;

  with FDataSupport do
    begin
      Latency := TimeStart;
      StmEnd := TimeStart;
    end;

  FFirstResp := False;
  FResponseEnabled := True;

  FDataSuport.StmBegin := GetTickCount;
  inherited StartTrial(Sender);
end;


procedure TSimpl.WriteData(Sender: TObject);  //Dados do Relatório
var
    Lat_Cmp,
    Dur_CmpResponse,
    Dur_Cmp,
    Res_Cmp,
    Disp,
    uDisp,
    PosComps,
    Res_Frq: string;

    a1: Integer;
begin
  Res_Frq := '';
  PosComps := '';
  Disp := '';
  uDisp := '';
  for a1 := 0 to ComponentCount - 1 do
    begin
      if Components[a1] is TKey then
        begin
          Res_Frq := Res_Frq + _Comp + IntToStr ((TKey(Components[a1]).Tag + 1)) + '=' +
                                   IntToStr (TKey(Components[a1]).ResponseCount) + #9;
        end;
    end;
  Res_Frq := Res_Frq + 'BK='+ IntToStr(FDataBkGndI.Counter);

  for a1:= 0 to FNumComp-1 do
    begin
      if FVetSupportC[a1].Msg = '' then FVetSupportC[a1].Msg:= '-';
      PosComps:= PosComps + FVetSupportC[a1].Msg + #32;
    end;

  if FDataCMsg = '' then FDataCMsg:= '--------';
  Res_Cmp:= LeftStr(FDataCMsg+#32#32#32#32#32#32#32#32, 8);


  if FDataSuport.Latency = 0 then
    begin
      Lat_Cmp:= '0' + #9 + 'ms';
      Dur_CmpResponse := '0' + #9 + 'ms';
    end
  else
    begin
      Lat_Cmp:= FormatFloat('00000000;;00000000',FDataSuport.Latency - TimeStart) + #9 + 'ms';
      Dur_CmpResponse := FormatFloat('00000000;;00000000',FDataSuport.StmEnd - TimeStart) + #9 + 'ms';
    end;

  Dur_Cmp := FormatFloat('00000000;;00000000',FDataSuport.StmEnd - FDataSuport.StmBegin) + #9 + 'ms';

  //Disp:= RightStr(IntToBin(FDataCsq, 9)+#0, 8);
  //uDisp := IntToStr(FDataUsb);

  Data := PosComps  + #9 +
          Res_Cmp   + #9 +
          Lat_Cmp  + #9 +  //#9
          Dur_CmpResponse + #9 +
          Dur_Cmp + #9 +
          Disp      + #9 +
          uDisp     + #9 +
          Res_Frq + #9 +
          Data;

  if Assigned(OnWriteTrialData) then OnWriteTrialData(Self);
end;

procedure TSimpl.EndCorrection(Sender: TObject);
begin
  if Assigned (OnEndCorrection) then OnEndCorrection (Sender);
end;

procedure TSimpl.EndTrial(Sender: TObject);
begin
  if FDataSuport.Latency = 0 then FDataSuport.Latency:= GetTickCount;
  WriteData(Sender);
  if FCanPassTrial then
    if Assigned(OnEndTrial) then OnEndTrial(sender);
end;

procedure TSimpl.Hit(Sender: TObject);
begin
  if Assigned(OnHit) then OnHit(Sender);
end;

procedure TSimpl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown (Key, Shift);
  if Key = 27 {ESC} then FResponseEnabled:= False;

  if (ssCtrl in Shift) and (Key = 13) {Enter} then
    begin
      FDataCMsg:= 'Cancel';
      if FCanPassTrial then FTimerCsq.Enabled:= True else
        begin
          FDataSuport.StmBegin := GetTickCount;
          FResponseEnabled:= True;
          EndTrial(Self);
        end;
    end;
  if (Key = 107) {+} then
    begin
      FDataCMsg:= FKPlus.Msg;
      FDataCsq:= FKPlus.Csq;
      //FDataUsb := FKPlus.Usb;
      Result:= FKPlus.Res;
      IETConsequence:= FKPlus.IET;
      TimeOut := FKPlus.TO_;
      Dispenser(FKPlus.Csq, FKPlus.Usb);
    end;

  if (Key = 109) {-} then
    begin
      FDataCMsg:= FKMinus.Msg;
      FDataCsq:= FKMinus.Csq;
      //FDataUsb := FKMinus.Usb;
      Result:= FKMinus.Res;
      TimeOut:= FKMinus.TO_;
      IETConsequence:= FKMinus.IET;

      Dispenser(FKMinus.Csq, FKMinus.Usb);
    end;

end;

procedure TSimpl.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then FResponseEnabled:= True;
end;

procedure TSimpl.Miss(Sender: TObject);
begin
  if Assigned(OnMiss) then OnMiss (Sender);
end;

procedure TSimpl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var TickCount : cardinal;
begin
  TickCount := GetTickCount;
  inherited MouseDown(Button,Shift, X, Y);

  DataTicks := DataTicks +
               FormatFloat('####,####',TickCount - FDataSuport.StmBegin) + #9 +
               '-' + #9 +
               '-' + #9 +
               IntToStr(X) + #9 + IntToStr(Y) + LineEnding;
  FDataBkGndI.Plus(1);

  CounterManager.OnBkgndResponse(FDataBkGndI);
  if Assigned(OnBkGndResponse) then OnBkGndResponse(Self);
end;

procedure TSimpl.None(Sender: TObject);
begin
  if Assigned(OnNone) then OnMiss (Sender);
end;

procedure TSimpl.BeginCorrection(Sender: TObject);
begin
  if Assigned (OnBeginCorrection) then OnBeginCorrection (Sender);
end;

procedure TSimpl.Consequence(Sender: TObject);
begin
  { The Dispenser requires working interfaces. They were not tested. }
  //Dispenser(FDataCsq, FDataUsb);
  CounterManager.OnConsequence(Sender);
  if Assigned(OnConsequence) then OnConsequence(Sender);
  if not (FLimitedHold > 0) then TrialResult(Sender);
  else
    if FConsequenceFired = False then FConsequenceFired := True;
end;

procedure TSimpl.Response(Sender: TObject);
var
    TickCount : cardinal;
    aTime, aCode, aStimulus, aLeft, aTop : string;
begin
  TickCount := GetTickCount;
  if FResponseEnabled then
    begin
      aTime := FormatFloat('####,####', TickCount - TimeStart);
      aCode := 'C' + IntToStr(TKey(Sender).Tag + 1);
      aStimulus := ExtractFileName(TKey(Sender).FullPath);
      aLeft := IntToStr(TKey(Sender).LastResponsePoint[0] + TKey(Sender).Left);
      aTop := IntToStr(TKey(Sender).LastResponsePoint[1] + TKey(Sender).Top);
      DataTicks := DataTicks +
        aTime + #9 + aCode + #9 + aStimulus + #9 + aLeft + #9 + aTop + LineEnding;

      if FFirstResp = False then
      begin
        FDataSuport.Latency := TickCount;
        FFirstResp := True;
      end;

      TKey(Sender).IncResponseCount;
      if Assigned(CounterManager.OnStmResponse) then CounterManager.OnStmResponse(Sender);
      if Assigned(OnStmResponse) then OnStmResponse (Self);
    end;
end;

end.

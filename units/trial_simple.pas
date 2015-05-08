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

  TSupportKey = Record
    Key: TKey;
    Csq: BYTE;
    Usb: String;
    Msg: String;
    Nxt: String;
    Res: String;
    IET: string;
    TO_: Integer;
  end;

  { TSimpl }

  TSimpl = Class(TTrial)
  protected
    FFirstResp : Boolean;
    FFlagCsq2Fired : Boolean;
    FTrialInterval: Integer;
    FCanPassTrial: Boolean;
    Ft            : cardinal;
    FDurCmp : cardinal;
    FLatCmp       : cardinal;
    FDataUsb     : String;
    FDataCMsg: String;
    FDataBkGndI: TCounter;
    FDataBkGndS: String;
    FDataCsq: Byte;
    FTimerCsq: TTimer;
    FFlagResp: Boolean;
    FKPlus: TSupportKey;
    FKMinus: TSupportKey;
    FNumKeyC: Integer;
    FPLP : TPLP;
    FVetSupportC: Array of TSupportKey;
    procedure BeginCorrection (Sender : TObject);
    procedure EndCorrection(Sender : TObject);
    procedure LimitedHoldOnTimer(Sender : TObject);
    procedure Consequence(Sender: TObject);
    procedure Consequence2(Sender: TObject);
    procedure Response(Sender: TObject);
    procedure Hit(Sender: TObject);
    procedure Miss(Sender: TObject);
    procedure None(Sender: TObject);

    procedure TimerCsqTimer(Sender: TObject);
    procedure Dispenser(Csq: Byte; Usb: string);
    procedure StartTrial(Sender: TObject); override;
    procedure EndTrial(Sender: TObject);

    procedure SetTimerCsq;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure WriteData(Sender: TObject); override;
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
  //
  FFlagCsq2Fired := False;
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
var s1,sName,sLoop,sColor: String; R: TRect; a1: Integer;
begin
  RootMedia := CfgTrial.SList.Values[_RootMedia];
  Randomize;
  FFlagResp:= False;
  NextTrial:= '-1';

  if Correction then FIsCorrection := True
  else FIsCorrection := False;

  Color:= StrToIntDef(CfgTrial.SList.Values[_BkGnd], 0);
  FCanPassTrial := StrToBoolDef(CfgTrial.SList.Values[_AutoNxt], True);
  if FCanPassTrial = False then
    begin
      FTrialInterval := StrToIntDef(CfgTrial.SList.Values[_CustomNxtValue], 10000);
      SetTimerCsq;
    end;

  If TestMode then Cursor:= 0
  else Cursor:= StrToIntDef(CfgTrial.SList.Values[_Cursor], 0);

  FNumKeyC:= StrToIntDef(CfgTrial.SList.Values[_NumComp], 0);

  SetLength(FVetSupportC, FNumKeyC);
  For a1:= 0 to FNumKeyC-1 do
    begin
      with FVetSupportC[a1] do
        begin
          Key:= TKey.Create(Self);
          Key.Tag:= a1;
          Key.Cursor:= Self.Cursor;
          Key.Parent:= Self;
          Key.OnConsequence:= @Consequence;
          Key.OnConsequence2:= @Consequence2;
          Key.OnResponse:= @Response;

          s1:= CfgTrial.SList.Values[_Comp+IntToStr(a1+1)+_cBnd];
          R.Top:= StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
          Delete(s1, 1, pos(#32, s1)); If Length(s1)>0 then While s1[1]=#32 do Delete(s1, 1, 1);
          R.Left:= StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
          Delete(s1, 1, pos(#32, s1)); If Length(s1)>0 then While s1[1]=#32 do Delete(s1, 1, 1);
          R.Right:= StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
          Delete(s1, 1, pos(#32, s1)); If Length(s1)>0 then While s1[1]=#32 do Delete(s1, 1, 1);
          R.Bottom:= StrToIntDef(s1, 0);
          Key.SetBounds(R.Left, R.Top, R.Right, R.Bottom);

          s1:= CfgTrial.SList.Values[_Comp+IntToStr(a1+1)+_cStm] + #32;
          sName := RootMedia + Copy(s1, 0, pos(#32, s1)-1);
          Delete(s1, 1, pos(#32, s1)); If Length(s1)>0 then While s1[1]=#32 do Delete(s1, 1, 1);
          sLoop := Copy(s1, 0, pos(#32, s1)-1);
          Delete(s1, 1, pos(#32, s1)); If Length(s1)>0 then While s1[1]=#32 do Delete(s1, 1, 1);
          sColor := Copy(s1, 0, pos(#32, s1)-1);
          Key.Color := StrToIntDef(sColor, $0000FF{clRed});
          Key.HowManyLoops:= StrToIntDef(sLoop, 0);
          Key.FullPath:= sName;
          Key.SchMan.Kind:= CfgTrial.SList.Values[_Comp+IntToStr(a1+1)+_cSch];

          Csq:= StrToIntDef(CfgTrial.SList.Values[_Comp+IntToStr(a1+1)+_cCsq], 0);
          Usb := CfgTrial.SList.Values[_Comp+IntToStr(a1+1)+_cUsb];//FRS232.GetUsbCsqFromValue ();

          Msg:= CfgTrial.SList.Values[_Comp+IntToStr(a1+1)+_cMsg];
          Res:= CfgTrial.SList.Values[_Comp+IntToStr(a1+1)+_cRes];
          Nxt:= CfgTrial.SList.Values[_Comp+IntToStr(a1+1)+_cNxt];
          TO_ := StrToIntDef(CfgTrial.SList.Values[_Comp+IntToStr(a1+1)+_cTO], 0);
          IET := CfgTrial.SList.Values[_Comp+IntToStr(a1+1)+_cIET];
        end;
    end;

  with FKPlus do begin
    Csq:= StrToIntDef(CfgTrial.SList.Values[_Kplus + _cCsq], 0);
    //Usb:= StrToIntDef(CfgTrial.SList.Values[_Kplus + _cUsb], 0);
    Msg:= CfgTrial.SList.Values[_Kplus + _cMsg];
    Res:= CfgTrial.SList.Values[_Kplus + _cRes];
    Nxt:= CfgTrial.SList.Values[_Kplus + _cNxt];
    IET := CfgTrial.SList.Values[_Kplus + _cIET];
    TO_:= StrToIntDef(CfgTrial.SList.Values[_Kplus + _cTO],0);
  end;

  with FKMinus do begin
    Csq:= StrToIntDef(CfgTrial.SList.Values[_Kminus + _cCsq], 0);
    //Usb:= StrToIntDef(CfgTrial.SList.Values[_Kminus + _cUsb], 0);
    Msg:= CfgTrial.SList.Values[_Kminus + _cMsg];
    Res:= CfgTrial.SList.Values[_Kminus + _cRes];
    Nxt:= CfgTrial.SList.Values[_Kminus + _cNxt];
    IET := CfgTrial.SList.Values[_Kminus + _cIET];
    TO_:= StrToIntDef(CfgTrial.SList.Values[_Kminus + _cTO],0);
  end;

  StartTrial(Self);
end;

procedure TSimpl.SetTimerCsq;
begin
  FTimerCsq.Interval := FTrialInterval;
end;

procedure TSimpl.StartTrial(Sender: TObject);
var a1 : integer;
begin
  inherited StartTrial(Sender);

  if FCanPassTrial then FTimerCsq.Enabled:= False else FTimerCsq.Enabled:= True;
  if FIsCorrection then
    begin
      BeginCorrection (Self);
    end;
  for a1:= 0 to FNumKeyC-1 do
    begin
      FVetSupportC[a1].Key.Play;
      FVetSupportC[a1].Key.Visible:= True;
    end;
  FFirstResp := False;
  FFlagResp:= True;
  Ft := GetTickCount;
  FLatCmp:= 0;
  FDurCmp := 0;
end;

procedure TSimpl.TimerCsqTimer(Sender: TObject);
begin
  FTimerCsq.Enabled:= False;
  FCanPassTrial := True;
  EndTrial(Sender);
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
    Res_Frq: String;

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

  for a1:= 0 to FNumKeyC-1 do
    begin
      If FVetSupportC[a1].Msg = '' then FVetSupportC[a1].Msg:= '-';
      PosComps:= PosComps + FVetSupportC[a1].Msg + #32;
    end;

  If FDataCMsg = '' then FDataCMsg:= '--------';
  Res_Cmp:= LeftStr(FDataCMsg+#32#32#32#32#32#32#32#32, 8);

  //Lat_Cmp:= FormatFloat('#####,###',FLatCmp - Ft) + #9 + 'ms';
  //FLatCmp := 0;
  {begin latência}
  if FLatCmp = 0 then
    begin
      Lat_Cmp:= '0' + #9 + 'ms';
      Dur_CmpResponse := '0' + #9 + 'ms';
    end
  else
    begin
      Lat_Cmp:= FormatFloat('#####,###',FLatCmp - Ft) + #9 + 'ms';
      Dur_CmpResponse := FormatFloat('#####,###',FDurCmp - FLatCmp) + #9 + 'ms';
    end;

  Dur_Cmp := FormatFloat('####,####',FDurCmp - Ft) + #9 + 'ms';
  {end Latência}
  //Disp:= RightStr(IntToBin(FDataCsq, 9)+#0, 8);
  //uDisp := IntToStr(FDataUsb);

  Data := PosComps  + #9 +
          Res_Cmp   + #9 +
          Lat_Cmp  + #9 +  //#9
          Dur_CmpResponse + #9 +
          Dur_Cmp + #9 +
          Disp      + #9 +
          uDisp     + #9 +
          Res_Frq;


  CounterManager.OnConsequence(Self);
  if Assigned(OnWriteTrialData) then OnWriteTrialData(Self);
end;

procedure TSimpl.EndCorrection(Sender: TObject);
begin
  if Assigned (OnEndCorrection) then OnEndCorrection (Sender);
end;

procedure TSimpl.LimitedHoldOnTimer(Sender: TObject);
begin

end;

procedure TSimpl.EndTrial(Sender: TObject);
begin
  if FLatCmp = 0 then FLatCmp:= GetTickCount;
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
  If Key = 27 {ESC} then FFlagResp:= False;

  If (ssCtrl in Shift) and (Key = 13) {Enter} then
    begin
      FDataCMsg:= 'Cancel';
      if FCanPassTrial then FTimerCsq.Enabled:= True else
        begin
          Ft := GetTickCount;
          FFlagResp:= True;
          EndTrial(Self);
        end;
    end;
  If (Key = 107) {+} then
    begin
      FDataCMsg:= FKPlus.Msg;
      FDataCsq:= FKPlus.Csq;
      //FDataUsb := FKPlus.Usb;
      Result:= FKPlus.Res;
      IETConsequence:= FKPlus.IET;
      TimeOut := FKPlus.TO_;
      Dispenser(FKPlus.Csq, FKPlus.Usb);
    end;

  If (Key = 109) {-} then
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

procedure TSimpl.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);

end;

procedure TSimpl.KeyUp(var Key: Word; Shift: TShiftState);
begin
  If Key = 27 then FFlagResp:= True;
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
               FormatFloat('####,####',TickCount - Ft) + #9 +
               '-' + #9 +
               '-' + #9 +
               IntToStr(X) + #9 + IntToStr(Y) + #13#10 + #9 + #9;
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
  If FFlagResp then
    begin
      FFlagResp:= False;
      FDurCmp := GetTickCount;

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
         if FFlagCsq2Fired then FDataCMsg := '1'
         else  FDataCMsg := '2';

         if (FFlagCsq2Fired = False) and (FVetSupportC[TKey(Sender).Tag].Res = T_HIT) then
           begin
             Result := 'MISS';
           end;
         if (FFlagCsq2Fired = False) and (FVetSupportC[TKey(Sender).Tag].Res = T_MISS) then
           begin
             FDataCsq := 0;
             FDataUSB := '0';
             TimeOut := 0;
             Result := 'HIT';
           end;

         if FFlagCsq2Fired and (FVetSupportC[TKey(Sender).Tag].Res = T_HIT) then TimeOut := 0;
         if FFlagCsq2Fired and (FVetSupportC[TKey(Sender).Tag].Res = T_MISS) then
           begin
             FDataCsq := 0;
             FDataUSB := '0';
           end;
      end
    else FDataCMsg:= FVetSupportC[TKey(Sender).Tag].Msg;

      //Dispenser(FDataCsq, FDataUsb);
      if Result = 'HIT' then  Hit(Sender);
      if Result = 'MISS' then  Miss(Sender);
      if Result = 'NONE' then  None(Sender);

      if FCanPassTrial then FTimerCsq.Enabled:= True else
        begin
          EndTrial(Sender);
          Ft := GetTickCount;
          FFlagResp:= True;
        end;
    end;
end;
procedure TSimpl.Consequence2(Sender: TObject);
begin
  if FFlagCsq2Fired = False then FFlagCsq2Fired := True;
  Dispenser(FDataCsq, FDataUsb)
  //FPLP.OutPortOn (FKPlus.Csq);
  //FRS232.Dispenser(FKPlus.Usb);
end;

procedure TSimpl.Response(Sender: TObject);
var TickCount : cardinal;
begin
  TickCount := GetTickCount;
  if FFlagResp then
    begin
      DataTicks :=
             DataTicks +
             FormatFloat('####,####',TickCount - Ft) + #9 +
             'C' + IntToStr(TKey(Sender).Tag + 1) + #9 +
             ExtractFileName(TKey(Sender).FullPath) + #9 +
             IntToStr(TKey(Sender).LastResponsePoint[0] + TKey(Sender).Left) + #9 +
             IntToStr(TKey(Sender).LastResponsePoint[1] + TKey(Sender).Top) + #13#10 + #9 + #9;

       if FFirstResp = False then
        begin
          FLatCmp := TickCount;
          FFirstResp := True;
        end;

      TKey(Sender).IncCounterResponse;
      if Assigned(CounterManager.OnStmResponse) then CounterManager.OnStmResponse (Sender);
      if Assigned(OnStmResponse) then OnStmResponse (Self);
    end;
end;

end.

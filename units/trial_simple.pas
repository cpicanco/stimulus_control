//
// Validation Project (PCRF) - Stimulus Control App
// Copyright (C) 2014,  Carlos Rafael Fernandes Picanço, cpicanco@ufpa.br
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

{$MODE Delphi}

interface

uses Dialogs,
     Classes, Types, SysUtils, Controls,
     Graphics, ExtCtrls, StrUtils, {IdGlobal, IdGlobalProtocols,}
     LCLIntf, LCLType, Forms,
     custom_timer,
     response_key, trial, countermanager,
     session_config, counter, constants, interface_library;

type

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

  TSimpl = Class(TTrial)
  protected
    //FComparisonFocus : integer;
    FFirstResp : Boolean;
    //FClockSwitchON : Boolean;
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
    //FTimerClock: TTimer;
    FClockThread: TClockThread;
    FFlagResp: Boolean;
    FKPlus: TSupportKey;
    FKMinus: TSupportKey;
    FNumKeyC: Integer;
    FPLP : TPLP;
    FVetSupportC: Array of TSupportKey;
    procedure BeginCorrection (Sender : TObject);
    procedure EndCorrection (Sender : TObject);

    procedure Consequence(Sender: TObject);
    procedure Consequence2(Sender: TObject);
    procedure Response(Sender: TObject);
    procedure Hit(Sender: TObject);
    procedure Miss(Sender: TObject);
    procedure None(Sender: TObject);

    procedure TimerCsqTimer(Sender: TObject);
    procedure TimerClockTimer(Sender: TObject);
    procedure Dispenser(Csq: Byte; Usb: string);
    procedure StartTrial;
    procedure EndTrial(Sender: TObject);

    procedure SetTimerCsq;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    procedure Play(Manager : TCounterManager; TestMode: Boolean; Correction : Boolean); override;
    procedure DispenserPlusCall; override;
  end;

implementation

constructor TSimpl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FHeader:=  #9 +
             'Pos.Cmp.' + #9 +        //header do Relatório
             'Res.Cmp.' + #9 +
             'Lat.Cmp.' + #9 + #9 +
             'Dur.Cmp.' + #9 + #9 +
             'Tmp.Cmp.' + #9 + #9 +
             'Disp.   ' + #9 +
             'UDsp.   ' + #9 +
             'Res.Frq.';

  FHeaderTicks:= 'Tempo_ms' + #9 +
                 'Cmp.Tipo' + #9 +
                 'FileName' + #9 +
                 'Left....' + #9 +
                 'Top.....';

  FDataBkGndI := TCounter.Create;

  FTimerCsq:= TTimer.Create(Self);
  with FTimerCsq do
    begin
      Enabled:= False;
      Interval:= 1;
      OnTimer:= TimerCsqTimer;
    end;

  //This Timer controls Sch integer increment.
  //Native TTimer was substituted for
  //a threaded custom timer (uTrial -> TClockThread) for better performance
  {FTimerClock:= TTimer.Create(Self);
  with FTimerClock do
    begin
      Enabled:= False;
      Interval:= 100;
      OnTimer:= TimerClockTimer;
    end;}
   FFlagCsq2Fired := False;
  //FComparisonFocus := -1;
  //FClockSwitchON := True;
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

procedure TSimpl.Play(Manager : TCounterManager; TestMode: Boolean; Correction : Boolean);
var s1,sName,sLoop,sColor: String; R: TRect; a1: Integer;
begin
  FRootMedia := FCfgTrial.SList.Values[_RootMedia];
  FManager := Manager;
  Randomize;
  FFlagResp:= False;
  FNextTrial:= '-1';

  if Correction then FIsCorrection := True
  else FIsCorrection := False;

  Color:= StrToIntDef(FCfgTrial.SList.Values[_BkGnd], 0);
  FCanPassTrial := StrToBoolDef(FCfgTrial.SList.Values[_AutoNxt], True);
  if FCanPassTrial = False then
    begin
      FTrialInterval := StrToIntDef(FCfgTrial.SList.Values[_CustomNxtValue], 10000);
      SetTimerCsq;
    end;

  If TestMode then Cursor:= 0
  else Cursor:= StrToIntDef(FCfgTrial.SList.Values[_Cursor], 0);

  FNumKeyC:= StrToIntDef(FCfgTrial.SList.Values[_NumComp], 0);

  SetLength(FVetSupportC, FNumKeyC);
  For a1:= 0 to FNumKeyC-1 do
    begin
      with FVetSupportC[a1] do
        begin
          Key:= TKey.Create(Self);
          Key.Tag:= a1;
          Key.Cursor:= Self.Cursor;
          Key.Parent:= Self;
          Key.OnConsequence:= Consequence;
          Key.OnConsequence2:= Consequence2;
          Key.OnResponse:= Response;

          s1:= FCfgTrial.SList.Values[_Comp+IntToStr(a1+1)+_cBnd];
          R.Top:= StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
          Delete(s1, 1, pos(#32, s1)); If Length(s1)>0 then While s1[1]=#32 do Delete(s1, 1, 1);
          R.Left:= StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
          Delete(s1, 1, pos(#32, s1)); If Length(s1)>0 then While s1[1]=#32 do Delete(s1, 1, 1);
          R.Right:= StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
          Delete(s1, 1, pos(#32, s1)); If Length(s1)>0 then While s1[1]=#32 do Delete(s1, 1, 1);
          R.Bottom:= StrToIntDef(s1, 0);
          Key.SetBounds(R.Left, R.Top, R.Right, R.Bottom);

          s1:= FCfgTrial.SList.Values[_Comp+IntToStr(a1+1)+_cStm] + #32;
          sName := RootMedia + Copy(s1, 0, pos(#32, s1)-1);
          Delete(s1, 1, pos(#32, s1)); If Length(s1)>0 then While s1[1]=#32 do Delete(s1, 1, 1);
          sLoop := Copy(s1, 0, pos(#32, s1)-1);
          Delete(s1, 1, pos(#32, s1)); If Length(s1)>0 then While s1[1]=#32 do Delete(s1, 1, 1);
          sColor := Copy(s1, 0, pos(#32, s1)-1);
          Key.Color := StrToIntDef(sColor, clRed);
          Key.HowManyLoops:= StrToIntDef(sLoop, 0);
          Key.FullPath:= sName;
          Key.SchMan.Kind:= FCfgTrial.SList.Values[_Comp+IntToStr(a1+1)+_cSch];

          Csq:= StrToIntDef(FCfgTrial.SList.Values[_Comp+IntToStr(a1+1)+_cCsq], 0);
          Usb := FCfgTrial.SList.Values[_Comp+IntToStr(a1+1)+_cUsb];//FRS232.GetUsbCsqFromValue ();

          Msg:= FCfgTrial.SList.Values[_Comp+IntToStr(a1+1)+_cMsg];
          Res:= FCfgTrial.SList.Values[_Comp+IntToStr(a1+1)+_cRes];
          Nxt:= FCfgTrial.SList.Values[_Comp+IntToStr(a1+1)+_cNxt];
          TO_ := StrToIntDef(FCfgTrial.SList.Values[_Comp+IntToStr(a1+1)+_cTO], 0);
          IET := FCfgTrial.SList.Values[_Comp+IntToStr(a1+1)+_cIET];
        end;
    end;

  with FKPlus do begin
    Csq:= StrToIntDef(FCfgTrial.SList.Values[_Kplus + _cCsq], 0);
    //Usb:= StrToIntDef(FCfgTrial.SList.Values[_Kplus + _cUsb], 0);
    Msg:= FCfgTrial.SList.Values[_Kplus + _cMsg];
    Res:= FCfgTrial.SList.Values[_Kplus + _cRes];
    Nxt:= FCfgTrial.SList.Values[_Kplus + _cNxt];
    IET := FCfgTrial.SList.Values[_Kplus + _cIET];
    TO_:= StrToIntDef(FCfgTrial.SList.Values[_Kplus + _cTO],0);
  end;

  with FKMinus do begin
    Csq:= StrToIntDef(FCfgTrial.SList.Values[_Kminus + _cCsq], 0);
    //Usb:= StrToIntDef(FCfgTrial.SList.Values[_Kminus + _cUsb], 0);
    Msg:= FCfgTrial.SList.Values[_Kminus + _cMsg];
    Res:= FCfgTrial.SList.Values[_Kminus + _cRes];
    Nxt:= FCfgTrial.SList.Values[_Kminus + _cNxt];
    IET := FCfgTrial.SList.Values[_Kminus + _cIET];
    TO_:= StrToIntDef(FCfgTrial.SList.Values[_Kminus + _cTO],0);
  end;

  StartTrial;
end;

procedure TSimpl.SetTimerCsq;
begin
  FTimerCsq.Interval := FTrialInterval;
end;

procedure TSimpl.StartTrial;
var a1 : integer;
begin
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
  FClockThread := TClockThread.Create(False);
  FClockThread.OnTimer := TimerClockTimer;
end;

procedure TSimpl.TimerCsqTimer(Sender: TObject);
begin
  FClockThread.FinishThreadExecution;
  FTimerCsq.Enabled:= False;
  FCanPassTrial := True;
  EndTrial(Sender);
end;

procedure TSimpl.TimerClockTimer(Sender: TObject);
var a1: Integer;
begin
  for a1:= 0 to FNumKeyC-1 do FVetSupportC[a1].Key.SchMan.Clock;
end;

procedure TSimpl.WriteData;  //Dados do Relatório
var Lat_Cmp,Dur_CmpResponse,Dur_Cmp, Res_Cmp, Disp, uDisp: String;
    PosComps, Res_Frq: String;
    a1: Integer;
begin
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

  FData:= PosComps  + #9 +
          Res_Cmp   + #9 +
          Lat_Cmp  + #9 +  //#9
          Dur_CmpResponse + #9 +
          Dur_Cmp + #9 +
          Disp      + #9 +
          uDisp     + #9 +
          Res_Frq;
          //   + #9 +
          //FDataBkGndS;        //obsoleto

  FManager.OnConsequence(Self);
  if Assigned(OnConsequence) then FOnConsequence (Self);
end;

procedure TSimpl.EndCorrection(Sender: TObject);
begin
  if Assigned (OnEndCorrection) then FOnEndCorrection (Sender);
end;

procedure TSimpl.EndTrial(Sender: TObject);
begin
  if FLatCmp = 0 then FLatCmp:= GetTickCount;
  WriteData(Sender);
  if FCanPassTrial then
    if Assigned(OnEndTrial) then FOnEndTrial(sender);
end;

procedure TSimpl.Hit(Sender: TObject);
begin
  if Assigned(OnHit) then FOnHit(Sender);
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
      FResult:= FKPlus.Res;
      FIETConsequence:= FKPlus.IET;
      FTimeOut := FKPlus.TO_;
      Dispenser(FKPlus.Csq, FKPlus.Usb);
    end;

  If (Key = 109) {-} then
    begin
      FDataCMsg:= FKMinus.Msg;
      FDataCsq:= FKMinus.Csq;
      //FDataUsb := FKMinus.Usb;
      FResult:= FKMinus.Res;
      FTimeOut:= FKMinus.TO_;
      FIETConsequence:= FKMinus.IET;

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
  if Assigned(OnMiss) then FOnMiss (Sender);
end;

procedure TSimpl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button,Shift, X, Y);
  FDataTicks:= FDataTicks +
               FormatFloat('####,####',GetTickCount - Ft) + #9 +
               '-' + #9 +
               '-' + #9 +
               IntToStr(X) + #9 + IntToStr(Y) + #13#10 + #9 + #9;
      FDataBkGndI.Plus(1);
    //  FDataBkGndS := FDataBkGndS + IntToStr(X)+ ',' + IntToStr(Y) + #9;          //obsoleto
      FManager.OnBkgndResponse(FDataBkGndI);
      if Assigned(OnBkGndResponse) then FOnBkGndResponse(Self);
end;

procedure TSimpl.None(Sender: TObject);
begin
  if Assigned(OnNone) then FOnMiss (Sender);
end;

procedure TSimpl.BeginCorrection(Sender: TObject);
begin
  if Assigned (OnBeginCorrection) then FOnBeginCorrection (Sender);
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
        if FVetSupportC[TKey(Sender).Tag].Res = T_HIT then FResult := 'HIT';

        if FVetSupportC[TKey(Sender).Tag].Res = T_MISS then FResult := 'MISS';

      end
    else FResult := 'NONE';

      //FDataCsq:= FVetSupportC[TKey(Sender).Tag].Csq;
      //FDataUsb := FVetSupportC[TKey(Sender).Tag].Usb;
      FTimeOut := FVetSupportC[TKey(Sender).Tag].TO_;
      FIETConsequence := FVetSupportC[TKey(Sender).Tag].IET;
      if FVetSupportC[TKey(Sender).Tag].Nxt = T_CRT then FNextTrial := T_CRT
      else FNextTrial := FVetSupportC[TKey(Sender).Tag].Nxt;

    if FVetSupportC[TKey(Sender).Tag].Msg = 'GONOGO' then
      begin       //GONOGO serve apenas para a diferenciação entre Go e NoGo nos esquemas RRRT
         if FFlagCsq2Fired then FDataCMsg := '1'
         else  FDataCMsg := '2';

         if (FFlagCsq2Fired = False) and (FVetSupportC[TKey(Sender).Tag].Res = T_HIT) then
           begin
             FResult := 'MISS';
           end;
         if (FFlagCsq2Fired = False) and (FVetSupportC[TKey(Sender).Tag].Res = T_MISS) then
           begin
             FDataCsq := 0;
             FDataUSB := '0';
             FTimeOut := 0;
             FResult := 'HIT';
           end;

         if FFlagCsq2Fired and (FVetSupportC[TKey(Sender).Tag].Res = T_HIT) then FTimeOut := 0;
         if FFlagCsq2Fired and (FVetSupportC[TKey(Sender).Tag].Res = T_MISS) then
           begin
             FDataCsq := 0;
             FDataUSB := '0';
           end;
      end
    else FDataCMsg:= FVetSupportC[TKey(Sender).Tag].Msg;

      //Dispenser(FDataCsq, FDataUsb);
      if FResult = 'HIT' then  Hit(Sender);
      if FResult = 'MISS' then  Miss(Sender);
      if FResult = 'NONE' then  None(Sender);

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
begin
  if FFlagResp then
    begin
      FDataTicks:=
             FDataTicks +
             FormatFloat('####,####',GetTickCount - Ft) + #9 +
             'C' + IntToStr(TKey(Sender).Tag + 1) + #9 +
             ExtractFileName(TKey(Sender).FullPath) + #9 +
             IntToStr(TKey(Sender).LastResponsePoint[0] + TKey(Sender).Left) + #9 +
             IntToStr(TKey(Sender).LastResponsePoint[1] + TKey(Sender).Top) + #13#10 + #9 + #9;

       if FFirstResp = False then
        begin
          FLatCmp := GetTickCount;
          FFirstResp := True;
        end;

      TKey(Sender).IncCounterResponse;
      if Assigned(FManager.OnStmResponse) then FManager.OnStmResponse (Sender);
      if Assigned(OnStmResponse) then FOnStmResponse (Self);
    end;
end;

end.

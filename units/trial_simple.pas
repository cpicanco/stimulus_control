{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit trial_simple;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils

    , response_key
    , trial_abstract
    //, countermanager
    //, config_session
    //, counter
    , constants
    //, interface_rs232
    //, interface_plp
    ;

type

  { Testing and refactoring required }

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
    BackgroundResponseCount : integer;
    Latency,
    StmBegin,
    StmEnd : Extended;
    BkGndMsg,
    CompMsg,
    Rs232Code : string;
    PLPCode : BYTE;
  end;

  { TSimpl }

  TSimpl = Class(TTrial)
  protected
    FDataSupport : TDataSupport;
    FKMinus : TSupportKey;
    FKPlus : TSupportKey;
    FResponseEnabled : Boolean;
    FConsequenceFired : Boolean;
    FFirstResp : Boolean;
    FNumComp : Integer;
    FTrialInterval : Integer;
    FVetSupportC : array of TSupportKey;
    procedure BeginCorrection (Sender: TObject);
    procedure Consequence(Sender: TObject);
    procedure Dispenser(Csq: Byte; Usb: string);
    procedure EndCorrection(Sender: TObject);
    procedure Hit(Sender: TObject);
    procedure Miss(Sender: TObject);
    procedure None(Sender: TObject);
    procedure Response(Sender: TObject);
    procedure TrialResult(Sender: TObject);
    // TTrial
    procedure BeforeEndTrial(Sender: TObject); override;
    procedure StartTrial(Sender: TObject); override;
    procedure WriteData(Sender: TObject); override;
    // TCustomControl
    procedure TrialKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrialKeyUp(Sender: TObject;var Key: Word; Shift: TShiftState);
    procedure TrialMouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Play(TestMode: Boolean; Correction : Boolean); override;
    //procedure DispenserPlusCall; override;
  end;

implementation

uses timestamps;

constructor TSimpl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnBeforeEndTrial := @BeforeEndTrial;
  OnKeyDown := @TrialKeyDown;
  OnKeyUp := @TrialKeyUp;
  OnMouseDown := @TrialMouseDown;

  Header :=  'Pos.Cmp.' + #9 +        //header do Relatório
             'Res.Cmp.' + #9 +
             'Lat.Cmp.' + #9 +
             'Dur.Cmp.' + #9 +
             'Tmp.Cmp.' + #9 +
             'Disp.   ' + #9 +
             'UDsp.   ' + #9 +
             'Res.Frq.';

  HeaderTicks := '____Time' + #9 +
                 '____Type' + #9 +
                 'FileName' + #9 +
                 '____Left' + #9 +
                 '_____Top';

end;


procedure TSimpl.Dispenser(Csq: Byte; Usb: string);
begin
  //PLP.OutPortOn (Csq);
  //RS232.Dispenser(Usb);
end;

//procedure TSimpl.DispenserPlusCall;
//begin
//  Dispenser(FKPlus.Csq, FKPlus.Usb);
//end;

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
  Color := StrToIntDef(CfgTrial.SList.Values[_BkGnd], Parent.Color);

  if TestMode then Cursor := 0
  else Cursor := StrToIntDef(CfgTrial.SList.Values[_Cursor], Parent.Cursor);

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

          s1 := CfgTrial.SList.Values[_Comp + IntToStr(a1 + 1) + _cBnd] + #32;
          R.Top := StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
          NextSpaceDelimitedParameter;

          R.Left := StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
          NextSpaceDelimitedParameter;

          R.Bottom := StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
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
var I : integer;
begin
  FDataSupport.StmEnd := TickCount;
  FResponseEnabled:= False;

  if Sender is TKey then
    begin
      I := TKey(Sender).Tag;
      //FDataSupport.PLPCode:= FVetSupportC[I].Csq;
      //FDataSupport.RS232Code := FVetSupportC[I].Usb;
      TimeOut := FVetSupportC[I].TO_;
      IETConsequence := FVetSupportC[I].IET;

      if (FVetSupportC[I].Res = T_HIT) or (FVetSupportC[I].Res = T_MISS)  then
        begin
          if FVetSupportC[I].Res = T_HIT then Result := T_HIT;
          if FVetSupportC[I].Res = T_MISS then Result := T_MISS;
        end
      else
        Result := T_NONE;

      if FVetSupportC[I].Nxt = T_CRT then
        NextTrial := T_CRT
      else
        NextTrial := FVetSupportC[I].Nxt;

      if FVetSupportC[I].Msg = 'GONOGO' then
        begin       //GONOGO serve apenas para a diferenciação entre Go e NoGo nos esquemas RRRT
          if FConsequenceFired then
            FDataSupport.CompMsg := 'GO'
          else
            FDataSupport.CompMsg := 'NOGO';

          if (FConsequenceFired = False) and (FVetSupportC[I].Res = T_HIT) then
            Result := 'MISS';

          if (FConsequenceFired = False) and (FVetSupportC[I].Res = T_MISS) then
           begin
             FDataSupport.PLPCode := 0;
             FDataSupport.RS232Code := '0';
             TimeOut := 0;
             Result := T_HIT;
           end;

          if FConsequenceFired and (FVetSupportC[I].Res = T_HIT) then
            TimeOut := 0;

          if FConsequenceFired and (FVetSupportC[I].Res = T_MISS) then
           begin
             FDataSupport.PLPCode := 0;
             FDataSupport.RS232Code := '0';
           end;
        end
      else
        FDataSupport.CompMsg:= FVetSupportC[I].Msg;
    end;

  if Result = T_HIT then
    Hit(Sender);

  if Result = T_MISS then
    Miss(Sender);

  if Result = T_NONE then
    None(Sender);
end;

procedure TSimpl.BeforeEndTrial(Sender: TObject);
begin
  WriteData(Sender);
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

  FDataSupport.StmBegin := TickCount;
  inherited StartTrial(Sender);
end;


procedure TSimpl.WriteData(Sender: TObject);
var
    Latency,
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
  Res_Frq := Res_Frq + 'BK='+ IntToStr(FDataSupport.BackgroundResponseCount);

  for a1:= 0 to FNumComp-1 do
    begin
      if FVetSupportC[a1].Msg = '' then FVetSupportC[a1].Msg:= '-';
      PosComps:= PosComps + FVetSupportC[a1].Msg + #32;
    end;

  if FDataSupport.CompMsg = '' then FDataSupport.CompMsg:= '--------';
  Res_Cmp:= LeftStr(FDataSupport.CompMsg+#32#32#32#32#32#32#32#32, 8);

  if not FFirstResp then
    Latency := FloatToStrF(FDataSupport.Latency - TimeStart,ffFixed,0,9)
  else Latency := #32#32#32#32#32#32 + 'NA';
  Dur_CmpResponse := FloatToStrF(FDataSupport.StmEnd - TimeStart,ffFixed,0,9);
  Dur_Cmp := FloatToStrF(FDataSupport.StmEnd - FDataSupport.StmBegin,ffFixed,0,9);

  //Disp:= RightStr(IntToBin(FDataSupport.PLPCode, 9)+#0, 8);
  //uDisp := IntToStr(FDataSupport.RS232Code);

  Data := PosComps  + #9 +
          Res_Cmp   + #9 +
          Latency  + #9 +  //#9
          Dur_CmpResponse + #9 +
          Dur_Cmp + #9 +
          Disp      + #9 +
          uDisp     + #9 +
          Res_Frq + #9 +
          Data;

  if Assigned(OnWriteTrialData) then OnWriteTrialData(Self);
end;

procedure TSimpl.TrialKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin

  if Key = 27 {ESC} then FResponseEnabled:= False;

  if (ssCtrl in Shift) and (Key = 13) {Enter} then
    begin
      FResponseEnabled := False;
      FDataSupport.CompMsg := 'Cancel';
      Result := T_NONE;
      IETConsequence := T_NONE;
      NextTrial := '0';
      EndTrial(Self);
    end;

  if (ssCtrl in Shift) and (Key = 81) {q} then
    begin
     FResponseEnabled:= False;
     FDataSupport.CompMsg := 'Cancel';
     Result := T_NONE;
     IETConsequence := T_NONE;
     NextTrial := 'END';
     Data := Data + LineEnding + '(Sessão cancelada)' + LineEnding;
     EndTrial(Self);
    end;

end;

procedure TSimpl.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

  if Key = 27 {esc} then FResponseEnabled:= True;

  if (Key = 107) {+} then
    begin
      FDataSupport.CompMsg := FKPlus.Msg;
      FDataSupport.PLPCode := FKPlus.Csq;
      FDataSupport.RS232Code := FKPlus.Usb;
      Result := FKPlus.Res;
      IETConsequence := FKPlus.IET;
      TimeOut := FKPlus.TO_;
      Dispenser(FKPlus.Csq, FKPlus.Usb);
    end;

  if (Key = 109) {-} then
    begin
      FDataSupport.CompMsg := FKMinus.Msg;
      FDataSupport.PLPCode := FKMinus.Csq;
      FDataSupport.RS232Code := FKMinus.Usb;
      Result := FKMinus.Res;
      TimeOut := FKMinus.TO_;
      IETConsequence := FKMinus.IET;

      Dispenser(FKMinus.Csq, FKMinus.Usb);
    end;
end;


procedure TSimpl.TrialMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
    LTickCount : Extended;
    aTime, aCode, aStimulus, aLeft, aTop : string;
begin
  LTickCount := TickCount;

  aTime := FloatToStrF(LTickCount - TimeStart, ffFixed, 0, 9);
  aCode := 'BK';
  aStimulus := '-';
  aLeft := IntToStr(X);
  aTop := IntToStr(Y);
  DataTicks := DataTicks +
    aTime + #9 + aCode + #9 + aStimulus + #9 + aLeft + #9 + aTop + LineEnding;

  Inc(FDataSupport.BackgroundResponseCount);

  CounterManager.OnBkgndResponse(Self);
  if Assigned(OnBkGndResponse) then OnBkGndResponse(Self);
end;

procedure TSimpl.Consequence(Sender: TObject);
begin
  { The Dispenser requires working interfaces. They were not tested in the cross platform scope. }
  Dispenser(FDataSupport.PLPCode, FDataSupport.RS232Code);
  CounterManager.OnConsequence(Sender);
  if Assigned(OnConsequence) then OnConsequence(Sender);
  if FLimitedHold = 0 then
    begin
      // we need TKey as sender, not TThread
      TrialResult(Sender);
      EndTrial(Sender)
    end
  else
    if FConsequenceFired = False then
      FConsequenceFired := True;
end;

procedure TSimpl.Response(Sender: TObject);
var
    LTickCount : Extended;
    aTime, aCode, aStimulus, aLeft, aTop : string;
begin
  LTickCount := TickCount;
  if FResponseEnabled then
    begin
      aTime := FloatToStrF(LTickCount - TimeStart, ffFixed, 0, 9);

      if Sender is TKey then
        begin
          TKey(Sender).IncResponseCount;
          aCode := 'C' + IntToStr(TKey(Sender).Tag + 1);
          aStimulus := ExtractFileName(TKey(Sender).FullPath);
          aLeft := IntToStr(TKey(Sender).LastResponsePoint[0] + TKey(Sender).Left);
          aTop := IntToStr(TKey(Sender).LastResponsePoint[1] + TKey(Sender).Top);
          DataTicks := DataTicks +
            aTime + #9 + aCode + #9 + aStimulus + #9 + aLeft + #9 + aTop + LineEnding;
        end;

      if FFirstResp = False then
        begin
          FDataSupport.Latency := LTickCount;
          FFirstResp := True;
        end;

      if Assigned(CounterManager.OnStmResponse) then CounterManager.OnStmResponse(Sender);
      if Assigned(OnStmResponse) then OnStmResponse (Self);
    end;
end;

procedure TSimpl.EndCorrection(Sender: TObject);
begin
  if Assigned (OnEndCorrection) then OnEndCorrection (Sender);
end;

procedure TSimpl.Hit(Sender: TObject);
begin
  if Assigned(OnHit) then OnHit(Sender);
end;

procedure TSimpl.None(Sender: TObject);
begin
  if Assigned(OnNone) then OnMiss (Sender);
end;

procedure TSimpl.BeginCorrection(Sender: TObject);
begin
  if Assigned (OnBeginCorrection) then OnBeginCorrection (Sender);
end;

procedure TSimpl.Miss(Sender: TObject);
begin
  if Assigned(OnMiss) then OnMiss (Sender);
end;

end.

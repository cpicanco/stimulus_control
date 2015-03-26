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
unit trial_message;

{$MODE Delphi}

interface

uses Controls, Classes, ExtCtrls, SysUtils, StdCtrls,
     Graphics, Forms, LCLIntf, LCLType

     , trial_abstract
     //, countermanager
     ;

type

  TMSG = class(TTrial)
  protected
    FTrialInterval: Integer;
    FFlagResp : Boolean;
    FCanPassTrial : Boolean;
    FTimerCsq: TTimer;
    FMemo: TMemo;
    FMemoPrompt: TLabel;
    Ft : Cardinal;
    FLat: Cardinal;
    procedure Consequence(Sender: TObject);
    procedure EndTrial(Sender: TObject);
    procedure SetTimerCsq;
    procedure TimerCsqTimer(Sender: TObject);
    procedure MemoEnter(Sender: TObject);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MemoClick(Sender: TObject);
    procedure Click; override;
    procedure WriteData(Sender: TObject); override;
    procedure StartTrial(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Play(TestMode: Boolean; Correction : Boolean); override;
    procedure DispenserPlusCall; override;
  end;

implementation

//uses fUnit6;

procedure TMSG.Click;
begin
  inherited Click;
  FLat := GetTickCount;
  DataTicks := DataTicks +
               FormatFloat('####,####',FLat - Ft) + #9 +
               'BkgndClk' + #9 +
               '-' + #9 +
               '-' + #9 +
               '-' + #13#10 + #9 + #9;

  EndTrial(Self);
end;

procedure TMSG.Consequence(Sender: TObject);
begin
If FFlagResp then
    begin
      FFlagResp:= False;
      FLat := GetTickCount;
      if FCanPassTrial then FTimerCsq.Enabled:= True else
        begin
          EndTrial(Sender);
          Ft := GetTickCount;
          FFlagResp:= True;
        end;
    end;
end;

constructor TMSG.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);

  Header := 'Mensagem' + #9 + 'Lat.Cmp.';

  HeaderTicks:= 'Tempo_ms' + #9 +
                 'Cmp.Tipo' + #9 +
                 'FileName' + #9 +
                 'Left....' + #9 +
                 'Top.....';

  FFlagResp := False;

  FTimerCsq:= TTimer.Create(Self);
  with FTimerCsq do
    begin
      Enabled:= False;
      Interval:= 1;
      OnTimer:= TimerCsqTimer;
    end;

  FMemo := TMemo.Create(Self);
  With FMemo do begin
    Text := #0;
    Parent := Self;
    Font.Name := 'TimesNewRoman';
    Font.Color := clWhite;
    ReadOnly := True;
    BorderStyle := bsNone;
    Color := clBlack;
    OnEnter := MemoEnter;
    Alignment := taCenter;
    OnClick := MemoClick;
  end;

  FMemoPrompt := TLabel.Create(Self);
  With FMemoPrompt do begin
    Caption := 'Click com o Mouse ou Pressione  <<Control>> + <<Enter>>  para Avançar';
    Parent := Self;
    Font.Name := 'TimesNewRoman';
    Font.Size := 14;
    OnClick := MemoClick;
  end;


end;

procedure TMSG.DispenserPlusCall;
begin
  //

end;

procedure TMSG.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown (Key, Shift);
  If (ssCtrl in Shift) and (Key = 13) then EndTrial(Self);
end;

procedure TMSG.MemoClick(Sender: TObject);
begin
  FLat := GetTickCount;
  DataTicks:= DataTicks +
               FormatFloat('####,####',FLat - Ft) + #9 +
               'MemoClk' + #9 +
               '-' + #9 +
               '-' + #9 +
               '-' + #13#10 + #9 + #9;

  EndTrial(Self);
end;

procedure TMSG.MemoEnter(Sender: TObject);
begin
  //SetFocus;
end;

procedure TMSG.Play(TestMode: Boolean; Correction : Boolean);
var H: Integer;
begin
  //FIscorrection := Correction; //Messages with corrections were not implemented yet

  NextTrial:= '-1';
  Color:= StrToIntDef(CfgTrial.SList.Values['BkGnd'], 0);
  FCanPassTrial := StrToBoolDef(CfgTrial.SList.Values['AutoNxt'], True);
  if FCanPassTrial = False then
    begin
      FTrialInterval := StrToIntDef(CfgTrial.SList.Values['CustomNxtValue'], 10000);
      SetTimerCsq;
    end;

  If TestMode then Cursor := 0
  else Cursor := StrToIntDef(CfgTrial.SList.Values['Cursor'], 0);

  FMemo.Text := CfgTrial.SList.Values['Msg'];
  FMemo.Width := StrToIntDef(CfgTrial.SList.Values['MsgWidth'], 640);
  FMemo.Font.Size := StrToIntDef(CfgTrial.SList.Values['MsgFontSize'], 28);
  FMemo.Font.Color := StrToIntDef(CfgTrial.SList.Values['MsgFontColor'], clWhite);
  FMemo.Color := StrToIntDef(CfgTrial.SList.Values['BkGnd'], clBlack);

  H:= (FMemo.Lines.Count + 2) * FMemo.Font.Height * -1;
  FMemo.SetBounds((Width-FMemo.Width)div 2, (Height-H)div 2, FMemo.Width, H);

  FMemo.Parent:= Self;
  If Cursor = 0 then FMemo.Cursor:= crArrow
  else FMemo.Cursor:= Cursor;

  FMemoPrompt.Visible:= StrToBoolDef(CfgTrial.SList.Values['Prompt'], False);
  If FMemoPrompt.Visible then begin
    FMemoPrompt.SetBounds((Width-FMemoPrompt.Width)div 2, (Height-FMemoPrompt.Height)-20, FMemoPrompt.Width, FMemoPrompt.Height);
    FMemoPrompt.Font.Color:= FMemo.Font.Color;
  end;

  StartTrial(Self);
end;

procedure TMSG.SetTimerCsq;
begin
  FTimerCsq.Interval := FTrialInterval;
end;

procedure TMSG.StartTrial(Sender: TObject);
begin
  inherited StartTrial(Sender);
  if FCanPassTrial then FTimerCsq.Enabled:= False else FTimerCsq.Enabled:= True;
  FFlagResp:= True;
  Ft := GetTickCount;
end;

procedure TMSG.TimerCsqTimer(Sender: TObject);
begin
  FTimerCsq.Enabled:= False;
  FLat := GetTickCount;
  DataTicks:= DataTicks +
               FormatFloat('####,####',FLat - Ft) + #9 +
               'TimeCsq' + #9 +
               '-' + #9 +
               '-' + #9 +
               '-' + #13#10 + #9 + #9;
  FCanPassTrial := True;
  EndTrial(Self);
end;

procedure TMSG.WriteData(Sender: TObject);
var Lat_Cmp : string;
begin
  Lat_Cmp := FormatFloat('#####,###',FLat - Ft) + #9 + 'ms';
  Data := FMemo.Lines.Text + #9 + Lat_Cmp;
  if Assigned(OnWriteTrialData) then OnWriteTrialData(Sender);
end;

procedure TMSG.EndTrial(Sender: TObject);
begin
  WriteData(Sender);
  if FCanPassTrial then
    if Assigned(OnEndTrial) then OnEndTrial(Sender);
end;

end.

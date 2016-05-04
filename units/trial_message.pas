//
// Validation Project (PCRF) - Stimulus Control App
// Copyright (C) 2014-2016,  Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.
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

{$mode objfpc}{$H+}

interface

uses  LCLIntf, LCLType, Controls,
      Classes, SysUtils, StdCtrls

     , trial_abstract
     , constants
     , timestamp
     //, countermanager
     ;

type

  TDataSupport = record
    //Latency,
    //Responses,
    TrialBegin,
    TrialEnd : Extended;
  end;

  { TMSG }

  TMSG = class(TTrial)
  protected
    FDataSupport : TDataSupport;
    FResponseEnabled : Boolean;
    FMemo : TMemo;
    FMemoPrompt : TLabel;
    //procedure Click; override;
    //procedure MemoEnter(Sender: TObject);
    procedure EndTrial(Sender: TObject);
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MemoClick(Sender: TObject);
    procedure StartTrial(Sender: TObject); override;
    procedure ThreadClock(Sender: TObject); override;
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Play(TestMode: Boolean; Correction : Boolean); override;
    procedure DispenserPlusCall; override;
  end;

resourcestring

  MessageMemoPrompt1 = 'Pressione  o botão  para avançar.';

implementation

constructor TMSG.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Header := 'Message' + #9 + '___Start' + #9 + 'Duration';

  FMemo := TMemo.Create(Self);
  with FMemo do begin
    Alignment := taCenter;
    AutoSize := True;
    BorderStyle := bsNone;
    Font.Name := 'TimesNewRoman';
    ReadOnly := True;
    Cursor := -1;
    //Text := #0;
    //Font.Color := clWhite;
    //Color := clBlack;
    //OnEnter := MemoEnter;
    OnClick := @MemoClick;
    Parent := Self;
  end;

  FMemoPrompt := TLabel.Create(Self);
  with FMemoPrompt do begin
    Caption := MessageMemoPrompt1;
    Font.Name := 'TimesNewRoman';
    Font.Size := 14;
    OnClick := @MemoClick;
    Parent := Self;
  end;

  Result := T_NONE;
  IETConsequence := T_NONE;
  Result := T_NONE;
end;

procedure TMSG.MemoClick(Sender: TObject);
begin
  if FResponseEnabled then
    if not (FLimitedHold > 0) then
      begin
        FDataSupport.TrialEnd := GetCustomTick;
        WriteData(Self);
        EndTrial(Sender);
      end;
end;

procedure TMSG.KeyUp(var Key: Word; Shift: TShiftState);
begin
  //inherited KeyUp(Key, Shift);
  if FResponseEnabled then
    if not (FLimitedHold > 0) then
      if (not (ssCtrl in Shift)) and (Key = 32) then
        begin
          FDataSupport.TrialEnd := GetCustomTick;
          WriteData(Self);
          EndTrial(Self);
        end;
end;

procedure TMSG.Play(TestMode: Boolean; Correction : Boolean);
var
  AHeight,
  AFontColor : Integer;
begin
  //FIscorrection := Correction; //Messages with corrections were not implemented yet

  // TCustomControl
  Color := StrToIntDef(CfgTrial.SList.Values[_BkGnd], $FFFFFF);
  AFontColor :=  StrToIntDef(CfgTrial.SList.Values[_MsgFontColor], $000000);

  if TestMode then Cursor := 0
  else Cursor := StrToIntDef(CfgTrial.SList.Values[_Cursor], 0);

  // TTrial
  NextTrial := CfgTrial.SList.Values[_NextTrial];
  FLimitedHold := StrToIntDef(CfgTrial.SList.Values[_LimitedHold], 0);

  // Self TMemo
  with FMemo do
    begin
      Cursor := Self.Cursor;
      Text := CfgTrial.SList.Values[_Msg];
      Width := StrToIntDef(CfgTrial.SList.Values[_MsgWidth], 640);
      Font.Size := StrToIntDef(CfgTrial.SList.Values[_MsgFontSize], 22);
      Font.Color := AFontColor;
      Color := StrToIntDef(CfgTrial.SList.Values[_BkGnd], Self.Color);

      AHeight := (Lines.Count + 2) * Font.Height * -1;
      SetBounds((Self.Width - Width) div 2, (Self.Height - AHeight) div 2, Width, AHeight);

      //Parent := Self;
    end;

  // Self TLabel
  FMemoPrompt.Visible := StrToBoolDef(CfgTrial.SList.Values[_Prompt], False);
  with FMemoPrompt do
    if Visible then
      begin
        SetBounds((Self.Width - Width) div 2, (Self.Height - Height) - 20, Width, Height);
        Font.Color:= AFontColor;
      end;

  StartTrial(Self);
end;

procedure TMSG.DispenserPlusCall;
begin
  // dispensers were not implemented yet
end;

procedure TMSG.StartTrial(Sender: TObject);
begin
  //Invalidate;
  FResponseEnabled := True;
  FDataSupport.TrialBegin := GetCustomTick;
  inherited StartTrial(Sender);
end;

procedure TMSG.WriteData(Sender: TObject);
var aStart, aDuration : string;
begin
  aStart := FormatFloat('00000000;;00000000', FDataSupport.TrialBegin - TimeStart);;
  aDuration := FormatFloat('00000000;;00000000', FDataSupport.TrialEnd - TimeStart);

  Data := 'FMemo.Lines.Text' + #9 + aStart + #9 + aDuration + Data;
  if Assigned(OnWriteTrialData) then OnWriteTrialData(Sender);
end;

procedure TMSG.EndTrial(Sender: TObject);
begin
  FResponseEnabled := False;


  //if Result = T_NONE then None(Sender);

  if Assigned(OnEndTrial) then OnEndTrial(Sender);
end;

procedure TMSG.ThreadClock(Sender: TObject);
begin
  if FResponseEnabled then
    begin
      FDataSupport.TrialEnd := GetCustomTick;
      WriteData(Sender);
      EndTrial(Sender);
    end;
end;

end.

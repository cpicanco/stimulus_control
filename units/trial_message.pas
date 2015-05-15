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

uses  LCLIntf, LCLType, Controls,
      Classes, SysUtils, StdCtrls

     , trial_abstract
     , constants
     //, countermanager
     ;

type

  TDataSupport = record
    //Latency : cardinal;
    //Responses : integer;
    TrialBegin : cardinal;
    TrialEnd : cardinal;
  end;

  { TMSG }

  TMSG = class(TTrial)
  protected
    FDataSupport : TDataSupport;
    FResponseEnabled : Boolean;
    FMemo : TMemo;
    FMemoPrompt : TLabel;
    procedure EndTrial(Sender: TObject);
    procedure ThreadClock(Sender: TObject); override;
    //procedure MemoEnter(Sender: TObject);
    procedure MemoClick(Sender: TObject);
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    //procedure Click; override;
    procedure WriteData(Sender: TObject); override;
    procedure StartTrial(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Play(TestMode: Boolean; Correction : Boolean); override;
  end;

resourcestring

  MessageMemoPrompt1 = 'Pressione  o botão  para avançar.';

implementation

constructor TMSG.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);

  Header := 'Message' + #9 + '___Start' + #9 + 'Duration';

  FMemo := TMemo.Create(Self);
  with FMemo do begin
    Alignment := taCenter;
    BorderStyle := bsNone;
    Font.Name := 'TimesNewRoman';
    Parent := Self;
    ReadOnly := True;
    Cursor := -1;
    //Text := #0;
    //Font.Color := clWhite;
    //Color := clBlack;
    //OnEnter := MemoEnter;
    OnClick := MemoClick;
  end;

  FMemoPrompt := TLabel.Create(Self);
  with FMemoPrompt do begin
    Caption := MessageMemoPrompt1;
    Font.Name := 'TimesNewRoman';
    Font.Size := 14;
    Parent := Self;
    OnClick := MemoClick;
  end;

  Result := T_NONE;
  IETConsequence := T_NONE;
  Result := T_NONE;
end;

procedure TMSG.MemoClick(Sender: TObject);
begin
  if not (FLimitedHold > 0) then EndTrial(Sender);
end;

procedure TMSG.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if not (FLimitedHold > 0) then
    if (scCtrl in Shift) and (Key = 32) then EndTrial(Self);
end;

procedure TMSG.Play(TestMode: Boolean; Correction : Boolean);
var H : Integer;
begin
  //FIscorrection := Correction; //Messages with corrections were not implemented yet

  // TCustomControl
  Color := StrToIntDef(CfgTrial.SList.Values[_BkGnd], $FFFFFF);

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
      Font.Color := StrToIntDef(CfgTrial.SList.Values[_MsgFontColor], $000000);
      Color := StrToIntDef(CfgTrial.SList.Values[_BkGnd], Self.Color);

      H := (Lines.Count + 2) * Font.Height * -1;
      SetBounds((Self.Width - Width) div 2, (Self.Height - H) div 2, Width, H);

      Parent := Self;
    end;

  // Self TLabel
  FMemoPrompt.Visible := StrToBoolDef(CfgTrial.SList.Values[_Prompt], False);
  with FMemoPrompt do
    if Visible then
      begin
        SetBounds((Self.Width - Width) div 2, (Self.Height - Height) - 20, Width, Height);
        Font.Color:= FMemo.Font.Color;
      end;

  StartTrial(Self);
end;

procedure TMSG.StartTrial(Sender: TObject);
begin
  FResponseEnabled := True;
  Invalidate;
  FDataSupport.TrialBegin := GetTickCount;
  inherited StartTrial(Sender);
end;

procedure TMSG.WriteData(Sender: TObject);
var aStart, aDuration : string;
begin
  aStart := FormatFloat('00000000;;00000000', FDataSupport.TrialBegin - TimeStart);;
  aDuration := FormatFloat('00000000;;00000000', FDataSupport.TrialEnd - TimeStart);

  Data := FMemo.Lines.Text + #9 + aStart + #9 + aDuration + Data;
  if Assigned(OnWriteTrialData) then OnWriteTrialData(Sender);
end;

procedure TMSG.EndTrial(Sender: TObject);
begin
  FResponseEnabled := False;
  Hide;
  FDataSupport.TrialEnd := GetTickCount;

  WriteData(Sender);
  //if Result = T_NONE then None(Sender);

  if Assigned(OnEndTrial) then OnEndTrial(Sender);
end;

procedure TMSG.ThreadClock(Sender: TObject);
begin
  EndTrial(Sender);
end;

end.

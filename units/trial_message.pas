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
      Classes, SysUtils, StdCtrls, Graphics

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
    FMessage,
    FMessagePrompt : TLabel;
    //procedure Click; override;
    procedure EndTrial(Sender: TObject); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MessageMouseUp(Sender: TObject;Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure StartTrial(Sender: TObject); override;
    procedure ThreadClock(Sender: TObject); override;
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Play(TestMode: Boolean; Correction : Boolean); override;
    procedure DispenserPlusCall; override;
  end;

resourcestring

  MessagePrompt1 = 'Pressione  o botão  para avançar.';

implementation

constructor TMSG.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Header := 'Message' + #9 + '___Start' + #9 + 'Duration';

  FMessage := TLabel.Create(Self);
  with FMessage do begin
    Cursor := -1;
    Alignment := taCenter;
    Anchors := [akLeft,akRight];
    //Layout := tlCenter;
    WordWrap := True;
    Font.Name := 'TimesNewRoman';
    OnMouseUp := @MessageMouseUp;
    Parent := Self;
  end;

  FMessagePrompt := TLabel.Create(Self);
  with FMessagePrompt do begin
    Cursor := -1;
    Caption := MessagePrompt1;
    Font.Name := 'TimesNewRoman';
    Font.Size := 14;
    OnMouseUp := @MessageMouseUp;
    Parent := Self;
  end;

  Result := T_NONE;
  IETConsequence := T_NONE;
  Result := T_NONE;
end;

procedure TMSG.MessageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FResponseEnabled then
    if FLimitedHold = 0 then
      EndTrial(Self);

end;

procedure TMSG.KeyUp(var Key: Word; Shift: TShiftState);
begin
  //inherited KeyUp(Key, Shift);
  if FResponseEnabled then
    if FLimitedHold = 0 then
      if (not (ssCtrl in Shift)) and (Key = 32) then
          EndTrial(Self);
end;

procedure TMSG.Play(TestMode: Boolean; Correction : Boolean);
var
  LFontColor : Integer;
begin
  //FIscorrection := Correction; //Messages with corrections were not implemented yet

  // TCustomControl
  Color := StrToIntDef(CfgTrial.SList.Values[_BkGnd], $FFFFFF);
  LFontColor :=  StrToIntDef(CfgTrial.SList.Values[_MsgFontColor], $000000);

  if TestMode then Cursor := 0
  else Cursor := StrToIntDef(CfgTrial.SList.Values[_Cursor], 0);

  // TTrial
  NextTrial := CfgTrial.SList.Values[_NextTrial];
  FLimitedHold := StrToIntDef(CfgTrial.SList.Values[_LimitedHold], 0);

  with FMessage do
    begin
      Cursor := Self.Cursor;
      Caption := CfgTrial.SList.Values[_Msg];
      Width := StrToIntDef(CfgTrial.SList.Values[_MsgWidth], 640);
      Font.Size := StrToIntDef(CfgTrial.SList.Values[_MsgFontSize], 22);
      Font.Color := LFontColor;
      Color := StrToIntDef(CfgTrial.SList.Values[_BkGnd], Self.Color);
      SetBounds((Self.Width - Width) div 2, (Self.Height - Height) div 2, Width, Height);
    end;

  FMessagePrompt.Visible := StrToBoolDef(CfgTrial.SList.Values[_Prompt], False);
  with FMessagePrompt do
    if Visible then
      begin
        Font.Color:= LFontColor;
        SetBounds((Self.Width - Width) div 2, (Self.Height - Height) - 20, Width, Height);
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
  aStart := FloatToStrF(FDataSupport.TrialBegin - TimeStart, ffFixed, 0, 9);
  aDuration := FloatToStrF(FDataSupport.TrialEnd - TimeStart, ffFixed, 0, 9);

  Data := FMessage.Caption + #9 + aStart + #9 + aDuration + Data;
  if Assigned(OnWriteTrialData) then OnWriteTrialData(Sender);
end;

procedure TMSG.EndTrial(Sender: TObject);
begin
  Hide;
  FDataSupport.TrialEnd := GetCustomTick;
  WriteData(Self);
  inherited EndTrial(Sender);
end;

procedure TMSG.ThreadClock(Sender: TObject);
begin
  if FResponseEnabled then
    begin
      Hide;
      WriteData(Self);
      FResponseEnabled := False;
      if Assigned(OnEndTrial) then OnEndTrial(Sender);
    end;
end;

end.

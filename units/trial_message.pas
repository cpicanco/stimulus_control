{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit trial_message;

{$mode objfpc}{$H+}

interface

uses  LCLIntf, LCLType, Controls,
      Classes, SysUtils, StdCtrls, Graphics

     , trial_abstract
     , constants
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
  private
    FDataSupport : TDataSupport;
    FResponseEnabled : Boolean;
    FMessagePrompt,
    FMessage : TLabel;
    procedure MessageMouseUp(Sender: TObject;Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure BeforeEndTrial(Sender: TObject); override;
    procedure StartTrial(Sender: TObject); override;
    procedure WriteData(Sender: TObject); override;
    //procedure ThreadClock(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Play(TestMode: Boolean; Correction : Boolean); override;
  end;

resourcestring

  MessagePrompt1 = 'Pressione  o botão  para avançar.';

implementation

uses
  timestamps
  {$ifdef DEBUG}
  , debug_logger
  {$endif}
  ;

constructor TMSG.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnKeyUp := @TrialKeyUp;
  OnBeforeEndTrial := @BeforeEndTrial;

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

  Header := '___Start' + #9 + 'Duration' + #9 + 'Message';
  Result := T_NONE;
  IETConsequence := T_NONE;
  Result := T_NONE;
end;

procedure TMSG.MessageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FResponseEnabled then
    if FLimitedHold = 0 then
      EndTrial(Sender);
end;

procedure TMSG.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FResponseEnabled then
    if FLimitedHold = 0 then
      if (not (ssCtrl in Shift)) and (Key = 32) then
          EndTrial(Sender);

  {$ifdef DEBUG}
    DebugLn(mt_Debug +  'TMSG.KeyUp:'+ GetTimeStampF);
  {$endif}
end;

procedure TMSG.BeforeEndTrial(Sender: TObject);
begin
  {$ifdef DEBUG}
    DebugLn(mt_Debug + 'TMSG.BeforeEndTrial:'+ TObject(Sender).ClassName);
  {$endif}
  FDataSupport.TrialEnd := TickCount;
  FResponseEnabled := False;
  WriteData(Self);
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

procedure TMSG.StartTrial(Sender: TObject);
begin
  FResponseEnabled := True;
  FDataSupport.TrialBegin := TickCount;
  inherited StartTrial(Sender);
end;


procedure TMSG.WriteData(Sender: TObject);
var aStart, aDuration : string;
begin
  aStart := FloatToStrF(FDataSupport.TrialBegin - TimeStart, ffFixed, 0, 9);
  aDuration := FloatToStrF(FDataSupport.TrialEnd - TimeStart, ffFixed, 0, 9);

  Data := aStart + #9 + aDuration + #9 + FMessage.Caption + Data;
  if Assigned(OnWriteTrialData) then OnWriteTrialData(Sender);
end;

end.

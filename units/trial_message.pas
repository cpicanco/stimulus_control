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
    FMessagePrompt,
    FMessage : TLabel;
    procedure MessageMouseUp(Sender: TObject;Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrialStart(Sender: TObject);
  protected
    procedure BeforeEndTrial(Sender: TObject); override;
    procedure WriteData(Sender: TObject); override;
    procedure Paint; override;
    //procedure ThreadClock(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Play(ACorrection : Boolean); override;
  end;

resourcestring

  MessagePrompt1 = 'Pressione  o botão  para avançar.';

implementation

uses
  constants, timestamps
  {$ifdef DEBUG}
  , debug_logger
  {$endif}
  ;

constructor TMSG.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnBeforeEndTrial := @BeforeEndTrial;
  OnTrialKeyUp := @TrialKeyUp;
  OnTrialStart := @TrialStart;

  FMessage := TLabel.Create(Self);
  with FMessage do begin
    Visible := False;
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
    Visible := False;
    Cursor := -1;
    Caption := MessagePrompt1;
    Font.Name := 'TimesNewRoman';
    Font.Size := 14;
    OnMouseUp := @MessageMouseUp;
    Parent := Self;
  end;

  Header := Header +
            '___Start' + #9 +
            'Duration' + #9 +
            'Message';

  Result := T_NONE;
  IETConsequence := T_NONE;
  Result := T_NONE;
end;

procedure TMSG.MessageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FResponseEnabled then
    EndTrial(Sender);
end;

procedure TMSG.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FResponseEnabled then
    if (not (ssCtrl in Shift)) and (Key = 32) then
      EndTrial(Sender);
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

procedure TMSG.Play(ACorrection : Boolean);
var
  LFontColor : Integer;
begin
  inherited Play(ACorrection);
  LFontColor :=  StrToIntDef(CfgTrial.SList.Values[_MsgFontColor], $000000);

  with FMessage do
    begin
      Cursor := Self.Cursor;
      Caption := CfgTrial.SList.Values[_Msg];
      Width := StrToIntDef(CfgTrial.SList.Values[_MsgWidth], 640);
      Font.Size := StrToIntDef(CfgTrial.SList.Values[_MsgFontSize], 22);
      Font.Color := LFontColor;
    end;

  with FMessagePrompt do
    begin
      Cursor := Self.Cursor;
      Font.Color:= LFontColor;
      Enabled := StrToBoolDef(CfgTrial.SList.Values[_Prompt], False);
    end;

  Config(Self);
end;

procedure TMSG.TrialStart(Sender: TObject);
begin
  with FMessage do
    begin
      Visible := True;
      SetBounds((Self.Width - Width) div 2, (Self.Height - Height) div 2, Width, Height);
    end;

  if FMessagePrompt.Enabled then
    with FMessagePrompt do
      begin
        Visible := True;
        SetBounds((Self.Width - Width) div 2, (Self.Height - Height) - 20, Width, Height);
      end;
  FDataSupport.TrialBegin := TickCount;
end;

procedure TMSG.WriteData(Sender: TObject);
var aStart, aDuration : string;
begin
  inherited WriteData(Sender);
  aStart := TimestampToStr(FDataSupport.TrialBegin - TimeStart);
  aDuration := TimestampToStr(FDataSupport.TrialEnd - TimeStart);

  Data := Data + aStart + #9 + aDuration + #9 + FMessage.Caption;
  if Assigned(OnWriteTrialData) then OnWriteTrialData(Sender);
end;

procedure TMSG.Paint;
begin
  inherited Paint;
end;

end.

{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

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
     , bass_player
     ;

type

  TDataSupport = record
    //Latency,
    //Responses,
    TrialBegin,
    TrialEnd : Extended;
  end;

  { TMSG }

  TMessageStyle  = (msgDefault,msgAudio);

  TMessageTrial = class(TTrial)
  private
    FAudio : TBassStream;
    FAudioPlaying : Boolean;
    FMessageStyle : TMessageStyle;
    FDataSupport : TDataSupport;
    FMessagePrompt,
    FMessage : TLabel;
    procedure MessageMouseUp(Sender: TObject;Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure TrialBeforeEnd(Sender: TObject);
    procedure TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrialStart(Sender: TObject);
  protected
    procedure WriteData(Sender: TObject); override;
    //procedure ThreadClock(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    procedure Play(ACorrection : Boolean); override;
  end;

resourcestring

  MessagePrompt1 = 'Pressione  o botão  para avançar.';

implementation

uses
  constants
  , timestamps
  , bass_player_callbacks
  {$ifdef DEBUG}
  , debug_logger
  {$endif}
  ;

constructor TMessageTrial.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
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
    Parent := TCustomControl(AOwner);
  end;

  FMessagePrompt := TLabel.Create(Self);
  with FMessagePrompt do begin
    Visible := False;
    Cursor := -1;
    Caption := MessagePrompt1;
    Font.Name := 'TimesNewRoman';
    Font.Size := 14;
    OnMouseUp := @MessageMouseUp;
    Parent := TCustomControl(AOwner);
  end;

  Header := Header +
            '___Start' + #9 +
            'Duration' + #9 +
            'Message';

  Result := T_NONE;
  IETConsequence := T_NONE;
  Result := T_NONE;

  GMessageTrialAudioWasPlayed := False;
  FAudioPlaying := False;
  FAudio := nil;
end;

destructor TMessageTrial.Destroy;
begin
  if Assigned(FAudio) then
    FAudio.Free;
  inherited Destroy;
end;

procedure TMessageTrial.MessageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    EndTrial(Sender);
end;

procedure TMessageTrial.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 32 then
    case FMessageStyle of
      msgDefault:
        EndTrial(Sender);
      msgAudio:
        if GMessageTrialAudioWasPlayed then
          EndTrial(Sender)
        else
          if not FAudioPlaying then
            begin
              FAudioPlaying:=True;
              FAudio.Play;
            end;
    end;
end;

procedure TMessageTrial.TrialBeforeEnd(Sender: TObject);
begin
  {$ifdef DEBUG}
    DebugLn(mt_Debug + 'TMSG.BeforeEndTrial:'+ TObject(Sender).ClassName);
  {$endif}
  FDataSupport.TrialEnd := TickCount;
  WriteData(Self);
end;

procedure TMessageTrial.Play(ACorrection : Boolean);
var
  LFontColor : Integer;
  LAudioFile : string;
begin
  inherited Play(ACorrection);
  LFontColor :=  StrToIntDef(CfgTrial.SList.Values[_MsgFontColor], $000000);
  FMessageStyle := TMessageStyle(Ord(StrToIntDef(CfgTrial.SList.Values[_Style], 0)));

  if FMessageStyle = msgAudio then
    begin
      LAudioFile := CfgTrial.SList.Values[_cStm];
      case LAudioFile of
        T_HIT  : LAudioFile := RootMedia+'CSQ1.wav';
        T_MISS : LAudioFile := RootMedia+'CSQ2.wav';
        else
          LAudioFile := RootMedia+LAudioFile;
      end;

      if FileExists(LAudioFile) then
        begin
          FAudio := TBassStream.Create(LAudioFile);
          FAudio.SyncProcedure := @EndOfMessageTrialAudio;
        end
      else
        raise Exception.Create('File does not exist: '+LAudioFile);
    end;

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

  if Self.ClassType = TMessageTrial then Config(Self);
end;

procedure TMessageTrial.TrialStart(Sender: TObject);
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

procedure TMessageTrial.WriteData(Sender: TObject);
var aStart, aDuration : string;
begin
  inherited WriteData(Sender);
  aStart := TimestampToStr(FDataSupport.TrialBegin - TimeStart);
  aDuration := TimestampToStr(FDataSupport.TrialEnd - TimeStart);

  Data := Data + aStart + #9 + aDuration + #9 + FMessage.Caption;
  if Assigned(OnTrialWriteData) then OnTrialWriteData(Sender);
end;

end.

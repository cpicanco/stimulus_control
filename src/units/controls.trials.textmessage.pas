{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.TextMessage;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils, StdCtrls, Graphics

  , Controls.Trials.Abstract
  , Controls.Trials.Helpers
  {$IFDEF AUDIO}, Audio.Bass_nonfree {$ENDIF}
  ;

type

  { TMSG }

  TMessageStyle  = (msgDefault,msgAudio);

  { TMessageTrial }

  TMessageTrial = class(TTrial)
  private
    {$IFDEF AUDIO}FAudio : TBassStream;{$ENDIF}
    FAudioPlaying : Boolean;
    FMessageStyle : TMessageStyle;
    FDataSupport : TDataSupport;
    FMessage : TLabel;
    //procedure MessageMouseUp(Sender: TObject;Button: TMouseButton; Shift:TShiftState; X,Y:Integer);
    procedure TrialBeforeEnd(Sender: TObject);
    procedure TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrialStart(Sender: TObject);
  protected
    procedure WriteData(Sender: TObject); override;
    //procedure ThreadClock(Sender: TObject); override;
  public
    constructor Create(AOwner: TCustomControl); override;
    destructor Destroy;override;
    function ConsequenceInterval: integer; override;
    function AsString : string; override;
    procedure Play(ACorrection : Boolean); override;
  end;

resourcestring

  MessagePrompt1 = 'Pressione  o botão  para avançar.';

implementation

uses
  constants
  , Timestamps
  {$IFDEF AUDIO}, Audio.BassCallbacks{$ENDIF}
  {$ifdef DEBUG}
  , Loggers.Debug
  {$endif}
  ;

constructor TMessageTrial.Create(AOwner: TCustomControl);
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
    Font.Name := 'Times New Roman';
    //OnMouseUp := @MessageMouseUp;
    Parent := TCustomControl(AOwner);
  end;

  Header := Header + #9 +
            rsReportStmBeg + #9 +
            rsReportStmDur + #9 +
            rsReportMsgTxt;

  Result := T_NONE;
  IETConsequence := T_NONE;
  Result := T_NONE;

  {$IFDEF AUDIO}GMessageTrialAudioWasPlayed := False;{$ENDIF}
  FAudioPlaying := False;
  {$IFDEF AUDIO}FAudio := nil;{$ENDIF}
end;

destructor TMessageTrial.Destroy;
begin
  {$IFDEF AUDIO}
  if Assigned(FAudio) then
    FAudio.Free;
  {$ENDIF}
  inherited Destroy;
end;

function TMessageTrial.ConsequenceInterval: integer;
begin
  Result:=0;
end;

function TMessageTrial.AsString: string;
begin
  Result := '';
end;

//
//procedure TMessageTrial.MessageMouseUp(Sender: TObject; Button: TMouseButton;
//  Shift: TShiftState; X, Y: Integer);
//begin
//  if Button = mbLeft then
//    EndTrial(Sender);
//end;

procedure TMessageTrial.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 32 then
    case FMessageStyle of
      msgDefault:
        EndTrial(Sender);
      {$IFDEF AUDIO}
      msgAudio:
        if GMessageTrialAudioWasPlayed then
          EndTrial(Sender)
        else
          if not FAudioPlaying then
            begin
              FAudioPlaying:=True;
              FAudio.Play;
            end;
      {$ELSE}
        msgAudio : { do nothing };
      {$ENDIF}
    end;
end;

procedure TMessageTrial.TrialBeforeEnd(Sender: TObject);
begin
  {$ifdef DEBUG}
    DebugLn(mt_Debug + 'TMSG.BeforeEndTrial:'+ TObject(Sender).ClassName);
  {$endif}
  FDataSupport.StmEnd := TickCount;
  WriteData(Self);
end;

procedure TMessageTrial.Play(ACorrection : Boolean);
var
  LFontColor : Integer;
  {$IFDEF AUDIO}LAudioFile : string;{$ENDIF}
  LParameters : TStringList;
begin
  inherited Play(ACorrection);
  LParameters := Configurations.Parameters;
  LFontColor :=  StrToIntDef(LParameters.Values[_MsgFontColor], $000000);
  FMessageStyle := TMessageStyle(Ord(StrToIntDef(LParameters.Values[_Style], 0)));
  {$IFDEF AUDIO}
  if FMessageStyle = msgAudio then
    begin
      LAudioFile := LParameters.Values[_cStm];
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
  {$ENDIF}

  with FMessage do
    begin
      Cursor := Self.Cursor;
      Caption := LParameters.Values[_Msg];
      Width := StrToIntDef(LParameters.Values[_MsgWidth], 640);
      Font.Size := StrToIntDef(LParameters.Values[_MsgFontSize], 22);
      Font.Color := LFontColor;
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

  FDataSupport.StmBegin := TickCount;
end;

procedure TMessageTrial.WriteData(Sender: TObject);
var aStart, aDuration : string;
begin
  inherited WriteData(Sender);
  aStart := TimestampToStr(FDataSupport.StmBegin - TimeStart);
  aDuration := TimestampToStr(FDataSupport.StmEnd - TimeStart);

  Data := Data + aStart + #9 + aDuration + #9 + FMessage.Caption;
end;

end.

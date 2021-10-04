{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.RTFMessage;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils, Graphics

  , Stimuli.NextButton
  , Controls.CustomRichMemo
  , Controls.Trials.Abstract
  , Controls.Trials.Helpers
  ;

type

  { TRTFMessage }

  TRTFMessage = class(TTrial)
    procedure EndButtonClick(Sender : TObject);
  private
    FStimulus : TNextButton;
    FDataSupport : TDataSupport;
    FMessage : TARichMemo;
    procedure TrialBeforeEnd(Sender: TObject);
    procedure TrialStart(Sender: TObject);
  protected
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TCustomControl); override;
    destructor Destroy;override;
    function ConsequenceInterval: integer; override;
    function AsString : string; override;
    procedure Play(ACorrection : Boolean); override;
  end;

implementation

uses Constants, Timestamps, Cheats;

constructor TRTFMessage.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  //OnTrialKeyUp := @TrialKeyUp;
  OnTrialStart := @TrialStart;

  FMessage := TARichMemo.Create(Self);
  //FMessage.Visible := False;
  with FMessage do begin
    Height := (Self.Height div 3) * 2;
    Width := (Self.Width div 8) * 5;
    Left := (Self.Width - Width) div 2;
    Top := (Self.Height - Height) div 2;
    Parent := TCustomControl(AOwner);
  end;

  FStimulus := TNextButton.Create(Self);
  with FStimulus do begin
   OnClick := @EndButtonClick;
   Sibling := FMessage;
  end;

  Header := Header + #9 +
            rsReportStmBeg + #9 +
            rsReportStmDur;

  Result := T_NONE;
  IETConsequence := T_NONE;
  Result := T_NONE;
end;

destructor TRTFMessage.Destroy;
begin
  inherited Destroy;
end;

function TRTFMessage.ConsequenceInterval: integer;
begin
  Result:=0;
end;

function TRTFMessage.AsString: string;
begin
  Result := '';
end;

procedure TRTFMessage.EndButtonClick(Sender: TObject);
begin
  EndTrial(Sender);
end;

procedure TRTFMessage.TrialBeforeEnd(Sender: TObject);
begin
  FDataSupport.StmEnd := TickCount;
  WriteData(Self);
end;


procedure TRTFMessage.Play(ACorrection : Boolean);
var
  LRtfStream : TStringStream;
begin
  inherited Play(ACorrection);
  LRtfStream := TStringStream.Create;
  try
    LRtfStream.LoadFromFile(Configurations.Parameters.Values[_Msg]);
    FMessage.LoadRichText(LRtfStream);
  finally
    LRtfStream.Free;
  end;
  if Self.ClassType = TRTFMessage then Config(Self);
end;

procedure TRTFMessage.TrialStart(Sender: TObject);
begin
  FStimulus.Start;
  FMessage.Visible := True;
  Self.SetFocus;
  FDataSupport.StmBegin := TickCount;

  if CheatsModeOn then begin
    ParticipantBot.Start(FStimulus.AsInterface);
  end;
end;

procedure TRTFMessage.WriteData(Sender: TObject);
var aStart, aDuration : string;
begin
  inherited WriteData(Sender);
  aStart := TimestampToStr(FDataSupport.StmBegin - TimeStart);
  aDuration := TimestampToStr(FDataSupport.StmEnd - TimeStart);

  Data := Data + aStart + #9 + aDuration;
end;

end.

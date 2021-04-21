{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit controls.trials.HTMLMessage;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils, StdCtrls, Graphics, IpHtml

  , Controls.Trials.Abstract
  , Controls.Trials.Helpers
  ;

type

  { TMSG }

  TMessageStyle  = (msgDefault,msgAudio);

  { TMessageTrial }

  THTMLMessage = class(TTrial)
  private
    FDataSupport : TDataSupport;
    FMessage : TIpHtmlPanel;
    procedure TrialBeforeEnd(Sender: TObject);
    procedure TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrialStart(Sender: TObject);
  protected
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TCustomControl); override;
    destructor Destroy;override;
    function AsString : string; override;
    procedure Play(ACorrection : Boolean); override;
  end;

resourcestring

  MessagePrompt1 = 'Pressione  o botão  para avançar.';

implementation

uses
  constants
  , Timestamps
  ;

constructor THTMLMessage.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialKeyUp := @TrialKeyUp;
  OnTrialStart := @TrialStart;

  FMessage := TIpHtmlPanel.Create(Self);
  with FMessage do begin
    Anchors := [akLeft,akRight];
    AllowTextSelect := False;
    FixedTypeface := 'Times New Roman';
    DefaultTypeFace := 'default';
    DefaultFontSize := 20;
    FlagErrors := False;
    ShowHints := False;
    Visible := False;
    WantTabs := False;
    Parent := TCustomControl(AOwner);
    OnKeyUp:=@TrialKeyUp;
  end;

  Header := Header + #9 +
            rsReportStmBeg + #9 +
            rsReportStmDur + #9 +
            rsReportMsgTxt;

  Result := T_NONE;
  IETConsequence := T_NONE;
  Result := T_NONE;
end;

destructor THTMLMessage.Destroy;
begin
  inherited Destroy;
end;

function THTMLMessage.AsString: string;
begin
  Result := '';
end;

procedure THTMLMessage.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 32 then
    EndTrial(Sender);
end;

procedure THTMLMessage.TrialBeforeEnd(Sender: TObject);
begin
  FDataSupport.StmEnd := TickCount;
  WriteData(Self);
end;

procedure THTMLMessage.Play(ACorrection : Boolean);
var
  LParameters : TStringList;
  LMessage : string;
begin
  inherited Play(ACorrection);
  LParameters := Configurations.SList;
  with FMessage do
    begin
       LMessage := LParameters.Values[_Msg];
       LMessage :=
       '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"'+
       '<html>' +
         '<head>' +
           '<meta http-equiv="content-type" content="text/html; charset=UTF-8">' +
           '<style type="text/css">' +
             '.container {text-align: center;}' +
           '</style>' +
         '</head>' +
         '<body><div class=container>'+LMessage+'</div></body>' +
       '</html>';
       FMessage.SetHtmlFromStr(LMessage);
    end;

  if Self.ClassType = THTMLMessage then Config(Self);
end;

procedure THTMLMessage.TrialStart(Sender: TObject);
begin
  with FMessage do
    begin
      Height := (Self.Height div 3) * 2;
      Width := (Self.Width div 8) * 5;
      SetBounds((Self.Width - Width) div 2, (Self.Height - Height) div 2, Width, Height);
      Visible := True;
    end;
  //Self.SetFocus;
  FDataSupport.StmBegin := TickCount;
end;

procedure THTMLMessage.WriteData(Sender: TObject);
var aStart, aDuration : string;
begin
  inherited WriteData(Sender);
  aStart := TimestampToStr(FDataSupport.StmBegin - TimeStart);
  aDuration := TimestampToStr(FDataSupport.StmEnd - TimeStart);

  Data := Data + aStart + #9 + aDuration + #9 + FMessage.Caption;
end;

end.

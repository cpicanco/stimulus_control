{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.HTMLMessage;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils, StdCtrls, Graphics, IpHtml

  , Controls.Trials.Abstract
  , Controls.Trials.Helpers
  ;

type

  { THTMLMessage }

  THTMLMessage = class(TTrial)
    procedure EndButtonClick(Sender : TObject);
  private
    FEndButton : TButton;
    FDataSupport : TDataSupport;
    FMessage : TIpHtmlPanel;
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

uses
  Constants
  , Timestamps
  ;

constructor THTMLMessage.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  //OnTrialKeyUp := @TrialKeyUp;
  OnTrialStart := @TrialStart;

  FMessage := TIpHtmlPanel.Create(Self);
  FMessage.Visible := False;
  with FMessage do begin
    Anchors := [akLeft,akRight];
    AllowTextSelect := False;
    FixedTypeface := 'Times New Roman';
    DefaultTypeFace := 'default';
    DefaultFontSize := 20;
    FlagErrors := False;
    ShowHints := False;
    WantTabs := False;

    //OnKeyUp:=@TrialKeyUp;
    //OnMouseDown:=@TrialMouseDown;
    Height := (Self.Height div 3) * 2;
    Width := (Self.Width div 8) * 5;
    Left := (Self.Width - Width) div 2;
    Top := (Self.Height - Height) div 2;
    Parent := TCustomControl(AOwner);
  end;

  FEndButton := TButton.Create(Self);
  with FEndButton do begin
    Caption := 'Continuar';
    AutoSize := True;
    Font.Name:='Times New Roman';
    Font.Size := 15;
    Top := FMessage.BoundsRect.Bottom + 10;
    Left := FMessage.BoundsRect.Right - Width - 50;
    OnClick := @EndButtonClick;
    Parent := TCustomControl(AOwner);
  end;

  Header := Header + #9 +
            rsReportStmBeg + #9 +
            rsReportStmDur;

  Result := T_NONE;
  IETConsequence := T_NONE;
  Result := T_NONE;
end;

destructor THTMLMessage.Destroy;
begin
  inherited Destroy;
end;

function THTMLMessage.ConsequenceInterval: integer;
begin
  Result:=0;
end;

function THTMLMessage.AsString: string;
begin
  Result := '';
end;

procedure THTMLMessage.EndButtonClick(Sender: TObject);
begin
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
  FMessage.Visible := True;
  Self.SetFocus;
  FDataSupport.StmBegin := TickCount;
end;

procedure THTMLMessage.WriteData(Sender: TObject);
var aStart, aDuration : string;
begin
  inherited WriteData(Sender);
  aStart := TimestampToStr(FDataSupport.StmBegin - TimeStart);
  aDuration := TimestampToStr(FDataSupport.StmEnd - TimeStart);

  Data := Data + aStart + #9 + aDuration;
end;

end.

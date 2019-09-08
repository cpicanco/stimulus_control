unit Controls.Stimuli.Text;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls;

type

  { TLabelStimulus }

  TLabelStimulus = class(TLabel)
  private

  public
    constructor Create(AOwner: TComponent; AParent : TWinControl); reintroduce;
    procedure LoadFromFile(AFilename : string);
    procedure CentralizeLeft;
    procedure CentralizeTopMiddle;
    procedure CentralizeBottom;
    procedure CentralizeTopRight;
    procedure CentralizeMiddleRight;
    procedure CentralizeOnTopOfControl(AControl : TGraphicControl);
  end;

implementation

uses Graphics;

{ TLabelStimulus }

constructor TLabelStimulus.Create(AOwner: TComponent; AParent: TWinControl);
begin
  inherited Create(AOwner);
  Visible := False;
  Alignment := taCenter;
  Anchors := [akLeft,akRight];
  Font.Name := 'Arial';
  Font.Color:= clBlack;
  Font.Size := 14;
  Parent := AParent;
end;

procedure TLabelStimulus.LoadFromFile(AFilename: string);
var LStringList : TStringList;
begin
  LStringList := TStringList.Create;
  try
    LStringList.LoadFromFile(AFilename);
    Caption := LStringList.Text;
    WordWrap := True;
  finally
    LStringList.Free;
  end;
end;

procedure TLabelStimulus.CentralizeLeft;
var
  LParent : TCustomControl;
begin
  LParent := TCustomControl(Parent);
  Width := (LParent.Width div 2)-150;
  Left := (LParent.Width div 4) - (Width div 2);
  Top := (LParent.Height div 2) - (Height div 2);
end;

procedure TLabelStimulus.CentralizeTopMiddle;
var
  LParent : TCustomControl;
begin
  LParent := TCustomControl(Parent);
  Width := (LParent.Width div 2)-150;
  Left := (LParent.Width div 2) - (Width div 2);
  Top := (LParent.Height div 2) - (LParent.Height div 4) - (Height div 2);

  if Top < 250 then Top := 250;
end;

procedure TLabelStimulus.CentralizeBottom;
var
  LParent : TCustomControl;
begin
  LParent := TCustomControl(Parent);
  Width := (LParent.Width div 2)-150;
  Left := (LParent.Width div 2) - (Width div 2);
  Top := (LParent.Height div 2) + (LParent.Height div 4) - (Height div 2);
end;

procedure TLabelStimulus.CentralizeTopRight;
var
  LParent : TCustomControl;
begin
  LParent := TCustomControl(Parent);
  Width := (LParent.Width div 2)-150;
  Left := LParent.Width - (LParent.Width div 4) - (Width div 2);
  Top := (LParent.Height div 4) - (Height div 2);
end;

procedure TLabelStimulus.CentralizeMiddleRight;
var
  LParent : TCustomControl;
begin
  LParent := TCustomControl(Parent);
  Width := (LParent.Width div 2)-150;
  Left := LParent.Width - (LParent.Width div 4) - (Width div 2);
  Top := LParent.Height - (LParent.Height div 2) - (Height div 2);
end;

procedure TLabelStimulus.CentralizeOnTopOfControl(AControl: TGraphicControl);
var
  LParent : TCustomControl;
begin
  LParent := TCustomControl(Parent);
  Width := (LParent.Width div 2)-150;
  Left := AControl.Left + (AControl.Width div 2) - (Width div 2);
  Top := AControl.Top - Height - 25;
end;

end.


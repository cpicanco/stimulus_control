unit Controls.GoLeftGoRight;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls;

type

  { TGoLeftGoRight }

  TGoLeftGoRight = class(TPanel)
  private
    FButtonLeft : TButton;
    FButtonRight : TButton;
    function GetOnButtonLeftClick: TNotifyEvent;
    function GetOnButtonRightClick: TNotifyEvent;
    procedure SetOnButtonRightClick(AValue: TNotifyEvent);
    procedure SetOnButtonLeftClick(AValue: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent; ASimbling : TGraphicControl); reintroduce;
    property OnButtonLeftClick : TNotifyEvent read GetOnButtonLeftClick write SetOnButtonLeftClick;
    property OnButtonRightClick : TNotifyEvent read GetOnButtonRightClick write SetOnButtonRightClick;
  end;

implementation

{ TGoLeftGoRight }

function TGoLeftGoRight.GetOnButtonLeftClick: TNotifyEvent;
begin
  Result := FButtonLeft.OnClick
end;

function TGoLeftGoRight.GetOnButtonRightClick: TNotifyEvent;
begin
  Result := FButtonRight.OnClick;
end;

procedure TGoLeftGoRight.SetOnButtonRightClick(AValue: TNotifyEvent);
begin
  if FButtonRight.OnClick=AValue then Exit;
  FButtonRight.OnClick:=AValue;
end;

procedure TGoLeftGoRight.SetOnButtonLeftClick(AValue: TNotifyEvent);
begin
  if FButtonLeft.OnClick=AValue then Exit;
  FButtonLeft.OnClick:=AValue;
end;

constructor TGoLeftGoRight.Create(AOwner: TComponent; ASimbling: TGraphicControl);
begin
  inherited Create(AOwner);
  BevelColor:=$1FFFFFFF;
  Height := 60;
  Width := 400;
  Top := ASimbling.Top + ASimbling.Height + 25;
  Left := ASimbling.Left + (ASimbling.Width div 2) - (Width div 2);
  FButtonLeft := TButton.Create(Self);
  FButtonLeft.Height := 55;
  FButtonLeft.Width := 150;
  FButtonLeft.Caption := 'Verdadeiro';
  FButtonLeft.Font.Size := 14;
  FButtonLeft.Left := (Self.Width div 4) - (FButtonLeft.Width div 2);
  FButtonLeft.Top := (Self.Height div 2) - (FButtonLeft.Height div 2);
  FButtonLeft.Parent := Self;

  FButtonRight := TButton.Create(Self);
  FButtonRight.Height := 55;
  FButtonRight.Width := 150;
  FButtonRight.Caption := 'Falso';
  FButtonRight.Font.Size := 14;
  FButtonRight.Left := Self.Width - (Self.Width div 4) - (FButtonRight.Width div 2);
  FButtonRight.Top := Self.Height - (Self.Height div 2) - (FButtonRight.Height div 2);
  FButtonRight.Parent := Self;
end;

end.


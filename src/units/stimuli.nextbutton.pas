unit Stimuli.NextButton;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Stimuli, Stimuli.Abstract;

type

  TSiblingType = (AtBottomRightofSibling, AtBottomCenterofSibling);

  { TNextButton }

  TNextButton = class(TStimulus, IStimuli)
  private
    FButton : TButton;
    FSibling : TControl;
    FSiblingType : TSiblingType;
    function GetOnClick : TNotifyEvent;
    procedure SetOnClick(AValue : TNotifyEvent);
    procedure SetSibing(AValue : TControl);
    procedure SetSiblingType(AValue : TSiblingType);
  published
    constructor Create(AOwner : TComponent); overload; override;
    function AsInterface : IStimuli;
    procedure DoExpectedResponse;
    procedure LoadFromParameters(AParameters : TStringList);
    procedure Start;
    procedure Stop;
    property OnClick : TNotifyEvent read GetOnClick write SetOnClick;
    property Sibling : TControl read FSibling write SetSibing;
    property SiblingType : TSiblingType read FSiblingType write SetSiblingType;
  end;

implementation

{ TNextButton }

procedure TNextButton.SetOnClick(AValue : TNotifyEvent);
begin
  if FButton.OnClick = AValue then Exit;
  FButton.OnClick := AValue;
end;

procedure TNextButton.SetSibing(AValue : TControl);
begin
  if FSibling = AValue then Exit;
  FSibling := AValue;
  FButton.Parent := FSibling.Parent;
end;

procedure TNextButton.SetSiblingType(AValue : TSiblingType);
begin
  if FSiblingType = AValue then Exit;
  FSiblingType := AValue;
end;

function TNextButton.GetOnClick : TNotifyEvent;
begin
  Result := FButton.OnClick;
end;

constructor TNextButton.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FButton := TButton.Create(AOwner);
  with FButton do begin
    Caption := 'Continuar';
    AutoSize := True;
    Font.Name:='Times New Roman';
    Font.Size := 15;
  end;
end;

function TNextButton.AsInterface : IStimuli;
begin
  Result := Self;
end;

procedure TNextButton.Start;
begin
  with FButton do begin
    case FSiblingType of
      AtBottomCenterofSibling : begin
        Top := FSibling.BoundsRect.Bottom + 20;
        Left := FSibling.BoundsRect.CenterPoint.X - Width;
      end;

      AtBottomRightofSibling : begin;
        Top := FSibling.BoundsRect.Bottom + 10;
        Left := FSibling.BoundsRect.Right - Width - 50;
      end;
    end;
    Show;
  end;
end;

procedure TNextButton.Stop;
begin
  FButton.Hide;
end;

procedure TNextButton.LoadFromParameters(AParameters : TStringList);
begin
  { do nothing }
end;

procedure TNextButton.DoExpectedResponse;
begin
  Mouse.CursorPos := FButton.BoundsRect.CenterPoint;
  FButton.Click;
end;

end.


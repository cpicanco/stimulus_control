unit Stimuli.Image.DragDropable;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, Controls, Stimuli, Stimuli.Image.Base;

type

  { TDragDropableItem }

  TDropMode = (dropRect, dropCircle);

  TDragDropableItem = class (TLightImage, IDragDropable)
  private
    FDropMode: TDropMode;
    FIsDragging : Boolean;
    FOnDragDrop: TDragDropEvent;
    FStartPosition : TPoint;
    FStartMouseDown : TPoint;
    FTarget: TObject;
    function GetDraggable: Boolean;
    procedure SetDropMode(AValue: TDropMode);
    procedure SetOnDragDrop(AValue: TDragDropEvent);
    procedure SetTarget(AValue: TObject);
    function IntersectsWith(Sender : TObject) : Boolean;
  protected
    procedure DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure MouseDown(Button: TMouseButton;
      Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton;
      Shift:TShiftState; X,Y:Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Target : TObject read FTarget write SetTarget;
    property Draggable : Boolean read GetDraggable;
    property DropMode : TDropMode read FDropMode write SetDropMode;
    property OnDragDrop : TDragDropEvent read FOnDragDrop write SetOnDragDrop;

  end;

implementation

uses Graphics;

{ TDragDropableItem }

function TDragDropableItem.GetDraggable: Boolean;
begin
  Result := FTarget <> nil;
end;

procedure TDragDropableItem.SetDropMode(AValue: TDropMode);
begin
  if FDropMode = AValue then Exit;
  FDropMode := AValue;
end;

procedure TDragDropableItem.SetOnDragDrop(AValue: TDragDropEvent);
begin
  if FOnDragDrop = AValue then Exit;
  FOnDragDrop := AValue;
end;

procedure TDragDropableItem.SetTarget(AValue: TObject);
begin
  if FTarget = AValue then Exit;
  FTarget := AValue;
end;

function TDragDropableItem.IntersectsWith(Sender: TObject): Boolean;
var
  LControl : TControl;

  function InsideCircle(ACenterX, ACenterY, ARadius, AX, AY : integer): Boolean;
  var Delta : integer;
  begin
    Delta := ((AX - ACenterX) * (AX - ACenterX)) +
             ((AY - ACenterY) * (AY - ACenterY));
    if (Delta <= (ARadius * ARadius)) then
        Result := True
    else
        Result := False;
  end;
begin
  if Sender is TControl then begin
    LControl := TDragDropableItem(Target);
    case DropMode of
      dropRect : begin
        Result := BoundsRect.IntersectsWith(LControl.BoundsRect);
      end;
      dropCircle : begin
        //Result := InsideCircle(
        //  LControl.BoundsRect.CenterPoint.X,
        //  LControl.BoundsRect.CenterPoint.Y,
      end;
    end;
  end;
end;

procedure TDragDropableItem.DragDrop(Sender, Source: TObject; X, Y: Integer);
begin

end;

procedure TDragDropableItem.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Draggable then begin
    if not FIsDragging then begin
      if Button in [mbLeft] then begin
        FStartPosition.X := Left;
        FStartPosition.Y := Top;
        FStartMouseDown.X := X;
        FStartMouseDown.Y := Y;
        FIsDragging := True;
      end;
    end;
  end;
end;

procedure TDragDropableItem.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FIsDragging then begin
    Left := Left - (FStartMouseDown.X -X);
    Top := Top - (FStartMouseDown.Y -Y);
  end;
end;

procedure TDragDropableItem.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Assigned(Target) then begin
    if Button in [mbLeft] then begin
      FIsDragging := False;
      if Target is TDragDropableItem then begin
        if IntersectsWith(Target) then begin
          Color := clGreen;
          if Assigned(OnDragDrop) then OnDragDrop(Target, Self, X, Y);
          Exit;
        end else begin
          Color := clRed;
          Left := FStartPosition.X;
          Top  := FStartPosition.Y;
        end;
      end;
    end;
  end;
end;

constructor TDragDropableItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTarget := nil;
  Kind := ikLetter;
end;

end.


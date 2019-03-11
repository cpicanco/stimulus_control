unit Controls.Counters.PerformanceReview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, Graphics;

type

  { TCounterPR }

  TCounterPR = class(TGraphicControl)
  private
    FX : integer; // Center
    FHeader : TLabel;
    function GetCaption: string;
    procedure SetCaption(AValue: string);
  protected
    function GetCursor : TCursor; override;
    procedure SetCursor(AValue:  TCursor); override;
    procedure Paint; override;
  public
    constructor Create(AOwner : TComponent); override;
    procedure CentralizeLeft;
    procedure CentralizeRight;
    procedure Show;
    property Caption : string read GetCaption write SetCaption;
  end;

implementation

{ TCounterPR }

function TCounterPR.GetCaption: string;
begin
  Result := FHeader.Caption;
end;

function TCounterPR.GetCursor: TCursor;
begin
  Result := inherited GetCursor;
end;

procedure TCounterPR.SetCaption(AValue: string);
begin
  if FHeader.Caption = AValue then Exit;
  FHeader.Caption := AValue;
  AutoSize:= False;
  AutoSize:= True;
end;

procedure TCounterPR.SetCursor(AValue: TCursor);
begin
  inherited SetCursor(AValue);
  FHeader.Cursor:=AValue;
end;

procedure TCounterPR.Paint;
begin
  inherited Paint;
  Canvas.Pen.Width := 10;
  Canvas.Pen.EndCap := pecSquare;
  Canvas.MoveTo(FX, Top + 50);
  Canvas.LineTo(FX, Height -50);
end;

constructor TCounterPR.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner.Owner);
  Align := alClient;
  FX := Width div 2;

  FHeader := TLabel.Create(AOwner);
  with FHeader do
    begin
      Visible := False;
      Cursor := -1;
      Alignment := taCenter;
      WordWrap := False;
      Font.Name := 'TimesNewRoman';
      Font.Color:= 0;
      Font.Size:= 28;

      Parent := TWinControl(AOwner.Owner);
    end;
end;

procedure TCounterPR.CentralizeLeft;
var
  LOwner : TCustomControl;
begin
  LOwner := TCustomControl(Owner.Owner);
  FHeader.Left := (LOwner.Width div 4) - (FHeader.Width div 2);
  FHeader.Top := (LOwner.Height div 2) - (FHeader.Height div 2);
end;

procedure TCounterPR.CentralizeRight;
var
  LOwner : TCustomControl;
begin
  LOwner := TCustomControl(Owner.Owner);
  FHeader.Left := LOwner.Width - (LOwner.Width div 4) - (FHeader.Width div 2);
  FHeader.Top := LOwner.Height - (LOwner.Height div 2) - (FHeader.Height div 2);
end;

procedure TCounterPR.Show;
begin
  FHeader.Show;
end;

end.


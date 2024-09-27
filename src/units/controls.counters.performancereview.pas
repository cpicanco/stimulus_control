unit Controls.Counters.PerformanceReview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, Graphics
  , Controls.Stimuli.Key
  ;

type

  { TCounterPR }

  TCounterPR = class(TGraphicControl)
  private
    FX : integer; // Center
    FHeader : TLabel;
    FImage : TKey;
    function GetCaption: string;
    procedure SetCaption(AValue: string);
  protected
    procedure Paint; override;
  public
    constructor Create(ABackground:TComponent); override;
    procedure CentralizeLeft;
    procedure CentralizeRight;
    procedure LoadImage(AImagePath : string);
    procedure Show;
    property Caption : string read GetCaption write SetCaption;
  end;

implementation

{ TCounterPR }

function TCounterPR.GetCaption: string;
begin
  Result := FHeader.Caption;
end;

procedure TCounterPR.SetCaption(AValue: string);
begin
  FHeader.Caption := AValue;
  AutoSize:= False;
  AutoSize:= True;
end;

procedure TCounterPR.Paint;
begin
  inherited Paint;
  Canvas.Pen.Width := 10;
  Canvas.Pen.EndCap := pecSquare;
  Canvas.MoveTo(FX, Top + 50);
  Canvas.LineTo(FX, Height -50);
end;

constructor TCounterPR.Create(ABackground: TComponent);
begin
  inherited Create(ABackground);
  Parent := TCustomControl(ABackground);
  Align := alClient;
  FX := Width div 2;
  FImage := TKey.Create(Self);
  with FImage do
    begin
      Cursor := -1;
      Width := 200;
      Height := 200;
      Parent := TCustomControl(ABackground);
    end;

  FHeader := TLabel.Create(Self);
  with FHeader do
    begin
      Visible := False;
      Cursor := -1;
      Alignment := taCenter;

      // anchor bindings
      Anchors := [akLeft,akBottom, akRight];
      AnchorSideBottom.Control := FImage;
      AnchorSideBottom.Side := asrTop;
      AnchorSideLeft.Control := FImage;
      AnchorSideLeft.Side := asrLeft;
      AnchorSideRight.Control := FImage;
      AnchorSideRight.Side := asrRight;
      BorderSpacing.Bottom := 25;

      //Layout := tlCenter;
      WordWrap := False;
      Font.Name := 'TimesNewRoman';
      Font.Color:= 0;
      Font.Size:= 28;

      Parent := TCustomControl(ABackground);
    end;
end;

procedure TCounterPR.CentralizeLeft;
var
  LOwner : TCustomControl;
begin
  LOwner := TCustomControl(Owner);
  FImage.Left := (LOwner.Width div 4) - (FImage.Width div 2);
  FImage.Top := (LOwner.Height div 2) - (FImage.Height div 2);
end;

procedure TCounterPR.CentralizeRight;
var
  LOwner : TCustomControl;
begin
  LOwner := TCustomControl(Owner);
  FImage.Left := LOwner.Width - (LOwner.Width div 4) - (FImage.Width div 2);
  FImage.Top := LOwner.Height - (LOwner.Height div 2) - (FImage.Height div 2);
end;

procedure TCounterPR.LoadImage(AImagePath: string);
begin
  FImage.Filename := AImagePath;
end;

procedure TCounterPR.Show;
begin
  FHeader.Show;
  FImage.Show;
end;

end.


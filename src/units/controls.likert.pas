unit Controls.Likert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, ExtCtrls, StdCtrls,
  Forms,
  Controls.Stimuli.Key;

type

  { TLikertScale }

  TLikertScale = class(TRadioGroup)
  private
    FTitle: TLabel;
    FImage1: TKey;
    FImage2: TKey;
    FConfirmationButton: TButton;
    function GetOnConfirmation: TNotifyEvent;
    procedure SetOnConfirmation(AValue: TNotifyEvent);
    procedure ShowConfirmationButton(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; AParent : TWinControl); reintroduce;
    function AsString: string;
    function FilenameLeft: string;
    function FilenameRight: string;
    procedure LoadImagePair(ALeftImage, ARightImage: string);
    procedure Hide; reintroduce;
    procedure Show; reintroduce;
    property OnConfirmation : TNotifyEvent read GetOnConfirmation write SetOnConfirmation;
  end;

implementation

{ TLikertScale }

procedure TLikertScale.ShowConfirmationButton(Sender: TObject);
begin
  if not FConfirmationButton.Visible then
    FConfirmationButton.Show;
end;

procedure TLikertScale.SetOnConfirmation(AValue: TNotifyEvent);
begin
  if FConfirmationButton.OnClick=AValue then Exit;
  FConfirmationButton.OnClick := AValue;
end;

function TLikertScale.GetOnConfirmation: TNotifyEvent;
begin
  Result := FConfirmationButton.OnClick;
end;

constructor TLikertScale.Create(AOwner: TComponent; AParent : TWinControl);
begin
  inherited Create(AOwner);
  OnClick := @ShowConfirmationButton;
  Parent := TWinControl(AParent);
  Items.Append('Tenho certeza que não vi.');
  Items.Append('Acho que não vi.');
  Items.Append('Não sei.');
  Items.Append('Acho que vi.');
  Items.Append('Tenho certeza que vi.');
  Font.Size := 18;
  Columns := 5;
  Align := alBottom;

  FConfirmationButton := TButton.Create(Self);
  FConfirmationButton.Caption := 'Confirmar';
  FConfirmationButton.Font.Size := 18;
  FConfirmationButton.Width := 150;
  FConfirmationButton.Height := 80;
  FConfirmationButton.Hide;
  FConfirmationButton.AnchorHorizontalCenterTo(Self);
  FConfirmationButton.AnchorSide[akBottom].Side := asrTop;
  FConfirmationButton.AnchorSide[akBottom].Control := Self;
  FConfirmationButton.Anchors := FConfirmationButton.Anchors - [akTop];
  FConfirmationButton.Anchors := FConfirmationButton.Anchors + [akBottom];
  FConfirmationButton.Parent := TWinControl(AParent);

  FTitle := TLabel.Create(Self);
  FTitle.Caption := 'O quanto você lembra desta imagem?';
  FTitle.BorderSpacing.Top := 100;
  FTitle.Font.Size := 20;
  FTitle.Align := alTop;
  FTitle.Layout := tlCenter;
  FTitle.Alignment := taCenter;
  FTitle.Parent := TWinControl(AParent);

  FImage1 := TKey.Create(Self);
  FImage1.Width := 300;
  FImage1.Height := 300;
  FImage1.Caption := '$';
  FImage1.Color := clBlack;
  FImage1.Left := (AParent.Width div 3) - (FImage1.Width div 2);
  FImage1.Top := (AParent.Height div 2) - (FImage1.Height div 2);
  FImage1.Show;
  FImage1.Parent := TWinControl(AParent);

  FImage2 := TKey.Create(Self);
  FImage2.Width := 300;
  FImage2.Height := 300;
  FImage2.Caption := '@';
  FImage2.Color := clBlack;
  FImage2.Left := AParent.Width - (AParent.Width div 3) - (FImage2.Width div 2);
  FImage2.Top := AParent.Height - (AParent.Height div 2) - (FImage2.Height div 2);
  FImage2.Show;
  FImage2.Parent := TWinControl(AParent);
end;

function TLikertScale.AsString: string;
begin
  Result :=
    FImage1.ShortName + ',' + FImage2.ShortName + ',' + IntToStr(ItemIndex+1);
end;

function TLikertScale.FilenameLeft: string;
begin
  Result := ExtractFileName(FImage1.Filename);
end;

function TLikertScale.FilenameRight: string;
begin
  Result := ExtractFileName(FImage2.Filename);
end;

procedure TLikertScale.LoadImagePair(ALeftImage, ARightImage: string);
begin
  FImage1.Filename := ALeftImage;
  FImage2.Filename := ARightImage;
end;

procedure TLikertScale.Hide;
begin
  inherited Hide;
  FTitle.Hide;
  FImage1.Hide;
  FImage2.Hide;
  FConfirmationButton.Hide;
end;

procedure TLikertScale.Show;
begin
  inherited Show;
  FTitle.Show;
  FImage1.Show;
  FImage2.Show;
end;

end.


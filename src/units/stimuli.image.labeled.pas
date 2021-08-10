{
  Stimulus Control
  Copyright (C) 2014-2020 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Stimuli.Image.Labeled;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, StdCtrls
  , Stimuli.Abstract
  , Stimuli.Image.Base
  , Schedules
  ;

type

  { TStimulusFigure }

  TStimulusLabeledFigure = class(TStimulus)
  private
    FLabel : TLabel;
    FImage : TLightImage;
    function GetLeft: integer;
    function GetParent: TWinControl;
    function GetTop: integer;
    function GetWidth: integer;
    function GetHeight: integer;
    procedure SetHeight(AValue: integer);
    procedure SetParent(AValue: TWinControl);
    procedure SetWidth(AValue: integer);
    procedure SetLeft(AValue: integer);
    procedure SetTop(AValue: integer);
  protected
    procedure SetSchedule(ASchedule : TSchedule); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(AFilename: string); override;
    procedure Start; override;
    procedure Stop; override;
    procedure HideCursor;
    property Parent : TWinControl read GetParent write SetParent;
    property Width : integer read GetWidth write SetWidth;
    property Height : integer read GetHeight write SetHeight;
    property Top : integer read GetTop write SetTop;
    property Left: integer read GetLeft write SetLeft;
  end;

implementation

{ TStimulusFigure }

procedure TStimulusLabeledFigure.SetHeight(AValue: integer);
begin
  if FImage.Height=AValue then Exit;
  FImage.Height:=AValue;
end;

procedure TStimulusLabeledFigure.SetParent(AValue: TWinControl);
begin
  if FImage.Parent=AValue then Exit;
  FImage.Parent := AValue;
  FLabel.Parent := AValue;
end;

function TStimulusLabeledFigure.GetWidth: integer;
begin
  Result := FImage.Width;
end;

function TStimulusLabeledFigure.GetHeight: integer;
begin
  Result := FImage.Height;
end;

function TStimulusLabeledFigure.GetLeft: integer;
begin
  Result := FImage.Left;
end;

function TStimulusLabeledFigure.GetParent: TWinControl;
begin
  Result := FImage.Parent;
end;

function TStimulusLabeledFigure.GetTop: integer;
begin
  Result := FImage.Top;
end;

procedure TStimulusLabeledFigure.SetLeft(AValue: integer);
begin
  if FImage.Left=AValue then Exit;
  FImage.Left:=AValue;
end;

procedure TStimulusLabeledFigure.SetTop(AValue: integer);
begin
  if FImage.Top=AValue then Exit;
  FImage.Top:=AValue;
end;

procedure TStimulusLabeledFigure.SetSchedule(ASchedule: TSchedule);
begin
  inherited SetSchedule(ASchedule);
  FImage.Schedule := Schedule;
end;

procedure TStimulusLabeledFigure.SetWidth(AValue: integer);
begin
  if FImage.Width=AValue then Exit;
  FImage.Width:=AValue;
end;

constructor TStimulusLabeledFigure.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImage := TLightImage.Create(Self);
  FImage.Visible := False;

  FLabel := TLabel.Create(Self);
  with FLabel do begin
    Caption := 'Você ganhou 1 ponto';
    Font.Name:='Times New Roman';
    Font.Color := 0;
    Font.Size := 50;
    Anchors := [akBottom, akLeft];
    AnchorSideBottom.Control := FImage;
    AnchorSideBottom.Side := asrTop;
    AnchorSideLeft.Control := FImage;
    AnchorSideLeft.Side := asrCenter;
    FLabel.Visible := False;
  end;
end;

destructor TStimulusLabeledFigure.Destroy;
begin
  inherited Destroy;
end;

procedure TStimulusLabeledFigure.LoadFromFile(AFilename: string);
begin
  try
    FImage.LoadFromFile(AFilename);
  except
    on E : Exception do
    begin
      Exception.Create(E.Message + #32 + AFilename);
    end;
  end;
end;

procedure TStimulusLabeledFigure.Start;
begin
  FImage.Centralize;
  FImage.Show;
  FLabel.Show;
end;

procedure TStimulusLabeledFigure.Stop;
begin
  FLabel.Hide;
  FImage.Hide;
end;

procedure TStimulusLabeledFigure.HideCursor;
begin
  FLabel.Cursor := -1;
  FImage.Cursor := -1;
end;

end.


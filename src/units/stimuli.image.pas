{
  Stimulus Control
  Copyright (C) 2014-2019 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Stimuli.Image;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics
  , Stimuli.Abstract
  , Stimuli.Image.Base
  , Schedules
  ;

type

  { TStimulusFigure }

  TStimulusFigure = class(TStimulus)
  private
    FBackground : TCustomControl;
    FImage : TLightImage;
    function GetLeft: integer;
    function GetTop: integer;
    function GetWidth: integer;
    function GetHeight: integer;
    procedure SetBackGround(AValue: TCustomControl);
    procedure SetHeight(AValue: integer);
    procedure SetWidth(AValue: integer);
    procedure SetLeft(AValue: integer);
    procedure SetTop(AValue: integer);
  protected
    procedure SetSchedule(ASchedule : TSchedule); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(AFilename: string); override;
    property BackGround : TCustomControl read FBackGround write SetBackGround;
    property Width : integer read GetWidth write SetWidth;
    property Height : integer read GetHeight write SetHeight;
    property Top : integer read GetTop write SetTop;
    property Left: integer read GetLeft write SetLeft;
  end;

implementation

{ TStimulusFigure }

procedure TStimulusFigure.SetHeight(AValue: integer);
begin
  if FImage.Height=AValue then Exit;
  FImage.Height:=AValue;
end;

function TStimulusFigure.GetWidth: integer;
begin
  Result := FImage.Width;
end;

function TStimulusFigure.GetHeight: integer;
begin
  Result := FImage.Height;
end;

procedure TStimulusFigure.SetBackGround(AValue: TCustomControl);
begin
  if FBackGround=AValue then Exit;
  FBackGround:=AValue;
  Fimage.Parent := FBackground;
end;

function TStimulusFigure.GetLeft: integer;
begin
  Result := FImage.Left;
end;

function TStimulusFigure.GetTop: integer;
begin
  Result := FImage.Top;
end;

procedure TStimulusFigure.SetLeft(AValue: integer);
begin
  if FImage.Left=AValue then Exit;
  FImage.Left:=AValue;
end;

procedure TStimulusFigure.SetTop(AValue: integer);
begin
  if FImage.Top=AValue then Exit;
  FImage.Top:=AValue;
end;

procedure TStimulusFigure.SetSchedule(ASchedule: TSchedule);
begin
  inherited SetSchedule(ASchedule);
  FImage.Schedule := Schedule;
end;

procedure TStimulusFigure.SetWidth(AValue: integer);
begin
  if FImage.Width=AValue then Exit;
  FImage.Width:=AValue;
end;

constructor TStimulusFigure.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImage := TLightImage.Create(Self);
end;

destructor TStimulusFigure.Destroy;
begin
  inherited Destroy;
end;

procedure TStimulusFigure.LoadFromFile(AFilename: string);
begin
  FImage.LoadFromFile(AFilename);
end;

end.


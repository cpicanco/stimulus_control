{
  Stimulus Control
  Copyright (C) 2014-2020 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Stimuli.Game.FreeSquare;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, StdCtrls
  , Stimuli.Abstract
  , Stimuli.Image.MovingSquare
  , Schedules
  ;

type

  { TFreeSquare }

  TFreeSquare = class(TStimulus)
  private
    FOnConsequence: TNotifyEvent;
    FParent : TWinControl;
    //FBackground : TLightImage;
    FMovingSquare : TMovingSquare;
    function GetParent: TWinControl;
    procedure SetParent(AValue: TWinControl);
    procedure Consequence(Sender : TObject);
    procedure Response(Sender : TObject);
  protected
    procedure SetSchedule(ASchedule : TSchedule); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(AFilename: string); override;
    procedure LoadFromParameters(AParameters : TStringList);
    procedure Start; override;
    procedure Stop; override;
    procedure FitScreen;
    property Parent : TWinControl read FParent write SetParent;
  end;

implementation

uses Forms;

{ TFreeSquare }

procedure TFreeSquare.SetParent(AValue: TWinControl);
begin
  if FParent=AValue then Exit;
  FParent := AValue;
  FMovingSquare.Parent := AValue;
end;

procedure TFreeSquare.Consequence(Sender: TObject);
begin
  if Assigned(OnConsequence) then OnConsequence(Sender);
end;

function TFreeSquare.GetParent: TWinControl;
begin
  Result := FParent;
end;

procedure TFreeSquare.Response(Sender: TObject);
begin
  if Assigned(OnResponse) then OnResponse(Sender);
end;


procedure TFreeSquare.SetSchedule(ASchedule: TSchedule);
begin
  inherited SetSchedule(ASchedule);
end;

constructor TFreeSquare.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Schedule.Load(FR, 2);
  Schedule.OnConsequence:=@Consequence;
  Schedule.OnResponse:=@Response;
  FMovingSquare := TMovingSquare.Create(Self);
  FMovingSquare.Schedule := Schedule;
end;

destructor TFreeSquare.Destroy;
begin
  inherited Destroy;
end;

procedure TFreeSquare.LoadFromFile(AFilename: string);
begin

end;

procedure TFreeSquare.LoadFromParameters(AParameters: TStringList);
begin

end;

procedure TFreeSquare.Start;
begin
  FMovingSquare.Show;
  FMovingSquare.Start;
  Schedule.Start;
end;

procedure TFreeSquare.Stop;
begin
  Schedule.Stop;
  FMovingSquare.Stop;
  FMovingSquare.Hide;
end;

procedure TFreeSquare.FitScreen;
begin
  FMovingSquare.Centralize;
end;

end.


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
  , Stimuli
  , Stimuli.Abstract
  , Stimuli.Image.MovingSquare
  , Schedules
  ;

type

  { TFreeSquare }

  TFreeSquare = class(TStimulus, IStimuli)
  private
    FFreezed : Boolean;
    //FOnConsequence: TNotifyEvent;
    FParent : TWinControl;
    FMovingSquare : TMovingSquare;
    function GetParent: TWinControl;
    procedure SetParent(AValue: TWinControl);
  public
    constructor Create(AOwner : TComponent); overload; override;
    constructor Create(AOwner : TComponent;
      ASchedule : TSchedule); override;
    destructor Destroy; override;
    procedure DoExpectedResponse;
    procedure FitScreen;
    procedure Freeze;
    procedure Hide;
    procedure LoadFromFile(AFilename: string);
    procedure LoadFromParameters(AParameters : TStringList);
    procedure Start;
    procedure Stop;
    property Parent : TWinControl read FParent write SetParent;
  end;

implementation

uses Forms, Constants;

{ TFreeSquare }

procedure TFreeSquare.SetParent(AValue: TWinControl);
begin
  if FParent=AValue then Exit;
  FParent := AValue;
  FMovingSquare.Parent := AValue;
end;

function TFreeSquare.GetParent: TWinControl;
begin
  Result := FParent;
end;

constructor TFreeSquare.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Schedule.OnConsequence:=@Consequence;
  Schedule.OnResponse:=@Response;
  FMovingSquare := TMovingSquare.Create(Self);
end;

constructor TFreeSquare.Create(AOwner : TComponent; ASchedule : TSchedule);
begin
  inherited Create(AOwner, ASchedule);
  Schedule.OnConsequence:=@Consequence;
  Schedule.OnResponse:=@Response;
  FMovingSquare := TMovingSquare.Create(Self);
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
  Schedule.Load(AParameters.Values[_Schedule]);
  FMovingSquare.Schedule := Schedule;
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

procedure TFreeSquare.Hide;
begin
  FMovingSquare.Hide;
end;

procedure TFreeSquare.Freeze;
begin
  FMovingSquare.Freeze;
  FFreezed := True;
end;

function TFreeSquare.Target : TControl;
begin
  Result := FMovingSquare;
end;

function TFreeSquare.BoundsRect : TRect;
begin
  Result := FMovingSquare.BoundsRect;
end;

procedure TFreeSquare.FitScreen;
var
  R : integer;
begin
  if FFreezed then begin
    FMovingSquare.Centralize;
    Exit;
  end;

  R := Random(3);
  case R of
   0: FMovingSquare.Centralize;
   1: FMovingSquare.CentralizeLeft;
   2: FMovingSquare.CentralizeRight;
  end;
end;

end.


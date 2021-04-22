{
  Stimulus Control
  Copyright (C) 2014-2020 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Stimuli.Choice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics
  , Stimuli.Abstract
  , Stimuli.Image.Base
  , Schedules
  ;

type

  { TBinaryChoice }

  TBinaryChoice = class(TStimulus)
  private
    FParent : TWinControl;
    FImageLeft : TLightImage;
    FImageRight : TLightImage;
    FNextTrialLeft : string;
    FNextTrialRight: string;
    function GetParent: TWinControl;
    procedure SetParent(AValue: TWinControl);
  protected
    procedure SetSchedule(ASchedule : TSchedule); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(AFilename: string); override;
    procedure LoadFromParameters(AParameters: TStringList);
    procedure SetScheduleConsequence(AConsequence : TNotifyEvent);
    function NextTrial(AComponentName : string) : string;
    procedure Start; override;
    procedure Stop; override;
    procedure FitScreen;
    property Parent : TWinControl read GetParent write SetParent;
  end;

implementation

uses Constants;

{ TStimulusFigure }

procedure TBinaryChoice.SetParent(AValue: TWinControl);
begin
  if FParent=AValue then Exit;
  FParent := AValue;
  FImageLeft.Parent := FParent;
  FImageRight.Parent := FParent;
end;


function TBinaryChoice.GetParent: TWinControl;
begin
  Result := FParent;
end;

procedure TBinaryChoice.SetSchedule(ASchedule: TSchedule);
begin
  inherited SetSchedule(ASchedule);
  FImageLeft.Schedule := Schedule;
  FImageRight.Schedule := Schedule;
end;

procedure TBinaryChoice.SetScheduleConsequence(AConsequence: TNotifyEvent);
begin
  FImageLeft.Schedule.OnConsequence := AConsequence;
  FImageRight.Schedule.OnConsequence := AConsequence;
end;

function TBinaryChoice.NextTrial(AComponentName: string): string;
begin
  case AComponentName of
    'Left' : Result := FNextTrialLeft;
    'Right': Result := FNextTrialRight;
  end;
end;

constructor TBinaryChoice.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageLeft := TLightImage.Create(Self);
  FImageLeft.Visible := False;
  FImageLeft.Schedule := TSchedule.Create(Self);
  FImageLeft.Schedule.Name := 'Left';
  FImageLeft.Schedule.Load(CRF);
  FImageLeft.EdgeColor:=clBlack;

  FImageRight := TLightImage.Create(Self);
  FImageRight.Visible := False;
  FImageRight.Schedule := TSchedule.Create(Self);
  FImageRight.Schedule.Name := 'Right';
  FImageRight.Schedule.Load(CRF);
  FImageRight.EdgeColor:=clBlack;
end;

destructor TBinaryChoice.Destroy;
begin
  inherited Destroy;
end;

procedure TBinaryChoice.LoadFromFile(AFilename: string);
begin

end;

procedure TBinaryChoice.LoadFromParameters(AParameters: TStringList);
begin
  FImageLeft.Caption := AParameters.Values['L'];
  FImageRight.Caption := AParameters.Values['R'];
  FNextTrialLeft := AParameters.Values['L'+_NextTrial];
  FNextTrialRight:= AParameters.Values['R'+_NextTrial];
end;

procedure TBinaryChoice.Start;
begin
  FImageLeft.Show;
  FImageLeft.Schedule.Start;

  FImageRight.Show;
  FImageRight.Schedule.Start;
end;

procedure TBinaryChoice.Stop;
begin
  FImageLeft.Hide;
  FImageRight.Hide;
end;

procedure TBinaryChoice.FitScreen;
begin
  FImageLeft.CentralizeLeft;
  FImageRight.CentralizeRight;
end;

end.


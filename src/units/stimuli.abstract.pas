{
  Stimulus Control
  Copyright (C) 2014-2020 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Stimuli.Abstract;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Stimuli, Schedules;

type

  { TStimulus }

  TStimulus = class(TComponent)
  private
    FSchedule : TSchedule;
    FOnConsequence: TNotifyEvent;
    FOnResponse: TNotifyEvent;
    FOnStop: TNotifyEvent;
    procedure Consequence(Sender : TObject);
    procedure Response(Sender : TObject);
    procedure SetOnConsequence(AValue: TNotifyEvent);
    procedure SetOnResponse(AValue: TNotifyEvent);
    procedure SetOnStop(AValue : TNotifyEvent);
  protected
    procedure SetSchedule(AValue: TSchedule); virtual;
  public
    constructor Create(AOwner : TComponent); overload; override;
    constructor Create(AOwner : TComponent;
      ASchedule : TSchedule); virtual; overload;
    function AsString : string; virtual;
    property Schedule : TSchedule read FSchedule write SetSchedule;
    property OnStop : TNotifyEvent read FOnStop write SetOnStop;
    property OnConsequence : TNotifyEvent read FOnConsequence write SetOnConsequence;
    property OnResponse : TNotifyEvent read FOnResponse write SetOnResponse;
  end;

implementation

{ TStimulus }

procedure TStimulus.Consequence(Sender: TObject);
begin
  if Assigned(OnConsequence) then OnConsequence(Self);
end;

procedure TStimulus.Response(Sender: TObject);
begin
  if Assigned(OnResponse) then OnResponse(Self);
end;

procedure TStimulus.SetOnConsequence(AValue: TNotifyEvent);
begin
  if FOnConsequence=AValue then Exit;
  FOnConsequence:=AValue;
end;

procedure TStimulus.SetOnResponse(AValue: TNotifyEvent);
begin
  if FOnResponse=AValue then Exit;
  FOnResponse:=AValue;
end;

procedure TStimulus.SetOnStop(AValue: TNotifyEvent);
begin
  if FOnStop=AValue then Exit;
  FOnStop:=AValue;
end;

procedure TStimulus.SetSchedule(AValue: TSchedule);
begin
  if FSchedule=AValue then Exit;
  FSchedule:=AValue;
end;

constructor TStimulus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSchedule := TSchedule.Create(Self);
  with FSchedule do begin
    OnConsequence:= @Self.Consequence;
    OnResponse:= @Self.Response;
  end;
end;

constructor TStimulus.Create(AOwner : TComponent; ASchedule : TSchedule);
begin
  inherited Create(AOwner);
  FSchedule := ASchedule;
end;

function TStimulus.AsString: string;
begin
  Result := 'Implement me';
end;

end.


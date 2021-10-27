{
  Stimulus Control
  Copyright (C) 2017-2020 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit SerialTimer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls;

type

  TTimerItem = record
    Interval : Cardinal;
    OnTimerEvent : TNotifyEvent;
  end;

  TTimerItems = array of TTimerItem;

  { TSerialTimer }

  TSerialTimer = class(TComponent)
  private
    FOnEndTimeSerie: TNotifyEvent;
    FTimer : TTimer;
    FCurrentItem : Cardinal;
    FTimerItems : TTimerItems;
    procedure SetCurrentItem(AValue: Cardinal);
    procedure SetOnEndTimeSerie(AValue: TNotifyEvent);
    procedure TimerOnTimer(Sender : TObject);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Append(ATimerItem : TTimerItem); overload;
    procedure Append(ATimerItems : TTimerItems); overload;
    procedure Clear;
    procedure Start;
    procedure Stop;
    procedure Restart;
    function Interval(AItem : Cardinal) : Cardinal;
    property CurrentItem : Cardinal read FCurrentItem write SetCurrentItem;
    property OnEndTimeSerie : TNotifyEvent read FOnEndTimeSerie write SetOnEndTimeSerie;
  end;

implementation

{ TSerialTimer }

procedure TSerialTimer.SetCurrentItem(AValue: Cardinal);
begin
  if (FCurrentItem > High(FTimerItems)) or
     (FCurrentItem = AValue) then Exit;
  if FTimer.Enabled then Stop;
  FCurrentItem := AValue;
end;

procedure TSerialTimer.SetOnEndTimeSerie(AValue: TNotifyEvent);
begin
  if FOnEndTimeSerie = AValue then Exit;
  FOnEndTimeSerie := AValue;
end;

procedure TSerialTimer.TimerOnTimer(Sender: TObject);
var
  LTimer : TTimer;
begin
  LTimer := TTimer(Sender);
  LTimer.Enabled := False;
  if Assigned(FTimerItems[FCurrentItem].OnTimerEvent) then begin
    FTimerItems[FCurrentItem].OnTimerEvent(Self);
  end;
  if FCurrentItem < High(FTimerItems) then begin
    Inc(FCurrentItem);
    LTimer.Interval := FTimerItems[FCurrentItem].Interval;
    LTimer.Enabled := True;
  end else
    if Assigned(OnEndTimeSerie) then OnEndTimeSerie(Self);
end;

constructor TSerialTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCurrentItem := 0;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.OnTimer := @TimerOnTimer;
end;

destructor TSerialTimer.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TSerialTimer.Append(ATimerItem: TTimerItem);
begin
  if ATimerItem.Interval > 0 then
    begin
      SetLength(FTimerItems, Length(FTimerItems)+1);
      FTimerItems[High(FTimerItems)] := ATimerItem;
    end;
end;

procedure TSerialTimer.Append(ATimerItems: TTimerItems);
var
  LTimerItem : TTimerItem;
begin
  if Length(ATimerItems) = 0 then Exit;
  for LTimerItem in ATimerItems do
    Append(LTimerItem);
end;

procedure TSerialTimer.Clear;
begin
  if FTimer.Enabled then Stop;
  SetLength(FTimerItems, 0);
  FCurrentItem := 0;
end;

procedure TSerialTimer.Start;
begin
  if Length(FTimerItems) = 0 then Exit;
  FTimer.Interval := FTimerItems[FCurrentItem].Interval;
  FTimer.Enabled := True;
end;

procedure TSerialTimer.Stop;
begin
  FTimer.Enabled := False;
end;

procedure TSerialTimer.Restart;
begin
  if FTimer.Enabled then Stop;
  FCurrentItem := 0;
  Start;
end;

function TSerialTimer.Interval(AItem: Cardinal): Cardinal;
begin
  Result := FTimerItems[AItem].Interval;
end;


end.



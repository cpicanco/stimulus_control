{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit CS.Sound;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Audio.CastleSound, SerialTimer;

type

  { TSerialSound }

  TSerialSound = class(TComponent)
  private
    FTone   : TSound;
    FLaught : TSound;
    FSerialTimer : TSerialTimer;
    FOnStartPlaying: TNotifyEvent;
    FOnStop: TNotifyEvent;
    procedure SetLaught(AValue: TSound);
    procedure SetTone(AValue: TSound);
    procedure StartTone(Sender : TObject);
    procedure StopTone(Sender : TObject);
    procedure StopLaugh(Sender : TObject);
    procedure LoadPresentationPattern;
  public
    constructor Create(AOwner : TComponent); override;
    procedure StartPlayingFromPattern;
    property Tone : TSound read FTone write SetTone;
    property Laught : TSound read FLaught write SetLaught;
  end;

var
  SerialSound : TSerialSound;

implementation

uses Session.Configuration.GlobalContainer, Forms;

{ TSerialSound }

type
  TDelays = 0..4;
  TVTInterval = record
    Start : Cardinal;
    Stop  : Cardinal;
  end;


procedure TSerialSound.StopTone(Sender: TObject);
begin
  FTone.Stop;
end;

procedure TSerialSound.StopLaugh(Sender: TObject);
begin
  FLaught.Stop;
end;

procedure TSerialSound.SetLaught(AValue: TSound);
begin
  if FLaught = AValue then Exit;
  FLaught := AValue;
end;

procedure TSerialSound.SetTone(AValue: TSound);
begin
  if FTone = AValue then Exit;
  FTone := AValue;
end;

procedure TSerialSound.StartTone(Sender: TObject);
begin
  FTone.Play;
end;

procedure TSerialSound.LoadPresentationPattern;
const
  SessionDuration : integer = 15*60*1000;
  TimeEdge : integer = 30*1000;
var
  i : integer;
  Amplitude    : integer;
  BaseTimeUnit : integer;
  Interval : TVTInterval;
  TimerItem  : TTimerItem;
begin
  BaseTimeUnit := ((SessionDuration - (TimeEdge*2)) div Length([TDelays])) div 2;
  Amplitude := (BaseTimeUnit*50) div 100;
  for i := Low(TDelays) to High(TDelays) do begin
    if i = 0 then begin
      TimerItem.Interval := 2000;   // override para testar
    end else begin
      TimerItem.Interval :=
        BaseTimeUnit - Amplitude + Random((2 * BaseTimeUnit) + 1);
    end;
    TimerItem.OnTimerEvent := @StartTone;
    FSerialTimer.Append(TimerItem);

    TimerItem.Interval := Round(FTone.Duration*1000);
    TimerItem.OnTimerEvent := @StopTone;
    FSerialTimer.Append(TimerItem);

    TimerItem.Interval := Round(FLaught.Duration*1000);
    TimerItem.OnTimerEvent := @StopLaugh;
    FSerialTimer.Append(TimerItem);
  end;

end;

constructor TSerialSound.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTone := TSound.Create(Self);
  FTone.LoadFromFile('tom.wav');
  FLaught := TSound.Create(Self);
  FLaught.LoadFromFile('risada-jocosa.wav');
  FSerialTimer := TSerialTimer.Create(Self);
  LoadPresentationPattern;
end;

procedure TSerialSound.StartPlayingFromPattern;
begin
  FSerialTimer.Start;
end;

initialization
  SerialSound := TSerialSound.Create(nil);

finalization
  SerialSound.Free;

end.


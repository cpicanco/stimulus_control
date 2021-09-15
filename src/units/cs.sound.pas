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
  Classes, SysUtils, ExtCtrls, Audio.CastleSound;

type

  { TSerialSound }

  TSerialSound = class(TComponent)
  private
    FAuditive : TSound;
    FOnStartPlaying: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FTimer : TTimer;
    procedure PlayFromPattern(Sender : TObject);
    procedure LoadPresentationPattern;
    procedure SetOnStartPlaying(AValue: TNotifyEvent);
    procedure SetOnStop(AValue: TNotifyEvent);
    procedure Stop(Sender : TObject);
    procedure StartPlaying(Sender : TObject);
  public
    constructor Create(AOwner : TComponent); override;
    procedure StartPlayingFromPattern;
    procedure LoadFromFile(AFilename : string);
    property OnStartPlaying : TNotifyEvent read FOnStartPlaying write SetOnStartPlaying;
    property OnStop : TNotifyEvent read FOnStop write SetOnStop;
  end;

var
  SerialSound : TSerialSound;

implementation

uses Session.Configuration.GlobalContainer, Forms;

{ TSerialSound }

type
  TDelaysIndexes = 0..4;

var
  CurrentDelay : TDelaysIndexes;
  Delays : array [TDelaysIndexes] of Cardinal;

procedure TSerialSound.Stop(Sender: TObject);
begin
  if CurrentDelay < High(TDelaysIndexes) then begin
    Inc(CurrentDelay);
    FTimer.Interval := Delays[CurrentDelay];
    FTimer.Enabled := True;
  end;
  if Assigned(OnStop) then OnStop(Self);
end;

procedure TSerialSound.StartPlaying(Sender: TObject);
begin
  if Assigned(OnStartPlaying) then OnStartPlaying(Self);
end;

procedure TSerialSound.LoadPresentationPattern;
const
  SessionDuration : integer = 15*60*1000;
  TimeEdge : integer = 30*1000;
var
  BaseTimeUnit : integer;
  Amplitude : integer;
  i : integer;
begin
  BaseTimeUnit := ((SessionDuration - (TimeEdge*2)) div Length(Delays)) div 2;
  Amplitude := (BaseTimeUnit*50) div 100;
  for i := Low(Delays) to High(Delays) do begin
    Delays[i] := BaseTimeUnit - Amplitude + Random((2 * BaseTimeUnit) + 1);
  end;
  CurrentDelay := Low(TDelaysIndexes);
  Delays[0] := 2000;
end;

procedure TSerialSound.SetOnStartPlaying(AValue: TNotifyEvent);
begin
  if FOnStartPlaying = AValue then Exit;
  FOnStartPlaying := AValue;
end;

procedure TSerialSound.SetOnStop(AValue: TNotifyEvent);
begin
  if FOnStop = AValue then Exit;
  FOnStop := AValue;
end;

procedure TSerialSound.PlayFromPattern(Sender: TObject);
begin
  FTimer.Enabled := False;
  FAuditive.Play;
end;

constructor TSerialSound.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAuditive := TSound.Create(Self);
  FAuditive.OnStop := @Stop;
  FAuditive.OnStartPlaying := @StartPlaying;
  LoadPresentationPattern;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := Delays[CurrentDelay];
  FTimer.OnTimer := @PlayFromPattern;
end;

procedure TSerialSound.StartPlayingFromPattern;
//var
//  L : integer;
//  Sum : integer = 0;
begin
  FTimer.Enabled := True;
  //for L in Delays do begin
  //  WriteLn(L div 1000);
  //  Inc(Sum, L div 1000);
  //end;
  //Write('Minutes:', Sum div 60);
end;

procedure TSerialSound.LoadFromFile(AFilename: string);
begin
  FAuditive.LoadFromFile(AFilename);
end;

initialization
  SerialSound := TSerialSound.Create(nil);

finalization
  SerialSound.Free;

end.


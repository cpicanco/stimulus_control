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
  Classes, SysUtils, Audio.CastleSound, SerialTimer, Dialogs, DateUtils;

type


  TPresentationPattern = (ppNone, ppB3, ppB4, ppC1);

  { TSerialSound }

  TSerialSound = class(TComponent)
  private
    FOnStop8Seconds: TNotifyEvent;
    FPresentationPattern: TPresentationPattern;
    //FTone   : TSound;
    FToneHigh : TSound;
    //FLaught : TSound;
    FSerialTimer : TSerialTimer;
    FOnStart : TNotifyEvent;
    //FOnStopTone : TNotifyEvent;
    //FOnStopLaught: TNotifyEvent;
    FOnStopToneHigh: TNotifyEvent;
    //procedure SetLaught(AValue: TSound);
    procedure StartToneHigh(Sender: TObject);
    procedure SetOnStart(AValue: TNotifyEvent);
    procedure SetOnStop8Seconds(AValue: TNotifyEvent);
    //procedure SetOnStopLaught(AValue: TNotifyEvent);
    //procedure SetOnStopTone(AValue: TNotifyEvent);
    procedure SetOnStopToneHigh(AValue: TNotifyEvent);
    procedure SetPresentationPattern(AValue: TPresentationPattern);
    //procedure SetTone(AValue: TSound);
    procedure SetToneHigh(AValue: TSound);
    procedure StartTone(Sender : TObject);
    procedure StartGrayScreen(Sender : TObject);
    //procedure StopTone(Sender : TObject);
    procedure StopToneHigh(Sender : TObject);
    //procedure StopLaugh(Sender : TObject);
    procedure Stop8Seconds(Sender : TObject);
    procedure LoadPresentationPattern;
  public
    constructor Create(AOwner : TComponent); override;
    procedure StartPlayingFromPattern;
    property OnStart : TNotifyEvent read FOnStart write SetOnStart;
    //property OnStopTone : TNotifyEvent read FOnStopTone write SetOnStopTone;
    //property OnStopLaught : TNotifyEvent read FOnStopLaught write SetOnStopLaught;
    property OnStop8Seconds : TNotifyEvent read FOnStop8Seconds write SetOnStop8Seconds;
    property OnStopToneHigh : TNotifyEvent read FOnStopToneHigh write SetOnStopToneHigh;
    property PresentationPattern : TPresentationPattern read FPresentationPattern write SetPresentationPattern;
  end;

var
  SerialSound : TSerialSound;

implementation

uses Session.Configuration.GlobalContainer, Forms;

{ TSerialSound }

type
  TDelays = 0..5;
  TDelaysArray = array [TDelays] of integer;
  //TVTInterval = record
  //  Start : Cardinal;
  //  Stop  : Cardinal;
  //end;


//procedure TSerialSound.StopTone(Sender: TObject);
//begin
//  FTone.Stop;
//  if Assigned(OnStopTone) then
//    OnStopTone(Self);
//  if FPresentationPattern = ppB3 then begin
//    FLaught.Play;
//  end;
//end;

procedure TSerialSound.StopToneHigh(Sender: TObject);
begin
  FToneHigh.Stop;
  if Assigned(OnStopToneHigh) then
    OnStopToneHigh(Self);
end;

//procedure TSerialSound.StopLaugh(Sender: TObject);
//begin
//  FLaught.Stop;
//  if Assigned(OnStopLaught) then
//    OnStopLaught(Self);
//  FToneHigh.Play;
//end;

procedure TSerialSound.Stop8Seconds(Sender: TObject);
begin
  if Assigned(OnStop8Seconds) then
    OnStop8Seconds(Self);
  if FPresentationPattern = ppB3 then begin
    FToneHigh.Play;
  end;
  if FPresentationPattern = ppB4 then begin
    FToneHigh.Play;
  end;
end;

//procedure TSerialSound.SetLaught(AValue: TSound);
//begin
//  if FLaught = AValue then Exit;
//  FLaught := AValue;
//end;

procedure TSerialSound.SetOnStart(AValue: TNotifyEvent);
begin
  if FOnStart = AValue then Exit;
  FOnStart := AValue;
end;

procedure TSerialSound.SetOnStop8Seconds(AValue: TNotifyEvent);
begin
  if FOnStop8Seconds=AValue then Exit;
  FOnStop8Seconds:=AValue;
end;

//procedure TSerialSound.SetOnStopLaught(AValue: TNotifyEvent);
//begin
//  if FOnStopLaught = AValue then Exit;
//  FOnStopLaught := AValue;
//end;

//procedure TSerialSound.SetOnStopTone(AValue: TNotifyEvent);
//begin
//  if FOnStopTone = AValue then Exit;
//  FOnStopTone := AValue;
//end;

procedure TSerialSound.SetOnStopToneHigh(AValue: TNotifyEvent);
begin
  if FOnStopToneHigh = AValue then Exit;
  FOnStopToneHigh := AValue;
end;

procedure TSerialSound.SetPresentationPattern(AValue: TPresentationPattern);
begin
  if FPresentationPattern = AValue then Exit;
  FPresentationPattern := AValue;
  LoadPresentationPattern;
end;

//procedure TSerialSound.SetTone(AValue: TSound);
//begin
//  if FTone = AValue then Exit;
//  FTone := AValue;
//end;

procedure TSerialSound.SetToneHigh(AValue: TSound);
begin
  if FToneHigh = AValue then Exit;
  FToneHigh := AValue;
end;

procedure TSerialSound.StartTone(Sender: TObject);
begin
  //FTone.Play;
  if Assigned(OnStart) then
    OnStart(Self);
end;

procedure TSerialSound.StartGrayScreen(Sender: TObject);
begin
  if Assigned(OnStart) then
    OnStart(Self);
end;

procedure TSerialSound.StartToneHigh(Sender: TObject);
begin
  FToneHigh.Play;

  if Assigned(OnStop8Seconds) then
    OnStop8Seconds(Self);
end;

procedure TSerialSound.LoadPresentationPattern;
const
  TimeUnitC1 = 172000;
  HighToneDuration = 20000;
var
  i : Integer;
  TimerItem  : TTimerItem;
  AbsoluteTimes: array[0..4] of Integer;
  LastTime: Integer;
begin
  case FPresentationPattern of
    ppB3 : begin
      AbsoluteTimes[0] := 150000;
      AbsoluteTimes[1] := 230000;
      AbsoluteTimes[2] := 290000;
      AbsoluteTimes[3] := 430000;
      AbsoluteTimes[4] := 490000;

      LastTime := 0;

      for i := Low(AbsoluteTimes) to High(AbsoluteTimes) do begin
        TimerItem.Interval := AbsoluteTimes[i] - LastTime;
        TimerItem.OnTimerEvent := @StartToneHigh;
        FSerialTimer.Append(TimerItem);

        LastTime := AbsoluteTimes[i];

        TimerItem.Interval := HighToneDuration;
        TimerItem.OnTimerEvent := @StopToneHigh;
        FSerialTimer.Append(TimerItem);

        LastTime := LastTime + HighToneDuration;
      end;
    end;

    ppB4 : begin
      AbsoluteTimes[0] := 20000;
      AbsoluteTimes[1] := 95000;
      AbsoluteTimes[2] := 210000;
      AbsoluteTimes[3] := 360000;
      AbsoluteTimes[4] := 530000;

      LastTime := 0;

      for i := Low(AbsoluteTimes) to High(AbsoluteTimes) do begin
        TimerItem.Interval := AbsoluteTimes[i] - LastTime;
        TimerItem.OnTimerEvent := @StartToneHigh;
        FSerialTimer.Append(TimerItem);

        LastTime := AbsoluteTimes[i];

        TimerItem.Interval := HighToneDuration;
        TimerItem.OnTimerEvent := @StopToneHigh;
        FSerialTimer.Append(TimerItem);

        LastTime := LastTime + HighToneDuration;
      end;
    end;

    ppC1 : begin
      for i := Low(TDelays) to High(TDelays) do begin
        TimerItem.Interval := TimeUnitC1;
        TimerItem.OnTimerEvent := @StartGrayScreen;
        FSerialTimer.Append(TimerItem);

        TimerItem.Interval := Round(8000);
        TimerItem.OnTimerEvent := @Stop8Seconds;
        FSerialTimer.Append(TimerItem);
      end;
    end;
  end;
end;

constructor TSerialSound.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //FTone := TSound.Create(Self);
  //FTone.LoadFromFile('tom.wav');
  //FTone.OnStop := @StopTone;

  //FLaught := TSound.Create(Self);
  //FLaught.LoadFromFile('risada-jocosa.wav');
  //FLaught.OnStop := @StopLaugh;

  FToneHigh := TSound.Create(Self);
  FToneHigh.LoadFromFile('tom-alto-20s.wav');
  //FToneHigh.OnStop := @StopToneHigh;

  FSerialTimer := TSerialTimer.Create(Self);
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


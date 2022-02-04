{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Stimuli.Sequence.DMTS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics
  , Stimuli
  , Stimuli.Abstract
  , Controls.ObsConfKey
  , Schedules
  , SerialTimer
  , Timestamps
  ;

type
  { TTBExpectedResponse }

  TTBExpectedResponse = (tbNone, tbC1, tbC2, tbC3);

  { TDMTSSequence }

  TDMTSSequence = class(TStimulus, IStimuli)
  private
    FDelayBegin: Extended;
    FLogEvent: TLogEvent;
    FStyle : TKeyStyle;
    FCursor : integer;
    FExpectedResponse : TTBExpectedResponse;
    FParent : TWinControl;
    FSerialTimer : TSerialTimer;
    FC3 : TKey;
    FC2 : TKey;
    FC1 : TKey;
    FSample : TKey;
    function GetCursor: integer;
    function GetOnEndSerialTimer: TNotifyEvent;
    function GetParent: TWinControl;
    function GetSampleDuration: Cardinal;
    procedure SetCursor(AValue: integer);
    procedure SetExpectedResponse(AValue: TTBExpectedResponse);
    procedure SetOnEndSerialTimer(AValue: TNotifyEvent);
    procedure SetParent(AValue: TWinControl);
    procedure HideSample(Sender : TObject);
    procedure ShowComparisons(Sender : TObject);
  protected
    procedure SetSchedule(ASchedule : TSchedule); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function AsInterface : IStimuli;
    function Comparison(Sender : TObject) : TKey;
    procedure DoExpectedResponse;
    procedure LoadFromParameters(AParameters : TStringList);
    procedure SetScheduleConsequence(AConsequence : TNotifyEvent);
    procedure Start;
    procedure Stop;
    procedure FitScreen;
    property LogEvent : TLogEvent read FLogEvent write FLogEvent;
    property ExpectedResponse : TTBExpectedResponse read FExpectedResponse write SetExpectedResponse;
    property Parent : TWinControl read FParent write SetParent;
    property OnEndSerialTimer : TNotifyEvent read GetOnEndSerialTimer write SetOnEndSerialTimer;
    property SampleDuration : Cardinal read GetSampleDuration;
    property Cursor : integer read GetCursor write SetCursor;
    property DelayBegin : Extended read FDelayBegin;
    property Comparison1 : TKey read FC1;
    property Comparison2 : TKey read FC2;
    property Comparison3 : TKey read FC3;
    property Sample : TKey read FSample;
  end;

implementation

uses Forms, Constants, Cheats;

{ TDMTSSequence }

procedure TDMTSSequence.SetParent(AValue: TWinControl);
begin
  if FParent=AValue then Exit;
  FParent := AValue;
  FC3.Parent := AValue;
  FC2.Parent := AValue;
  FC1.Parent := AValue;
  FSample.Parent := AValue;
end;

procedure TDMTSSequence.HideSample(Sender: TObject);
begin
  FSample.Stop;
  FDelayBegin := LogEvent('S.Delay.Inicio');
end;


procedure TDMTSSequence.ShowComparisons(Sender: TObject);
begin
  FC1.Show;
  FC2.Show;
  FC3.Show;
end;

function TDMTSSequence.GetOnEndSerialTimer: TNotifyEvent;
begin
  Result := FSerialTimer.OnEndTimeSerie;
end;

function TDMTSSequence.GetCursor: integer;
begin
  Result := FCursor;
end;

function TDMTSSequence.GetParent: TWinControl;
begin
  Result := FParent;
end;

function TDMTSSequence.GetSampleDuration: Cardinal;
begin
  Result := FSerialTimer.Interval(0);
end;

procedure TDMTSSequence.SetCursor(AValue: integer);
begin
  if FCursor = AValue then Exit;
  FCursor := AValue;
end;

procedure TDMTSSequence.SetExpectedResponse(AValue: TTBExpectedResponse);
begin
  if FExpectedResponse=AValue then Exit;
  FExpectedResponse:=AValue;
end;

procedure TDMTSSequence.SetOnEndSerialTimer(AValue: TNotifyEvent);
begin
  if FSerialTimer.OnEndTimeSerie=AValue then Exit;
  FSerialTimer.OnEndTimeSerie:=AValue;
end;

procedure TDMTSSequence.SetSchedule(ASchedule: TSchedule);
begin
  inherited SetSchedule(ASchedule);
end;

constructor TDMTSSequence.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExpectedResponse:=tbNone;

  FC3 := TKey.Create(Self);
  FC3.Schedule := TSchedule.Create(Self);
  FC3.Schedule.Name := 'C3';
  FC3.Schedule.Load(CRF);

  FC2 := TKey.Create(Self);
  FC2.Schedule := TSchedule.Create(Self);
  FC2.Schedule.Name := 'C2';
  FC2.Schedule.Load(CRF);

  FC1 := TKey.Create(Self);
  FC1.Schedule := TSchedule.Create(Self);
  FC1.Schedule.Name := 'C1';
  FC1.Schedule.Load(CRF);

  FSample := TKey.Create(Self);
  FSample.Schedule := TSchedule.Create(Self);
  FSample.Schedule.Name := 'S';
  FSample.Schedule.Load(EXT);

  FSerialTimer := TSerialTimer.Create(Self);
end;

destructor TDMTSSequence.Destroy;
begin
  inherited Destroy;
end;

function TDMTSSequence.AsInterface : IStimuli;
begin
  Result := Self;
end;

function TDMTSSequence.Comparison(Sender: TObject): TKey;
begin
  if Sender is TKey then begin
    Result := TKey(Sender);
  end;
end;

procedure TDMTSSequence.DoExpectedResponse;
var
  LComparison : TKey;
begin
  case ExpectedResponse of
    tbC1 : begin
      LComparison := Comparison1;
    end;

    tbC2: begin
      LComparison := Comparison2;
    end;

    tbC3: begin
      LComparison := Comparison3;
    end;

    tbNone: begin
      case Random(3) of
        0 : LComparison := Comparison1;
        1 : LComparison := Comparison2;
        2 : LComparison := Comparison3;
      end;
    end;
  end;
  LComparison.DoExpectedResponse;
end;

procedure TDMTSSequence.LoadFromParameters(AParameters: TStringList);
var
  LTimerItems : TTimerItems = nil;
begin
  SetLength(LTimerItems, 2);
  LTimerItems[0].OnTimerEvent := @HideSample;
  LTimerItems[1].OnTimerEvent := @ShowComparisons;
  FSerialTimer.Clear;

  case AParameters.Values[_Style] of
    'Auditive' : FStyle := ksAuditive;
    'Visual'   : FStyle := ksVisual;
  end;

  ExpectedResponse := tbC1;

  FSample.Key := _Samp;
  FC1.Key := 'C1';
  FC2.Key := 'C2';
  FC3.Key := 'C3';

  FSample.LoadFromParameters(AParameters);
  FC1.LoadFromParameters(AParameters);
  FC2.LoadFromParameters(AParameters);
  FC3.LoadFromParameters(AParameters);

  LTimerItems[0].Interval := 2000;
  LTimerItems[1].Interval := AParameters.Values[_Delay].ToInteger;
  FSerialTimer.Append(LTimerItems);
end;

procedure TDMTSSequence.SetScheduleConsequence(AConsequence: TNotifyEvent);
begin
  FC1.OnConsequence := AConsequence;
  FC2.OnConsequence := AConsequence;
  FC3.OnConsequence := AConsequence;
end;

procedure TDMTSSequence.Start;
begin
  FSample.Start;
  FSerialTimer.Start;
end;

procedure TDMTSSequence.Stop;
begin
  FSerialTimer.Stop;
  FSample.Stop;
  FC1.Stop;
  FC2.Stop;
  FC3.Stop;
end;

procedure TDMTSSequence.FitScreen;
begin
  FSample.FitScreen;
  FC1.FitScreen;
  FC2.FitScreen;
  FC3.FitScreen;
end;

end.


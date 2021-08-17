{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Stimuli.Sequence.TemporalBissection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, StdCtrls
  , Stimuli.Abstract
  , Stimuli.Image.Base
  , Schedules
  , SerialTimer
  ;

type
  { TTBExpectedResponse }

  TTBExpectedResponse = (tbNone, tbLeft, tbRight);

  { TTBSequence }

  TTBSequence = class(TStimulus)
  private
    FCursor : integer;
    FExpectedResponse : TTBExpectedResponse;
    FParent : TWinControl;
    FSerialTimer : TSerialTimer;
    FImageLeft : TLightImage;
    FImageRight : TLightImage;
    FImageMid : TLightImage;
    function GetCursor: integer;
    function GetOnEndSerialTimer: TNotifyEvent;
    function GetParent: TWinControl;
    function GetSampleDuration: Cardinal;
    procedure SetCursor(AValue: integer);
    procedure SetExpectedResponse(AValue: TTBExpectedResponse);
    procedure SetOnEndSerialTimer(AValue: TNotifyEvent);
    procedure SetParent(AValue: TWinControl);
    procedure ShowLeftRightImages(Sender : TObject);
  protected
    procedure SetSchedule(ASchedule : TSchedule); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(AFilename: string); override;
    procedure LoadFromParameters(AParameters : TStringList);
    procedure SetScheduleConsequence(AConsequence : TNotifyEvent);
    procedure Start; override;
    procedure Stop; override;
    procedure FitScreen;
    property ExpectedResponse : TTBExpectedResponse read FExpectedResponse write SetExpectedResponse;
    property Parent : TWinControl read FParent write SetParent;
    property OnEndSerialTimer : TNotifyEvent read GetOnEndSerialTimer write SetOnEndSerialTimer;
    property SampleDuration : Cardinal read GetSampleDuration;
    property Cursor : integer read GetCursor write SetCursor;
    property ComparisonLeft  : TLightImage read FImageLeft;
    property ComparisonRight  : TLightImage read FImageRight;
    property Sample : TLightImage read FImageMid;
  end;

implementation

uses Forms, Constants, Cheats;

{ TTBSequence }

procedure TTBSequence.SetParent(AValue: TWinControl);
begin
  if FParent=AValue then Exit;
  FParent := AValue;
  FImageLeft.Parent := AValue;
  FImageRight.Parent := AValue;
  FImageMid.Parent := AValue;
end;


procedure TTBSequence.ShowLeftRightImages(Sender: TObject);
begin
  FImageMid.Hide;
  FImageLeft.Show;
  FImageLeft.Schedule.Start;

  FImageRight.Show;
  FImageRight.Schedule.Start;
end;

function TTBSequence.GetOnEndSerialTimer: TNotifyEvent;
begin
  Result := FSerialTimer.OnEndTimeSerie;
end;

function TTBSequence.GetCursor: integer;
begin
  Result := FCursor;
end;

function TTBSequence.GetParent: TWinControl;
begin
  Result := FParent;
end;

function TTBSequence.GetSampleDuration: Cardinal;
begin
  Result := FSerialTimer.Interval(0);
end;

procedure TTBSequence.SetCursor(AValue: integer);
begin
  if FCursor = AValue then Exit;
  FCursor := AValue;
  FImageLeft.Cursor  := FCursor;
  FImageMid.Cursor   := FCursor;
  FImageRight.Cursor := FCursor;
end;

procedure TTBSequence.SetExpectedResponse(AValue: TTBExpectedResponse);
begin
  if FExpectedResponse=AValue then Exit;
  FExpectedResponse:=AValue;
end;

procedure TTBSequence.SetOnEndSerialTimer(AValue: TNotifyEvent);
begin
  if FSerialTimer.OnEndTimeSerie=AValue then Exit;
  FSerialTimer.OnEndTimeSerie:=AValue;
end;

procedure TTBSequence.SetSchedule(ASchedule: TSchedule);
begin
  inherited SetSchedule(ASchedule);
end;

constructor TTBSequence.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExpectedResponse:=tbLeft;

  FImageLeft := TLightImage.Create(Self);
  FImageLeft.Kind:=ikCircle;
  FImageLeft.Visible := False;
  FImageLeft.Schedule := TSchedule.Create(Self);
  FImageLeft.Schedule.Name := 'Left';
  FImageLeft.Schedule.Load(CRF);
  FImageLeft.EdgeColor:=clGray;
  FImageLeft.Color := clGray;

  FImageRight := TLightImage.Create(Self);
  FImageRight.Kind:=ikCircle;
  FImageRight.Visible := False;
  FImageRight.Schedule := TSchedule.Create(Self);
  FImageRight.Schedule.Name := 'Right';
  FImageRight.Schedule.Load(CRF);
  FImageRight.EdgeColor:=clGray;
  FImageRight.Color:= clGray;

  FImageMid := TLightImage.Create(Self);
  FImageMid.Kind:=ikSquare;
  FImageMid.Visible := False;
  FImageMid.Color  := clGray;

  FSerialTimer := TSerialTimer.Create(Self);
end;

destructor TTBSequence.Destroy;
begin
  inherited Destroy;
end;

procedure TTBSequence.LoadFromFile(AFilename: string);
begin
  { Implement me }
end;

procedure TTBSequence.LoadFromParameters(AParameters: TStringList);
var
  LTimerItems : TTimerItems;
  LExpectedResponse : string;
begin
  SetLength(LTimerItems, 1);
  LTimerItems[0].OnTimerEvent := @ShowLeftRightImages;
  FSerialTimer.Clear;

  LExpectedResponse := AParameters.Values[_ExpectedResponse];
  case LExpectedResponse of
    'Left' :
      begin
        { long }
        ExpectedResponse := tbLeft;
        if CheatsModeOn then begin
          LTimerItems[0].Interval := DurationLong div 10;
        end else begin
          LTimerItems[0].Interval := DurationLong;
        end;
      end;
    'Right' :
      begin
        { short }
        ExpectedResponse := tbRight;
        if CheatsModeOn then begin
          LTimerItems[0].Interval := DurationShort div 10;
        end else begin
          LTimerItems[0].Interval := DurationShort;
        end;
      end;
    else
      begin
        { generalization }
        ExpectedResponse := tbNone;
        if CheatsModeOn then begin
          LTimerItems[0].Interval := LExpectedResponse.ToInteger div 10;
        end else begin
          LTimerItems[0].Interval := LExpectedResponse.ToInteger;
        end;
      end;

  end;
  FSerialTimer.Append(LTimerItems);
end;

procedure TTBSequence.SetScheduleConsequence(AConsequence: TNotifyEvent);
begin
  FImageLeft.Schedule.OnConsequence := AConsequence;
  FImageRight.Schedule.OnConsequence := AConsequence;
end;

procedure TTBSequence.Start;
begin
  FImageMid.Show;
  FSerialTimer.Start;
end;

procedure TTBSequence.Stop;
begin
  FSerialTimer.Stop;
  FImageMid.Hide;
  FImageLeft.Hide;
  FImageRight.Hide;
end;

procedure TTBSequence.FitScreen;
var
  LSize : integer;
begin
  LSize := (Screen.Height div 4);
  with FImageLeft do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 4) - (Width div 2);
    Top := (Screen.Height div 2) - (Height div 2);
  end;

  with FImageRight do
  begin
    Width := LSize;
    Height:= LSize;
    Left := ((Screen.Width div 4)*3) - (Width div 2);
    Top := (Screen.Height div 2) - (Height div 2);
  end;

  with FImageMid do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2);
    Top := (Screen.Height div 2) - (Height div 2);
  end;
end;

end.

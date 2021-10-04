{
  Stimulus Control
  Copyright (C) 2014-2020 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit Stimuli.Sequence.ContextualMatchingToSample;

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

  { TCMTSTrialType }
  TCMTSTrialType = (Equal, Different);

  { TCMTSExpectedResponse }
  TCMTSExpectedResponse = (Left, Right);

  { TCMTSSequence }
  TCMTSSequence = class(TStimulus)
  private
    FTrialType : TCMTSTrialType;
    FExpectedResponse : TCMTSExpectedResponse;
    FParent : TWinControl;
    FSerialTimer : TSerialTimer;
    FLeftBottomImage : TLightImage;
    FRightBottomImage : TLightImage;
    FMidImage : TLightImage;
    FTopImage : TLightImage;
    function GetOnEndSerialTimer: TNotifyEvent;
    function GetParent: TWinControl;
    procedure SetExpectedResponse(AValue: TCMTSExpectedResponse);
    procedure SetOnEndSerialTimer(AValue: TNotifyEvent);
    procedure SetParent(AValue: TWinControl);
    procedure SetTrialType(AValue: TCMTSTrialType);
    procedure ShowMidImage(Sender : TObject);
    procedure ShowTopLabel(Sender : TObject);
  protected
    procedure SetSchedule(ASchedule : TSchedule); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(AFilename: string);
    procedure LoadFromParameters(AParameters : TStringList);
    procedure Start;
    procedure Stop;
    procedure FitScreen;
    property ExpectedResponse : TCMTSExpectedResponse read FExpectedResponse write SetExpectedResponse;
    property TrialType : TCMTSTrialType read FTrialType write SetTrialType;
    property Parent : TWinControl read FParent write SetParent;
    property OnEndSerialTimer : TNotifyEvent read GetOnEndSerialTimer write SetOnEndSerialTimer;
  end;

implementation

uses Forms, Constants;

{ TCMTSSequence }


procedure TCMTSSequence.SetParent(AValue: TWinControl);
begin
  if FParent=AValue then Exit;
  FParent := AValue;
  FTopImage.Parent := AValue;
  FLeftBottomImage.Parent := AValue;
  FRightBottomImage.Parent := AValue;
  FMidImage.Parent := AValue;
end;

procedure TCMTSSequence.SetTrialType(AValue: TCMTSTrialType);
begin
  if FTrialType=AValue then Exit;
  FTrialType:=AValue;
end;

procedure TCMTSSequence.ShowMidImage(Sender: TObject);
begin
  FMidImage.Show;
end;

procedure TCMTSSequence.ShowTopLabel(Sender: TObject);
begin
  FTopImage.Show;
end;

function TCMTSSequence.GetParent: TWinControl;
begin
  Result := FParent;
end;

function TCMTSSequence.GetOnEndSerialTimer: TNotifyEvent;
begin
  Result := FSerialTimer.OnEndTimeSerie;
end;

procedure TCMTSSequence.SetExpectedResponse(AValue: TCMTSExpectedResponse);
begin
  if FExpectedResponse=AValue then Exit;
  FExpectedResponse:=AValue;
end;

procedure TCMTSSequence.SetOnEndSerialTimer(AValue: TNotifyEvent);
begin
  if FSerialTimer.OnEndTimeSerie=AValue then Exit;
  FSerialTimer.OnEndTimeSerie:=AValue;
end;

procedure TCMTSSequence.SetSchedule(ASchedule: TSchedule);
begin
  inherited SetSchedule(ASchedule);
end;

constructor TCMTSSequence.Create(AOwner: TComponent);
var
  LTimerItems : TTimerItems;
begin
  inherited Create(AOwner);
  FTrialType:=Equal;
  FExpectedResponse:=Left;
  FLeftBottomImage := TLightImage.Create(Self);
  FRightBottomImage := TLightImage.Create(Self);
  FMidImage := TLightImage.Create(Self);
  FTopImage := TLightImage.Create(Self);

  SetLength(LTimerItems, 2);
  LTimerItems[0].Interval := 500;
  LTimerItems[0].OnTimerEvent := @ShowMidImage;
  LTimerItems[1].Interval := 500;
  LTimerItems[1].OnTimerEvent := @ShowTopLabel;

  FSerialTimer := TSerialTimer.Create(Self);
  FSerialTimer.Clear;
  FSerialTimer.Append(LTimerItems);
end;

destructor TCMTSSequence.Destroy;
begin
  inherited Destroy;
end;

procedure TCMTSSequence.LoadFromFile(AFilename: string);
var
  LStringList : TStringList;
begin
  LStringList := TStringList.Create;
  try
    LStringList.LoadFromFile(AFilename);
    FLeftBottomImage.LoadFromFile(LStringList[0]);
    FMidImage.Caption := LStringList[0];
    FRightBottomImage.LoadFromFile(LStringList[1]);
    FTopImage.Caption := LStringList[2];
  finally
    LStringList.Free;
  end;
end;


procedure TCMTSSequence.LoadFromParameters(AParameters: TStringList);
var
  LStm1 : string;
  LStm2 : string;
  LTopStimulus : string;
  procedure SetLabel(ALabel : TLabel);
  begin
    ALabel.Visible:=False;
    ALabel.AutoSize:=False;
    ALabel.Font.Color:=clBlack;
    ALabel.Font.Size:=45;
    ALabel.Font.Bold:=True;
    ALabel.Alignment := taCenter;
    ALabel.Layout := tlCenter;
  end;
begin
  LStm1 := AParameters.Values[_Comp + '1' + _cStm];
  LStm2 := AParameters.Values[_Comp + '2' + _cStm];
  LTopStimulus := AParameters.Values[_Comp + '3' + _cStm];
  if ExtractFileExt(LTopStimulus) = '' then
    FTopImage.Caption := LTopStimulus
  else
    FTopImage.LoadFromFile(LTopStimulus);
  FMidImage.LoadFromFile(LStm1);

  case AParameters.Values[_ExpectedResponse] of
    'Left' : ExpectedResponse := Left;
    'Right' : ExpectedResponse := Right;
  end;

  case AParameters.Values[_Type] of
    'Equal' :
    begin
      TrialType := Equal;
      case ExpectedResponse of
        Left :
          begin
            FLeftBottomImage.LoadFromFile(LStm1);
            FRightBottomImage.LoadFromFile(LStm2);
          end;
        Right :
          begin
            FRightBottomImage.LoadFromFile(LStm1);
            FLeftBottomImage.LoadFromFile(LStm2);
          end;
      end;
    end;
    'Different'  :
      begin
        TrialType := Different;
        case ExpectedResponse of
          Left :
            begin
              FLeftBottomImage.LoadFromFile(LStm2);
              FRightBottomImage.LoadFromFile(LStm1);
            end;
          Right :
            begin
              FRightBottomImage.LoadFromFile(LStm2);
              FLeftBottomImage.LoadFromFile(LStm1);
            end;
        end;
      end;
  end;
end;

procedure TCMTSSequence.Start;
begin
  FLeftBottomImage.Show;
  FRightBottomImage.Show;
  FSerialTimer.Start;
end;

procedure TCMTSSequence.Stop;
begin
  FSerialTimer.Stop;
  FTopImage.Hide;
  FMidImage.Hide;
  FLeftBottomImage.Hide;
  FRightBottomImage.Hide;
end;

procedure TCMTSSequence.FitScreen;
var
  LSize : integer;
const
  LGap : integer = 20;
begin
  LSize := (Screen.Height div 4) - (4*LGap);
  with FLeftBottomImage do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 4) - (Width div 2);
    Top := ((Screen.Height div 4)*3)-LGap;
  end;

  with FRightBottomImage do
  begin
    Width := LSize;
    Height:= LSize;
    Left := ((Screen.Width div 4)*3) - (Width div 2);
    Top := ((Screen.Height div 4)*3)-LGap;
  end;

  with FMidImage do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2);
    Top := (Screen.Height div 2) - (Height div 2);
  end;

  with FTopImage do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2);
    Top := FRightBottomImage.Top - LGap - (FRightBottomImage.Height*4);
  end;
end;

end.

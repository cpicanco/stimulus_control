{
  Stimulus Control
  Copyright (C) 2014-2020 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Stimuli.Sequence.BeforeAfter;

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

  { TTrialType }
  TTrialType = (Before, After);

  { TExpectedResponse }
  TExpectedResponse = (Left, Right);

  { TBeforeAfterSequence }

  TBeforeAfterSequence = class(TStimulus)
  private
    FTrialType : TTrialType;
    FExpectedResponse : TExpectedResponse;
    FParent : TWinControl;
    FSerialTimer : TSerialTimer;
    FLeftBottomImage : TLightImage;
    FLeftMidImage : TLightImage;
    FFirstTopImage : TLightImage;
    FLeftLabel : TLightImage;
    FRightBottomImage : TLightImage;
    FRightMidImage : TLightImage;
    FSecondTopImage : TLightImage;
    FRightLabel : TLightImage;
    FTopLabel : TLabel;
    function GetOnEndSerialTimer: TNotifyEvent;
    function GetParent: TWinControl;
    procedure SetExpectedResponse(AValue: TTrialType);
    procedure SetExpectedResponse(AValue: TExpectedResponse);
    procedure SetOnEndSerialTimer(AValue: TNotifyEvent);
    procedure SetParent(AValue: TWinControl);
    procedure SetTrialType(AValue: TTrialType);
    procedure ShowLabel(Sender : TObject);
    procedure ShowMid(Sender : TObject);
    procedure ShowTopLeft(Sender : TObject);
    procedure HideTopLeft(Sender : TObject);
    procedure ShowTopRight(Sender : TObject);
    procedure HideTopRight(Sender : TObject);
    procedure ShowInstruction(Sender : TObject);
    procedure SetShowType1; // has no final instruction at the top
    procedure SetShowType2; // has a final instruction at the top
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
    property ExpectedResponse : TExpectedResponse read FExpectedResponse write SetExpectedResponse;
    property TrialType : TTrialType read FTrialType write SetTrialType;
    property Parent : TWinControl read FParent write SetParent;
    property OnEndSerialTimer : TNotifyEvent read GetOnEndSerialTimer write SetOnEndSerialTimer;
  end;

implementation

uses Forms, Constants;

{ TBeforeAfterSequence }

const
  {$IFDEF DEBUG}
  BaseTimeFastForward : integer = 100;
  {$ELSE}
  BaseTime1 : integer = 500;
  BaseTime2 : integer = 1000;
  {$ENDIF}


procedure TBeforeAfterSequence.SetParent(AValue: TWinControl);
begin
  if FParent=AValue then Exit;
  FParent := AValue;
  FTopLabel.Parent := AValue;
  FLeftBottomImage.Parent := AValue;
  FLeftMidImage.Parent := AValue;
  FFirstTopImage.Parent := AValue;
  FLeftLabel.Parent := AValue;
  FRightBottomImage.Parent := AValue;
  FRightMidImage.Parent := AValue;
  FSecondTopImage.Parent := AValue;
  FRightLabel.Parent := AValue;
end;

procedure TBeforeAfterSequence.SetTrialType(AValue: TTrialType);
begin
  if FTrialType=AValue then Exit;
  FTrialType:=AValue;
end;

procedure TBeforeAfterSequence.ShowLabel(Sender: TObject);
begin
  FLeftLabel.Show;
  FRightLabel.Show;
end;

procedure TBeforeAfterSequence.ShowMid(Sender: TObject);
begin
  FLeftMidImage.Show;
  FRightMidImage.Show;
end;

procedure TBeforeAfterSequence.ShowTopLeft(Sender: TObject);
begin
  FFirstTopImage.Show;
end;

procedure TBeforeAfterSequence.HideTopLeft(Sender: TObject);
begin
  FFirstTopImage.Hide;
end;

procedure TBeforeAfterSequence.ShowTopRight(Sender: TObject);
begin
  FSecondTopImage.Show;
end;

procedure TBeforeAfterSequence.HideTopRight(Sender: TObject);
begin
  FSecondTopImage.Hide;
end;

procedure TBeforeAfterSequence.ShowInstruction(Sender: TObject);
begin
  FTopLabel.Show;
end;

procedure TBeforeAfterSequence.SetShowType1;
var
  LTimerItems : TTimerItems;
  LBaseTime1, LBaseTime2 : integer;
begin
  LTimerItems := Default(TTimerItems);
  {$IFDEF DEBUG}
  LBasetime1 := BaseTimeFastForward;
  LBasetime2 := BaseTimeFastForward;
  {$ELSE}
  LBaseTime1 := BaseTime1;
  LBaseTime2 := BaseTime2;
  {$ENDIF}

  SetLength(LTimerItems, 6);
  LTimerItems[0].Interval := LBaseTime1;
  LTimerItems[0].OnTimerEvent := @ShowLabel;
  LTimerItems[1].Interval := LBaseTime1;
  LTimerItems[1].OnTimerEvent := @ShowMid;
  LTimerItems[2].Interval := LBaseTime1;
  LTimerItems[2].OnTimerEvent := @ShowTopLeft;
  LTimerItems[3].Interval := LBaseTime2;
  LTimerItems[3].OnTimerEvent := @HideTopLeft;
  LTimerItems[4].Interval := LBaseTime1;
  LTimerItems[4].OnTimerEvent := @ShowTopRight;
  LTimerItems[5].Interval := LBaseTime2;
  LTimerItems[5].OnTimerEvent := @HideTopRight;

  FSerialTimer.Clear;
  FSerialTimer.Append(LTimerItems);
end;

procedure TBeforeAfterSequence.SetShowType2;
var
  LTimerItems : TTimerItems;
begin
  LTimerItems := Default(TTimerItems);
  FTopLabel.Visible:=False;
  FTopLabel.AutoSize:=False;
  FTopLabel.Color := clDefault;
  FTopLabel.Font.Size:=18;
  FTopLabel.Alignment := taCenter;
  FTopLabel.Layout := tlCenter;
  FToplabel.WordWrap:=True;

  SetLength(LTimerItems, 7);
  LTimerItems[0].Interval := 500;
  LTimerItems[0].OnTimerEvent := @ShowLabel;
  LTimerItems[1].Interval := 500;
  LTimerItems[1].OnTimerEvent := @ShowMid;
  LTimerItems[2].Interval := 500;
  LTimerItems[2].OnTimerEvent := @ShowTopLeft;
  LTimerItems[3].Interval := 1000;
  LTimerItems[3].OnTimerEvent := @HideTopLeft;
  LTimerItems[4].Interval := 500;
  LTimerItems[4].OnTimerEvent := @ShowTopRight;
  LTimerItems[5].Interval := 1000;
  LTimerItems[5].OnTimerEvent := @HideTopRight;
  LTimerItems[6].Interval := 1000;
  LTimerItems[6].OnTimerEvent := @ShowInstruction;

  FSerialTimer.Clear;
  FSerialTimer.Append(LTimerItems);
end;

function TBeforeAfterSequence.GetParent: TWinControl;
begin
  Result := FParent;
end;

function TBeforeAfterSequence.GetOnEndSerialTimer: TNotifyEvent;
begin
  Result := FSerialTimer.OnEndTimeSerie;
end;

procedure TBeforeAfterSequence.SetExpectedResponse(AValue: TTrialType);
begin
  if FTrialType=AValue then Exit;
  FTrialType:=AValue;
end;

procedure TBeforeAfterSequence.SetExpectedResponse(AValue: TExpectedResponse);
begin
  if FExpectedResponse=AValue then Exit;
  FExpectedResponse:=AValue;
end;

procedure TBeforeAfterSequence.SetOnEndSerialTimer(AValue: TNotifyEvent);
begin
  if FSerialTimer.OnEndTimeSerie=AValue then Exit;
  FSerialTimer.OnEndTimeSerie:=AValue;
end;

procedure TBeforeAfterSequence.SetSchedule(ASchedule: TSchedule);
begin
  inherited SetSchedule(ASchedule);
end;

constructor TBeforeAfterSequence.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTrialType:=Before;
  FExpectedResponse:=Left;
  FLeftBottomImage := TLightImage.Create(Self);
  FLeftLabel := TLightImage.Create(Self);
  FLeftMidImage := TLightImage.Create(Self);
  FFirstTopImage := TLightImage.Create(Self);
  FRightLabel := TLightImage.Create(Self);
  FSecondTopImage := TLightImage.Create(Self);
  FRightBottomImage := TLightImage.Create(Self);
  FRightMidImage := TLightImage.Create(Self);
  FTopLabel := TLabel.Create(Self);
  FSerialTimer := TSerialTimer.Create(Self);
end;

destructor TBeforeAfterSequence.Destroy;
begin
  inherited Destroy;
end;

procedure TBeforeAfterSequence.LoadFromFile(AFilename: string);
var
  LStringList : TStringList;
begin
  LStringList := TStringList.Create;
  try
    LStringList.LoadFromFile(AFilename);
    FFirstTopImage.LoadFromFile(LStringList[0]);
    FLeftBottomImage.LoadFromFile(LStringList[0]);
    FLeftLabel.Caption := LStringList[1];

    FLeftMidImage.LoadFromFile(LStringList[2]);

    FSecondTopImage.LoadFromFile(LStringList[2]);
    FRightBottomImage.LoadFromFile(LStringList[2]);
    FRightLabel.Caption := LStringList[1];
    FRightMidImage.LoadFromFile(LStringList[0]);

    FTopLabel.Caption := LStringList[3];
  finally
    LStringList.Free;
  end;
end;

procedure TBeforeAfterSequence.LoadFromParameters(AParameters: TStringList);
var
  LStm1 : string;
  LStm2 : string;
  LLabel : string;
begin
  LStm1 := AParameters.Values[_Comp + '1' + _cStm];
  LStm2 := AParameters.Values[_Comp + '2' + _cStm];
  LLabel := AParameters.Values[_Comp + '3' + _cStm];
  case AParameters.Values[_Type] of
    'Before' : TrialType := Before;
    'After'  : TrialType := After;
  end;

  case TrialType of
    Before :
      begin
        FFirstTopImage.LoadFromFile(LStm1);
        FSecondTopImage.LoadFromFile(LStm2);
      end;
    After  :
      begin
        FFirstTopImage.LoadFromFile(LStm2);
        FSecondTopImage.LoadFromFile(LStm1);
      end;
  end;

  case AParameters.Values[_ExpectedResponse] of
    'Left' :
      begin
        ExpectedResponse:=Left;
        FLeftBottomImage.LoadFromFile(LStm1);
        if ExtractFileExt(LLabel) = '' then
          FLeftLabel.Caption := LLabel
        else
          FLeftLabel.LoadFromFile(LLabel);

        FLeftMidImage.LoadFromFile(LStm2);

        FRightBottomImage.LoadFromFile(LStm2);
        if ExtractFileExt(LLabel) = '' then
          FRightLabel.Caption := LLabel
        else
          FRightLabel.LoadFromFile(LLabel);
        FRightMidImage.LoadFromFile(LStm1);
      end;
    'Right' :
      begin
        ExpectedResponse:=Right;
        FLeftBottomImage.LoadFromFile(LStm2);
        if ExtractFileExt(LLabel) = '' then
          FLeftLabel.Caption := LLabel
        else
          FLeftLabel.LoadFromFile(LLabel);
        FLeftMidImage.LoadFromFile(LStm1);

        FRightBottomImage.LoadFromFile(LStm1);
        if ExtractFileExt(LLabel) = '' then
          FRightLabel.Caption := LLabel
        else
          FRightLabel.LoadFromFile(LLabel);
        FRightMidImage.LoadFromFile(LStm2);
      end;
  end;
  if StrToBool(AParameters.Values['HasInstruction']) then
  begin
    SetShowType2;
    FTopLabel.Caption := AParameters.Values[_cMsg];
  end else begin
    SetShowType1;
  end;
end;

procedure TBeforeAfterSequence.Start;
begin
  FLeftBottomImage.Show;
  FRightBottomImage.Show;
  FSerialTimer.Start;
end;

procedure TBeforeAfterSequence.Stop;
begin
  FSerialTimer.Stop;
  FTopLabel.Hide;
  FLeftBottomImage.Hide;
  FLeftMidImage.Hide;
  FFirstTopImage.Hide;
  FLeftLabel.Hide;
  FRightBottomImage.Hide;
  FRightMidImage.Hide;
  FSecondTopImage.Hide;
  FRightLabel.Hide;
end;

procedure TBeforeAfterSequence.FitScreen;
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

  with FLeftLabel do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 4) - (Width div 2);
    Top := FLeftBottomImage.Top - (LGap div 2) - Height;
  end;

  with FLeftMidImage do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 4) - (Width div 2);
    Top := FLeftBottomImage.Top - LGap - (Height*2);
  end;


  with FRightBottomImage do
  begin
    Width := LSize;
    Height:= LSize;
    Left := ((Screen.Width div 4)*3) - (Width div 2);
    Top := ((Screen.Height div 4)*3)-LGap;
  end;

  with FRightLabel do
  begin
    Width := LSize;
    Height:= LSize;
    Left := ((Screen.Width div 4)*3) - (Width div 2);
    Top := FRightBottomImage.Top - (LGap div 2) - Height;
  end;

  with FRightMidImage do
  begin
    Width := LSize;
    Height:= LSize;
    Left := ((Screen.Width div 4)*3) - (Width div 2);
    Top := FRightBottomImage.Top - LGap - (Height*2);
  end;

  with FFirstTopImage do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2);
    Top := FLeftBottomImage.Top - LGap - (Height*4);
  end;

  with FSecondTopImage do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2);
    Top := FRightBottomImage.Top - LGap - (Height*4);
  end;

  with FTopLabel do
  begin
    Width := LSize*4;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2);
    Top := FRightBottomImage.Top - LGap - (FRightBottomImage.Height*4);
  end;
end;

end.


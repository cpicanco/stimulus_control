{
  Stimulus Control
  Copyright (C) 2014-2020 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit Stimuli.Sequence.RelationalFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, StdCtrls
  , Stimuli.Abstract
  , Stimuli.Image.Base
  , Keyboard.FrameResponse
  , Schedules
  , SerialTimer
  ;

type

  TShowType = (Type1, Type2, Type3, Type4);
  TBottonTrialType = (Before, After);

  TTopTrialItem = (Equal, Different);
  TTopTrialType = array [0..7] of TTopTrialItem;

  TRFTTrialType = record
    Top : TTopTrialType;
    Bottom : TBottonTrialType;
  end;

  { TRFTSequence }
  TRFTSequence = class(TStimulus)
  private
    FK1 : string;
    FK2 : string;
    FK3 : string;
    FK4 : string;
    FShowType: TShowType;
    FTrialType : TRFTTrialType;
    FExpectedResponse : TFrameResponse;
    FParent : TWinControl;
    FSerialTimer : TSerialTimer;
    // Bottom
    FBottomC1 : TLightImage;
    FBottomC2 : TLightImage;
    FBottomC3 : TLightImage;
    FBottomC4 : TLightImage;
    FBottomContext1 : TLightImage;
    FBottomContext2 : TLightImage;
    FBottomContext3 : TLightImage;

    //Left
    FLeftContext1 : TLightImage;
    FLeftContext2 : TLightImage;
    FLeftContext3 : TLightImage;
    FLeftContext4 : TLightImage;
    FLeftA1 : TLightImage;
    FLeftA2 : TLightImage;
    FLeftA3 : TLightImage;
    FLeftA4 : TLightImage;
    FLeftB1 : TLightImage;
    FLeftB2 : TLightImage;
    FLeftB3 : TLightImage;
    FLeftB4 : TLightImage;

    //Right
    FRightContext1 : TLightImage;
    FRightContext2 : TLightImage;
    FRightContext3 : TLightImage;
    FRightContext4 : TLightImage;
    FRightB1 : TLightImage;
    FRightB2 : TLightImage;
    FRightB3 : TLightImage;
    FRightB4 : TLightImage;
    FRightC1 : TLightImage;
    FRightC2 : TLightImage;
    FRightC3 : TLightImage;
    FRightC4 : TLightImage;
    function GetOnEndSerialTimer: TNotifyEvent;
    function GetParent: TWinControl;
    procedure SetExpectedResponse(AValue: TFrameResponse);
    procedure SetOnEndSerialTimer(AValue: TNotifyEvent);
    procedure SetParent(AValue: TWinControl);
    procedure SetTrialType(AValue: TRFTTrialType);
    procedure ShowBottom1(Sender : TObject);
    procedure ShowBottom2(Sender : TObject);
    procedure ShowBottom3(Sender : TObject);
    procedure ShowBottom4(Sender : TObject);
    procedure ShowBottom5(Sender : TObject);
    procedure ShowBottom6(Sender : TObject);
    procedure ShowBottom7(Sender : TObject);
    procedure ShowTop(Sender : TObject);
    procedure ShowLeft(Sender : TObject);
    procedure ShowLeft1(Sender : TObject);
    procedure ShowLeft2(Sender : TObject);
    procedure ShowLeft3(Sender : TObject);
    procedure ShowRight(Sender : TObject);
    procedure ShowRight1(Sender : TObject);
    procedure ShowRight2(Sender : TObject);
    procedure ShowRight3(Sender : TObject);
    procedure SetShowType1; // 1) bottom from bottom to top; 2) top from bottom to top and from right to left;
    procedure SetShowType2; // 1) top from bottom to top and from right to left; 2) bottom from bottom to top;
    procedure SetShowType3; // 1) top show all at once; 2) bottom from bottom to top;
    function IndexOfKey(AKey : string) : integer;
  protected
    procedure SetSchedule(ASchedule : TSchedule); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(AFilename: string); override;
    procedure LoadFromParameters(AParameters : TStringList);
    procedure Start; override;
    procedure Stop; override;
    procedure FitScreen;
    property ExpectedResponse : TFrameResponse read FExpectedResponse write SetExpectedResponse;
    property TrialType : TRFTTrialType read FTrialType write SetTrialType;
    property Parent : TWinControl read FParent write SetParent;
    property OnEndSerialTimer : TNotifyEvent read GetOnEndSerialTimer write SetOnEndSerialTimer;
  end;

implementation

uses Forms, Constants, math;

{ TRFTSequence }


procedure TRFTSequence.SetParent(AValue: TWinControl);
begin
  if FParent=AValue then Exit;
  FParent := AValue;
  FBottomC1.Parent := AValue;
  FBottomC2.Parent := AValue;
  FBottomC3.Parent := AValue;
  FBottomC4.Parent := AValue;
  FBottomContext1.Parent := AValue;
  FBottomContext2.Parent := AValue;
  FBottomContext3.Parent := AValue;
  FLeftContext1.Parent := AValue;
  FLeftContext2.Parent := AValue;
  FLeftContext3.Parent := AValue;
  FLeftContext4.Parent := AValue;
  FLeftA1.Parent := AValue;
  FLeftA2.Parent := AValue;
  FLeftA3.Parent := AValue;
  FLeftA4.Parent := AValue;
  FLeftB1.Parent := AValue;
  FLeftB2.Parent := AValue;
  FLeftB3.Parent := AValue;
  FLeftB4.Parent := AValue;
  FRightContext1.Parent := AValue;
  FRightContext2.Parent := AValue;
  FRightContext3.Parent := AValue;
  FRightContext4.Parent := AValue;
  FRightB1.Parent := AValue;
  FRightB2.Parent := AValue;
  FRightB3.Parent := AValue;
  FRightB4.Parent := AValue;
  FRightC1.Parent := AValue;
  FRightC2.Parent := AValue;
  FRightC3.Parent := AValue;
  FRightC4.Parent := AValue;
end;

procedure TRFTSequence.SetTrialType(AValue: TRFTTrialType);
begin
  //if FTrialType=AValue then Exit;
  FTrialType:=AValue;
end;

procedure TRFTSequence.ShowBottom1(Sender: TObject);
begin
  if FBottomC1.Tag = 0 then FBottomC1.Show;
  if FBottomC2.Tag = 0 then FBottomC2.Show;
  if FBottomC3.Tag = 0 then FBottomC3.Show;
  if FBottomC4.Tag = 0 then FBottomC4.Show;
end;

procedure TRFTSequence.ShowBottom2(Sender: TObject);
begin
  FBottomContext1.Show;
end;

procedure TRFTSequence.ShowBottom3(Sender: TObject);
begin
  if FBottomC1.Tag = 1 then FBottomC1.Show;
  if FBottomC2.Tag = 1 then FBottomC2.Show;
  if FBottomC3.Tag = 1 then FBottomC3.Show;
  if FBottomC4.Tag = 1 then FBottomC4.Show;
end;

procedure TRFTSequence.ShowBottom4(Sender: TObject);
begin
  FBottomContext2.Show;
end;

procedure TRFTSequence.ShowBottom5(Sender: TObject);
begin
  if FBottomC1.Tag = 2 then FBottomC1.Show;
  if FBottomC2.Tag = 2 then FBottomC2.Show;
  if FBottomC3.Tag = 2 then FBottomC3.Show;
  if FBottomC4.Tag = 2 then FBottomC4.Show;
end;

procedure TRFTSequence.ShowBottom6(Sender: TObject);
begin
  FBottomContext3.Show;
end;

procedure TRFTSequence.ShowBottom7(Sender: TObject);
begin
  if FBottomC1.Tag = 3 then FBottomC1.Show;
  if FBottomC2.Tag = 3 then FBottomC2.Show;
  if FBottomC3.Tag = 3 then FBottomC3.Show;
  if FBottomC4.Tag = 3 then FBottomC4.Show;
end;

procedure TRFTSequence.ShowTop(Sender: TObject);
begin
  FLeftB1.Show;
  FLeftB2.Show;
  FLeftB3.Show;
  FLeftB4.Show;
  FLeftA1.Show;
  FLeftA2.Show;
  FLeftA3.Show;
  FLeftA4.Show;
  FLeftContext1.Show;
  FLeftContext2.Show;
  FLeftContext3.Show;
  FLeftContext4.Show;
  FRightC1.Show;
  FRightC2.Show;
  FRightC3.Show;
  FRightC4.Show;
  FRightB1.Show;
  FRightB2.Show;
  FRightB3.Show;
  FRightB4.Show;
  FRightContext1.Show;
  FRightContext2.Show;
  FRightContext3.Show;
  FRightContext4.Show;
end;

procedure TRFTSequence.ShowLeft(Sender: TObject);
begin
  FLeftB1.Show;
  FLeftB2.Show;
  FLeftB3.Show;
  FLeftB4.Show;
  FLeftA1.Show;
  FLeftA2.Show;
  FLeftA3.Show;
  FLeftA4.Show;
  FLeftContext1.Show;
  FLeftContext2.Show;
  FLeftContext3.Show;
  FLeftContext4.Show;
end;

procedure TRFTSequence.ShowLeft1(Sender: TObject);
begin
  FLeftB1.Show;
  FLeftB2.Show;
  FLeftB3.Show;
  FLeftB4.Show;
end;

procedure TRFTSequence.ShowLeft2(Sender: TObject);
begin
  FLeftA1.Show;
  FLeftA2.Show;
  FLeftA3.Show;
  FLeftA4.Show;
end;

procedure TRFTSequence.ShowLeft3(Sender: TObject);
begin
  FLeftContext1.Show;
  FLeftContext2.Show;
  FLeftContext3.Show;
  FLeftContext4.Show;
end;

procedure TRFTSequence.ShowRight(Sender: TObject);
begin
  FRightC1.Show;
  FRightC2.Show;
  FRightC3.Show;
  FRightC4.Show;
  FRightB1.Show;
  FRightB2.Show;
  FRightB3.Show;
  FRightB4.Show;
  FRightContext1.Show;
  FRightContext2.Show;
  FRightContext3.Show;
  FRightContext4.Show;
end;

procedure TRFTSequence.ShowRight1(Sender: TObject);
begin
  FRightC1.Show;
  FRightC2.Show;
  FRightC3.Show;
  FRightC4.Show;
end;

procedure TRFTSequence.ShowRight2(Sender: TObject);
begin
  FRightB1.Show;
  FRightB2.Show;
  FRightB3.Show;
  FRightB4.Show;
end;

procedure TRFTSequence.ShowRight3(Sender: TObject);
begin
  FRightContext1.Show;
  FRightContext2.Show;
  FRightContext3.Show;
  FRightContext4.Show;
end;

procedure TRFTSequence.SetShowType1;
var
  LTimerItems : TTimerItems;
begin
  SetLength(LTimerItems, 12);
  LTimerItems[0].Interval := 500;
  LTimerItems[0].OnTimerEvent := @ShowBottom2;
  LTimerItems[1].Interval := 500;
  LTimerItems[1].OnTimerEvent := @ShowBottom3;
  LTimerItems[2].Interval := 500;
  LTimerItems[2].OnTimerEvent := @ShowBottom4;
  LTimerItems[3].Interval := 500;
  LTimerItems[3].OnTimerEvent := @ShowBottom5;
  LTimerItems[4].Interval := 500;
  LTimerItems[4].OnTimerEvent := @ShowBottom6;
  LTimerItems[5].Interval := 500;
  LTimerItems[5].OnTimerEvent := @ShowBottom7;
  LTimerItems[6].Interval := 500;
  LTimerItems[6].OnTimerEvent := @ShowRight1;
  LTimerItems[7].Interval := 500;
  LTimerItems[7].OnTimerEvent := @ShowRight2;
  LTimerItems[8].Interval := 500;
  LTimerItems[8].OnTimerEvent := @ShowRight3;
  LTimerItems[9].Interval := 500;
  LTimerItems[9].OnTimerEvent := @ShowLeft1;
  LTimerItems[10].Interval := 500;
  LTimerItems[10].OnTimerEvent := @ShowLeft2;
  LTimerItems[11].Interval := 500;
  LTimerItems[11].OnTimerEvent := @ShowLeft3;

  FSerialTimer.Clear;
  FSerialTimer.Append(LTimerItems);
end;

procedure TRFTSequence.SetShowType2;
var
  LTimerItems : TTimerItems;
begin
  SetLength(LTimerItems, 12);
  LTimerItems[0].Interval := 500;
  LTimerItems[0].OnTimerEvent := @ShowRight2;
  LTimerItems[1].Interval := 500;
  LTimerItems[1].OnTimerEvent := @ShowRight3;
  LTimerItems[2].Interval := 500;
  LTimerItems[2].OnTimerEvent := @ShowLeft1;
  LTimerItems[3].Interval := 500;
  LTimerItems[3].OnTimerEvent := @ShowLeft2;
  LTimerItems[4].Interval := 500;
  LTimerItems[4].OnTimerEvent := @ShowLeft3;
  LTimerItems[5].Interval := 500;
  LTimerItems[5].OnTimerEvent := @ShowBottom1;
  LTimerItems[6].Interval := 500;
  LTimerItems[6].OnTimerEvent := @ShowBottom2;
  LTimerItems[7].Interval := 500;
  LTimerItems[7].OnTimerEvent := @ShowBottom3;
  LTimerItems[8].Interval := 500;
  LTimerItems[8].OnTimerEvent := @ShowBottom4;
  LTimerItems[9].Interval := 500;
  LTimerItems[9].OnTimerEvent := @ShowBottom5;
  LTimerItems[10].Interval := 500;
  LTimerItems[10].OnTimerEvent := @ShowBottom6;
  LTimerItems[11].Interval := 500;
  LTimerItems[11].OnTimerEvent := @ShowBottom7;
  FSerialTimer.Clear;
  FSerialTimer.Append(LTimerItems);
end;

procedure TRFTSequence.SetShowType3;
var
  LTimerItems : TTimerItems;
begin
  SetLength(LTimerItems, 7);
  LTimerItems[0].Interval := 2000;
  LTimerItems[0].OnTimerEvent := @ShowBottom1;
  LTimerItems[1].Interval := 500;
  LTimerItems[1].OnTimerEvent := @ShowBottom2;
  LTimerItems[2].Interval := 500;
  LTimerItems[2].OnTimerEvent := @ShowBottom3;
  LTimerItems[3].Interval := 500;
  LTimerItems[3].OnTimerEvent := @ShowBottom4;
  LTimerItems[4].Interval := 500;
  LTimerItems[4].OnTimerEvent := @ShowBottom5;
  LTimerItems[5].Interval := 500;
  LTimerItems[5].OnTimerEvent := @ShowBottom6;
  LTimerItems[6].Interval := 500;
  LTimerItems[6].OnTimerEvent := @ShowBottom7;

  FSerialTimer.Append(LTimerItems);
end;

function TRFTSequence.IndexOfKey(AKey: string): integer;
begin
  if AKey = FK1 then Result := 0;
  if AKey = FK2 then Result := 1;
  if AKey = FK3 then Result := 2;
  if AKey = FK4 then Result := 3;
end;

function TRFTSequence.GetParent: TWinControl;
begin
  Result := FParent;
end;

function TRFTSequence.GetOnEndSerialTimer: TNotifyEvent;
begin
  Result := FSerialTimer.OnEndTimeSerie;
end;

procedure TRFTSequence.SetExpectedResponse(AValue: TFrameResponse);
begin
  if FExpectedResponse=AValue then Exit;
  FExpectedResponse:=AValue;
end;

procedure TRFTSequence.SetOnEndSerialTimer(AValue: TNotifyEvent);
begin
  if FSerialTimer.OnEndTimeSerie=AValue then Exit;
  FSerialTimer.OnEndTimeSerie:=AValue;
end;

procedure TRFTSequence.SetSchedule(ASchedule: TSchedule);
begin
  inherited SetSchedule(ASchedule);
end;

constructor TRFTSequence.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //FTrialType := ;
  FExpectedResponse := TFrameResponse.Create;
  FBottomC1 := TLightImage.Create(Self);
  FBottomC2 := TLightImage.Create(Self);
  FBottomC3 := TLightImage.Create(Self);
  FBottomC4 := TLightImage.Create(Self);
  FBottomContext1 := TLightImage.Create(Self);
  FBottomContext2 := TLightImage.Create(Self);
  FBottomContext3 := TLightImage.Create(Self);
  FLeftContext1 := TLightImage.Create(Self);
  FLeftContext2 := TLightImage.Create(Self);
  FLeftContext3 := TLightImage.Create(Self);
  FLeftContext4 := TLightImage.Create(Self);
  FLeftA1 := TLightImage.Create(Self);
  FLeftA2 := TLightImage.Create(Self);
  FLeftA3 := TLightImage.Create(Self);
  FLeftA4 := TLightImage.Create(Self);
  FLeftB1 := TLightImage.Create(Self);
  FLeftB2 := TLightImage.Create(Self);
  FLeftB3 := TLightImage.Create(Self);
  FLeftB4 := TLightImage.Create(Self);
  FRightContext1 := TLightImage.Create(Self);
  FRightContext2 := TLightImage.Create(Self);
  FRightContext3 := TLightImage.Create(Self);
  FRightContext4 := TLightImage.Create(Self);
  FRightB1 := TLightImage.Create(Self);
  FRightB2 := TLightImage.Create(Self);
  FRightB3 := TLightImage.Create(Self);
  FRightB4 := TLightImage.Create(Self);
  FRightC1 := TLightImage.Create(Self);
  FRightC2 := TLightImage.Create(Self);
  FRightC3 := TLightImage.Create(Self);
  FRightC4 := TLightImage.Create(Self);
  FSerialTimer := TSerialTimer.Create(Self);
end;

destructor TRFTSequence.Destroy;
begin
  inherited Destroy;
end;

procedure TRFTSequence.LoadFromFile(AFilename: string);
var
  LStringList : TStringList;
begin
  LStringList := TStringList.Create;
  try
    //LStringList.LoadFromFile(AFilename);
    //FLeftBottomImage.LoadFromFile(LStringList[0]);
    //FMidImage.Caption := LStringList[0];
    //FRightBottomImage.LoadFromFile(LStringList[1]);
    //FTopImage.Caption := LStringList[2];
  finally
    LStringList.Free;
  end;
end;

procedure TRFTSequence.LoadFromParameters(AParameters: TStringList);
var
  LBottomCue : string;
  LCue1 : string;
  LCue2 : string;
  LCue3 : string;
  LCue4 : string;
  LCue5 : string;
  LCue6 : string;
  LCue7 : string;
  LCue8 : string;
  LA1 : string;
  LA2 : string;
  LA3 : string;
  LA4 : string;
  LB1 : string;
  LB2 : string;
  LB3 : string;
  LB4 : string;
  LC1 : string;
  LC2 : string;
  LC3 : string;
  LC4 : string;
begin
  case AParameters.Values['ShowType'] of
    '1': FShowType:=Type1;
    '2': FShowType:=Type2;
    '3': FShowType:=Type3;
    '4': FShowType:=Type4;
    else FShowType:=Type4;
  end;
  case FShowType of
    Type1: SetShowType1;
    Type2: SetShowType2;
    Type3: SetShowType3;
    Type4: { do nothing };
  end;
  LBottomCue := AParameters.Values['BottomCue'];
  LCue1 := AParameters.Values['CueImage1'];
  LCue2 := AParameters.Values['CueImage2'];
  LCue3 := AParameters.Values['CueImage3'];
  LCue4 := AParameters.Values['CueImage4'];
  LCue5 := AParameters.Values['CueImage5'];
  LCue6 := AParameters.Values['CueImage6'];
  LCue7 := AParameters.Values['CueImage7'];
  LCue8 := AParameters.Values['CueImage8'];
  LA1 := AParameters.Values['A1'];
  LA2 := AParameters.Values['A2'];
  LA3 := AParameters.Values['A3'];
  LA4 := AParameters.Values['A4'];
  LB1 := AParameters.Values['B1'];
  LB2 := AParameters.Values['B2'];
  LB3 := AParameters.Values['B3'];
  LB4 := AParameters.Values['B4'];
  LC1 := AParameters.Values['C1'];
  LC2 := AParameters.Values['C2'];
  LC3 := AParameters.Values['C3'];
  LC4 := AParameters.Values['C4'];
  FK1 := AParameters.Values['KC1'];
  FK2 := AParameters.Values['KC2'];
  FK3 := AParameters.Values['KC3'];
  FK4 := AParameters.Values['KC4'];

  if ExtractFileExt(LBottomCue) = '' then
  begin
    FBottomContext1.Caption := LBottomCue;
    FBottomContext2.Caption := LBottomCue;
    FBottomContext3.Caption := LBottomCue;
  end else begin
    FBottomContext1.LoadFromFile(LBottomCue);
    FBottomContext2.LoadFromFile(LBottomCue);
    FBottomContext3.LoadFromFile(LBottomCue);
  end;

  if ExtractFileExt(LCue1) = '' then
    FRightContext1.Caption :=  ExtractFileName(LCue1)
  else
    FRightContext1.LoadFromFile(LCue1);

  if ExtractFileExt(LCue2) = '' then
    FRightContext2.Caption := ExtractFileName(LCue2)
  else
    FRightContext2.LoadFromFile(LCue2);

  if ExtractFileExt(LCue3) = '' then
    FRightContext3.Caption := ExtractFileName(LCue3)
  else
    FRightContext3.LoadFromFile(LCue3);

  if ExtractFileExt(LCue4) = '' then
    FRightContext4.Caption := ExtractFileName(LCue4)
  else
    FRightContext4.LoadFromFile(LCue4);

  if ExtractFileExt(LCue5) = '' then
    FLeftContext1.Caption := ExtractFileName(LCue5)
  else
    FLeftContext1.LoadFromFile(LCue5);

  if ExtractFileExt(LCue6) = '' then
    FLeftContext2.Caption := ExtractFileName(LCue6)
  else
    FLeftContext2.LoadFromFile(LCue6);

  if ExtractFileExt(LCue7) = '' then
    FLeftContext3.Caption := ExtractFileName(LCue7)
  else
    FLeftContext3.LoadFromFile(LCue7);

  if ExtractFileExt(LCue8) = '' then
    FLeftContext4.Caption := ExtractFileName(LCue8)
  else
    FLeftContext4.LoadFromFile(LCue8);

  if ExtractFileExt(LC1) = '' then
  begin
    FBottomC1.Caption := ExtractFileName(LC1);
    FRightC1.Caption := ExtractFileName(LC1);
  end else begin
    FBottomC1.LoadFromFile(LC1);
    FRightC1.LoadFromFile(LC1);
  end;

  if ExtractFileExt(LC2) = '' then
  begin
    FBottomC2.Caption := ExtractFileName(LC2);
    FRightC2.Caption := ExtractFileName(LC2);
  end else begin
    FBottomC2.LoadFromFile(LC2);
    FRightC2.LoadFromFile(LC2);
  end;

  if ExtractFileExt(LC3) = '' then
  begin
    FBottomC3.Caption := ExtractFileName(LC3);
    FRightC3.Caption := ExtractFileName(LC3);
  end else begin
    FBottomC3.LoadFromFile(LC3);
    FRightC3.LoadFromFile(LC3);
  end;

  if ExtractFileExt(LC4) = '' then
  begin
    FBottomC4.Caption := ExtractFileName(LC4);
    FRightC4.Caption := ExtractFileName(LC4);
  end else begin
    FBottomC4.LoadFromFile(LC4);
    FRightC4.LoadFromFile(LC4);
  end;

  if ExtractFileExt(LB1) = '' then
  begin
    FLeftB1.Caption := ExtractFileName(LB1);
    FRightB1.Caption := ExtractFileName(LB1);
  end else begin
    FLeftB1.LoadFromFile(LB1);
    FRightB1.LoadFromFile(LB1);
  end;

  if ExtractFileExt(LB2) = '' then
  begin
    FLeftB2.Caption := ExtractFileName(LB2);
    FRightB2.Caption := ExtractFileName(LB2);
  end else begin
    FLeftB2.LoadFromFile(LB2);
    FRightB2.LoadFromFile(LB2);
  end;

  if ExtractFileExt(LB3) = '' then
  begin
    FLeftB3.Caption := ExtractFileName(LB3);
    FRightB3.Caption := ExtractFileName(LB3);
  end else begin
    FLeftB3.LoadFromFile(LB3);
    FRightB3.LoadFromFile(LB3);
  end;

  if ExtractFileExt(LB4) = '' then
  begin
    FLeftB4.Caption := ExtractFileName(LB4);
    FRightB4.Caption := ExtractFileName(LB4);
  end else begin
    FLeftB4.LoadFromFile(LB4);
    FRightB4.LoadFromFile(LB4);
  end;

  if ExtractFileExt(LA1) = '' then
    FLeftA1.Caption := ExtractFileName(LA1)
  else
    FLeftA1.LoadFromFile(LA1);

  if ExtractFileExt(LA2) = '' then
    FLeftA2.Caption := ExtractFileName(LA2)
  else
    FLeftA2.LoadFromFile(LA2);

  if ExtractFileExt(LA3) = '' then
    FLeftA3.Caption := ExtractFileName(LA3)
  else
    FLeftA3.LoadFromFile(LA3);

  if ExtractFileExt(LA4) = '' then
    FLeftA4.Caption := ExtractFileName(LA4)
  else
    FLeftA4.LoadFromFile(LA4);
end;

procedure TRFTSequence.Start;
  procedure ShowAll;
  begin
    FBottomC1.Show;
    FBottomC2.Show;
    FBottomC3.Show;
    FBottomC4.Show;
    FBottomContext1.Show;
    FBottomContext2.Show;
    FBottomContext3.Show;
    FLeftContext1.Show;
    FLeftContext2.Show;
    FLeftContext3.Show;
    FLeftContext4.Show;
    FLeftA1.Show;
    FLeftA2.Show;
    FLeftA3.Show;
    FLeftA4.Show;
    FLeftB1.Show;
    FLeftB2.Show;
    FLeftB3.Show;
    FLeftB4.Show;
    FRightContext1.Show;
    FRightContext2.Show;
    FRightContext3.Show;
    FRightContext4.Show;
    FRightB1.Show;
    FRightB2.Show;
    FRightB3.Show;
    FRightB4.Show;
    FRightC1.Show;
    FRightC2.Show;
    FRightC3.Show;
    FRightC4.Show;
    if Assigned(FSerialTimer.OnEndTimeSerie) then FSerialTimer.OnEndTimeSerie(Self);
  end;
begin
  case FShowType of
    Type1 : ShowBottom1(Self);
    Type2 : ShowRight1(Self);
    Type3 : ShowTop(Self);
    Type4 :
      begin
        ShowAll;
        Exit;
      end;
  end;
  FSerialTimer.Start;
end;

procedure TRFTSequence.Stop;
begin
  FSerialTimer.Stop;
  FBottomC1.Hide;
  FBottomC2.Hide;
  FBottomC3.Hide;
  FBottomC4.Hide;
  FBottomContext1.Hide;
  FBottomContext2.Hide;
  FBottomContext3.Hide;

  FLeftContext1.Hide;
  FLeftContext2.Hide;
  FLeftContext3.Hide;
  FLeftContext4.Hide;
  FLeftA1.Hide;
  FLeftA2.Hide;
  FLeftA3.Hide;
  FLeftA4.Hide;
  FLeftB1.Hide;
  FLeftB2.Hide;
  FLeftB3.Hide;
  FLeftB4.Hide;

  FRightContext1.Hide;
  FRightContext2.Hide;
  FRightContext3.Hide;
  FRightContext4.Hide;
  FRightB1.Hide;
  FRightB2.Hide;
  FRightB3.Hide;
  FRightB4.Hide;
  FRightC1.Hide;
  FRightC2.Hide;
  FRightC3.Hide;
  FRightC4.Hide;
end;

procedure TRFTSequence.FitScreen;
var
  LSize : integer;
const
  Panel : integer = 15;
  HP1 : integer = 3;
  HP2 : integer = 4;
  HP3 : integer = 5;
  HP4 : integer = 6;

  procedure SetBottomTopAndTag(ALightImage: TLightImage; AIndexOfKey : integer);
  begin
    case AIndexOfKey of
      0 :
        begin
          ALightImage.Tag := 0;
          ALightImage.Top := ((Screen.Height div Panel)*(Panel-1));
        end;
      1 :
        begin
          ALightImage.Tag := 1;
          ALightImage.Top := ((Screen.Height div Panel)*(Panel-3));
        end;
      2 :
        begin
          ALightImage.Tag := 2;
          ALightImage.Top := ((Screen.Height div Panel)*(Panel-5));
        end;
      3 :
        begin
          ALightImage.Tag := 3;
          ALightImage.Top := ((Screen.Height div Panel)*(Panel-7));
        end
      else raise Exception.Create('Stimuli.Sequence.RelationalFrame GetBottomTop Exception');
    end;
  end;

begin
  LSize := (Screen.Height div Panel) - 2;
  with FBottomC1 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2);
    SetBottomTopAndTag(FBottomC1, IndexOfKey('Z'));
  end;

  with FBottomContext1 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2);
    Top := ((Screen.Height div Panel)*(Panel-2));
  end;

  with FBottomC2 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2);
    SetBottomTopAndTag(FBottomC2, IndexOfKey('C'));
  end;

  with FBottomContext2 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2);
    Top := ((Screen.Height div Panel)*(Panel-4));
  end;

  with FBottomC3 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2);
    SetBottomTopAndTag(FBottomC3, IndexOfKey('B'));
  end;

  with FBottomContext3 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2);
    Top := ((Screen.Height div Panel)*(Panel-6));
  end;

  with FBottomC4 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2);
    SetBottomTopAndTag(FBottomC4, IndexOfKey('M'));
  end;


  // L B

  with FLeftB1 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) - ((Screen.Height div Panel)*(Panel-HP1));
    Top := ((Screen.Height div Panel)*(Panel-8));
  end;

  with FLeftB2 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) - ((Screen.Height div Panel)*(Panel-HP2));
    Top := ((Screen.Height div Panel)*(Panel-8));
  end;

  with FLeftB3 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) - ((Screen.Height div Panel)*(Panel-HP3));
    Top := ((Screen.Height div Panel)*(Panel-8));
  end;

  with FLeftB4 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) - ((Screen.Height div Panel)*(Panel-HP4));
    Top := ((Screen.Height div Panel)*(Panel-8));
  end;

  // L A
  with FLeftA1 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) - ((Screen.Height div Panel)*(Panel-HP1));
    Top := ((Screen.Height div Panel)*(Panel-9));
  end;

  with FLeftA2 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) - ((Screen.Height div Panel)*(Panel-HP2));
    Top := ((Screen.Height div Panel)*(Panel-9));
  end;

  with FLeftA3 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) - ((Screen.Height div Panel)*(Panel-HP3));
    Top := ((Screen.Height div Panel)*(Panel-9));
  end;

  with FLeftA4 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) - ((Screen.Height div Panel)*(Panel-HP4));
    Top := ((Screen.Height div Panel)*(Panel-9));
  end;


  // L Context


  with FLeftContext1 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) - ((Screen.Height div Panel)*(Panel-HP1));
    Top := ((Screen.Height div Panel)*(Panel-10));
  end;

  with FLeftContext2 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) - ((Screen.Height div Panel)*(Panel-HP2));
    Top := ((Screen.Height div Panel)*(Panel-10));
  end;

  with FLeftContext3 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) - ((Screen.Height div Panel)*(Panel-HP3));
    Top := ((Screen.Height div Panel)*(Panel-10));
  end;

  with FLeftContext4 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) - ((Screen.Height div Panel)*(Panel-HP4));
    Top := ((Screen.Height div Panel)*(Panel-10));
  end;


  // R C

  with FRightC4 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) + ((Screen.Height div Panel)*(Panel-HP1));
    Top := ((Screen.Height div Panel)*(Panel-8));
  end;

  with FRightC3 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) + ((Screen.Height div Panel)*(Panel-HP2));
    Top := ((Screen.Height div Panel)*(Panel-8));
  end;

  with FRightC2 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) + ((Screen.Height div Panel)*(Panel-HP3));
    Top := ((Screen.Height div Panel)*(Panel-8));
  end;

  with FRightC1 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) + ((Screen.Height div Panel)*(Panel-HP4));
    Top := ((Screen.Height div Panel)*(Panel-8));
  end;

  // R B

  with FRightB4 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) + ((Screen.Height div Panel)*(Panel-HP1));
    Top := ((Screen.Height div Panel)*(Panel-9));
  end;

  with FRightB3 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) + ((Screen.Height div Panel)*(Panel-HP2));
    Top := ((Screen.Height div Panel)*(Panel-9));
  end;

  with FRightB2 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) + ((Screen.Height div Panel)*(Panel-HP3));
    Top := ((Screen.Height div Panel)*(Panel-9));
  end;

  with FRightB1 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) + ((Screen.Height div Panel)*(Panel-HP4));
    Top := ((Screen.Height div Panel)*(Panel-9));
  end;

  // R Context

  with FRightContext4 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) + ((Screen.Height div Panel)*(Panel-HP1));
    Top := ((Screen.Height div Panel)*(Panel-10));
  end;

  with FRightContext3 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) + ((Screen.Height div Panel)*(Panel-HP2));
    Top := ((Screen.Height div Panel)*(Panel-10));
  end;

  with FRightContext2 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) + ((Screen.Height div Panel)*(Panel-HP3));
    Top := ((Screen.Height div Panel)*(Panel-10));
  end;

  with FRightContext1 do
  begin
    Width := LSize;
    Height:= LSize;
    Left := (Screen.Width div 2) - (Width div 2) + ((Screen.Height div Panel)*(Panel-HP4));
    Top := ((Screen.Height div Panel)*(Panel-10));
  end;
end;

end.

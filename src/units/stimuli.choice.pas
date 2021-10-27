{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Stimuli.Choice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, StdCtrls
  , Stimuli
  , Stimuli.Abstract
  , Stimuli.Image.Base
  , Schedules
  ;

type

  { TBinaryChoiceMessage }
  TBinaryChoiceMessage = record
    CurrentTrial: integer;
    LastNow : Extended;
    Now : Extended;
    Later : Extended;
    Sender : TObject;
    Delay: string;
  end;

  { TBinaryChoice }
  TBinaryChoice = class(TStimulus, IStimuli)
  private
    FParent : TWinControl;
    FLabelLeft : TLabel;
    FLabelRight: TLabel;
    FImageLeft : TLightImage;
    FImageRight : TLightImage;
    FNextTrialLeft : string;
    FNextTrialRight: string;
    function GetParent: TWinControl;
    procedure SetParent(AValue: TWinControl);
  protected
    procedure SetSchedule(ASchedule : TSchedule); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function MessageFromParameters(
      AParameters : TStringList) : TBinaryChoiceMessage;
    function NextTrial(Sender: TObject) : string;
    function AsInterface : IStimuli;
    procedure DoExpectedResponse;
    procedure FitScreen;
    procedure LoadFromParameters(AParameters: TStringList);
    procedure LoadMessage(AMessage : TBinaryChoiceMessage);
    procedure NextNow(var AMessage : TBinaryChoiceMessage);
    procedure SetScheduleConsequence(AConsequence : TNotifyEvent);
    procedure Start;
    procedure Stop;
    property Parent : TWinControl read GetParent write SetParent;
  end;

implementation

uses Constants;

{ TBinaryChoice }

procedure TBinaryChoice.SetParent(AValue: TWinControl);
begin
  if FParent=AValue then Exit;
  FParent := AValue;
  FLabelLeft.Parent := FParent;
  FLabelRight.Parent := FParent;
  FImageLeft.Parent := FParent;
  FImageRight.Parent := FParent;
end;

procedure TBinaryChoice.LoadMessage(AMessage : TBinaryChoiceMessage);
var
  OldDecimalSeparator : char;
begin
  OldDecimalSeparator := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := '.';

  FLabelLeft.Caption := 'Agora';
  FImageLeft.Caption :=
  'Ganhar '+#13+'R$' +
  FloatToStrF(AMessage.Now, ffFixed, 0, 2);

  case AMessage.CurrentTrial of
    0 : begin
      FLabelRight.Font.Bold := True;
      FLabelLeft.Font.Bold := True;
    end

    else begin
      FLabelRight.Font.Bold := False;
      FLabelLeft.Font.Bold := False;
    end;
  end;
  FLabelRight.Caption := 'Daqui ' + AMessage.Delay;
  FImageRight.Caption :=
  'Ganhar '+#13+'R$' +
  FloatToStrF(AMessage.Later, ffFixed, 0, 2);

  DefaultFormatSettings.DecimalSeparator := OldDecimalSeparator;
end;


function TBinaryChoice.GetParent: TWinControl;
begin
  Result := FParent;
end;

procedure TBinaryChoice.SetSchedule(ASchedule: TSchedule);
begin
  inherited SetSchedule(ASchedule);
  FImageLeft.Schedule := Schedule;
  FImageRight.Schedule := Schedule;
end;

procedure TBinaryChoice.SetScheduleConsequence(AConsequence: TNotifyEvent);
begin
  FImageLeft.Schedule.OnConsequence := AConsequence;
  FImageRight.Schedule.OnConsequence := AConsequence;
end;

function TBinaryChoice.NextTrial(Sender: TObject): string;
var
  LSchedule : TSchedule;
begin
  LSchedule :=  TSchedule(Sender);
  if LSchedule = FImageLeft.Schedule then Result := FNextTrialLeft;
  if LSchedule = FImageRight.Schedule then Result := FNextTrialRight;
end;

function TBinaryChoice.AsInterface : IStimuli;
begin
  Result := Self;
end;

procedure TBinaryChoice.DoExpectedResponse;
var
  R : integer;
begin
  R := Random(2);
  case R of
    0 : FImageLeft.DoResponse;
    1 : FImageRight.DoResponse;
  end;
end;

procedure TBinaryChoice.NextNow(var AMessage : TBinaryChoiceMessage);
const
  LFactor : real = 0.5;
var
  LNow : real;
  LDelta : real;
  LSchedule : TSchedule;
begin
  if AMessage.Sender is TSchedule then begin
    LSchedule :=  TSchedule(AMessage.Sender);
    case AMessage.CurrentTrial of
      0 : begin
        if LSchedule = FImageLeft.Schedule then begin
          LNow := AMessage.Now * 0.50;
        end;

        if LSchedule = FImageRight.Schedule then begin
          LNow := AMessage.Now * 1.50
        end;

        AMessage.LastNow := AMessage.Now;
        AMessage.Now     := LNow;
      end;

      // recursive logic for geometric progression
      else
        LDelta := Abs(AMessage.Now - AMessage.LastNow) * LFactor;
        AMessage.LastNow := AMessage.Now;

        if LSchedule = FImageLeft.Schedule then begin
          LNow := AMessage.LastNow - LDelta;
        end;
        if LSchedule = FImageRight.Schedule then begin
          LNow := AMessage.LastNow + LDelta;
        end;

        AMessage.Now := LNow;
    end;
  end;
end;

constructor TBinaryChoice.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageLeft := TLightImage.Create(Self);
  FImageLeft.Visible := False;
  FImageLeft.Schedule := TSchedule.Create(Self);
  FImageLeft.Schedule.Name := 'Left';
  FImageLeft.Schedule.Load(CRF);
  FImageLeft.EdgeColor:=clBlack;

  FImageRight := TLightImage.Create(Self);
  FImageRight.Visible := False;
  FImageRight.Schedule := TSchedule.Create(Self);
  FImageRight.Schedule.Name := 'Right';
  FImageRight.Schedule.Load(CRF);
  FImageRight.EdgeColor:=clBlack;

  FLabelLeft := TLabel.Create(Self);
  FLabelLeft.Visible := False;
  FLabelLeft.Alignment := taCenter;
  FLabelLeft.Layout := tlCenter;
  FLabelLeft.Font.Size := 20;
  FLabelLeft.Font.Color := clBlack;
  FLabelLeft.BorderSpacing.Around := 10;
  with FLabelLeft do begin
    Anchors := [akBottom, akLeft];
    AnchorSideBottom.Control := FImageLeft;
    AnchorSideBottom.Side := asrTop;
    AnchorSideLeft.Control := FImageLeft;
    AnchorSideLeft.Side := asrCenter;
  end;

  FLabelRight := TLabel.Create(Self);
  FLabelRight.Visible := False;
  FLabelRight.Alignment := taCenter;
  FLabelRight.Layout := tlCenter;
  FLabelRight.Font.Size := 20;
  FLabelRight.Font.Color := clBlack;
  FLabelRight.BorderSpacing.Around := 10;
  with FLabelRight do begin
    Anchors := [akBottom, akLeft];
    AnchorSideBottom.Control := FImageRight;
    AnchorSideBottom.Side := asrTop;
    AnchorSideLeft.Control := FImageRight;
    AnchorSideLeft.Side := asrCenter;
  end;
end;

destructor TBinaryChoice.Destroy;
begin
  inherited Destroy;
end;

procedure TBinaryChoice.LoadFromParameters(AParameters: TStringList);
begin
  FNextTrialLeft := AParameters.Values['L'+_NextTrial];
  FNextTrialRight:= AParameters.Values['R'+_NextTrial];
end;

function TBinaryChoice.MessageFromParameters(
  AParameters : TStringList) : TBinaryChoiceMessage;
begin
  Result.Delay := AParameters.Values['Delay'];
  if AParameters.Values[_LastNow] = '' then begin
    Result.Now := StrToFloatDef(AParameters.Values['L'], 50);
  end else begin
    Result.Now := StrToFloat(AParameters.Values[_Now]);
    Result.LastNow := StrToFloat(AParameters.Values[_LastNow]);
  end;
  Result.Later := StrToFloatDef(AParameters.Values['R'], 100);
end;

procedure TBinaryChoice.Start;
begin
  FLabelLeft.Show;
  FImageLeft.Show;
  FImageLeft.Schedule.Start;

  FLabelRight.Show;
  FImageRight.Show;
  FImageRight.Schedule.Start;
end;

procedure TBinaryChoice.Stop;
begin
  FLabelLeft.Hide;
  FLabelRight.Hide;

  FImageLeft.Hide;
  FImageRight.Hide;
end;

procedure TBinaryChoice.FitScreen;
begin
  FImageLeft.CentralizeLeft;
  FImageRight.CentralizeRight;
end;

end.


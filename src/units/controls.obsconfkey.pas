{
  Stimulus Control
  Copyright (C) 2014-2022 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.ObsConfKey; // observation and confirmation key

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls, StdCtrls
  , Stimuli
  , Stimuli.Abstract
  , Stimuli.Image.Base
  , Audio.CastleSound
  , Schedules
  ;

type

  TKeyStyle = (ksAuditive, ksVisual);

  { TKey }

  TKey = class(TStimulus, IStimuli)
  private
    FKey : string;
    FKeyStyle: TKeyStyle;
    FPosition: integer;
    FTimer : TTimer;
    FImage : TLightImage;
    FSound : TSound;
    FButtonConfirmation : TButton;
    function GetParent: TWinControl;
    function GetVisible: Boolean;
    procedure SetParent(AValue: TWinControl);
    procedure SetPosition(AValue: integer);
    procedure ToggleImage(Sender: TObject);
    procedure ConfirmationButtonClick(Sender: TObject);
    procedure Observe(Sender: TObject);
  protected
    procedure SetSchedule(ASchedule : TSchedule); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function AsInterface : IStimuli;
    function ShortName : string;
    procedure DoExpectedResponse;
    procedure HideCursor;
    procedure FitScreen;
    procedure LoadFromParameters(AParameters: TStringList);
    procedure Start;
    procedure Show;
    procedure Stop;
    property Parent : TWinControl read GetParent write SetParent;
    property Position : integer read FPosition write SetPosition;
    property Style : TKeyStyle read FKeyStyle write FKeyStyle;
    property Key : string read FKey write FKey;
    property Visible : Boolean read GetVisible;
  end;

implementation

uses Constants, Experiments.Grids;

{ TKey }

procedure TKey.SetParent(AValue: TWinControl);
begin
  if FImage.Parent=AValue then Exit;
  FImage.Parent := AValue;
  //FImageObs.Parent := AValue;
  FButtonConfirmation.Parent := AValue;
end;

procedure TKey.SetPosition(AValue: integer);
begin
  if FPosition = AValue then Exit;
  FPosition := AValue;
end;

procedure TKey.ToggleImage(Sender: TObject);
begin
  FButtonConfirmation.Hide;
  FTimer.Enabled := False;
  FImage.Kind := ikLetterRect;
  FImage.Invalidate;
  if Style = ksAuditive then begin
    FSound.Stop;
  end;
end;

procedure TKey.ConfirmationButtonClick(Sender: TObject);
begin
  OnConsequence(Self);
end;

function TKey.GetParent: TWinControl;
begin
  Result := FImage.Parent;
end;

function TKey.GetVisible: Boolean;
begin
  Result := FImage.Visible;
end;

procedure TKey.SetSchedule(ASchedule: TSchedule);
begin
  inherited SetSchedule(ASchedule);
  FImage.Schedule := ASchedule;
end;

constructor TKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 2000;
  FTimer.OnTimer := @ToggleImage;

  FImage := TLightImage.Create(Self);
  FImage.Visible := False;

  FButtonConfirmation := TButton.Create(Self);
  FButtonConfirmation.Visible := False;
  FButtonConfirmation.AutoSize := True;
  FButtonConfirmation.Caption := 'Confirmar';
  FButtonConfirmation.Font.Name:='Times New Roman';
  FButtonConfirmation.Font.Size := 15;
  FButtonConfirmation.OnClick := @ConfirmationButtonClick;
end;

destructor TKey.Destroy;
begin
  inherited Destroy;
end;

function TKey.AsInterface : IStimuli;
begin
  Result := IStimuli(Self);
end;

function TKey.ShortName: string;
begin
  case Style of
    ksAuditive : begin
      Result := FSound.ShortName;
    end;

    ksVisual   : begin
      Result := FImage.ShortName;
    end;
  end;
end;

procedure TKey.LoadFromParameters(AParameters : TStringList);
var
  LFilename : string;
begin
  FButtonConfirmation.Name := Key;
  LFilename := AParameters.Values[Key];
  case AParameters.Values[_Style] of
    'Auditivo' : begin
      FImage.Caption := 'Ouvir';
      Style := ksAuditive;

    end;
    'Visual'   : begin
      FImage.Caption := 'Ver';
      Style := ksVisual;
    end
    else
      Exception.Create('Estilo desconhecido: ' + AParameters.Values[_Style])
  end;

  Position := AParameters.Values[Key+_cBnd].ToInteger;

  try
    case Style of
      ksAuditive : begin
        FImage.LoadFromFile('audio_fundo.jpg');
        FSound := TSound.Create(Self);
        FSound.LoadFromFile(LFilename+'.wav');
      end;

      ksVisual   : begin
        FImage.LoadFromFile(LFilename+'.jpg');

      end;
    end;
    if Key.Contains('S') then begin
      FImage.Kind := ikBitmap;
    end else begin;
      FImage.Kind := ikLetterRect;
      FImage.OnClick := @Observe;
    end;
  except
    on E : Exception do
    begin
      Exception.Create(E.Message + #32 + LFilename);
    end;
  end;
end;

procedure TKey.Observe(Sender: TObject);
begin
  FImage.Kind := ikBitmap;
  FImage.Invalidate;
  FTimer.Enabled := True;
  FButtonConfirmation.Show;
  if Style = ksAuditive then begin
    FSound.Play;
  end;
end;

procedure TKey.Start;
begin
  //FImage.Centralize;
  FImage.Show;
  FImage.BringToFront;
  if Style = ksAuditive then begin
    FSound.Play;
  end;
end;

procedure TKey.Show;
begin
  FImage.Show;
  FImage.BringToFront;
end;

procedure TKey.Stop;
begin
  FImage.Hide;
  FButtonConfirmation.Hide;
  if Style = ksAuditive then begin
    if FSound.Playing then begin
      FSound.Stop;
    end;
  end;
end;

procedure TKey.DoExpectedResponse;
begin
  FButtonConfirmation.Click;
end;

procedure TKey.HideCursor;
begin
  FImage.Cursor := -1;
end;

procedure TKey.FitScreen;
begin
  FImage.BoundsRect := RectFromPosition(Position);
  FButtonConfirmation.Top :=
    FImage.BoundsRect.Bottom + 10;
  FButtonConfirmation.Left :=
    FImage.BoundsRect.CenterPoint.X - (FImage.Width div 2);
end;

end.


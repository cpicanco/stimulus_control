{
  Stimulus Control
  Copyright (C) 2018 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.PerformanceReview;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Graphics
    , Controls
    , Controls.Trials.Abstract
    , Controls.Trials.Helpers
    , Controls.Counters.PerformanceReview
    ;

type

  { TPerformanceReview }

  {
    Show hit and miss counters
  }
  TPerformanceReview = class(TTrial)
  private
    FHitsCounter : TCounterPR;
    FMissCounter : TCounterPR;
    FDataSupport : TDataSupport;
    procedure TrialKeyUp(Sender: TObject;
      var Key: Word; Shift: TShiftState);
    procedure TrialStart(Sender: TObject);
    procedure TrialBeforeEnd(Sender: TObject);
  protected
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TCustomControl); override;
    destructor Destroy; override;
    function AsString : string; override;
    procedure Play(ACorrection : Boolean=False);override;
  end;

resourcestring
  RSLabelHits = 'Acertos:';
  RSLabelMiss = 'Erros:';

implementation

uses timestamps, Session.Configuration.GlobalContainer;

{ TPerformanceReview }

procedure TPerformanceReview.TrialKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = 32 then
  begin
    if FDataSupport.Latency = TimeStart then
      FDataSupport.Latency := TickCount;
    EndTrial(Sender);
  end;
end;

procedure TPerformanceReview.TrialBeforeEnd(Sender: TObject);
begin
  WriteData(Sender);
end;

procedure TPerformanceReview.TrialStart(Sender: TObject);
begin
  FDataSupport.StmBegin := TickCount;
  FHitsCounter.Show;
  FMissCounter.Show;
end;

procedure TPerformanceReview.WriteData(Sender: TObject);
var
  LLatency : string;
begin
  inherited WriteData(Sender);

  if FDataSupport.Latency = TimeStart then
    LLatency := 'NA'
  else LLatency := TimestampToStr(FDataSupport.Latency - TimeStart);

  Data :=  Data +
           TimestampToStr(FDataSupport.StmBegin - TimeStart) + #9 +
           LLatency;
end;

constructor TPerformanceReview.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialKeyUp := @TrialKeyUp;
  OnTrialStart := @TrialStart;

  Header := 'StmBegin' + #9 +
             '_Latency';
  Data := '';
end;

destructor TPerformanceReview.Destroy;
begin

  inherited Destroy;
end;

function TPerformanceReview.AsString: string;
begin
  Result := '';
end;

procedure TPerformanceReview.Play(ACorrection: Boolean);
begin
  inherited Play(ACorrection);
  RootMedia:= GlobalContainer.RootMedia;

  FHitsCounter := TCounterPR.Create(Self);
  FHitsCounter.Cursor := Cursor;
  FHitsCounter.Caption := RSLabelHits+#32+IntToStr(Counters.BlcHits);
  FHitsCounter.CentralizeLeft;

  FMissCounter := TCounterPR.Create(Self);
  FHitsCounter.Cursor := Cursor;
  FMissCounter.Caption := RSLabelMiss+#32+IntToStr(Counters.BlcMisses);
  FMissCounter.CentralizeRight;

  if Self.ClassType = TPerformanceReview then Config(Self);
end;


end.


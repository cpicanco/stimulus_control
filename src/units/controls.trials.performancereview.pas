{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.PerformanceReview;

{$mode objfpc}{$H+}

interface

uses LCLIntf, Classes, SysUtils, Graphics
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
    procedure TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrialStart(Sender: TObject);
    procedure TrialBeforeEnd(Sender: TObject);
  protected
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Play(ACorrection : Boolean=False);override;
  end;

resourcestring
  RSLabelHits = 'Acertos:';
  RSLabelMiss = 'Erros:';

implementation

uses timestamps, Session.Configuration;

{ TPerformanceReview }

procedure TPerformanceReview.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (key = 32) { space } then
    EndTrial(Self);
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

  if Assigned(OnTrialWriteData) then OnTrialWriteData(Self);
end;

constructor TPerformanceReview.Create(AOwner: TComponent);
var
  LCfgTrial : TCfgTrial;
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialKeyUp := @TrialKeyUp;
  OnTrialStart := @TrialStart;

  Header := 'StmBegin' + #9 +
             '_Latency';
  Data := '';
  LCfgTrial.Id := 0;
  LCfgTrial.Kind:= 'PRW';
  LCfgTrial.Name := 'Performance Review';
  LCfgTrial.NumComp:=0;
  LCfgTrial.SList:=TStringList.Create;
  CfgTrial := LCfgTrial;
end;

destructor TPerformanceReview.Destroy;
begin
  if Assigned(CfgTrial.SList) then
    CfgTrial.SList.Free;
  FHitsCounter.Free;
  FMissCounter.Free;
  inherited Destroy;
end;

procedure TPerformanceReview.Play(ACorrection: Boolean);
begin
  FIscorrection := ACorrection;
  RootMedia:= GlobalContainer.RootMedia;

  FHitsCounter := TCounterPR.Create(Owner);
  FHitsCounter.Caption := RSLabelHits+#32+IntToStr(CounterManager.Hits);
  FHitsCounter.LoadImage(RootMedia+'CSQ1.png');
  FHitsCounter.CentralizeLeft;

  FMissCounter := TCounterPR.Create(Owner);
  FMissCounter.Caption := RSLabelMiss+#32+IntToStr(CounterManager.Misses);
  FMissCounter.LoadImage(RootMedia+'CSQ2.png');
  FMissCounter.CentralizeRight;

  if Self.ClassType = TPerformanceReview then Config(Self);
end;


end.


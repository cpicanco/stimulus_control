{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.Performancereview.Custom;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils, StdCtrls, Graphics

  , Controls.Trials.Abstract
  , Controls.Trials.Helpers
  , Controls.Counters.PerformanceReview
  ;

type

  { TPerformanceReview }

  TPerformanceReview = class(TTrial)
  private
    FHitsCounter : TCounterPR;
    FMissCounter : TCounterPR;
    FDataSupport : TDataSupport;
    procedure TrialBeforeEnd(Sender: TObject);
    procedure TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrialStart(Sender: TObject);
  protected
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TCustomControl); override;
    destructor Destroy;override;
    function AsString : string; override;
    procedure Play(ACorrection : Boolean); override;
  end;

resourcestring
  RSLabelHits = 'Acertos:';
  RSLabelMiss = 'Erros:';

implementation

uses Constants, Timestamps, Session.Configuration.GlobalContainer;

constructor TPerformanceReview.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialKeyUp := @TrialKeyUp;
  OnTrialStart := @TrialStart;

  Header := 'StmBegin' + #9 +
             '_Latency' + #9 +
             'Bloc.Hits' + #9 +
             'Bloc.Misses';
  Result := T_NONE;
  IETConsequence := T_NONE;
  Result := T_NONE;
end;

destructor TPerformanceReview.Destroy;
begin
  FHitsCounter.Free;
  FMissCounter.Free;
  inherited Destroy;
end;

function TPerformanceReview.AsString: string;
begin
  Result := '';
end;

procedure TPerformanceReview.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 32 then
    EndTrial(Sender);
end;

procedure TPerformanceReview.TrialBeforeEnd(Sender: TObject);
begin
  FDataSupport.StmEnd := TickCount;
  WriteData(Sender);
end;

procedure TPerformanceReview.Play(ACorrection : Boolean);
begin
  inherited Play(ACorrection);
  FHitsCounter := TCounterPR.Create(Owner);
  FHitsCounter.Cursor := Cursor;
  FHitsCounter.Caption := RSLabelHits+#32+IntToStr(Counters.BlcHits);
  FHitsCounter.CentralizeLeft;

  FMissCounter := TCounterPR.Create(Owner);
  FHitsCounter.Cursor := Cursor;
  FMissCounter.Caption := RSLabelMiss+#32+IntToStr(Counters.BlcMisses);
  FMissCounter.CentralizeRight;
  if Self.ClassType = TPerformanceReview then Config(Self);
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
           LLatency + #9 +
           IntToStr(Counters.BlcHits) + #9 +
           IntToStr(Counters.BlcMisses);
end;

end.

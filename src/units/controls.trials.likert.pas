unit Controls.Trials.Likert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , Controls
  , Controls.Likert
  , Controls.Trials.Abstract
  , Controls.Trials.Helpers
  ;

type

  { TLikertTrial }

  TLikertTrial = record
    Left : string;
    Right : string;
  end;

  { TLikert }

  TLikert = class(TTrial)
  private
    FDataSupport : TDataSupport;
    FLikertScale : TLikertScale;
    procedure TrialBeforeEnd(Sender: TObject);
    procedure TrialStart(Sender: TObject);
    procedure LikertScaleSelectionChange(Sender: TObject);
    procedure LikertScaleConfirmation(Sender: TObject);
  protected
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TCustomControl); override;
    destructor Destroy; override;
    procedure Hide; override;
    procedure Show; override;
    function AsString : string; override;
    procedure Play(ACorrection : Boolean); override;
  end;

implementation

uses Constants, Timestamps, Session.ConfigurationFile;

{ TLikert }

procedure TLikert.TrialBeforeEnd(Sender: TObject);
begin
  FDataSupport.StmEnd := TickCount;
  WriteData(Self);
end;

procedure TLikert.TrialStart(Sender: TObject);
begin
  FLikertScale.Show;
  FDataSupport.StmBegin := TickCount;
end;

procedure TLikert.LikertScaleSelectionChange(Sender: TObject);
begin
  LogEvent('selection_change,' + FLikertScale.AsString);
end;

procedure TLikert.LikertScaleConfirmation(Sender: TObject);
begin
  LogEvent('confirmation,' + FLikertScale.AsString);
  FDataSupport.Latency := TickCount;
  EndTrial(Sender);
end;

procedure TLikert.WriteData(Sender: TObject);
var LStart, LDuration, LLatency : string;
begin
  inherited WriteData(Sender);
  LStart := TimestampToStr(FDataSupport.StmBegin - TimeStart);
  LDuration := TimestampToStr(FDataSupport.StmEnd - TimeStart);
  LLatency := TimestampToStr(FDataSupport.Latency - TimeStart);
  Data := Data +
    LStart + #9 +
    LDuration + #9 +
    LLatency + #9 +
    FLikertScale.AsString;
  if Assigned(OnTrialWriteData) then OnTrialWriteData(Sender);
end;

constructor TLikert.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialStart := @TrialStart;
  FLikertScale := TLikertScale.Create(Self, Self.Parent);
  FLikertScale.OnConfirmation := @LikertScaleConfirmation;
  FLikertScale.OnSelectionChanged := @LikertScaleSelectionChange;
  Header :=
    Header + #9 +
    rsReportStmBeg + #9 +
    rsReportStmDur + #9 +
    rsReportRspLat + #9 +
    rsReportEvent;

  IETConsequence := T_NONE;
  Result := T_NONE;
end;

destructor TLikert.Destroy;
begin
  inherited Destroy;
end;

procedure TLikert.Hide;
begin
  Visible := False;
  FLikertScale.Hide;
end;

procedure TLikert.Show;
begin
  Visible := True;
  FLikertScale.Show;
end;

function TLikert.AsString: string;
var
  LTrial : TStringList;
begin
  LTrial := TStringList.Create;
  LTrial.Append(TConfigurationFile.FullTrialSection(
    CounterManager.CurrentBlc, CounterManager.CurrentTrial));
  LTrial.Values[_Kind] := T_LIK;
  LTrial.Values[_Cursor] := IntToStr(Cursor);
  LTrial.Values[_Left] := FLikertScale.FilenameLeft;
  LTrial.Values[_Right] := FLikertScale.FilenameRight;
  Result := LTrial.Text;
  LTrial.Free;
end;

procedure TLikert.Play(ACorrection: Boolean);
var
  LParameters : TStringList;
begin
  inherited Play(ACorrection);
  LParameters := Configurations.SList;

  FLikertScale.LoadImagePair(
    RootMedia + LParameters.Values[_Left],
    RootMedia + LParameters.Values[_Right]);
  FLikertScale.Hide;

  if Self.ClassType = TLikert then Config(Self);
end;


end.


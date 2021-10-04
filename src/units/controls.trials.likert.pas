unit Controls.Trials.Likert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , Controls
  , Controls.Likert
  , Controls.Trials.Abstract
  ;

type

  { TLikertTrial }

  TLikertTrial = record
    Left : string;
    Right : string;
  end;

  TDataLine = record
    BackgroundResponses,
    Responses : integer;
    Latency,
    StmBegin,
    StmEnd : Extended;
    LikertScale : string;
  end;

  { TLikert }

  TLikert = class(TTrial)
  private
    FDataLine : TDataLine;
    FLikertScale : TLikertScale;
    procedure TrialBeforeEnd(Sender: TObject);
    procedure TrialStart(Sender: TObject);
    procedure LikertScaleClick(Sender: TObject);
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

uses Constants, Timestamps,
  Session.ConfigurationFile,
  Session.Configuration.GlobalContainer;

{ TLikert }

procedure TLikert.TrialBeforeEnd(Sender: TObject);
begin
  FDataLine.StmEnd := TickCount;
  WriteData(Self);
end;

procedure TLikert.TrialStart(Sender: TObject);
begin
  FLikertScale.Show;
  FDataLine.StmBegin := TickCount;
end;

procedure TLikert.LikertScaleClick(Sender: TObject);
begin
  FDataLine.Latency := LogEvent('LikertScaleClick,' + FLikertScale.AsString);
  FDataLine.LikertScale := FLikertScale.AsString;
  EndTrial(Sender);
end;

procedure TLikert.WriteData(Sender: TObject);
var LStart : string;
begin
  inherited WriteData(Sender);
  LStart := TimestampToStr(FDataLine.StmBegin - TimeStart);
  Data := Data +
    LStart + #9 +
    TimestampToStr(FDataLine.Latency) + #9 +
    FDataLine.LikertScale;
end;

constructor TLikert.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialStart := @TrialStart;
  FLikertScale := TLikertScale.Create(Self, Self.Parent);
  FLikertScale.OnClick := @LikertScaleClick;
  Header :=
    Header + #9 +
    rsReportStmBeg + #9 +
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
    Counters.CurrentBlc, Counters.CurrentTrial));
  LTrial.Values[_Kind] := T_LIK;
  LTrial.Values[_Cursor] := IntToStr(Cursor);
  LTrial.Values[_Left] := FLikertScale.FilenameLeft;
  LTrial.Values[_Right] := FLikertScale.FilenameRight;
  Result := LTrial.Text;
  LTrial.Free;
end;

procedure TLikert.Play(ACorrection: Boolean);
var
  Parameters : TStringList;
begin
  inherited Play(ACorrection);
  Parameters := Configurations.Parameters;

  FLikertScale.LoadImagePair(
    RootMedia + Parameters.Values[_Left],
    RootMedia + Parameters.Values[_Right]);
  FLikertScale.Hide;

  if Self.ClassType = TLikert then Config(Self);
end;


end.


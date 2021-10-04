unit Controls.Trials.ClickAndSound;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , Controls
  , Controls.Stimuli.Key
  , Controls.Trials.Abstract
  , Controls.Trials.Helpers
  , CastleSoundEngine
  ;

type

  { TClickSound }

  TClickSound = class(TTrial)
  private
    FDataSupport : TDataSupport;
    FStimulus : TKey;
    FSoundBuffer: TSoundBuffer;
    procedure TrialBeforeEnd(Sender: TObject);
    procedure TrialStart(Sender: TObject);
    procedure BackgroundMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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

uses Constants, Timestamps, Session.ConfigurationFile
  , Session.Configuration.GlobalContainer;

{ TClickSound }

procedure TClickSound.TrialBeforeEnd(Sender: TObject);
begin
  FDataSupport.StmEnd := TickCount;
  WriteData(Self);
end;

procedure TClickSound.TrialStart(Sender: TObject);
begin
  FStimulus.Show;
  FDataSupport.StmBegin := TickCount;
  OnMouseDown:=@BackgroundMouseDown;
end;

procedure TClickSound.BackgroundMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    SoundEngine.PlaySound(FSoundBuffer);
end;

//procedure TClickSound.TrialClick(Sender: TObject);
//begin
//  LogEvent('selection_change,' + FStimulus.AsString);
//end;

//procedure TClickSound.LikertScaleConfirmation(Sender: TObject);
//begin
//  LogEvent('confirmation,' + FStimulus.AsString);
//  FDataSupport.Latency := TickCount;
//  EndTrial(Sender);
//end;

procedure TClickSound.WriteData(Sender: TObject);
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
    FStimulus.ShortName;
  //if Assigned(OnTrialWriteData) then OnTrialWriteData(Sender);
end;

constructor TClickSound.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialStart := @TrialStart;

  Header :=
    Header + #9 +
    rsReportStmBeg + #9 +
    rsReportStmDur + #9 +
    rsReportRspLat + #9 +
    rsReportEvent;

  IETConsequence := T_NONE;
  Result := T_NONE;
end;

destructor TClickSound.Destroy;
begin
  inherited Destroy;
end;

procedure TClickSound.Hide;
begin
  Visible := False;
  FStimulus.Hide;
end;

procedure TClickSound.Show;
begin
  Visible := True;
  FStimulus.Show;
end;

function TClickSound.AsString: string;
var
  LTrial : TStringList;
begin
  LTrial := TStringList.Create;
  LTrial.Append(TConfigurationFile.FullTrialSection(
    Counters.CurrentBlc, Counters.CurrentTrial));
  LTrial.Values[_Kind] := T_LIK;
  LTrial.Values[_Cursor] := IntToStr(Cursor);
  LTrial.Values[_cStm] := ExtractFileName(FStimulus.Filename);
  Result := LTrial.Text;
  LTrial.Free;
end;

procedure TClickSound.Play(ACorrection: Boolean);
var
  LParameters : TStringList;
begin
  inherited Play(ACorrection);
  LParameters := Configurations.Parameters;
  FSoundBuffer := SoundEngine.LoadBuffer(RootMedia+'tone.wav');

  FStimulus := TKey.Create(Self.Parent);
  FStimulus.Parent := Self.Parent;
  FStimulus.Centralize;
  FStimulus.Filename := RootMedia + LParameters.Values[_cStm];
  FStimulus.Hide;

  if Self.ClassType = TClickSound then Config(Self);
end;

initialization
  SoundEngine.MinAllocatedSources := 4;
  SoundEngine.ALContextOpen;

end.



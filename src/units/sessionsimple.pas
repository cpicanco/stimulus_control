unit SessionSimple;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Session.BlocsSimple;

type

  { TSession }

  TSession = class(TComponent)
  private
    FBloc : TBloc;
    FOnBeforeStart: TNotifyEvent;
    FOnEndSession: TNotifyEvent;
    function GetBaseFilename: string;
    procedure PlayBloc;
    procedure EndBloc(Sender : TObject);
    procedure EndSession;
    procedure SetOnBeforeStart(AValue: TNotifyEvent);
    procedure SetOnEndSession(AValue: TNotifyEvent);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Play(ASessionName : string; AParticipantName: string);
    property OnEndSession : TNotifyEvent read FOnEndSession write SetOnEndSession;
    property OnBeforeStart : TNotifyEvent read FOnBeforeStart write SetOnBeforeStart;
    property BaseFilename : string read GetBaseFilename;
  end;

implementation

uses
  Timestamps
, FileUtil
, LazFileUtils
, Loggers.Reports
, Session.ConfigurationFile
, Session.Configuration.GlobalContainer;

{ TSession }

var
  Header : string;
  FirstFilename: string = '001';
  DataFilename : string = '';
  TimestampsFilename : string = '';

procedure TSession.PlayBloc;
begin
  with GlobalContainer.CounterManager do
  if CurrentBlc < ConfigurationFile.BlocCount then
    begin
      FBloc.BeforePlay;
      SetVirtualTrialValue(ConfigurationFile.Bloc[CurrentBlc+1].VirtualTrialValue);
      FBloc.Play;
    end
  else EndSession;
end;

function TSession.GetBaseFilename: string;
begin
  if DataFilename = '' then
    Result := FirstFilename
  else
    Result := ExtractFileNameWithoutExt(DataFilename);
end;

procedure TSession.EndBloc(Sender: TObject);
begin
  GlobalContainer.CounterManager.OnEndBlc(Sender);
  PlayBloc;
end;

procedure TSession.EndSession;
var
  Footer : string;
begin
  Footer := HEND_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time)+ LineEnding;
  FreeLogger(LGTimestamps,Footer);
  FreeLogger(LGData, Footer);
  if Assigned(OnEndSession) then OnEndSession(Self);
end;

procedure TSession.SetOnBeforeStart(AValue: TNotifyEvent);
begin
  if FOnBeforeStart=AValue then Exit;
  FOnBeforeStart:=AValue;
end;

procedure TSession.SetOnEndSession(AValue: TNotifyEvent);
begin
  if FOnEndSession=AValue then Exit;
  FOnEndSession:=AValue;
end;

constructor TSession.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //MakeConfigurationFile(0, SessionBlocs);
  FirstFilename := GlobalContainer.RootData + FirstFilename;
  FBloc := TBloc.Create(Self);
  FBloc.OnEndBloc := @EndBloc;
  //FBloc.OnInterTrialStop := @InterTrialStop;
end;

destructor TSession.Destroy;
begin
  ConfigurationFile.Free;
  inherited Destroy;
end;

procedure TSession.Play(ASessionName: string; AParticipantName: string);
begin
  Header := HSUBJECT_NAME + #9 + AParticipantName + LineEnding +
            HSESSION_NAME + #9 + ASessionName + LineEnding +
            HBEGIN_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time) + LineEnding;
  DataFilename := CreateLogger(LGData, FirstFilename, Header);
  TimestampsFilename := CreateLogger(LGTimestamps, FirstFilename, Header);
  if Assigned(OnBeforeStart) then OnBeforeStart(Self);
  GlobalContainer.TimeStart := TickCount;
  PlayBloc;
end;

end.


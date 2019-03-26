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
    FOnEndSession: TNotifyEvent;
    procedure PlayBloc;
    procedure EndBloc(Sender : TObject);
    procedure EndSession;
    procedure SetOnEndSession(AValue: TNotifyEvent);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Play(ASessionName : string; AParticipantName: string);
    property OnEndSession : TNotifyEvent read FOnEndSession write SetOnEndSession;
  end;

implementation

uses
  Timestamps
, FileUtil
, Experiments.Maues
, Loggers.Reports
, Session.ConfigurationFile
, Session.Configuration.GlobalContainer;

{ TSession }

var
  Header : string;
  DataFile : string;

procedure TSession.PlayBloc;
begin
  with GlobalContainer.CounterManager do
  if CurrentBlc < ConfigurationFile.BlocCount then
    begin
      SetVirtualTrialValue(ConfigurationFile.Bloc[CurrentBlc+1].VirtualTrialValue);
      FBloc.Play;
    end
  else EndSession;
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

procedure TSession.SetOnEndSession(AValue: TNotifyEvent);
begin
  if FOnEndSession=AValue then Exit;
  FOnEndSession:=AValue;
end;

constructor TSession.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MakeConfigurationFile(0, 6);
  FBloc := TBloc.Create(Self);
  FBloc.OnEndBloc := @EndBloc;
  //FBloc.OnInterTrialStop := @InterTrialStop;
end;

destructor TSession.Destroy;
begin
  ConfigurationFile.Free;
  inherited Destroy;
end;

procedure TSession.Play(ASessionName : string; AParticipantName: string);
begin
  DataFile := '001';
  Header := HSUBJECT_NAME + #9 + AParticipantName + LineEnding +
            HSESSION_NAME + #9 + ASessionName + LineEnding +
            HBEGIN_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time) + LineEnding;

  DataFile := GlobalContainer.RootData + DataFile;
  CreateLogger(LGData, DataFile, Header);
  DataFile := CreateLogger(LGTimestamps, DataFile, Header);
  CopyFile(ConfigurationFilename, ExtractFileNameWithoutExt(DataFile)+'.ini');
  GlobalContainer.TimeStart := TickCount;
  PlayBloc;
end;

end.


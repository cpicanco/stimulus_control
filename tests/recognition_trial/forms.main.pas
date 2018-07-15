unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TBackground }

  TBackground = class(TForm)
    ButtonStart: TButton;
    EditParticipant: TEdit;
    OpenDialog: TOpenDialog;
    RadioGroupCondition: TRadioGroup;
    procedure ButtonStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure InterTrialStop(Sender: TObject);
    procedure EndBloc(Sender: TObject);
    //procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure RadioGroupConditionSelectionChanged(Sender: TObject);
  private
    FHeader : string;
    //procedure ShowTrialConsole;
  public
    {$IFDEF WINDOWS}
    OriginalBounds: TRect;
    OriginalWindowState: TWindowState;
    ScreenBounds: TRect;
    procedure SwitchFullScreen;
    {$ENDIF}
  end;

var
  Background: TBackground;

implementation

{$R *.lfm}

uses FileUtil
   , Timestamps
   , Loggers.Reports
   , Session.BlocsSimple
   , Session.Backgrounds
   , Session.ConfigurationFile
   , Session.Configuration.GlobalContainer
   , Experiments.Ravi
   ;

{ TBackground }

var
  FDataFile : string;
  Bloc : TBloc;

procedure ClearConfigurationFile;
begin
  ConfigurationFile.Free;
  NewConfigurationFile;
end;

procedure TBackground.ButtonStartClick(Sender: TObject);
var
  SessionName : string;
begin
  ButtonStart.Enabled:=False;
  ButtonStart.Hide;
  RadioGroupCondition.Enabled:=False;
  RadioGroupCondition.Hide;
  EditParticipant.Enabled:=False;
  EditParticipant.Hide;
  Session.Backgrounds.Background := Self;
  {$IFDEF WINDOWS}SwitchFullScreen;{$ENDIF}

  SessionName := RadioGroupCondition.Items[RadioGroupCondition.ItemIndex];
  FDataFile := '001';
  FHeader := HSUBJECT_NAME + #9 + EditParticipant.Text + LineEnding +
             HSESSION_NAME + #9 + SessionName + LineEnding +
             HBEGIN_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time) + LineEnding;

  FDataFile := GlobalContainer.RootData + FDataFile;
  CreateLogger(LGData, FDataFile, FHeader);
  FDataFile := CreateLogger(LGTimestamps, FDataFile, FHeader);
  CopyFile(ConfigurationFilename, ExtractFileNameWithoutExt(FDataFile)+'.ini');
  GlobalContainer.TimeStart := TickCount;
  Bloc.Play;
end;

procedure TBackground.FormCreate(Sender: TObject);
begin
  Bloc := TBloc.Create(Self);
  Bloc.OnEndBloc := @EndBloc;
  Bloc.OnInterTrialStop := @InterTrialStop;
end;

procedure TBackground.InterTrialStop(Sender: TObject);
var
  MouseOnset : TPoint;
begin
  MouseOnset.X := Screen.Width div 2;
  MouseOnset.Y := Screen.Height div 2;
  Mouse.CursorPos := MouseOnset;
end;

procedure TBackground.EndBloc(Sender: TObject);
var
  Footer : string;
begin
  ShowMessage('Fim.');
  Footer := HEND_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time)+ LineEnding;
  FreeLogger(LGTimestamps,Footer);
  FreeLogger(LGData, Footer);
  WindowState := wsNormal;
end;

//procedure TBackground.FormKeyPress(Sender: TObject; var Key: char);
//begin
//  case key of
//    't' : ShowTrialConsole;
//  end;
//end;

procedure TBackground.RadioGroupConditionSelectionChanged(Sender: TObject);
begin
  ClearConfigurationFile;
  MakeConfigurationFile(RadioGroupCondition.ItemIndex);
end;

//procedure TBackground.ShowTrialConsole;
//var
//  LNextTrial : string;
//  LNextTrialI : integer;
//begin
//  LNextTrial := InputBox('Trial Console', 'Insert the Next Trial', '-');
//  LNextTrialI := StrToIntDef(LNextTrial, -1);
//  if LNextTrialI <> -1 then
//  begin
//    //while Trial = nil do Application.ProcessMessages;
//    GlobalContainer.CounterManager.CurrentTrial := LNextTrialI;
//  end;
//end;

{$IFDEF WINDOWS}
// http://wiki.freepascal.org/Application_full_screen_mode
procedure TBackground.SwitchFullScreen;
begin
  if BorderStyle <> bsNone then begin
    // To full screen
    OriginalWindowState := WindowState;
    OriginalBounds := BoundsRect;

    BorderStyle := bsNone;
    BoundsRect := Screen.MonitorFromWindow(Handle).BoundsRect;
  end else begin
    // From full screen
    BorderStyle := bsSizeable;
    if OriginalWindowState = wsMaximized then
      WindowState := wsMaximized
    else
      BoundsRect := OriginalBounds;
  end;
end;
{$ENDIF}

end.


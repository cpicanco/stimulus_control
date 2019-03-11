unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin;

type

  { TBackground }

  TBackground = class(TForm)
    ButtonShowStimuli: TButton;
    ButtonStart: TButton;
    EditParticipant: TEdit;
    LabelSessionBlocs: TLabel;
    PanelConfigurations: TPanel;
    RadioGroupCondition: TRadioGroup;
    SpinEditSessionBlocs: TSpinEdit;
    procedure ButtonShowStimuliClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    //procedure InterTrialStop(Sender: TObject);
    procedure EndSession(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure ExperimentParametersChange(Sender: TObject);
  private
    procedure ShowTrialConsole;
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

uses
     Session.Backgrounds
   , Session.ConfigurationFile
   , Session.Configuration.GlobalContainer
   , SessionSimple
   , Experiments.Maues
   , Forms.CheckStimuli
   ;

{ TBackground }

var
  LSession : TSession;

procedure TBackground.ButtonStartClick(Sender: TObject);
begin
  PanelConfigurations.Hide;
  if FormCheckStimuli.Visible then
    FormCheckStimuli.Hide;

  Session.Backgrounds.Background := Self;
  {$IFDEF WINDOWS}SwitchFullScreen;{$ENDIF}
  LSession.Play(RadioGroupCondition.Items[RadioGroupCondition.ItemIndex], EditParticipant.Text);
end;

procedure TBackground.ButtonShowStimuliClick(Sender: TObject);
begin
  ShowStimuli;
end;

procedure TBackground.FormCreate(Sender: TObject);
begin
  LSession := TSession.Create(Self);
  LSession.OnEndSession:=@EndSession;
end;

//procedure TBackground.InterTrialStop(Sender: TObject);
//var
//  MouseOnset : TPoint;
//begin
//  MouseOnset.X := Screen.Width div 2;
//  MouseOnset.Y := Screen.Height div 2;
//  Mouse.CursorPos := MouseOnset;
//end;

procedure TBackground.EndSession(Sender: TObject);
begin
  ShowMessage('Fim.');
  WindowState := wsNormal;
end;

procedure TBackground.FormKeyPress(Sender: TObject; var Key: char);
begin
  case key of
    't' : ShowTrialConsole;
  end;
end;

procedure TBackground.ExperimentParametersChange(Sender: TObject);
begin
  ConfigurationFile.Free;
  MakeConfigurationFile(RadioGroupCondition.ItemIndex, SpinEditSessionBlocs.Value);
end;


procedure TBackground.ShowTrialConsole;
var
  LNextTrial : string;
  LNextTrialI : integer;
begin
  LNextTrial := InputBox('Trial Console', 'Insert the Next Trial', '-');
  LNextTrialI := StrToIntDef(LNextTrial, -1);
  if LNextTrialI <> -1 then
  begin
    //while Trial = nil do Application.ProcessMessages;
    GlobalContainer.CounterManager.CurrentTrial := LNextTrialI;
  end;
end;

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


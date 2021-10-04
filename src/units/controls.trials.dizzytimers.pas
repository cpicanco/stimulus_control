{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.DizzyTimers;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils

  , Controls.Trials.Abstract
  , Schedules
  , Controls.Stimuli.Key
  , custom_timer
  ;

type

  TDataSupport = record
    Responses : integer;
    Cycle,
    Latency,
    StmBegin,
    Timer2 : Extended;
  end;

  TDizzyTimer = record
    Color1 : Boolean;
    Color2 : Boolean;
    Host : cardinal;
    Main : cardinal;
    Max : cardinal;
    Min : cardinal;
    Timer1 : TClockThread;
    Timer2 : TClockThread;
    Version : string;
    Mode : string;
    Schedule : string;
  end;

  { TDZT }

  // free operant style
  TDZT = Class(TTrial)
  private
    FConsequence : string;
    FCycles : integer;
    FDataSupport : TDataSupport;
    FDizzyTimer : TDizzyTimer;
    FSchedule : TSchedule;
    FStimuli : array of TRect;
    function GetCycles: integer;
    function RandomInRange(AFrom, ATo : integer):integer;
    procedure Consequence(Sender: TObject);
    procedure Response(Sender: TObject);
    procedure TrialBeforeEnd(Sender: TObject);
    procedure TrialResult(Sender: TObject);
    procedure TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrialPaint;
    procedure TrialStart(Sender: TObject);
    procedure UpdateTimer1(Sender: TObject);
    procedure UpdateTimer2(Sender: TObject);
  protected { TTrial }
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TCustomControl); override;
    destructor Destroy; override;
    procedure Play(ACorrection : Boolean); override;
    property Cycles : integer read GetCycles;

  end;

implementation

uses strutils, constants, timestamps, Session.Configuration.GlobalContainer
     {$ifdef DEBUG}
     , Loggers.Debug
     , dialogs
     {$endif}
     ;
constructor TDZT.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialKeyUp := @TrialKeyUp;
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialStart := @TrialStart;
  OnTrialPaint := @TrialPaint;

  Header :=  rsReportStmBeg + #9 +
             rsReportRspLat + #9 +
             '___Cycle' + #9 +
             '__Timer2' + #9 +
             '_Version' + #9 +
             '____Mode' + #9 +
             rsReportRspFrq;

  FCycles := 0;
  FDataSupport.Responses:= 0;
end;

destructor TDZT.Destroy;
begin
  with FDizzyTimer do
    begin
      if Assigned(Timer1) then
        begin
          Timer1.Enabled := False;
          Timer1.Terminate;
        end;
      if Assigned(Timer2) then
        begin
          Timer2.Enabled := False;
          Timer2.Terminate;
        end;
    end;
  inherited Destroy;
end;

function TDZT.GetCycles: integer;
begin
  Result := FCycles;
end;

function TDZT.RandomInRange(AFrom, ATo: integer): integer;
begin
  Result := Random(ATo - AFrom + 1) + AFrom;
end;

procedure TDZT.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 32 {space} then
    begin
      FSchedule.DoResponse;
      // first response
      if FDataSupport.Latency = TimeStart then
          FDataSupport.Latency := TickCount;
      LogEvent('R');
    end;
end;

procedure TDZT.Consequence(Sender: TObject);
var LConsequence : TKey;
begin
  LConsequence := TKey.Create(Parent);
  with LConsequence do
    begin
      Parent:= TCustomControl(Owner);
      Loops := 0;
      Filename := RootMedia + FConsequence;
      Play;
    end;
  LogEvent('C');

  if Assigned(Counters.OnConsequence) then Counters.OnConsequence(Self);
end;

procedure TDZT.TrialResult(Sender: TObject);
begin
  Result := T_NONE;
  IETConsequence := T_NONE;
end;

procedure TDZT.UpdateTimer1(Sender: TObject);
var LTickCount : Extended;
    ACode : string;
begin
  LTickCount := TickCount;
  FDataSupport.Cycle := LTickCount;
  Inc(FCycles);

  with FDizzyTimer do
    begin
      Main := RandomInRange(Min, Max);
      Host := RandomInRange(Main div 3,(2 * Main) div 3);

      if TClockThread(Sender) = Timer1 then
        begin
          Timer1.Interval := Main * 1000;
          Timer2.Interval := Host * 1000;
          Timer2.Enabled := True;
          Color1 := not Color1;
          if Color1 then
            ACode := '1a'
          else
            ACode := '1b';
        end;

      if TClockThread(Sender) = Timer2 then
        begin
          Timer2.Interval := Main * 1000;
          Timer1.Interval := Host * 1000;
          Timer1.Enabled := True;
          Color2 := not Color2;
          if Color2 then
            ACode := '2a'
          else
            ACode := '2b';
        end;
    end;

  if FDizzyTimer.Mode = 'A' then // do nothing
  else
    if FDizzyTimer.Mode = 'B' then
      if FSchedule.ScheduleName = Schedules.EXT then
        begin
          FSchedule.Load(FDizzyTimer.Schedule);
        end
      else
        begin
          FSchedule.Load(Schedules.EXT);
        end;
  LogEvent(ACode);
  Invalidate;

  // must pass Self here, see TBLC.WriteTrialData
  WriteData(Self);
end;

procedure TDZT.UpdateTimer2(Sender: TObject);
var
  LTickCount : Extended;
  ACode : string;
begin
  TClockThread(Sender).Enabled := False;

  if TClockThread(Sender) = FDizzyTimer.Timer1 then
    begin
      FDizzyTimer.Color1 := not FDizzyTimer.Color1;
      if FDizzyTimer.Color1 then
        ACode := '1a'
      else
        ACode := '1b';
    end;

  if TClockThread(Sender) = FDizzyTimer.Timer2 then
    begin
      FDizzyTimer.Color2 := not FDizzyTimer.Color2;
      if FDizzyTimer.Color2 then
        ACode := '2a'
      else
        ACode := '2b';
    end;

  LTickCount := TickCount;
  FDataSupport.Timer2 := LTickCount;
  LogEvent(ACode);

  Invalidate;
  FDataSupport.Latency := TimeStart;
end;


procedure TDZT.TrialPaint;
const
  clRed : integer = $FF0000;
  clGreen : integer = $00FF00;
  clBlue : integer = $0000FF;
  clCiano : integer = $FFFF00;
begin
  if FDizzyTimer.Color1 then
    Canvas.Pen.Color := clBlue
  else Canvas.Pen.Color := clRed;

  Canvas.Brush.Color := Canvas.Pen.Color;
  Canvas.Rectangle(FStimuli[0]);

  if FDizzyTimer.Color2 then
    Canvas.Pen.Color := clGreen
  else Canvas.Pen.Color := clCiano;

  Canvas.Brush.Color := Canvas.Pen.Color;
  Canvas.Ellipse(FStimuli[1]);
end;

procedure TDZT.Play(ACorrection: Boolean);
var
  s1 : string;
  R : TRect;

  NumComp, a1 : Integer;
begin
  inherited Play(ACorrection);
  FResponseEnabled := False;

  // self
  FDizzyTimer.Schedule := Configurations.Parameters.Values[_Schedule];
  FSchedule := TSchedule.Create(self);
  with FSchedule do
    begin
      OnConsequence := @Consequence;
      OnResponse:= @Response;
      Load(FDizzyTimer.Schedule);
    end;

  if FSchedule.Loaded then
    //AddToClockList(FSchedule)
  else
    raise Exception.Create(ExceptionNoScheduleFound);

  s1 := Configurations.Parameters.Values[_Trial + _cRes] + #32;

  with FDizzyTimer do
    begin
      Color1 := True;
      Color2 := True;

      Min := StrToIntDef(ExtractDelimited(1,s1,[#32]), 0);
      Max := StrToIntDef(ExtractDelimited(2,s1,[#32]), 0);
      Version := ExtractDelimited(3,s1,[#32]);
      Mode := ExtractDelimited(4,s1,[#32]);
      Mode := UpperCase(Mode);

      Main := RandomInRange(Min, Max);
      Host := RandomInRange(Main div 3,(2 * Main) div 3); // 1/3 and 2/3  of main

      Timer1 := TClockThread.Create(True);
      Timer2 := TClockThread.Create(True);

      s1 := UpperCase(Version);
      if (s1 = 'L') or (s1 = 'LEFT') then
        begin
          Timer1.Interval := Main * 1000;
          Timer2.Interval := Host * 1000;
          Timer1.OnTimer := @UpdateTimer1;
          Timer2.OnTimer := @UpdateTimer2;
        end
      else if (s1 = 'R') or (s1 = 'RIGHT') then
        begin
          Timer1.Interval := Host * 1000;
          Timer2.Interval := Main * 1000;
          Timer1.OnTimer := @UpdateTimer2;
          Timer2.OnTimer := @UpdateTimer1;
        end;

      //AddToClockList(@Timer1.Start);
      //AddToClockList(@Timer2.Start);

      {$ifdef DEBUG}
        DebugLn(mt_Debug + 'FDizzyTimer:' + IntToStr(Main) +','+ IntToStr(Host) );
      {$endif}
    end;

  FConsequence := Configurations.Parameters.Values[_Trial + _cCsq];

  if FConsequence = T_HIT then FConsequence := 'CSQ1.wav';
  if FConsequence = T_MISS then FConsequence := 'CSQ2.wav';

  NumComp := StrToIntDef(Configurations.Parameters.Values[_NumComp], 0);
  SetLength(FStimuli, NumComp);

  for a1 := 0 to NumComp -1 do
    begin
        s1:= Configurations.Parameters.Values[_Comp + IntToStr(a1+1) + _cBnd] + #32;

        R.Top:= StrToIntDef(ExtractDelimited(1,s1,[#32]), 0);
        R.Left:= StrToIntDef(ExtractDelimited(2,s1,[#32]), 0);
        R.Bottom := R.Top + StrToIntDef(ExtractDelimited(3,s1,[#32]), 100);
        R.Right := R.Left + StrToIntDef(ExtractDelimited(4,s1,[#32]), 100);

        {$ifdef DEBUG}
          DebugLn(mt_Debug + 'R' + IntToStr(a1+1) + ':' +
            IntToStr(R.Top) +','+ IntToStr(R.Left)+','+ IntToStr(R.Bottom)+','+ IntToStr(R.Right));
        {$endif}

        FStimuli[a1] := R;
    end;
  if Self.ClassType = TDZT then Config(Self);
end;


procedure TDZT.TrialStart(Sender: TObject);
var
  LTickCount : Extended;

  procedure KeyStart(var aKey : TKey);
  begin
    aKey.Play;
    aKey.Visible:= True;
  end;

begin
  FDataSupport.Latency := TimeStart;
  LTickCount := TickCount;

  with FDataSupport do
    begin
      Cycle := TimeStart;
      Latency := TimeStart;
      Timer2 := TimeStart;
    end;

  FDataSupport.StmBegin := LTickCount;
end;

procedure TDZT.TrialBeforeEnd(Sender: TObject);
begin
  FDataSupport.Cycle := TickCount;
  FDataSupport.Timer2 := TimeStart;
  TrialResult(Sender);
  WriteData(Self);
end;


procedure TDZT.WriteData(Sender: TObject);  //
var
    Latency, Timer2 : string;
begin
  inherited WriteData(Sender);
  if FDataSupport.Latency = TimeStart then
    Latency := #32#32#32#32#32#32 + 'NA'
  else Latency := TimestampToStr(FDataSupport.Latency - TimeStart);

  if FDataSupport.Timer2 = TimeStart then
    Timer2 := #32#32#32#32#32#32 + 'NA'
  else Timer2 := TimestampToStr(FDataSupport.Timer2 - TimeStart);

  Data := Data + #9 +
          TimestampToStr(FDataSupport.StmBegin - TimeStart) + #9 +
          Latency + #9 +
          TimestampToStr(FDataSupport.Cycle - TimeStart) + #9 +
          Timer2 + #9 +
          FDizzyTimer.Version + #9 +
          FDizzyTimer.Mode + #9 +
          Format('%-*.*d', [4, 8, FDataSupport.Responses]);

  //if Assigned(OnTrialWriteData) then OnTrialWriteData(Sender);
end;

procedure TDZT.Response(Sender: TObject);
begin
  Inc(FDataSupport.Responses);

  if Assigned(Counters.OnStmResponse) then Counters.OnStmResponse(Sender);
  if Assigned(OnStmResponse) then OnStmResponse (Self);
end;

end.

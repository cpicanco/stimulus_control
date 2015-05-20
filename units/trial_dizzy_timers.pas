//
// Validation Project (PCRF) - Stimulus Control App
// Copyright (C) 2014-2015,  Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.
//
// cpicanco@ufpa.br
//
// This file is part of Validation Project (PCRF).
//
// Validation Project (PCRF) is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Validation Project (PCRF) is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Validation Project (PCRF).  If not, see <http://www.gnu.org/licenses/>.
//
unit trial_dizzy_timers;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils

    , trial_abstract
    , constants
    , schedules_main
    , response_key
    , custom_timer
    ;

type

  TDataSupport = record
    Cycle : cardinal;
    Latency : cardinal;
    Responses : integer;
    StmBegin : cardinal;
    Timer2 : cardinal;
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
    FFirstResp : Boolean;
    FResponseEnabled : Boolean;
    FSchedule : TSchMan;
    FStimuli : array of TRect;
    function GetCycles: integer;
  protected
    procedure Consequence(Sender: TObject);
    procedure EndTrial(Sender: TObject);
    procedure Hit(Sender: TObject);
    procedure Miss(Sender: TObject);
    procedure None(Sender: TObject);
    procedure Response(Sender: TObject);
    procedure TrialResult(Sender: TObject);
    procedure UpdateTimer1(Sender: TObject);
    procedure UpdateTimer2(Sender: TObject);
    // TTrial
    procedure StartTrial(Sender: TObject); override;
    procedure ThreadClock(Sender: TObject); override;
    procedure WriteData(Sender: TObject); override;
    // TCustomControl
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Play(TestMode: Boolean; Correction : Boolean); override;
    property Cycles : integer read GetCycles;

  end;

implementation

{$ifdef DEBUG}
uses
  debug_logger
, dialogs;
{$endif}

constructor TDZT.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Header :=  'StmBegin' + #9 +
             '_Latency' + #9 +
             '___Cycle' + #9 +
             '__Timer2' + #9 +
             '_Version' + #9 +
             '____Mode' + #9 +
             'RespFreq'
             ;
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

procedure TDZT.Consequence(Sender: TObject);
var aConsequence : TKey;
begin
  {$ifdef DEBUG}
    DebugLn(mt_Debug + 'C:' + RootMedia + FConsequence);
  {$endif}
  aConsequence := TKey.Create(Self);
  aConsequence.Parent:= Self;
  aConsequence.HowManyLoops := 0;
  aConsequence.FullPath := RootMedia + FConsequence;
  aConsequence.Play;

  CreateClientThread('C:' + FormatFloat('00000000;;00000000', GetTickCount - TimeStart));
  if Assigned(CounterManager.OnConsequence) then CounterManager.OnConsequence(Self);
end;

procedure TDZT.TrialResult(Sender: TObject);
begin
  Result := T_NONE;
  IETConsequence := T_NONE;

  if not (FLimitedHold > 0) then EndTrial(Sender);
end;

procedure TDZT.UpdateTimer1(Sender: TObject);
begin
  FDataSupport.Cycle := GetTickCount;
  Inc(FCycles);

  with FDizzyTimer do
    begin
      Main := Random(Max - Min) + Min + 1;
      Host := Random(((2 * Main) div 3) - (Main div 3)) + (Main div 3) + 1;

      if TClockThread(Sender) = Timer1 then
        begin
          Timer1.Interval := Main * 1000;
          Timer2.Interval := Host * 1000;
          Timer2.Enabled := True;
          Color1 := not Color1;
        end;

      if TClockThread(Sender) = Timer2 then
        begin
          Timer2.Interval := Main * 1000;
          Timer1.Interval := Host * 1000;
          Timer1.Enabled := True;
          Color2 := not Color2;
        end;
    end;

  if FDizzyTimer.Mode = 'A' then // do nothing
  else
    if FDizzyTimer.Mode = 'B' then
      if FSchedule.Kind = T_EXT then
        FSchedule.Kind := FDizzyTimer.Schedule
      else
        FSchedule.Kind := T_EXT;

  Invalidate;

  // must pass Self here, see TBLC.WriteTrialData
  WriteData(Self);
end;

procedure TDZT.UpdateTimer2(Sender: TObject);
begin
  TClockThread(Sender).Enabled := False;
  FDataSupport.Timer2 := GetTickCount;

  if TClockThread(Sender) = FDizzyTimer.Timer2 then
    FDizzyTimer.Color2 := not FDizzyTimer.Color2;

  if TClockThread(Sender) = FDizzyTimer.Timer1 then
    FDizzyTimer.Color1 := not FDizzyTimer.Color1;

  Invalidate;
end;

procedure TDZT.ThreadClock(Sender: TObject);
var TickCount : cardinal;
begin
  TickCount := GetTickCount;
  FDataSupport.Cycle := TickCount;
  FDataSupport.Timer2 := TimeStart;
  TrialResult(Sender);
  WriteData(Self);
  EndTrial(Sender);
end;

procedure TDZT.KeyDown(var Key: Word; Shift: TShiftState);
begin
  //inherited KeyDown (Key, Shift);

  if (Key = 27 {ESC}) and (FResponseEnabled = True) then
    begin
      FResponseEnabled:= False;
      Invalidate;
    end;

  if ssCtrl in Shift then
     begin
       if Key = 81 {q} then
         begin
           Data := Data + #13#10 + '(Sessão cancelada)' + #9#9#9#9#9#9#9#9#9 + #13#10;
           Result := 'NONE';
           IETConsequence := 'NONE';
           NextTrial := 'END';
           None(Self);
           EndTrial(Self);
         end;
     end;
end;

procedure TDZT.KeyUp(var Key: Word; Shift: TShiftState);
var TickCount : cardinal;
begin
  inherited KeyUp(Key, Shift);
  TickCount := GetTickCount;

  if FResponseEnabled then
    begin
      if Key = 32 {space} then
        begin
          FSchedule.DoResponse;
          if FFirstResp then
            begin
              CreateClientThread('*R:' + FormatFloat('00000000;;00000000', TickCount - TimeStart));
              FFirstResp := False;
              FDataSupport.Latency := TickCount;
            end
          else CreateClientThread('R:' + FormatFloat('00000000;;00000000', TickCount - TimeStart));
        end;
    end;

  if Key = 27 {ESC} then
    begin
      FResponseEnabled := True;
      Invalidate;
    end;
end;

procedure TDZT.Paint;
const
  clRed : integer = $FF0000;
  clGreen : integer = $00FF00;
  clBlue : integer = $0000FF;
  clYellow : integer = $FFFF00;
begin
  inherited Paint;
  if FResponseEnabled then
    begin

      if FDizzyTimer.Color1 then
        Canvas.Pen.Color := clBlue
      else Canvas.Pen.Color := clRed;

      Canvas.Brush.Color := Canvas.Pen.Color;
      Canvas.Rectangle(FStimuli[0]);

      if FDizzyTimer.Color2 then
        Canvas.Pen.Color := clGreen
      else Canvas.Pen.Color := clYellow;

      Canvas.Brush.Color := Canvas.Pen.Color;
      Canvas.Ellipse(FStimuli[1]);

    end;
end;

procedure TDZT.Play(TestMode: Boolean; Correction : Boolean);
var
  s1 : string;
  R : TRect;

  NumComp, a1 : Integer;

  procedure NextSpaceDelimitedParameter;
  begin
    Delete(s1, 1, pos(#32, s1));
    if Length(s1) > 0 then while s1[1] = #32 do Delete(s1, 1, 1);
  end;

begin
  FResponseEnabled := False;
  Randomize;

  // self descends from TCustomControl
  Color := StrToIntDef(CfgTrial.SList.Values[_BkGnd], 0);

  if TestMode then Cursor:= 0
  else Cursor := StrToIntDef(CfgTrial.SList.Values[_Cursor], 0);

  // self descends from TTrial
  Result := '';
  NextTrial := CfgTrial.SList.Values[_NextTrial];
  RootMedia := CfgTrial.SList.Values[_RootMedia];
  FLimitedHold := StrToIntDef(CfgTrial.SList.Values[_LimitedHold], 0);

  // self
  FDizzyTimer.Schedule := CfgTrial.SList.Values[_Schedule];
  FSchedule := TSchMan.Create(self);
  with FSchedule do
    begin
      OnConsequence := @Consequence;
      OnResponse:= @Response;
      Kind := FDizzyTimer.Schedule;
      if Loaded then
        begin
          SetLength(FClockList, Length(FClockList) + 1);
          FClockList[Length(FClockList) -1] := StartMethod;
        end
      else raise Exception.Create(ExceptionNoScheduleFound);
    end;

  s1 := CfgTrial.SList.Values[_Trial + _cRes] + #32;

  with FDizzyTimer do
    begin
      Color1 := True;
      Color2 := True;

      Min := StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
      NextSpaceDelimitedParameter;

      Max := StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
      NextSpaceDelimitedParameter;

      Version := Copy(s1, 0, pos(#32, s1)-1);
      NextSpaceDelimitedParameter;

      Mode := Copy(s1, 0, pos(#32, s1)-1);
      Mode := UpperCase(Mode);

      Main := Random(Max - Min) + Min + 1;
      Host := Random(((2 * Main) div 3) - (Main div 3)) + (Main div 3) + 1;

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

      SetLength(FClockList, Length(FClockList) + 1);
      FClockList[Length(FClockList) -1] := @Timer1.Start;

      SetLength(FClockList, Length(FClockList) + 1);
      FClockList[Length(FClockList) -1] := @Timer2.Start;

      {$ifdef DEBUG}
        DebugLn(mt_Debug + 'FDizzyTimer:' + IntToStr(Main) +','+ IntToStr(Host) );
      {$endif}
    end;

  FConsequence := CfgTrial.SList.Values[_Trial + _cCsq];

  if FConsequence = T_HIT then FConsequence := 'CSQ1.wav';
  if FConsequence = T_MISS then FConsequence := 'CSQ2.wav';

  NumComp := StrToIntDef(CfgTrial.SList.Values[_NumComp], 0);
  SetLength(FStimuli, NumComp);

  for a1 := 0 to NumComp -1 do
    begin
        s1:= CfgTrial.SList.Values[_Comp + IntToStr(a1+1) + _cBnd] + #32;

        R.Top:= StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
        NextSpaceDelimitedParameter;

        R.Left:= StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
        NextSpaceDelimitedParameter;

        R.Bottom := R.Top + StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 100);
        NextSpaceDelimitedParameter;

        R.Right := R.Left + StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 100);

        {$ifdef DEBUG}
          DebugLn(mt_Debug + 'R' + IntToStr(a1+1) + ':' +
            IntToStr(R.Top) +','+ IntToStr(R.Left)+','+ IntToStr(R.Bottom)+','+ IntToStr(R.Right));
        {$endif}

        FStimuli[a1] := R;
    end;
  StartTrial(Self);
end;

procedure TDZT.StartTrial(Sender: TObject);
var
  TickCount : cardinal;

  procedure KeyStart(var aKey : TKey);
  begin
    aKey.Play;
    aKey.Visible:= True;
  end;

begin
  FFirstResp := True;
  TickCount := GetTickCount;

  with FDataSupport do
    begin
      Cycle := TimeStart;
      Latency := TimeStart;
      Timer2 := TimeStart;
    end;

  FResponseEnabled := True;
  Invalidate;
  FDataSupport.StmBegin := TickCount;
  CreateClientThread('S:' + FormatFloat('00000000;;00000000', TickCount - TimeStart));
  inherited StartTrial(Sender);
end;


procedure TDZT.WriteData(Sender: TObject);  //
var
    Latency, Timer2, Version, Mode : string;
begin
  if not FFirstResp then
    Latency := FormatFloat('00000000;;00000000',FDataSupport.Latency - TimeStart)
  else Latency := #32#32#32#32#32#32 + 'NA';

  if FDataSupport.Timer2 <> TimeStart then
    Timer2 :=  FormatFloat('00000000;;00000000',FDataSupport.Timer2 - TimeStart)
  else Timer2 := #32#32#32#32#32#32 + 'NA';

  Version := FDizzyTimer.Version;

  Mode := FDizzyTimer.Mode;

  {

  Header :=  'StmBegin' + #9 +
             '_Latency' + #9 +
             '___Cycle' + #9 +
             '__Timer2' + #9 +
             '_Version' + #9 +
             '____Mode' + #9 +
             'RespFreq'
             ;

  }
  Data :=  FormatFloat('00000000;;00000000', FDataSupport.StmBegin - TimeStart) + #9 +
           Latency + #9 +
           FormatFloat('00000000;;00000000', FDataSupport.Cycle - TimeStart) + #9 +
           Timer2 + #9 +
           Version + #9 +
           Mode + #9 +
           Format('%-*.*d', [4, 8, FDataSupport.Responses])
           + Data;


  if Assigned(OnWriteTrialData) then OnWriteTrialData(Sender);

  Data := '';
  FFirstResp := True;
end;

procedure TDZT.EndTrial(Sender: TObject);
begin
  FResponseEnabled := False;
  Hide;
  CreateClientThread('E:' + FormatFloat('00000000;;00000000', GetTickCount - TimeStart));
  if Result = T_HIT then Hit(Sender);
  if Result = T_MISS then  Miss(Sender);
  if Result = T_NONE then  None(Sender);

  if Assigned(OnEndTrial) then OnEndTrial(Sender);
end;

procedure TDZT.Hit(Sender: TObject);
begin
  if Assigned(OnHit) then OnHit(Sender);
end;

procedure TDZT.Miss(Sender: TObject);
begin
  if Assigned(OnMiss) then OnMiss (Sender);
end;

procedure TDZT.None(Sender: TObject);
begin
  if Assigned(OnNone) then OnNone (Sender);
end;

procedure TDZT.Response(Sender: TObject);
begin
  Inc(FDataSupport.Responses);

  if Assigned(CounterManager.OnStmResponse) then CounterManager.OnStmResponse(Sender);
  if Assigned(OnStmResponse) then OnStmResponse (Self);
end;

end.

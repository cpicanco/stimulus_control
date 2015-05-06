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
unit main_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls

, LCLIntf

, custom_timer
, schedules_main
, cumulative_record
;

type

  { TFormSchedules }

  TFormSchedules = class(TForm)
    lblClock: TLabel;
    lblLatency: TLabel;
    lblScheduleEnd: TLabel;
    LabelSchedules: TLabel;
    lblResponseDelta: TLabel;
    ListBoxSchedules: TListBox;
    PanelClock: TPanel;
    PanelDelta: TPanel;
    PanelLatency: TPanel;
    PanelEnd: TPanel;
    PanelCumulativeRecord: TPanel;
    PanelOperandum: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure ListBoxSchedulesClick(Sender: TObject);
    procedure PanelOperandumMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FFirstResponse : boolean;

    FTimeDeltaR,
    FTimeOldResponse,
    FTimeScheduleBegin,
    FTimeLatency,
    FTimeConsequence : cardinal;

    FClock : TClockThread;
    FSchedule : TSchMan;
    FCumulativeRecord : TCummulativeRecord;
    procedure ClockOnTimer(Sender : TObject);
    procedure Response(Sender : TObject);
    procedure Consequence(Sender : TObject);
    procedure ResetTimer;

    { private declarations }
  public
    { public declarations }
  end;

var
  FormSchedules: TFormSchedules;

implementation

{$R *.lfm}

{ TFormSchedules }

procedure TFormSchedules.ListBoxSchedulesClick(Sender: TObject);
begin
  FSchedule.Kind := ListBoxSchedules.Items.Strings[ListBoxSchedules.ItemIndex];
  FSchedule.Start;
  ResetTimer;

  FCumulativeRecord.Reset;
end;

procedure TFormSchedules.PanelOperandumMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var TickCount : cardinal;
begin
  TickCount := GetTickCount;
  if FFirstResponse then
    begin
      FFirstResponse := False;
      PanelLatency.Caption := IntToStr(TickCount - FTimeScheduleBegin);
      FTimeLatency := TickCount;
    end
  else
    begin
      FTimeDeltaR := TickCount - FTimeOldResponse;
      PanelDelta.Caption := IntToStr(FTimeDeltaR);
    end;

  FSchedule.DoResponse;
  FTimeOldResponse := TickCount;
end;

procedure TFormSchedules.ClockOnTimer(Sender: TObject);
begin
  PanelClock.Caption := IntToStr(GetTickCount - FTimeScheduleBegin);
end;

procedure TFormSchedules.FormCreate(Sender: TObject);
begin
  Randomize;

  FSchedule := TSchMan.Create(PanelOperandum);
  FSchedule.OnResponse := @Response;
  FSchedule.OnConsequence := @Consequence;
  FSchedule.Kind := 'EXT';
  ResetTimer;

  FCumulativeRecord := TCummulativeRecord.Create(PanelCumulativeRecord);

  FClock := TClockThread.Create(True);
  FClock.OnTimer := @ClockOnTimer;
  FClock.Interval := 1000;
  FClock.Start;
end;

procedure TFormSchedules.Response(Sender: TObject);
begin
  FCumulativeRecord.DecNY(1);
end;

procedure TFormSchedules.Consequence(Sender: TObject);
begin
  FCumulativeRecord.DrawEvent(True);
  FTimeConsequence := GetTickCount;
  PanelEnd.Caption := IntToStr(FTimeConsequence - FTimeScheduleBegin);
end;

procedure TFormSchedules.ResetTimer;
begin
  FTimeScheduleBegin := GetTickCount;
  FFirstResponse := True;
  FTimeConsequence := FTimeScheduleBegin;
  FTimeLatency := FTimeScheduleBegin;
  FTimeOldResponse := 0;
  FTimeDeltaR := 0;

  PanelDelta.Caption := IntToStr(FTimeDeltaR);
  PanelLatency.Caption := IntToStr(FTimeLatency - FTimeScheduleBegin);
  PanelEnd.Caption := IntToStr(FTimeConsequence - FTimeScheduleBegin);
end;

end.


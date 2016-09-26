{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit main_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls

, LCLIntf, LCLType, TAGraph, TASeries, TATypes, Menus

, custom_timer
, schedules_main
, cumulative_record
;

type

  { TFormSchedules }

  TFormSchedules = class(TForm)
    chkSaveBeforeReset: TCheckBox;
    lblClock: TLabel;
    lblLatency: TLabel;
    lblScheduleEnd: TLabel;
    LabelSchedules: TLabel;
    lblResponseDelta: TLabel;
    ListBoxSchedules: TListBox;
    MainMenu1: TMainMenu;
    miAlgorithmTest: TMenuItem;
    miAbout: TMenuItem;
    PanelClock: TPanel;
    PanelDelta: TPanel;
    PanelLatency: TPanel;
    PanelEnd: TPanel;
    PanelCumulativeRecord: TPanel;
    PanelOperandum: TPanel;
    procedure chkSaveBeforeResetChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBoxSchedulesClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miAlgorithmTestClick(Sender: TObject);
    procedure PanelOperandumMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FFirstResponse : boolean;

    FTimeDeltaR,
    FTimeOldResponse,
    FTimeScheduleBegin,
    FTimeLatency,
    FTimeConsequence : cardinal;
    FThreadMethod : TThreadMethod;
    FClock : TClockThread;
    FSchedule : TSchMan;
    FCumulativeRecord : TCummulativeRecord;
    procedure ClockOnTimer(Sender : TObject);
    procedure Response(Sender : TObject);
    procedure Consequence(Sender : TObject);
    procedure ResetTimer;
    procedure BeforeReset(Sender : TObject);

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
  with FSchedule do
    begin
      Kind := ListBoxSchedules.Items.Strings[ListBoxSchedules.ItemIndex];
      if Loaded then
      FThreadMethod := StartMethod;
      TThreadMethod(FThreadMethod);
    end;
  ResetTimer;
  FCumulativeRecord.Reset;
end;

procedure TFormSchedules.miAboutClick(Sender: TObject);
var
  FormAbout : TForm;
  LabelCredits : TMemo;
begin
  FormAbout := TForm.Create(nil);

  LabelCredits := TMemo.Create(FormAbout);
  with LabelCredits do
    begin
      Align := alClient;
      AutoSize := True;
      Text :=
        'Stimulus Control' + LineEnding +
        'Copyright (C) 2014-2015,  Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.' + LineEnding +
        LineEnding +
        'This software is part of Validation Project (PCRF).' + LineEnding +
        'You can see on GitHub:' + LineEnding +
        '<http://github.com/cpicanco/validation_project>' + LineEnding +
        LineEnding +
        'Stimulus Control is free software: you can redistribute it and/or modify' + LineEnding +
        'it under the terms of the GNU General Public License as published by' + LineEnding +
        'the Free Software Foundation, either version 3 of the License, or' + LineEnding +
        '(at your option) any later version.' + LineEnding +
        LineEnding +
        'Stimulus Control is distributed in the hope that it will be useful,' + LineEnding +
        'but WITHOUT ANY WARRANTY; without even the implied warranty of' + LineEnding +
        'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the' + LineEnding +
        'GNU General Public License for more details.' + LineEnding +
        LineEnding +
        'You should have received a copy of the GNU General Public License' + LineEnding +
        'along with Validation Project (PCRF).  If not, see <http://www.gnu.org/licenses/>.';
      Parent := FormAbout;
    end;

  with FormAbout do
    try
      Width := 600;
      Height := 320;
      Position := poMainFormCenter;

      if ShowModal = mrClose then
        begin
         // do nothing
        end;
    finally
      Free;
    end;
end;

procedure TFormSchedules.miAlgorithmTestClick(Sender: TObject);
var
  //hMin, hMax, hInterval,
  i, x, y : integer;
  FormChart : TForm;
  aPlot : TChart;
  aSerie : TLineSeries;
  //Histogram : array of integer;
  MeanResponse, Variation : integer;
begin
  // histogram

  //hInterval := 5;
  //
  //SetLength(Histogram, hInterval);
  //for i := Low(Histogram) to High(Histogram) do Histogram[i] := 0;
  //
  //for x := 0 to 1000 do
  //  begin
  //    y := MeanResponse - Variation + Random((2 * Variation) + 1);
  //    if y = 0 then y := MeanResponse;
  //    for i := Low(Histogram) to High(Histogram) do
  //      begin
  //        hMax := hInterval * (i + 1);
  //        hMin := hMax - hInterval;
  //        if (y >= hMin) and (y <= hMax) then Inc(Histogram[i]);
  //      end
  //  end;
  //
  //for x := Low(Histogram) to High(Histogram) do aSerie.AddXY(x, Histogram[i]);

  FormChart := TForm.Create(nil);
  aPlot := TChart.Create(FormChart);
  aSerie := TLineSeries.Create(aPlot);

  with aSerie do
    begin
      LineType := ltNone;
      ShowPoints := True;
      Pointer.HorizSize := 3;
      Pointer.VertSize := 3;
      Pointer.Style := psCircle;
    end;

  MeanResponse := 100;
  Variation := 100;

  for x := 0 to 500 do
    begin
      y := MeanResponse - Variation + Random((2 * Variation) + 1);
      if y = 0 then y := MeanResponse;
      aSerie.AddXY(x, y);
    end;

  with aPlot do
    begin
      Align := alClient;
      AutoSize := True;
      AxisList.Axes[0].Range.UseMin := True;
      AxisList.Axes[0].Grid.Visible := False;
      AxisList.Axes[0].Title.Visible := True;
      AxisList.Axes[0].Title.Caption := 'Responses';

      AxisList.Axes[1].Range.UseMin := True;
      AxisList.Axes[1].Grid.Visible := False;
      AxisList.Axes[1].Title.Visible := True;
      AxisList.Axes[1].Title.Caption := 'Random Samples';

      AddSeries(aSerie);
      Parent := FormChart;
    end;

  with FormChart do
    try
      Caption := 'Returned Values over Random Sampling';
      Width := 600;
      Height := 320;
      Position := poMainFormCenter;

      if ShowModal = mrClose then
        begin
         // do nothing
        end;
    finally
      Free;
    end;
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
  FCumulativeRecord.OnBeforeReset := @BeforeReset;

  FClock := TClockThread.Create(True);
  FClock.OnTimer := @ClockOnTimer;
  FClock.Start;
end;

procedure TFormSchedules.chkSaveBeforeResetChange(Sender: TObject);
begin
  FCumulativeRecord.ResetEventEnabled := chkSaveBeforeReset.Checked;
end;

procedure TFormSchedules.FormDestroy(Sender: TObject);
begin
  FClock.FreeOnTerminate := False;
  FClock.Enabled := False;
  FClock.Terminate;
  FClock.Free;
end;

procedure TFormSchedules.Response(Sender: TObject);
begin
  FCumulativeRecord.DecNY(1);
end;

procedure TFormSchedules.Consequence(Sender: TObject);
begin
  FTimeConsequence := GetTickCount;
  FCumulativeRecord.DrawEvent(True);
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

procedure TFormSchedules.BeforeReset(Sender: TObject);
var
  Bitmap : TBitmap;
  aFilename : string;
  i : integer;
  DestRect, OrigRect : TRect;
begin
  i := 1;
  aFilename := ExtractFilePath(Application.ExeName);
  aFilename := aFilename + 'cummulative_record_' + Format('%.3d', [i]) + '.bmp';
  while FileExistsUTF8(aFilename) do
   begin
     Inc(i);
     aFilename := 'cummulative_record_' + Format('%.3d', [i]) + '.bmp';
   end;

  DestRect := Rect(0,0,PanelCumulativeRecord.Width, PanelCumulativeRecord.Height);
  OrigRect := Rect(PanelCumulativeRecord.Left,
                   PanelCumulativeRecord.Top,
                   PanelCumulativeRecord.Left + PanelCumulativeRecord.Width,
                   PanelCumulativeRecord.Top + PanelCumulativeRecord.Height);

  Bitmap := TBitmap.Create;
  with Bitmap do
    try
      Width := 1;
      Height := 1;

      // Load from form window
      LoadFromDevice(Self.Canvas.Handle);

      // Copy only the Panel image
      Canvas.CopyRect(DestRect, Canvas, OrigRect);
      Width := PanelCumulativeRecord.Width;
      Height := PanelCumulativeRecord.Height;

      // Save
      SaveToFile(aFileName);
    finally
      Free;
    end;
end;

end.


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

, schedules_main
, cumulative_record
;

type

  { TFormSchedules }

  TFormSchedules = class(TForm)
    LabelSchedules: TLabel;
    ListBoxSchedules: TListBox;
    PanelCumulativeRecord: TPanel;
    PanelOperandum: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure ListBoxSchedulesClick(Sender: TObject);
    procedure PanelOperandumClick(Sender: TObject);
    procedure PanelOperandumMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    //FFirstResponse : boolean;
    FSchedule : TSchMan;
    FCumulativeRecord : TCummulativeRecord;
    procedure Response(Sender : TObject);
    procedure Consequence(Sender : TObject);

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
end;

procedure TFormSchedules.PanelOperandumClick(Sender: TObject);
begin

end;

procedure TFormSchedules.PanelOperandumMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSchedule.DoResponse;
end;

procedure TFormSchedules.FormCreate(Sender: TObject);
begin
  Randomize;

  FSchedule := TSchMan.Create(PanelOperandum);
  FSchedule.Kind := 'EXT';
  FSchedule.OnResponse := @Response;
  FSchedule.OnConsequence := @Consequence;

  FCumulativeRecord := TCummulativeRecord.Create(PanelCumulativeRecord);
end;

procedure TFormSchedules.Response(Sender: TObject);
begin
  FCumulativeRecord.DecNY(1);
end;

procedure TFormSchedules.Consequence(Sender: TObject);
begin
  FCumulativeRecord.DrawEvent(True);
end;

end.


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
unit userconfigs_get_matrix;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,

  criatore;

type

  { TMatrixConfigForm }

  TMatrixConfigForm = class(TForm)
    btnCancel: TButton;
    grpStimuliSpacement: TGroupBox;
    grpStimuliWidthHeight: TGroupBox;
    grpGeneralConfigs: TGroupBox;
    grpMatrixRowCol: TGroupBox;
    btnClearAll         : TButton;
    btnOK                 : TButton;
    btnShowMatrix      : TButton;
    edtDisty: TEdit;
    edtDistx: TEdit;
    edtRow: TEdit;
    edtCol: TEdit;
    edtWidth: TEdit;
    edtHeight: TEdit;
    lbl3                    : TLabel;
    lbl2                : TLabel;
    lbl1                  : TLabel;
    cbSSquare              : TCheckBox;
    cbSquare              : TCheckBox;
    chkCustomResolution: TCheckBox;
    lbCoordenates         : TListBox;
    chkPlotEquallySpaced              : TCheckBox;
    chkPlotProportional         : TCheckBox;
    btnRandomizeMatrix: TButton;
    edtScreenLeft: TEdit;
    edtScreenTop: TEdit;
    lblLeftTop: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure edtRowChange(Sender: TObject);
    procedure edtWidthChange(Sender: TObject);
    procedure edtDistxChange(Sender: TObject);
    procedure cbSquareClick(Sender: TObject);
    procedure cbSSquareClick(Sender: TObject);
    procedure chkPlotEquallySpacedClick(Sender: TObject);
    procedure chkPlotProportionalClick(Sender: TObject);
    procedure btnShowMatrixClick(Sender: TObject);
    procedure btnClearAllClick(Sender: TObject);
    procedure btnRandomizeMatrixClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbCoordenadasEndDock(Sender, Target: TObject; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtScreenLeftChange(Sender: TObject);
  private
    FBackground: TForm;
    FCanDraw    : Boolean;
    FDrawCoordenates : TAleatorizator;
    //FEscriba : TEscriba;
    //FOnPosCfg: TNotifyEvent;
  public
    property aBackGround : TForm read FBackground write FBackground;

  end;

var
  MatrixConfigForm : TMatrixConfigForm;

resourcestring
  CMatrizDefault = '1';
  CSizeDefault = '100';
  CCustomResDefLeft = '1024';
  CCustomResDefTop = '768';

implementation

uses background, userconfigs_simple_discrimination_matrix;

{$R *.lfm}

procedure TMatrixConfigForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  {if ssCtrl in Shift then
    if (Key = 69) or (Key = 101) then
      frmTextEditor.FormStyle := fsStayOnTop;}
end;

procedure TMatrixConfigForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  {if ssCtrl in Shift then
    if (Key = 69) or (Key = 101) then
      frmTextEditor.FormStyle := fsMDIChild;}
end;

procedure TMatrixConfigForm.lbCoordenadasEndDock(Sender, Target: TObject; X, Y: Integer);
begin
  lbCoordenates.Top := 8;
  lbCoordenates.Left := 195;
  lbCoordenates.Width := 150;
  lbCoordenates.Height := 127;
end;

procedure TMatrixConfigForm.edtRowChange(Sender: TObject);
begin
  if (TEdit(Sender).Text = '') or
     (StrToIntDef(TEdit(Sender).Text, 1) < 1) or
     (StrToIntDef(TEdit(Sender).Text, 1) > 127) then
    begin
      TEdit(Sender).Text := CMatrizDefault;
      TEdit(Sender).SelectAll;
    end;
  if (TEdit(Sender).ComponentIndex = edtRow.ComponentIndex) then
    if cbSquare.Checked then edtCol.Text := edtRow.Text;
end;

procedure TMatrixConfigForm.btnCancelClick(Sender: TObject);
var i : integer;
begin
  for i := 0 to Application.ComponentCount -1 do
    if Application.Components[i].ClassName = 'TMatrixForm' then
      begin
        TForm(Application.Components[i]).Visible := True;
        Break;
      end;
  aBackground.Free;
  Free;
end;

procedure TMatrixConfigForm.btnOKClick(Sender: TObject);
var i : integer;
begin
  //showmessage(IntToStr(lbCoordenates.Items.Count));
  for i := 0 to Application.ComponentCount -1 do
    if Application.Components[i].ClassName = 'TMatrixForm' then
      begin
        TForm(Application.Components[i]).Visible := True;
        Break;
      end;
  if lbCoordenates.Items.Text <> '' then
    TMatrixForm(Application.Components[i]).SetMatrix(lbCoordenates.Items);
  aBackground.Free;
  Free;
end;

procedure TMatrixConfigForm.edtDistxChange(Sender: TObject);
begin
  if TEdit(Sender).Text = '' then
    begin
      TEdit(Sender).Text := '0';
      TEdit(Sender).SelectAll;
    end;

  if (TEdit(Sender).ComponentIndex = edtDistx.ComponentIndex) then
    if chkPlotEquallySpaced.Checked then edtDisty.Text := edtDistx.Text;
end;

procedure TMatrixConfigForm.edtScreenLeftChange(Sender: TObject);
begin
    if (TEdit(Sender).Text = '') or (TEdit(Sender).Text = '0') then
    begin
      if (TEdit(Sender).ComponentIndex = edtScreenLeft.ComponentIndex) then
        TEdit(Sender).Text := CCustomResDefLeft else
      if (TEdit(Sender).ComponentIndex = edtScreenTop.ComponentIndex) then
      TEdit(Sender).Text := CCustomResDefTop;

      TEdit(Sender).SelectAll;
    end;
end;

procedure TMatrixConfigForm.edtWidthChange(Sender: TObject);
begin
  if (TEdit(Sender).Text = '') or (TEdit(Sender).Text = '0') then
    begin
      TEdit(Sender).Text := CSizeDefault;
      TEdit(Sender).SelectAll;
    end;

  if (TEdit(Sender).ComponentIndex = edtWidth.ComponentIndex) then
    if cbSSquare.Checked then edtHeight.Text := edtWidth.Text;
end;

procedure TMatrixConfigForm.cbSquareClick(Sender: TObject);
begin
    if cbSquare.Checked then
      begin
        edtCol.Text := edtRow.Text;
        edtCol.Enabled := False;
      end
    else
      begin
        edtCol.Enabled := True;
      end;
end;
procedure TMatrixConfigForm.cbSSquareClick(Sender: TObject);
begin
    if cbSSquare.Checked then
      begin
        edtHeight.Text := edtWidth.Text;
        edtHeight.Enabled:= False;
      end
    else
      begin
        edtHeight.Enabled:= True;
      end;
end;
procedure TMatrixConfigForm.chkPlotEquallySpacedClick(Sender: TObject);
begin
    if chkPlotEquallySpaced.Checked then
      begin
        edtDisty.Text := edtDistx.Text;
        edtDisty.Enabled:= False;
      end
    else
      begin
        edtDisty.Enabled:= True;
      end;
end;

procedure TMatrixConfigForm.chkPlotProportionalClick(Sender: TObject);
begin
    if chkPlotProportional.Checked then
      begin
        edtDistx.Enabled := False;
        edtDisty.Enabled := False;
        chkPlotEquallySpaced.Enabled := False;
      end
    else
      begin
        edtDistx.Enabled := True;
        chkPlotEquallySpaced.Enabled := True;
        if not chkPlotEquallySpaced.Checked then edtDisty.Enabled := True;
      end;
end;

procedure TMatrixConfigForm.btnRandomizeMatrixClick(Sender: TObject);
begin
 if FCanDraw = False then
  begin
    FDrawCoordenates.RandomizePositions (True);
  end;
end;

procedure TMatrixConfigForm.FormActivate(Sender: TObject);
begin
  FDrawCoordenates := TAleatorizator.Create(aBackground, lbCoordenates.Items);
end;

procedure TMatrixConfigForm.FormCreate(Sender: TObject);
begin
  FCanDraw := True;
  edtScreenLeft.Text := IntToStr(Screen.Width);
  edtScreenTop.Text := IntToStr(Screen.Height);
end;

procedure TMatrixConfigForm.btnClearAllClick(Sender: TObject);
begin
  if FCanDraw = False then
    begin
      lbCoordenates.Items.Clear;
      FDrawCoordenates.ClearAll;
      FCanDraw := True;
    end;
end;

procedure TMatrixConfigForm.btnShowMatrixClick(Sender: TObject);
begin
 if FCanDraw then
  begin
    FDrawCoordenates.Distribuido := chkPlotProportional.Checked;
    if chkCustomResolution.Checked then
      begin
        FDrawCoordenates.SetVariables (edtDistx.Text, edtDisty.Text,
                                       edtWidth.Text, edtHeight.Text,
                                       edtRow.Text, edtCol.Text,
                                       edtScreenLeft.Text, edtScreenTop.Text);
      end
    else
      begin
        FDrawCoordenates.SetVariables (edtDistx.Text, edtDisty.Text,
                                        edtWidth.Text, edtHeight.Text,
                                        edtRow.Text, edtCol.Text,
                                        '-1', '-1');
      end;
    FDrawCoordenates.DrawStmFromCoordenates;
    FCanDraw := False;
  end;
end;
end.


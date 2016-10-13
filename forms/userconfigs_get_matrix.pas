{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit userconfigs_get_matrix;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics,
     Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Menus

     , criatore
     , constants
     ;

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
    miSaveAsTXTwithKeys: TMenuItem;
    miSaveAsTXT: TMenuItem;
    puMenu: TPopupMenu;
    SaveDialog1: TSaveDialog;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure edtDistxEditingDone(Sender: TObject);
    procedure edtRowChange(Sender: TObject);
    procedure edtRowEditingDone(Sender: TObject);
    procedure edtScreenLeftEditingDone(Sender: TObject);
    procedure edtScreenTopEditingDone(Sender: TObject);
    procedure edtWidthChange(Sender: TObject);
    procedure edtDistxChange(Sender: TObject);
    procedure cbSquareClick(Sender: TObject);
    procedure cbSSquareClick(Sender: TObject);
    procedure chkPlotEquallySpacedClick(Sender: TObject);
    procedure chkPlotProportionalClick(Sender: TObject);
    procedure btnShowMatrixClick(Sender: TObject);
    procedure btnClearAllClick(Sender: TObject);
    procedure btnRandomizeMatrixClick(Sender: TObject);
    procedure edtWidthEditingDone(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbCoordenadasEndDock(Sender, Target: TObject; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  published
    procedure SaveAsTXT(Sender: TObject);
  private
    FBackground: TForm;
    FCanDraw    : Boolean;
    FDrawCoordenates : TAleatorizator;
    procedure SetBackground(AValue: TForm);

    //FEscriba : TEscriba;
    //FOnPosCfg: TNotifyEvent;
  public
    property BackGround : TForm read FBackground write SetBackground;

  end;

var
  FrmMatrixConfig : TMatrixConfigForm;

resourcestring
  CMatrizDefault = '1';
  CSizeDefault = '100';
  CCustomResDefLeft = '1024';
  CCustomResDefTop = '768';

implementation

uses userconfigs_simple_discrimination_matrix;

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
 if (TEdit(Sender).ComponentIndex = edtRow.ComponentIndex) then
   if cbSquare.Checked then edtCol.Text := edtRow.Text;
end;

procedure TMatrixConfigForm.edtRowEditingDone(Sender: TObject);
begin
 if (TEdit(Sender).Text = '') or
    (StrToIntDef(TEdit(Sender).Text, 1) < 1) or
    (StrToIntDef(TEdit(Sender).Text, 1) > 127) then
   begin
     TEdit(Sender).Text := CMatrizDefault;
     TEdit(Sender).SelectAll;
   end;
end;

procedure TMatrixConfigForm.edtScreenLeftEditingDone(Sender: TObject);
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

procedure TMatrixConfigForm.edtScreenTopEditingDone(Sender: TObject);
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

procedure TMatrixConfigForm.btnCancelClick(Sender: TObject);
var i : integer;
begin
  for i := 0 to Application.ComponentCount -1 do
    if Application.Components[i].ClassName = 'TMatrixForm' then
      begin
        TForm(Application.Components[i]).Visible := True;
        Break;
      end;
  Background.Hide;
  {$IFDEF WINDOWS}
  Close;
  {$ELSE}
  Free;
  {$ENDIF}
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
  FDrawCoordenates.ClearAll;
  Background.Hide;
  {$IFDEF WINDOWS}
  Close;
  {$ELSE}
  Free;
  {$ENDIF}
end;

procedure TMatrixConfigForm.edtDistxEditingDone(Sender: TObject);
begin
  if TEdit(Sender).Text = '' then
    begin
      TEdit(Sender).Text := '0';
      TEdit(Sender).SelectAll;
    end;
end;

procedure TMatrixConfigForm.edtDistxChange(Sender: TObject);
begin
  if (TEdit(Sender).ComponentIndex = edtDistx.ComponentIndex) then
    if chkPlotEquallySpaced.Checked then edtDisty.Text := edtDistx.Text;
end;

procedure TMatrixConfigForm.SaveAsTXT(Sender: TObject);
var
  L : TStringList;
  i : integer;

begin
  if lbCoordenates.Items.Count > 0 then
    if SaveDialog1.Execute then
      begin
        if Sender = miSaveAsTXT then
          lbcoordenates.Items.SaveToFile(SaveDialog1.FileName);

        if Sender = miSaveAsTXTwithKeys then
          begin
            L := TStringList.Create;
            try
              for i := 0 to lbCoordenates.Items.Count - 1 do
                L.Append( _Comp + IntToStr(i + 1) + KBnd + lbCoordenates.Items.Strings[i] );
              L.SaveToFile(SaveDialog1.FileName);
            finally
              L.Free;
            end;
          end;
      end;
end;

procedure TMatrixConfigForm.SetBackground(AValue: TForm);
begin
  if FBackground=AValue then Exit;
  FBackground:=AValue;
  FDrawCoordenates := TAleatorizator.Create(FBackground, lbCoordenates.Items);
end;

procedure TMatrixConfigForm.edtWidthChange(Sender: TObject);
begin
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

procedure TMatrixConfigForm.edtWidthEditingDone(Sender: TObject);
begin
 if (TEdit(Sender).Text = '') or (TEdit(Sender).Text = '0') then
   begin
     TEdit(Sender).Text := CSizeDefault;
     TEdit(Sender).SelectAll;
   end;
end;

procedure TMatrixConfigForm.FormActivate(Sender: TObject);
begin

end;

procedure TMatrixConfigForm.FormCreate(Sender: TObject);
begin
  FCanDraw := True;
  edtScreenLeft.Text := IntToStr(Screen.Width);
  edtScreenTop.Text := IntToStr(Screen.Height);
  SaveDialog1.InitialDir:= GetCurrentDir;
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



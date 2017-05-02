{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit userconfigs_get_matrix;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics,
     Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Menus, XMLPropStorage

     , criatore
     , constants
     ;

type

  { TFormMatrixConfig }

  TFormMatrixConfig = class(TForm)
    btnCancel: TButton;
    btnClearAll: TButton;
    btnOK: TButton;
    btnRandomizeMatrix: TButton;
    btnShowMatrix: TButton;
    cbSquare: TCheckBox;
    cbSSquare: TCheckBox;
    chkCustomResolution: TCheckBox;
    chkPlotEquallySpaced: TCheckBox;
    chkPlotProportional: TCheckBox;
    edtCol: TEdit;
    edtDistx: TEdit;
    edtDisty: TEdit;
    edtHeight: TEdit;
    edtRow: TEdit;
    edtScreenLeft: TEdit;
    edtScreenTop: TEdit;
    edtWidth: TEdit;
    grpGeneralConfigs: TGroupBox;
    grpMatrixRowCol: TGroupBox;
    grpStimuliSpacement: TGroupBox;
    grpStimuliWidthHeight: TGroupBox;
    lbCoordenates: TListBox;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lblLeftTop: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miSaveAsTXT: TMenuItem;
    miSaveAsTXTwithKeys: TMenuItem;
    PanelMain: TPanel;
    puMenu: TPopupMenu;
    SaveDialog1: TSaveDialog;
    XMLPropStorage1: TXMLPropStorage;
    procedure btnOKClick(Sender: TObject);
    procedure edtDistxEditingDone(Sender: TObject);
    procedure edtRowChange(Sender: TObject);
    procedure edtRowEditingDone(Sender: TObject);
    procedure edtScreenEditingDone(Sender: TObject);
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbCoordenadasEndDock(Sender, Target: TObject; X, Y: Integer);
  published
    procedure SaveAsTXT(Sender: TObject);
  private
    FCanDraw    : Boolean;
    FDrawCoordenates : TAleatorizator;
    FPositions: string;
    function GetPositions: TStrings;
  public
    property Positions : string read FPositions;
  end;

var
  FormMatrixConfig : TFormMatrixConfig;

resourcestring
  CMatrizDefault = '1';
  CSizeDefault = '100';
  CCustomResDefLeft = '1024';
  CCustomResDefTop = '768';

implementation

{$R *.lfm}

procedure TFormMatrixConfig.lbCoordenadasEndDock(Sender, Target: TObject; X, Y: Integer);
begin
  lbCoordenates.Top := 8;
  lbCoordenates.Left := 195;
  lbCoordenates.Width := 150;
  lbCoordenates.Height := 127;
end;

procedure TFormMatrixConfig.edtRowChange(Sender: TObject);
begin
 if (TEdit(Sender).ComponentIndex = edtRow.ComponentIndex) then
   if cbSquare.Checked then edtCol.Text := edtRow.Text;
end;

procedure TFormMatrixConfig.edtRowEditingDone(Sender: TObject);
begin
 if (TEdit(Sender).Text = '') or
    (StrToIntDef(TEdit(Sender).Text, 1) < 1) or
    (StrToIntDef(TEdit(Sender).Text, 1) > 127) then
   begin
     TEdit(Sender).Text := CMatrizDefault;
     TEdit(Sender).SelectAll;
   end;
end;

procedure TFormMatrixConfig.edtScreenEditingDone(Sender: TObject);
begin
 if (TEdit(Sender).Text = '') or (TEdit(Sender).Text = '0') then
 begin
   if TEdit(Sender) = edtScreenLeft then
     TEdit(Sender).Text := CCustomResDefLeft;

   if TEdit(Sender) = edtScreenTop then
     TEdit(Sender).Text := CCustomResDefTop;

   TEdit(Sender).SelectAll;
 end;
end;

procedure TFormMatrixConfig.edtDistxEditingDone(Sender: TObject);
begin
  if TEdit(Sender).Text = '' then
    begin
      TEdit(Sender).Text := '0';
      TEdit(Sender).SelectAll;
    end;
end;

procedure TFormMatrixConfig.btnOKClick(Sender: TObject);
begin
  FPositions := lbCoordenates.Items.Text;
end;

procedure TFormMatrixConfig.edtDistxChange(Sender: TObject);
begin
  if (TEdit(Sender).ComponentIndex = edtDistx.ComponentIndex) then
    if chkPlotEquallySpaced.Checked then edtDisty.Text := edtDistx.Text;
end;

procedure TFormMatrixConfig.SaveAsTXT(Sender: TObject);
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

function TFormMatrixConfig.GetPositions: TStrings;
begin
  Result := FDrawCoordenates.Items;
end;

procedure TFormMatrixConfig.edtWidthChange(Sender: TObject);
begin
  if (TEdit(Sender).ComponentIndex = edtWidth.ComponentIndex) then
    if cbSSquare.Checked then edtHeight.Text := edtWidth.Text;
end;

procedure TFormMatrixConfig.cbSquareClick(Sender: TObject);
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
procedure TFormMatrixConfig.cbSSquareClick(Sender: TObject);
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
procedure TFormMatrixConfig.chkPlotEquallySpacedClick(Sender: TObject);
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

procedure TFormMatrixConfig.chkPlotProportionalClick(Sender: TObject);
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

procedure TFormMatrixConfig.btnRandomizeMatrixClick(Sender: TObject);
begin
 if FCanDraw = False then
   FDrawCoordenates.RandomizePositions(True);
end;

procedure TFormMatrixConfig.edtWidthEditingDone(Sender: TObject);
begin
 if (TEdit(Sender).Text = '') or (TEdit(Sender).Text = '0') then
   begin
     TEdit(Sender).Text := CSizeDefault;
     TEdit(Sender).SelectAll;
   end;
end;

procedure TFormMatrixConfig.FormCreate(Sender: TObject);
begin
  FDrawCoordenates := TAleatorizator.Create(Self);
  FCanDraw := True;
  edtScreenLeft.Text := IntToStr(Screen.Width);
  edtScreenTop.Text := IntToStr(Screen.Height);
  SaveDialog1.InitialDir:= GetCurrentDir;
end;

procedure TFormMatrixConfig.FormDestroy(Sender: TObject);
begin
  if Assigned(FDrawCoordenates) then
    FDrawCoordenates.Free;
end;

procedure TFormMatrixConfig.FormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight  then
    PanelMain.Visible:=True;

  if Button = mbLeft then
    PanelMain.Visible:=False;
end;

procedure TFormMatrixConfig.btnClearAllClick(Sender: TObject);
begin
  if FCanDraw = False then
    begin
      lbCoordenates.Items.Clear;
      FDrawCoordenates.ClearAll;
      FCanDraw := True;
    end;
end;

procedure TFormMatrixConfig.btnShowMatrixClick(Sender: TObject);
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
    FDrawCoordenates.OutputItems := lbCoordenates.Items;
    FDrawCoordenates.DrawStmFromCoordenates;
    FCanDraw := False;
    PanelMain.BringToFront;
  end;
end;
end.



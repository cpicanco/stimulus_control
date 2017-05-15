{
  Stimulus Control
  Copyright (C) 2010-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit userconfigs_stimuli_sets;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Types,
  Graphics, Controls, Forms, Dialogs, Spin,
  StdCtrls, ExtCtrls, ExtDlgs, Buttons,
  ComCtrls,  Grids

  , response_key
  , constants
  , escriba;

type

  TrialType = (Condicional, Simples, GoNoGo, Span);

  TGridKey = record
    Index : Integer;
    Files : TStringList;
    Key : TKey;
    Sample : Boolean;
    Square : TRect;
  end;

  TTrialSet = record
    BoundColor : TColor;
    theGrid : array of TGridKey;
  end;

  TSessionTrialSets = array of TTrialSet;

type

  TCStringGrid = class(TStringGrid)
  private
    FHideFocusRect: Boolean;
  protected
    procedure Paint;override;
  public
    constructor Create(AOwner: TComponent); override;
    property HideFocusRect:Boolean read FHideFocusRect write FHideFocusRect;
  end;

  TFormStimuliSets = class(TForm)
    dlgOpenPic: TOpenPictureDialog;
    lblStmName: TLabel;
    pgc: TPageControl;
    tbGen: TTabSheet;
    tbShow: TTabSheet;
    rgTrialType: TRadioGroup;
    grpNumComp: TGroupBox;
    seNumComp: TSpinEdit;
    btnMountTrials: TButton;
    btnForward: TBitBtn;
    btnBackward: TBitBtn;
    btn1: TButton;
    pnlTotal: TPanel;
    grpResumo: TGroupBox;
    lst1: TListBox;
    lblBlc: TLabel;
    edtBlc: TEdit;
    btnApply: TButton;
    procedure rgTrialTypeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure seNumCompChange(Sender: TObject);
    procedure btnClick(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btnMountTrialsClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btn2Click(Sender: TObject);
  private
    strngrd1 : TCStringGrid;
    FTrialSet: TTrialSet;
    FEscriba: TEscriba;
    FSeTrial: TSpinEdit;
    FSeBlc: TSpinEdit;
    FOnTrialChange: TNotifyEvent;
    FOnStmChange: TNotifyEvent;
    FOnBlcChange: TNotifyEvent;
    procedure KeyMouseEnter(Sender:TObject);
    procedure KeyMouseLeave(Sender:TObject);
    procedure KeyMouseDown(Sender : TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure strngrd1KeyPress(Sender: TObject; var Key: Char);
    procedure strngrd1SetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure strngrd1SelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure strngrd1DrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure btnMountEngine;
    procedure SendToEscriba;
    procedure EscribaCall;
  public
    procedure SetControls;
    property Escriba : TEscriba read FEscriba write FEscriba;
    property SeBlc : TSpinEdit read FSeBlc write FSeBlc;
    property SeTrial : TSpinEdit read FSeTrial write FSeTrial;
    property OnBlcChange : TNotifyEvent read FOnBlcChange write FOnBlcChange;
    property OnTrialChange : TNotifyEvent read FOnTrialChange write FOnTrialChange;
    property OnStmChange : TNotifyEvent read FOnStmChange write FOnStmChange;
    { Public declarations }
  end;

var
  FormStimuliSets: TFormStimuliSets;

implementation

{$R *.lfm}

procedure TFormStimuliSets.btn1Click(Sender: TObject);
begin
  if lst1.Items.Count > 0 then
  begin
    lst1.Items.Clear;
    pnlTotal.Caption := 'Total';
  end;
end;

procedure TFormStimuliSets.btn2Click(Sender: TObject);
begin
  edtBlc.Text := IntToStr(SeBlc.Value);
  ShowMessage(Escriba.Blcs[SeBlc.Value -1].Name);
end;

procedure TFormStimuliSets.btnApplyClick(Sender: TObject);
begin
  SendToEscriba;
end;

procedure TFormStimuliSets.btnClick(Sender: TObject);
var aIndex, aTag : Integer;
begin
  //if btnBackward = TBitBtn(Sender) then
  //begin
  //  if TKey(btnBackward.Parent) is TKey then
  //  begin
  //    aTag := TKey(btnBackward.Parent).Tag;
  //    if FTrialSet.theGrid[aTag].Index >= 0 then
  //    begin
  //      aIndex := FTrialSet.theGrid[aTag].Index -1;
  //      FTrialSet.theGrid[aTag].Index := aIndex;
  //      if aIndex >= 0 then
  //        begin
  //          lblStmName.Caption := ExtractFileName(FTrialSet.theGrid[aTag].Files.Strings[aIndex]);
  //          FTrialSet.theGrid[aTag].Key.FullPath:= FTrialSet.theGrid[aTag].Files.Strings[aIndex];
  //        end
  //      else
  //        begin
  //          lblStmName.Caption := '';
  //          FTrialSet.theGrid[aTag].Key.FullPath:= '';
  //          FTrialSet.theGrid[aTag].Key.Refresh;
  //        end;
  //    end;
  //  end;
  //end;
  //if btnForward = TBitBtn(Sender) then
  //begin
  //  if TKey(btnForward.Parent) is TKey then
  //  begin
  //    aTag := TKey(btnForward.Parent).Tag;
  //    if FTrialSet.theGrid[aTag].Index < (FTrialSet.theGrid[aTag].Files.Count -1) then
  //    begin
  //      aIndex := FTrialSet.theGrid[aTag].Index +1;
  //      FTrialSet.theGrid[aTag].Index := aIndex;
  //      lblStmName.Caption := ExtractFileName(FTrialSet.theGrid[aTag].Files.Strings[aIndex]);
  //      FTrialSet.theGrid[aTag].Key.FullPath:= FTrialSet.theGrid[aTag].Files.Strings[aIndex];
  //    end
  //    else
  //      begin
  //        lblStmName.Caption := '';
  //        FTrialSet.theGrid[aTag].Key.FullPath:= '';
  //        FTrialSet.theGrid[aTag].Key.Refresh;
  //        FTrialSet.theGrid[aTag].Index := -1;
  //      end;
  //  end;
  //end;

end;

procedure TFormStimuliSets.btnMountEngine;
var ID, c: integer; s1 : string;

  procedure AddStmFromGridS(Grid, Stm : Integer);   //editar
  begin
    if Stm <= FTrialSet.theGrid[Grid].Files.Count -1 then
      with strngrd1 do Cells[Grid + 1, RowCount - 1] := ExtractFileName(FTrialSet.theGrid[Grid].Files.Strings[Stm])
    else ShowMessage(IntToStr(Stm));
  end;

  procedure AddStmFromGrid(Grid, Stm : Integer); //resumo
  begin
    if Stm <= FTrialSet.theGrid[Grid].Files.Count -1 then
        s1 := s1 + ExtractFileName(FTrialSet.theGrid[Grid].Files.Strings[Stm]) + ','
    else ShowMessage(IntToStr(Stm));
    AddStmFromGridS(Grid, Stm);
  end;

  procedure AddTrialFromIndex; //each trial is the current index's
  var I: Integer;
  begin
    with strngrd1 do
      begin
        Cells[0, RowCount] := 'T' + IntToStr(RowCount);  //Give a name to the new String Edit line on Edit Tab
        RowCount := RowCount + 1;
      end;
    for I:= Low(FTrialSet.theGrid) to High(FTrialSet.theGrid) do
        AddStmFromGrid(I,FTrialSet.theGrid[I].Index);
    lst1.Items.Add(s1);
    s1:= '';
  end;

  procedure IndexReset(aGrid : Integer);
  var i : Integer;
  begin
    for  I := aGrid to (High(FTrialSet.theGrid)) do
      FTrialSet.theGrid[I].Index := 0
  end;

  procedure IndexInc(aGrid : Integer);
  begin
    AddTrialFromIndex; //Save
    Inc(FTrialSet.theGrid[aGrid].Index);
  end;

      function MoveGrid(var ID:integer): Boolean;   //begin from right most, the highest grid
      var IDMaxIndex, IDCurIndex, LowID, HighID: Integer;
      begin
        Result := True;
        LowID := Low(FTrialSet.theGrid);
        HighID := High(FTrialSet.theGrid);
        //Application.ProcessMessages;
        if  (ID < LowID) then
          begin
            //ShowMessage('False');
            AddTrialFromIndex;
            Result := False;
          end
        else
          begin
            IDMaxIndex:= FTrialSet.theGrid[ID].Files.Count -1;
            IDCurIndex := FTrialSet.theGrid[ID].Index;

            if IDCurIndex = IDMaxIndex then
              begin
                ID := ID - 1;
                Result:= MoveGrid(ID);//moveleft
                Exit;
              end;
            if   (ID < HighID)
             and (IDCurIndex < IDMaxIndex) then
              begin
                IndexInc(ID);                 //increment/move down
                IndexReset(ID + 1);           //reset everything on the right
                MoveGrid(HighID);             //move to the most right/next down
                Exit;
              end;
            if  (ID = (HighID))
             and (IDCurIndex < IDMaxIndex) then
              begin
                IndexInc(ID);          //increment/move down
                MoveGrid(ID)           //next increment/move down
              end;
          end;
      end;
begin
  ID := High(FTrialSet.theGrid);

  with strngrd1 do
    begin
      ColCount := ID + 2;
      for c := 1 to ColCount - 1 do Cells[c, 0] := 'C' + IntToStr(c);
    end;
  //strngrd1.RowCount := 1;
  IndexReset(Low(FTrialSet.theGrid));  //0's for everyone
  MoveGrid(ID); //begin from the most right

  with strngrd1 do
    if RowCount > 1 then
      begin
        FixedCols := 1;
        FixedRows := 1;
        Repaint;
      end;
  pnlTotal.Caption := IntToStr(lst1.Items.Count);
end;

procedure TFormStimuliSets.btnMountTrialsClick(Sender: TObject);
begin
  btnMountEngine;
end;

procedure TFormStimuliSets.EscribaCall;
begin

end;

procedure TFormStimuliSets.FormCreate(Sender: TObject);
begin
  strngrd1 := TCStringGrid.Create(tbShow);
  with strngrd1 do
    begin
      OnDrawCell := strngrd1DrawCell;
      OnSetEditText := strngrd1SetEditText;
      OnSelectCell := strngrd1SelectCell;
      OnKeyPress := strngrd1KeyPress;
      Parent := tbShow;
      Cells[0, 0] := 'Tent\Stm';
      Width := Parent.Width - Left;
      Height := Parent.Height - Top;
    end;

  FTrialSet.BoundColor := clInactiveCaption;
end;


procedure TFormStimuliSets.KeyMouseDown(Sender : TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  procedure GetFileNameFromPictureDialog (I : Integer);
  var K : Integer;
  begin
    if dlgOpenPic.Execute then
      begin
        if dlgOpenPic.Files.Count > 0 then
        for K := 0 to (dlgOpenPic.Files.Count - 1) do
          begin
            FTrialSet.theGrid[I].Files.Add(dlgOpenPic.Files.Strings[K]);
            FTrialSet.theGrid[I].Key.Caption:= IntToStr(FTrialSet.theGrid[I].Files.Count);
            FTrialSet.theGrid[I].Key.Refresh;
          end;
        dlgOpenPic.Files.Clear;
      end;
  end;
begin
  if ssDouble in Shift then GetFileNameFromPictureDialog (TKey(Sender).Tag);
end;

procedure TFormStimuliSets.KeyMouseEnter(Sender: TObject);
var btnBackwardTop,btnBackwardLeft, btnForwardTop, btnForwardLeft : Integer; aSquare : TRect;
begin
  TKey(Sender).Edge := clRed;
  TKey(Sender).Refresh;

  aSquare := Rect(
               TPoint(TKey(Sender).ClientToScreen(Point(0, 0))).X,
               TPoint(TKey(Sender).ClientToScreen(Point(0, 0))).Y,
               TPoint(TKey(Sender).ClientToScreen(Point(TKey(Sender).Width, TKey(Sender).Height))).X,
               TPoint(TKey(Sender).ClientToScreen(Point(TKey(Sender).Width, TKey(Sender).Height))).Y
               );


  btnBackwardLeft := TKey(Sender).ClientRect.Left + (TKey(Sender).Width div 2) - btnBackward.Width - 6;
  btnBackwardTop  := TKey(Sender).ClientRect.Bottom - btnBackward.Height - 4;

  //btnBackward.Parent := TKey(Sender);
  //btnBackward.Left := btnBackwardLeft;
  //btnBackward.Top := btnBackwardTop;
  //btnBackward.Visible := True;
  //btnBackward.BringToFront;

  lblStmName.Left := TKey(Sender).Left;
  lblStmName.Top  := TKey(Sender).Top - (lblStmName.Height + 12);
  lblStmName.Caption := ExtractFileName(TKey(Sender).FullPath);
  lblStmName.Visible := True;

  btnForwardLeft := TKey(Sender).ClientRect.Left + (TKey(Sender).Width div 2) + 6;
  btnForwardTop := TKey(Sender).ClientRect.Bottom - btnForward.Height - 4;

  //btnForward.Parent := TKey(Sender);
  //btnForward.Left := btnForwardLeft;
  //btnForward.Top := btnForwardTop;
  //btnForward.Visible := True;
  //btnForward.BringToFront;
end;

procedure TFormStimuliSets.KeyMouseLeave(Sender: TObject);
var aSquare : TRect;
begin

   aSquare := Rect(
                   TPoint(TKey(Sender).ClientToScreen(Point(0, 0))).X,
                   TPoint(TKey(Sender).ClientToScreen(Point(0, 0))).Y,
                   TPoint(TKey(Sender).ClientToScreen(Point(TKey(Sender).Width, TKey(Sender).Height))).X,
                   TPoint(TKey(Sender).ClientToScreen(Point(TKey(Sender).Width, TKey(Sender).Height))).Y
                   );

   if not PtInRect(aSquare, Mouse.CursorPos) then
   begin
     //lblStmName.Left := TKey(Sender).ClientRect.CenterPoint.X - (lblStmName.Width div 2);
     //lblStmName.Top  := TKey(Sender).ClientRect.Top - (lblStmName.Height * 2);
     lblStmName.Visible := False;
     //lblStmName.Caption := '';

     btnBackward.Visible := False;
     btnForward.Visible := False;

     btnBackward.Parent := tbGen;
     btnForward.Parent := tbGen;

     TKey(Sender).Edge := clInactiveCaption;
     TKey(Sender).Refresh;
   end;
end;

procedure TFormStimuliSets.rgTrialTypeClick(Sender: TObject);
var
  Origin: TPoint;
  I, K, oX, oY, sR, NumComp : Integer;
begin

  oX := rgTrialType.Left + 15;
  oY := rgTrialType.Top + rgTrialType.Height + 45;
  sR := 130;
  Origin := Point(oX, oY);

  NumComp := seNumComp.Value;
  case rgTrialType.ItemIndex of
    Ord(Condicional):
      begin
        if not (seNumComp.MaxValue = 0) then
          seNumComp.MaxValue := 0;

        if ((Length(FTrialSet.theGrid) - 1) < NumComp) then
          begin
            case NumComp of
              1..MaxInt:
                begin
                  SetLength(FTrialSet.theGrid, NumComp + 1);
                  for I := Low(FTrialSet.theGrid) to High(FTrialSet.theGrid) do
                    begin
                      FTrialSet.theGrid[I].Index:= -1;
                      FTrialSet.theGrid[I].Files := TStringList.Create;
                      if I = 0 then
                        FTrialSet.theGrid[I].Sample := True
                      else
                        FTrialSet.theGrid[I].Sample := False;

                      FTrialSet.theGrid[I].Square := Rect(Origin.X,Origin.Y, sR, sR);

                      if not Assigned(FTrialSet.theGrid[I].Key) then
                        FTrialSet.theGrid[I].Key := TKey.Create(tbGen);
                      with  FTrialSet.theGrid[I].Key do
                        begin
                          Cursor := crHandPoint;
                          Tag := I;
                          EditMode := True;
                          OnMouseEnter:= KeyMouseEnter;
                          OnMouseLeave:= KeyMouseLeave;
                          OnMouseDown := KeyMouseDown;
                          if I = 0 then
                            Color := clBlue
                          else
                            Color := clGreen;
                          SetBounds(FTrialSet.theGrid[I].Square.Left,
                                    FTrialSet.theGrid[I].Square.Top, sR, sR);
                          //FTrialSet.theGrid[I].Key.FullPath := 'D:\Programas\Projects\EAM 4.2\Files Settings\Estímulos\A1.bmp';
                          FTrialSet.theGrid[I].Key.Parent:= tbGen;
                          FTrialSet.theGrid[I].Key.Show;
                        end;
                      oX := oX + sR + 15;
                      Origin := Point(oX, oY);
                    end;
                end;
            end;
          end
        else
          begin
            case NumComp of
              0:
                begin
                  for I := tbGen.ComponentCount - 1 downto 0 do
                  if (tbGen.Components[i] is TKey)
                  then
                    begin
                      tbGen.Components[i].Free;
                    end;
                  for I := High(FTrialSet.theGrid) downto Low(FTrialSet.theGrid) do
                    FTrialSet.theGrid[I].Files.Free;
                  SetLength(FTrialSet.theGrid, NumComp);
                end;
              1..MaxInt:
                begin
                  for I := tbGen.ComponentCount - 1 downto 0 do
                  if (tbGen.Components[i] is TKey)
                   and ((TKey(tbGen.Components[i]).Tag) <> 0)
                   and ((TKey(tbGen.Components[i]).Tag) > (NumComp))
                  then
                    begin
                      tbGen.Components[i].Free;
                    end;
                  for I := High(FTrialSet.theGrid) downto NumComp + 1 do
                    FTrialSet.theGrid[I].Files.Free;
                  SetLength(FTrialSet.theGrid, NumComp);
                end;
            end;
          end;
    end;

    Ord(Simples):
      begin
        if ((Length(FTrialSet.theGrid) - 1) < NumComp) then
          begin
            case NumComp of
              0:
                begin

                end;

              1..MaxInt:
                begin
                  SetLength(FTrialSet.theGrid, NumComp);
                  for I := Low(FTrialSet.theGrid) to High(FTrialSet.theGrid) do
                    begin
                      FTrialSet.theGrid[I].Index:= -1;
                      FTrialSet.theGrid[I].Files := TStringList.Create;
                      FTrialSet.theGrid[I].Sample := False;

                      FTrialSet.theGrid[I].Square := Rect(Origin.X,Origin.Y, sR, sR);

                      if not Assigned(FTrialSet.theGrid[I].Key) then
                        FTrialSet.theGrid[I].Key := TKey.Create(Self);

                      with  FTrialSet.theGrid[I].Key do
                        begin
                          Cursor := crHandPoint;
                          Tag := I;
                          EditMode := True;
                          Color:= clGreen;
                          OnMouseEnter:= KeyMouseEnter;
                          OnMouseLeave:= KeyMouseLeave;
                          OnMouseDown := KeyMouseDown;
                          SetBounds(FTrialSet.theGrid[I].Square.Left,
                                    FTrialSet.theGrid[I].Square.Top, sR, sR);
                          FTrialSet.theGrid[I].Key.Parent:= tbGen;
                          FTrialSet.theGrid[I].Key.Show;
                        end;
                      oX := oX + sR + 15;
                      Origin := Point(oX, oY);
                    end;
                end;
            end;
          end
        else
          begin
            for I := ComponentCount - 1 downto 0 do
              if (Components[i] is TKey) then
                begin
                  //ShowMessage('oi');
                  if ((TKey(tbGen.Components[i]).Tag) >= (NumComp)) then
                    begin
                      tbGen.Components[i].Free;
                    end;
                  for K := High(FTrialSet.theGrid) downto NumComp do
                    FTrialSet.theGrid[K].Files.Free;
                  SetLength(FTrialSet.theGrid, NumComp);
                end;
          end;
      end;

    Ord(GoNoGo):
      begin
        seNumComp.Value := 1;
        seNumComp.MaxValue := 1;
        SetLength(FTrialSet.theGrid, 2);
        for I := Low(FTrialSet.theGrid) to High(FTrialSet.theGrid) do
          begin
            if I = 0 then
              FTrialSet.theGrid[I].Sample := True
            else
              FTrialSet.theGrid[I].Sample := False;

            FTrialSet.theGrid[I].Square := Rect(Origin.X,Origin.Y, sR, sR);
            //FTrialSet.theGrid[I].CompIndex := -1;
            oX := oX + sR + 15;
            Origin := Point(oX, oY);
          end;
      end;

    Ord(Span):
      begin

      end;
  end;
  Refresh;
end;

procedure TFormStimuliSets.SendToEscriba;
var i, j, blc : Integer;
  procedure SaveComparisonStm(b, i, j: Integer);
  begin
    FEscriba.Blcs[b].Trials[i].SList.Values[_Comp + IntToStr(j) + _cStm] :=
      strngrd1.Cells[j, i + 1] + ' 0 255';
  end;

  procedure SaveStandardSStmParameters(b, i: integer);
  begin
    Escriba.Blcs[b].Trials[i].SList.Add(_Delayed + '=' + '-1');
    Escriba.Blcs[b].Trials[i].SList.Add(_Delay + '=' + '0');
    Escriba.Blcs[b].Trials[i].SList.Add(_Samp + _cStm + '=' + 'Escolher');
    Escriba.Blcs[b].Trials[i].SList.Add(_Samp + _cBnd + '=' + '1');
    Escriba.Blcs[b].Trials[i].SList.Add(_Samp + _cSch + '=' + T_CRF);
    Escriba.Blcs[b].Trials[i].SList.Add(_Samp + _cMsg + '=' + 'AUTO');
    Application.ProcessMessages;
  end;

  procedure SaveStandardStmParameters(b, i, j: integer);
  begin
    Escriba.Blcs[b].Trials[i].SList.Add (_Comp + IntToStr(j) + _cBnd  + '=' + IntToStr(j));
    Escriba.Blcs[b].Trials[i].SList.Add (_Comp + IntToStr(j) + _cStm  + '=' + 'Escolher 0 255');
    Escriba.Blcs[b].Trials[i].SList.Add (_Comp + IntToStr(j) + _cIET  + '=' + 'Escolher 0 255');
    Escriba.Blcs[b].Trials[i].SList.Add (_Comp + IntToStr(j) + _cCsq  + '=' + '0');
    Escriba.Blcs[b].Trials[i].SList.Add (_Comp + IntToStr(j) + _cUsb  + '=' + '0');
    Escriba.Blcs[b].Trials[i].SList.Add (_Comp + IntToStr(j) + _cSch  + '=' + T_CRF);
    if j = 1 then Escriba.Blcs[b].Trials[i].SList.Add (_Comp + IntToStr(j) + _cRes + '=' + T_HIT)
    else Escriba.Blcs[b].Trials[i].SList.Add (_Comp + IntToStr(j) + _cRes  + '=' + T_MISS);
    Escriba.Blcs[b].Trials[i].SList.Add (_Comp + IntToStr(j) + _cMsg  + '=' + 'AUTO');
    Escriba.Blcs[b].Trials[i].SList.Add (_Comp + IntToStr(j) + _cNxt + '=' + '0');
    Application.ProcessMessages;
  end;
begin
  //escriba
  //blc base 0
  //i base 0
  //j base 1

  blc := (SeBlc.Value -1);

  if blc >= 0 then
  with strngrd1 do
    for i := 1 to RowCount -1 do
      for j := 1 to ColCount -1 do
        //ShowMessage(Cells[j, i]);
        SaveComparisonStm(blc,i -1, j);
end;

procedure TFormStimuliSets.seNumCompChange(Sender: TObject);
begin
  if seNumComp.Value < 0 then seNumComp.Value := 0;
  rgTrialTypeClick(Sender);
end;

procedure TFormStimuliSets.SetControls;
begin
  dlgOpenPic.InitialDir := Escriba.Media;
  edtBlc.Text := IntToStr(SeBlc.Value);
end;


procedure TFormStimuliSets.strngrd1DrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var j : Integer;
  procedure PaintBlack;
  var aRect : TRect;
  begin
    aRect := Rect;
    with TStringGrid(Sender) do
    begin
      Canvas.Brush.Color := clBlack;
      Canvas.FillRect(Rect);
      Canvas.Font.Color := clWhite;
      InflateRect(aRect, -2, -2);
      Canvas.TextRect(aRect,aRect.Left,aRect.Top, 'NA');
    end;
    State := [gdFixed];
  end;

  procedure PaintFocus;
  var     aRect : TRect;
  begin
    aRect := Rect;
    with TStringGrid(Sender) do
    begin
      Canvas.Brush.Color := clRed;
      Canvas.FillRect(Rect);
      Canvas.Font.Color := clBlack;
      InflateRect(aRect, -2, -2);
      Canvas.TextRect(aRect,aRect.Left,aRect.Top, Cells[ACol,ARow]);
    end;
  end;

  procedure PaintNormal;
  var     aRect : TRect;
  begin
    aRect := Rect;
    with TStringGrid(Sender) do
    begin
      Canvas.Brush.Color := clWindow;
      Canvas.FillRect(Rect);
      Canvas.Font.Color := clBlack;
      InflateRect(aRect, -2, -2);
      Canvas.TextRect(aRect,aRect.Left,aRect.Top, Cells[ACol,ARow]);
    end;
  end;

  procedure PaintFixed;
  var     aRect : TRect;
  begin
    aRect := Rect;
    with TStringGrid(Sender) do
    begin
      Canvas.Brush.Color := FixedColor;
      Canvas.FillRect(Rect);
      Canvas.Font.Color := clBlack;
      InflateRect(aRect, -2, -2);
      Canvas.TextRect(aRect,aRect.Left,aRect.Top, Cells[ACol,ARow]);
    end;
  end;
begin
  with TStringGrid(Sender) do
    if Cells[ACol,ARow] = '' then PaintBlack
    else
      begin
        if gdFixed in State then PaintFixed
        else PaintNormal;


            for j := 0 to ColCount - 1 do
              begin
                //ShowMessage(Cells[ACol,ARow] + ' ' + Cells[j,ARow]);
                if (ACol = j) then
                  else
                  if (Cells[ACol,ARow] = Cells[j,ARow]) then
                  begin

                     PaintFocus;

                  end;
                    //else PaintNormal;
              end;

      end;
end;

procedure TFormStimuliSets.strngrd1KeyPress(Sender: TObject; var Key: Char);
begin
  //if not CharInSet (Key, ['0'..'9', #8, #9]) then Key := #0;
  //strngrd1.Repaint;
end;

procedure TFormStimuliSets.strngrd1SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  //with TStringGrid(Sender) do
  //  if Cells[ACol,ARow] = '' then CanSelect := False;
  strngrd1.Repaint;
end;

procedure TFormStimuliSets.strngrd1SetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  //with TStringGrid(Sender) do
  //  begin
  //    if (value <> '')
  //      and (FPosArray.IndexOf(value) = -1) then Cells [ACol, ARow] := '1';
  //    Repaint;
  //  end;
  strngrd1.Repaint;
end;

{ TCStringGrid }

constructor TCStringGrid.Create(AOwner: TComponent);
begin
  inherited;
    Anchors:= [akTop, akRight, akLeft, akBottom];
    //AutoSize:= True;
    BorderStyle := bsNone;

    DefaultDrawing := False;
    FixedCols := 1;
    FixedRows := 1;
    //Height := 131;
    HideFocusRect:= True;
    Left := 8;
    Options:= [goEditing, goFixedHorzLine,goHorzLine, goFixedVertLine,goVertLine,
               goAlwaysShowEditor];
    ColCount := 1;
    RowCount := 1;
    ScrollBars := ssBoth;
    TabOrder := 2;
    Top:= 70;
    //Width:= 646;
end;

procedure TCStringGrid.Paint;
var
 L_Rect:Trect;
begin
  inherited;
    if HideFocusRect then
      begin
        L_Rect := CellRect(Col,Row);
        InflateRect(L_Rect,-1,-1);
        DrawFocusrect(Col,Row,L_Rect)
      end;
end;

end.

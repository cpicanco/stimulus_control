{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit userconfigs_positions;

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Spin, ExtCtrls, Menus, StdCtrls, Grids
  , config_session_fileutils
  , config_session
  ;

type

  TCStringGrid = class(TStringGrid)
  private
    FHideFocusRect: Boolean;
  protected
    //procedure DrawCell(ACol, ARow: Longint; ARect: TRect;AState: TGridDrawState); override;
    procedure Paint;override;
  public
    constructor Create(AOwner: TComponent); override;
    property HideFocusRect:Boolean Read FHideFocusRect Write FHideFocusRect;
  end;

  { TFormRandomizePositions }

  TFormRandomizePositions = class(TForm)
    btnApp: TButton;
    btnRand: TButton;
    chkAllDifferent: TCheckBox;
    LabelBeginAt: TLabel;
    LabelEndAt: TLabel;
    LabelGap: TLabel;
    LabelLine: TLabel;
    LabelLines: TLabel;
    LabelNumPos: TLabel;
    LabelSeqToWrite: TLabel;
    LabelTrial: TLabel;
    PanelNumPos: TPanel;
    pBalanced: TMenuItem;
    pmRand: TPopupMenu;
    pRand: TMenuItem;
    pSpan: TMenuItem;
    seBeginAt: TSpinEdit;
    seEndAt: TSpinEdit;
    seGap: TSpinEdit;
    seSeqToWrite: TSpinEdit;
    procedure StringGridKeyPress(Sender: TObject; var Key: Char);
    procedure StringGridSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure StringGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure StringGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure btnRandClick(Sender: TObject);
    procedure seGapChange(Sender: TObject);
    procedure chkAllDifferentClick(Sender: TObject);
    procedure btnAppClick(Sender: TObject);
    procedure seGapKeyPress(Sender: TObject; var Key: Char);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    StringGrid : TCStringGrid;
    FSession : TConfigurationFile;
    FBlackList : TStringList;
    FWhiteList : TStringList;
    FPositions : TStringList;
    FLatinSquare : array of array of integer;
    FBloc : TCfgBlc;
    FOnBlcChange: TNotifyEvent;
    FOnTrialChange: TNotifyEvent;
    procedure CellsEliminator;
    procedure PutOnGrid(Trial : integer; var aSquareLine : integer);
    procedure Rand;
    procedure RandLatinSquareBalanced;
    procedure RandSequence;
    procedure SetSpin;
    function EqualPositionsInRows(ARow1, ARow2 : integer):Boolean;
  public
    procedure LoadPositionsFromFile(AFilename: string; ABlc : Integer);
    property OnBlcChange : TNotifyEvent read FOnBlcChange write FOnBlcChange;
    property OnTrialChange : TNotifyEvent read FOnTrialChange write FOnTrialChange;
  end;

var
  FormRandomizePositions: TFormRandomizePositions;

implementation

uses constants;

{$R *.lfm}

procedure TFormRandomizePositions.btnAppClick(Sender: TObject);
var LRow, LCol : integer;
begin
  with StringGrid do
    for LRow := 1 to RowCount -1 do
      for LCol := 1 to ColCount - 1 do
        begin
          if FSession.ReadTrialString(FBloc.ID,LRow+1,_Kind) = T_MTS then
            if LCol = 1 then
              FSession.WriteToTrial(LRow,FBloc.ID,_Samp+_cBnd, FPositions.Values[Cells[LCol,LRow]]);

          if LCol > 1 then
            if (LCol - 1) <= FSession.ReadTrialInteger(FBloc.ID,LRow+1,_NumComp) then
              FSession.WriteToTrial(LRow,FBloc.ID,_Comp + IntToStr(LCol-1)+_cBnd, FPositions.Values[Cells[LCol,LRow]]);
        end;
  StringGrid.Invalidate;
  FSession.UpdateFile;
end;

procedure TFormRandomizePositions.btnRandClick(Sender: TObject);
var i, seq, SquareLine: integer;
begin
  SquareLine := 0;
  i:= seBeginAt.Value - 1;
  while (i < StringGrid.RowCount -1) and (i <= seEndAt.Value - 1) do
    begin
      if not chkAllDifferent.Checked then Rand;
      for seq := 0 to seSeqToWrite.Value -1 do
        begin
          if chkAllDifferent.Checked then Rand;
          PutOnGrid(i + 1, SquareLine);
          Inc(i);
        end;
      Inc(i, seGap.Value);
    end;
    StringGrid.Repaint;
end;

procedure TFormRandomizePositions.CellsEliminator;
begin
  with StringGrid do
    begin
      Clear;
      ColCount := 2;
      RowCount := 2;
    end;
end;


procedure TFormRandomizePositions.LoadPositionsFromFile(AFilename: string; ABlc: Integer
  );
var
  i, j : integer;
begin
  FSession := TConfigurationFile.Create(AFilename);
  FSession.CacheUpdates := True;
  FBloc := FSession.Bloc[ABlc];
  SetSpin;

  FPositions := TStringList.Create;
  FPositions.Sorted:= False;
  FPositions.Duplicates := dupIgnore;
  FSession.ReadPositionsInBloc(FBloc.ID,FPositions);

  WriteLn(FPositions.Text);
  halt;

  with StringGrid do
    begin
      ColCount := FPositions.Count + 2;   // +1 for for fixed col, +1 for MTS samples
      RowCount := FBloc.NumTrials + 1;
      Cells[0,0] := 'Tent\Stm';

      // write col names
      for j := 1 to ColCount- 1 do
        begin
          if j = 1 then
            Cells[j, 0] := 'Modelo';

          if j > 1 then
            Cells[j, 0] := 'C' + IntToStr(j - 1);
        end;

      // write row names
      for i := 1 to RowCount - 1 do
        Cells[0, i] := 'T' + IntToStr(i);

      // fill cells
      for i := 1 to RowCount - 1 do
        for j := 1 to ColCount - 1 do
          begin
            if j = 1 then
              Cells[j, i] := FSession.ReadTrialString(FBloc.ID,i,_Samp+_cBnd);

            if j > 1 then
              Cells[j, i] := FSession.ReadTrialString(FBloc.ID,i,_Comp+IntToStr(j-1)+_cBnd);
          end;
      Repaint;
    end;
end;


procedure TFormRandomizePositions.chkAllDifferentClick(Sender: TObject);
begin
  if chkAllDifferent.Checked then
    chkAllDifferent.Hint := 'Desmarque se quiser n sequências iguais ao aleatorizar.'
  else
    chkAllDifferent.Hint := 'Marque se quiser n sequências diferentes ao aleatorizar.'
end;


procedure TFormRandomizePositions.FormCreate(Sender: TObject);
begin
  StringGrid := TCStringGrid.Create(Self);
  with StringGrid do
    begin
      OnDrawCell := @StringGridDrawCell;
      OnSetEditText := @StringGridSetEditText;
      OnSelectCell := @StringGridSelectCell;
      OnKeyPress := @StringGridKeyPress;
      Parent := Self;
    end;
end;

procedure TFormRandomizePositions.FormDestroy(Sender: TObject);
begin
  FPositions.Free;
  FBlackList.Free;
  FWhiteList.Free;
  CellsEliminator;
end;

procedure TFormRandomizePositions.PutOnGrid(Trial: integer;
  var aSquareLine: integer);
var r, n, c1, NumComp: integer;
begin
  if Trial <= FBloc.NumTrials then
    begin
      NumComp := FSession.ReadTrialInteger(FBloc.ID,Trial, _NumComp);
      if pRand.Checked then
        begin
          c1 := 0;
          for n := 0 to NumComp - 1 do
            begin
              StringGrid.Cells[n + 2, Trial] := FPositions.Names[c1];
              Inc(c1);
              if n = 0 then
                begin
                  if FPositions.Count > 1 then
                    begin
                      r := Round(Random * (FPositions.Count - 1));
                      StringGrid.Cells[1, Trial] := FPositions.Names[r];
                    end
                  else StringGrid.Cells[1, Trial] := '1';
                end;
            end;
        end;

      if pBalanced.Checked then
        begin
          for n := 0 to NumComp - 1 do
            begin
              StringGrid.Cells[n + 1, Trial] := IntToStr(FLatinSquare[n, aSquareLine]);
              if n = 0 then
                begin
                  if FPositions.Count > 1 then
                    begin
                      r := FLatinSquare[FPositions.Count - 1, aSquareLine];
                      StringGrid.Cells[1, Trial] := IntToStr(r);
                    end
                  else StringGrid.Cells[1, Trial] := '1';
                end;
            end;
          if aSquareLine = (FPositions.Count - 1) then
            aSquareLine := 0
          else
            Inc(aSquareLine);
        end;
    end;
end;

procedure TFormRandomizePositions.Rand;
begin
  Randomize;
  if pRand.Checked then RandSequence;
  if pBalanced.Checked then RandLatinSquareBalanced;
end;

procedure TFormRandomizePositions.RandLatinSquareBalanced;
var   jumbled, sequence, rotateS, signs: array of integer;
      i, j, k, size : integer;
//      s1 : string;
//      a1 : integer;

  procedure SetArrayLength (aSize : integer);
  begin
    SetLength(signs, aSize);
    SetLength(rotateS, aSize);
    SetLength(jumbled, aSize);
    SetLength(sequence, aSize);
    SetLength(FLatinSquare, aSize, aSize);
  end;

  procedure Shuffle(var array1 : array of integer ; aSize : integer);   //embaralhar lista
  var v, aTemp, aRandom : integer;
  begin
    for v := 0 to aSize - 1 do array1[v] := v + 1;

    for v := 0 to aSize - 1 do
      begin
        aRandom := Round(Random * (aSize - 1));
        aTemp := array1[aRandom];
        array1[aRandom] := array1[v];
        array1[v] := aTemp;
      end;
  end;

  procedure Rotate(var array2 : array of integer; aSize, aTimes : integer);   //primeiro elemento torna-se último, elementos restantes para esquerda n vezes
  var aTemp, v, x : integer;
  begin
    for x := 0 to aTimes - 1 do
      begin
        aTemp := array2[0];
        for v := Low(array2) to High(array2) do array2[v] := array2[v + 1];
        array2[aSize - 1] := aTemp;
      end;
  end;

begin
  //how many?: 1 for each NumPos cicle on seSeqToWrite.
  Size := FPositions.Count;
  SetArrayLength(Size);

  shuffle(jumbled, size);                      //gerar lista de referência; aleatória
  shuffle(rotateS, size);                      //gerar lista de rotações; aleatória
  for i := 0 to size - 1 do signs[i] := i + 1; //gerar lista de elementos; ordenada

  for i := 0 to size - 1 do
    begin
      for k := 0 to size - 1 do sequence[k] := jumbled[k]; //gerar lista de trabalho a partir da lista de referência
      rotate(sequence, size, rotateS[i]);                  //mover elementos da lista de trabalho
      for j := 0 to size - 1 do FLatinSquare[j, sequence[j] - 1] := signs[i]; //preencher Latin Square
    end;
end;


procedure TFormRandomizePositions.RandSequence;
var r, n : integer;
begin
  for n := 0 to FPositions.Count - 1 do
    begin
      repeat
        r := Round(Random * (FPositions.Count - 1));
      until n <> r;
      FPositions.Exchange(n,r);
    end;
end;

procedure TFormRandomizePositions.seGapChange(Sender: TObject);
begin
  if seGap.Value = 1 then
    LabelLine.Caption := 'linha.'
  else
    LabelLine.Caption := 'linhas.';
end;

procedure TFormRandomizePositions.seGapKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key, ['0'..'9', #8, #9]) then
    Key := #0;
end;

function TFormRandomizePositions.EqualPositionsInRows(ARow1, ARow2: integer
  ): Boolean;
var
  LCol: Integer;
begin
  Result := True;
  with StringGrid do
    for LCol := 1 to ColCount -1 do
      if Cells[LCol, ARow1] <> Cells[LCol, ARow2] then
        begin
          Result := False;
          Break;
        end;
end;

procedure TFormRandomizePositions.SetSpin;
begin
  seSeqToWrite.MaxValue := FBloc.NumTrials;
  seSeqToWrite.MinValue := 1;
  if seSeqToWrite.MaxValue = seSeqToWrite.MinValue then
    seSeqToWrite.Increment := 0
  else
    seSeqToWrite.Increment := 1;
  seSeqToWrite.Value := 1;

  seGap.MaxValue := FBloc.NumTrials - 1;
  seGap.MinValue := 0;
  if seGap.MaxValue = seGap.MinValue then
    seGap.Increment := 0
  else
    seGap.Increment := 1;
  seGap.Value := 0;

  seBeginAt.MaxValue := FBloc.NumTrials;
  seBeginAt.MinValue := 1;
  if seBeginAt.MaxValue = seBeginAt.MinValue then
    seBeginAt.Increment := 0
  else
    seBeginAt.Increment := 1;
  seBeginAt.Value := 1;

  seEndAt.MaxValue := FBloc.NumTrials;
  seEndAt.MinValue := 1;
  if seEndAt.MaxValue = seEndAt.MinValue then
    seEndAt.Increment := 0
  else
    seEndAt.Increment := 1;
  seEndAt.Value := FBloc.NumTrials;
end;

procedure TFormRandomizePositions.StringGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
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
        DrawText(Canvas.Handle, 'NA', Length('NA'), aRect, DT_CENTER);
      end;
    State := [gdFixed];
  end;

  procedure PaintFocus;
  var
    aRect : TRect;
  begin
    aRect := Rect;
    with TStringGrid(Sender) do
      begin
        Canvas.Brush.Color := clRed;
        Canvas.FillRect(Rect);
        Canvas.Font.Color := clBlack;
        InflateRect(aRect, -2, -2);
        DrawText(Canvas.Handle, PChar(Cells[ACol,ARow]), Length(Cells[ACol,ARow]), aRect, DT_CENTER);
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
        DrawText(Canvas.Handle, PChar(Cells[ACol,ARow]), Length(Cells[ACol,ARow]), aRect, DT_LEFT);
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
        DrawText(Canvas.Handle, PChar(Cells[ACol,ARow]), Length(Cells[ACol,ARow]), aRect, DT_CENTER);
      end;
  end;
begin
  with TStringGrid(Sender) do
    if Cells[ACol,ARow] = '' then
      PaintBlack
    else
      begin
        if gdFixed in State then
          PaintFixed
        else
          PaintNormal;

        for j := 1 to ColCount - 1 do
          if not (ACol = j) then
            if (Cells[ACol,ARow] = Cells[j,ARow]) then
              PaintFocus;
      end;
end;

procedure TFormRandomizePositions.StringGridKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet (Key, ['0'..'9', #8, #9]) then
    Key := #0;
  StringGrid.Invalidate;
end;

procedure TFormRandomizePositions.StringGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  with TStringGrid(Sender) do
    begin
      if Cells[ACol,ARow] = '' then
        CanSelect := False;
      Invalidate;
    end;
end;

procedure TFormRandomizePositions.StringGridSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  with TStringGrid(Sender) do
    begin
      if (value <> '') and (FPositions.IndexOf(value) = -1) then
        Cells[ACol, ARow] := '1';
      Invalidate;
    end;
end;

{ TStringgrid }

constructor TCStringGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Anchors:= [akTop, akRight, akLeft, akBottom];
  BorderStyle := bsNone;
  DefaultDrawing := False;
  FixedCols := 1;
  FixedRows := 1;
  Height := 131;
  HideFocusRect:= True;
  Left := 8;
  Options:= [goEditing, goFixedHorzLine,goHorzLine, goFixedVertLine,goVertLine, goAlwaysShowEditor];
  TabOrder := 2;
  Top:= 70;
  Width:= 791;
end;

procedure TCStringGrid.Paint;
var
 L_Rect:Trect;
begin
  inherited Paint;
  if HideFocusRect then
    begin
      L_Rect := CellRect(Col,Row);
      InflateRect(L_Rect,-1,-1);
      DrawFocusrect(Col,Row,L_Rect);
    end;
end;

end.

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
    btnOK: TButton;
    btnCancel: TButton;
    btnRandomize: TButton;
    LabelBeginAt: TLabel;
    LabelEndAt: TLabel;
    LabelGap: TLabel;
    LabelLine: TLabel;
    LabelLines: TLabel;
    LabelSeqToWrite: TLabel;
    LabelTrial: TLabel;
    pBalanced: TMenuItem;
    pmRand: TPopupMenu;
    pRand: TMenuItem;
    seBeginAt: TSpinEdit;
    seEndAt: TSpinEdit;
    seGap: TSpinEdit;
    seSeqToWrite: TSpinEdit;
    procedure StringGridKeyPress(Sender: TObject; var Key: Char);
    procedure StringGridSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure StringGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure StringGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure btnRandClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    StringGrid : TCStringGrid;
    FSession : TConfigurationFile;
    FBlackList : TStringList;
    FWhiteList : TStringList;
    FPositions : TStringList;
    FPosiNames : TStringList;
    FLatinSquare : array of array of integer;
    FBloc : TCfgBlc;
    FOnBlcChange: TNotifyEvent;
    FOnTrialChange: TNotifyEvent;
    procedure CellsEliminator;
    procedure PutOnGrid(ATrial : integer; ANumComp :Integer; ASquareLine : integer = 0);
    procedure Rand;
    procedure RandLatinSquareBalanced;
    procedure RandSequence;
    procedure SetSpin;
    function EqualPositionsInRows(ARow1, ARow2 : integer; Offset : integer):Boolean;
  public
    procedure LoadPositionsFromFile(AFilename: string; ABlc : Integer; APositions : string = '');
    property OnBlcChange : TNotifyEvent read FOnBlcChange write FOnBlcChange;
    property OnTrialChange : TNotifyEvent read FOnTrialChange write FOnTrialChange;
  end;

var
  FormRandomizePositions: TFormRandomizePositions;

implementation

uses math, strutils, FileUtil, constants;

{$R *.lfm}

procedure TFormRandomizePositions.btnOKClick(Sender: TObject);
var
  LRow, LCol : integer;
  LReportMessage : string;
begin
  with StringGrid do
    for LRow := 1 to RowCount -1 do
      for LCol := 1 to ColCount - 1 do
        begin
          if FSession.ReadTrialString(FBloc.ID,LRow,_Kind) = T_MTS then
            if LCol = 1 then
              begin
                // position
                FSession.WriteToTrial(LRow,FBloc.ID,_Samp+_cBnd, FPositions.Values[Cells[LCol,LRow]]);

                // position report message
                LReportMessage:=ExtractDelimited(1,FSession.ReadTrialString(FBloc.ID,LRow,_Samp+_cStm),[#32]);
                LReportMessage:=ExtractFileNameWithoutExt(LReportMessage);
                LReportMessage:=LReportMessage +' - '+Cells[LCol,LRow];
                FSession.WriteToTrial(LRow,FBloc.ID,_Samp+_cMsg,LReportMessage);

              end;

          if LCol > 1 then
            if (LCol - 1) <= FSession.ReadTrialInteger(FBloc.ID,LRow,_NumComp) then
              begin
                // position
                FSession.WriteToTrial(LRow,FBloc.ID,_Comp+IntToStr(LCol-1)+_cBnd,FPositions.Values[Cells[LCol,LRow]]);

                // position report message
                LReportMessage:=ExtractDelimited(1,FSession.ReadTrialString(FBloc.ID,LRow+1,_Comp+IntToStr(LCol-1)+_cStm),[#32]);
                LReportMessage:=ExtractFileNameWithoutExt(LReportMessage);
                LReportMessage:=LReportMessage +' - '+Cells[LCol,LRow];
                FSession.WriteToTrial(LRow,FBloc.ID,_Comp+IntToStr(LCol-1)+_cMsg,LReportMessage);
              end;
        end;
  StringGrid.Invalidate;
  FSession.UpdateFile;
end;

procedure TFormRandomizePositions.btnRandClick(Sender: TObject);
var i, j, LSquareLine , LSquareID: integer;
  LNumComp: LongInt;
begin
  i:= seBeginAt.Value - 1;
  if pRand.Checked then
    while (i < StringGrid.RowCount -1) and (i <= seEndAt.Value - 1) do
      begin
        for j := 0 to seSeqToWrite.Value -1 do
          begin
            LNumComp := FSession.ReadTrialInteger(FBloc.ID, i+1, _NumComp);
            Rand;
            PutOnGrid(i + 1, LNumComp);
            Inc(i);
          end;
        Inc(i, seGap.Value);
      end;

  if pBalanced.Checked then
    begin
      LSquareID := 0;
      LSquareLine := 0;
      Rand;
      while (i < StringGrid.RowCount -1) and (i <= seEndAt.Value - 1) do
        begin
          LNumComp := FSession.ReadTrialInteger(FBloc.ID, i+1, _NumComp);
          PutOnGrid(i + 1, LNumComp, LSquareLine);
          if i > 0 then
            if i = FPositions.Count*LSquareID then
              while EqualPositionsInRows(i, i+1, LNumComp+1) do
                begin
                  Rand;
                  PutOnGrid(i + 1, LNumComp, LSquareLine);
                end;

          if LSquareLine = (FPositions.Count - 1) then
            begin
              Rand;
              LSquareLine := 0;
              Inc(LSquareID);
            end
          else
            Inc(LSquareLine);
          Inc(i);
        end;
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


procedure TFormRandomizePositions.LoadPositionsFromFile(AFilename: string;
  ABlc: Integer; APositions: string);
var
  i, j : integer;
begin
  FSession := TConfigurationFile.Create(AFilename);
  FSession.CacheUpdates := True;
  FBloc := FSession.Bloc[ABlc];
  SetSpin;

  FPosiNames := TStringList.Create;
  FPositions := TStringList.Create;
  FPositions.Sorted := True;
  //FPositions.Sorted:=True;
  if APositions <> '' then
    // use custom positions
    FPositions.Text := APositions
  else
    // use existing positions
    FSession.ReadPositionsInBloc(FBloc.ID,FPositions);

  for i := 0 to FPositions.Count-1 do
    FPosiNames.Append(FPositions.Names[i]);

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
              if j-1 <= FSession.ReadTrialInteger(FBloc.ID,i,_NumComp) then
                Cells[j, i] := FSession.ReadTrialString(FBloc.ID,i,_Comp+IntToStr(j-1)+_cBnd)
              else
                Cells[j, i] := 'NA';

            if Cells[j, i] = '' then
              Cells[j, i] := '1';
          end;
      Repaint;
    end;
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
  FPosiNames.Free;
  FPositions.Free;
  FBlackList.Free;
  FWhiteList.Free;
  CellsEliminator;
end;

procedure TFormRandomizePositions.PutOnGrid(ATrial: integer; ANumComp: Integer;
  ASquareLine: integer);
var
  n : integer;
begin
  if ATrial <= FBloc.NumTrials then
    begin
      if pRand.Checked then
        begin
          for n := 0 to FPosiNames.Count - 1 do
            begin
              if n = 0 then
                StringGrid.Cells[n + 1, ATrial] := FPosiNames[n];

              if n > 0 then
                if n <= ANumComp then
                  StringGrid.Cells[n + 1, ATrial] := FPosiNames[n];
            end;
        end;

      if pBalanced.Checked then
        begin
          for n := 0 to FPositions.Count - 1 do
            begin
              if n = 0 then
                StringGrid.Cells[n + 1, ATrial] := IntToStr(FLatinSquare[n, ASquareLine]);

              if n > 0 then
                if n <= ANumComp then
                  StringGrid.Cells[n + 1, ATrial] := IntToStr(FLatinSquare[n, ASquareLine]);
            end;
        end;
    end;
end;

procedure TFormRandomizePositions.Rand;
begin
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

  procedure Rotate(var array2 : array of integer; aTimes : integer);   //primeiro elemento torna-se último, elementos restantes para esquerda n vezes
  var aTemp, v, x : integer;
  begin
    for x := 0 to aTimes - 1 do
      begin
        aTemp := array2[0];
        for v := Low(array2) to High(array2)-1 do
          array2[v] := array2[v + 1];
        array2[High(array2)] := aTemp;
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
      for k := 0 to size - 1 do
        sequence[k] := jumbled[k];                   //gerar lista de trabalho a partir da lista de referência

      rotate(sequence, rotateS[i]);                  //mover elementos da lista de trabalho
      for j := 0 to size - 1 do
        FLatinSquare[j, sequence[j] - 1] := signs[i]; //preencher Latin Square
    end;
end;


procedure TFormRandomizePositions.RandSequence;
var
  r, n : integer;
begin
  for n := 0 to FPosiNames.Count - 1 do
    begin
      r := RandomRange(0, FPosiNames.Count);
      FPosiNames.Exchange(n,r);
    end;
end;

function TFormRandomizePositions.EqualPositionsInRows(ARow1, ARow2: integer;
  Offset: integer): Boolean;
var
  LCol: Integer;
begin
  Result := False;
  with StringGrid do
    for LCol := 1 to Offset do
      if Cells[LCol, ARow1] = Cells[LCol, ARow2] then
        begin
          Result := True;
          Break
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
        DrawText(Canvas.Handle, PChar(Cells[ACol,ARow]), Length(Cells[ACol,ARow]), aRect, DT_CENTER);

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
    if Cells[ACol,ARow] = 'NA' then
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
  if not CharInSet(Key, ['0'..'9', #8, #9]) then
    Key := #0;
  StringGrid.Invalidate;
end;

procedure TFormRandomizePositions.StringGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  with TStringGrid(Sender) do
    begin
      //if (Cells[ACol,ARow] = '') or (Cells[ACol,ARow] = 'NA') then
      CanSelect := False;
      //Invalidate;
    end;
end;

procedure TFormRandomizePositions.StringGridSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  with TStringGrid(Sender) do
    begin
      if FPosiNames.IndexOf(Value) = -1 then
        Cells[ACol, ARow] := '1';
      Invalidate;
    end;
end;

{ TStringgrid }

constructor TCStringGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align:=alClient;
  BorderSpacing.Top:=75;
  BorderStyle := bsNone;
  DefaultDrawing := False;
  FixedCols := 1;
  FixedRows := 1;
  HideFocusRect:= True;
  Options:= [{goEditing, }goFixedHorzLine,goHorzLine, goFixedVertLine,goVertLine{, goAlwaysShowEditor}];
  TabOrder := 2;
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

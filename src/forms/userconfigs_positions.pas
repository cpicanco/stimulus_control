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
  , Session.Configuration
  , Session.ConfigurationFile
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
    ButtonSave: TButton;
    ButtonBlocBackward: TButton;
    ButtonBlocFoward: TButton;
    GroupBoxPositions: TGroupBox;
    GroupBoxBlocs: TGroupBox;
    LabelCurrentBloc: TLabel;
    LabelBeginAt: TLabel;
    LabelEndAt: TLabel;
    LabelGap: TLabel;
    LabelLine: TLabel;
    LabelLines: TLabel;
    LabelSeqToWrite: TLabel;
    LabelTrial: TLabel;
    ListBoxPositions: TListBox;
    OpenDialog: TOpenDialog;
    pRandFixedSample: TMenuItem;
    pBalancedFixedSample: TMenuItem;
    pBalanced: TMenuItem;
    pmRand: TPopupMenu;
    pRand: TMenuItem;
    seBeginAt: TSpinEdit;
    seEndAt: TSpinEdit;
    seGap: TSpinEdit;
    seSeqToWrite: TSpinEdit;
    procedure ListBoxPositionsDblClick(Sender: TObject);
    procedure StringGridKeyPress(Sender: TObject; var Key: Char);
    procedure StringGridSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure StringGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure StringGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure btnRandClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnBlocClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    StringGrid : TCStringGrid;
    FSession : TConfigurationFile;
    FPositions : TStringList;
    FPosiNames : TStringList;
    FLatinSquare : array of array of integer;
    FBloc : TCfgBlc;
    FOnBlcChange: TNotifyEvent;
    FOnTrialChange: TNotifyEvent;
    procedure CellsEliminator;
    procedure FillGrid;
    procedure LoadBloc(ABloc : integer);
    procedure PutOnGrid(ATrial : integer; ANumComp :Integer; ASquareLine : integer = 0);
    procedure Rand;
    procedure RandLatinSquare(ASize : integer; ASigns : array of integer);
    procedure RandSequence;
    procedure SetSpin;
    function EqualPositionsInRows(ARow1, ARow2 : integer; AColOnset, AColOffset : integer):Boolean;
  public
    procedure LoadFromFile(AFilename: string; ABlc : Integer; APositions : string = '');overload;
    procedure LoadFromFile(AFilename: string); overload;
    procedure WriteFixedSamplePosition(APosition: string); // maybe it will move to config_session_fileutils
    property OnBlcChange : TNotifyEvent read FOnBlcChange write FOnBlcChange;
    property OnTrialChange : TNotifyEvent read FOnTrialChange write FOnTrialChange;
  end;

var
  FormRandomizePositions: TFormRandomizePositions;

resourcestring
  rs_COL_SAMPLE = 'Modelo';
  rs_CELL_TRIALS_STM = 'Tent\Stm';
  rsBlocs = 'Bloco';
  rsMessNoBlocFound = 'Nenhum Bloco foi encontrado.';

implementation

uses math, strutils, FileUtil, LazFileUtils,constants;

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

procedure TFormRandomizePositions.btnBlocClick(Sender: TObject);
begin
  if Sender = ButtonBlocBackward then
    if FBloc.ID > 1 then
      LoadBloc(FBloc.ID-1);

  if Sender = ButtonBlocFoward then
    if FBloc.ID < FSession.BlocCount then
      LoadBloc(FBloc.ID+1);
end;

procedure TFormRandomizePositions.btnRandClick(Sender: TObject);
var
  i, j : integer;
  LSquareLine , LSquareID: integer;
  LNumComp: LongInt;
  LColOnset , LSquareSize: integer;
begin
  if pRand.Checked or pRandFixedSample.Checked then
    begin
      if pRand.Checked then
        begin
          FPosiNames.Clear;
          for i := 0 to FPositions.Count-1 do
            FPosiNames.Append(FPositions.Names[i]);
        end;

      if pRandFixedSample.Checked then
        begin
          FPosiNames.Clear;
          if FPositions.Count > 0 then
            for i := 1 to FPositions.Count-1 do
              FPosiNames.Append(FPositions.Names[i]);

          for i := 1 to StringGrid.RowCount -1 do
            if FSession.Trial[FBloc.ID, i].Kind = T_MTS then
              StringGrid.Cells[1,i] := '1'
            else
              StringGrid.Cells[1,i] := RSNA;
        end;

      i:= seBeginAt.Value - 1;
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
    end;

  if pBalanced.Checked or pBalancedFixedSample.Checked then
    begin
      if pBalanced.Checked then
        begin
          LColOnset:=1;
          LSquareSize:= FPositions.Count;
        end;

      if pBalancedFixedSample.Checked then
        begin
          LSquareSize:=FPositions.Count-1;
          for i := 1 to StringGrid.RowCount -1 do
            if FSession.Trial[FBloc.ID, i].Kind = T_MTS then
              StringGrid.Cells[1,i] := '1'
            else
              StringGrid.Cells[1,i] := RSNA;
        end;

      LSquareID := 0;
      LSquareLine := 0;
      Rand;
      i:= seBeginAt.Value - 1;
      while (i < StringGrid.RowCount -1) and (i <= seEndAt.Value - 1) do
        begin
          LNumComp := FSession.ReadTrialInteger(FBloc.ID, i+1, _NumComp);
          PutOnGrid(i + 1, LNumComp, LSquareLine);
          if i > 0 then
            if i = (LSquareSize*LSquareID) then
              if FSession.Trial[FBloc.ID, i].Kind = FSession.Trial[FBloc.ID, i+1].Kind then
                begin
                  case FSession.Trial[FBloc.ID, i].Kind of
                    T_Simple :
                      begin
                        LColOnset:=2;


                      end;

                    T_MTS :
                      begin
                        if pBalanced.Checked then
                          LColOnset := 1;

                        if pBalancedFixedSample.Checked then
                          LColOnset := 2;

                        while EqualPositionsInRows(i, i+1, LColOnset, LNumComp+1) do
                          begin
                            Rand;
                            PutOnGrid(i + 1, LNumComp, LSquareLine);
                          end;
                      end;
                  end;

                end;

          if LSquareLine = (LSquareSize - 1) then
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

procedure TFormRandomizePositions.FillGrid;
var
  i : integer;
  j : integer;

  function BndCodeToPositionName(ABndCode : string) : string;
  var
    i : integer;
    LEmpty : string = '';
  begin
    Result := LEmpty;
    if ABndCode <> LEmpty then
      with FPositions do
        for i := 0 to Count-1 do
          if Values[Names[i]] = ABndCode then
            begin
              Result := Names[i];
              Break;
            end;
  end;

begin
  with StringGrid do
    begin
      Clear;
      ColCount := FPositions.Count + 2;   // +1 for fixed col, +1 for MTS samples
      RowCount := FBloc.NumTrials + 1;   //+1 for fixed row
      Cells[0,0] := RS_CELL_TRIALS_STM;

      // write col names
      for j := 1 to ColCount- 1 do
        begin
          if j = 1 then
            Cells[j, 0] := RS_COL_SAMPLE;

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
              if FSession.Trial[FBloc.ID, i].Kind = T_MTS then
                Cells[j, i] := BndCodeToPositionName(FSession.ReadTrialString(FBloc.ID,i,_Samp+_cBnd))
              else
                Cells[j, i] := RSNA;

            if j > 1 then
              if j-1 <= FSession.ReadTrialInteger(FBloc.ID,i,_NumComp) then
                Cells[j, i] := BndCodeToPositionName(FSession.ReadTrialString(FBloc.ID,i,_Comp+IntToStr(j-1)+_cBnd))
              else
                Cells[j, i] := RSNA;

            if Cells[j, i] = '' then
              Cells[j, i] := '1';
          end;
      Repaint;
    end;
end;

procedure TFormRandomizePositions.LoadBloc(ABloc: integer);
var
  i: Integer;
begin
  FBloc := FSession.Bloc[ABloc];
  LabelCurrentBloc.Caption := IntToStr(FBloc.ID) + ' / ' + IntToStr(FSession.BlocCount);
  if FBloc.Name = '' then
    Caption := rsBlocs + ' - ' + IntToStr(FBloc.ID)
  else
    Caption := FBloc.Name;

  SetSpin;
  FPositions.Clear;
  FSession.ReadPositionsInBloc(FBloc.ID, FPositions);
  ListBoxPositions.Items := FPositions;
  FPosiNames.Clear;
  for i := 0 to FPositions.Count-1 do
    FPosiNames.Append(FPositions.Names[i]);

  FillGrid;
end;


procedure TFormRandomizePositions.LoadFromFile(AFilename: string;
  ABlc: Integer; APositions: string);
var
  i : integer;
begin
  FSession := TConfigurationFile.Create(AFilename);
  FSession.CacheUpdates := True;
  FBloc := FSession.Bloc[ABlc];
  if FBloc.Name = '' then
    Caption := rsBlocs + ' - ' + IntToStr(FBloc.ID)
  else
    Caption := FBloc.Name;

  SetSpin;
  if APositions <> '' then // use custom positions
    begin
      FPositions.Text := APositions;
      ListBoxPositions.Items := FPositions;
    end;

  FPosiNames.Clear;
  for i := 0 to FPositions.Count-1 do
    FPosiNames.Append(FPositions.Names[i]);

  FillGrid;
end;

procedure TFormRandomizePositions.LoadFromFile(AFilename: string);
begin
  btnOK.OnClick := nil;
  GroupBoxBlocs.Visible:= True;
  ButtonSave.Visible := True;
  ButtonSave.OnClick := @btnOKClick;
  FSession := TConfigurationFile.Create(AFilename);
  FSession.CacheUpdates := True;
  if FSession.BlocCount > 0 then
    LoadBloc(1)
  else
    ShowMessage(rsMessNoBlocFound);
end;

procedure TFormRandomizePositions.WriteFixedSamplePosition(APosition: string);
var
  i , j: integer;
  LReportMessage: String;
begin
  for i := 0 to FSession.BlocCount-1 do
    for j := 0 to FSession.TrialCount[i]-1 do
      if FSession.ReadTrialString(i,j,_Kind) = T_MTS then
          begin
            // position
            FSession.WriteToTrial(j,i,_Samp+_cBnd, APosition);

            // position report message
            LReportMessage:=ExtractDelimited(1,FSession.ReadTrialString(i,j,_Samp+_cStm),[#32]);
            LReportMessage:=ExtractFileNameWithoutExt(LReportMessage);
            FSession.WriteToTrial(j,i,_Samp+_cMsg,LReportMessage);
          end;
end;

procedure TFormRandomizePositions.FormCreate(Sender: TObject);
begin
  FPosiNames := TStringList.Create;
  FPositions := TStringList.Create;
  FPositions.Sorted := True;
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
  CellsEliminator;
  if Assigned(FSession) then
    FSession.Free;
end;

procedure TFormRandomizePositions.PutOnGrid(ATrial: integer; ANumComp: Integer;
  ASquareLine: integer);
var
  n : integer;
begin
  if ATrial <= FBloc.NumTrials then
    begin
      //////////////////////////// FPositions.Count >2 ///////////////////////////

      if pRand.Checked then
        for n := 0 to FPosiNames.Count - 1 do
          begin
            if FSession.Trial[FBloc.ID, ATrial].Kind = T_MTS then
              begin
                if n = 0 then
                  StringGrid.Cells[n + 1, ATrial] := FPosiNames[n];

                if n > 0 then
                  if n <= ANumComp then
                    StringGrid.Cells[n + 1, ATrial] := FPosiNames[n];
              end;

            if FSession.Trial[FBloc.ID, ATrial].Kind = T_Simple then
              if (n + 1) <= ANumComp then
                StringGrid.Cells[n + 2, ATrial] := FPosiNames[n];
          end;

      if pBalanced.Checked then
        for n := 0 to Length(FLatinSquare) - 1 do
          begin
            if FSession.Trial[FBloc.ID, ATrial].Kind = T_MTS then
              if n = 0 then
                StringGrid.Cells[n + 1, ATrial] := IntToStr(FLatinSquare[n, ASquareLine])
              else
                if n <= ANumComp then
                  StringGrid.Cells[n + 1, ATrial] := IntToStr(FLatinSquare[n, ASquareLine]);

            if FSession.Trial[FBloc.ID, ATrial].Kind = T_Simple then
              if (n + 1) <= ANumComp then
                StringGrid.Cells[n + 2, ATrial] := IntToStr(FLatinSquare[n, ASquareLine]);
          end;

      ////////////////////////////////// MTS only //////////////////////////////////

      if FSession.Trial[FBloc.ID, ATrial].Kind = T_MTS then
        begin
          if pBalancedFixedSample.Checked then
            for n := 0 to Length(FLatinSquare) - 1 do
              if n+1 <= ANumComp then
                StringGrid.Cells[n + 2, ATrial] := IntToStr(FLatinSquare[n, ASquareLine]);

          if pRandFixedSample.Checked then
            for n := 0 to FPosiNames.Count - 1 do
              if n+1 <= ANumComp then
                StringGrid.Cells[n + 2, ATrial] := FPosiNames[n];

        end;
    end;
end;

procedure TFormRandomizePositions.Rand;
var
  LSigns : array of integer;
  i: Integer;
begin
  if pRand.Checked or pRandFixedSample.Checked then
    RandSequence;

  if pBalanced.Checked then
    begin
      SetLength(LSigns,FPositions.Count);
      for i := 0 to FPositions.Count - 1 do LSigns[i] := i + 1;
      RandLatinSquare(FPositions.Count, LSigns);
    end;

  if pBalancedFixedSample.Checked then
    begin
      SetLength(LSigns,FPositions.Count-1);
      for i := 0 to FPositions.Count - 2 do LSigns[i] := i + 2;
      RandLatinSquare(FPositions.Count-1, LSigns);
    end;
end;

procedure TFormRandomizePositions.RandLatinSquare(ASize: integer;
  ASigns: array of integer);
var
  jumbled, sequence, rotateS: array of integer;
  i, j, k : integer;

  procedure SetArrayLength;
  begin
    SetLength(rotateS, ASize);
    SetLength(jumbled, ASize);
    SetLength(sequence, ASize);
    SetLength(FLatinSquare, ASize, ASize);
  end;

  procedure Shuffle(var array1 : array of integer);   //embaralhar lista
  var v, aTemp, aRandom : integer;
  begin
    for v := 0 to ASize - 1 do array1[v] := v + 1;

    for v := 0 to ASize - 1 do
      begin
        aRandom := Round(Random * (ASize - 1));
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

  //procedure WriteSquare;
  //var
  //  i, j : integer;
  //begin
  //  for i := 0 to ASize - 1 do
  //    begin
  //      for j := 0 to ASize - 1 do
  //        Write(FLatinSquare[j,i], #32);
  //      WriteLn('');
  //    end;
  //  WriteLn('');
  //end;

begin
  SetArrayLength;

  shuffle(jumbled);                      //gerar lista de referência; aleatória
  shuffle(rotateS);                      //gerar lista de rotações; aleatória

  for i := 0 to ASize - 1 do
    begin
      for k := 0 to ASize - 1 do
        sequence[k] := jumbled[k];                     //gerar lista de trabalho a partir da lista de referência

      rotate(sequence, rotateS[i]);                    //mover elementos da lista de trabalho
      for j := 0 to ASize - 1 do
        FLatinSquare[j, sequence[j] - 1] := ASigns[i]; //preencher Latin Square
    end;

  //WriteSquare;
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
  AColOnset, AColOffset: integer): Boolean;
var
  LCol: Integer;
begin
  Result := False;
  with StringGrid do
    for LCol := AColOnSet to AColOffset do
      if Cells[LCol, ARow1] = Cells[LCol, ARow2] then
        begin
          Result := True;
          Break
        end;
  //with StringGrid do
  //for LCol := AColOnSet to AColOffset do
  //  if Cells[LCol, ARow1] = Cells[LCol, ARow2] then
  //    begin
  //      Write(Cells[LCol, ARow1]+' ');
  //    end;
  //WriteLn('');
  //with StringGrid do
  //for LCol := AColOnSet to AColOffset do
  //  if Cells[LCol, ARow1] = Cells[LCol, ARow2] then
  //    begin
  //      Write(Cells[LCol, ARow2]+' ');
  //    end;
  //WriteLn('');
  //WriteLn('');
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
        //Canvas.Brush.Color := clRed;
        Canvas.Pen.Color:=clRed;
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
    if Cells[ACol,ARow] = rsNA then
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

procedure TFormRandomizePositions.ListBoxPositionsDblClick(Sender: TObject);
var
  i: Integer;
begin
  if OpenDialog.Execute then
    if FileExists(OpenDialog.FileName) then
      begin
        ListBoxPositions.Items.LoadFromFile(OpenDialog.FileName);
        if ListBoxPositions.Items.Count > 1 then
          begin
            FPositions.Text:=ListBoxPositions.Items.Text;
            FPosiNames.Clear;
            for i := 0 to FPositions.Count-1 do
              FPosiNames.Append(FPositions.Names[i]);

            FillGrid;
            btnRandClick(Sender);
          end;
      end;
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

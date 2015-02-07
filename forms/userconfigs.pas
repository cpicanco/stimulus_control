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
unit userconfigs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, types,
  StdCtrls, ComCtrls, Spin, ExtDlgs, Grids, Menus, Buttons

, bass_player
, draw_methods
, regdata
, session
, session_config
, countermanager
, escriba
, constants
;

type

  { TUserConfig }

  TUserConfig = class(TForm)
    btnRun: TButton;
    btnRandomize: TButton;
    btnSave: TButton;
    btnNextStimuli: TButton;
    btnNextGeneral: TButton;
    btnNextTrials: TButton;
    cbShowRepetitions: TCheckBox;
    chkUseMedia: TCheckBox;
    btnFillType: TButton;
    chkPlayOnSecondMonitor: TCheckBox;
    gbRepetitions: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    lblITI: TLabel;
    lblmilisec: TLabel;
    leParticipant: TLabeledEdit;
    leSchedule: TLabeledEdit;
    leSessionName: TLabeledEdit;
    Memo1: TMemo;
    piMatrix: TMenuItem;
    piAxes: TMenuItem;
    OpenDialog1: TOpenDialog;
    piTrials: TMenuItem;
    piExpectedResponse: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    pgRodar: TPageControl;
    pmRand: TPopupMenu;
    pmFillType: TPopupMenu;
    SaveDialog1: TSaveDialog;
    seCount: TSpinEdit;
    seITI: TSpinEdit;
    StringGrid1: TStringGrid;
    tbSave: TTabSheet;
    tbGeneral: TTabSheet;
    tbStimuli: TTabSheet;
    tbTrials: TTabSheet;
    procedure btnCheckClick(Sender: TObject);
    procedure btnFillTypeClick(Sender: TObject);
    procedure btnNextGeneralClick(Sender: TObject);
    procedure btnNextStimuliClick(Sender: TObject);
    procedure btnNextTrialsClick(Sender: TObject);
    procedure btnRandomizeClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);

    procedure chkUseMediaChange(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure ImageDblClick(Sender: TObject);
    procedure piClick(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
    procedure StringGrid1ColRowMoved(Sender: TObject; IsColumn: Boolean;
      sIndex, tIndex: Integer);
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      Rect: TRect; aState: TGridDrawState);
  private
    FRepetitionMatrix : array of array of boolean;
    FLastColCheckRepetition : integer;
    FNumTrials : integer;
    FSession : TSession;
    FManager : TCounterManager;
    FConfigs : TCfgSes;
    FEscriba : TEscriba;
    procedure RandTrialIndex;
    procedure ResetRepetionMatrix;
    procedure CheckRepetitionCol(aCol : integer);
    procedure EndSession(Sender : TObject);
    //SimpleGui : TSimpleGui;
    //FData : TRegData;
    //FConfig : TConfig;
  public
    { public declarations }

  end;

var
  UserConfig: TUserConfig;
resourcestring
  rsPosition = 'Bnd';
  rsComparison = 'C';
  rsTrials = 'Tentativas';
  rsConsequence = 'Consequência';
  rsAngle = 'Ângulo';
  rsExpectedResponse = 'Resposta';
  rsContingency = 'Contingência';
  rsPositive = 'Positiva';
  rsNegative = 'Negativa';
  rsDefaultPositiveCsq = 'CSQ+';
  rsDefaultNegativeCsq = 'CSQ-';
  rsSize = 'Tamanho';
  rsDefBlc = 'Bloco 1';
  rsEndSession = 'Fim.';
  rsSchedule = 'Esquema';
  rsFillTypeAxes = 'Eixos';
  rsFillTypeMatriz = 'Matriz';
  rsRandomizeTrials = 'Randomizar ordem das tentativas';
  rsRandomizeResponses = 'Randomizar respostas';


implementation

{$R *.lfm}

uses background, userconfigs_trial_mirrored, userconfigs_simple_discrimination_matrix;

{ TUserConfig }

procedure TUserConfig.FormPaint(Sender: TObject);
  //var p1, p2 : TPoint;
begin
  //TopBottomLine(Canvas, lblGeneral);
  //TopBottomLine(Canvas, lblArea);
  //TopBottomLine(Canvas, lblStimulus);
end;


procedure TUserConfig.ImageDblClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    TImage(Sender).Picture.LoadFromFile(OpenPictureDialog1.FileName);
end;

procedure TUserConfig.piClick(Sender: TObject);
begin
  if (TMenuItem(Sender) = piAxes) and (btnFillType.Caption <> rsFillTypeAxes) then
    begin
      btnFillType.Caption := rsFillTypeAxes;
      leSchedule.Text := 'FT 20';
      StringGrid1.ColCount := 9;

      with StringGrid1 do
        begin
          Cells[0, 0] := rsTrials;
          Cells[1, 0] := rsAngle;
          Cells[2, 0] := 'x0';
          Cells[3, 0] := 'y0';
          Cells[4, 0] := 'x1';
          Cells[5, 0] := 'y1';
          Cells[6, 0] := rsExpectedResponse;
          Cells[7, 0] := rsSize;
          Cells[8, 0] := rsSchedule;
        end;

      ResetRepetionMatrix;
    end;

  if (TMenuItem(Sender) = piMatrix) and (btnFillType.Caption <> rsFillTypeMatriz) then
    begin
      btnFillType.Caption := rsFillTypeMatriz;
      leSchedule.Text := 'FRFT 3 40 0 0';
      StringGrid1.ColCount := 4;
      with StringGrid1 do
        begin
          Cells[0, 0] := rsTrials;
          Cells[1, 0] := rsSchedule;
          Cells[2, 0] := rsContingency;
          Cells[3, 0] := rsConsequence;
        end;

      ResetRepetionMatrix;
    end;

  if TMenuItem(Sender) = piTrials then
    begin
      btnRandomize.Hint := rsRandomizeTrials;
    end;

  if TMenuItem(Sender) = piExpectedResponse then
    begin
      btnRandomize.Hint := rsRandomizeResponses;
    end;

  TMenuItem(Sender).Checked := True;
end;

procedure TUserConfig.StringGrid1Click(Sender: TObject);
begin
  //showmessage(inttostr(StringGrid1.Col) + ' ' + inttostr(StringGrid1.Row));
  if cbShowRepetitions.Checked and (StringGrid1.Col <> 0) then
    begin
      FLastColCheckRepetition := StringGrid1.Col;
      CheckRepetitionCol(StringGrid1.Col);
    end;
end;

procedure TUserConfig.StringGrid1ColRowMoved(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  if not IsColumn then
    begin
      ResetRepetionMatrix;
      if (FLastColCheckRepetition <> -1) and cbShowRepetitions.Checked then CheckRepetitionCol(FLastColCheckRepetition);
    end;
end;

procedure TUserConfig.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  Rect: TRect; aState: TGridDrawState);
var OldCanvas : TCanvas;
  procedure SaveOldCanvas;
  begin
    OldCanvas.Brush.Style := StringGrid1.Canvas.Brush.Style;
    OldCanvas.Pen.Width := StringGrid1.Canvas.Pen.Width;
    OldCanvas.Pen.Color := StringGrid1.Canvas.Pen.Color;
  end;

  procedure LoadOldCanvas;
  begin
    StringGrid1.Canvas.Brush.Style := OldCanvas.Brush.Style;
    StringGrid1.Canvas.Pen.Width := OldCanvas.Pen.Width;
    StringGrid1.Canvas.Pen.Color := OldCanvas.Pen.Color;
  end;

begin
  OldCanvas := TCanvas.Create;
  try
    if (aCol >= Low(FRepetitionMatrix)) and
       (aCol <= High(FRepetitionMatrix)) then
         if (aRow >= Low(FRepetitionMatrix[aCol])) and
            (aRoW <= High(FRepetitionMatrix[aCol])) then
    if FRepetitionMatrix[aCol,aRow] then
      begin
        SaveOldCanvas;
        StringGrid1.Canvas.Brush.Style := bsClear;
        StringGrid1.Canvas.Pen.Width:= 3;
        StringGrid1.Canvas.Pen.Color := clRed;
        StringGrid1.Canvas.Rectangle(Rect);
        LoadOldCanvas;
      end;

  finally
    OldCanvas.Free;
  end;
end;

procedure TUserConfig.RandTrialIndex;
var i, NumTrials, RandLine: integer;
    InCell : string;
    StrArray : array of string;
  procedure SaveLine (Line : integer);
  var j : integer;
  begin
    for j := 0 to StringGrid1.ColCount - 1 do
      begin
        InCell := StringGrid1.Cells[(j), (Line)];
        StrArray[j] := InCell;
      end;
  end;
  procedure SendToLine (Line : integer);
  var j : integer;
  begin
    for j := 0 to StringGrid1.ColCount - 1 do
      begin
        InCell := StrArray[j];
        StringGrid1.Cells[(j), (Line)] := InCell;
      end;
  end;
  procedure ChangeLines (New, Old : integer);
  var j : integer;
  begin
    for j := 0 to StringGrid1.ColCount - 1 do
      begin
        InCell := StringGrid1.Cells[(j), (Old)];
        StringGrid1.Cells[(j), (New)] := InCell;
      end;
  end;
begin
  Randomize;
  SetLength(StrArray, StringGrid1.ColCount);
  NumTrials := FNumTrials;
  for i := 0 to NumTrials - 1 do
    begin
      if NumTrials > 1 then
        begin
          RandLine := Round (Random * (NumTrials - 1));
          //if ArraySize > 2 then
          //  while r = n do r := Round (Random * (ArraySize - 1));
          SaveLine (i + 1);
          ChangeLines (i + 1, RandLine + 1);
          SendToLine (RandLine + 1);
        end
      else;
    end;
end;

procedure TUserConfig.ResetRepetionMatrix;
var
  aRowCount, aColCount, i, j : integer;
begin
  aRowCount := StringGrid1.RowCount;
  aColCount := StringGrid1.ColCount;
  SetLength(FRepetitionMatrix, aColCount, aRowCount);
  for j := 0 to aColCount -1 do
    for i := 0 to aRowCount -1 do
      FRepetitionMatrix[j, i] := False;
  Invalidate;
  StringGrid1.Invalidate;
end;

procedure TUserConfig.CheckRepetitionCol(aCol : integer);
var aRowCount, aColCount, aRow, i, j,
    aBeginRow, aEndRow, Count : integer;
    LastLine, Reset : Boolean;

begin
  Count := 1;
  Reset := True;
  LastLine := False;

  aRowCount := StringGrid1.RowCount;
  aColCount := StringGrid1.ColCount;

  for aRow := 1 to aRowCount -2 do
    begin
      if aRow = aRowCount -2 then LastLine := True;

      if Reset then
        begin
          aBeginRow := aRow;
          Reset := False;
        end;

      if StringGrid1.Cells[aCol, aRow] = StringGrid1.Cells[aCol, aRow + 1] then
        begin
          Inc(Count);
          aEndRow := aRow + 1;
          if LastLine then
            if Count >= seCount.Value then
              for i := aBeginRow to aEndRow do FRepetitionMatrix[aCol, i] := True;
        end
      else
        begin
          if Count >= seCount.Value then
            for i := aBeginRow to aEndRow do FRepetitionMatrix[aCol, i] := True;
          Count := 1;
          Reset := True;
        end;
    end;
  Invalidate;
  StringGrid1.Invalidate;
end;

procedure TUserConfig.EndSession(Sender: TObject);
begin
  bkgnd.Free;
  ShowMessage(rsEndSession)
end;

procedure TUserConfig.btnRandomizeClick(Sender: TObject);
var aRow : integer;

begin
  if piTrials.Checked then
    begin
      RandTrialIndex;
    end;

  if piExpectedResponse.Checked then
    begin
      with StringGrid1 do
      for aRow := 1 to RowCount -1 do
        Cells[6, aRow] := IntToStr(Round(Random * 1))
    end;

  ResetRepetionMatrix;
  if FLastColCheckRepetition <> -1 then CheckRepetitionCol(FLastColCheckRepetition);
end;

procedure TUserConfig.btnCheckClick(Sender: TObject);
begin
//
end;

procedure TUserConfig.btnNextGeneralClick(Sender: TObject);
begin
  //FEscriba.Name := leSessionName.Text;

  pgRodar.TabIndex := 1;
end;

procedure TUserConfig.btnNextStimuliClick(Sender: TObject);
begin
  pgRodar.TabIndex := 2;
end;

procedure TUserConfig.btnNextTrialsClick(Sender: TObject);
var
  aTrial, aStm, aBlc, aCol, NumTrials : integer;
  aHStm : integer;
  aValues : string;

  function GetBndString (StmNumber : integer) : string;
  begin
    if StmNumber = 1 then
      Result := StringGrid1.Cells[2, aTrial + 1] + #32 +
                StringGrid1.Cells[3, aTrial + 1] + #32 +
                StringGrid1.Cells[7, aTrial + 1]
    else
      Result := StringGrid1.Cells[4, aTrial + 1] + #32 +
                StringGrid1.Cells[5, aTrial + 1] + #32 +
                StringGrid1.Cells[7, aTrial + 1];
  end;

  function GetGapBool(StmNumber : integer) : Boolean;
  begin
    Result := StrToBool(StringGrid1.Cells[6 {expected response}, aTrial + 1]);  //0 is false, any other value is true
    if (StmNumber = 1) then
    else Result := not Result;
  end;

  function GetNumComp : integer;
  var TopRightCell : string;
  begin
    with StringGrid1 do
      begin
        TopRightCell := Cells[ColCount - 1, 0];
        Delete(TopRightCell, Pos(rsPosition, TopRightCell), Length(rsPosition));
        Delete(TopRightCell, Pos(rsComparison, TopRightCell), Length(rsComparison));
        Result := StrToInt(TopRightCell);
      end;

  end;

  procedure NextValue(var S : string);
  begin
    Delete( S, 1, pos( #32, S ) );
    if Length( S ) > 0 then
      while S[1] = #32 do
        Delete( S, 1, 1 );
  end;

begin
  NumTrials := StringGrid1.RowCount -1;

  if piAxes.Checked then
    begin
      aBlc := 0;

      FEscriba.Name := leSessionName.Text;
      FEscriba.Subject := leParticipant.Text;
      FEscriba.Blcs[aBlc].ITI := seITI.Value;
      FEscriba.SesType  := 'CIC';
      FEscriba.Data := 'Data';
      FEscriba.Media:= 'Media';
     // FEscriba.SetVariables;
      FEscriba.SetMain;

      FEscriba.Blcs[aBlc].Name := rsDefBlc;
      FEscriba.Blcs[aBlc].BkGnd := clWhite;
      FEscriba.Blcs[aBlc].VirtualTrialValue:= 1;
      FEscriba.Blcs[aBlc].NumTrials:= NumTrials;
      FEscriba.SetBlc(aBlc, True);

      FEscriba.SetLengthVetTrial(aBlc);
      for aTrial := Low(FEscriba.Blcs[aBlc].Trials) to High(FEscriba.Blcs[aBlc].Trials) do
        begin
          FEscriba.Blcs[aBlc].Trials[aTrial].Id := aTrial + 1;
          FEscriba.Blcs[aBlc].Trials[aTrial].Kind := T_MRD;
          FEscriba.Blcs[aBlc].Trials[aTrial].NumComp := 2;

          FEscriba.Blcs[aBlc].Trials[aTrial].SList.BeginUpdate;
          FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_BkGnd] := IntToStr (clWhite);
          FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_Cursor] := IntToStr (crDefault);
          FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_UseMedia] := BoolToStr(chkUseMedia.Checked, '1','0');
          FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_ShowStarter] := BoolToStr(True, '1','0');
          FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_Angle] := StringGrid1.Cells[1, aTrial + 1];
          FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_Schedule] := StringGrid1.Cells[8, aTrial + 1];
          FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_NextTrial] := '0';

          FEscriba.Blcs[aBlc].Trials[aTrial].Name := FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_Angle] + #32 + IntToStr(FEscriba.Blcs[aBlc].Trials[aTrial].Id);

          for aStm := 1 to 2 do
            begin
              FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_Comp + IntToStr(aStm) + _cBnd] := GetBndString(aStm);
              FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_Comp + IntToStr(aStm) + _cGap] := BoolToStr(GetGapBool(aStm), '1','0');
              FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_Comp + IntToStr(aStm) + _cGap_Degree] := '0';
              FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_Comp + IntToStr(aStm) + _cGap_Length] := '360'
            end;
          FEscriba.Blcs[aBlc].Trials[aTrial].SList.EndUpdate;
          FEscriba.SetTrial(aTrial);
        end;
    end;

  if piMatrix.Checked then
    begin
      aBlc := 0;

      FEscriba.Name := leSessionName.Text;
      FEscriba.Subject := leParticipant.Text;
      FEscriba.Blcs[aBlc].ITI := seITI.Value;
      FEscriba.SesType  := 'CIC';
      FEscriba.Data := 'Data';
      FEscriba.Media:= 'Media';
     // FEscriba.SetVariables;
      FEscriba.SetMain;

      FEscriba.Blcs[aBlc].Name := rsDefBlc;
      FEscriba.Blcs[aBlc].BkGnd := clWhite;
      FEscriba.Blcs[aBlc].VirtualTrialValue:= 1;
      FEscriba.Blcs[aBlc].NumTrials:= NumTrials;
      FEscriba.SetBlc(aBlc, True);

      FEscriba.SetLengthVetTrial(aBlc);
      for aTrial := Low(FEscriba.Blcs[aBlc].Trials) to High(FEscriba.Blcs[aBlc].Trials) do
        begin
          FEscriba.Blcs[aBlc].Trials[aTrial].Id := aTrial + 1;
          FEscriba.Blcs[aBlc].Trials[aTrial].Kind := T_FPE;
          FEscriba.Blcs[aBlc].Trials[aTrial].NumComp := GetNumComp;

          FEscriba.Blcs[aBlc].Trials[aTrial].SList.BeginUpdate;
          FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_BkGnd] := IntToStr (clWhite);
          FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_Cursor] := IntToStr (crDefault);
          FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_UseMedia] := BoolToStr(chkUseMedia.Checked, '1','0');
          FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_ShowStarter] := BoolToStr(True, '1','0');
          FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_Schedule] := StringGrid1.Cells[1, aTrial + 1];
          FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_ExpectedResponse] := StringGrid1.Cells[2, aTrial + 1];
          // configurar o IET
          FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_Trial + _cIET] := StringGrid1.Cells[3, aTrial + 1];

          FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_NextTrial] := '0';

          FEscriba.Blcs[aBlc].Trials[aTrial].Name := StringGrid1.Cells[2, aTrial + 1] + #32 + IntToStr(FEscriba.Blcs[aBlc].Trials[aTrial].Id);


          aHStm := GetNumComp;

          for aStm := 0 to aHStm do
            begin
              FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_Comp + IntToStr(aStm) + _cBnd] := StringGrid1.Cells[aStm + 4 + (aHStm -1), aTrial + 1];
              aValues :=  StringGrid1.Cells[aStm + 3, aTrial + 1] + #32;
              FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_Comp + IntToStr(aStm) + _cGap] := Copy( aValues, 0, pos( #32, aValues ) - 1);
              NextValue(aValues);
              FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_Comp + IntToStr(aStm) + _cGap_Degree] := Copy( aValues, 0, pos( #32, aValues ) - 1);
              NextValue(aValues);
              FEscriba.Blcs[aBlc].Trials[aTrial].SList.Values[_Comp + IntToStr(aStm) + _cGap_Length] := Copy( aValues, 0, pos( #32, aValues ) - 1);

            end;
          FEscriba.Blcs[aBlc].Trials[aTrial].SList.EndUpdate;
          FEscriba.SetTrial(aTrial);
        end;
    end;

  pgRodar.TabIndex := 3;
end;

procedure TUserConfig.chkUseMediaChange(Sender: TObject);
begin
  Image1.Visible := chkUseMedia.Checked;
  Image2.Visible := chkUseMedia.Checked
end;


procedure TUserConfig.btnFillTypeClick(Sender: TObject);
var
  aRow, aCol, aNode, aTrial, aAxis : integer;
  cAngle,              //Angle
  cSize,               //Size. width, heigth
  cX0, cY0,            //line
  cX1, cY1 : string;   //MirroredLine

  procedure AddAxesToGrid;
  begin
    with BresenhamLineForm.Axis do
      begin
        cX0 := IntToStr(List[aAxis].Line[aNode].X);
        cY0 := IntToStr(List[aAxis].Line[aNode].Y);
        cX1 := IntToStr(List[aAxis].MirroredLine[aNode].X);
        cY1 := IntToStr(List[aAxis].MirroredLine[aNode].Y);
      end;

    with StringGrid1 do
      begin
        if (aRow +1) > RowCount then RowCount := aRow +1;
        Cells[0, aRow] := IntToStr(aRow);    //Trial Number
        Cells[1, aRow] := cAngle;
        Cells[2, aRow] := cX0;
        Cells[3, aRow] := cY0;
        Cells[4, aRow] := cX1;
        Cells[5, aRow] := cY1;
        Cells[6, aRow] := IntToStr(Round(Random * 1));
        Cells[7, aRow] := cSize;
        Cells[8, aRow] := leSchedule.Text;
        Inc(aRow);
        end;
  end;

  procedure AddMatrixTrialToGrid;
  var
    aComp, LowComp, HighComp : integer;
    aContingency, aConsequence, aPosition : string;

  begin
    if MatrixForm.Trials[aTrial].Positive then
      begin
        aContingency := rsPositive;
        aConsequence := rsDefaultPositiveCsq;
      end
    else
      begin
        aContingency := rsNegative;
        aConsequence := rsDefaultNegativeCsq;
      end;

    with StringGrid1 do
      begin
        if (aRow + 1) > RowCount then RowCount := aRow + 1;
        Cells[0, aRow] := IntToStr(aRow);    //Trial Number
        Cells[1, aRow] := leSchedule.Text;
        Cells[2, aRow] := aContingency;
        Cells[3, aRow] := aConsequence;
        aCol := 4;

        LowComp := Low(MatrixForm.Trials[aTrial].Comps);
        HighComp := High(MatrixForm.Trials[aTrial].Comps);
        //ShowMessage(IntToStr(LowComp) + ' ' + IntToStr(HighComp));
        for aComp := LowComp to HighComp do
          begin
            if (aCol + 1) > ColCount then ColCount := aCol + 1;
            Cells[aCol, 0] := rsComparison + IntToStr(aComp + 1);
            Cells[aCol, aRow] := MatrixForm.Trials[aTrial].Comps[aComp].Path;
            Inc(aCol);
          end;

        LowComp := Low(MatrixForm.Trials[aTrial].Comps);
        HighComp := High(MatrixForm.Trials[aTrial].Comps);
        for aComp := LowComp to HighComp do
          begin
            if (aCol + 1) > ColCount then ColCount := aCol + 1;
            Cells[aCol, 0] := rsComparison + IntToStr(aComp + 1) + rsPosition;
            aPosition := IntToStr(MatrixForm.Trials[aTrial].Comps[aComp].Top) + #32 +
                         IntToStr(MatrixForm.Trials[aTrial].Comps[aComp].Left) + #32 +
                         IntToStr(MatrixForm.Trials[aTrial].Comps[aComp].Width) + #32 +
                         IntToStr(MatrixForm.Trials[aTrial].Comps[aComp].Height);

            Cells[aCol, aRow] := aPosition;
            Inc(aCol);
          end;
        Inc(aRow);
      end;
  end;

begin
  if piAxes.Checked then
  begin
    aRow := 1;
    BresenhamLineForm := TBresenhamLineForm.Create(Application);
    if BresenhamLineForm.ShowModal = mrOk then
      begin
        with BresenhamLineForm.Axis do
          for aAxis := Low(List) to High(List) do
            begin
              cAngle := List[aAxis].Angle;
              cSize := IntToStr(List[aAxis].Size);
              for aNode := Low(List[aAxis].Line) to High(List[aAxis].Line) do
                for aTrial := 0 to List[aAxis].TrialsPerNode[aNode] -1 do AddAxesToGrid;
            end;
        FNumTrials := aRow -1;
        BresenhamLineForm.Free;
        ResetRepetionMatrix;
        FLastColCheckRepetition := -1;
      end;
  end;

  if piMatrix.Checked then
    begin
      aRow := 1;
      aCol := 0;
      MatrixForm := TMatrixForm.Create(Application);
      if MatrixForm.ShowModal = mrOk then
          begin
            for aTrial := Low(MatrixForm.Trials) to High(MatrixForm.Trials) do AddMatrixTrialToGrid;

            FNumTrials := aRow - 1;
            MatrixForm.Free;
            ResetRepetionMatrix;
            FLastColCheckRepetition := -1;
          end;
    end;
end;

procedure TUserConfig.FormCreate(Sender: TObject);
var aRowCount, aColCount, i,j : integer;
begin
  Randomize;
  FLastColCheckRepetition := -1;
  StringGrid1.ColCount := 9;
  Caption := Application.Title + ' - ' + 'cpicanco@ufpa.br';

  with StringGrid1 do
    begin
      Cells[0, 0] := rsTrials;
      Cells[1, 0] := rsAngle;
      Cells[2, 0] := 'x0';
      Cells[3, 0] := 'y0';
      Cells[4, 0] := 'x1';
      Cells[5, 0] := 'y1';
      Cells[6, 0] := rsExpectedResponse;
      Cells[7, 0] := rsSize;
      Cells[8, 0] := rsSchedule;
      aRowCount := RowCount;
      aColCount := ColCount;
    end;

  ResetRepetionMatrix;

  FEscriba := TEscriba.Create(Application);
  FEscriba.Memo := Memo1;
  FEscriba.NumBlc := 1;
  FEscriba.SetLengthVetBlc;
end;

procedure TUserConfig.FormDestroy(Sender: TObject);
begin
  //FPosArray.Free;
end;

procedure TUserConfig.btnRunClick(Sender: TObject);
var aDirectory, aDataName : string;
begin
  //FileName := GetCurrentDir + PathDelim + leParticipant.Text + PathDelim + 'Dados_001.txt';
  //FData:= TRegData.Create(Self, Filename);
  //FData.SaveData('Participante:' + #9 + leParticipant.Text + #13#10 +
  //               'Sessão:' + #9 + IntToStr(FData.SessionNumber) + #13#10 +
  //               'Data:' + #9 + DateTimeToStr(Date)+ #13#10 + #13#10);
  aDataName := 'Data_000.txt';
  aDirectory := GetCurrentDirUTF8 + PathDelim + leParticipant.Text;
  if ForceDirectoriesUTF8(aDirectory) then
      OpenDialog1.InitialDir := aDirectory;

  if OpenDialog1.Execute then
    begin
      bkgnd := Tbkgnd.Create(Application);
      with bkgnd do
        begin
          if chkPlayOnSecondMonitor.Checked then Left := Screen.Width + 1;
          Show;
          SetFullScreen(True);
        end;

      FConfigs := TCfgSes.Create(bkgnd);
      FConfigs.LoadFromFile(OpenDialog1.Filename, False);

      FManager := TCounterManager.Create(bkgnd);

      FSession := TSession.Create(bkgnd);
      FSession.BackGround := bkgnd;
      FSession.OnEndSess:= @EndSession;
      FSession.SessName := FConfigs.Name;
      FSession.SubjName := FConfigs.Subject;
      FSession.TestMode := False;
      FSession.ShowCounter := False;
      FSession.AudioDevice := TBassAudioDevice.Create(WindowHandle);

      FSession.Play(FConfigs, FManager, aDataName);
    end;

end;

procedure TUserConfig.btnSaveClick(Sender: TObject);
var aDirectory : string;
begin
  aDirectory := GetCurrentDirUTF8 + PathDelim + leParticipant.Text;
  if ForceDirectoriesUTF8(aDirectory) then
    SaveDialog1.InitialDir := aDirectory;

  if SaveDialog1.Execute then
    FEscriba.SaveMemoTextToTxt(SaveDialog1.FileName);
end;

end.


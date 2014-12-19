//
// Validation Project (PCRF) - Stimulus Control App
// Copyright (C) 2014,  Carlos Rafael Fernandes Picanço, cpicanco@ufpa.br
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
    chkUseMedia: TCheckBox;
    Eixos: TButton;
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
    OpenDialog1: TOpenDialog;
    piTrials: TMenuItem;
    piExpectedResponse: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    pgRodar: TPageControl;
    pmRand: TPopupMenu;
    SaveDialog1: TSaveDialog;
    seCount: TSpinEdit;
    seITI: TSpinEdit;
    sbtnRepetitions: TSpeedButton;
    StringGrid1: TStringGrid;
    tbSave: TTabSheet;
    tbGeneral: TTabSheet;
    tbStimuli: TTabSheet;
    tbTrials: TTabSheet;
    procedure btnCheckClick(Sender: TObject);
    procedure btnNextGeneralClick(Sender: TObject);
    procedure btnNextStimuliClick(Sender: TObject);
    procedure btnNextTrialsClick(Sender: TObject);
    procedure btnRandomizeClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure chkUseMediaChange(Sender: TObject);
    procedure EixosClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure ImageDblClick(Sender: TObject);
    procedure piClick(Sender: TObject);
    procedure sbtnRepetitionsClick(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      Rect: TRect; aState: TGridDrawState);
  private
    FCellToDraw : array of array of boolean;
    FNumTrials : integer;
    FSession : TSession;
    FManager : TCounterManager;
    FConfigs : TCfgSes;
    FEscriba : TEscriba;
    procedure RandTrialIndex;
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
  rsTrials = 'Tentativas';
  rsAngle = 'Ângulo';
  rsExpectedResponse = 'Resposta';
  rsSize = 'Tamanho';
  rsDefBlc = 'Bloco 1';
  rsSchedule = 'Esquema';

{TODO -oRafael -cFunctionality: choose axis and trials for each axis}
{TODO -oRafael -cFunctionality: step from number of trials}
{TODO -oRafael -cFunctionality: 'expected response' randomization}

implementation

{$R *.lfm}

uses background, userconfigs_trial_mirrored;

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
  if TMenuItem(Sender) = piTrials then
    begin
      piTrials.Checked := True;
      piExpectedResponse.Checked := False;
    end;
  if TMenuItem(Sender) = piExpectedResponse then
    begin
      piExpectedResponse.Checked := True;
      piTrials.Checked := False;
    end;
end;

procedure TUserConfig.sbtnRepetitionsClick(Sender: TObject);
begin
  sbtnRepetitions.Down := not sbtnRepetitions.Down;
end;

procedure TUserConfig.StringGrid1Click(Sender: TObject);
begin
  //showmessage(inttostr(StringGrid1.Col) + ' ' + inttostr(StringGrid1.Row));
  CheckRepetitionCol(StringGrid1.Col);
  Invalidate;
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
    if (aCol >= Low(FCellToDraw)) and
       (aCol <= High(FCellToDraw)) then
         if (aRow >= Low(FCellToDraw[aCol])) and
            (aRoW <= High(FCellToDraw[aCol])) then
    if FCellToDraw[aCol,aRow] then
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
  SetLength(FCellToDraw, aColCount, aRowCount);
  for j := 0 to aColCount -1 do
    for i := 0 to aRowCount -1 do
      FCellToDraw[j, i] := False;

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
              for i := aBeginRow to aEndRow do FCellToDraw[aCol, i] := True;
        end
      else
        begin
          if Count >= seCount.Value then
            for i := aBeginRow to aEndRow do FCellToDraw[aCol, i] := True;
          Count := 1;
          Reset := True;
        end;
    end;
end;

procedure TUserConfig.EndSession(Sender: TObject);
begin
  bkgnd.Free;
  ShowMessage('Fim.')
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
  Invalidate;
  StringGrid1.Repaint;
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
var aTrial, aStm, aBlc, NumTrials : integer;

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

begin
  NumTrials := StringGrid1.RowCount -1;
  aBlc := 0;

  FEscriba.Name := leSessionName.Text;
  FEscriba.Subject := leParticipant.Text;
  FEscriba.Blcs[aBlc].ITI := seITI.Value;
  FEscriba.SesType  := 'CIC';
  FEscriba.Data := '/Data/';
  FEscriba.Media:= '/Media/';
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
  pgRodar.TabIndex := 3;
end;

procedure TUserConfig.chkUseMediaChange(Sender: TObject);
begin
  Image1.Visible := chkUseMedia.Checked;
  Image2.Visible := chkUseMedia.Checked
end;

procedure TUserConfig.EixosClick(Sender: TObject);
var
  aRow, aNode, aTrial, aAxis : integer;
  cAngle,              //Angle
  cSize,               //Size. width, heigth
  cX0, cY0,            //line
  cX1, cY1 : string;   //MirroredLine

  procedure AddToGrid;
  begin
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
              for aTrial := 0 to List[aAxis].TrialsPerNode[aNode] -1 do
                begin
                  cX0 := IntToStr(List[aAxis].Line[aNode].X);
                  cY0 := IntToStr(List[aAxis].Line[aNode].Y);
                  cX1 := IntToStr(List[aAxis].MirroredLine[aNode].X);
                  cY1 := IntToStr(List[aAxis].MirroredLine[aNode].Y);
                  AddToGrid;
                end;
          end;
      FNumTrials := aRow -1;
      BresenhamLineForm.Free;
    end
  else;
end;

procedure TUserConfig.FormCreate(Sender: TObject);
var aRowCount, aColCount, i,j : integer;
begin
  Randomize;
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

  aRowCount := StringGrid1.RowCount;
  aColCount := StringGrid1.ColCount;
  SetLength(FCellToDraw, aColCount, aRowCount);
  for j := 0 to aColCount -1 do
    for i := 0 to aRowCount -1 do
      FCellToDraw[j, i] := False;

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
          SetFullScreen(True);
          if chkPlayOnSecondMonitor.Checked then Left := Screen.Width + 1;
          Show;
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


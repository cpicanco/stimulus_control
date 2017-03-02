{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit userconfigs;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, LazFileUtils, Forms, Controls,
     Graphics, Dialogs, ExtCtrls, types, StdCtrls,
     ComCtrls, Spin, ExtDlgs, Grids, Menus, Buttons, XMLPropStorage

    , draw_methods
    , bass_player
    , pupil_communication
    , session
    , config_session
    , escriba
    , constants
    , git_vertioning
    ;

type

  { TFormUserConfig }

  TFormUserConfig = class(TForm)
    btnClientTest: TButton;
    btnFillCondition: TButton;
    btnRun: TButton;
    btnRandomize: TButton;
    btnSave: TButton;
    btnTargetFile: TButton;
    btnContentFile: TButton;
    btnApply: TButton;
    btnExportStimulus: TButton;
    btnTrialsDone: TButton;
    btnPrependTrial: TButton;
    chkPupilClient: TCheckBox;
    chkShowRepetitions: TCheckBox;
    btnGridType: TButton;
    chkDrawTrialGroup: TCheckBox;
    chkPlayOnSecondMonitor: TCheckBox;
    edtTrialGroup: TEdit;
    edtTarget: TEdit;
    edtContent: TEdit;
    gbRepetitions: TGroupBox;
    gbRepetitionsBlocks: TGroupBox;
    gbTrialGroup: TGroupBox;
    Image1: TImage;
    leParticipant: TLabeledEdit;
    leFillValue: TLabeledEdit;
    leSessionName: TLabeledEdit;
    Memo1: TMemo;
    MemoAppInfo: TMemo;
    piGo_NoGo: TMenuItem;
    piTrialsWithConstraints: TMenuItem;
    PanelHeader: TPanel;
    piFillEven: TMenuItem;
    piFillOdd: TMenuItem;
    piFillAll: TMenuItem;
    piMatrix: TMenuItem;
    piAxes: TMenuItem;
    OpenDialog1: TOpenDialog;
    piTrials: TMenuItem;
    piExpectedResponse: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    pgRodar: TPageControl;
    pmRandomize: TPopupMenu;
    pmGridType: TPopupMenu;
    pmFillCondition: TPopupMenu;
    SaveDialog1: TSaveDialog;
    seCount: TSpinEdit;
    seBlocksCount: TSpinEdit;
    stAppTitle: TStaticText;
    stVersion: TStaticText;
    StringGrid1: TStringGrid;
    tbVersion: TTabSheet;
    tbTools: TTabSheet;
    tbSave: TTabSheet;
    tbGeneral: TTabSheet;
    tbStimuli: TTabSheet;
    tbTrials: TTabSheet;
    XMLPropStorage1: TXMLPropStorage;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCheckClick(Sender: TObject);
    procedure btnClientTestClick(Sender: TObject);
    procedure btnExportStimulusClick(Sender: TObject);
    procedure btnFillConditionClick(Sender: TObject);
    procedure btnGridTypeClick(Sender: TObject);
    procedure btnNextGeneralClick(Sender: TObject);
    procedure btnNextStimuliClick(Sender: TObject);
    procedure btnPrependTrialClick(Sender: TObject);
    procedure btnTrialsDoneClick(Sender: TObject);
    procedure btnRandomizeClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure chkDrawTrialGroupChange(Sender: TObject);
    procedure chkShowRepetitionsChange(Sender: TObject);
    procedure chkUseMediaChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImageDblClick(Sender: TObject);
    procedure pgRodarChange(Sender: TObject);

    procedure piClick(Sender: TObject);
    procedure piFillEvenClick(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
    procedure StringGrid1ColRowMoved(Sender: TObject; IsColumn: Boolean;
      sIndex, tIndex: Integer);
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      Rect: TRect; aState: TGridDrawState);
    procedure XMLPropStorage1RestoreProperties(Sender: TObject);
    procedure XMLPropStorage1SaveProperties(Sender: TObject);
  published
    procedure btnClick(Sender: TObject);
  private
    FRepetitionMatrix : array of array of boolean;
    FLastFocusedCol : integer;
    //FNumTrials : integer;
    FSession : TSession;
    //FManager : TCounterManager;
    FConfigs : TCfgSes;
    FAudioDevice : TBassAudioDevice;
    FEscriba : TEscriba;
    //FData : TRegData;
    function MeetCondition(aCol, aRow : integer): boolean;
    function SaneConstraints: Boolean;
    function RepetitionBlocksMeetsConstraints(ALowRow,AHighRow:integer) : Boolean;
    procedure CheckRepetitionCol(aCol : integer);
    procedure EndSession(Sender : TObject);
    procedure RandTrialOrder(BeginRow, EndRow : integer);
    procedure ResetRepetionMatrix;
    procedure ReceiveTimestamp(Sender: TObject; ARequest, AResponse: String);
    //SimpleGui : TSimpleGui;
    //FData : TRegData;
    //FConfig : TConfig;
  public
    { public declarations }

  end;

var
  FormUserConfig: TFormUserConfig;
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
  rsStimulus = 'Figura';
  rsLimitedHold = 'Tempo/Estímulo';
  rsDefaultPositiveCsq = 'NONE,MISS,HIT,';
  rsDefaultNegativeCsq = 'NONE,HIT,MISS,';
  rsSize = 'Tamanho';
  rsDefBlc = 'Bloco 1';
  rsEndSession = 'Fim.';
  rsSchedule = 'Esquema';
  rsFillTypeAxes = 'Eixos';
  rsFillTypeMatriz = 'Matriz';
  rsFillTypeGoNoGo = 'Go/No-Go';
  rsRandomizeTrials = 'Randomizar ordem das tentativas';
  rsRandomizeResponses = 'Randomizar respostas';
  rsRandomizeGroupTrial = 'Randomizar em grupos ordem das tentativas';
  rsRandomizeTrialsWithConstraints = 'Randomizar ordem das tentativas com restrições.';

implementation

{$R *.lfm}

uses background
     , config_session_global_container
     , userconfigs_trial_mirrored
     , userconfigs_simple_discrimination_matrix
     , userconfigs_go_nogo
     , ini_helpers
     {$ifdef DEBUG}
     , debug_logger
     {$endif}
     ;

const
  CSESSION_SERVER = '127.0.1.1:5020';

{ TFormUserConfig }


procedure TFormUserConfig.ImageDblClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    TImage(Sender).Picture.LoadFromFile(OpenPictureDialog1.FileName);
end;

procedure TFormUserConfig.pgRodarChange(Sender: TObject);
begin

end;


procedure TFormUserConfig.piClick(Sender: TObject);
begin
  StringGrid1.Clean;

  if (TMenuItem(Sender) = piAxes) and (btnGridType.Caption <> rsFillTypeAxes) then
    begin
      btnGridType.Caption := rsFillTypeAxes;

      StringGrid1.ColCount := 9;
      StringGrid1.RowCount := 2;
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

  if (TMenuItem(Sender) = piMatrix) and (btnGridType.Caption <> rsFillTypeMatriz) then
    begin
      btnGridType.Caption := rsFillTypeMatriz;

      StringGrid1.ColCount := 4;
      StringGrid1.RowCount := 2;
      with StringGrid1 do
        begin
          Cells[0, 0] := rsTrials;
          Cells[1, 0] := rsSchedule;
          Cells[2, 0] := rsContingency;
          Cells[3, 0] := rsConsequence;
        end;

      ResetRepetionMatrix;
    end;

  if (TMenuItem(Sender) = piGo_NoGo) and (btnGridType.Caption <> rsFillTypeGoNoGo) then
    begin
      btnGridType.Caption := rsFillTypeGoNoGo;

      StringGrid1.ColCount := 6;
      StringGrid1.RowCount := 2;
      with StringGrid1 do
        begin
          Cells[0, 0] := rsTrials;
          Cells[1, 0] := rsConsequence;
          Cells[2, 0] := rsSchedule;
          Cells[3, 0] := rsLimitedHold;
          Cells[4, 0] := rsStimulus;
          Cells[5, 0] := rsSize;
        end;

      ResetRepetionMatrix;
    end;

  // Randomize Buttom
  if TMenuItem(Sender) = piTrials then
      btnRandomize.Hint := rsRandomizeTrials;

  if TMenuItem(Sender) = piExpectedResponse then
      btnRandomize.Hint := rsRandomizeResponses;

  if TMenuItem(Sender) = piTrialsWithConstraints then
      btnRandomize.Hint := rsRandomizeTrialsWithConstraints;

  TMenuItem(Sender).Checked := True;
end;

procedure TFormUserConfig.piFillEvenClick(Sender: TObject);
begin

end;


procedure TFormUserConfig.StringGrid1Click(Sender: TObject);
begin
  //showmessage(inttostr(StringGrid1.Col) + ' ' + inttostr(StringGrid1.Row));
  if StringGrid1.Col <> 0 then
    begin
      FLastFocusedCol := StringGrid1.Col;
      leFillValue.EditLabel.Caption := StringGrid1.Cells[StringGrid1.Col,0];
      if chkShowRepetitions.Checked then CheckRepetitionCol(StringGrid1.Col);
    end;
end;

procedure TFormUserConfig.StringGrid1ColRowMoved(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  if not IsColumn then
    begin
      ResetRepetionMatrix;
      if (FLastFocusedCol <> -1) and chkShowRepetitions.Checked then CheckRepetitionCol(FLastFocusedCol);
    end;
end;

procedure TFormUserConfig.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  Rect: TRect; aState: TGridDrawState);
var
  OldCanvas : TCanvas;

  i, aInc : integer;

  procedure SaveOldCanvas;
  begin
    OldCanvas.Brush.Style := StringGrid1.Canvas.Brush.Style;
    OldCanvas.Pen.Width := StringGrid1.Canvas.Pen.Width;
    OldCanvas.Pen.Color := StringGrid1.Canvas.Pen.Color;
    OldCanvas.Pen.Mode := StringGrid1.Canvas.Pen.Mode;

  end;

  procedure LoadOldCanvas;
  begin
    StringGrid1.Canvas.Brush.Style := OldCanvas.Brush.Style;
    StringGrid1.Canvas.Pen.Width := OldCanvas.Pen.Width;
    StringGrid1.Canvas.Pen.Color := OldCanvas.Pen.Color;
    StringGrid1.Canvas.Pen.Mode := OldCanvas.Pen.Mode;
  end;

begin
  OldCanvas := TCanvas.Create;
  SaveOldCanvas;
  try
    if (aCol >= Low(FRepetitionMatrix)) and
       (aCol <= High(FRepetitionMatrix)) then
      if (aRow >= Low(FRepetitionMatrix[aCol])) and
        (aRoW <= High(FRepetitionMatrix[aCol])) then
        if FRepetitionMatrix[aCol,aRow] then
          begin
            StringGrid1.Canvas.Brush.Style := bsClear;
            StringGrid1.Canvas.Pen.Width:= 3;
            StringGrid1.Canvas.Pen.Color := clRed;
            StringGrid1.Canvas.Rectangle(Rect);
            LoadOldCanvas;
          end;

  if chkDrawTrialGroup.Checked then
    begin
      StringGrid1.Canvas.Pen.Color := clBlue;
      aInc := StrToInt(edtTrialGroup.Text);
      i := 0;

      while i <= StringGrid1.RowCount -1 do
        begin
          if aRow = i then
            begin
              StringGrid1.Canvas.Pen.Color := clBlue;
              StringGrid1.Canvas.Pen.Width := 2;
              StringGrid1.Canvas.Pen.Mode := pmMerge;

              StringGrid1.Canvas.MoveTo(Rect.Left -1, Rect.Bottom -2);
              StringGrid1.Canvas.LineTo(Rect.Right -1, Rect.Bottom -2);
              LoadOldCanvas;
            end;
          Inc(i, aInc);
        end;
      LoadOldCanvas;
    end;

  finally
    OldCanvas.Free;
  end;
end;

procedure TFormUserConfig.XMLPropStorage1RestoreProperties(Sender: TObject);
begin
  if FileExistsUTF8('stringgrid.csv') then
    StringGrid1.LoadFromCSVFile('stringgrid.csv',',',True,0,False);
  ResetRepetionMatrix;
end;

procedure TFormUserConfig.XMLPropStorage1SaveProperties(Sender: TObject);
begin
  StringGrid1.SaveToCSVFile('stringgrid.csv');
end;

function TFormUserConfig.MeetCondition(aCol, aRow : integer): boolean;
//var
//  aRowString : string;
begin
  //conditions needs aCol;
  Result := False;

  if piFillAll.Checked then
    Result := piFillAll.Checked;

  if piFillOdd.Checked then
    if (StrToIntDef(StringGrid1.Cells[0, aRow], 2) mod 2) <> 0 then
      Result := piFillOdd.Checked;

  if piFillEven.Checked then
    if (StrToIntDef(StringGrid1.Cells[0, aRow], 1) mod 2) = 0 then
      Result := piFillEven.Checked;
end;

function TFormUserConfig.SaneConstraints: Boolean;
begin
  Result := True;
end;

function TFormUserConfig.RepetitionBlocksMeetsConstraints(ALowRow, AHighRow: integer): Boolean;
var
  LRow, LCol,
  LCount,
  LBlockSize, LMaxBlocksAllowed, LBlockCount: integer;
begin
  Result := False;
  LBlockSize := seCount.Value;
  LMaxBlocksAllowed := seBlocksCount.Value;
  LBlockCount := 0;
  LCol := FLastFocusedCol;

  LCount := 1;
  for LRow := ALowRow to AHighRow do
    begin
      if (StringGrid1.Cells[LCol, LRow] = StringGrid1.Cells[LCol, LRow + 1]) then
        begin
          // repetition count
          Inc(LCount);

          // constraint 1
          if LCount > LBlockSize then
            Exit;

          // block count
          if LCount = LBlockSize then
            Inc(LBlockCount);

          // constraint 2
          if LBlockCount > LMaxBlocksAllowed then
            Exit;
        end
      else
        LCount := 1;

    end;
  Result := True;
end;

{
  Each trial is a Row in the StringGrid1 except the first.
}
procedure TFormUserConfig.RandTrialOrder(BeginRow, EndRow : integer);
var
  i, RandLine: integer;
  InCell : string;
  StrArray : array of string;

  procedure SaveLine (Line : integer);
  var j : integer;
  begin
    for j := 0 to StringGrid1.ColCount - 1 do
      begin
        InCell := StringGrid1.Cells[j, Line];
        StrArray[j] := InCell;
      end;
  end;

  procedure SendToLine (Line : integer);
  var j : integer;
  begin
    for j := 0 to StringGrid1.ColCount - 1 do
      begin
        InCell := StrArray[j];
        StringGrid1.Cells[j, Line] := InCell;
      end;
  end;

  procedure ChangeLines (New, Old : integer);
  var j : integer;
  begin
    for j := 0 to StringGrid1.ColCount - 1 do
      begin
        InCell := StringGrid1.Cells[j, Old];
        StringGrid1.Cells[j, New] := InCell;
      end;
  end;

begin
  SetLength(StrArray, StringGrid1.ColCount);

  for i := BeginRow to EndRow do
    begin
      RandLine := Random(EndRow - BeginRow) + BeginRow;
      {$ifdef DEBUG}
        DebugLn(mt_Information + 'RandTrialOrder  ' + IntToStr(BeginRow) + ',' +  IntToStr(EndRow) );
        DebugLn(mt_Information + IntToStr(i) + ',' + IntToStr(RandLine));
      {$endif}

      SaveLine (i);
      ChangeLines (i, RandLine);
      SendToLine (RandLine);
    end;
end;

procedure TFormUserConfig.ResetRepetionMatrix;
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

procedure TFormUserConfig.ReceiveTimestamp(Sender: TObject; ARequest,
  AResponse: String);
begin
  case ARequest of
    REQ_TIMESTAMP : ShowMessage(Sender.ClassName + #32 + AResponse);
  end;
end;

procedure TFormUserConfig.CheckRepetitionCol(aCol : integer);
var
  LRowCount, LRow, i,
  LBeginRow, LEndRow, LCount : integer;
  LReset : Boolean;

begin
  LCount := 1;
  LReset := True;
  LRowCount := StringGrid1.RowCount;

  for LRow := 1 to LRowCount -2 do
    begin
      if LReset then
        begin
          LBeginRow := LRow;
          LReset := False;
        end;

      if StringGrid1.Cells[aCol, LRow] = StringGrid1.Cells[aCol, LRow + 1] then
        begin
          Inc(LCount);
          LEndRow := LRow + 1;
          if LCount >= seCount.Value then
            for i := LBeginRow to LEndRow do
              FRepetitionMatrix[aCol, i] := True;
        end
      else
        begin
          LCount := 1;
          LReset := True;
        end;
    end;
  Invalidate;
  StringGrid1.Invalidate;
end;

procedure TFormUserConfig.EndSession(Sender: TObject);
begin
  while FrmBackground.ComponentCount > 0 do
    FrmBackground.Components[0].Free;
  //FrmBackground.Invalidate;
  FrmBackground.Hide;
  ShowMessage(rsEndSession)
end;

procedure TFormUserConfig.btnRandomizeClick(Sender: TObject);
var
  LRow,
  LBeginRow, LEndRow: integer;
  procedure RandomTrialGroup(AWithConstraints : Boolean);
  begin
    LRow {increment} := StrToInt(edtTrialGroup.Text);
    LBeginRow := 1;
    LEndRow :=  LRow {increment} + 1;
    while LEndRow <= StringGrid1.RowCount - 1 do
      begin
        if AWithConstraints then
          repeat
            RandTrialOrder(LBeginRow, LEndRow -1);
          until RepetitionBlocksMeetsConstraints(LBeginRow,LEndRow -2)
        else
          RandTrialOrder(LBeginRow, LEndRow -1);
        Inc(LBeginRow, LRow {increment});
        Inc(LEndRow, LRow {increment});
      end;
    ResetRepetionMatrix;
    CheckRepetitionCol(FLastFocusedCol);
  end;

begin
  if (FLastFocusedCol <> -1) then
    begin
      if chkDrawTrialGroup.Checked then
        begin
          RandomTrialGroup(piTrialsWithConstraints.Checked);
          Exit;
        end;

      if piTrials.Checked then
        RandTrialOrder(1, StringGrid1.RowCount -1);

      if piTrialsWithConstraints.Checked and SaneConstraints then
        repeat
          RandTrialOrder(1, StringGrid1.RowCount -1);
        until RepetitionBlocksMeetsConstraints(1,StringGrid1.RowCount -2);

      if piExpectedResponse.Checked then
        with StringGrid1 do
          for LRow := 1 to RowCount -1 do
            Cells[6, LRow] := IntToStr(Round(Random * 1));

      ResetRepetionMatrix;
      CheckRepetitionCol(FLastFocusedCol);
    end
  else ShowMessage('Escolha o alvo da randomização clicando sobre uma célula de uma coluna.');
end;

procedure TFormUserConfig.btnCheckClick(Sender: TObject);
begin
  //
end;

{

  btnApplyClick may become a class in the near future

}

procedure TFormUserConfig.btnApplyClick(Sender: TObject);
var

  i, line,
  //VirtualTrialValue,
  NumTrials : integer;

  s1,
  Content : string;

  NewFile,
  Sections : TStringList;

  IniTarget,
  IniContent : TCIniFile;
begin
  if FileExists(edtTarget.Text) and FileExists(edtContent.Text) then
    begin

      // initialize  string lists
      NewFile := TStringList.Create;
      NewFile.Duplicates := dupIgnore;

      Sections := TStringList.Create;
      Sections.Duplicates := dupIgnore;
      try
        Sections.LoadFromFile(edtTarget.Text);

        // get content
        IniContent := TCIniFile.Create(edtContent.Text);
        try
          IniContent.ReadSectionRaw('Trial', Sections);
          Content := Sections.Text;
        finally
          IniContent.Free;
        end;

        // get target
        IniTarget := TCIniFile.Create(edtTarget.Text);
        try
          // get sections
          // Sections.Clear;
          // IniTarget.ReadSections(Sections);

          i := 1;
          // get old number of trials
          NumTrials:= GetNumTrials(IniTarget);
          //showmessage(inttostr(numtrials));

          // get old virtual trial value
          //Delete(s1, 1, Pos(#32, s1));
          //if Length(s1) > 0 then while s1[1] = #32 do Delete(s1, 1, 1);
          //VirtualTrialValue:= StrToIntDef(s1, 0);

          // Update the NumTrial key
          Inc(NumTrials);

          //for this method we must have made a copy first
          //IniTarget.WriteString(GetSection(0,1, False), 'NumTrials', IntToStr(NumTrials) + #32 + IntToStr(VirtualTrialValue) );
          //IniTarget.UpdateFile;


          // this method does not work with multiple blocs
          // load/copy target stream
          NewFile.LoadFromFile(edtTarget.Text);
          i := NewFile.IndexOf('NumTrials=');

          for i := 0 to NewFile.Count -1 do
            begin
              if Pos('NumTrials', NewFile.Strings[i]) <> 0 then Break;
            end;

          NewFile.Strings[i] := 'NumTrials=' + #9 + IntToStr(NumTrials) + #32 + '0';

          //ShowMessage(NewFile.Strings[i]);

        finally
          IniTarget.Free;
        end;
        // insert the content before the first trial
        s1 := NewFile.Text;
        Insert(GetSection(0,1) + LineEnding + Content + LineEnding, s1, Pos(GetSection(1,1), NewFile.Text));
        NewFile.Text := s1;
        // loop incrementing trial section numbers
        for i := 0 to NumTrials -1 do
          begin

            // Make i + 1 turns to 0
            line := NewFile.IndexOf(GetSection(i + 1,1));
            if line <> -1 then
              begin
                NewFile[line] := GetSection(0,1);
              end;

            // Inc i
            line := NewFile.IndexOf(GetSection(0,1));
            NewFile[line] := GetSection(i + 1,1);
          end;

         NewFile.SaveToFile(edtTarget.Text + '.new');

      finally
        NewFile.Free;
        Sections.Free;
      end;
    end;
end;

procedure TFormUserConfig.btnClientTestClick(Sender: TObject);
var PupilClient : TPupilCommunication;
begin
  if chkPupilClient.Checked then
    begin
      PupilClient := TPupilCommunication.Create('127.0.1.1:5020');
      PupilClient.OnRequestReceived := @ReceiveTimestamp;

      PupilClient.Start;
      PupilClient.Request(REQ_TIMESTAMP);

      Sleep(1000);
      PupilClient.Terminate;
    end;
end;

procedure TFormUserConfig.btnExportStimulusClick(Sender: TObject);
var
    Bitmap : TBitmap;
    R : TRect;

    procedure DoWhiteBackGround;
    begin
      with Bitmap do
        begin
          Canvas.Brush.Color := clWhite;
          Canvas.Pen.Mode := pmWhite;
          Canvas.Pen.Color := clWhite;
          Canvas.Rectangle(0,0,Width, Height);
        end;
    end;

begin
  if SaveDialog1.Execute then
    begin

      // create a bitmap to draw
      Bitmap := TBitmap.Create;
      with Bitmap do
        try
          begin
            // give it a size
            Width := 500;
            Height := 500;
            R.TopLeft := Point(0, 0);
            R.BottomRight := Point(0+Width,0+Height);

            DoWhiteBackGround;
            DrawCustomEllipse(Canvas, R,GetInnerRect(R,Width, Height), False, 0, 0);
            SaveToFile(SaveDialog1.FileName + '_1.bmp');

            DoWhiteBackGround;
            DrawCustomEllipse(Canvas,R,GetInnerRect(R,Width, Height), True, 0, 16*360);
            SaveToFile(SaveDialog1.FileName + '_2.bmp');

            DoWhiteBackGround;
            DrawCustomEllipse(Canvas, R,GetInnerRect(R,Width, Height), True, 16*45, 16*1);
            SaveToFile(SaveDialog1.FileName + '_3.bmp');
          end;

        finally
          Bitmap.Free;
        end;
    end;
end;

procedure TFormUserConfig.btnFillConditionClick(Sender: TObject);
var
    aRow,
    aRowCount : integer;

begin
  if (FLastFocusedCol <> -1) and (FLastFocusedCol <> 0) then
    begin
      aRowCount := StringGrid1.RowCount;
      for aRow := 1 to aRowCount -1 do
        if MeetCondition(FLastFocusedCol, aRow) then StringGrid1.Cells[FLastFocusedCol, aRow] := leFillValue.Text;
    end;
end;

procedure TFormUserConfig.btnNextGeneralClick(Sender: TObject);
begin
  pgRodar.TabIndex := 1;
end;

procedure TFormUserConfig.btnNextStimuliClick(Sender: TObject);
begin
  pgRodar.TabIndex := 2;
end;

procedure TFormUserConfig.btnPrependTrialClick(Sender: TObject);
begin
  // todo: edit configuration files, prepend
  // (edtTarget.Text);
end;

procedure TFormUserConfig.btnTrialsDoneClick(Sender: TObject);
var
  aTrial, aStm, aBlc, aNumTrials : integer;
  aHStm : integer;
  aValues : string;
  T : TCfgTrial;
  B : TCfgBlc;

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

  // '0' false; '1' true
  function GetGapString(StmNumber : integer) : String;
  var aGap : Boolean;
  begin
    if StmNumber = 1 then
      Result := StringGrid1.Cells[6 {expected response}, aTrial + 1]
    else
      begin
        aGap := StrToBool(StringGrid1.Cells[6 , aTrial + 1]);
        Result := BoolToStr(not aGap, '1', '0');
      end;
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
  aNumTrials := StringGrid1.RowCount -1;
  FEscriba.SessionServer := CSESSION_SERVER;
  if piAxes.Checked then
    begin
      aBlc := 0;

      FEscriba.SessionName := leSessionName.Text;
      FEscriba.SessionSubject := leParticipant.Text;
      FEscriba.Blcs[aBlc].ITI := 1000;
      FEscriba.SessionType  := T_CIC;
      FEscriba.Data := 'Data';
      FEscriba.Media:= 'Media';
     // FEscriba.SetVariables;
      FEscriba.SetMain;

      B := FEscriba.Blcs[aBlc];
      with B do
        begin
          Name := rsDefBlc;
          BkGnd := clWhite;
          VirtualTrialValue:= 1;
          NumTrials := aNumTrials;
        end;
      FEscriba.Blcs[aBlc] := B;
      FEscriba.SetBlc(aBlc, True);
      FEscriba.SetLengthVetTrial(aBlc);
      B := FEscriba.Blcs[aBlc];

      for aTrial := Low(B.Trials) to High(B.Trials) do
        begin
          T := FEscriba.Blcs[aBlc].Trials[aTrial];
          with T do
            begin
              Id := aTrial + 1;
              Kind := T_MRD;
              NumComp := 2;
              Name := SList.Values[_Angle] + #32 + IntToStr(Id);

              SList.BeginUpdate;
              SList.Values[_BkGnd] := IntToStr (clWhite);
              SList.Values[_Cursor] := IntToStr (crNone);
              SList.Values[_UseMedia] := BoolToStr(False, '1','0');
              SList.Values[_ShowStarter] := BoolToStr(True, '1','0');
              SList.Values[_Angle] := StringGrid1.Cells[1, aTrial + 1];
              SList.Values[_Schedule] := StringGrid1.Cells[8, aTrial + 1];
              SList.Values[_NextTrial] := '0';

              for aStm := 1 to 2 do
                begin
                  SList.Values[_Comp + IntToStr(aStm) + _cBnd] := GetBndString(aStm);
                  SList.Values[_Comp + IntToStr(aStm) + _cGap] := GetGapString(aStm);
                  SList.Values[_Comp + IntToStr(aStm) + _cGap_Degree] := '0';
                  SList.Values[_Comp + IntToStr(aStm) + _cGap_Length] := '360'
                end;
              SList.EndUpdate;

            end;
          FEscriba.Blcs[aBlc].Trials[aTrial] := T;
          FEscriba.SetTrial(aTrial);
        end;
    end;

  if piMatrix.Checked then
    begin
      aBlc := 0;

      FEscriba.SessionName := leSessionName.Text;
      FEscriba.SessionSubject := leParticipant.Text;
      FEscriba.Blcs[aBlc].ITI := 1000;
      FEscriba.SessionType  := 'CIC';
      FEscriba.Data := 'Data';
      FEscriba.Media:= 'Media';
      // FEscriba.SetVariables;
      FEscriba.SetMain;

      B := FEscriba.Blcs[aBlc];
      with B do
        begin
          Name := rsDefBlc;
          BkGnd := clWhite;
          VirtualTrialValue:= 1;
          NumTrials := aNumTrials;
        end;
      FEscriba.Blcs[aBlc] := B;
      FEscriba.SetBlc(aBlc, True);
      FEscriba.SetLengthVetTrial(aBlc);
      B := FEscriba.Blcs[aBlc];

      for aTrial := Low(B.Trials) to High(B.Trials) do
        begin
          T := FEscriba.Blcs[aBlc].Trials[aTrial];
          with T do
            begin
              Id := aTrial + 1;
              Kind := T_FPE;
              NumComp := GetNumComp;
              Name := StringGrid1.Cells[2, aTrial + 1] + #32 + IntToStr(Id);

              SList.BeginUpdate;
              SList.Values[_BkGnd] := IntToStr(clWhite);
              SList.Values[_Cursor] := IntToStr(crNone);
              // SList.Values[_UseMedia] := BoolToStr(False, '1','0');
              SList.Values[_ShowStarter] := BoolToStr(True, '1','0');
              SList.Values[_LimitedHold] := '2500';
              SList.Values[_Schedule] := StringGrid1.Cells[1, aTrial + 1];
              SList.Values[_ExpectedResponse] := StringGrid1.Cells[2, aTrial + 1];
              SList.Values[_Trial + _cIET] := StringGrid1.Cells[3, aTrial + 1];   // configure the IETConsenquence
              SList.Values[_NextTrial] := '0';

              aHStm := GetNumComp;
              for aStm := 0 to aHStm do
                begin
                  SList.Values[_Comp + IntToStr(aStm) + _cBnd] := StringGrid1.Cells[aStm + 4 + (aHStm -1), aTrial + 1];
                  aValues :=  StringGrid1.Cells[aStm + 3, aTrial + 1] + #32;
                  SList.Values[_Comp + IntToStr(aStm) + _cGap] := Copy( aValues, 0, pos( #32, aValues ) - 1);
                  NextValue(aValues);
                  SList.Values[_Comp + IntToStr(aStm) + _cGap_Degree] := Copy( aValues, 0, pos( #32, aValues ) - 1);
                  NextValue(aValues);
                  SList.Values[_Comp + IntToStr(aStm) + _cGap_Length] := Copy( aValues, 0, pos( #32, aValues ) - 1);

                end;
              SList.EndUpdate;
            end;
          FEscriba.Blcs[aBlc].Trials[aTrial] := T;
          FEscriba.SetTrial(aTrial);
        end;
    end;

  if piGo_NoGo.Checked then
    begin
      aBlc := 0;

      FEscriba.SessionName := leSessionName.Text;
      FEscriba.SessionSubject := leParticipant.Text;
      FEscriba.Blcs[aBlc].ITI := 1000;
      FEscriba.SessionType  := 'CIC';
      FEscriba.Data := 'Data';
      FEscriba.Media:= 'Media';
      // FEscriba.SetVariables;
      FEscriba.SetMain;

      B := FEscriba.Blcs[aBlc];
      with B do
        begin
          Name := rsDefBlc;
          BkGnd := clWhite;
          VirtualTrialValue:= 1;
          NumTrials := aNumTrials;
        end;
      FEscriba.Blcs[aBlc] := B;
      FEscriba.SetBlc(aBlc, True);
      FEscriba.SetLengthVetTrial(aBlc);
      B := FEscriba.Blcs[aBlc];

      for aTrial := Low(B.Trials) to High(B.Trials) do
        begin
          T := FEscriba.Blcs[aBlc].Trials[aTrial];
          with T do
            begin
              Id := aTrial + 1;
              Kind := T_GNG;
              Name := StringGrid1.Cells[1, aTrial + 1] + #32 + IntToStr(Id);

              SList.BeginUpdate;
              SList.Values[_Consequence] := StringGrid1.Cells[1, aTrial + 1];
              SList.Values[_Schedule] := StringGrid1.Cells[2, aTrial + 1];
              SList.Values[_LimitedHold] := StringGrid1.Cells[3, aTrial + 1];;
              SList.Values[_Comp+'1'+_cStm] := StringGrid1.Cells[4, aTrial + 1];   // configure the IETConsenquence
              SList.Values[_Comp+'1'+_cBnd] := StringGrid1.Cells[5, aTrial + 1];   // configure the IETConsenquence
              SList.EndUpdate;
            end;
          FEscriba.Blcs[aBlc].Trials[aTrial] := T;
          FEscriba.SetTrial(aTrial, False);
        end;
    end;

  pgRodar.TabIndex := 3;
end;

procedure TFormUserConfig.chkUseMediaChange(Sender: TObject);
begin
  //GUI to select custom images from media files was not implemented yet
  //Image1.Visible := chkUseMedia.Checked;
  //Image2.Visible := chkUseMedia.Checked
end;

//procedure TFormUserConfig.FormActivate(Sender: TObject);
//var i : integer;
//  function GetMonitorString(Monitor : TMonitor) : string;
//  begin
//    with Monitor do
//      Result := 'Monitor' + #32 + IntToStr(MonitorNum) + #32 + IntToStr(Width) + #32 + IntToStr(Height);
//  end;
//begin
//  WriteLn('Monitors');
//  with Screen do
//    for i := 0 to MonitorCount - 1 do
//        WriteLn(GetMonitorString(Monitors[i]));
//end;


procedure TFormUserConfig.btnGridTypeClick(Sender: TObject);
var
  aRow, aCol, aTrial, aNode, aAxis, aRepeat : integer;
  cAngle,              //Angle
  cSize,               //Size. width, heigth
  cX0, cY0,            //line
  cX1, cY1 : string;   //MirroredLine
  Closed : Boolean;

  procedure AddAxesToGrid;
  begin
    with FrmBresenhamLine.Axis do
      begin
        cX0 := IntToStr(List[aAxis].Line[aNode].X);
        cY0 := IntToStr(List[aAxis].Line[aNode].Y);
        cX1 := IntToStr(List[aAxis].MirroredLine[aNode].X);
        cY1 := IntToStr(List[aAxis].MirroredLine[aNode].Y);
        if aTrial = 0 then
          Closed := True
        else Closed := False;
      end;

    with StringGrid1 do
      begin
        if (aRow +1) > RowCount then RowCount := aRow + 1;
        Cells[0, aRow] := IntToStr(aRow);    //Trial Number
        Cells[1, aRow] := cAngle;
        Cells[2, aRow] := cX0;
        Cells[3, aRow] := cY0;
        Cells[4, aRow] := cX1;
        Cells[5, aRow] := cY1;
        Cells[6, aRow] := BoolToStr(Closed, '1', '0');
        Cells[7, aRow] := cSize;
        Cells[8, aRow] := 'FT 2000';
        Inc(aRow);
      end;
  end;

  procedure AddMatrixTrialToGrid;
  var
    aComp, LowComp, HighComp : integer;
    aContingency, aConsequence, aPosition : string;

  begin
    if FrmMatrix.Trials[aTrial].Positive then
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
        Cells[1, aRow] := 'FR 3 0';
        Cells[2, aRow] := aContingency;
        Cells[3, aRow] := aConsequence;
        aCol := 4;

        LowComp := Low(FrmMatrix.Trials[aTrial].Comps);
        HighComp := High(FrmMatrix.Trials[aTrial].Comps);
        //ShowMessage(IntToStr(LowComp) + ' ' + IntToStr(HighComp));
        for aComp := LowComp to HighComp do
          begin
            if (aCol + 1) > ColCount then ColCount := aCol + 1;
            Cells[aCol, 0] := rsComparison + IntToStr(aComp + 1);
            Cells[aCol, aRow] := FrmMatrix.Trials[aTrial].Comps[aComp].Path;
            Inc(aCol);
          end;

        LowComp := Low(FrmMatrix.Trials[aTrial].Comps);
        HighComp := High(FrmMatrix.Trials[aTrial].Comps);
        for aComp := LowComp to HighComp do
          begin
            if (aCol + 1) > ColCount then ColCount := aCol + 1;
            Cells[aCol, 0] := rsComparison + IntToStr(aComp + 1) + rsPosition;
            aPosition := IntToStr(FrmMatrix.Trials[aTrial].Comps[aComp].Top) + #32 +
                         IntToStr(FrmMatrix.Trials[aTrial].Comps[aComp].Left) + #32 +
                         IntToStr(FrmMatrix.Trials[aTrial].Comps[aComp].Width) + #32 +
                         IntToStr(FrmMatrix.Trials[aTrial].Comps[aComp].Height);

            Cells[aCol, aRow] := aPosition;
            Inc(aCol);
          end;
        Inc(aRow);
      end;
  end;

  procedure AddGoNoGoTrialsToGrid;
  var
    aConsequence : string;
  begin
    if FormGo_NoGo.Trials[aTrial].Positive then
      aConsequence := rsPositive
    else
      aConsequence := rsNegative;

    with StringGrid1 do
      begin
        if (aRow + 1) > RowCount then RowCount := aRow + 1;
        Cells[0, aRow] := IntToStr(aTrial+1);
        Cells[1, aRow] := aConsequence;
        Cells[2, aRow] := FormGo_NoGo.Schedule.Text;
        Cells[3, aRow] := IntToStr(FormGo_NoGo.SpinLimitedHold.Value);
        Cells[4, aRow] := ExtractFileName(FormGo_NoGo.Trials[aTrial].Path);
        Cells[5, aRow] := IntToStr(FormGo_NoGo.SpinSize.Value);
      end;
    Inc(aRow);
  end;

begin
  {
    Example:

    4 axis: 0, 45, 90, 135
    3 nodes per axis
    2 trials per node
    ______________________________

    Equals to a Group of 24 trials to repeat.

    It gives 96 trials repeating by 4.
    ______________________________

  }
  if piAxes.Checked then
  begin
    aRow := 1;
    FrmBresenhamLine := TBresenhamLineForm.Create(Application);
    if chkPlayOnSecondMonitor.Checked then
      FrmBresenhamLine.MonitorToShow := 1
    else
      FrmBresenhamLine.MonitorToShow := 0;

    if FrmBresenhamLine.ShowModal = mrOk then
      begin
        for aRepeat := 0 to FrmBresenhamLine.seRepeat.Value -1 do
          with FrmBresenhamLine.Axis do
            for aAxis := Low(List) to High(List) do
              begin
                cAngle := List[aAxis].Angle;
                cSize := IntToStr(List[aAxis].Size);
                for aNode := Low(List[aAxis].Line) to High(List[aAxis].Line) do
                  for aTrial := 0 to List[aAxis].TrialsPerNode[aNode] - 1 do AddAxesToGrid;
              end;

        //FNumTrials := aRow - 1;
        {$ifdef DEBUG}
          DebugLn(mt_Information + FrmBresenhamLine.ClassName +  ' instance returned ' + IntToStr(aRow - 1) + ' trials.');
        {$endif}
        FrmBresenhamLine.Free;
        ResetRepetionMatrix;
        FLastFocusedCol := -1;
      end
    else FrmBresenhamLine.Free;
  end;
  {
    Example:

    3 x 3 matriz = 9 positions
    2 trials per position
    ______________________________

    Equals to a Group of 18 trials to repeat.

    It gives 72 trials repeating it by 4.
    ______________________________

  }
  if piMatrix.Checked then
    begin
      aRow := 1;
      aCol := 0;
      FrmMatrix := TMatrixForm.Create(Application);
      if chkPlayOnSecondMonitor.Checked then
        FrmMatrix.MonitorToShow := 1
      else
        FrmMatrix.MonitorToShow := 0;

      if FrmMatrix.ShowModal = mrOk then
        begin
          for aTrial := Low(FrmMatrix.Trials) to High(FrmMatrix.Trials) do AddMatrixTrialToGrid;

          //FNumTrials := aRow - 1;
          {$ifdef DEBUG}
            DebugLn(mt_Information + FrmMatrix.ClassName + ' instance returned ' + IntToStr(aRow - 1) + ' trials.');
          {$endif}
          FrmMatrix.Free;
          ResetRepetionMatrix;
          FLastFocusedCol := -1;
        end
      else FrmMatrix.Free;
    end;

  if piGo_NoGo.Checked then
    begin
      aRow := 1;
      GGlobalContainer := TGlobalContainer.Create;
      FormGo_NoGo := TFormGo_NoGo.Create(Application);
      if chkPlayOnSecondMonitor.Checked then
        begin
          GGlobalContainer.MonitorToShow := 1;
          FormGo_NoGo.MonitorToShow := GGlobalContainer.MonitorToShow;
        end
      else
        begin
          GGlobalContainer.MonitorToShow := 0;
          FormGo_NoGo.MonitorToShow := GGlobalContainer.MonitorToShow;
        end;

      if FormGo_NoGo.ShowModal = mrOK then
        begin
          for aTrial := Low(FormGo_NoGo.Trials) to High(FormGo_NoGo.Trials) do AddGoNoGoTrialsToGrid;

          FormGo_NoGo.Free;
          ResetRepetionMatrix;
          FLastFocusedCol := -1;
        end;

      GGlobalContainer.Free;
    end;
end;

procedure TFormUserConfig.FormCreate(Sender: TObject);
begin
  FAudioDevice := TBassAudioDevice.Create(WindowHandle);
  FrmBackground := TBackground.Create(Application);
  FLastFocusedCol := -1;
  //StringGrid1.ColCount := 9;
  Caption := Application.Title;
  stAppTitle.Caption := Application.Title;
  stVersion.Caption := CurrentVersion(GetCommitTag(True));
  MemoAppInfo.Lines.Append(
  'Stimulus Control' + LineEnding +
  'Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.' + LineEnding + LineEnding +
  'The present software is distributed under the terms of the GNU General Public License (GPL v3.0).' + LineEnding + LineEnding +
  'You should have received a copy of the GNU General Public License' + LineEnding +
  'along with this program. If not, see <http://www.gnu.org/licenses/>.' + LineEnding + LineEnding +
  'Last Commit: ' + GetCommitTag(False).Text
  );

  //with StringGrid1 do
  //  begin
  //    Cells[0, 0] := rsTrials;
  //    Cells[1, 0] := rsAngle;
  //    Cells[2, 0] := 'x0';
  //    Cells[3, 0] := 'y0';
  //    Cells[4, 0] := 'x1';
  //    Cells[5, 0] := 'y1';
  //    Cells[6, 0] := rsExpectedResponse;
  //    Cells[7, 0] := rsSize;
  //    Cells[8, 0] := rsSchedule;
  //    //aRowCount := RowCount;
  //    //aColCount := ColCount;
  //  end;
  Randomize;
  ResetRepetionMatrix;

  FEscriba := TEscriba.Create(Application);
  FEscriba.Memo := Memo1;
  FEscriba.NumBlc := 1;
  FEscriba.SetLengthVetBlc;
end;

procedure TFormUserConfig.FormDestroy(Sender: TObject);
begin
  FAudioDevice.Free;
end;

procedure TFormUserConfig.btnRunClick(Sender: TObject);
var LDirectory : string;
begin
  LDirectory := GetCurrentDirUTF8 + PathDelim + leParticipant.Text;
  if ForceDirectoriesUTF8(LDirectory) then
      OpenDialog1.InitialDir := LDirectory;

  if OpenDialog1.Execute then
    begin
      FrmBackground.Show;
      FrmBackground.SetFullScreen(True);
      if chkPlayOnSecondMonitor.Checked then
         FrmBackground.Left := Screen.Width + 1;

      FSession := TSession.Create(nil);
      FConfigs := TCfgSes.Create(FSession);
      if chkPlayOnSecondMonitor.Checked then
        FConfigs.GlobalContainer.MonitorToShow := 1
      else
        FConfigs.GlobalContainer.MonitorToShow := 0;

      FConfigs.LoadFromFile(OpenDialog1.Filename, False);
      FConfigs.PupilEnabled := chkPupilClient.Checked;

      with FSession do
        begin
          OnEndSess:= @EndSession;
          AudioDevice := FAudioDevice;
          BackGround := FrmBackground;
          Configs := FConfigs;
          TestMode := False;
          ShowCounter := False;
          Play('000');
        end;
    end;

end;

procedure TFormUserConfig.btnSaveClick(Sender: TObject);
var aDirectory : string;
begin
  aDirectory := GetCurrentDirUTF8 + PathDelim + leParticipant.Text;
  if ForceDirectoriesUTF8(aDirectory) then
    SaveDialog1.InitialDir := aDirectory;

  if SaveDialog1.Execute then
    FEscriba.SaveMemoTextToTxt(SaveDialog1.FileName);
end;

procedure TFormUserConfig.chkDrawTrialGroupChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TFormUserConfig.chkShowRepetitionsChange(Sender: TObject);
begin
  ResetRepetionMatrix;
end;

procedure TFormUserConfig.btnClick(Sender: TObject);
begin
  OpenDialog1.InitialDir := GetCurrentDirUTF8;
  if OpenDialog1.Execute then
    begin
      if Sender = btnTargetFile then
        edtTarget.Text := OpenDialog1.FileName;
      if Sender = btnContentFile then
        edtContent.Text := OpenDialog1.FileName;
    end;
end;

end.


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
    , config_session_guiutils
    , escriba
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
    ColorButtonBloc: TColorButton;
    ComboBoxSessionType: TComboBox;
    ComboBoxBlocCounter: TComboBox;
    EditParticipant: TEdit;
    EditBlocName: TEdit;
    EditBlocChainingPath: TEdit;
    EditSessionName: TEdit;
    edtTrialGroup: TEdit;
    edtTarget: TEdit;
    edtContent: TEdit;
    gbRepetitions: TGroupBox;
    gbRepetitionsBlocks: TGroupBox;
    gbTrialGroup: TGroupBox;
    Image1: TImage;
    LabelParticipant: TLabel;
    LabelSessionName: TLabel;
    LabelSessionType: TLabel;
    LabelBlocBkGnd: TLabel;
    LabelBlocCounter: TLabel;
    LabelBlocCrtConsecutiveHit: TLabel;
    LabelBlocMaxCorrection: TLabel;
    LabelBlocMaxRepetition: TLabel;
    LabelBlocName: TLabel;
    LabelBlocCrtHitPorcentage: TLabel;
    LabelBlocITI: TLabel;
    LabelBlocChainingPath: TLabel;
    LabelBlocVirtualTrial: TLabel;
    leFillValue: TLabeledEdit;
    Memo1: TMemo;
    MemoAppInfo: TMemo;
    piMTS: TMenuItem;
    piBlocChaining: TMenuItem;
    piGo_NoGo: TMenuItem;
    piTrialsWithConstraints: TMenuItem;
    PanelHeader: TPanel;
    piFillEven: TMenuItem;
    piFillOdd: TMenuItem;
    piFillAll: TMenuItem;
    piFPE: TMenuItem;
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
    seBlocMaxRepetition: TSpinEdit;
    seBlocMaxCorrection: TSpinEdit;
    seCount: TSpinEdit;
    seBlocksCount: TSpinEdit;
    seBlocITI: TSpinEdit;
    seBlocCrtHitPorcentage: TSpinEdit;
    seBlocVirtualTrial: TSpinEdit;
    seBlocCrtConsecutiveHit: TSpinEdit;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    stAppTitle: TStaticText;
    stVersion: TStaticText;
    StringGrid1: TStringGrid;
    tbBlocs: TTabSheet;
    tbVersion: TTabSheet;
    tbTools: TTabSheet;
    tbSave: TTabSheet;
    tbGeneral: TTabSheet;
    tbStimuli: TTabSheet;
    tbTrials: TTabSheet;
    XMLPropStorage1: TXMLPropStorage;
    procedure btnCheckClick(Sender: TObject);
    procedure btnClientTestClick(Sender: TObject);
    procedure btnExportStimulusClick(Sender: TObject);
    procedure btnFillConditionClick(Sender: TObject);
    procedure btnGridTypeClick(Sender: TObject);
    procedure btnNextGeneralClick(Sender: TObject);
    procedure btnNextStimuliClick(Sender: TObject);
    procedure btnTrialsDoneClick(Sender: TObject);
    procedure btnRandomizeClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure chkDrawTrialGroupChange(Sender: TObject);
    procedure chkShowRepetitionsChange(Sender: TObject);
    procedure chkUseMediaChange(Sender: TObject);
    procedure EditBlocChainingPathDblClick(Sender: TObject);
    procedure EditBlocChainingPathEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImageDblClick(Sender: TObject);
    procedure piClick(Sender: TObject);
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
    FSession : TSession;
    FConfigs : TCfgSes;
    FAudioDevice : TBassAudioDevice;
    FEscriba : TEscriba;
    FGuiMain : TGuiMain;
    FGuiBloc : TGuiBloc;
    function MeetCondition(aCol, aRow : integer): boolean;
    function SaneConstraints: Boolean;
    function RepetitionBlocksMeetsConstraints(ALowRow,AHighRow:integer) : Boolean;
    procedure CheckRepetitionCol(aCol : integer);
    procedure EndSession(Sender : TObject);
    procedure RandTrialOrder(BeginRow, EndRow : integer);
    procedure ResetRepetionMatrix;
    procedure ReceiveTimestamp(Sender: TObject; ARequest, AResponse: String);
    procedure CreateFormBloc;
  public
    { public declarations }

  end;

var
  FormUserConfig: TFormUserConfig;

implementation

{$R *.lfm}

uses background, strutils
     , config_session_global_container
     , userconfigs_trial_mirrored
     , userconfigs_feature_positive
     , userconfigs_go_nogo
     , userconfigs_mts
     , userconfigs_blocs
     , userconfigs_positions
     , versioning_git
     , versioning_lazarus
     , constants
     {$ifdef DEBUG}
     , debug_logger
     {$endif}
     ;

const
  CSESSION_SERVER = '127.0.1.1:5020';

var
  LAST_BLOC_INIFILE_PATH : string;

{ TFormUserConfig }


procedure TFormUserConfig.ImageDblClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    TImage(Sender).Picture.LoadFromFile(OpenPictureDialog1.FileName);
end;

procedure TFormUserConfig.piClick(Sender: TObject);
begin
  if (TMenuItem(Sender) = piAxes) and (btnGridType.Caption <> rsFillTypeAxes) then
    begin
      StringGrid1.Clean;
      btnGridType.Caption := rsFillTypeAxes;
      SetGridHeader(StringGrid1,rsFillTypeAxes);
      ResetRepetionMatrix;
    end;

  if (TMenuItem(Sender) = piFPE) and (btnGridType.Caption <> rsFillTypeMatriz) then
    begin
      StringGrid1.Clean;
      btnGridType.Caption := rsFillTypeMatriz;
      SetGridHeader(StringGrid1,rsFillTypeMatriz);
      ResetRepetionMatrix;
    end;

  if (TMenuItem(Sender) = piGo_NoGo) and (btnGridType.Caption <> rsFillTypeGoNoGo) then
    begin
      StringGrid1.Clean;
      btnGridType.Caption := rsFillTypeGoNoGo;
      SetGridHeader(StringGrid1,rsFillTypeGoNoGo);
      ResetRepetionMatrix;
    end;

  if (TMenuItem(Sender) = piMTS) and (btnGridType.Caption <> rsFillTypeMTS) then
      begin
        StringGrid1.Clean;
        btnGridType.Caption := rsFillTypeMTS;
        SetGridHeader(StringGrid1,rsFillTypeMTS);
        ResetRepetionMatrix;
      end;

  if TMenuItem(Sender) = piBlocChaining then
    begin
      if btnGridType.Caption <> rsFillTypeBlocChaining then
        begin
          StringGrid1.Clean;
          btnGridType.Caption := rsFillTypeBlocChaining;
          SetGridHeader(StringGrid1,rsFillTypeBlocChaining);
          ResetRepetionMatrix;
          CreateFormBloc;
        end;
    end
  else
    if Assigned(FormBlocs) then
      begin
        FormBlocs.Free;
        FormBlocs := nil;
        StringGrid1.PopupMenu := nil
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
  if btnGridType.Caption = rsFillTypeBlocChaining then
    CreateFormBloc;
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

procedure TFormUserConfig.CreateFormBloc;
begin
  FormBlocs := TFormBlocs.Create(Application);
  FormBlocs.BlocsPath := EditBlocChainingPath.Text;
  StringGrid1.PopupMenu := FormBlocs.PopupMenuBlocs;
  FormBlocs.StringGrid := StringGrid1;
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

procedure TFormUserConfig.btnTrialsDoneClick(Sender: TObject);
var
  aTrial, aStm, aBlc, aNumTrials : integer;
  aValues : string;
  T : TCfgTrial;
  B : TCfgBlc;
  LDefaultMain : TStringList;
  LDefaultBloc : TStringList;
  LGuiMain : TGuiMain;
  LGuiBloc : TGuiBloc;

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

  procedure SetupDefaultsFromGui;
  begin
    LGuiMain.Blocs := StringGrid1;
    LGuiMain.Participant := EditParticipant;
    LGuiMain.SessionName := EditSessionName;
    LGuiMain.SessionType := ComboBoxSessionType;
    LGuiMain.PupilAddress := nil;
    LGuiMain.RootData := nil;
    LGuiMain.RootMedia := nil;

    LGuiBloc.BlocITI := seBlocITI;
    LGuiBloc.CrtConsecutiHits := seBlocCrtConsecutiveHit;
    LGuiBloc.CrtHitPorcentage := seBlocCrtHitPorcentage;
    LGuiBloc.Counter := ComboBoxBlocCounter;
    LGuiBloc.BlocName := EditBlocName;
    LGuiBloc.Color := ColorButtonBloc;
    LGuiBloc.MaxCorrection := seBlocMaxCorrection;
    LGuiBloc.MaxRepetition := seBlocMaxRepetition;
    LGuiBloc.Trials := nil;
    LGuiBloc.VirtualTrial := nil;
    LDefaultMain := TStringList.Create;
    LDefaultBloc := TStringList.Create;
    MainFromGui(LDefaultMain,LGuiMain);
    BlocFromGui(LDefaultBloc,LGuiBloc);
  end;

  procedure FreeDefaults;
  begin
    LDefaultMain.Free;
    LDefaultBloc.Free;
  end;

begin
  Memo1.Clear;
  aNumTrials := StringGrid1.RowCount -1;
  FEscriba.SessionServer := CSESSION_SERVER;
  if piAxes.Checked then
    begin
      aBlc := 0;

      FEscriba.SessionName := EditSessionName.Text;
      FEscriba.SessionSubject := EditParticipant.Text;
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

  if piFPE.Checked then
    begin
      aBlc := 0;

      FEscriba.SessionName := EditSessionName.Text;
      FEscriba.SessionSubject := EditParticipant.Text;
      FEscriba.Blcs[aBlc].ITI := 2300;
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
              Name := StringGrid1.Cells[1, aTrial + 1] + #32 + IntToStr(Id);

              SList.BeginUpdate;
              SList.Values[_BkGnd] := IntToStr(clWhite);
              SList.Values[_Cursor] := IntToStr(crNone);
              SList.Values[_ShowStarter] := BoolToStr(False, '1','0');
              SList.Values[_LimitedHold] := '1700';
              SList.Values[_Schedule] := 'CRF';
              SList.Values[_Contingency] := StringGrid1.Cells[1, aTrial + 1];
              SList.Values[_NextTrial] := '0';

              for aStm := 1 to NumComp do
                begin
                  SList.Values[_Comp + IntToStr(aStm) + _cBnd] := StringGrid1.Cells[aStm + 1 + NumComp, aTrial + 1];
                  aValues :=  StringGrid1.Cells[aStm + 1, aTrial + 1] + #32;
                  SList.Values[_Comp + IntToStr(aStm) + _cGap] := ExtractDelimited(1,aValues,[#32]);
                  SList.Values[_Comp + IntToStr(aStm) + _cGap_Degree] := ExtractDelimited(2,aValues,[#32]);
                  SList.Values[_Comp + IntToStr(aStm) + _cGap_Length] := ExtractDelimited(3,aValues,[#32]);
                end;
              SList.EndUpdate;
            end;
          FEscriba.Blcs[aBlc].Trials[aTrial] := T;
          FEscriba.SetTrial(aTrial);
        end;
    end;

  if piGo_NoGo.Checked then
    begin
      SetupDefaultsFromGui;
      FormGo_NoGo.WriteToDisk(LDefaultMain,LDefaultBloc,StringGrid1,LAST_BLOC_INIFILE_PATH);
      Memo1.Lines.LoadFromFile(LAST_BLOC_INIFILE_PATH);
      FreeDefaults;
    end;

  if piBlocChaining.Checked then
    begin
      SetupDefaultsFromGui;
      FormBlocs.WriteToDisk(LDefaultMain,LDefaultBloc);
      Memo1.Lines.LoadFromFile(FormBlocs.BlocsPath+DirectorySeparator+LAST_BLOCS_INI_FILENAME);
      FreeDefaults;
    end;

  pgRodar.TabIndex := 4;
end;

procedure TFormUserConfig.chkUseMediaChange(Sender: TObject);
begin
  //GUI to select custom images from media files was not implemented yet
  //Image1.Visible := chkUseMedia.Checked;
  //Image2.Visible := chkUseMedia.Checked
end;

procedure TFormUserConfig.EditBlocChainingPathDblClick(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
    EditBlocChainingPath.Text := SelectDirectoryDialog1.FileName;
end;

procedure TFormUserConfig.EditBlocChainingPathEditingDone(Sender: TObject);
begin
  if btnGridType.Caption = rsFillTypeBlocChaining then
    begin
      FormBlocs.BlocsPath := EditBlocChainingPath.Text;
      StringGrid1.PopupMenu := FormBlocs.PopupMenuBlocs;
    end;
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
    aContingency, aPosition : string;

  begin
    if FormFPE.Trials[aTrial].Positive then
      aContingency := rsPositive
    else
      aContingency := rsNegative;


    with StringGrid1 do
      begin
        if (aRow + 1) > RowCount then RowCount := aRow + 1;
        Cells[0, aRow] := IntToStr(aRow);    //Trial Number
        Cells[1, aRow] := aContingency;
        aCol := 2;

        LowComp := Low(FormFPE.Trials[aTrial].Comps);
        HighComp := High(FormFPE.Trials[aTrial].Comps);
        //ShowMessage(IntToStr(LowComp) + ' ' + IntToStr(HighComp));
        for aComp := LowComp to HighComp do
          begin
            if (aCol + 1) > ColCount then ColCount := aCol + 1;
            Cells[aCol, 0] := rsComparison + IntToStr(aComp + 1);
            Cells[aCol, aRow] := FormFPE.Trials[aTrial].Comps[aComp].Path;
            Inc(aCol);
          end;

        LowComp := Low(FormFPE.Trials[aTrial].Comps);
        HighComp := High(FormFPE.Trials[aTrial].Comps);
        for aComp := LowComp to HighComp do
          begin
            if (aCol + 1) > ColCount then ColCount := aCol + 1;
            Cells[aCol, 0] := rsComparison + IntToStr(aComp + 1) + rsPosition;
            aPosition := IntToStr(FormFPE.Trials[aTrial].Comps[aComp].Top) + #32 +
                         IntToStr(FormFPE.Trials[aTrial].Comps[aComp].Left) + #32 +
                         IntToStr(FormFPE.Trials[aTrial].Comps[aComp].Width) + #32 +
                         IntToStr(FormFPE.Trials[aTrial].Comps[aComp].Height);

            Cells[aCol, aRow] := aPosition;
            Inc(aCol);
          end;
        Inc(aRow);
      end;
  end;
begin
  GGlobalContainer := TGlobalContainer.Create;
  if chkPlayOnSecondMonitor.Checked and (Screen.MonitorCount > 1) then
    GGlobalContainer.MonitorToShow := 1
  else
    GGlobalContainer.MonitorToShow := 0;
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
    FrmBresenhamLine.MonitorToShow := GGlobalContainer.MonitorToShow;
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

  aRow := 1;
  if piFPE.Checked then
    begin
      aCol := 0;
      FormFPE := TFormFPE.Create(Application);
      FormFPE.MonitorToShow := GGlobalContainer.MonitorToShow;
      if FormFPE.ShowModal = mrOk then
        begin
          for aTrial := Low(FormFPE.Trials) to High(FormFPE.Trials) do AddMatrixTrialToGrid;

          //FNumTrials := aRow - 1;
          {$ifdef DEBUG}
            DebugLn(mt_Information + FormFPE.ClassName + ' instance returned ' + IntToStr(aRow - 1) + ' trials.');
          {$endif}
          FormFPE.Free;
          ResetRepetionMatrix;
          FLastFocusedCol := -1;
        end
      else FormFPE.Free;
    end;

  if piGo_NoGo.Checked then
    begin
      FormGo_NoGo := TFormGo_NoGo.Create(Application);
      FormGo_NoGo.MonitorToShow := GGlobalContainer.MonitorToShow;
      if FormGo_NoGo.ShowModal = mrOK then
        begin
          FormGo_NoGo.AddTrialsToGui(StringGrid1);
          FormGo_NoGo.Free;
          ResetRepetionMatrix;
          FLastFocusedCol := -1;
        end;
    end;

  if piMTS.Checked then
    begin
      FormMTS := TFormMTS.Create(Application);
      FormMTS.MonitorToShow := GGlobalContainer.MonitorToShow;
      if FormMTS.ShowModal = mrOK then
        begin
          FormMTS.AddTrialsToGui(StringGrid1);
          FormMTS.Free;
          ResetRepetionMatrix;
          FLastFocusedCol := -1;
        end;
    end;

  if piBlocChaining.Checked then
    begin
      if (EditBlocChainingPath.Text <> '') and DirectoryExistsUTF8(EditBlocChainingPath.Text) then
        begin
          FormBlocs.InvalidateGraph;
          if FormBlocs.ShowModal = mrOK then
            begin
              // May allow editing
              //if FormBlocs.Count > 0 then
              //  for aBloc := 0 to FormBlocs.Count-1 do
              //    FormBlocs.AppendBlocToStringGrid(aBloc);
              ResetRepetionMatrix;
              FLastFocusedCol := -1;
            end;
        end;
    end;
  GGlobalContainer.Free;
end;

procedure TFormUserConfig.FormCreate(Sender: TObject);
var
  LInitialDirectory,
  LGitCommit : string;
begin
  LInitialDirectory := ExtractFilePath(Application.ExeName);
  LAST_BLOC_INIFILE_PATH := LInitialDirectory+LAST_BLOC_INI_FILENAME;
  OpenDialog1.InitialDir := LInitialDirectory;
  SaveDialog1.InitialDir:=LInitialDirectory;
  SelectDirectoryDialog1.InitialDir := LInitialDirectory;

  LabelBlocName.Caption := rsBlocName;
  LabelBlocCrtHitPorcentage.Caption := rsBlocCrtHitPorcentage;
  LabelBlocCrtConsecutiveHit.Caption := rsBlocCrtConsecutiveHit;
  LabelBlocMaxRepetition.Caption := rsBlocMaxBlcRepetition;
  LabelBlocITI.Caption := rsBlocITI;
  LabelBlocBkGnd.Caption := rsBlocBkGnd;
  LabelBlocCounter.Caption := rsBlocCounter;
  LabelBlocVirtualTrial.Caption := rsBlocVirtualTrialValue;
  LabelBlocMaxCorrection.Caption := rsBlocMaxCorrection;
  //LabelBlocName.Caption := rsBlocNextBlocOnCriteria;
  //LabelBlocName.Caption := rsBlocNextBlocOnNotCriteria;

  FAudioDevice := TBassAudioDevice.Create(WindowHandle);
  FrmBackground := TBackground.Create(Application);
  FLastFocusedCol := -1;
  //StringGrid1.ColCount := 9;
  Caption := Application.Title;
  stAppTitle.Caption := Application.Title;
  try
    stVersion.Caption := CurrentVersion(GetCommitTag(True));
    LGitCommit := GetCommitTag(False).Text;
  except
    on E : Exception do
      begin
        stVersion.Caption := FileVersion;
        LGitCommit:='A git repository was not found inside the program''s folder.';
      end;
  end;

  MemoAppInfo.Lines.Append(
    'Stimulus Control' + LineEnding +
    'Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.' + LineEnding + LineEnding +
    'The present software is distributed under the terms of the GNU General Public License (GPL v3.0).' + LineEnding + LineEnding +
    'You should have received a copy of the GNU General Public License' + LineEnding +
    'along with this program. If not, see <http://www.gnu.org/licenses/>.' + LineEnding + LineEnding +
    'File Version: '+ FileVersion + LineEnding +
    'ZMQ Version: ' + ZMQVersion + LineEnding +
    'Last Commit: ' + LGitCommit
  );
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
  LDirectory := GetCurrentDirUTF8 + PathDelim + EditParticipant.Text;
  if ForceDirectoriesUTF8(LDirectory) then
      OpenDialog1.InitialDir := LDirectory;

  if OpenDialog1.Execute then
    begin
      //FormRandomizePositions := TFormRandomizePositions.Create(Self);
      //FormRandomizePositions.LoadPositionsFromFile(OpenDialog1.Filename,1);
      //FormRandomizePositions.ShowModal;
      FrmBackground.Show;
      FrmBackground.SetFullScreen(True);

      FSession := TSession.Create(FrmBackground);
      FConfigs := TCfgSes.Create(FSession);
      if chkPlayOnSecondMonitor.Checked and (Screen.MonitorCount > 1) then
        begin
          FConfigs.GlobalContainer.MonitorToShow := 1;
          FrmBackground.Left := Screen.Width + 1;
        end
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
  aDirectory := GetCurrentDirUTF8 + PathDelim + EditParticipant.Text;
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


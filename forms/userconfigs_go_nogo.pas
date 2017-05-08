{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit userconfigs_go_nogo;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Classes, SysUtils, Forms, Controls, Graphics,
     Dialogs, ExtCtrls, StdCtrls, Spin, ActnList, ExtDlgs, Grids, XMLPropStorage
     , response_key
     ;

type

    TTrial = record
      Id : integer;
      Positive : Boolean;
      Path : string;
    end;

    TTrials = array of TTrial;

    { TFormGo_NoGo }

    TFormGo_NoGo = class(TForm)
      btnClose: TButton;
      btnMinimizeTopTab: TButton;
      btnOk: TButton;
      ButtonStimuli: TButton;
      cbPreview: TCheckBox;
      LabelPresentations: TLabel;
      LabelPresentations1: TLabel;
      LabelPos: TLabel;
      LabelCountPos: TLabel;
      LabelNeg: TLabel;
      LabelCountNeg: TLabel;
      OpenPictureDialog: TOpenPictureDialog;
      gbStimuli: TGroupBox;
      LabelSize: TLabel;
      LabelLimitedHold: TLabel;
      Panel1: TPanel;
      PreviewTimer: TTimer;
      Schedule: TEdit;
      SpinPresentations: TSpinEdit;
      SpinSize: TSpinEdit;
      SpinLimitedHold: TSpinEdit;
      XMLPropStorage1: TXMLPropStorage;
      procedure btnMinimizeTopTabClick(Sender: TObject);
      procedure ButtonClick(Sender: TObject);
      procedure cbPreviewChange(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure FormKeyPress(Sender: TObject; var Key: char);
      procedure FormPaint(Sender: TObject);
      procedure PreviewTimerTimer(Sender: TObject);
      procedure SpinLimitedHoldEditingDone(Sender: TObject);
      procedure SpinSizeEditingDone(Sender: TObject);
      procedure XMLPropStorage1RestoreProperties(Sender: TObject);
    private
      FStimulus : TKey;
      FCurrentTrial : integer;
      FMonitor: integer;
      FTrials : TTrials;
      procedure SetMonitor(AValue: integer);
      procedure SetTrials(APositive, ANegative : TStringList);
    public
      procedure Resize; override;
      procedure AddTrialsToGui(ATrialGrid : TStringGrid);
      procedure WriteToDisk(ADefaultMainSection: TStrings; ADefaultBlocSection : TStrings;
        ATrialGrid : TStringGrid; AFilename : string);
      property Trials : TTrials read FTrials write FTrials;
      property MonitorToShow : integer read FMonitor write SetMonitor;
    end;

var
  FormGo_NoGo: TFormGo_NoGo;

implementation

{$R *.lfm}

uses LazFileUtils, config_session_fileutils, constants;

{ TFormGo_NoGo }

procedure TFormGo_NoGo.FormCreate(Sender: TObject);
begin
  ButtonStimuli.Caption := 'Adicionar'+LineEnding+'Estímulos';
  WindowState := wsFullScreen;
  Canvas.Pen.Width:=10;
  OpenPictureDialog.InitialDir := ExtractFilePath(Application.ExeName);
  FCurrentTrial := 0;
  FStimulus := TKey.Create(Self);
  with FStimulus do
    begin
      ShowHint := True;
      Width := SpinSize.Value;
      Height:= SpinSize.Value;
      Edge:= clBlack;
      Color := clWhite; //clWhite
      //Loops:= StrToIntDef(LLoop, 0);
      //FullPath:= LName;
      //Schedule.Kind:= CfgTrial.SList.Values[_Schedule];
      Visible := True;
      Parent := Self;
    end;
end;

procedure TFormGo_NoGo.FormKeyPress(Sender: TObject; var Key: char);
begin
  if key in [#32] then
    Panel1.Visible := not Panel1.Visible;
end;

procedure TFormGo_NoGo.FormPaint(Sender: TObject);
var
  R : TRect;
begin
  if Length(FTrials) > 0 then
    begin
      R := FStimulus.BoundsRect;
      if InflateRect(R,10,10) then
        begin
          if FTrials[FCurrentTrial].Positive then
            Canvas.Pen.Color:=clGreen
          else
            Canvas.Pen.Color:=clRed;
          Canvas.Rectangle(R);
        end;
    end;
end;

procedure TFormGo_NoGo.PreviewTimerTimer(Sender: TObject);
begin
  if FCurrentTrial < High(Trials) then
    Inc(FCurrentTrial)
  else FCurrentTrial := 0;

  if Length(FTrials) > 0 then
    begin
      FStimulus.FullPath := FTrials[FCurrentTrial].Path;
      FStimulus.Invalidate;
    end;
  Invalidate;
end;

procedure TFormGo_NoGo.SpinLimitedHoldEditingDone(Sender: TObject);
begin
  PreviewTimer.Interval := SpinLimitedHold.Value;
end;

procedure TFormGo_NoGo.SpinSizeEditingDone(Sender: TObject);
begin
  with FStimulus do
    begin
      Width := SpinSize.Value;
      Height := SpinSize.Value;
      Centralize;
    end;
end;

procedure TFormGo_NoGo.XMLPropStorage1RestoreProperties(Sender: TObject);
begin
  SpinSizeEditingDone(Sender);
end;

procedure TFormGo_NoGo.SetMonitor(AValue: integer);
begin
  if FMonitor = AValue then Exit;
  FMonitor := AValue;
  Left := Screen.Monitors[FMonitor].Left;
end;

procedure TFormGo_NoGo.SetTrials(APositive, ANegative: TStringList);
var
  i: Integer;
begin
  SetLength(FTrials, APositive.Count+ANegative.Count);
  if APositive.Count > 0 then
    for i := 0 to APositive.Count -1 do
      begin
        FTrials[i].Id := 0;
        FTrials[i].Path := APositive[i];
        FTrials[i].Positive:=True;
      end;

  if ANegative.Count > 0 then
    for i := 0 to ANegative.Count -1 do
      begin
        FTrials[APositive.Count+i].Id := 0;
        FTrials[APositive.Count+i].Path := ANegative[i];
        FTrials[APositive.Count+i].Positive:=False;
      end;
end;

procedure TFormGo_NoGo.btnMinimizeTopTabClick(Sender: TObject);
begin
  Panel1.Visible := not Panel1.Visible;
end;

procedure TFormGo_NoGo.ButtonClick(Sender: TObject);
var
  LPositive, LNegative : TStringList;
  i: Integer;
begin
  LPositive := TStringList.Create;
  LNegative := TStringList.Create;
  OpenPictureDialog.Title:='Escolhas as figuras positivas';
  if OpenPictureDialog.Execute then
    if OpenPictureDialog.Files.Count > 0 then
      for i := 0 to (OpenPictureDialog.Files.Count - 1) do
        LPositive.Append(OpenPictureDialog.Files.Strings[i]);

  OpenPictureDialog.Title:='Escolhas as figuras negativas';
  if OpenPictureDialog.Execute then
    if OpenPictureDialog.Files.Count > 0 then
      for i := 0 to (OpenPictureDialog.Files.Count - 1) do
        LNegative.Append(OpenPictureDialog.Files.Strings[i]);

  SetTrials(LPositive,LNegative);
  LabelCountPos.Caption := IntToStr(LPositive.Count);
  LabelCountNeg.Caption := IntToStr(LNegative.Count);
  LPositive.Free;
  LNegative.Free;
end;

procedure TFormGo_NoGo.cbPreviewChange(Sender: TObject);
begin
  PreviewTimer.Enabled := not PreviewTimer.Enabled;
  // tkey visible
end;

procedure TFormGo_NoGo.Resize;
begin
  inherited Resize;
  if Assigned(FStimulus) then
    FStimulus.Centralize;
end;

procedure TFormGo_NoGo.AddTrialsToGui(ATrialGrid: TStringGrid);
var
  LRow, i, j: Integer;
  LConsequence : string;
begin
  ATrialGrid.RowCount := 2;
  LRow := 1;
  for i := Low(Trials) to High(Trials) do
    begin
      if Trials[i].Positive then
        LConsequence := rsPositive
      else
        LConsequence := rsNegative;

      for j := 0 to SpinPresentations.Value-1 do
        begin
          with ATrialGrid do
            begin
              if (LRow + 1) > RowCount then RowCount := LRow + 1;
              Cells[0, LRow] := IntToStr(LRow);
              Cells[1, LRow] := LConsequence;
              Cells[2, LRow] := Schedule.Text;
              Cells[3, LRow] := IntToStr(SpinLimitedHold.Value);
              Cells[4, LRow] := ExtractFileName(Trials[i].Path);
              Cells[5, LRow] := IntToStr(SpinSize.Value);
            end;
          Inc(LRow);
        end;
    end;
end;

procedure TFormGo_NoGo.WriteToDisk(ADefaultMainSection: TStrings;
  ADefaultBlocSection: TStrings; ATrialGrid: TStringGrid; AFilename: string);
var
  LRow : integer;
  FNewBloc : TConfigurationFile;
begin
  if FileExistsUTF8(AFilename) then
    DeleteFileUTF8(AFilename);
  FNewBloc := TConfigurationFile.Create(AFilename);
  FNewBloc.CacheUpdates:=True;
  FNewBloc.WriteMain(ADefaultMainSection);
  FNewBloc.WriteBlocIfEmpty(1,ADefaultBlocSection);
  with ATrialGrid do
    for LRow := 1 to RowCount-1 do
      begin
          FNewBloc.WriteToTrial(LRow, _Kind, T_GNG);
          FNewBloc.WriteToTrial(LRow, _Name,           Cells[1, LRow] + #32 + IntToStr(LRow));
          FNewBloc.WriteToTrial(LRow, _Consequence,    Cells[1, LRow]);
          FNewBloc.WriteToTrial(LRow, _Schedule,       Cells[2, LRow]);
          FNewBloc.WriteToTrial(LRow, _LimitedHold,    Cells[3, LRow]);
          FNewBloc.WriteToTrial(LRow, _Comp+'1'+_cStm, Cells[4, LRow]);   // configure the IETConsenquence
          FNewBloc.WriteToTrial(LRow, _Comp+'1'+_cBnd, Cells[5, LRow]);   // configure the IETConsenquence
      end;

  // update numblc and numtrials
  FNewBloc.Invalidate;

  // Save changes to disk
  FNewBloc.UpdateFile;
  FNewBloc.Free;
end;


end.


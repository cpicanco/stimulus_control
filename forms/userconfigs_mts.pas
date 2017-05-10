{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit userconfigs_mts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IDEWindowIntf, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Spin, Grids, XMLPropStorage
  , config_session_gui_comparison
  , response_key
  ;

type

  TStimulus = record
    List : TStringList;
    Key : TKey;
  end;

  TSimpleMTSTrial = class(TComponent)
  private

  public
    Sample : TStimulus;
    Comparisons : array of TStimulus;
  end;

  { TFormMTS }

  TFormMTS = class(TForm)
    btnClose: TButton;
    btnMinimizeTopTab: TButton;
    btnOk: TButton;
    EditDefaultCsqHIT: TEdit;
    EditDefaultCsqMISS: TEdit;
    gbStimuli: TGroupBox;
    LabelDefaultCsqHIT: TLabel;
    LabelDefaultCsqMISS: TLabel;
    LabelDefaultCursor: TLabel;
    LabelPresentations: TLabel;
    LabelComparisons: TLabel;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    SpinCursor: TSpinEdit;
    SpinPresentations: TSpinEdit;
    SpinComparisons: TSpinEdit;
    XMLPropStorage1: TXMLPropStorage;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpinComparisonsEditingDone(Sender: TObject);
    procedure SpinCursorEditingDone(Sender: TObject);
    procedure XMLPropStorage1RestoreProperties(Sender: TObject);
  private
    FMonitor: integer;
    FTrial : TSimpleMTSTRial;
    FSample : TGUIKeySettings;
    FComparison : TGUIKeySettings;
    procedure SetMonitor(AValue: integer);
    procedure UpdateComparisons(ATrial : TSimpleMTSTrial);
    procedure SetDefaultsToComparison(AList : TStrings; ATag : integer);
    procedure SetKeyPosition(AKeyUp, AKeyDown : TKey);
    procedure ShowKeySettings(Sender : TObject);
    procedure KeyClick(Sender : TObject);
    procedure AllEditsDone(Sender : TObject);
  public
    procedure AddTrialsToGui(ATrialGrid : TStringGrid);
    procedure WriteToDisk(ADefaultMainSection: TStrings; ADefaultBlocSection : TStrings;
            ATrialGrid : TStringGrid; AFilename : string);
    property MonitorToShow : integer read FMonitor write SetMonitor;
  end;

var
  FormMTS: TFormMTS;

implementation

{$R *.lfm}

uses constants, config_session_fileutils, LazFileUtils;

{ TFormMTS }

procedure TFormMTS.FormCreate(Sender: TObject);
begin
  OpenDialog.InitialDir := ExtractFilePath(Application.ExeName);
  OnClick:=@KeyClick;
  FTrial := TSimpleMTSTrial.Create(Self);
  FTrial.Sample.List := TStringList.Create;
  FTrial.Sample.Key := TKey.Create(Self);
  with FTrial.Sample.Key do
    begin
      Caption:=rsSample;
      ShowHint:=True;
      Hint := Caption;
      Top:= 300;
      Left:=50;
      OnClick:=@KeyClick;
      Width:=300;
      Height:=300;
      Tag := 0;
      Color:=clDkGray;
      Edge:=clBlack;
      Parent := Self;
      Show;
    end;

  UpdateComparisons(FTrial);

  FSample := TGUIKeySettings.Create(Self,True);
  FSample.OnShow:=@ShowKeySettings;
  FSample.OpenDialog := OpenDialog;
  FSample.Top := 300;
  FSample.Left:= 300;
  FSample.Parent := Self;
  FSample.Hide;

  FComparison := TGUIKeySettings.Create(Self);
  FComparison.OnShow:=@ShowKeySettings;;
  FComparison.OpenDialog := OpenDialog;
  FComparison.Top := 300;
  FComparison.Left:= FComparison.Width + FSample.Left + 20;
  FComparison.Parent := Self;
  FComparison.Hide;
end;

procedure TFormMTS.FormActivate(Sender: TObject);
begin
  WindowState:=wsMaximized;
end;

procedure TFormMTS.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  FTrial.Sample.List.Free;
  for i := 0 to High(FTrial.Comparisons) do
    FTrial.Comparisons[i].List.Free;
end;

procedure TFormMTS.SpinComparisonsEditingDone(Sender: TObject);
begin
  UpdateComparisons(FTrial);
end;

procedure TFormMTS.SpinCursorEditingDone(Sender: TObject);
begin
  if Sender is TSpinEdit then
    LabelDefaultCursor.Cursor := TSpinEdit(Sender).Value;
end;

procedure TFormMTS.XMLPropStorage1RestoreProperties(Sender: TObject);
begin
  UpdateComparisons(FTrial);
end;

procedure TFormMTS.UpdateComparisons(ATrial: TSimpleMTSTrial);
var
  LComparisons : integer;
begin
  with ATrial do
    begin
      LComparisons := High(Comparisons);
      while (LComparisons+1) > SpinComparisons.Value do
        begin
          if Assigned(Comparisons[LComparisons].List) then
            Comparisons[LComparisons].List.Free;

          if Assigned(Comparisons[LComparisons].Key) then
            Comparisons[LComparisons].Key.Free;

          SetLength(FTrial.Comparisons,LComparisons);
          LComparisons := High(Comparisons);
        end;

      while (LComparisons+1) < SpinComparisons.Value do
        begin
          SetLength(FTrial.Comparisons,LComparisons+2);
          LComparisons := High(Comparisons);
          Comparisons[LComparisons].List := TStringList.Create;
          Comparisons[LComparisons].Key := TKey.Create(Self);
          Comparisons[LComparisons].Key.Tag := LComparisons+1;
          SetDefaultsToComparison(Comparisons[LComparisons].List, Comparisons[LComparisons].Key.Tag);
          if LComparisons = 0 then
            SetKeyPosition(nil, Comparisons[LComparisons].Key)
          else
            if LComparisons > 0 then
              SetKeyPosition(Comparisons[LComparisons-1].Key, Comparisons[LComparisons].Key)
        end;

    end;
end;

procedure TFormMTS.SetDefaultsToComparison(AList: TStrings; ATag: integer);
begin
  if ATag = 1 then
    begin
      AList.Values[_Comp+IntToStr(ATag)+_cIET] := EditDefaultCsqHIT.Text;
      AList.Values[_Comp+IntToStr(ATag)+_cRes] := T_HIT;
    end
  else
    begin
      AList.Values[_Comp+IntToStr(ATag)+_cIET] := EditDefaultCsqMISS.Text;
      AList.Values[_Comp+IntToStr(ATag)+_cRes] := T_MISS;
    end;
end;

procedure TFormMTS.SetMonitor(AValue: integer);
begin
  if FMonitor=AValue then Exit;
  FMonitor:=AValue;
end;

procedure TFormMTS.SetKeyPosition(AKeyUp, AKeyDown: TKey);
begin
  with AKeyDown do
    begin
      Left := FTrial.Sample.Key.Left + FTrial.Sample.Key.Width + 50;
      ShowHint:=True;
      Width := 300;
      Height := 300;
      Edge:= clBlack;
      OnClick:=@KeyClick;
      if Tag = 1 then
        begin
          Color := clGreen;
          Caption := 'S+';
          Hint := rsComparison+IntToStr(Tag)+#32+'(S+)';
        end
      else
        begin
          Color := clRed;
          Caption := 'S-';
          Hint := rsComparison+IntToStr(Tag)+#32+'(S-)';
        end;
    end;

  if AKeyUp = nil then
    AKeyDown.Top:=200
  else
    AKeyDown.Top:=AKeyUp.Top+AKeyUp.Height+20;
  AKeyDown.Parent := Self;
  AKeyDown.Show;
end;

procedure TFormMTS.ShowKeySettings(Sender: TObject);
begin
  if Sender = FSample then
    FComparison.Hide;

  if Sender = FComparison then
    FSample.Hide;
end;

procedure TFormMTS.KeyClick(Sender: TObject);
var
  ATag : integer;
begin
  if Sender is TKey then
    begin
      ATag := TKey(Sender).Tag;
      if ATag = 0 then
        begin
          FSample.Top:=TKey(Sender).Top;
          FSample.Left:=TKey(Sender).BoundsRect.Right+25;
          FSample.ResponseKey := FTrial.Sample.Key;
          FSample.ConfigList := FTrial.Sample.List;
          FSample.BringToFront;
          FSample.Show;
        end;

      if ATag > 0 then
        begin
          FComparison.Top:=TKey(Sender).Top;
          FComparison.Left:=TKey(Sender).BoundsRect.Right+25;
          FComparison.ConfigList := FTrial.Comparisons[ATag-1].List;
          FComparison.Tag:=ATag;
          FComparison.ResponseKey := TKey(Sender);//FTrial.Comparisons[ATag-1].Key;
          FComparison.BringToFront;
          FComparison.Show;
        end;
    end;

  if Sender is TFormMTS then
    begin
      FSample.Hide;
      FComparison.Hide;
    end;
end;

procedure TFormMTS.AllEditsDone(Sender: TObject);
begin
  TEdit(Sender).Text := TEdit(Sender).Text + ' 0 0 1000';
end;

procedure TFormMTS.AddTrialsToGui(ATrialGrid: TStringGrid);
var
  LRow, i: Integer;
begin
  with ATrialGrid do
    begin
      if (RowCount = 2) and (Cells[0,1] = '') then
        LRow := RowCount-1
      else
        LRow := RowCount;
      for i := 0 to SpinPresentations.Value-1 do
        begin
          if LRow >= RowCount then
            RowCount := LRow + 1;

          Cells[0, LRow] := IntToStr(LRow);
          Objects[0, LRow] := FTrial;
          Cells[1, LRow] := FTrial.Sample.Key.ShortName+'->'+FTrial.Comparisons[0].Key.ShortName;
          Cells[2, LRow] := FTrial.Comparisons[0].List.Values[_Comp+'1'+_cRes];
          Inc(LRow);
        end;
    end;
end;

procedure TFormMTS.WriteToDisk(ADefaultMainSection: TStrings;
  ADefaultBlocSection: TStrings; ATrialGrid: TStringGrid; AFilename: string);
var
  LRow , i: integer;
  FNewBloc : TConfigurationFile;
  LTrial : TSimpleMTSTrial;
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
        LTrial := TSimpleMTSTrial(Objects[0,LRow]);
        with LTrial do
          begin
            FNewBloc.WriteToTrial(LRow,_Kind, T_MTS);
            FNewBloc.WriteToTrial(LRow,_Name, Cells[1, LRow]);
            FNewBloc.WriteToTrial(LRow, _Cursor, IntToStr(SpinCursor.Value));
            FNewBloc.WriteToTrial(LRow, Sample.List);
            FNewBloc.WriteToTrial(LRow,_NumComp,IntToStr(Length(Comparisons)));
            for i := Low(Comparisons) to High(Comparisons) do
              FNewBloc.WriteToTrial(LRow, Comparisons[i].List);
          end;
      end;

  // update numblc and numtrials
  FNewBloc.Invalidate;

  // Save changes to disk
  FNewBloc.UpdateFile;
  FNewBloc.Free;

end;

end.


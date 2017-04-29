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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, Grids, ExtDlgs
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
    cbPreview: TCheckBox;
    EditDefaultCsqHit: TEdit;
    EditDefaultCsqMISS: TEdit;
    gbStimuli: TGroupBox;
    LabelDefaultCsqHIT: TLabel;
    LabelDefaultCsqMISS: TLabel;
    LabelPresentations: TLabel;
    LabelComparisons: TLabel;
    LabelSize: TLabel;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    SpinPresentations: TSpinEdit;
    SpinComparisons: TSpinEdit;
    SpinSize: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpinComparisonsEditingDone(Sender: TObject);
  private
    FMonitor: integer;
    FTrial : TSimpleMTSTRial;
    FSample : TGUIKeySettings;
    FComparison : TGUIKeySettings;
    procedure SetMonitor(AValue: integer);
    procedure UpdateComparisons(ATrial : TSimpleMTSTrial);
    procedure SetKeyPosition(AKeyUp, AKeyDown : TKey);
    procedure ShowKeySettings(Sender : TObject);
    procedure KeyClick(Sender : TObject);
  public
    procedure AddTrialsToGui(AStringGrid : TStringGrid);
    procedure WriteToDisk(ADefaultMainSection: TStrings; ADefaultBlocSection : TStrings;
            ATrialGrid : TStringGrid; AFilename : string);
    property MonitorToShow : integer read FMonitor write SetMonitor;
  end;

var
  FormMTS: TFormMTS;

implementation

{$R *.lfm}

uses constants;

{ TFormMTS }

procedure TFormMTS.FormCreate(Sender: TObject);
begin
  OpenDialog.InitialDir := ExtractFilePath(Application.ExeName);
  OnClick:=@KeyClick;
  WindowState:=wsFullScreen;
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
      Width:=SpinSize.Value;
      Height:=SpinSize.Value;
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
          if LComparisons = 0 then
            SetKeyPosition(nil, Comparisons[LComparisons].Key)
          else
            if LComparisons > 0 then
              SetKeyPosition(Comparisons[LComparisons-1].Key, Comparisons[LComparisons].Key)
        end;

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
      Width:=SpinSize.Value;
      Height:=SpinSize.Value;
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
          FSample.Show;
        end;

      if ATag > 0 then
        begin
          FComparison.Top:=TKey(Sender).Top;
          FComparison.Left:=TKey(Sender).BoundsRect.Right+25;
          FComparison.ConfigList := FTrial.Comparisons[ATag-1].List;
          FComparison.Tag:=ATag;
          FComparison.ResponseKey := TKey(Sender);//FTrial.Comparisons[ATag-1].Key;
          FComparison.Show;
        end;
    end;

  if Sender is TFormMTS then
    begin
      FSample.Hide;
      FComparison.Hide;
    end;
end;

procedure TFormMTS.AddTrialsToGui(AStringGrid: TStringGrid);
begin

end;

procedure TFormMTS.WriteToDisk(ADefaultMainSection: TStrings;
  ADefaultBlocSection: TStrings; ATrialGrid: TStringGrid; AFilename: string);
begin

end;

end.


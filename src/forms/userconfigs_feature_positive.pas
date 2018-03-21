{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit userconfigs_feature_positive;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, FileUtil, IDEWindowIntf, Forms, Controls, Graphics,
     Dialogs, ExtCtrls, StdCtrls, Spin, ActnList, Grids, XMLPropStorage
     , GUI.Helpers.Grids
     , Canvas.Helpers
     ;

type

    { TTrial }
    TComparison = record
        Top : integer;
        Left : integer;
        Width : integer;
        Height : integer;
        Path : string;
    end;


    TStmCircleGrid = array of TComparison;

    TTrial = record
        Id : integer;
        Positive : Boolean;
        CenterStyle: string;
        Comps : array of TComparison;
    end;

    TTrials = array of TTrial;

    { TFormFPE }

    TFormFPE = class(TForm)
      btnClose: TButton;
      btnMinimizeTopTab: TButton;
      btnOk: TButton;
      gbLimitedHold: TGroupBox;
      gbTrials: TGroupBox;
      LabelGapLength: TLabel;
      LabelLimitedHold: TLabel;
      LabelStimuliNumber: TLabel;
      LabelSize: TLabel;
      LabelBorder: TLabel;
      LabelTrials: TLabel;
      Panel1: TPanel;
      RadioGroupEffect: TRadioGroup;
      seGapLength: TSpinEdit;
      seLimitedHold: TSpinEdit;
      seSize: TSpinEdit;
      seBorder: TSpinEdit;
      seStimuliNumber: TSpinEdit;
      seTrials: TSpinEdit;
      PreviewTimer: TTimer;
      XMLPropStorage1: TXMLPropStorage;
      procedure btnEditNodes1Click(Sender: TObject);
      procedure btnMinimizeTopTabClick(Sender: TObject);
      procedure Button2Click(Sender: TObject);
      procedure cbPreviewChange(Sender: TObject);
      procedure FormActivate(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure FormKeyPress(Sender: TObject; var Key: char);
      procedure FormPaint(Sender: TObject);
      procedure PreviewTimerTimer(Sender: TObject);
      procedure RadioGroupEffectSelectionChanged(Sender: TObject);
      procedure seGapLengthEditingDone(Sender: TObject);
    private
    // fullscreen
      //FFullScreen : Boolean;
      //FOriginalBounds : TRect;
      //FOriginalWindowState : TWindowState;
      //FScreenBounds : TRect;
    // other
      FDrawMask : Boolean;
      FCanDraw : Boolean;
      FCurrentTrial : integer;
      FMonitor: integer;
      FTrials : TTrials;
      FBitmap : TBitmap;
      function GetCircleGrid(AMonitor : integer) : TStmCircleGrid;
      procedure SetMonitor(AValue: integer);
    public
      procedure AddTrialsToGrid(ATrialGrid : TStringGrid);
      procedure SetCircleGrid(AStmCircleGrid : TStmCircleGrid);
      procedure SetCircleGridMixed(AStmCircleGrid : TStmCircleGrid);
      procedure WriteToDisk(ADefaultMainSection: TStrings; ADefaultBlocSection : TStrings;
        ATrialGrid : TStringGrid; AFilename : string);
      property MonitorToShow : integer read FMonitor write SetMonitor;
    end;

var
  FormFPE: TFormFPE;

resourcestring
  rsPositive = 'Positiva';
  rsNegative = 'Negativa';
  rsComparison = 'C';
  rsPosition = 'Bnd';

implementation

{$R *.lfm}

uses LazFileUtils, Session.ConfigurationFile, strutils, constants;
{ TFormFPE }

procedure TFormFPE.FormActivate(Sender: TObject);
begin
  //BorderStyle := bsNone;
  FCurrentTrial := 0;
  WindowState:=wsMaximized;
  case RadioGroupEffect.ItemIndex of
    0..1 : SetCircleGrid(GetCircleGrid(MonitorToShow));
    2 : SetCircleGridMixed(GetCircleGrid(MonitorToShow));
  end;
end;

procedure TFormFPE.FormCreate(Sender: TObject);
begin
  FBitmap := TBitmap.Create;
  RandomMask(FBitmap);
  FDrawMask := True;
end;

procedure TFormFPE.FormDestroy(Sender: TObject);
begin
  FBitmap.Free;
end;

procedure TFormFPE.FormKeyPress(Sender: TObject; var Key: char);
begin
  if key in [#32] then
    Panel1.Visible := not Panel1.Visible;
end;

procedure TFormFPE.FormPaint(Sender: TObject);
var
  i : integer;
  OldCanvas : TCanvas;
  aWidth, aHeight : integer;
  aR : TRect;
  aGapValues : string;
  aGapDegree : integer;
  aGap : Boolean = False;
  aGapLength : integer;
  LX, LY: Int64;
  LSize: LongInt;

  procedure NextValue(var S : string);
  begin
    Delete( S, 1, pos( #32, S ) );
    if Length( S ) > 0 then
      while S[1] = #32 do
        Delete( S, 1, 1 );
  end;

  procedure SaveOldCanvas;
  begin
    OldCanvas.Brush.Color := Canvas.Brush.Color;
    OldCanvas.Brush.Style := Canvas.Brush.Style;

    OldCanvas.Pen.Color := Canvas.Pen.Color;
    OldCanvas.Pen.Style := Canvas.Pen.Style;
    OldCanvas.Pen.Mode := Canvas.Pen.Mode;
  end;

  procedure LoadOldCanvas;
  begin
    Canvas.Brush.Color := OldCanvas.Brush.Color;
    Canvas.Brush.Style := OldCanvas.Brush.Style;

    Canvas.Pen.Color := OldCanvas.Pen.Color;
    Canvas.Pen.Style := OldCanvas.Pen.Style;
    Canvas.Pen.Mode := OldCanvas.Pen.Mode;
  end;

begin
  if FCanDraw then
    begin
      OldCanvas := TCanvas.Create;
      SaveOldCanvas;
      if FDrawMask then
        Canvas.StretchDraw(ClientRect,FBitmap);
      LoadOldCanvas;

      try
        //DrawCircle(Canvas, 300, 300, 100, True, 50, 5 );

        for i := Low(FTrials[FCurrentTrial].Comps) to Length(FTrials[FCurrentTrial].Comps)-1 do
        begin
          aR.Left := FTrials[FCurrentTrial].Comps[i].Left;
          aR.Top := FTrials[FCurrentTrial].Comps[i].Top;
          aWidth := FTrials[FCurrentTrial].Comps[i].Width;
          aR.Right := aR.Left + aWidth;
          aHeight := FTrials[FCurrentTrial].Comps[i].Height;
          aR.Bottom := aR.Top + aHeight;

          aGapValues := FTrials[FCurrentTrial].Comps[i].Path;
          //WriteLn(aGapValues);
          aGap := StrToBoolDef(Copy(aGapValues,0,pos(#32,aGapValues)-1),False);
          NextValue(aGapValues);

          aGapDegree := 16*StrToIntDef(Copy(aGapValues,0,pos(#32,aGapValues)-1),1);
          NextValue(aGapValues);

          aGapLength := 16*(360-StrToIntDef(Copy(aGapValues,0,pos(#32,aGapValues)-1),360));
          DrawCustomEllipse(Canvas,AR,aGapDegree,aGapLength);
          LX := Round(Width / 2);
          LY := Round(Height / 2);
          LSize := (AR.Right-AR.Left) div 2;
          case UpperCase(FTrials[FCurrentTrial].CenterStyle) of
            'O' :
              with Canvas do
              begin
                Brush.Style:= bsClear;
                Pen.Mode := pmBlack;
                Pen.Width := 5;
                Rectangle(LX - LSize, LY - LSize, LX + LSize, LY + LSize);
              end;

            'X' :
              with Canvas do
              begin
                Brush.Style:= bsSolid;
                Pen.Mode := pmBlack;
                Pen.Width := 5;
                Line(LX - LSize, LY - LSize, LX + LSize, LY + LSize);
                Line(LX + LSize, LY - LSize, LX - LSize, LY + LSize);
              end;
            else { do nothing };
          end;
        end;
        Canvas.TextOut(FTrials[i].Comps[0].Left - 10, FTrials[i].Comps[0].Top - 10 , IntToStr(FCurrentTrial + 1));
        Canvas.TextOut(FTrials[i].Comps[0].Left - 20, FTrials[i].Comps[0].Top - 30 , BoolToStr(FTrials[i].Positive, 'Positive', 'Negative'));
      finally
        LoadOldCanvas;
        OldCanvas.Free;
      end;
    end;
end;

procedure TFormFPE.PreviewTimerTimer(Sender: TObject);
begin
  if FCurrentTrial < High(FTrials) then
    Inc(FCurrentTrial)
  else FCurrentTrial := 0;
  Invalidate;
end;

procedure TFormFPE.RadioGroupEffectSelectionChanged(Sender: TObject);
begin
  case RadioGroupEffect.ItemIndex of
    0..1 : SetCircleGrid(GetCircleGrid(MonitorToShow));
    2    : SetCircleGridMixed(GetCircleGrid(MonitorToShow));
  end;
end;

procedure TFormFPE.seGapLengthEditingDone(Sender: TObject);
begin
  case RadioGroupEffect.ItemIndex of
    0..1 : SetCircleGrid(GetCircleGrid(MonitorToShow));
    2    : SetCircleGridMixed(GetCircleGrid(MonitorToShow));
  end;
end;

function TFormFPE.GetCircleGrid(AMonitor: integer): TStmCircleGrid;
var
  LP : TPoint;
  LR : TRect;
  LW, LH,
  LSLeft, LSTop, LSCount, i, LSSize, LBorderSize: Integer;
begin
  LBorderSize := seBorder.Value;
  LSSize := seSize.Value;
  LW := Screen.Monitors[AMonitor].Width;
  LH := Screen.Monitors[AMonitor].Height;
  //LR := GetCentralRect(LW,LH,(LSSize+LBorderSize) div 2);
  LR := GetCentralRect(LW, LH, LBorderSize, LBorderSize, LBorderSize, LBorderSize);

  LSCount := seStimuliNumber.Value;
  SetLength(Result,LSCount);
  for i := 0 to LSCount -1 do
    begin
      LP := GetPointFromAngle(i*(360/LSCount),LR);
      LSLeft:= LP.X- (LSSize div 2);
      LSTop := LP.Y- (LSSize div 2);
      Result[i].Left := LSLeft;
      Result[i].Top := LSTop;
      Result[i].Width := LSSize;
      Result[i].Height := LSSize;
    end;
end;

procedure TFormFPE.SetMonitor(AValue: integer);
begin
  if FMonitor = AValue then Exit;
  FMonitor := AValue;
  Left := Screen.Monitors[FMonitor].Left;
end;

procedure TFormFPE.AddTrialsToGrid(ATrialGrid: TStringGrid);
var
  aComp, LowComp, HighComp : integer;
  aContingency, aPosition : string;
  aCol, aRow, aTrial : integer;
begin
  aRow := 1;
  aCol := 0;
  for aTrial := Low(FTrials) to High(FTrials) do
  begin
    if FTrials[aTrial].Positive then
      aContingency := rsPositive
    else
      aContingency := rsNegative;

    with ATrialGrid do
      begin
        if (aRow + 1) > RowCount then RowCount := aRow + 1;
        Cells[0, aRow] := IntToStr(aRow);    //Trial Number
        Cells[1, aRow] := aContingency + #32 + FTrials[aTrial].CenterStyle;
        Cells[2, aRow] := aContingency;
        Cells[3, aRow] := '';
        aCol := 4;
        LowComp := Low(FTrials[aTrial].Comps);
        HighComp := High(FTrials[aTrial].Comps);
        for aComp := LowComp to HighComp do
        begin
          if (aCol + 1) > ColCount then ColCount := aCol + 1;
          Cells[aCol, 0] := rsComparison + IntToStr(aComp + 1);
          Cells[aCol, aRow] := FTrials[aTrial].Comps[aComp].Path;
          Inc(aCol);
        end;

        for aComp := LowComp to HighComp do
        begin
          if (aCol + 1) > ColCount then ColCount := aCol + 1;
          Cells[aCol, 0] := rsComparison + IntToStr(aComp + 1) + rsPosition;
          aPosition := IntToStr(FTrials[aTrial].Comps[aComp].Top) + #32 +
                       IntToStr(FTrials[aTrial].Comps[aComp].Left) + #32 +
                       IntToStr(FTrials[aTrial].Comps[aComp].Width) + #32 +
                       IntToStr(FTrials[aTrial].Comps[aComp].Height);

          Cells[aCol, aRow] := aPosition;
          Inc(aCol);
        end;
        Inc(aRow);
      end;
    end;
end;

procedure TFormFPE.SetCircleGrid(AStmCircleGrid: TStmCircleGrid);
var
  LCount,LTrial, i : integer;
  LHasGap : Boolean;
  LStimulusToGap,
  LGapDegree : integer;
  LGapLength : integer;
begin
  LStimulusToGap := 0;
  LCount := High(AStmCircleGrid)+1;
  FCanDraw := False;
  // seTrials.Value default is 4, and 4 coordenates
  // we are making a 72 trials session, 4 * 2 trials for each coordenate;
  // times 2 cause we are making a go/no-go session, each positive trial must have a negative correlate
  // here 36 positive, 36 negative
  SetLength(FTrials, (seTrials.Value*LCount)*2);

  // set coordenates to memory
  for LTrial := Low(FTrials) to High(FTrials) do
    begin
      SetLength(FTrials[LTrial].Comps, LCount);
      FTrials[LTrial].CenterStyle := '';
      LHasGap := (LTrial mod 2) = 0;
      case RadioGroupEffect.ItemIndex of
       0: FTrials[LTrial].Positive := LHasGap;
       1: FTrials[LTrial].Positive := not LHasGap;
      end;

      for i := Low(AStmCircleGrid) to High(AStmCircleGrid) do
        begin
          FTrials[LTrial].Comps[i].Left := AStmCircleGrid[i].Left;
          FTrials[LTrial].Comps[i].Top := AStmCircleGrid[i].Top;
          FTrials[LTrial].Comps[i].Width := AStmCircleGrid[i].Width;
          FTrials[LTrial].Comps[i].Height := AStmCircleGrid[i].Height;

          if LHasGap and (i = LStimulusToGap) then  // there is a gap
            begin
              LGapDegree := Random(361);
              LGapLength := seGapLength.Value;
              FTrials[LTrial].Comps[i].Path := '1' + #32 + IntToStr(LGapDegree) + #32 + IntToStr(LGapLength) + #32;
            end
          else // no gap
            FTrials[LTrial].Comps[i].Path := '0' + #32 + '0' + #32 + '0' + #32;
        end;

      if LHasGap then
        if LStimulusToGap = High(AStmCircleGrid) then
          LStimulusToGap := 0
        else
          Inc(LStimulusToGap);
    end;

  LabelTrials.Caption := IntToStr(High(FTrials)+1);
  FCanDraw := True;
  Invalidate;
end;

procedure TFormFPE.SetCircleGridMixed(AStmCircleGrid: TStmCircleGrid);
var
  LCount,LTrial, i : integer;
  LHasGap : Boolean;
  LStimulusToGap,
  LGapDegree : integer;
  LGapLength : integer;
begin
  LStimulusToGap := 0;
  LCount := High(AStmCircleGrid)+1;
  FCanDraw := False;
  {
    9 stimuli (LCount)
    4 trials for each S (seTrials.Value)
    2 go/no-go
    2 fp/fn
    _______
    144 trials
  }
  SetLength(FTrials, (seTrials.Value*LCount)*2*2);
  for LTrial := Low(FTrials) to High(FTrials) do
  begin
    SetLength(FTrials[LTrial].Comps, LCount);

    LHasGap := (LTrial mod 2) = 0;
    if LTrial < (Length(FTrials) div 2) then
      begin
        FTrials[LTrial].Positive := LHasGap;
        FTrials[LTrial].CenterStyle := 'O';
      end
    else
      begin
        FTrials[LTrial].Positive := not LHasGap;
        FTrials[LTrial].CenterStyle := 'X';
      end;

    for i := Low(AStmCircleGrid) to High(AStmCircleGrid) do
      begin
        FTrials[LTrial].Comps[i].Left := AStmCircleGrid[i].Left;
        FTrials[LTrial].Comps[i].Top := AStmCircleGrid[i].Top;
        FTrials[LTrial].Comps[i].Width := AStmCircleGrid[i].Width;
        FTrials[LTrial].Comps[i].Height := AStmCircleGrid[i].Height;

        if LHasGap and (i = LStimulusToGap) then  // there is a gap
          begin
            LGapDegree := Random(361);
            LGapLength := seGapLength.Value;
            FTrials[LTrial].Comps[i].Path := '1' + #32 + IntToStr(LGapDegree) + #32 + IntToStr(LGapLength) + #32;
          end
        else // no gap
          FTrials[LTrial].Comps[i].Path := '0' + #32 + '0' + #32 + '0' + #32;
      end;

    if LHasGap then
      if LStimulusToGap = High(AStmCircleGrid) then
        LStimulusToGap := 0
      else
        Inc(LStimulusToGap);
  end;

  LabelTrials.Caption := IntToStr(High(FTrials)+1);
  FCanDraw := True;
  Invalidate;
end;

procedure TFormFPE.btnMinimizeTopTabClick(Sender: TObject);
begin
  Panel1.Visible := not Panel1.Visible;
end;

procedure TFormFPE.btnEditNodes1Click(Sender: TObject);
begin
  case RadioGroupEffect.ItemIndex of
   0..1: SetCircleGrid(GetCircleGrid(MonitorToShow));
   2: SetCircleGridMixed(GetCircleGrid(MonitorToShow));
  end;
end;

procedure TFormFPE.Button2Click(Sender: TObject);
begin
  Inc(FcurrentTrial);
end;

procedure TFormFPE.cbPreviewChange(Sender: TObject);
begin
  PreviewTimer.Enabled := not PreviewTimer.Enabled;
  //ShowMessage(BoolToStr(PreviewTimer.Enabled));
  Invalidate;
end;

procedure TFormFPE.WriteToDisk(ADefaultMainSection: TStrings;
  ADefaultBlocSection: TStrings; ATrialGrid: TStringGrid; AFilename: string);
var
  LRow : integer;
  LNewBloc : TConfigurationFile;
  LNumComp : Integer;
  LValues : string;
  i : integer;
  function GetNumComp : integer;
  var TopRightCell : string;
  begin
    with ATrialGrid do
      begin
        TopRightCell := Cells[ColCount - 1, 0];
        Delete(TopRightCell, Pos(rsPosition, TopRightCell), Length(rsPosition));
        Delete(TopRightCell, Pos(rsComparison, TopRightCell), Length(rsComparison));
        Result := StrToInt(TopRightCell);
      end;
  end;
begin
  if FileExistsUTF8(AFilename) then
    DeleteFileUTF8(AFilename);

  LNewBloc := TConfigurationFile.Create(AFilename);
  LNewBloc.CacheUpdates:=True;
  LNewBloc.WriteMain(ADefaultMainSection);
  LNewBloc.WriteBlocIfEmpty(1,ADefaultBlocSection);

  with ATrialGrid do
    for LRow := 1 to RowCount-1 do
      begin
         //SList.Values[_BkGnd] := IntToStr(clWhite);
          LNewBloc.WriteToTrial(LRow, _Kind, T_FPFN);
          LNewBloc.WriteToTrial(LRow, _Name,            Cells[1, LRow]);
          LNewBloc.WriteToTrial(LRow, _Comp+_Style,     ExtractDelimited(2,Cells[1, LRow],[#32]));
          LNewBloc.WriteToTrial(LRow, _Contingency,     Cells[2, LRow]);
          LNewBloc.WriteToTrial(LRow, _ShouldPlaySound, Cells[3, LRow]);
          LNewBloc.WriteToTrial(LRow, _Schedule,        T_CRF);
          LNewBloc.WriteToTrial(LRow, _ShowStarter,     BoolToStr(False, '1','0'));
          LNewBloc.WriteToTrial(LRow, _Cursor,          IntToStr(crNone));
          LNewBloc.WriteToTrial(LRow, _LimitedHold,     '4000');
          LNewBloc.WriteToTrial(LRow, _NextTrial,       '0');

          LNumComp := GetNumComp;
          LNewBloc.WriteToTrial(LRow, _NumComp,         IntToStr(LNumComp));
          for i := 1 to LNumComp do
            begin
              LNewBloc.WriteToTrial(LRow,_Comp+IntToStr(i)+_cBnd, Cells[i+3+LNumComp, LRow]);
              LValues :=  Cells[i + 3, LRow] + #32;
              LNewBloc.WriteToTrial(LRow,_Comp + IntToStr(i) + _cGap, ExtractDelimited(1,LValues,[#32]));
              LNewBloc.WriteToTrial(LRow,_Comp + IntToStr(i) + _cGap_Degree, ExtractDelimited(2,LValues,[#32]));
              LNewBloc.WriteToTrial(LRow,_Comp + IntToStr(i) + _cGap_Length, ExtractDelimited(3,LValues,[#32]));
            end;
      end;

  // update numblc and numtrials
  LNewBloc.Invalidate;

  // Save changes to disk
  LNewBloc.UpdateFile;
  LNewBloc.Free;
end;

end.


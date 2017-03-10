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

uses Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
     Dialogs, ExtCtrls, StdCtrls, Spin, ActnList
     , grid_helpers
     , draw_methods
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

    TStmMatrix = array of array of TComparison;

    TStmCircleGrid = array of TComparison;

    TTrial = record
        Id : integer;
        Positive : Boolean;
        //Name: string;
        //Kind: string;
        //NumComp: integer;
        Comps : array of TComparison;
    end;

    TTrials = array of TTrial;

    { TFormFPE }

    TFormFPE = class(TForm)
      btnEditNodes: TButton;
      btnClose: TButton;
      btnEditNodes1: TButton;
      btnMinimizeTopTab: TButton;
      btnOk: TButton;
      gbTrials: TGroupBox;
      gbStimuli: TGroupBox;
      gbGapLength: TGroupBox;
      Panel1: TPanel;
      RadioGroupEffect: TRadioGroup;
      RadioGroupGrids: TRadioGroup;
      seTrials: TSpinEdit;
      seGapLength: TSpinEdit;
      PreviewTimer: TTimer;
      procedure btnEditNodes1Click(Sender: TObject);
      procedure btnMinimizeTopTabClick(Sender: TObject);
      procedure Button2Click(Sender: TObject);
      procedure cbPreviewChange(Sender: TObject);
      procedure FormActivate(Sender: TObject);
      procedure FormKeyPress(Sender: TObject; var Key: char);
      procedure FormPaint(Sender: TObject);
      procedure PreviewTimerTimer(Sender: TObject);
    private
    // fullscreen
      FFullScreen : Boolean;
      //FOriginalBounds : TRect;
      //FOriginalWindowState : TWindowState;
      //FScreenBounds : TRect;
    // other
      FCanDraw : Boolean;
      FCurrentTrial : integer;
      FMonitor: integer;
      FTrials : TTrials;
      function GetMatrix(AMonitor : integer) : TStmMatrix;
      function GetCircleGrid(AMonitor : integer) : TStmCircleGrid;
      procedure SetMonitor(AValue: integer);
    public
      procedure SetMatrix(AStmMatrix: TStmMatrix);
      procedure SetCircleGrid(AStmCircleGrid : TStmCircleGrid);
      procedure SetFullScreen(TurnOn : Boolean);
      property Trials : TTrials read FTrials write FTrials;
      property MonitorToShow : integer read FMonitor write SetMonitor;
    end;

var
  FormFPE: TFormFPE;

implementation

{$R *.lfm}

{ TFormFPE }

procedure TFormFPE.FormActivate(Sender: TObject);
begin
  BorderStyle := bsNone;
  WindowState := wsFullScreen;
  Left := Screen.Monitors[MonitorToShow].Left;
  FCurrentTrial := 0;
  Randomize;
  //SetMatrix(GetMatrix(MonitorToShow));
  SetCircleGrid(GetCircleGrid(MonitorToShow));
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
  aGap : Boolean;
  aGapDegree : integer;
  aGapLength : integer;

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
      try
        //DrawCircle(Canvas, 300, 300, 100, True, 50, 5 );
        if RadioGroupGrids.ItemIndex <> -1 then
          begin
            for i := Low(Trials[FCurrentTrial].Comps) to Length(Trials[FCurrentTrial].Comps)-1 do
              begin
                aR.Left := Trials[FCurrentTrial].Comps[i].Left;
                aR.Top := Trials[FCurrentTrial].Comps[i].Top;
                aWidth := Trials[FCurrentTrial].Comps[i].Width;
                aR.Right := aR.Left + aWidth;
                aHeight := Trials[FCurrentTrial].Comps[i].Height;
                aR.Bottom := aR.Top + aHeight;

                aGapValues := Trials[FCurrentTrial].Comps[i].Path;
                //WriteLn(aGapValues);
                aGap := StrToBoolDef(Copy(aGapValues,0,pos(#32,aGapValues)-1),False);
                NextValue(aGapValues);

                aGapDegree := 16 * StrToIntDef(Copy(aGapValues,0,pos(#32,aGapValues)-1),1);
                NextValue(aGapValues);

                aGapLength := 16 * StrToIntDef(Copy(aGapValues,0,pos(#32,aGapValues)-1),360);
                case RadioGroupGrids.ItemIndex of
                 0: DrawCustomEllipse(Canvas,aR,GetInnerRect(aR,aWidth,aHeight),aGap,aGapDegree,aGapLength);
                 1: DrawCustomEllipse(Canvas,AR,aGap,aGapDegree,aGapLength);
                end;
              end;
            i := FCurrentTrial + 1;
            Canvas.TextOut(Trials[i-1].Comps[0].Left - 10, Trials[i-1].Comps[0].Top - 10 , IntToStr(i));
            Canvas.TextOut(Trials[i-1].Comps[0].Left - 20, Trials[i-1].Comps[0].Top - 30 , BoolToStr(Trials[FCurrentTrial].Positive, 'Positive', 'Negative'));
          end;

      finally
        LoadOldCanvas;
        OldCanvas.Free;
      end;
    end;
end;

procedure TFormFPE.PreviewTimerTimer(Sender: TObject);
begin
  if FCurrentTrial < High(Trials) then
    Inc(FCurrentTrial)
  else FCurrentTrial := 0;
  Invalidate;
end;

function TFormFPE.GetMatrix(AMonitor: integer): TStmMatrix;
var
  i,j,
  LRowCount,LColCount,SHeight,SWidth,SYGap,SXGap,SLeft,STop: integer;
  function GetPositionFromSegment(ASegmentSize,AStep,ASteps,
    AStimulusSize,AInterStimulusGap : integer):integer;
  var
    LSize : integer;
  begin
    LSize := AStimulusSize + AInterStimulusGap;
    Result := Round((LSize*AStep)-((LSize*ASteps)/2)+((ASegmentSize+AInterStimulusGap)/2));
  end;
begin
  SHeight := 150;
  SWidth := 150;
  SYGap := 100;
  SXGap := 100;
  SLeft := 0;
  STop := 0;
  LRowCount := 3;
  LColCount := 3;
  SetLength(Result, LRowCount,LColCount);
  for i := Low(Result) to High(Result) do
    begin
      SLeft := GetPositionFromSegment(Screen.Monitors[AMonitor].Width,i,LColCount,SWidth,SXGap);
      for j:= Low(Result[i]) to High(Result[i]) do
        begin
          STop := GetPositionFromSegment(Screen.Monitors[AMonitor].Height,j,LRowCount,SHeight,SYGap);
          Result[i][j].Left := SLeft;
          Result[i][j].Top := STop;
          Result[i][j].Width := SWidth;
          Result[i][j].Height := SHeight;
        end;
    end;
end;

function TFormFPE.GetCircleGrid(AMonitor: integer): TStmCircleGrid;
var
  LP : TPoint;
  LR : TRect;
  LW, LH,
  LSLeft, LSTop, LSCount, i, LSSize: Integer;
begin
  LSSize := 150;
  LW := Screen.Monitors[AMonitor].Width;
  LH := Screen.Monitors[AMonitor].Height;
  LR := GetCentralRect(LW,LH,LSSize div 2);

  LSCount := 9;
  SetLength(Result,LSCount);
  for i := 0 to LSCount -1 do
    begin
      LP := GetPointFromAngle(i*(360/LSCount),LR);
      LSLeft:= LP.X-(LSSize div 2);
      LSTop := LP.Y-(LSSize div 2);
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
end;

procedure TFormFPE.SetMatrix(AStmMatrix : TStmMatrix);
var
  LCount,LTrial, LStimulusToGap, LComp, i, j : integer;
  LGapDegree : integer;
  LGapLength : integer;
  LHasGap : Boolean;
begin
  LStimulusToGap := 0;
  LCount := (High(AStmMatrix)+1)*(High(AStmMatrix[0])+1);
  FCanDraw := False;
  // seTrials.Value default is 4, and 4 coordenates
  // we are making a 72 trials session, 4 * 2 trials for each coordenate;
  // times 2 cause we are making a go/no-go session, each positive trial must have a negative correlate
  // here 36 positive, 36 negative
  SetLength( FTrials, (seTrials.Value*LCount)*2);

  // set coordenates to memory
  for LTrial := Low(Trials) to High(Trials) do
    begin
      SetLength(FTrials[LTrial].Comps, LCount);

      LComp := 0;
      for i := Low(AStmMatrix) to High(AStmMatrix) do
        for j := Low(AStmMatrix[i]) to High(AStmMatrix[i]) do
          begin
            FTrials[LTrial].Comps[LComp].Left := AStmMatrix[i][j].Left;
            FTrials[LTrial].Comps[LComp].Top := AStmMatrix[i][j].Top;
            FTrials[LTrial].Comps[LComp].Width := AStmMatrix[i][j].Width;
            FTrials[LTrial].Comps[LComp].Height := AStmMatrix[i][j].Height;
            Inc(LComp);
          end;

      LHasGap := (LTrial mod 2) = 0;
      case RadioGroupEffect.ItemIndex of
       0: Trials[LTrial].Positive := LHasGap;
       1: Trials[LTrial].Positive := not LHasGap;
      end;

      for LComp := 0 to LCount -1 do
        begin
          if LHasGap and (i = LStimulusToGap) then
            begin
              LGapDegree := 1+Random(360);
              LGapLength := seGapLength.Value;
              Trials[LTrial].Comps[LComp].Path := '1' + #32 + IntToStr(LGapDegree) + #32 + IntToStr(LGapLength) + #32;
            end
          else
            Trials[LTrial].Comps[LComp].Path := '0' + #32 + '1' + #32 + '360' + #32;
        end;

      if LHasGap then
        if LStimulusToGap = LCount-1 then
          LStimulusToGap := 0
        else
          Inc(LStimulusToGap);
    end;

  FCanDraw := True;
  Invalidate;
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
  for LTrial := Low(Trials) to High(Trials) do
    begin
      SetLength(FTrials[LTrial].Comps, LCount);

      LHasGap := (LTrial mod 2) = 0;
      case RadioGroupEffect.ItemIndex of
       0: Trials[LTrial].Positive := LHasGap;
       1: Trials[LTrial].Positive := not LHasGap;
      end;

      for i := Low(AStmCircleGrid) to High(AStmCircleGrid) do
        begin
          FTrials[LTrial].Comps[i].Left := AStmCircleGrid[i].Left;
          FTrials[LTrial].Comps[i].Top := AStmCircleGrid[i].Top;
          FTrials[LTrial].Comps[i].Width := AStmCircleGrid[i].Width;
          FTrials[LTrial].Comps[i].Height := AStmCircleGrid[i].Height;

          if LHasGap and (i = LStimulusToGap) then  // there is a gap
            begin
              LGapDegree := 1+Random(360);
              LGapLength := seGapLength.Value;
              Trials[LTrial].Comps[i].Path := '1' + #32 + IntToStr(LGapDegree) + #32 + IntToStr(LGapLength) + #32;
            end
          else // no gap
            Trials[LTrial].Comps[i].Path := '0' + #32 + '1' + #32 + '360' + #32;
        end;

      if LHasGap then
        if LStimulusToGap = High(AStmCircleGrid) then
          LStimulusToGap := 0
        else
          Inc(LStimulusToGap);
    end;

  FCanDraw := True;
  Invalidate;
end;

procedure TFormFPE.btnMinimizeTopTabClick(Sender: TObject);
begin
  Panel1.Visible := not Panel1.Visible;
end;

procedure TFormFPE.btnEditNodes1Click(Sender: TObject);
begin
  case RadioGroupGrids.ItemIndex of
   0: SetMatrix(GetMatrix(MonitorToShow));
   1: SetCircleGrid(GetCircleGrid(MonitorToShow));
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

procedure TFormFPE.SetFullScreen(TurnOn: Boolean);
begin
  if TurnOn then
    begin
      //fullscreen true
      {$IFDEF MSWINDOWS}
      // to do
      {$ENDIF}

      {$IFDEF LINUX}
      WindowState := wsFullScreen;
      {$ENDIF}
    end
  else
    begin
      //fullscreen false
      {$IFDEF MSWINDOWS}
      // to do
      {$ENDIF}

      {$IFDEF LINUX}
      WindowState := wsNormal;
      {$ENDIF}
    end;
  FFullScreen := TurnOn;
end;

end.


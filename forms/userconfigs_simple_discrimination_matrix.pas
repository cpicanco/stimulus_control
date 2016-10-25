{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit userconfigs_simple_discrimination_matrix;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
     Dialogs, ExtCtrls, StdCtrls, Spin, ActnList
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

    TTrial = record
        Id : integer;
        Positive : Boolean;
        //Name: string;
        //Kind: string;
        //NumComp: integer;
        Comps : array of TComparison;
    end;

    TTrials = array of TTrial;

    { TMatrixForm }

    TMatrixForm = class(TForm)
      btnEditNodes: TButton;
      btnClose: TButton;
      btnEditNodes1: TButton;
      btnMinimizeTopTab: TButton;
      btnOk: TButton;
      cbChooseGrid: TComboBox;
      cbPreview: TCheckBox;
      cbUseStimuliSet: TCheckBox;
      gbTrials: TGroupBox;
      gbStimuli: TGroupBox;
      gbGapLength: TGroupBox;
      Panel1: TPanel;
      seTrials: TSpinEdit;
      seGapLength: TSpinEdit;
      PreviewTimer: TTimer;
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
      procedure SetMonitor(AValue: integer);
    public
      procedure SetMatrix(AStmMatrix: TStmMatrix);
      procedure SetFullScreen(TurnOn : Boolean);
      property Trials : TTrials read FTrials write FTrials;
      property MonitorToShow : integer read FMonitor write SetMonitor;
    end;

var
  FrmMatrix: TMatrixForm;

implementation

{$R *.lfm}

{ TMatrixForm }

procedure TMatrixForm.FormActivate(Sender: TObject);
begin
  BorderStyle := bsNone;
  WindowState := wsFullScreen;
  Left := Screen.Monitors[MonitorToShow].Left;
  FCurrentTrial := 0;
  Randomize;
  SetMatrix(GetMatrix(MonitorToShow));
end;

procedure TMatrixForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if key in [#32] then
    Panel1.Visible := not Panel1.Visible;
end;

procedure TMatrixForm.FormPaint(Sender: TObject);
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
        if cbPreview.Checked then
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

                DrawCustomEllipse(Canvas,aR,GetInnerRect(aR,aWidth,aHeight),aGap,aGapDegree,aGapLength);
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

procedure TMatrixForm.PreviewTimerTimer(Sender: TObject);
begin
  if FCurrentTrial < High(Trials) then
    Inc(FCurrentTrial)
  else FCurrentTrial := 0;
  Invalidate;
end;

function TMatrixForm.GetMatrix(AMonitor: integer): TStmMatrix;
var
  i,j,
  LRowCount,LColCount,SHeight,SWidth,SYGap,SXGap,SLeft,STop: integer;
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
      SLeft := ((SWidth+SXGap)*i)+(Screen.Monitors[AMonitor].Width div 2)
                  -(((SWidth+SXGap)*LColCount) div 2)+((SXGap) div 2);
      for j:= Low(Result[i]) to High(Result[i]) do
        begin
          STop := ((SHeight+SYGap)*j)+(Screen.Monitors[AMonitor].Height div 2)
               -(((SHeight+SYGap)*LRowCount) div 2)+(SYGap div 2);
          Result[i][j].Left := SLeft;
          Result[i][j].Top := STop;
          Result[i][j].Width := SWidth;
          Result[i][j].Height := SHeight;
        end;
    end;
end;

procedure TMatrixForm.SetMonitor(AValue: integer);
begin
  if FMonitor = AValue then Exit;
  FMonitor := AValue;
end;

procedure TMatrixForm.SetMatrix(AStmMatrix : TStmMatrix);
var
  LCount,LTrial, LComp, i, j, t : integer;
  {aGap,} aGapDegree, aGapLength : integer;
  aPositiveTrial, inct : Boolean;
begin
  LCount := (High(AStmMatrix)+1)*(High(AStmMatrix[0])+1);
  t := 0;
  inct := False;
  FCanDraw := False;
  // seTrials.Value default is 4, and 4 coordenates
  // we are making a 32 trials session, 4 * 2 trials for each coordenate;
  // times 2 cause we are making a go/no-go session, each positive trial must have a negative correlate
  // here 36 positive, 36 negative
  SetLength( FTrials, ( seTrials.Value * LCount ) * 2 );

  // set coordenates to memory
  for LTrial := Low( Trials ) to High( Trials ) do
    begin
      SetLength( FTrials[LTrial].Comps, LCount );

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

      for LComp := 0 to LCount -1 do
        begin
          { TODO 5 -oRafael -cFunctionality : Need to allow choosing different filepaths of media stimulus.
          For now the "comp[i].path" is a string containing the following values of the circles:
          'Gap GapDegree GapLength'. }
          aPositiveTrial := ((LTrial mod 2) = 0) and (LComp = t);
          if aPositiveTrial then
            begin
              // positive
              inct := True;
              aGapDegree := 1+Random(360);
              aGapLength := seGapLength.Value;
              Trials[LTrial].Comps[LComp].Path := '1' + #32 + IntToStr(aGapDegree) + #32 + IntToStr(aGapLength) + #32;
            end
          else
            begin
              // negative
              Trials[LTrial].Comps[LComp].Path := '0' + #32 + '1' + #32 + '360' + #32;
              end;
            if t = LCount then t := 0;
        end;
      if inct then
        begin
          Inc(t);
          Trials[LTrial].Positive := True;
          inct := False;
        end
      else Trials[LTrial].Positive := False;
    end;

  FCanDraw := True;
  Invalidate;
end;

procedure TMatrixForm.btnMinimizeTopTabClick(Sender: TObject);
begin
  Panel1.Visible := not Panel1.Visible;
end;

procedure TMatrixForm.Button2Click(Sender: TObject);
begin
  Inc(FcurrentTrial);
end;

procedure TMatrixForm.cbPreviewChange(Sender: TObject);
begin
  PreviewTimer.Enabled := not PreviewTimer.Enabled;
  //ShowMessage(BoolToStr(PreviewTimer.Enabled));
  Invalidate;
end;

procedure TMatrixForm.SetFullScreen(TurnOn: Boolean);
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


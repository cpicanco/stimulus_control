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

     {$IFDEF LCLGTK2}
     , gtk2, gdk2, glib2
     {$ENDIF}

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
      btnGetMatrix: TButton;
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
      procedure btnGetMatrixClick(Sender: TObject);
      procedure btnMinimizeTopTabClick(Sender: TObject);
      procedure Button1Click(Sender: TObject);
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
      FTrials : TTrials;
    public
      procedure SetMatrix(MatrixCoordenates : TStrings);
      procedure SetFullScreen(TurnOn : Boolean);
      property Trials : TTrials read FTrials write FTrials;
    end;

var
  FrmMatrix: TMatrixForm;

implementation

{$R *.lfm}

uses userconfigs_get_matrix, background;

{ TMatrixForm }

procedure TMatrixForm.FormActivate(Sender: TObject);
begin
    SetFullScreen(True);
    FCurrentTrial := 0;
    Randomize;
end;

procedure TMatrixForm.FormKeyPress(Sender: TObject; var Key: char);
begin
    if key in [#32] then
    begin
      Panel1.Visible := not Panel1.Visible;
      //gbAddRmvAxis.Visible := not gbAddRmvAxis.Visible;
    end;
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

procedure TMatrixForm.SetMatrix(MatrixCoordenates : TStrings);
var
    i, j, t : integer;
    aTop, aLeft, aWidth, aHeight : integer;
    {aGap,} aGapDegree, aGapLength : integer;
    aPositiveTrial, inct : Boolean;
    Coordenates : string;

  procedure NextValue(var S : string);
  begin
    Delete( S, 1, pos( #32, S ) );
    if Length( S ) > 0 then
      while S[1] = #32 do
        Delete( S, 1, 1 );
  end;

  procedure GetCoordenates(var CoordenateString : string);
  var pos42 : integer;
  begin
    // ASCII, Decimals
    // #32 (space)
    // #42 ( * )

    // delete #42 in the string
    pos42 := Pos( #42, CoordenateString );
    while pos42 <> 0  do
      begin
        Delete( CoordenateString, 1, pos42 -1 );
        pos42 := Pos( #42, CoordenateString );
      end;

    // Assign values to local variables
    aTop   := StrToIntDef( Copy( CoordenateString, 0, pos( #32, CoordenateString ) - 1), 0);
    NextValue(CoordenateString);

    aLeft  := StrToIntDef( Copy( CoordenateString, 0, pos( #32, CoordenateString ) - 1), 0);
    NextValue(CoordenateString);

    aWidth := StrToIntDef( Copy( CoordenateString, 0, pos( #32, CoordenateString ) - 1), 0);
    NextValue(CoordenateString);

    aHeight := StrToIntDef( CoordenateString, 0 );
  end;

begin
  if MatrixCoordenates.Count > 0 then
    begin
      t := 0;
      inct := False;
      FCanDraw := False;
      // seTrials.Value default is 4, and 4 coordenates
      // we are making a 32 trials session, 4 * 2 trials for each coordenate;
      // times 2 cause we are making a go/no-go session, each positive trial must have a negative correlate
      // here 36 positive, 36 negative
      SetLength( FTrials, ( seTrials.Value * MatrixCoordenates.Count ) * 2 );

      // set coordenates to memory
      for i := Low( Trials ) to High( Trials ) do
        begin
        SetLength( FTrials[i].Comps, MatrixCoordenates.Count );

          for j := 0 to MatrixCoordenates.Count -1 do
            begin
              Coordenates := MatrixCoordenates.Strings[j];
              GetCoordenates( Coordenates );
              Trials[i].Id := i;
              Trials[i].Comps[j].Top := aTop;
              Trials[i].Comps[j].Left := aLeft;
              Trials[i].Comps[j].Width := aWidth;
              Trials[i].Comps[j].Height := aHeight;

              { TODO 5 -oRafael -cFunctionality : Need to allow choosing different filepaths of media stimulus.
              For now the "comp[i].path" is a string containing the following values of the circles:
              'Gap GapDegree GapLength'. }
              aPositiveTrial := ((i mod 2) = 0) and (j = t);
              if aPositiveTrial then
                begin
                  // positive
                  inct := True;
                  aGapDegree := 1+Random(360);
                  aGapLength := seGapLength.Value;
                  Trials[i].Comps[j].Path := '1' + #32 + IntToStr(aGapDegree) + #32 + IntToStr(aGapLength) + #32;
                end
              else
                begin
                  // negative
                  Trials[i].Comps[j].Path := '0' + #32 + '1' + #32 + '360' + #32;
                  end;
                if t = MatrixCoordenates.Count then t := 0;
            end;
          if inct then
            begin
              Inc(t);
              Trials[i].Positive := True;
              inct := False;
            end
          else Trials[i].Positive := False;
        end;
    end;
  FCanDraw := True;
  Invalidate;
end;

procedure TMatrixForm.btnGetMatrixClick(Sender: TObject);
begin
  //WindowState := wsMinimized;
  Visible := False;
  FrmMatrixConfig := TMatrixConfigForm.Create(Application);
  with FrmBackground do
    begin
      color := clBlack;
      //WindowState := wsMaximized;
      Show;
      SetFullScreen(True);
    end;
  FrmMatrixConfig.Show;
  FrmMatrixConfig.BackGround := FrmBackground;
end;

procedure TMatrixForm.btnMinimizeTopTabClick(Sender: TObject);
begin
  Panel1.Visible := not Panel1.Visible;
end;

procedure TMatrixForm.Button1Click(Sender: TObject);
var i : Integer;
    //a : string;
begin
  for i := 0 To Length(Trials[FCurrentTrial].Comps) -1 do
    begin
      Showmessage(Trials[FCurrentTrial].Comps[i].Path);
    end;

  //Invalidate;
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


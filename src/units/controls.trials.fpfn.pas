{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.FPFN;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils, ExtCtrls, Graphics

  , Controls.Trials.Abstract
  , Controls.Trials.Helpers
  , Schedules
  //, Audio.Bass_nonfree
  ;

type

  TFPFNDrawing = (fpfnClearCircles, fpfnFullOuterInnerCircles);

  { TFPFN }

  {
   Implements a specific type of GO/NO-GO trial.
   Implements feature positive and feature negative displays.
  }
  TFPFN = Class(TTrial)
  private
    {
      True = GO
      False = NO-GO
    }
    FConsequenceFired : Boolean;
    FStyle : TGoNoGoStyle;
    FContingency : string;
    FDataSupport : TDataSupport;
    FForeground : TBitmap;
    FFeaturesToDraw : TFPFNDrawing;
    FSchedule : TSchedule;
    //FSound : TBassStream;
    FShouldPlaySound : Boolean;
    FCenterStyle : string;

    // go
    procedure Consequence(Sender: TObject);
    procedure DrawForeground(ACircles: array of TCircle; AFeaturesToDraw: TFPFNDrawing);
    procedure PlaySound;
    procedure Response(Sender: TObject);

    // no-go
    procedure TrialBeforeEnd(Sender: TObject);
    procedure TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrialPaint;
    procedure TrialResult(Sender: TObject);
    procedure TrialStart(Sender: TObject);
  protected { TTrial }
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TCustomControl); override;
    destructor Destroy; override;
    procedure Hide;override;
    procedure Play(ACorrection : Boolean); override;
    //procedure DispenserPlusCall; override;
  end;

implementation

uses background, strutils, constants, Timestamps, Canvas.Helpers
  , Session.Configuration.GlobalContainer;

{ TFPFN }


constructor TFPFN.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialKeyUp := @TrialKeyUp;
  OnTrialStart := @TrialStart;
  OnTrialPaint := @TrialPaint;
  Header :=  Header + #9 +
             rsReportStmBeg + #9 +
             rsReportRspLat + #9 +
             rsReportStmEnd + #9 +
             rsReportRspFrq + #9 +
             rsReportRspExp;

  FForeGround := TBitmap.Create;
  FDataSupport.Responses:= 0;
  FContingency := '';
  FForeground := TBitmap.Create;
end;

destructor TFPFN.Destroy;
begin
  FForeGround.Free;
  //if Assigned(FSound) then
  //  FSound.Free;
  inherited Destroy;
end;

procedure TFPFN.Hide;
begin
  inherited Hide;
  TBackground(Self.Parent).UpdateMask;
end;

procedure TFPFN.TrialResult(Sender: TObject);
begin
  if Result = T_NONE then
    begin
      if FConsequenceFired then
        case UpperCase(FContingency) of
          'POSITIVA': Result := T_HIT;
          'NEGATIVA': Result := T_MISS;
        end
      else
        case UpperCase(FContingency) of
          'POSITIVA': Result := T_MISS;
          'NEGATIVA': Result := T_HIT;
        end;

      case NextTrial of
        T_CRT:NextTrial := T_CRT;
        'IF_HIT_JUMP_NEXT_TRIAL':
            if Result = T_HIT then
              NextTrial := IntToStr(Counters.CurrentTrial+3);
      end;
    end;
end;

procedure TFPFN.TrialBeforeEnd(Sender: TObject);
begin
  FDataSupport.StmEnd := TickCount;
  TrialResult(Sender);
  if gngPlayGoOnBeforeEnd in FStyle then
    begin
      if FConsequenceFired then
        PlaySound;
    end
  else
    begin
      if gngPlayNoGo in FStyle then
        if not FConsequenceFired then
          PlaySound;
    end;

  LogEvent(Result);
  WriteData(Sender);
end;

procedure TFPFN.Consequence(Sender: TObject);
begin
  if FConsequenceFired = False then FConsequenceFired := True;
  TrialResult(Sender);

  if gngPlayGoOnBeforeEnd in FStyle then
    // do nothing
  else
    if gngPlayGo in FStyle then
      PlaySound;

  if Assigned(Counters.OnConsequence) then Counters.OnConsequence(Self);
end;

procedure TFPFN.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 32 then
    begin
      LogEvent('R');
      FSchedule.DoResponse;
    end;
end;

procedure TFPFN.TrialPaint;
begin
  Canvas.Draw(0,0,FForeground);
end;

procedure TFPFN.PlaySound;
var
  LSoundFile : string;
begin
  case Result of
    T_HIT  : LSoundFile := RootMedia+'CSQ1.wav';
    T_MISS : LSoundFile := RootMedia+'CSQ2.wav';
  end;

  if FileExists(LSoundFile) and FShouldPlaySound then
    begin
      LogEvent('C');
      //if Assigned(FSound) then
      //  begin
      //    FSound.Free;
      //    FSound := TBassStream.Create(LSoundFile);
      //  end
      //else
      //  FSound := TBassStream.Create(LSoundFile);
      //FSound.Play;
    end;
end;

procedure TFPFN.DrawForeground(ACircles: array of TCircle;
  AFeaturesToDraw: TFPFNDrawing);
var
  i : integer;
  LR : TRect;
  LX, LY, LSize : integer;
begin
  FForeground.Width:= Width;
  FForeground.Height:= Height;
  FForeground.TransparentColor:=clFuchsia;
  FForeground.Transparent:=True;
  LR := Rect(0,0,Width, Height);
  with FForeground.Canvas do
    begin
      // Transparent part of foreground
      Pen.Color:=clFuchsia;
      Brush.Style:=bsSolid;
      Brush.Color:=clFuchsia;
      Rectangle(LR);
    end;

  // FForeground do not change, we need to draw only once
  for i := Low(ACircles) to High(ACircles) do
    with ACircles[i] do
      case AFeaturesToDraw of
        fpfnClearCircles:
          DrawCustomEllipse(FForeground.Canvas, OuterRect, gap_degree, gap_length);
        fpfnFullOuterInnerCircles:
          DrawCustomEllipse(FForeground.Canvas, OuterRect, InnerRect, gap, gap_degree, gap_length);
      end;

  LX := Round(Width / 2);
  LY := Round(Height / 2);
  LSize := (ACircles[0].OuterRect.Right-ACircles[0].OuterRect.Left) div 2;
  case UpperCase(FCenterStyle) of
    'O' :
      with FForeground.Canvas do
      begin
        Brush.Style:= bsClear;
        Pen.Mode := pmBlack;
        Pen.Width := 5;
        Rectangle(LX - LSize, LY - LSize, LX + LSize, LY + LSize);
      end;

    'X' :
      with FForeground.Canvas do
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

procedure TFPFN.Play(ACorrection: Boolean);
var
  s1: string;
  LOuterR : TRect;
  i, LWidth, LHeight, LNumComp : Integer;
  LCircles : array of TCircle;
  Parameters : TStringList;
begin
  inherited Play(ACorrection);
  Parameters := Configurations.Parameters;
  FFeaturesToDraw := TFPFNDrawing(StrToIntDef(Parameters.Values[_DrawingType], 0));

  FStyle := [];
  s1 := Parameters.Values[_Style];
  for i := 1 to WordCount(s1,[#32]) do
    FStyle += [StringToStyle(ExtractDelimited(i,s1,[#32]))];

  if FStyle = [] then
    FStyle := [gngPlayGo,gngPlayGoOnBeforeEnd];

  FSchedule := TSchedule.Create(self);
  with FSchedule do
    begin
      OnConsequence := @Consequence;
      OnResponse:= @Response;
      Load(Parameters.Values[_Schedule]);
      Enabled := False;
    end;
  //AddToClockList(FSchedule);
  FContingency := Parameters.Values[_Contingency];
  FCenterStyle := Parameters.Values[_Comp+_Style];

  FShouldPlaySound := StrToBoolDef(Parameters.Values[_ShouldPlaySound], True);
  LNumComp := StrToIntDef(Parameters.Values[_NumComp], 1);
  SetLength(LCircles, LNumComp);
  for i := 0 to LNumComp -1 do
    begin
        s1:= Parameters.Values[_Comp + IntToStr(i + 1) + _cBnd];
        LOuterR.Top:= StrToInt(ExtractDelimited(1,s1,[#32]));
        LOuterR.Left:= StrToInt(ExtractDelimited(2,s1,[#32]));
        LWidth := StrToInt(ExtractDelimited(3,s1,[#32]));
        LOuterR.Right := LOuterR.Left + LWidth;
        LHeight := StrToInt(ExtractDelimited(4,s1,[#32]));
        LOuterR.Bottom := LOuterR.Top + LHeight;

        with LCircles[i] do
          begin
            OuterRect := LOuterR;
            case FFeaturesToDraw of
              fpfnClearCircles: {do nothing};
              fpfnFullOuterInnerCircles.:InnerRect := GetInnerRect(LOuterR, LWidth, LHeight);
            end;
            gap := StrToBoolDef(Parameters.Values[_Comp + IntToStr(i+1) + _cGap], False );
            gap_degree := 16 * StrToIntDef(Parameters.Values[_Comp + IntToStr(i + 1) + _cGap_Degree], Random(361));
            gap_length := 16 * (360-StrToIntDef(Parameters.Values[_Comp + IntToStr(i + 1) + _cGap_Length], 5 ));
          end;
      end;

  TBackground(Self.Parent).DrawMask:=True;

  DrawForeground(LCircles, FFeaturesToDraw);

  if Self.ClassType = TFPFN then Config(Self);
end;

procedure TFPFN.TrialStart(Sender: TObject);
begin
  FConsequenceFired := False;
  FDataSupport.Latency := TimeStart;
  FDataSupport.StmBegin := TickCount;
  Invalidate;
end;

procedure TFPFN.WriteData(Sender: TObject);
var LLatency : string;
begin
  inherited WriteData(Sender);

  if FDataSupport.Latency = TimeStart then
    LLatency := 'NA'
  else LLatency := TimestampToStr(FDataSupport.Latency - TimeStart);

  Data :=  Data +
           TimestampToStr(FDataSupport.StmBegin - TimeStart) + #9 +
           LLatency + #9 +
           TimestampToStr(FDataSupport.StmEnd - TimeStart) + #9 +
           Format('%-*.*d', [4,8, FDataSupport.Responses]) + #9 +
           FContingency + #32 + FCenterStyle;

  //if Assigned(OnTrialWriteData) then OnTrialWriteData(Self);
end;

procedure TFPFN.Response(Sender: TObject);
begin
  Inc(FDataSupport.Responses);
  if FDataSupport.Latency = TimeStart then
    FDataSupport.Latency := TickCount;

  if Assigned(Counters.OnStmResponse) then Counters.OnStmResponse(Sender);
  if Assigned(OnStmResponse) then OnStmResponse (Self);
end;

end.

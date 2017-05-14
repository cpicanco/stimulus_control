{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit trial_feature_positive;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils, ExtCtrls, Graphics

  , trial_abstract
  , trial_helpers
  , schedules_main
  , bass_player
  ;

type

  TFPEDrawing = (fpeClearCircles, fpeFullOuterInnerCircles);

  { TFPE }

  {
   Implements a specific type of GO/NO-GO trial.
   Implements feature positive and feature negative displays.
  }
  TFPE = Class(TTrial)
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
    FFeaturesToDraw : TFPEDrawing;
    FSchedule : TSchedule;
    FSound : TBassStream;

    // go
    procedure Consequence(Sender: TObject);
    procedure DrawForeground(ACircles: array of TCircle; AFeaturesToDraw: TFPEDrawing);
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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Hide;override;
    procedure Play(ACorrection : Boolean); override;
    //procedure DispenserPlusCall; override;
  end;

implementation

uses background, strutils, constants, timestamps, draw_methods;

{ TFPE }


constructor TFPE.Create(AOwner: TComponent);
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

destructor TFPE.Destroy;
begin
  FForeGround.Free;
  if Assigned(FSound) then
    FSound.Free;
  inherited Destroy;
end;

procedure TFPE.Hide;
begin
  inherited Hide;
  TBackground(Self.Parent).UpdateMask;
end;

procedure TFPE.TrialResult(Sender: TObject);
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
              NextTrial := IntToStr(CounterManager.CurrentTrial+3);
      end;
    end;
end;

procedure TFPE.TrialBeforeEnd(Sender: TObject);
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

procedure TFPE.Consequence(Sender: TObject);
begin
  if FConsequenceFired = False then FConsequenceFired := True;
  TrialResult(Sender);

  if gngPlayGoOnBeforeEnd in FStyle then
    // do nothing
  else
    if gngPlayGo in FStyle then
      PlaySound;

  if Assigned(CounterManager.OnConsequence) then CounterManager.OnConsequence(Self);
end;

procedure TFPE.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 32 then
    begin
      LogEvent('R');
      FSchedule.DoResponse;
    end;
end;

procedure TFPE.TrialPaint;
begin
  Canvas.Draw(0,0,FForeground);
end;

procedure TFPE.PlaySound;
var
  LSoundFile : string;
begin
  case Result of
    T_HIT  : LSoundFile := RootMedia+'CSQ1.wav';
    T_MISS : LSoundFile := RootMedia+'CSQ2.wav';
  end;

  if FileExists(LSoundFile) then
    begin
      LogEvent('C');
      if Assigned(FSound) then
        begin
          FSound.Free;
          FSound := TBassStream.Create(LSoundFile);
        end
      else
        FSound := TBassStream.Create(LSoundFile);
      FSound.Play;
    end;
end;

procedure TFPE.DrawForeground(ACircles: array of TCircle;
  AFeaturesToDraw: TFPEDrawing);
var
  i : integer;
  LR : TRect;
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
        fpeClearCircles:
          DrawCustomEllipse(FForeground.Canvas, OuterRect, gap, gap_degree, gap_length);
        fpeFullOuterInnerCircles:
          DrawCustomEllipse(FForeground.Canvas, OuterRect, InnerRect, gap, gap_degree, gap_length);
      end;
end;

procedure TFPE.Play(ACorrection: Boolean);
var
  s1: string;
  LOuterR : TRect;
  i, LWidth, LHeight, LNumComp : Integer;
  LCircles : array of TCircle;

begin
  inherited Play(ACorrection);
  FFeaturesToDraw := TFPEDrawing(StrToIntDef(CfgTrial.SList.Values[_DrawingType], 0));

  FStyle := [];
  s1 := CfgTrial.SList.Values[_Style];
  for i := 1 to WordCount(s1,[#32]) do
    FStyle += [StringToStyle(ExtractDelimited(i,s1,[#32]))];

  if FStyle = [] then
    FStyle := [gngPlayGo,gngPlayGoOnBeforeEnd];

  FSchedule := TSchedule.Create(self);
  with FSchedule do
    begin
      OnConsequence := @Consequence;
      OnResponse:= @Response;
      Kind := CfgTrial.SList.Values[_Schedule];
      Enabled := False;
    end;
  AddToClockList(FSchedule);

  // Use alias as defaults
  FContingency := CfgTrial.SList.Values[_Contingency];

  LNumComp := StrToIntDef(CfgTrial.SList.Values[_NumComp], 1);
  SetLength(LCircles, LNumComp);
  for i := 0 to LNumComp -1 do
    begin
        s1:= CfgTrial.SList.Values[_Comp + IntToStr(i + 1) + _cBnd];
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
              fpeClearCircles: {do nothing};
              fpeFullOuterInnerCircles:InnerRect := GetInnerRect(LOuterR, LWidth, LHeight);
            end;
            gap := StrToBoolDef(CfgTrial.SList.Values[_Comp + IntToStr(i+1) + _cGap], False );
            gap_degree := 16 * StrToIntDef(CfgTrial.SList.Values[_Comp + IntToStr(i + 1) + _cGap_Degree], 1+Random(360));
            gap_length := 16 * StrToIntDef(CfgTrial.SList.Values[_Comp + IntToStr(i + 1) + _cGap_Length], 5 );
          end;
      end;

  if not TBackground(Self.Parent).DrawMask then
    TBackground(Self.Parent).DrawMask:=True;

  DrawForeground(LCircles, FFeaturesToDraw);

  if Self.ClassType = TFPE then Config(Self);
end;

procedure TFPE.TrialStart(Sender: TObject);
begin
  FConsequenceFired := False;
  FDataSupport.Latency := TimeStart;
  FDataSupport.StmBegin := TickCount;
  Invalidate;
end;

procedure TFPE.WriteData(Sender: TObject);
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
           FContingency;

  if Assigned(OnTrialWriteData) then OnTrialWriteData(Self);
end;

procedure TFPE.Response(Sender: TObject);
begin
  Inc(FDataSupport.Responses);
  if FDataSupport.Latency = TimeStart then
    FDataSupport.Latency := TickCount;

  if Assigned(CounterManager.OnStmResponse) then CounterManager.OnStmResponse(Sender);
  if Assigned(OnStmResponse) then OnStmResponse (Self);
end;

end.

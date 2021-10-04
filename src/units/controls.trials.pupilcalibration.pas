{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.PupilCalibration;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Classes, SysUtils, Graphics, ExtCtrls, Controls

  , Controls.Trials.Abstract
  , Controls.Trials.Helpers
  {$IFNDEF NO_LIBZMQ}
  , Pupil.Client
  {$ENDIF}
  ;

type

  TDot = record
    X : integer;
    Y : integer;
    Size : integer;
  end;

  { TCLB }

  {
    Presents calibration dots
    Starts Pupil Calibration
    Reacts to successful Pupil Calibration
  }
  TCLB = class(TTrial)
  private
    FTimer : TTimer;
    FBlocking,
    FShowDots : Boolean;
    FDots : array of TDot;
    FDataSupport : TDataSupport;
    procedure StartPupilCalibration;
    procedure None(Sender: TObject);
    procedure HideCalibrationFailedMessage(Sender: TObject);
    {$IFNDEF NO_LIBZMQ}
    procedure PupilCalibrationSuccessful(Sender: TObject; APupilMessage : TPupilMessage);
    procedure PupilCalibrationFailed(Sender: TObject; APupilMessage : TPupilMessage);
    {$ENDIF}
    procedure TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrialStart(Sender: TObject);
    procedure TrialBeforeEnd(Sender: TObject);
    procedure TrialPaint;
  protected
    procedure WriteData(Sender: TObject); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TCustomControl); override;
    procedure Play(ACorrection : Boolean); override;
  end;



implementation

uses strutils, constants, timestamps, Session.Configuration.GlobalContainer
    {$IFNDEF NO_LIBZMQ}
    , background
    {$ENDIF}
    ;

{ TCLB }

{$IFNDEF NO_LIBZMQ}
procedure TCLB.PupilCalibrationSuccessful(Sender: TObject;
  APupilMessage: TPupilMessage);
begin
  FrmBackground.Show;
  FrmBackground.SetFullScreen(True);
  if not FBlocking then
    EndTrial(Self);
end;

procedure TCLB.PupilCalibrationFailed(Sender: TObject;
  APupilMessage: TPupilMessage);
begin
  FTimer.Enabled:=True;
  Invalidate;
end;

{$ENDIF}

procedure TCLB.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = 67) { c } then
    if GlobalContainer.PupilEnabled then
      StartPupilCalibration
    else
      EndTrial(Self);

  if (ssCtrl in Shift) and (Key = 80) { p } then
end;

procedure TCLB.StartPupilCalibration;
begin
  {$IFNDEF NO_LIBZMQ}
  GlobalContainer.PupilClient.Request(REQ_SHOULD_START_CALIBRATION, True);
  {$ELSE}
  EndTrial(Self);
  {$ENDIF}
  //FrmBackground.Hide;
end;

procedure TCLB.None(Sender: TObject);
begin
  { Implement an OnNone event here }
  if Assigned(OnNone) then OnNone(Sender);
end;

procedure TCLB.HideCalibrationFailedMessage(Sender: TObject);
begin
  FTimer.Enabled:=False;
  Invalidate;
end;

procedure TCLB.TrialBeforeEnd(Sender: TObject);
begin
  // Trial Result
  Result := 'NONE';
  IETConsequence := 'NONE';
  FDataSupport.StmEnd := TickCount;

  // Write Data
  WriteData(Sender);
end;

procedure TCLB.TrialStart(Sender: TObject);
begin
  FDataSupport.StmBegin := TickCount;
  if GlobalContainer.PupilEnabled then
    StartPupilCalibration;
end;

procedure TCLB.TrialPaint;
var
  aleft, atop, asize, i : integer;
  LRect: TRect;
  S: String;
  Sw : integer = 0;
  Sh : integer = 0;
begin
  if FShowDots then
    with Canvas do
      for i := Low(FDots) to High(FDots) do
      begin
        with FDots[i] do
        begin
          aleft := X;
          atop  := Y;
          asize := Size;
        end;
        S := IntToStr(i+1);
        LRect := Rect(aleft, atop, aleft + asize, atop + asize);
        Brush.Style:=bsSolid;
        Ellipse(LRect);

        GetTextSize(S, Sw, Sh);
        aLeft := LRect.Left+(asize div 2)-(Sw div 2);
        aTop := LRect.Top+(asize div 2)-(Sh div 2);
        Brush.Style:=bsClear;
        TextOut(aleft,atop,S);
      end;

  if FTimer.Enabled then
  begin
    Canvas.Font.Size:=30;
    Canvas.TextOut(0,0,'A calibragem falhou.');
  end;

end;

procedure TCLB.WriteData(Sender: TObject);
begin
  inherited WriteData(Sender);
  Data := Data + #9 +
          TimestampToStr(FDataSupport.StmBegin - TimeStart) + #9 +
          TimestampToStr(FDataSupport.StmEnd - TimeStart) + #9 +
          IntToStr(Length(FDots));
  //if Assigned(OnTrialWriteData) then OnTrialWriteData(Self);
end;

procedure TCLB.Paint;
begin
  inherited Paint;

end;

constructor TCLB.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialKeyUp := @TrialKeyUp;
  OnTrialStart := @TrialStart;
  OnTrialPaint:=@TrialPaint;
  FShowDots := False;

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 4000;
  FTimer.OnTimer:=@HideCalibrationFailedMessage;
  with Canvas do
    begin
      Brush.Color := clBlack;
      Pen.Color   := clBlack;

      Brush.Style := bsSolid;
      Pen.Style   := psSolid;
      //Pen.Mode    := pmBlack;

      Font.Size:=20;
      Font.Color:=clWhite;
    end;

  Header := rsReportStmBeg + #9 +
            rsReportStmEnd + #9 +
            '____Dots';
end;

procedure TCLB.Play(ACorrection: Boolean);
var
  s1 : string;
  NumComp,
  i : integer;
  Parameters : TStringList;
begin
  inherited Play(ACorrection);
  Parameters := Configurations.Parameters;
  NumComp := StrToIntDef(Parameters.Values[_NumComp], 0);
  FShowDots := StrToBoolDef(Parameters.Values[_ShowDots], False);
  FBlocking := StrToBoolDef(Parameters.Values[_Blocking], False);
  if NumComp > 0 then
    begin
      SetLength(FDots, NumComp);
      for i := Low(FDots) to High(FDots) do
        begin
          s1 := Parameters.Values[_Comp + IntToStr(i + 1) + _cBnd] + #32;
          FDots[i].Y := StrToIntDef(ExtractDelimited(1,s1,[#32]), 0); // top, left, width, height
          FDots[i].X := StrToIntDef(ExtractDelimited(2,s1,[#32]), 0);
          FDots[i].Size := StrToIntDef(ExtractDelimited(3,s1,[#32]), 0);
        end;
    end;

  {$IFNDEF NO_LIBZMQ}
  if GlobalContainer.PupilEnabled then
  begin
    GlobalContainer.PupilClient.OnCalibrationSuccessful := @PupilCalibrationSuccessful;
    GlobalContainer.PupilClient.OnCalibrationFailed := @PupilCalibrationFailed;
  end;
  {$ENDIF}


  if Self.ClassType = TCLB then Config(Self);
end;


end.


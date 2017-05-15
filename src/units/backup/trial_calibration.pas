{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit trial_calibration;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Classes, SysUtils
    {$IFNDEF NO_LIBZMQ}
    , pupil_communication
    {$ENDIF}
    , trial_abstract
    , Graphics
    ;

type

  { TODO -oRafael -ccalibration : Implement self driven calibration. }

  TDot = record
    X : integer;
    Y : integer;
    Size : integer;
  end;

  FDataSupport = record
    TrialBegin : Extended;
    TrialEnd : Extended;
    Dots : array of TDot;
  end;

  { TCLB }

  {
    Calibration Trial
  }
  TCLB = class(TTrial)
  private
    FBlocking,
    FShowDots : Boolean;
    FDataSupport : FDataSupport;
    procedure StartPupilCalibration;
    procedure None(Sender: TObject);
    {$IFNDEF NO_LIBZMQ}
    procedure PupilCalibrationSuccessful(Sender: TObject; AMultiPartMessage : TMPMessage);
    {$ENDIF}
    procedure TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrialStart(Sender: TObject);
    procedure TrialBeforeEnd(Sender: TObject);
    procedure TrialPaint;
  protected
    procedure WriteData(Sender: TObject); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Play(ACorrection : Boolean); override;
  end;



implementation

uses strutils, constants, timestamps, background;

{ TCLB }

{$IFNDEF NO_LIBZMQ}
procedure TCLB.PupilCalibrationSuccessful(Sender: TObject;
  AMultiPartMessage: TMPMessage);
begin
  FrmBackground.Show;
  FrmBackground.SetFullScreen(True);
  if not FBlocking then
    EndTrial(Self);
end;
{$ENDIF}

procedure TCLB.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (key = 67) { c } then
    if GlobalContainer.PupilEnabled then
      StartPupilCalibration
    else
      EndTrial(Self);
end;

procedure TCLB.StartPupilCalibration;
begin
  {$IFNDEF NO_LIBZMQ}
  GlobalContainer.PupilClient.Request(REQ_SHOULD_START_CALIBRATION, True);
  {$ENDIF}
  //FrmBackground.Hide;
end;

procedure TCLB.None(Sender: TObject);
begin
  { Implement an OnNone event here }
  if Assigned(OnNone) then OnNone(Sender);
end;

procedure TCLB.TrialBeforeEnd(Sender: TObject);
begin
  // Trial Result
  Result := 'NONE';
  IETConsequence := 'NONE';
  FDataSupport.TrialEnd := TickCount;

  // Write Data
  WriteData(Sender);
end;

procedure TCLB.TrialStart(Sender: TObject);
begin
  FDataSupport.TrialBegin := TickCount;
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
      for i := Low(FDataSupport.Dots) to High(FDataSupport.Dots) do
        begin
          with FDataSupport.Dots[i] do
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
end;

procedure TCLB.WriteData(Sender: TObject);
begin
  inherited WriteData(Sender);
  Data := Data + #9 +
          TimestampToStr(FDataSupport.TrialBegin - TimeStart) + #9 +
          TimestampToStr(FDataSupport.TrialEnd - TimeStart) + #9 +
          IntToStr(Length(FDataSupport.Dots));
  if Assigned(OnTrialWriteData) then OnTrialWriteData(Self);
end;

procedure TCLB.Paint;
begin
  inherited Paint;

end;

constructor TCLB.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialKeyUp := @TrialKeyUp;
  OnTrialStart := @TrialStart;
  OnTrialPaint:=@TrialPaint;
  FShowDots := False;

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

  Header := 'StmBegin' + #9 +
            '__StmEnd' + #9 +
            '____Dots';
end;

procedure TCLB.Play(ACorrection: Boolean);
var
  s1 : string;

  NumComp,
  i : integer;
begin
  inherited Play(ACorrection);

  NumComp := StrToIntDef(CfgTrial.SList.Values[_NumComp], 0);
  FShowDots := StrToBoolDef(CfgTrial.SList.Values[_ShowDots], False);
  FBlocking := StrToBoolDef(CfgTrial.SList.Values[_Blocking], False);
  if NumComp > 0 then
    begin
      SetLength(FDataSupport.Dots, NumComp);
      for i := Low(FDataSupport.Dots) to High(FDataSupport.Dots) do
        begin
          s1 := CfgTrial.SList.Values[_Comp + IntToStr(i + 1) + _cBnd] + #32;
          FDataSupport.Dots[i].Y := StrToIntDef(ExtractDelimited(1,s1,[#32]), 0); // top, left, width, height
          FDataSupport.Dots[i].X := StrToIntDef(ExtractDelimited(2,s1,[#32]), 0);
          FDataSupport.Dots[i].Size := StrToIntDef(ExtractDelimited(3,s1,[#32]), 0);
        end;
    end;

  {$IFNDEF NO_LIBZMQ}
  if GlobalContainer.PupilEnabled then
    GlobalContainer.PupilClient.OnCalibrationSuccessful := @PupilCalibrationSuccessful;
  {$ENDIF}

  if Self.ClassType = TCLB then Config(Self);
end;


end.


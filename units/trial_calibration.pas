{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit trial_calibration;

{$mode objfpc}{$H+}

interface

uses LCLIntf, Classes, SysUtils
    , pupil_communication
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
    procedure None(Sender: TObject);
    procedure PupilCalibrationSuccessful(Sender: TObject; AMultiPartMessage : TMPMessage);
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

uses constants, timestamps, background;

{ TCLB }

procedure TCLB.PupilCalibrationSuccessful(Sender: TObject;
  AMultiPartMessage: TMPMessage);
begin
  FrmBackground.Show;
  FrmBackground.SetFullScreen(True);
  if not FBlocking then
    EndTrial(Self);
end;


procedure TCLB.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (key = 67) { c } then
    begin
      if GlobalContainer.PupilEnabled then
        GlobalContainer.PupilClient.Request(REQ_SHOULD_START_CALIBRATION);
      FrmBackground.Hide;
    end;
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
end;

procedure TCLB.TrialPaint;
var
  aleft, atop, asize, i : integer;
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
          Ellipse(Rect(aleft, atop, aleft + asize, atop + asize));
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
  FShowDots := False;

  with Canvas do
    begin
      Brush.Color := clBlack;
      Pen.Color   := clBlack;

      Brush.Style := bsSolid;
      Pen.Style   := psSolid;
      Pen.Mode    := pmBlack;
    end;

  Header := 'StmBegin' + #9 +
            '__StmEnd' + #9 +
            '____Dots'
            ;
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
          FDataSupport.Dots[i].Y := StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0); // top, left, width, height
          NextSpaceDelimitedParameter(s1);

          FDataSupport.Dots[i].X := StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
          NextSpaceDelimitedParameter(s1);

          FDataSupport.Dots[i].Size := StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
        end;
    end;

  if GlobalContainer.PupilEnabled then
    with GlobalContainer.PupilClient do
      begin
      OnCalibrationSuccessful := @PupilCalibrationSuccessful;
      Request(REQ_SHOULD_START_CALIBRATION);
      end;

  if Self.ClassType = TCLB then Config(Self);
end;


end.


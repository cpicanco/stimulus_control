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
    , trial_abstract
    , Graphics
    , constants
    , timestamps
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
    FShowDots : Boolean;
    FDataSupport : FDataSupport;
  protected
    procedure None(Sender: TObject);
    procedure ThreadClock(Sender: TObject); //override;
    procedure StartTrial(Sender: TObject); override;
    procedure WriteData(Sender: TObject); override;

    { TCustomControl overrides }
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Play(Correction : Boolean); override;
  end;



implementation

{ TCLB }

procedure TCLB.None(Sender: TObject);
begin
  { This trial type does not implement an OnNone event }
  if Assigned(OnNone) then OnNone(Sender);
end;

procedure TCLB.ThreadClock(Sender: TObject);
begin
  Hide;
  FDataSupport.TrialEnd := TickCount;
  WriteData(Sender);

  if Assigned(OnWriteTrialData) then OnWriteTrialData (Self);
  if Assigned(OnEndTrial) then OnEndTrial(Sender);
end;

procedure TCLB.StartTrial(Sender: TObject);
begin
  FShowDots := True;
  FDataSupport.TrialBegin := TickCount;
  inherited StartTrial(Sender);
end;

procedure TCLB.WriteData(Sender: TObject);
begin
  Data := //Format('%-*.*d', [4,8,CfgTrial.Id + 1]) + #9 +
           FloatToStrF(FDataSupport.TrialBegin - TimeStart, ffFixed, 0,9) + #9 +
           FloatToStrF(FDataSupport.TrialEnd - TimeStart, ffFixed, 0,9) + #9 +
           IntToStr(Length(FDataSupport.Dots)) +
           Data;

end;

procedure TCLB.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if (Key = 27 {ESC}) and (FShowDots = True) then
    begin
      FShowDots:= False;
      Invalidate;
    end;
end;

procedure TCLB.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);

  if Key = 27 {ESC} then
    begin
      FShowDots := True;
      Invalidate;
    end;

  if ssCtrl in Shift then
    begin
      if key = 66 {b} then
        begin
          Result := 'NONE';
          IETConsequence := 'NONE';
          NextTrial := '0'; // NextTrial
          EndTrial(Self)
        end;

      if key = 67 {c} then // start pupil calibration
        begin
          Result := 'NONE';
          IETConsequence := 'NONE';
          NextTrial := '0'; // NextTrial
          EndTrial(Self)
        end;
    end;
end;

procedure TCLB.Paint;
var
  aleft, atop, asize,
  i : integer;

begin
  inherited Paint;

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

constructor TCLB.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

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

procedure TCLB.Play(Correction: Boolean);
var
  s1 : string;

  NumComp,
  i : integer;

begin
  NumComp := StrToIntDef(CfgTrial.SList.Values[_NumComp], 0);
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
  StartTrial(Self);
end;


end.


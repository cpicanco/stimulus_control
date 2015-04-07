//
// Validation Project (PCRF) - Stimulus Control App
// Copyright (C) 2014-2015,  Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.
//
// cpicanco@ufpa.br
//
// This file is part of Validation Project (PCRF).
//
// Validation Project (PCRF) is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Validation Project (PCRF) is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Validation Project (PCRF).  If not, see <http://www.gnu.org/licenses/>.
//
unit trial_calibration;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, Classes, SysUtils
  , trial_abstract
  , Graphics
  , constants
  ;

type

  TDot = record
    X : integer;
    Y : integer;
    Size : integer;
  end;

  FDataSupport = record
    StmBegin : cardinal;
    StmEnd : cardinal;
    Dots : array of TDot;
  end;

  { TCLB }

  TCLB = class(TTrial)
  private
    { private declarations }
    FShowDots : Boolean;
    FCurrTrial : FDataSupport;
  protected
    procedure None(Sender: TObject);
    //procedure ThreadClock(Sender: TObject); override;
    procedure StartTrial(Sender: TObject); override;
    procedure EndTrial(Sender: TObject);
    procedure WriteData(Sender: TObject); override;

    { TCustomControl overrides }
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Play(TestMode: Boolean; Correction : Boolean); override;
  end;



implementation

{ TCLB }

procedure TCLB.None(Sender: TObject);
begin
  { This trial type does not implement an OnNone event }
  if Assigned(OnNone) then OnNone(Sender);
end;

procedure TCLB.StartTrial(Sender: TObject);
begin
  { This trial type does not requires a timer }
  //inherited StartTrial(Sender);
  FShowDots := True;
  FCurrTrial.StmBegin := GetTickCount;
end;

procedure TCLB.EndTrial(Sender: TObject);
begin
  FCurrTrial.StmEnd := GetTickCount;
  WriteData(Sender);

  if Assigned(OnWriteTrialData) then OnWriteTrialData (Self);
  if Assigned(OnEndTrial) then OnEndTrial(Sender);
end;

procedure TCLB.WriteData(Sender: TObject);
begin
  Data := //Format('%-*.*d', [4,8,CfgTrial.Id + 1]) + #9 +
           FormatFloat('00000000;;00000000', FCurrTrial.StmBegin - TimeStart) + #9 +
           FormatFloat('00000000;;00000000', FCurrTrial.StmEnd - TimeStart) + #9 +
           IntToStr(Length(FCurrTrial.Dots)) +
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

  if key = 98 {b} then
    begin
      Result := 'NONE';
      IETConsequence := 'NONE';
      NextTrial := '0'; // NextTrial
      EndTrial(Self)
    end;
end;

procedure TCLB.Paint;
var
  aRect : TRect;

  aleft, atop, asize,
  i : integer;

begin
  inherited Paint;

  if FShowDots then
    with Canvas do
      for i := Low(FCurrTrial.Dots) to High(FCurrTrial.Dots) do
        begin
          aleft := FCurrTrial.Dots[i].X;
          atop  := FCurrTrial.Dots[i].Y;
          asize := FCurrTrial.Dots[i].Size;

          aRect := Rect(aleft, atop, aleft + asize, atop + asize);
          Ellipse(aRect);
        end;
end;

constructor TCLB.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FShowDots := False;

  with Canvas do
    begin
      Brush.Color := clBlack;
      Brush.Style := bsSolid;
      Pen.Color   := clBlack;
      Pen.Style   := psSolid;
      Pen.Mode    := pmCopy;
    end;

  Header := 'StmBegin' + #9 +
            '__StmEnd' + #9 +
            '____Dots'
            ;
end;

procedure TCLB.Play(TestMode: Boolean; Correction: Boolean);
var
  s1 : string;

  NumComp,
  i : integer;

  procedure NextSpaceDelimitedParameter;
  begin
    Delete(s1, 1, pos(#32, s1));
    if Length(s1) > 0 then while s1[1] = #32 do Delete(s1, 1, 1);
  end;

begin
  NumComp := StrToIntDef(CfgTrial.SList.Values[_NumComp], 0);
  SetLength(FCurrTrial.Dots, NumComp);
  for i := Low(FCurrTrial.Dots) to High(FCurrTrial.Dots) do
    begin
      s1 := CfgTrial.SList.Values[_Comp + IntToStr(i + 1) + _cBnd] + #32;
      FCurrTrial.Dots[i].X := StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);

      NextSpaceDelimitedParameter;
      FCurrTrial.Dots[i].Y := StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);

      NextSpaceDelimitedParameter;
      FCurrTrial.Dots[i].Size := StrToIntDef(Copy(s1, 0, pos(#32, s1)-1), 0);
    end;
  StartTrial(Self);
end;

end.


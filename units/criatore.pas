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
unit criatore;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls,
     ExtCtrls, Forms, Dialogs

     , config_session
     ;

type

  TPositions = array of TCoordenates;

  TPointer = class (TObject)
    public
    FPIndex : ShortInt;
    FP : array [0..1] of TPoint;
    FCapturing : Boolean;
    FFocused : Boolean;
    FMouseDownSpot : TPoint;
  end;

  { TDrawCoordenates }

  TDrawCoordenates = class(TObject)
    strict private
      FBackGround : TForm;
      FPointer    : TPointer;
      FResolution : TPointer;
      FPanel      : TPanel;
      FDistribuir : Boolean;
      fH          : Integer; // Altura do Monitor
      fW          : Integer; // Comprimento do Monitor
      Distx       : Integer; // Distância horizontal;
      Disty       : Integer; // Distância vertical entre estímulos;
      sH          : Integer; // Altura do S
      sW          : Integer; // Comprimento do S
      ni          : Integer; // n Linhas
      nj          : Integer; // n Colunas
      Yk          : Integer; // Coordenada Left
      Xv          : Integer; // Coordenada Top
    protected
      FOnDraw : TNotifyEvent;
      FOnChange : TNotifyEvent;
      FString     : TStringList;
      FOutputString : TStrings;
    strict protected
      //Background
      procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure FormKeyPress(Sender: TObject; var Key: char);
      procedure FormPaint(Sender: TObject);
      //Panel
      procedure FocusPanel(Sender : TPanel);
      procedure PanelMouseMove(Sender: TObject; Shift: TShiftState; X,  Y: Integer);
      procedure PanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure PanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure PanelDblClick(Sender: TObject);
      procedure SetBackGroundFeatures;
      procedure SetPanelSizePosition (X, Y, W, H : Integer);
      procedure SetPanelFeatures (Count : Integer);
    public
      constructor Create(aBackGround : TForm; OutputStrings : TStrings);
      destructor Destroy; override;
      procedure ClearAll;
      procedure SetVariables (SDistx, SDisty, SsW, SsH, Sni, Snj, SLeft, STop: string);
      procedure DrawStmFromCoordenates;
      property BackGround : TForm read FBackGround write FBackGround;
      property Distribuido : Boolean read FDistribuir write FDistribuir;
      property Items : TStringList read FString write FString;
      property OnDraw : TNotifyEvent read FOnDraw write FOnDraw;
      property OnChange : TNotifyEvent read FOnChange write FOnChange;
  end;

  { TAleatorizator }

  TAleatorizator  = class (TDrawCoordenates)
    strict private
        FPositions : TPositions;
        procedure GetPositionsFromForm (Sender: TObject);
        procedure GetCoordenatesFromForm (Sender: TObject);
        procedure DrawPositionsToForm (ArraySize : Integer);
    public
        constructor Create (aBackGround : TForm; OutputStrings : TStrings);
        //procedure SetWriter (Escritor : TEscriba);
        function GetPositions : TPositions;
        procedure RandomizePositions (CanDraw : Boolean);
  end;

implementation

constructor TDrawCoordenates.Create(aBackGround : TForm; OutputStrings : TStrings);
begin
  BackGround := aBackGround;
  SetBackGroundFeatures;
  FOutputString := OutputStrings;
  FString := TStringList.Create;
  FPointer := TPointer.Create;
  FResolution := TPointer.Create;
end;


destructor TDrawCoordenates.Destroy;
begin
  FString.Free;
  FPointer.Free;
  FResolution.Free;
  ClearAll;
  inherited Destroy;
end;

procedure TDrawCoordenates.ClearAll;
var k : Integer;
begin
  FPointer.FFocused := False;
  K := 0;
  if Assigned(BackGround) then
  begin
    with  BackGround do
      while ComponentCount > 0 do
        if Components[k] is TPanel then TPanel(Components[k]).Free;
    BackGround.Invalidate;
  end;

end;

procedure TDrawCoordenates.PanelDblClick(Sender: TObject);
begin
    TPanel(Sender).Height := TPanel(Sender).Width;
end;

procedure TDrawCoordenates.DrawStmFromCoordenates;
var
  v, k, p : integer;
begin
  begin
    p   := 0;
    if FDistribuir then
      begin
        for v  := 1 to ni do
          begin
            Xv := (Disty * v) - Round (sH / 2);
            for  k   := 1 to nj do
              begin
                Yk  := (Distx * k) - Round (sW / 2);
                Inc (p);
                SetPanelFeatures (p);
                SetPanelSizePosition (Xv, Yk, sW, sH);
              end;
          end;
      end
    else
      begin
        for v  := 0 to ni - 1 do
          begin
            Xv :=  ((sH + Disty) * v) +
              Round (fH / 2) -
              Round (((sH + Disty) * ni) / 2) +
              Round ((Disty) / 2);
            for  k   := 0 to nj - 1 do
              begin
                Yk  := ((sW + Distx) * k) +
                  Round (fW / 2) -
                  Round (((sW + Distx) * nj) / 2) +
                  Round ((Distx) / 2);
                Inc(p);
                SetPanelFeatures (p);
                SetPanelSizePosition (Xv, Yk, sW, sH);
              end;
          end;

      end;
    if Assigned (OnDraw) then FOnDraw (Self);
  end;
end;

procedure TDrawCoordenates.FocusPanel(Sender: TPanel);
begin
  FPanel := TPanel(Sender);
  FPointer.FP[0].X := TPanel(Sender).Left;
  FPointer.FP[0].Y := TPanel(Sender).Top;
  FPointer.FP[1].X := TPanel(Sender).Left + TPanel(Sender).Width;
  FPointer.FP[1].Y := TPanel(Sender).Top + TPanel(Sender).Height;
end;

procedure TDrawCoordenates.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  if Button = mbLeft then
    begin
        FPointer.FCapturing := True;
        FPointer.FPIndex := -1;
        for I := 0 to Length(FPointer.FP) - 1 do
          begin
            if (FPointer.FP[I].X - X)*(FPointer.FP[I].X - X) +
               (FPointer.FP[I].Y - Y)*(FPointer.FP[I].Y - Y) < 30 then
            begin
              FPointer.FPIndex := I;
              Break;
            end
          else FPointer.FFocused := False;
          end;
        FormMouseMove(Sender, Shift, X, Y);
    end;
end;

procedure TDrawCoordenates.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  i : integer;
begin
  if (FPointer.FPIndex >= 0) and FPointer.FCapturing then
    begin
        if (FPointer.FPIndex = 0) then
          begin
            FPointer.FP[FPointer.FPIndex].X := X;
            FPointer.FP[FPointer.FPIndex].Y := Y;
          end;
        if (FPointer.FPIndex = 1) then
          begin
            FPointer.FP[FPointer.FPIndex].X := X;
            FPointer.FP[FPointer.FPIndex].Y := Y;
          end;
      FPanel.BoundsRect := Rect(FPointer.FP[0].x,FPointer.FP[0].y, FPointer.FP[1].x, FPointer.FP[1].y);
      TForm(sender).Invalidate;
    end
  else
    begin
      for I := 0 to Length(FPointer.FP) - 1 do
        begin
          if (FPointer.FP[I].X - X)*(FPointer.FP[I].X - X) +
             (FPointer.FP[I].Y - Y)*(FPointer.FP[I].Y - Y) < 30 then
            begin
              //FPointer.FFocused := True;
              with TForm(Sender) do
                begin
                  Cursor := crCross;
                  Invalidate;
                end;
              Exit;
            end;
      FPointer.FFocused := False;
      TForm(Sender).Cursor := crDefault;
        end;
    end;
end;

procedure TDrawCoordenates.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FPointer.FCapturing := False;
  TForm(sender).Invalidate;
end;

procedure TDrawCoordenates.FormKeyPress(Sender: TObject; var Key: char);
var i : integer;
begin
  if key = #32 then
    for i := 0 to Application.ComponentCount -1 do
      if Application.Components[i].ClassName = 'TMatrixConfigForm' then
        TForm(Application.Components[i]).BringToFront;
end;

procedure TDrawCoordenates.FormPaint(Sender: TObject);
  procedure PaintElypses;
   var i:Integer;
   begin
     if Sender is TForm then
     with TForm(Sender).Canvas do
       begin
         Pen.Width := 1;
         Pen.Color := TForm(Sender).Color;
         Pen.Mode := pmNotXor;
         Brush.Style := bsClear;
         for i := 0 to Length(FPointer.FP) - 1 do
           Ellipse(FPointer.FP[i].x - 10, FPointer.FP[i].Y - 10,
                   FPointer.FP[i].x + 10, FPointer.FP[i].Y + 10);
       end;
   end;

   procedure PaintResolutionSquare;
   begin
     if Sender is TForm then
     with TForm(Sender).Canvas do
       begin
         Pen.Width := 1;
         Pen.color := clRed;
         Pen.Mode := pmCopy;
         Brush.Style := bsClear;
         Rectangle(FResolution.FP[0].X, FResolution.FP[0].Y,
                   FResolution.FP[1].X, FResolution.FP[1].Y);
       end;
   end;
begin
  PaintResolutionSquare;

  if FPointer.FFocused then
    PaintElypses;

  if Sender is TForm then
  with TForm(Sender).Canvas do
    begin
      Pen.Mode := pmCopy;
      Brush.style := bsSolid;
    end;
end;

procedure TDrawCoordenates.PanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Sender is TPanel then
    begin
      if Button = mbLeft then
        begin
          FPointer.FCapturing := True;
          FPointer.FFocused := True;
          FPointer.FMouseDownSpot.X := X;
          FPointer.FMouseDownSpot.Y := Y;
          PanelMouseMove(Sender, Shift, X, Y);
        end;
    end;
end;

procedure TDrawCoordenates.PanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var i: integer; s1 : string;
begin
  if Sender is TPanel then
    begin
      FPointer.FFocused := True;
      if FPointer.FCapturing then
        begin

          TPanel(Sender).Left := TPanel(Sender).Left -(FPointer.FMouseDownSpot.X -X);
          TPanel(Sender).Top := TPanel(Sender).Top - (FPointer.FMouseDownSpot.Y -Y);
          s1 := IntToStr(TPanel(Sender).Top)    + #32 +
                IntToStr(TPanel(Sender).Left)   + #32 +
                IntToStr(TPanel(Sender).Width)  + #32 +
                IntToStr(TPanel(Sender).Height) + '*';
          TPanel(Sender).Hint := s1;
          FOutputString.Strings [TPanel(Sender).Tag - 1] := s1;

              FPointer.FP[0].X := TPanel(Sender).Left;
              FPointer.FP[0].Y := TPanel(Sender).Top;
              FPointer.FP[1].X := TPanel(Sender).Left + TPanel(Sender).Width;
              FPointer.FP[1].Y := TPanel(Sender).Top + TPanel(Sender).Height;

          TPanel(Sender).Parent.Invalidate;
          if ssCtrl in Shift then
          begin
            with TPanel(Sender).Parent do
              begin
              for i := 0 to ComponentCount -1 do
                begin
                  if Components[i] is TPanel then
                      TPanel(Components[i]).Left := TPanel(Sender).Left;
                  if Components[i] is TPanel then
                      TPanel(Components[i]).Top := TPanel(Sender).Top;
                end;
                Invalidate;
              end;
          end;
        end;
    end;
end;

procedure TDrawCoordenates.PanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Sender is TPanel then
    if FPointer.FCapturing then
      begin
        FPointer.FCapturing := False;
        FocusPanel (TPanel(Sender));
        BackGround.Invalidate;
        if Assigned (OnChange) then FOnChange (Sender);
      end;
end;

procedure TDrawCoordenates.SetBackGroundFeatures;
begin
  BackGround.OnMouseDown:= @FormMouseDown;
  BackGround.OnMouseMove := @FormMouseMove;
  BackGround.OnMouseUp := @FormMouseUp;
  BackGround.OnPaint := @FormPaint;
  BackGround.OnKeyPress := @FormKeyPress;
end;

procedure TDrawCoordenates.SetPanelFeatures(Count: Integer);
begin
  FPanel := TPanel.Create(BackGround);
  FPanel.OnMouseDown := @PanelMouseDown;
  FPanel.OnMouseMove := @PanelMouseMove;
  FPanel.OnMouseUp := @PanelMouseUp;
  FPanel.OnDblClick := @PanelDblClick;
  FPanel.BevelInner := bvNone;
  FPanel.BevelOuter := bvNone;
  FPanel.Constraints.MinWidth := 1;
  FPanel.Constraints.MinHeight := 1;
  FPanel.Name := 's' + IntToStr(Count);
  FPanel.Tag := Count;
  FPanel.Parent := BackGround;
  FPanel.Visible:= False;
end;

procedure TDrawCoordenates.SetPanelSizePosition(X, Y, W, H : Integer);
begin
  FPanel.Top := X;
  FPanel.Left := Y;
  FPanel.Width := W;
  FPanel.Height := H;
  FPanel.Color := RGB (179, 207, 104);
  FPanel.ShowHint := True;
  FPanel.Hint := IntToStr(X) + #32 + IntToStr(Y) + #32 + IntToStr(W) + #32 + IntToStr(H);
  FPanel.Visible := True;

  FString.Add(Format('%d %d %d %d', [X, Y, W, H]));
end;

procedure TDrawCoordenates.SetVariables (SDistx, SDisty, SsW, SsH, Sni, Snj, SLeft, STop: string);
begin
    with BackGround do
      begin
        if SLeft = '-1' then
          begin
            fW  := ClientWidth    //Comprimento do monitor
          end
        else fW := StrToInt(SLeft);

        if STop = '-1' then
          fH  := ClientHeight   //Altura do Monitor
        else fH := StrToInt(STop);

        FResolution.FP[0].x := 0;  //left
        FResolution.FP[0].y := 0;  //top
        FResolution.FP[1].x := fW; //Right
        FResolution.FP[1].y := fH; //Bottom
        FResolution.FFocused := True;
        Invalidate;
      end;

  ni  := Abs (StrToInt (Sni)); // n Linhas
  nj  := Abs (StrToInt (Snj));  // n Colunas
  sW  := Abs (StrToInt (SsW));  // Comprimento do S
  sH  := Abs (StrToInt (SsH)); // Altura do S

  if FDistribuir = False then
    begin
      Disty    := Abs (StrToInt (SDisty)); //Distância vertical entre estímulos;
      Distx    := Abs (StrToInt (SDistx)); //Distância horizontal;
    end
  else
    begin
      Disty := Round (fH / (ni + 1));
      Distx := Round (fW / (nj + 1));
    end;
end;



{ TAleatorizator }


constructor TAleatorizator.Create(aBackGround : TForm; OutputStrings : TStrings);
begin
  inherited Create(aBackGround, OutputStrings);
  OnDraw := @GetPositionsFromForm;
  OnChange := @GetCoordenatesFromForm;
  SetLength(FPositions, 0);
end;

procedure TAleatorizator.RandomizePositions (CanDraw : Boolean);
var rnd1, ArraySize, n  : cardinal; Aux : TCoordenates;
begin
  ArraySize := Length(FPositions);
  if ArraySize > 1 then
    begin
      ClearAll;
      Randomize;
      for n := 0 to ArraySize - 1 do
        begin
          rnd1 := Round (Random * (ArraySize -1));
          while rnd1 = n do rnd1 := Round (Random * (ArraySize -1));
          Aux := FPositions [n];
          FPositions [n]:= FPositions [rnd1];
          FPositions [rnd1] := Aux;
        end;
      if CanDraw then DrawPositionsToForm(ArraySize);
    end;
end;

procedure TAleatorizator.GetCoordenatesFromForm (Sender: TObject);
var i, count: Cardinal;
begin
  count := 0;
  with BackGround do
    for i := 0 to ComponentCount - 1 do
        if Components[i] is TPanel then
          begin
            FPositions[count].Index := Components[i].Tag;
            FPositions[count].Top := TPanel(Components[i]).Top;
            FPositions[count].Left := TPanel(Components[i]).Left;
            FPositions[count].Width := TPanel(Components[i]).Width;
            FPositions[count].Height := TPanel(Components[i]).Height;
            Inc(count);
          end;
  end;

function TAleatorizator.GetPositions: TPositions;
begin
  Result := FPositions;
end;

procedure TAleatorizator.GetPositionsFromForm (Sender: TObject);
var i, count: Cardinal;
begin
  FOutputString.Text := FString.Text;
  FString.Clear;
  count := 0;
  with BackGround do
    for i := 0 to ComponentCount - 1 do
      if Components[i] is TPanel then Inc (Count);
  if Count > 0 then
    begin
      SetLength(FPositions, Count);
      if Assigned (OnChange) then FOnChange (Sender);
    end;
end;

procedure TAleatorizator.DrawPositionsToForm(ArraySize: Integer);
var n : Integer;
begin
  for n := 0 to ArraySize - 1 do
  begin
    SetPanelFeatures (n + 1);
    SetPanelSizePosition (FPositions[n].Top,
                          FPositions[n].Left,
                          FPositions[n].Width,
                          FPositions[n].Height);
  end;

  if Assigned (OnDraw) then FOnDraw (Self);
end;


end.

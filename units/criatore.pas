{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit criatore;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls,
     ExtCtrls, Forms, Dialogs

     , config_session
     ;

type

  TPositions = array of TCoordenates;

  { TCorner }

  TCorner = class(TGraphicControl)
  private
    FCenter: TPoint;
    procedure SetCenter(AValue: TPoint);
    procedure CornerChangeBounds(Sender:TObject);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Center : TPoint read FCenter write SetCenter;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  { TPointer }

  TPointer = class(TObject)
  private
    FAnchoredControl : TCustomControl;
    FDragging: Boolean;
    FMouseInside : TCorner;
    FCorner : array [0..1] of TCorner;
    FMouseDownSpot : TPoint;
    FVisible: Boolean;
    procedure ChangeAnchoredControlBounds;
    function GetAnchored: Boolean;
    procedure SetVisible(AValue: Boolean);
    procedure CornerMoveTo(Sender: TObject; X,Y : integer);
    procedure CornerMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
    procedure CornerMouseUp(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
    procedure CornerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    property MouseDownSpot : TPoint read FMouseDownSpot write FMouseDownSpot;
  public
    constructor Create(ABackGround : TForm);
    destructor Destroy; override;
    procedure AnchorToControl(AControl: TCustomControl);
    procedure MoveToControl(AControl: TCustomControl);
    property MouseInside : TCorner read FMouseInside;
    property Anchored : Boolean read GetAnchored;
    property Dragging : Boolean read FDragging write FDragging;
    property Visible : Boolean read FVisible write SetVisible;
  end;

  { TDrawCoordenates }

  TDrawCoordenates = class(TObject)
  strict private
    FMouseDownSpot: TPoint;
    FDragging : Boolean;
    FBackGround : TForm;
    FResolution : TRect;
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
    FPointer    : TPointer;
    FPanel      : TPanel;
    FOnDraw : TNotifyEvent;
    FOnChange : TNotifyEvent;
    FString     : TStringList;
    FOutputString : TStrings;
  strict protected
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormPaint(Sender: TObject);
    //Panel
    procedure FocusPanel(Sender : TPanel);
    procedure PanelMoveto(Sender: TObject; X,  Y: Integer);
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

  TAleatorizator = class(TDrawCoordenates)
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

{ TCorner }

procedure TCorner.Paint;
begin
  inherited Paint;
  Canvas.Ellipse(0, 0, Width, Height);
end;

procedure TCorner.SetCenter(AValue: TPoint);
begin
  if (FCenter.X = AValue.X) and (FCenter.Y = AValue.Y) then Exit;
  FCenter:=AValue;
  Left := AValue.X - (Width div 2);
  Top := AValue.Y - (Height div 2);
end;

procedure TCorner.CornerChangeBounds(Sender: TObject);
begin
  // Center := Point(Left + (Width div 2), Top + (Height div 2));
end;

constructor TCorner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clGray;
  //OnChangeBounds := @CornerChangeBounds;
  with Canvas do
    begin
      Pen.Width := 1;
      Pen.Mode := pmWhite;
      Brush.Style := bsClear;
    end;
  Width  := 10;
  Height := 10;
  Center := Point(5, 5);
  Invalidate;
end;

{ TPointer }

procedure TPointer.MoveToControl(AControl: TCustomControl);
begin
  with AControl do
    begin
      FCorner[0].Center := Point(Left, Top);
      FCorner[1].Center := Point(Left + Width, Top + Height);
    end;
end;

procedure TPointer.ChangeAnchoredControlBounds;
begin
  if Anchored then
    FAnchoredControl.BoundsRect := Rect(FCorner[0].Center.X,
                                        FCorner[0].Center.Y,
                                        FCorner[1].Center.X,
                                        FCorner[1].Center.Y);
end;

function TPointer.GetAnchored: Boolean;
begin
  Result := Assigned(FAnchoredControl);
end;

procedure TPointer.SetVisible(AValue: Boolean);
var i : integer;
begin
  //if FVisible=AValue then Exit;
  FVisible:=AValue;
  for i := 0 to 1 do
    FCorner[i].Visible:=FVisible;
end;

procedure TPointer.CornerMoveTo(Sender: TObject; X, Y: integer);
begin
  TCorner(Sender).Center := Point(TCorner(Sender).Center.X - (MouseDownSpot.X - X),
                                  TCorner(Sender).Center.Y - (MouseDownSpot.Y - Y));
end;

procedure TPointer.CornerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    begin
      MouseDownSpot := Point(X, Y);
      CornerMoveTo(Sender, X, Y);
      ChangeAnchoredControlBounds;
      Dragging := True;
    end;
end;

procedure TPointer.CornerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Dragging := False;
end;

procedure TPointer.CornerMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Dragging then
    begin
      CornerMoveTo(Sender, X, Y);
      ChangeAnchoredControlBounds;
    end;
end;

constructor TPointer.Create(ABackGround: TForm);
var i : integer;
begin
  inherited Create;
  FDragging := False;
  FMouseInside := nil;
  FAnchoredControl := nil;
  for i := 0 to 1 do
    begin
      FCorner[i] := TCorner.Create(ABackGround);
      with FCorner[i] do
        begin
          OnMouseDown := @CornerMouseDown;
          OnMouseMove:= @CornerMouseMove;
          OnMouseUp:= @CornerMouseUp;
          Cursor := crCross;
          Parent := ABackGround;
        end;
    end;
end;

destructor TPointer.Destroy;
var i : integer;
begin
  for i := 0 to 1 do
    FCorner[i].Free;
  inherited Destroy;
end;


procedure TPointer.AnchorToControl(AControl: TCustomControl);
begin
  if FAnchoredControl=AControl then Exit;
  FAnchoredControl:=AControl;
  MoveToControl(AControl);
  FCorner[0].BringToFront;
  FCorner[1].BringToFront;
end;

constructor TDrawCoordenates.Create(aBackGround : TForm; OutputStrings : TStrings);
begin
  FPointer := TPointer.Create(aBackGround);
  BackGround := aBackGround;
  SetBackGroundFeatures;
  FOutputString := OutputStrings;
  FString := TStringList.Create;
end;

destructor TDrawCoordenates.Destroy;
begin
  FString.Free;
  FPointer.Free;
  ClearAll;
  inherited Destroy;
end;

procedure TDrawCoordenates.ClearAll;
var
  i : integer;
  LIsClean : Boolean;
begin
  with BackGround do
    repeat
      LIsClean := True;
      for i := 0 to ComponentCount -1 do
        begin
          if Components[i] is TPanel then
            begin
              LIsClean := False;
              TPanel(Components[i]).Free;
              Break;
            end;
        end;
    until LIsClean;
  if Assigned(FPointer) then FPointer.Visible := False;
  BackGround.Invalidate;
end;

procedure TDrawCoordenates.PanelDblClick(Sender: TObject);
begin
  TPanel(Sender).Height := TPanel(Sender).Width;
end;

procedure TDrawCoordenates.DrawStmFromCoordenates;
var
  v, k, p : integer;
begin
  p := 0;
  if FDistribuir then
    begin
      for v := 1 to ni do
        begin
          Xv := (Disty * v) - Round(sH / 2);
          for  k   := 1 to nj do
            begin
              Yk  := (Distx * k) - Round(sW / 2);
              Inc(p);
              SetPanelFeatures(p);
              SetPanelSizePosition(Xv, Yk, sW, sH);
            end;
        end;
    end
  else
    begin
      for v  := 0 to ni - 1 do
        begin
          Xv := ((sH + Disty) * v) +
            Round(fH / 2) -
            Round(((sH + Disty) * ni) / 2) +
            Round((Disty) / 2);
          for  k   := 0 to nj - 1 do
            begin
              Yk := ((sW + Distx) * k) +
                Round(fW / 2) -
                Round(((sW + Distx) * nj) / 2) +
                Round((Distx) / 2);
              Inc(p);
              SetPanelFeatures(p);
              SetPanelSizePosition(Xv, Yk, sW, sH);
            end;
        end;
    end;
  if Assigned(OnDraw) then FOnDraw(Self);
end;

procedure TDrawCoordenates.FocusPanel(Sender: TPanel);
begin
  if Sender is TPanel then
    begin
      FPanel := TPanel(Sender);
      FPointer.AnchorToControl(FPanel);
    end;
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
var
  OldCanvas : TCanvas;

  procedure PaintResolutionSquare;
  begin
  with TForm(Sender).Canvas do
   begin
     Pen.Width := 1;
     Pen.color := clRed;
     Pen.Mode := pmCopy;
     Brush.Style := bsClear;
     Rectangle(FResolution);
   end;
  end;

  procedure SaveOldCanvas;
  begin
   with TForm(Sender).Canvas do
     begin
       OldCanvas.Brush.Color := Brush.Color;
       OldCanvas.Brush.Style := Brush.Style;
       OldCanvas.Pen.Color := Pen.Color;
       OldCanvas.Pen.Style := Pen.Style;
       OldCanvas.Pen.Mode := Pen.Mode;
     end;
  end;

  procedure LoadOldCanvas;
  begin
   with TForm(Sender).Canvas do
     begin
       Brush.Color := OldCanvas.Brush.Color;
       Brush.Style := OldCanvas.Brush.Style;
       Pen.Color := OldCanvas.Pen.Color;
       Pen.Style := OldCanvas.Pen.Style;
       Pen.Mode := OldCanvas.Pen.Mode;
     end;
  end;
begin
  if Sender is TForm then
    begin
      OldCanvas := TCanvas.Create;
      SaveOldCanvas;
      try
        PaintResolutionSquare;
      finally
        LoadOldCanvas;
        OldCanvas.Free;
      end;
    end;
end;

procedure TDrawCoordenates.PanelMoveto(Sender: TObject; X, Y: Integer);
var s1 : string;
begin
  TPanel(Sender).Left := TPanel(Sender).Left - (FMouseDownSpot.X -X);
  TPanel(Sender).Top := TPanel(Sender).Top - (FMouseDownSpot.Y -Y);
  s1 := IntToStr(TPanel(Sender).Top)    + #32 +
        IntToStr(TPanel(Sender).Left)   + #32 +
        IntToStr(TPanel(Sender).Width)  + #32 +
        IntToStr(TPanel(Sender).Height) + '*';
  TPanel(Sender).Hint := s1;
  FOutputString.Strings[TPanel(Sender).Tag - 1] := s1;
  FPointer.MoveToControl(TPanel(Sender));
end;

procedure TDrawCoordenates.PanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Button = mbLeft then
    begin
      FDragging := True;
      FPointer.AnchorToControl(TPanel(Sender));
      FMouseDownSpot := Point(X, Y);
      PanelMoveTo(Sender, X, Y);
    end;
end;

procedure TDrawCoordenates.PanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  if FDragging then
    begin
      PanelMoveTo(Sender, X, Y);
      if ssCtrl in Shift then
        with TPanel(Sender).Parent do
          begin
            for i := 0 to ComponentCount -1 do
              begin
                if Components[i] is TPanel then
                    TPanel(Components[i]).Left := TPanel(Sender).Left;
                if Components[i] is TPanel then
                    TPanel(Components[i]).Top := TPanel(Sender).Top;
              end;
              //Invalidate;
          end;
    end;
end;

procedure TDrawCoordenates.PanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if FDragging then
    begin
      FDragging := False;
      if Assigned(OnChange) then FOnChange(Sender);
    end;
end;

procedure TDrawCoordenates.SetBackGroundFeatures;
begin
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

procedure TDrawCoordenates.SetVariables(SDistx, SDisty, SsW, SsH, Sni, Snj, SLeft, STop: string);
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

      FResolution.Left := 0;  //left
      FResolution.Top := 0;  //top
      FResolution.Right := fW; //Right
      FResolution.Bottom := fH; //Bottom
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

procedure TAleatorizator.RandomizePositions(CanDraw : Boolean);
var
  rnd1, ArraySize, n  : cardinal;
  Aux : TCoordenates;
begin
  ArraySize := Length(FPositions);
  if ArraySize > 1 then
    begin
      ClearAll;
      Randomize;
      for n := 0 to ArraySize - 1 do
        begin
          rnd1 := Round(Random * (ArraySize -1));
          while rnd1 = n do rnd1 := Round(Random * (ArraySize -1));
          Aux := FPositions [n];
          FPositions [n]:= FPositions [rnd1];
          FPositions [rnd1] := Aux;
        end;
      if CanDraw then DrawPositionsToForm(ArraySize);
    end;
end;

procedure TAleatorizator.GetCoordenatesFromForm(Sender: TObject);
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

procedure TAleatorizator.GetPositionsFromForm(Sender: TObject);
var i, count: Cardinal;
begin
  FOutputString.Text := FString.Text;
  FString.Clear;
  count := 0;
  with BackGround do
    for i := 0 to ComponentCount - 1 do
      if Components[i] is TPanel then Inc(Count);
  if Count > 0 then
    begin
      SetLength(FPositions, Count);
      if Assigned(OnChange) then FOnChange(Sender);
    end;
  FPointer.Visible := True;
end;

procedure TAleatorizator.DrawPositionsToForm(ArraySize: Integer);
var n : Integer;
begin
  for n := 0 to ArraySize - 1 do
    begin
      SetPanelFeatures(n + 1);
      SetPanelSizePosition(FPositions[n].Top,
                           FPositions[n].Left,
                           FPositions[n].Width,
                           FPositions[n].Height);
    end;
  if Assigned(OnDraw) then FOnDraw(Self);
end;


end.

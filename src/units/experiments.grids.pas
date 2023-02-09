{todo: Transformar em classe}

unit Experiments.Grids;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TGridItem = record
    Index : integer;
    Top : integer;
    Left : integer;
    SquareSide : integer;
    Item : TObject;
  end;

  TGridItens = array of TGridItem;
  TMatrix = array of array of TGridItem;

  TRandomPositions = record
    Samples: TGridItens;
    Comparisons: TGridItens;
  end;

  TGrid = class
    private

    public

  end;

{Cria grade quadrada como uma matriz AN x AN. Quando ADistribute = true, a
distância horizontal e vertical entre os estímulos é diferente, e quando false
é igual}
function GetCentralGrid(AN: integer; ASquareSide: real;
  ADistribute : Boolean = True) : TMatrix;

{Cria grade circular considerando j como modelo central e i como comparações em
torno de um diâmetro. AN = número de estímulos i; ASquareSide = lado do quadrado
dos estímulos}
function GetCircularCentralGrid(AN: integer; ASquareSide: real): TMatrix;

function RectFromPosition(APosition : integer) : TRect;

{Cria seleção randômica de modelos e comparações em posições diferentes no AGrid}
function GetRandomPositionFromGrid(AGrid: TMatrix; ASamples: integer;
         AComparisons: integer): TRandomPositions;

{Randomiza as posições dos estímulos}
procedure RandomizePositions(var APositions: TRandomPositions; AGrid: TMatrix);

var
  ScreenInCentimeters : real = 39.624;
  Grid : TMatrix;
  RandomPositions: TRandomPositions;

implementation

uses Forms, GUI.Helpers.Grids;

{
  GetPositionFromSegment returns Left or Top position based on:
    ASegment = Width or Height from which get the Left or Top position.
    ASteps = Desired number os columns or rows.
    AStep = Target column or row.
    AStimulusSide = Width or height of the target stimulus.
    AInterStimulusSpace = Desired horizontal or vertical space from one stimulus to another.
}
function GetPositionFromSegment(ASegment, AStep, ASteps,
  AStimulusSide, AInterStimulusSpace : integer):integer;
var
  LSize : integer;
begin
  LSize := AStimulusSide + AInterStimulusSpace;
  Result := Round((LSize*AStep)-((LSize*ASteps)/2)+((ASegment+AInterStimulusSpace)/2));
end;

function CmToScreenPixels(AMeasure : real) : integer;
begin
  Result := Round(AMeasure*(Screen.Width/ScreenInCentimeters));
end;

{
  3x3
  0..1..2
  3..4..5
  6..7..8
}
function GetCentralGrid(AN: integer; ASquareSide: real;
  ADistribute: Boolean): TMatrix;
var
  LIndex      : integer = 0;
  //LSegment    : integer = 0;
  //LSteps      : integer = 0;
  //LStep       : integer = 0;
  LSquareSide : integer = 0;
  LInterSpaceW : integer = 0;
  LInterSpaceH : integer = 0;
  j : integer = 0;
  i : integer = 0;
begin
  Result := Default(TMatrix);
  SetLength(Result, AN, AN);
  LSquareSide := CmToScreenPixels(ASquareSide);
  if ADistribute then begin
    LInterSpaceW := (Screen.Width -  (LSquareSide * AN)) div AN;
    LInterSpaceH := (Screen.Height - (LSquareSide * AN)) div AN;
  end else begin
    if Screen.Width > Screen.Height then begin
      LInterSpaceH := (Screen.Height - (LSquareSide * AN)) div AN;
      LInterSpaceW := LInterSpaceH;
    end else begin
      LInterSpaceW := (Screen.Width -  (LSquareSide * AN)) div AN;
      LInterSpaceH := LInterSpaceW;
    end;
  end;
  for j := Low(Result) to High(Result) do begin
    for i := Low(Result[j]) to High(Result[j]) do begin
      with Result[j][i] do begin
        Index := LIndex;
        Top := GetPositionFromSegment(
          Screen.Height, j, AN, LSquareSide, LInterSpaceH);
        Left := GetPositionFromSegment(
          Screen.Width, i, AN, LSquareSide, LInterSpaceW);
        SquareSide := LSquareSide;
      end;
      Inc(LIndex);
    end;
  end;
end;

function GetCircularCentralGrid(AN: integer; ASquareSide: real): TMatrix;
var
  LIndex      : integer = 0;
  //LSegment    : integer = 0;
  //LSteps      : integer = 0;
  //LStep       : integer = 0;
  LSquareSide : integer = 0;
  LDegree : integer = 0;
  LDegreeI : integer = 0;
  LPoint : TPoint;
  LRect  : TRect;
  j : integer = 0;
  i : integer = 0;
const
  BaseDegree : integer = 360;
begin
  Result := Default(TMatrix);
  SetLength(Result, 2);
  SetLength(Result[0], AN);
  SetLength(Result[1], 1);
  LSquareSide := CmToScreenPixels(ASquareSide);
  LDegree := BaseDegree;
  LDegreeI := BaseDegree div AN;
  LRect := GetCentralRect(Screen.Width, Screen.Height, LSquareSide div 2);
  for j := Low(Result) to High(Result) do begin
    for i := Low(Result[j]) to High(Result[j]) do begin
      with Result[j][i] do begin
        case j of
          0:  begin
            Index := LIndex;
            LPoint := GetPointFromAngle(LDegree, LRect);
            Top := LPoint.Y - (LSquareSide div 2);
            Left := LPoint.X - (LSquareSide div 2);
            SquareSide := LSquareSide;
            Inc(LDegree, LDegreeI);
          end;

          1: begin
            Index := LIndex;
            Top := (Screen.Height div 2) - (LSquareSide div 2);
            Left := (Screen.Width div 2) - (LSquareSide div 2);
            SquareSide := LSquareSide;
          end;
        end;
      end;
      Inc(LIndex);
    end;
  end;
end;

function RectFromPosition(APosition: integer): TRect;
var
  j, i: Integer;
begin
  for j := Low(Grid) to High(Grid) do begin
    for i := Low(Grid[j]) to High(Grid[j]) do begin
      with Grid[j][i] do begin
        if Index = APosition then begin
          Result := Rect(Left, Top, Left+SquareSide, Top+SquareSide);
        end;
      end;
    end;
  end;
end;

function IsDifferentPosition(j, i: integer; APositions: TRandomPositions; AGrid: TMatrix): boolean;
var
  Item: TGridItem;
begin
  Result := true;

  for Item in APositions.Samples do
  begin
    if Item.Index = AGrid[j, i].Index then
    begin
      Result := false;
      break;
    end;
  end;

  for Item in APositions.Comparisons do
  begin
    if Item.Index = AGrid[j, i].Index then
    begin
      Result := false;
      break;
    end;
  end;
end;

function GetRandomItem(APositions: TRandomPositions; AGrid: TMatrix): TGridItem;
var
  i, j: integer;
begin
  repeat
    i := Random(Length(AGrid));
    j := Random(Length(AGrid));
  until IsDifferentPosition(j, i, APositions, AGrid);

  Result := AGrid[j, i];
end;

function GetRandomPositionFromGrid(AGrid: TMatrix; ASamples: integer;
         AComparisons: integer): TRandomPositions;
var
  RandomItem: TGridItem;
  i, j: integer;

begin
  SetLength(Result.Samples, ASamples);
  SetLength(Result.Comparisons, AComparisons);

  for i := low(Result.Samples) to high(Result.Samples) do
      Result.Samples[i].Index := -1;
  for i := low(Result.Comparisons) to high(Result.Comparisons) do
      Result.Comparisons[i].Index := -1;

  for i := low(Result.Samples) to high(Result.Samples) do
  begin
    RandomItem := GetRandomItem(Result, AGrid);
    Result.Samples[i] := RandomItem;
  end;

  for i := low(Result.Comparisons) to high(Result.Comparisons) do
  begin
    RandomItem := GetRandomItem(Result, AGrid);
    Result.Comparisons[i] := RandomItem;
  end;

end;

procedure RandomizePositions(var APositions: TRandomPositions; AGrid: TMatrix);
var
  RandomItem: TGridItem;
  i, j: integer;

begin
  for i := low(APositions.Samples) to high(APositions.Samples) do
      APositions.Samples[i].Index := -1;
  for i := low(APositions.Comparisons) to high(APositions.Comparisons) do
      APositions.Comparisons[i].Index := -1;

  for i := low(APositions.Samples) to high(APositions.Samples) do
  begin
    RandomItem := GetRandomItem(APositions, AGrid);
    APositions.Samples[i].Index := RandomItem.Index;
    APositions.Samples[i].Top := RandomItem.Top;
    APositions.Samples[i].SquareSide := RandomItem.SquareSide;
    APositions.Samples[i].Left := RandomItem.Left;
  end;

  for i := low(APositions.Comparisons) to high(APositions.Comparisons) do
  begin
    RandomItem := GetRandomItem(APositions, AGrid);
    APositions.Comparisons[i].Index := RandomItem.Index;
    APositions.Comparisons[i].Top := RandomItem.Top;
    APositions.Comparisons[i].SquareSide := RandomItem.SquareSide;
    APositions.Comparisons[i].Left := RandomItem.Left;
  end;

end;

initialization
  Grid := GetCentralGrid(3, 3.0, false);
  RandomPositions := GetRandomPositionFromGrid(Grid, 1, 4);

end.


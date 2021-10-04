unit Experiments.Eduardo.Comum.DelayDiscountTable;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Loggers.Tables;

type

  TNormalization = (NormRaw, NormEduardo, NormRafael);

  TAUC = (aucNone, aucOdum, aucEduardo);

  TTrapezoid = record
    X1 : Extended;
    X2 : Extended;
    Y1 : Extended;
    Y2 : Extended;
    Area : Extended;
  end;

  { TDelayDiscountTable }

  TDelayDiscountTable = class(TComponent)
  private
    IndiferencePoint : TFloatColumn;
    function GetFilename : string;
    function Trapezoid(AIndex: integer; Normalize: TNormalization;
      AUC: TAUC):TTrapezoid;
    procedure WriteTable;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  public
    procedure AddIndiferencePoint(AValue : Extended);
  end;


implementation

uses Math, Session.Configuration.GlobalContainer;

type

  TIndexes = 0..4;

const
  Delays : array [TIndexes] of integer =
    (1, 3, 6, 12, 24);
  NormalizedDelays : array [TIndexes] of Extended =
    (0.041, 0.125, 0.250, 0.500, 1.000);

{ TDelayDiscountTable }

function TDelayDiscountTable.GetFilename: string;
begin
  Result := GlobalContainer.BaseFilename + #95 + Self.name + '.data';
end;

function TDelayDiscountTable.Trapezoid(AIndex: integer;
  Normalize: TNormalization; AUC: TAUC): TTrapezoid;
begin
  Result := Default(TTrapezoid);
  case AIndex of
    0..3 : begin
      if (AIndex+1) > IndiferencePoint.Count-1 then begin
        Exit;
      end;
        case Normalize of
          NormRaw: begin
            Result.X1 := Delays[AIndex];
            Result.X2 := Delays[AIndex+1];
            Result.Y1 := IndiferencePoint[AIndex];
            Result.Y2 := IndiferencePoint[AIndex+1];
          end;

          NormEduardo: begin
            Result.X1 := (Delays[AIndex])/100;
            Result.X2 := (Delays[AIndex+1])/100;
            Result.Y1 := IndiferencePoint[AIndex]/NormalizedDelays[AIndex];
            Result.Y2 := IndiferencePoint[AIndex+1]/NormalizedDelays[AIndex+1];
          end;

          NormRafael: begin
            Result.X1 := (NormalizedDelays[AIndex]);
            Result.X2 := (NormalizedDelays[AIndex+1]);
            Result.Y1 := IndiferencePoint[AIndex]/100;
            Result.Y2 := IndiferencePoint[AIndex+1]/100;
          end;
        end;
      with Result do begin
        case AUC of
          aucNone   : Area := 0;
          aucEduardo: Area := (X1-X2)/(Y1-Y2);
          aucOdum   : Area := (X2-X1)*((Y1+Y2)/2);
        end;
      end;
    end;
    else begin
      Exception.Create(
        'DelayDiscountTable.Trapezoid Error: Unknown DelayDiscount Index');
    end;
  end;
end;

procedure TDelayDiscountTable.WriteTable;
var
  Table : TTabDelimitedReport;
  TrapezoidsAreaSum : Extended;
  TrapezoidsArea : array of Extended;
  Trapezoids : array of TTrapezoid;
  LTrapezoid : TTrapezoid;
  i: Integer;
begin
  Trapezoids := nil;
  TrapezoidsArea := nil;
  SetLength(Trapezoids, Length(Delays)-1);
  SetLength(TrapezoidsArea, Length(Delays)-1);

  Table := TTabDelimitedReport.Create;
  Table.Filename := GetFilename;
  try
    Table.WriteRow(['Bruto']);
    Table.WriteRow(['X1', 'X2', 'Y1', 'Y2']);
    for i := Low(Trapezoids) to High(Trapezoids) do begin
      LTrapezoid := Trapezoid(i, NormRaw, aucNone);
      with LTrapezoid do begin
        Table.WriteRow(
          [X1.ToString, X2.ToString, Y1.ToString, Y2.ToString]);
      end;
    end;

    Table.WriteRow(['']);
    Table.WriteRow(['Normalizado (Odum et al, 2020)']);
    Table.WriteRow(['X1', 'X2', 'Y1', 'Y2', 'Area = (X2-X1)*((Y1+Y2)/2)']);
    for i := Low(Trapezoids) to High(Trapezoids) do begin
      Trapezoids[i] := Trapezoid(i, NormRafael, aucOdum);
      TrapezoidsArea[i] := Trapezoids[i].Area;
      with Trapezoids[i] do begin
        Table.WriteRow(
          [X1.ToString, X2.ToString, Y1.ToString, Y2.ToString, Area.ToString]);
      end;
    end;

    Table.WriteRow(['']);
    Table.WriteRow(['Normalizado (Eduardo)']);
    Table.WriteRow(['X1', 'X2', 'Y1', 'Y2', 'Area = (X1-X2)/(Y1-Y2)']);
    for i := Low(Trapezoids) to High(Trapezoids) do begin
      Trapezoids[i] := Trapezoid(i, NormRafael, aucEduardo);
      TrapezoidsArea[i] := Trapezoids[i].Area;
      with Trapezoids[i] do begin
        Table.WriteRow(
          [X1.ToString, X2.ToString, Y1.ToString, Y2.ToString, Area.ToString]);
      end;
    end;
    TrapezoidsAreaSum := Sum(TrapezoidsArea);
    Table.WriteRow(['-', '-', '-', 'Soma:', TrapezoidsAreaSum.ToString]);
  finally
    Table.Free;
  end;
end;

constructor TDelayDiscountTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IndiferencePoint := TFloatColumn.Create;
  IndiferencePoint.Clear;
end;

destructor TDelayDiscountTable.Destroy;
begin
  WriteTable;
  IndiferencePoint.Free;
  inherited Destroy;
end;

procedure TDelayDiscountTable.AddIndiferencePoint(AValue: Extended);
begin
  if IndiferencePoint.Count >= Length(Delays) then begin
    Exception.Create(
      'DelayDiscountTable.AddIndiferencePoint error: too many points');
  end else begin
    IndiferencePoint.Add(AValue);
  end;
end;

end.


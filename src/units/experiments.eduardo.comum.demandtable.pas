unit Experiments.Eduardo.Comum.DemandTable;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Loggers.Tables, Loggers.Helpers;

type

  { TDemandTable }

  TDemandTable = class(TComponent)
  private
    FData : TDemandDataColumn;
    function GetFilename : string;
    procedure WriteTable;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  public
    procedure AddRow(APrice, AConsuming : Extended);
  end;

implementation

uses Session.Configuration.GlobalContainer;

{ TDemandTable }

constructor TDemandTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FData := TDemandDataColumn.Create;
end;

destructor TDemandTable.Destroy;
begin
  WriteTable;
  FData.Free;

  inherited Destroy;
end;

procedure TDemandTable.AddRow(APrice, AConsuming: Extended);
var
  Datum : TDemandDatum;
begin
  Datum.Price := APrice;
  Datum.Consuming := AConsuming;
  Datum.Product := APrice*AConsuming;
  FData.Add(Datum);
end;

//function Compare(const Item1, Item2: TDemandDatum): Integer;
//var
//  R:Extended;
//begin
//  R := Item1.Product-Item2.Product;
//  if R < 0 then begin Result := -1; Exit; end;
//  if R > 0 then begin Result := 1; Exit; end;
//  if R = 0 then begin Result := 0; Exit; end;
//end;

function TDemandTable.GetFilename: string;
begin
  Result := GlobalContainer.BaseFilename + #95 + Self.name + '.data';
end;

procedure TDemandTable.WriteTable;
const
  Omax : string = '*';
var
  Table : TTabDelimitedReport;
  Intensity : Extended;
  Breakpoint : Extended = -1;
  Products : array of Extended = nil;
  i : integer;
  MaxDatum : TDemandDatum;
begin
  Table := TTabDelimitedReport.Create;
  Table.Filename := GetFilename;
  try
    Table.WriteRow(['Preço', 'Consumo', 'Preço*Consumo']);

    MaxDatum := FData[0];
    for i:=1 to FData.Count-1 do begin
      if FData[i].Product>MaxDatum.Product then begin
        MaxDatum:=FData[i];
      end;
    end;

    for i := 0 to FData.Count-1 do begin
      with FData[i] do begin
        if FData[i] = MaxDatum then begin
          Table.WriteRow(
            [Price.ToString, Consuming.ToString, Product.ToString, Omax]);
        end else begin
          Table.WriteRow([Price.ToString, Consuming.ToString, Product.ToString]);
        end;
      end;
    end;
    Intensity := FData.First.Consuming;
    SetLength(Products, FData.Count);
    for i := 0 to FData.Count-1 do begin
      Products[i] := FData[i].Product;
      if FData[i].Consuming = 0 then begin
        Breakpoint := FData[i].Consuming;
      end;
    end;
    Table.WriteRow(['Legenda: '+ Omax + ' = Omax.']);
    Table.WriteRow(['']);
    Table.WriteRow(['Intensidade', 'Breakpoint']);
    Table.WriteRow([Intensity.ToString, Breakpoint.ToString]);
  finally
    Table.Free;
  end;
end;

end.


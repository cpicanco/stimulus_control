unit Loggers.Helpers;

{$mode objfpc}{$H+}{$MODESWITCH advancedrecords}

interface

uses Classes, fgl;

type

  { TDemandDatum }

  TDemandDatum = record
    Price : Extended;
    Consuming : Extended;
    Product : Extended;
    class operator = (A, B : TDemandDatum) : Boolean;
    class operator + (A, B : TDemandDatum) : TDemandDatum;
  end;

  { TGenericList }

  generic TGenericList<T> = class(specialize TFPGList<T>)
  private
    FIsTest : Boolean;
    procedure SetIsTest(AValue: Boolean);
  public
    function Sum : T;
    property IsTest : Boolean read FIsTest write SetIsTest;
  end;

implementation

{ TDemandDatum }

class operator TDemandDatum. = (A, B: TDemandDatum): Boolean;
begin
  Result := (A.Consuming=B.Consuming) and (A.Price=B.Price);
end;

class operator TDemandDatum. + (A, B: TDemandDatum): TDemandDatum;
begin
  with Result do begin
    Consuming := A.Consuming+B.Consuming;
    Price := A.Price+B.Price;
    Product := Consuming*Price;
  end;
end;

{ TGenericList }

procedure TGenericList.SetIsTest(AValue: Boolean);
begin
  if FIsTest = AValue then Exit;
  FIsTest := AValue;
end;

function TGenericList.Sum: T;
var
  i: Integer;
begin
  Result := Default(T);
  if Count = 0 then Exit;
  for i := 0 to Count -1 do begin
    Result := Result + Items[i];
  end;
end;

end.


unit Loggers.Helpers;

{$mode objfpc}{$H+}{$MODESWITCH advancedrecords}

interface

uses Classes;

type

  { TDemandDatum }

  TDemandDatum = record
    Price : Extended;
    Consuming : Extended;
    Product : Extended;
    class operator = (A, B : TDemandDatum) : Boolean;
  end;

implementation

{ TDemandDatum }

class operator TDemandDatum.=(A, B: TDemandDatum): Boolean;
begin
  Result := (A.Price=B.Price) and (A.Consuming=B.Consuming);
end;

end.


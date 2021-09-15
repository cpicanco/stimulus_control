program regis_neto_vi;

uses Classes, SysUtils, Math;

type
  TArrayOfInteger = array of integer;
  TConsequences = 0..4;

function Append(AArray : TArrayOfInteger; AItem : integer) : TArrayOfInteger;
begin
  SetLength(AArray, Length(AArray)+1);
  AArray[High(AArray)] := AItem;
  Result := AArray;
end;

function RegisNeto(AValue, AAmplitude : integer) : Cardinal;
begin
  Result := AValue;
    if AAmplitude > 0 then
      Result := AValue - AAmplitude + Random((2 * AAmplitude) + 1);
end;

var
  Consequence : integer;
  Consequences : array [TConsequences] of integer = (0, 1, 2, 3, 4);

  // TimeAt60 : 5 min;
  R : integer;
  Porcentage : integer = 30;
  Value : integer;
  Amplitude: integer;
  VI : integer;
  VIS : array of integer = (1, 2, 3, 4, 5, 10, 20, 30, 40, 60);

  RandomVIS : TArrayOfInteger;

begin
  Randomize;
  for VI in VIS do begin
    Value := VI * 1000;
    WriteLn('Value:', Value);
    Amplitude := (Value*Porcentage) div 100;
    WriteLn('Amplitude:', Amplitude);
    repeat
      RandomVIS := Default(TArrayOfInteger);
      for Consequence in Consequences do begin
        R := RegisNeto(Value, Amplitude);
        //Assert(R >= (Value - Amplitude), 'Amplitude error');
        //Assert(R <= (Value + Amplitude), 'Amplitude error');
        RandomVIS := Append(RandomVIS, R);
      end;
    until Mean(RandomVIS) = Value;
    for Value  in RandomVIS do begin
      WriteLn(Value);
    end;
    WriteLn('Mean:' + Mean(RandomVIS).ToString);
  end;
end.


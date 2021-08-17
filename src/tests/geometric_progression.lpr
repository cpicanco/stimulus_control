program geometric_progression;

uses Classes, SysUtils;

  function FormatTrial(ATrial : integer) : string;
  begin
    Result :=Format('T%.2d',[ATrial]);
  end;

  procedure WriteDiscountBloc;
  var
    LFirstNow, LNow, LLastNow : real;

    procedure WriteNow(i : integer;
      ALastNow: real; ADelta: real);
    const
      LFactor : real = 0.5;
    var
      LNow : real;
    begin
      Inc(i);
      if i >= 9 then Exit;
      LNow := ALastNow + (ABS(ADelta) * LFactor);
      WriteLn(FormatTrial(i+2), #9 , FloatToStrF(LNow, ffFixed, 0,2));
      WriteNow(i, LNow, LNow - ALastNow);

      LNow := ALastNow - (ABS(ADelta) * LFactor);
      WriteLn(FormatTrial(i+2), #9 , FloatToStrF(LNow, ffFixed, 0,2));
      WriteNow(i, LNow, LNow - ALastNow);
    end;

  begin
    LFirstNow := 50;
    LLastNow := LFirstNow;
    LNow := LFirstNow * 0.50;  // 25
    WriteLn(FormatTrial(2), #9 , FloatToStrF(LNow, ffFixed, 0,2));
    WriteNow(0, LNow, LNow - LLastNow);

    LNow := LFirstNow * 1.50;  // 75
    WriteLn(FormatTrial(2), #9 , FloatToStrF(LNow, ffFixed, 0,2));
    WriteNow(0, LNow, LNow - LLastNow);
  end;
begin
  WriteDiscountBloc;
end.

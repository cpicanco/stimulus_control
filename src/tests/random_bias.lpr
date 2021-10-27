program random_bias;

uses Classes;

var
  R : Extended;
  i : integer;

  StringList : TStringList;

begin
  Randomize;

  StringList := TStringList.Create;
  try
    for i := 0 to 99 do
    begin
      R := Random;
      if R < 0.8 then StringList.Append('A') else StringList.Append('.');
    end;
    StringList.SaveToFile('Left.txt');
    StringList.Clear;
    for i := 0 to 99 do
    begin
      R := Random;
      if R < 0.2 then StringList.Append('A') else StringList.Append('.');
    end;
    StringList.SaveToFile('Right.txt');
  finally
    StringList.Free;
  end;
end.


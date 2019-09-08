unit FileMethods;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils;

procedure FindFilesFor(out AStimuliArray: TStringArray; AFolder : string; AExtensions : string = '*.txt;*.TXT');

implementation

procedure FindFilesFor(out AStimuliArray: TStringArray; AFolder: string; AExtensions : string = '*.txt;*.TXT');
var
  Files : TStringList;
  i : integer;
begin
  Files := TStringList.Create;
  try
    FindAllFiles(Files, AFolder, AExtensions, True);
    SetLength(AStimuliArray, Files.Count);
    for i := Low(AStimuliArray) to High(AStimuliArray) do
      AStimuliArray[i] := Files[i];
  finally
    Files.Free;
  end;
end;

end.


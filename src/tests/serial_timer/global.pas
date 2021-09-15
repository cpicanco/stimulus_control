unit Global;

{$mode objfpc}{$H+}

interface

type

  TContainer = class
    RootMedia: string;
    ExeName : string;
  end;

var
  Container : TContainer;

implementation

uses SysUtils, Forms;

initialization
  Container := TContainer.Create;
  with Container do
  begin
    ExeName := Application.ExeName;
    RootMedia := ExtractFilePath(ExeName) +  'media' + DirectorySeparator;
    ForceDirectories(RootMedia);
  end

finalization
  Container.Free;

end.


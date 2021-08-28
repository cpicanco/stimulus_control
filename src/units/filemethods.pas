unit FileMethods;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure FindFilesFor(out AStimuliArray: TStringArray;
  AFolder : string;
  AExtensions : string = '*.bmp;*.jpg');

procedure AppendFilesTo(var AStimuliArray: TStringArray;
  AFolder: string;
  AExtensions : string = '*.bmp;*.jpg');

procedure LoadMessageFromFile(var AMessage : string; AFilename : string);

function NewConfigurationFile : string;

implementation

uses Forms, FileUtil, LazFileUtils, Constants, Session.ConfigurationFile;

procedure FindFilesFor(out AStimuliArray: TStringArray;
  AFolder: string;
  AExtensions : string = '*.bmp;*.jpg');
var
  Files : TStringList;
  i : integer;
begin
  AStimuliArray := nil;
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

procedure AppendFilesTo(var AStimuliArray: TStringArray;
  AFolder: string;
  AExtensions : string = '*.bmp;*.jpg');
var
  LOldLength : integer;
  Files : TStringList;
  i : integer;
begin
  LOldLength := Length(AStimuliArray);
  Files := TStringList.Create;
  try
    FindAllFiles(Files, AFolder, AExtensions, True);
    SetLength(AStimuliArray, LOldLength+Files.Count);
    i := Length(AStimuliArray);
    for i := LOldLength to High(AStimuliArray) do
      AStimuliArray[i] := Files[i -LOldLength];
  finally
    Files.Free;
  end;
end;

procedure LoadMessageFromFile(var AMessage : string; AFilename : string);
var
  LStringList : TStringList;
begin
  LStringList := TStringList.Create;
  try
    LStringList.LoadFromFile(AFilename);
    AMessage := LStringList.Text;
  finally
    LStringList.Free;
  end;
end;

function NewConfigurationFile : string;
begin
  NewConfigurationFile := ExtractFilePath(
    Application.ExeName) + 'last_session.ini';
  if FileExists(NewConfigurationFile) then
    DeleteFile(NewConfigurationFile);
  ConfigurationFile := TConfigurationFile.Create(NewConfigurationFile);
  ConfigurationFile.CacheUpdates := True;
  ConfigurationFile.WriteString(_Main, _NumBlc, '3');
  ConfigurationFile.WriteToBloc(1, _Name, 'SS');
  ConfigurationFile.Invalidate;
end;

end.


{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Loggers.DataFile;

{$mode objfpc}{$H+}

interface

uses Classes;

type

  { TRegData }

  TRegData = class(TComponent)
  private
    FFileName: string;
    FFile: TextFile;
    FSessionNumber: integer;
    procedure CloseFile;
    procedure UpdateFileName(ANewFileName : string);
    function OpenNoOverride(AFilename : string):string;
  public
    constructor Create(AOwner: TComponent; AFileName: String); reintroduce;
    destructor Destroy; override;
    function IsOpened : Boolean;
    procedure SaveData(AData: string);
    procedure SaveLine(ALine: string);
    procedure CloseAndOpen;
    procedure AppendFile;
    procedure AssignFile;
    property SessionNumber : integer read FSessionNumber write FSessionNumber;
    property FileName : string read FFileName write UpdateFileName;
  end;

implementation

{
  Do not use the DebugLn inside this unit
  it will create a circular reference.
  use writeln instead.
}
uses
  SysUtils, LazFileUtils
  {$ifdef DEBUG}
  , Dialogs
  , Loggers.Debug
  {$endif}
  ;

procedure TRegData.UpdateFileName(ANewFileName: string);
begin
  if (ANewFileName = '') or (ANewFileName = FFilename) then Exit;
  CloseFile;
  FFileName := OpenNoOverride(ANewFileName);
end;

function TRegData.OpenNoOverride(AFilename: string): string;
var
  i : Integer;
  FilePath, LExtension: string;
begin
  if AFileName <> '' then
    begin
      ForceDirectoriesUTF8(ExtractFilePath(AFilename));
      FilePath := ExtractFilePath(AFilename);
      LExtension := ExtractFileExt(AFilename);
      i := 0;

      // ensure to never override an exinting file
      while FileExistsUTF8(AFilename) do begin
        Inc(i);
        AFilename := FilePath + StringOfChar(#48, 3 - Length(IntToStr(i))) + IntToStr(i) + LExtension;
      end;

      FSessionNumber := i;

      // as override is impossible, don't mind about an Assign/Rewrite conditional
      System.Assign(FFile, AFilename);
      System.Rewrite(FFile);
      {$ifdef DEBUG}
        WriteLn(FFile, mt_Debug + 'Saving data to:' + AFilename );
      {$endif}
      Result := AFilename;
   end;
end;

constructor TRegData.Create(AOwner: TComponent; AFileName: String);
begin
  inherited Create(AOwner);
  FFilename := OpenNoOverride(AFilename);
end;

destructor TRegData.Destroy;
begin
  CloseFile;
  inherited Destroy;
end;

function TRegData.IsOpened: Boolean;
begin
  Result := TextRec(FFile).Mode = 55218;
end;

procedure TRegData.SaveData(AData: string);
begin
  Write(FFile, AData);
  System.Flush(FFile);
end;

procedure TRegData.SaveLine(ALine: string);
begin
  WriteLn(FFile, ALine);
end;

procedure TRegData.CloseAndOpen;
begin
  System.Flush(FFile);
  System.Append(FFile);
end;

procedure TRegData.AppendFile;
begin
  System.Append(FFile);
end;

procedure TRegData.AssignFile;
begin
  System.Assign(FFile, FFileName);
end;

procedure TRegData.CloseFile;
begin
  if FFilename <> '' then
    if TextRec(FFile).Mode = 55218 then // file is opened either read or write
      System.Close(FFile);
end;


end.


{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit regdata;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, LazFileUtils;

type

  { TRegData }

  TRegData = class(TComponent)
  private
    FFileName: string;
    FFile: TextFile;
    FSessionNumber: integer;
    procedure Close;
    procedure UpdateFileName(ANewFileName : string);
    function OpenNoOverride(AFilename : string):string;
  public
    constructor Create(AOwner: TComponent; AFileName: String); reintroduce;
    destructor Destroy; override;
    procedure SaveData(AData: string);
    procedure AppendF;
    procedure AssignFFile;
    procedure CloseFFile;
    property SessionNumber : integer read FSessionNumber write FSessionNumber;
    property DataFile : TextFile read FFile write FFile;
    property FileName : string read FFileName write UpdateFileName;
  end;

implementation

   {
   Do not use the DebugLn inside this unit
   it will create a circular reference.
   use writeln instead.
   }

{$ifdef DEBUG}
  uses Dialogs
    , debug_logger
    ;
{$endif}

procedure TRegData.Close;
begin
  if FFilename <> '' then
    if TextRec(FFile).Mode = 55218 then // file is opened read/write
      begin
        CloseFile(FFile);
      end
end;

procedure TRegData.UpdateFileName(ANewFileName: string);
begin
  if (ANewFileName = '') or (ANewFileName = FFilename) then Exit;
  Close;
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
        AssignFile(FFile, AFilename);
        Rewrite(FFile);
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
// With the current implementation
// if undefined DEBUG, CloseFile should be called only once
begin
  Close;
  inherited Destroy;
end;

procedure TRegData.SaveData(AData: string);
begin
  Write(FFile, AData);
end;

procedure TRegData.AppendF;
begin
  Append(FFile);
end;

procedure TRegData.AssignFFile;
begin
  AssignFile(FFile, FFileName);
end;

procedure TRegData.CloseFFile;
begin
  CloseFile(FFile);
end;


end.


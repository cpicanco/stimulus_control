//
// Validation Project (PCRF) - Stimulus Control App
// Copyright (C) 2014-2016,  Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.
//
// cpicanco@ufpa.br
//
// This file is part of Validation Project (PCRF).
//
// Validation Project (PCRF) is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Validation Project (PCRF) is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Validation Project (PCRF).  If not, see <http://www.gnu.org/licenses/>.
//
unit regdata;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, FileUtil
    //, Dialogs
    ;

type

  { TRegData }

  TRegData = class(TComponent)
  private
    FFileName: string;
    FFile: TextFile;
    FSessionNumber: integer;
    procedure UpdateFileName(NewFileName : string);
  public
    constructor Create(AOwner: TComponent; FileName: String); reintroduce;
    destructor Destroy; override;
    procedure SaveData(Data: string);
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
   yeah.. need to find a way to debug the debugger.

   }

{$ifdef DEBUG}
  uses debug_logger, Dialogs;
{$endif}

procedure TRegData.UpdateFileName(NewFileName : string);
begin
  if (NewFileName <> '') and (NewFileName <> FFilename) then
    if FileExistsUTF8(NewFileName) then
      FFileName := NewFileName;
end;

constructor TRegData.Create(AOwner: TComponent; FileName: String);
var
  i, ExtensionLength : Integer;

  sName, aSeparator, aExtension: string;

      {

       We expect a filename with that structure:
       Data_001.txt or Data_001.timestamp
       4 char => sName
       1 char = aSeparator
       3 char = StringOfChar
       4 char = '.TXT' Extention or 10 char = '.timestamps' extension

       note : we begin from i := 1;
      }

begin
  inherited Create(AOwner);
  if FileName <> '' then
    begin
      ForceDirectoriesUTF8(ExtractFilePath(FileName)); { *Converted from ForceDirectories*  }

      if Pos('timestamps', FileName) <> 0 then
        begin
          ExtensionLength := 10;
        end
      else ExtensionLength := 3;

      i := 0;
      sName:= Copy(FileName, 0, Length(FileName)- (ExtensionLength + 5));
      aExtension := Copy(FileName, Length(FileName) - ExtensionLength, ExtensionLength + 1);
      aSeparator := '_';

      // ensure to never override an exinting data file
      while FileExistsUTF8(FileName) do begin
        Inc(i);
        FileName:= sName + aSeparator + StringOfChar(#48, 3 - Length(IntToStr(i))) + IntToStr(i) + aExtension;
      end;

      FSessionNumber := i;
      FFileName := FileName;

      // as override is impossible, don't mind about an Assign/Rewrite conditional
      AssignFile(FFile, FileName);
      Rewrite(FFile);

      {$ifdef DEBUG}
        WriteLn(FFile, mt_Debug + 'Saving data to:' + FFileName )
      {$endif}
   end;
end;

destructor TRegData.Destroy;
// With the current implementation
// if undefined DEBUG, CloseFile should be called only once
begin
  if FFilename <> '' then
    if TextRec(FFile).Mode = 55218 then // file is opened read/write
      begin
        CloseFile(FFile);
      end;
  inherited Destroy;
end;

procedure TRegData.SaveData(Data: string);
//var bol : Boolean;
begin
  if FFileName <> '' then
    begin
      Write(FFile, Data);
    end
  else {$ifdef DEBUG}  WriteLn( FFile, mt_Warning + 'Filename is empty.' + '[' + Data + ']' + 'will not be saved.' + LineEnding)  {$endif} ;
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


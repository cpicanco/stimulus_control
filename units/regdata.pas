//
// Validation Project (PCRF) - Stimulus Control App
// Copyright (C) 2014,  Carlos Rafael Fernandes Picanço, cpicanco@ufpa.br
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

uses SysUtils, Classes, FileUtil;

type

  { TRegData }

  TRegData = class(TComponent)
  private
    FITIBEGIN: DWord;
    FITIEND: DWord;
    FLatencyLblBegin: Dword;
    FLatencyLblResponse: Dword;
    FLatencyStmBegin: Dword;
    FLatencyStmResponse: Dword;
    FFile: TextFile;
    FSessionNumber: integer;
  public
    constructor Create(AOwner: TComponent; FileName: String); reintroduce;
    destructor Destroy; override;
    procedure SaveData(Data: string);
    property SessionNumber : integer read FSessionNumber write FSessionNumber;
    property LatencyLblBegin : Dword read FLatencyLblBegin write FLatencyLblBegin;
    property LatencyLblResponse : Dword read FLatencyLblResponse write FLatencyLblResponse;
    property LatencyStmBegin : Dword read FLatencyStmBegin write FLatencyStmBegin;
    property LatencyStmResponse : Dword read FLatencyStmResponse write FLatencyStmResponse;
    property ITIBEGIN : DWord read FITIBEGIN write FITIBEGIN;
    property ITIEND : DWord read FITIEND write FITIEND;
  end;

implementation

constructor TRegData.Create(AOwner: TComponent; FileName: string);
var a1: Integer; s1, s2: string;
begin
  inherited Create(AOwner);

  ForceDirectoriesUTF8(ExtractFilePath(FileName)); { *Converted from ForceDirectories*  }

  a1:= 1;
  s1:= Copy(FileName, 0, Length(FileName)-8);
  s2:= Copy(FileName, Length(FileName)-3, 4);
  while FileExistsUTF8(FileName) { *Converted from FileExists*  } do begin
    Inc(a1);
    FileName:= s1 + '_' + StringOfChar(#48, 3 - Length(IntToStr(a1))) + IntToStr(a1) + s2;
  end;
  FSessionNumber := a1;
  AssignFile(FFile, FileName);
  Rewrite(FFile);
end;

destructor TRegData.Destroy;
begin
  CloseFile(FFile);
  inherited Destroy;
end;

procedure TRegData.SaveData(Data: string);
begin
  Write(FFile, Data);
end;

end.


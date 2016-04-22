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
unit timestamps_logger;

{$mode objfpc}{$H+}

interface

uses regdata;

  {

    We choose to use as external logger to avoid synchronization issues
    with the client thread. Sometimes the program is faster than the client.
    The consequence is that the program sometimes destroys the filename
    reference during the client writing operations leading to crashes.

  }

procedure TimestampLn(msg: string);
procedure UpdateTimestampsFileName(NewFilename : string);

implementation

uses FileUtil, SysUtils;

var
  Timestamps : TRegData;

procedure TimestampLn(msg: string);
begin
  if TextRec(Timestamps.DataFile).Mode = 55218 then
    begin
      WriteLn(Timestamps.DataFile, msg);
      Timestamps.CloseFFile;
    end
  else
    begin
      Timestamps.AssignFFile;
      Timestamps.AppendF;
      WriteLn(Timestamps.DataFile, msg);
      Timestamps.CloseFFile;
    end;
end;

procedure UpdateTimestampsFileName(NewFilename: string);
begin
  Timestamps.FileName := NewFilename;
end;

initialization
begin
  Timestamps := TRegData.Create(nil, '');
end

finalization
begin
  Timestamps.Free;
end

end.


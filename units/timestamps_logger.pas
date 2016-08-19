{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit timestamps_logger;

{$mode objfpc}{$H+}

interface

  {

    We choose to use as external logger to avoid synchronization issues
    with the client thread. Sometimes the program is faster than the client.
    The consequence is that the program sometimes destroys the filename
    reference during the client writing operations leading to crashes.

  }

procedure TimestampLn(msg: string);
procedure UpdateTimestampsFileName(NewFilename : string);

implementation

uses SysUtils
     , regdata
     , timestamp
     ;

var
  Timestamps : TRegData;

procedure TimestampLn(msg: string);
begin
  if not TextRec(Timestamps.DataFile).Mode = 55218 then
    begin
      Timestamps.AssignFFile;
      Timestamps.AppendF;
    end;
  WriteLn(Timestamps.DataFile, msg);
  Timestamps.CloseFFile;
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


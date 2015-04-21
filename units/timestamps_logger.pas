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


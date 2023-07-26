unit Versioning.Lazarus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , fileinfo         // reads exe resources as long as you register the appropriate units
  {$IFDEF WINDOWS}
  , winpeimagereader // need this for reading exe info
  {$ENDIF}

  {$IFDEF LINUX}
  , elfreader        // needed for reading ELF executables
  {$ENDIF}

  {$IFDEF DARWIN}
  , machoreader      // needed for reading MACH-O executables}
  {$ENDIF}
  ;
function FileVersion :string;
function ZMQVersion : string;

implementation

{$IFNDEF NO_LIBZMQ}
uses zmq;
{$ENDIF}

function FileVersion : string;
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    try
      FileVerInfo.ReadFileInfo;
    except
      on E : Exception do
        Result := 'Unknown';
    end;
    Result := 'v'+FileVerInfo.VersionStrings.Values['FileVersion'];
  finally
    FileVerInfo.Free;
  end;
end;

function ZMQVersion : string;
var
  LPatch, LMinor, LMajor: Integer;
begin
  LMajor := 0; LMinor := 0; LPatch := 0;
  {$IFNDEF NO_LIBZMQ}
  zmq_version(LMajor,LMinor,LPatch);
  {$ENDIF}
  Result := 'v'+IntToStr(LMajor)+'.'+IntToStr(LMinor)+'.'+IntToStr(LPatch);
end;

end.


{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit data_logger;

{$mode objfpc}{$H+}

interface

uses regdata, config_session;

type

  // LGData have blc, trial data.
  // LGTimestamps for stm and response data.
  TLoggers = (LGData, LGTimestamps);

  function GetSaveDataProc(ADataLogger: TLoggers): TDataProcedure;
  function GetLogger(ADataLogger: TLoggers) : TRegData;
  procedure FreeLogger(ADataLogger: TLoggers; AFooter : string);
  procedure CreateLogger(ADataLogger: TLoggers; AFilename, AHeader : string);

implementation

uses timestamps_helpers;

var
  GTimestampLogger,
  GDataLogger : TRegData;

function GetSaveDataProc(ADataLogger: TLoggers): TDataProcedure;
var LRegdata : TRegData;
begin
  LRegdata := GetLogger(ADataLogger);
  Result := @LRegdata.SaveData;
end;

function GetLogger(ADataLogger: TLoggers): TRegData;
begin
  case ADataLogger of
    LGTimestamps: Result := GTimestampLogger;
    LGData: Result := GDataLogger;
  end;
end;

procedure FreeLogger(ADataLogger: TLoggers; AFooter: string);
var LRegdata : TRegData;
begin
  LRegdata := GetLogger(ADataLogger);
  with LRegdata do
    begin
      SaveData(AFooter);
      Free;
    end;
end;

procedure CreateLogger(ADataLogger: TLoggers; AFilename, AHeader: string);
  function Ext(LG : TLoggers): string;
  begin
    case LG of
      LGTimestamps: Result := '.timestamps';
      LGData: Result := '.data';
    end;
  end;

  function RegData:TRegData;
  begin
    Result := TRegData.Create(nil, AFilename + Ext(ADataLogger));
    Result.SaveData(AHeader);
  end;
begin
  case ADataLogger of
    LGTimestamps: GTimestampLogger := RegData;
    LGData: GDataLogger := RegData;
  end;
end;


end.


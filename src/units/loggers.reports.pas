{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Loggers.Reports;

{$mode objfpc}{$H+}

interface

uses Loggers.DataFile;

type

  { TDataProcedure }
  TDataProcedure = procedure (S : string) of object;

  // LGData have blc, trial data.
  // LGTimestamps for stm and response data.
  TLoggers = (LGData, LGTimestamps);

  function GetSaveDataProc(ADataLogger: TLoggers): TDataProcedure;
  function GetLogger(ADataLogger: TLoggers) : TRegData;
  function CreateLogger(ADataLogger: TLoggers;
    AFilename, AHeader : string) : string;

  procedure FreeLogger(ADataLogger: TLoggers; AFooter : string);

resourcestring
  HSUBJECT_NAME      = 'Nome_Sujeito:';
  HSESSION_NAME      = 'Nome_Sessão:';
  HFIRST_TIMESTAMP   = 'Primeira_Timestamp:';
  HBEGIN_TIME        = 'Início:';
  HEND_TIME          = 'Término:';
  HSESSION_CANCELED  = '----------Sessão Cancelada----------';
  HTEST_MODE         = '(Modo de Teste)';

implementation

uses Forms;

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

function CreateLogger(ADataLogger: TLoggers; AFilename, AHeader: string) : string;
var
  LRegData : TRegData;

  function Ext(LG : TLoggers): string;
  begin
    case LG of
      LGTimestamps: Result := '.timestamps';
      LGData: Result := '.data';
    end;
  end;

  function RegData:TRegData;
  begin
    Result := TRegData.Create(Application, AFilename + Ext(ADataLogger));
    Result.SaveData(AHeader);
  end;
begin
  LRegData := RegData;
  case ADataLogger of
    LGTimestamps: GTimestampLogger := LRegData;
    LGData: GDataLogger := LRegData;
  end;
  Result := LRegData.FileName;
end;


end.


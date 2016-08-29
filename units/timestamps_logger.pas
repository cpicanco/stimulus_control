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

uses regdata;

  function GetLogger : TRegData;
  procedure SetLogger(Footer : string); overload;
  procedure SetLogger(Filename, Header : string); overload;

implementation

uses timestamps_helpers;

function GetLogger: TRegData;
begin
  Result := TimestampLogger;
end;

procedure SetLogger(Footer: string);
begin
  with TimestampLogger do
    begin
      SaveData(Footer);
      Free;
    end;
end;

procedure SetLogger(Filename, Header: string);
begin
  TimestampLogger := TRegData.Create(nil, Filename);
  TimestampLogger.SaveData(Header);
end;


end.


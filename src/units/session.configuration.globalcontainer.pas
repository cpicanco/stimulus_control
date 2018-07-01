{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Session.Configuration.GlobalContainer;

{$mode objfpc}{$H+}

interface

uses
  CounterManager;

type

  { TGlobalContainer }

  TGlobalContainer = class
    {$IFNDEF NO_LIBZMQ}
    PupilClient: TPupilClient;
    {$ENDIF}
    PupilEnabled : Boolean;
    CounterManager : TCounterManager;
    RootData: string;
    RootMedia: string;
    ExeName : string;
    TimeStart : Extended;
    TestMode : Boolean;
    MonitorToShow : Byte;
  end;
var
  GlobalContainer : TGlobalContainer;

implementation

uses SysUtils, Forms, Timestamps;

initialization
  GlobalContainer := TGlobalContainer.Create;
  with GlobalContainer do
  begin
    ExeName := Application.ExeName;
    RootData := ExtractFilePath(ExeName) + 'data' + DirectorySeparator;
    RootMedia := ExtractFilePath(ExeName) +  'media' + DirectorySeparator;
    ForceDirectories(RootData);
    ForceDirectories(RootMedia);
    CounterManager := TCounterManager.Create(nil);
    MonitorToShow := 0;
    TimeStart := TickCount;
    PupilEnabled := False;
    TestMode := False;
  end

finalization
  GlobalContainer.CounterManager.Free;
  GlobalContainer.Free;

end.


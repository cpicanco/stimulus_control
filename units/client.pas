//
// Validation Project (PCRF) - Stimulus Control App
// Copyright (C) 2014-2015,  Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.
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
unit client;

{$mode objfpc}{$H+}

interface

uses
   Classes
 , SysUtils
 , Process
 ;

type

  TShowStatusEvent = procedure(Status: String) of object;

  { TClientThread }

  TClientThread = class(TThread)
  private
    FMsg : string;
    FTrialIndex : string;
    FCode : string;
    FTimestampsPath : string;
    FOnShowStatus: TShowStatusEvent;
    procedure ShowStatus;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended : boolean; TrialIndex : integer; Code : string; OutputPath : string);
    destructor Destroy; override;
    property OnShowStatus: TShowStatusEvent read FOnShowStatus write FOnShowStatus;
  end;

implementation

constructor TClientThread.Create(CreateSuspended : boolean; TrialIndex : integer; Code : string; OutputPath : string);
begin
  FreeOnTerminate := True;
  FTrialIndex := IntToStr(TrialIndex);
  FCode := Code;
  FTimestampsPath := OutputPath;

  inherited Create(CreateSuspended);
end;

destructor TClientThread.Destroy;
begin
  //
  inherited Destroy;
end;

procedure TClientThread.ShowStatus;
// this method is executed by the mainthread and can therefore access all GUI elements.
begin
  if Assigned(FOnShowStatus) then
  begin
    FOnShowStatus(FMsg);
  end;
end;

procedure TClientThread.Execute;
var
  Python : TProcess;
  argv : string;
begin
  FMsg := '[Begin]';
  Synchronize( @Showstatus );

  argv := '"' + GetCurrentDir + PathDelim + 'gettimestamp.py' + '"' + #32 +
          '"' + FTimestampsPath + '"'  + #32 +
          '"' + FTrialIndex + '"' + #32 + '"' + FCode + '"';
  FMsg := argv;
  Synchronize( @Showstatus );

  Python := TProcess.Create(nil);
  Python.CommandLine := 'python' + #32 + argv;
  try
    Python.Execute;
  finally
    Python.Free;
  end;

  FMsg := '[End]';
  Synchronize( @Showstatus );
end;

end.

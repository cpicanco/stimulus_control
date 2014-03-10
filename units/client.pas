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
unit client;

{$mode objfpc}{$H+}

interface

uses
   Classes
 , SysUtils
 , Process
 //, zmq
 //, zmqapi
// , zmqcpp
// , zmq_utils
 ;

type

  TShowStatusEvent = procedure(Status: String) of Object;

  { TClientThread }

  TClientThread = class(TThread)
  private
    FMsg : string;
    FTrialIndex : string;
    FCode : string;
    //FContext: TZMQContext;
    //FSubscriber : TZMQSocket;
    //FSyncClient: TZMQSocket;
    FOnShowStatus: TShowStatusEvent;
    procedure ShowStatus;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended : boolean; TrialIndex : integer; Code : string);
    destructor Destroy; override;
    property OnShowStatus: TShowStatusEvent read FOnShowStatus write FOnShowStatus;
  end;

implementation

constructor TClientThread.Create(CreateSuspended : boolean; TrialIndex : integer; Code : string);
begin
  FreeOnTerminate := True;
  FTrialIndex := IntToStr(TrialIndex);
  FCode := Code;

  //TProcess.Options;
  //Network setup
  //FContext := TZMQContext.Create( 1 );
  //FSubscriber := TZMQSocket.Create( FContext, stSub );
  //FSubscriber.connect( 'tcp://127.0.0.1:5000' );
  //FSubscriber.RcvTimeout := 5;

  //filter messages starting with 'STRING'
  // '' receives all messages
  //FSubscriber.Subscribe( 'Pupil' );

  //  0MQ is so fast, we need to wait a while... (?)
  //Sleep (1);
  inherited Create(CreateSuspended);
end;

destructor TClientThread.Destroy;
begin
  //FSubscriber.Free;
  //FContext.Free;
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

  argv := '"' + GetCurrentDir + PathDelim + 'gettimestamp.py"' + #32 + '"' + GetCurrentDir + PathDelim + 'timestamps"'  + #32 + '"' + FTrialIndex + '"' + #32 + '"' + FCode + '"';

  Python := TProcess.Create(nil);
  Python.CommandLine := 'python' + #32 + argv;
  //Python.Parameters.Add(GetCurrentDir + PathDelim + 'gettimestamp.py');
  //Python.Parameters.Add(GetCurrentDir + PathDelim + 'timestamps');
  //Python.Parameters.Add(FTrialIndex);
  Python.Execute;
      {while ( not Terminated ) do
        begin

          // '/local/path/gettimestamp.py /local/path/timestamps 0'
          //ExecuteProcess( '/usr/bin/python2.7', argv, [] );

          {
          try
            FSubscriber.recv( newMsg );
          except
            on e : Exception do
              begin
                FMsg := '[recv error]';
                Synchronize( @Showstatus );
              end;
          end;
          }

          if Length( argv ) > 1 then
            begin
              FMsg := argv;
              Synchronize( @Showstatus );
            end;

        end;
      }
  Python.Free;
  FMsg := '[End]';
  Synchronize( @Showstatus );
end;

end.

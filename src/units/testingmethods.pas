unit TestingMethods;

{$mode objfpc}{$H+}

interface

procedure ShowTrialConsole;

implementation

uses SysUtils, Dialogs, Session.Configuration.GlobalContainer;

procedure ShowTrialConsole;
var
  LNextTrial : string;
  LNextTrialI : integer;
begin
  LNextTrial := InputBox('Trial Console', 'Insert the Next Trial', '-');
  LNextTrialI := StrToIntDef(LNextTrial, -1);
  if LNextTrialI <> -1 then
  begin
    //while Trial = nil do Application.ProcessMessages;
    GlobalContainer.CounterManager.CurrentTrial := LNextTrialI;
  end;
end;

end.


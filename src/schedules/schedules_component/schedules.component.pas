unit Schedules.Component;

{$mode objfpc}{$H+}

interface

procedure Register;

implementation

uses
  Classes, Schedules;

procedure Register;
begin
  RegisterComponents('Stimulus Control',[TSchedule]);
end;

end.

program leticia_experiment;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Forms.Main;

{$R *.res}

begin
  Randomize;
  RequireDerivedFormResource:=True;
  Application.CreateForm(TBackground, Background);
  Application.Scaled:=True;
  Application.Initialize;
  Application.Run;
end.


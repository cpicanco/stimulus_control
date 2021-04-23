unit Experiments.Eduardo;

{$mode objfpc}{$H+}

interface

function MakeConfigurationFile(ADesign : string) : string;

implementation

uses FileMethods
   , Experiments.Eduardo.Experimento1
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   ;

function MakeConfigurationFile(ADesign : string) : string;
begin
  Result := NewConfigurationFile;
  Experiments.Eduardo.Experimento1.WriteToConfigurationFile(ADesign);
  ConfigurationFile.Invalidate;
  ConfigurationFile.UpdateFile;
end;





end.

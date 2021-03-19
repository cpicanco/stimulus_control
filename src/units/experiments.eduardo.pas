unit Experiments.Eduardo;

{$mode objfpc}{$H+}

interface

function MakeConfigurationFile : string;

implementation

uses FileMethods
   , Experiments.Eduardo.Experimento1
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   ;

function MakeConfigurationFile : string;
begin
  Result := NewConfigurationFile;
  Experiments.Eduardo.Experimento1.WriteToConfigurationFile;
  ConfigurationFile.Invalidate;
  ConfigurationFile.UpdateFile;
end;





end.

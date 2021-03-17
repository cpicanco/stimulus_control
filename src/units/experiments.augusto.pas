unit Experiments.Augusto;

{$mode objfpc}{$H+}

interface

function MakeConfigurationFile : string;

implementation

uses FileMethods
   , Experiments.BeforeAfter
   , Experiments.SameDifferent
   , Experiments.Derivation
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   ;

function MakeConfigurationFile : string;
begin
  Result := NewConfigurationFile;
  Experiments.BeforeAfter.WriteToConfigurationFile;
  Experiments.SameDifferent.WriteToConfigurationFile;
  Experiments.Derivation.WriteToConfigurationFile;
  ConfigurationFile.Invalidate;
  ConfigurationFile.UpdateFile;
end;





end.

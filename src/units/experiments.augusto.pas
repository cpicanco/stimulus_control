unit Experiments.Augusto;

{$mode objfpc}{$H+}

interface

function MakeConfigurationFile : string;

implementation

uses FileMethods
   , Experiments.Augusto.BeforeAfter
   , Experiments.Augusto.SameDifferent
   , Experiments.Augusto.Derivation
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   ;

function MakeConfigurationFile : string;
begin
  Result := NewConfigurationFile;
  Experiments.Augusto.BeforeAfter.WriteToConfigurationFile;
  Experiments.Augusto.SameDifferent.WriteToConfigurationFile;
  Experiments.Augusto.Derivation.WriteToConfigurationFile;
  ConfigurationFile.Invalidate;
  ConfigurationFile.UpdateFile;
end;





end.

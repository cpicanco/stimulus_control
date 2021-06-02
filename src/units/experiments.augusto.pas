unit Experiments.Augusto;

{$mode objfpc}{$H+}

interface

function MakeConfigurationFile : string;

function MakeSecondConfigurationFileHigh : string;
function MakeSecondConfigurationFileLow : string;

implementation

uses FileMethods
   , Experiments.BeforeAfter
   , Experiments.SameDifferent
   , Experiments.Derivation
   , Experiments.SecondDerivation
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

function MakeSecondConfigurationFileHigh: string;
begin
  Result := NewConfigurationFile;
  Experiments.BeforeAfter.WriteToConfigurationFile;
  Experiments.SameDifferent.WriteToConfigurationFile;
  Experiments.SecondDerivation.WriteToConfigurationFileHigh;
  ConfigurationFile.Invalidate;
  ConfigurationFile.UpdateFile;
end;

function MakeSecondConfigurationFileLow: string;
begin
  Result := NewConfigurationFile;
  Experiments.BeforeAfter.WriteToConfigurationFile;
  Experiments.SameDifferent.WriteToConfigurationFile;
  Experiments.SecondDerivation.WriteToConfigurationFileLow;
  ConfigurationFile.Invalidate;
  ConfigurationFile.UpdateFile;
end;





end.

{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Experiments.Eduardo;

{$mode objfpc}{$H+}

interface

function MakeConfigurationFile(ADesign : string; AExperiment : byte) : string;

implementation

uses FileMethods
   , Experiments.Eduardo.Experimento1
   , Experiments.Eduardo.Experimento2
   , Experiments.Eduardo.Experimento3
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   ;

function MakeConfigurationFile(ADesign: string; AExperiment: byte): string;
begin
  Result := NewConfigurationFile;
  case AExperiment of
    0: Experiments.Eduardo.Experimento1.WriteToConfigurationFile(ADesign);
    1: Experiments.Eduardo.Experimento2.WriteToConfigurationFile(ADesign);
    2: Experiments.Eduardo.Experimento3.WriteToConfigurationFile(ADesign);
    //3: Experiments.Eduardo.Experimento4.WriteToConfigurationFile(ADesign);
  end;
  ConfigurationFile.Invalidate;
  ConfigurationFile.UpdateFile;
end;





end.

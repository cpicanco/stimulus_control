{
  Stimulus Control
  Copyright (C) 2014-2022 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Experiments.Vinicius;

{$mode objfpc}{$H+}

interface

function MakeConfigurationFile(ADesign : string; AExperiment : byte;
  AVisual : Boolean; AFirstCondition: Boolean; AParticipantN: integer) : string;

implementation

uses FileMethods
   , Experiments.Vinicius.Experimento1
   , Experiments.Vinicius.Experimento2
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   ;

function MakeConfigurationFile(ADesign: string; AExperiment: byte;
  AVisual: Boolean; AFirstCondition: Boolean; AParticipantN: integer): string;
begin
  Result := NewConfigurationFile;
  case AExperiment of
    0: Experiments.Vinicius.Experimento1.WriteToConfigurationFile(
      ADesign, AVisual, AFirstCondition, AParticipantN);
    1: Experiments.Vinicius.Experimento2.WriteToConfigurationFile(ADesign);
  end;
  ConfigurationFile.Invalidate;
  ConfigurationFile.UpdateFile;
end;





end.

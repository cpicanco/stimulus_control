{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Experiments.Eduardo.Experimento3;

{$mode objfpc}{$H+}

interface

procedure WriteToConfigurationFile(ADesign : string);

implementation

uses Classes, SysUtils
   , Constants
   , LazFileUtils
   , StrUtils
   , Experiments.Eduardo.Comum
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   , Cheats
   ;

procedure WriteOperantTask(ABlc : integer; AName: string;
  ASchedule: string; AType : string);
var
  i : integer;
begin
  i := ConfigurationFile.TrialCount[ABlc]+1;
  with ConfigurationFile do
  begin
    WriteToTrial(i, ABlc, _Name, AName);
    WriteToTrial(i, ABlc, _Cursor, '0');
    WriteToTrial(i, ABlc, _Kind, T_EO1);
    WriteToTrial(i, ABlc, _Schedule, ASchedule);
    WriteToTrial(i, ABlc, _CounterType, '2');
    WriteToTrial(i, ABlc, _ITI, ITI.ToString);
    WriteToTrial(i, ABlc, 'Type', AType);
  end;
end;

// MIN20
procedure WriteB1(ABlc : integer; AType : string);
var
  i : integer;
begin
  for i := 0 to 19 do
    WriteOperantTask(ABlc,
      'Treino Operante-FI'+#32+Experiment3FIValue.ToString+ABlc.ToString+'-'+(i+1).ToString,
      'FI'+#32+Experiment3FIValue.ToString, AType);
end;

// MIN30-MAX40
procedure WriteB1SecondHalf(ABlc : integer; AType : string);
var
  i : integer;
begin
  for i := 0 to 9 do
    WriteOperantTask(ABlc,
      'Treino Operante-FI'+#32+Experiment3FIValue.ToString+ABlc.ToString+'-'+(i+1).ToString,
      'FI'+#32+Experiment3FIValue.ToString, AType);
end;

// Teste
// 26 tentativas de treino
// 4 tentativas de teste
procedure WriteConditionTest(ABlc : integer; AType : string);
var
  i, j, r : integer;
  LStringList : TStringList;
begin
  LStringList := TStringList.Create;
  try
    LStringList.Clear;

    for i := 0 to 1 do
      LStringList.Append('EXT');

    for i := 0 to 12 do
      LStringList.Append('FI'+#32+Experiment3FITestValue.ToString);

    j := 0;
    repeat
      for i := 0 to LStringList.Count-1 do
      begin
        r := Random(LStringList.Count);
        LStringlist.Exchange(i, r);
      end;

      for i := 0 to LStringList.Count-1 do
        WriteOperantTask(ABlc, 'Mudança de Pico-'+
          LStringlist[i]+'-'+(i+1).ToString,
          LStringlist[i], AType);
      Inc(j);
     until j = 1;
  finally
    LStringList.Free;
  end;
end;

procedure WriteToConfigurationFile(ADesign : string);
var
  LCondition : char;
  LConditionI : integer = 0;
  LNextBlocOnCriteria : string;

  procedure WriteBCondition;
  var
    i : integer;
  begin
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem B');
    WriteMSG(LConditionI, 'M1', MessageE3B);

    // 20 tentativas
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'B1-MIN 20');
    WriteB1(LConditionI, 'E3B1');

    // 40 no máximo, critério de 90% de acerto
    for i := 0 to 1 do
    begin
      Inc(LConditionI);
      ConfigurationFile.WriteToBloc(LConditionI, _Name, 'B1-MIN30-MAX40-90% de acerto');
      ConfigurationFile.WriteToBloc(LConditionI, _CrtHitPorcentage, '90');
      ConfigurationFile.WriteToBloc(LConditionI, _NextBlocOnCriteria, LNextBlocOnCriteria);
      WriteB1SecondHalf(LConditionI, 'E3B1');
    end;

    // 26 treino, 4 teste, sem critério
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'B2-Mudança de Pico');
    WriteConditionTest(LConditionI, 'E3B2');
  end;


  procedure WriteCCondition;
  var
    i : integer;
  begin
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem C');
    WriteMSG(LConditionI, 'M1', MessageE3B);

    // 20 tentativas
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'C1-MIN 20');
    WriteB1(LConditionI, 'E3C1');

    // 40 no máximo, critério de 90% de acerto
    for i := 0 to 1 do
    begin
      Inc(LConditionI);
      ConfigurationFile.WriteToBloc(LConditionI, _Name, 'C1-MAX 40-90% de acerto');
      ConfigurationFile.WriteToBloc(LConditionI, _CrtHitPorcentage, '90');
      ConfigurationFile.WriteToBloc(LConditionI, _NextBlocOnCriteria, LNextBlocOnCriteria);
      WriteB1SecondHalf(LConditionI, 'E3C1');
    end;

    // 26 treino, 4 teste, sem critério
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'C-Mudança de Pico com Reforço independente');
    WriteConditionTest(LConditionI, 'E3C2');
  end;

begin
  ITI := SurveyITI;
  SetupStimuli;
  if (ADesign = 'ABA') or (ADesign = 'ACA') then
  begin
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem A0');
    WriteMSG(LConditionI, 'M0', MessageA0);
    LNextBlocOnCriteria := '5'; // zero based
  end else begin
    LNextBlocOnCriteria := '4';
  end;

  for LCondition in ADesign do
  case LCondition of
    'A': WriteACondition(LConditionI);
    'B': WriteBCondition;
    'C': WriteCCondition;
  end;
end;

end.

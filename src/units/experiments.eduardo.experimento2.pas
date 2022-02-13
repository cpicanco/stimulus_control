{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Experiments.Eduardo.Experimento2;

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

procedure WriteTemporalBissection(ABlc : integer; AName: string;
  ASampleTime: string; AConsequence : string; IsTestTrial : Boolean);
var
  i : integer;
  LConsequenceL : Boolean;
  LConsequenceR : Boolean;

  function HasConsequence(AProbability : Extended) : Boolean;
  var
    R : Extended;
  begin
    R := Random;
    if R < AProbability then
      Result := True
    else
      Result := False;
  end;

begin
  i := ConfigurationFile.TrialCount[ABlc]+1;
  with ConfigurationFile do
  begin
    WriteToTrial(i, ABlc, _Name,
      AName + #32 + ASampleTime + #32 + AConsequence);
    WriteToTrial(i, ABlc, _Cursor, '-1');
    WriteToTrial(i, ABlc, _Kind, T_TMB);
    WriteToTrial(i, ABlc, _ITI, ITI.ToString);
    WriteToTrial(i, ABlc, _ExpectedResponse, ASampleTime);
    WriteToTrial(i, ABlc, 'IsTestTrial', IsTestTrial.ToString);

    LConsequenceL :=
      HasConsequence(StrToFloat(ExtractDelimited(1, AConsequence,[#32])));
    LConsequenceR :=
      HasConsequence(StrToFloat(ExtractDelimited(2, AConsequence,[#32])));
    WriteToTrial(i, ABlc, _Consequence,
      LConsequenceL.ToString + #32 + LConsequenceR.ToString);
  end;
end;

// 10, 50% left, 50% right, 100% de reforço
procedure WriteTraining1(ABlc : integer; ABias : string);
var
  i, r : integer;
  LStringList : TStringList;
begin
  LStringList := TStringList.Create;
  try
    LStringList.Clear;

    for i := 0 to 4 do
    begin
      LStringList.Append(SP1000.ToString);
      LStringList.Append(SP4000.ToString);
    end;

    for i := 0 to LStringlist.Count-1 do
    begin
      r := Random(LStringlist.Count);
      LStringlist.Exchange(i, r);
    end;

    for i := 0 to LStringlist.Count-1 do
      WriteTemporalBissection(ABlc, 'Temporal Bissection '+(i+1).ToString,
        LStringlist[i], ABias, False);

  finally
    LStringList.Free;
  end;
end;

// 10, 50% left, 50% right
procedure WriteTraining2(ABlc : integer; AConsequence : string);
var
  i, r : integer;
  LStringList : TStringList;
begin
  LStringList := TStringList.Create;
  try
    LStringList.Clear;
    for i := 0 to 4 do
    begin
      LStringList.Append(SP1000.ToString);
      LStringList.Append(SP4000.ToString);
    end;

    for i := 0 to LStringlist.Count-1 do
    begin
      r := Random(LStringlist.Count);
      LStringlist.Exchange(i, r);
    end;

    for i := 0 to LStringlist.Count-1 do
      WriteTemporalBissection(ABlc, 'Temporal Bissection '+(i+1).ToString,
        LStringlist[i], AConsequence, False);
  finally
    LStringList.Free;
  end;
end;

// teste 8 blocos de 7 (2 treino, 5 de teste), total 16 treino, 40 teste
procedure WriteTrainingTest(ABlc : integer);
var
  j, i, r : integer;
  LConsequence : string;
  LStringList     : TStringList;
  S4000 : string;
  S3170 : string;
  S2520 : string;
  S2000 : string;
  S1590 : string;
  S1260 : string;
  S1000 : string;
begin
  LStringList := TStringList.Create;
  try
    LStringList.Clear;

    S4000 := SP4000.ToString;
    S3170 := SP3170.ToString;
    S2520 := SP2520.ToString;
    S2000 := SP2000.ToString;
    S1590 := SP1590.ToString;
    S1260 := SP1260.ToString;
    S1000 := SP1000.ToString;

    LStringList.Append(S4000);
    LStringList.Append(S3170);
    LStringList.Append(S2520);
    LStringList.Append(S2000);
    LStringList.Append(S1590);
    LStringList.Append(S1260);
    LStringList.Append(S1000);

    for j := 0 to 9 do begin
      for i := 0 to LStringList.Count-1 do
      begin
        r := Random(LStringList.Count);
        LStringList.Exchange(i, r);
      end;

      for i := 0 to LStringList.Count -1 do
      begin
        if (LStringList[i] = S4000) or
           (LStringList[i] = S1000) then begin
           LConsequence := '1.0 1.0';
        end;

        if (LStringList[i] = S3170) or
           (LStringList[i] = S2520) or
           (LStringList[i] = S2000) or
           (LStringList[i] = S1590) or
           (LStringList[i] = S1260) then begin
          LConsequence := '0.0 0.0';
        end;
        WriteTemporalBissection(ABlc,
        'Temporal Bissection Test '+(j+1).ToString,
          LStringList[i], LConsequence, True);
      end;
    end;
  finally
    LStringList.Free;
  end;
end;

procedure WriteToConfigurationFile(ADesign : string);
var
  LCondition : char;
  LConditionI : integer = 0;
  LTable : integer = 0;
  LNextBlocOnCriteria : string;

  procedure SetITIForBCD;
  begin
    ITI := TemporalBissectionITI;
  end;

  procedure WriteBCondition; // sem viés
  var
    i : integer;
  begin
    SetITIForBCD;
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem B|sem viés');
    ConfigurationFile.WriteToBloc(
      LConditionI, 'EndTable',
      'DemandTable_A_Bloco'+LTable.ToString);
    WriteMSG(LConditionI, 'M1', MessageE2B);

    // 10 a 40
    LNextBlocOnCriteria := (LConditionI+4).ToString;
    for i := 0 to 3 do
    begin
      Inc(LConditionI);
      ConfigurationFile.WriteToBloc(LConditionI, _Name, 'B1|10 a 40 tentativas|100% reforço|sem viés');
      ConfigurationFile.WriteToBloc(LConditionI, _CrtHitPorcentage, '100');
      ConfigurationFile.WriteToBloc(LConditionI, _NextBlocOnCriteria, LNextBlocOnCriteria);
      if i = 3 then begin
        ConfigurationFile.WriteToBloc(LConditionI, _AutoEndSession, BoolToStr(True));
      end else begin
        ConfigurationFile.WriteToBloc(LConditionI, _AutoEndSession, BoolToStr(False));
      end;
      WriteTraining1(LConditionI, '1.0 1.0');
    end;

    // 60 no mínimo
    for i := 0 to 5 do
    begin
      Inc(LConditionI);
      ConfigurationFile.WriteToBloc(LConditionI, _Name, 'B2|60 tentativas|50% reforço|sem viés');
      if i = 0 then begin
        LTable := LConditionI;
        ConfigurationFile.WriteToBloc(
          LConditionI, 'BeginTable',
          'Experiment2Table_'+LCondition+'_Bloco'+LTable.ToString);
      end;
      WriteTraining2(LConditionI, '0.5 0.5');
    end;

    //// 100 no máximo, critério de 90% de acerto
    //LNextBlocOnCriteria := (LConditionI+5).ToString;
    //for i := 0 to 4 do
    //begin
    //  Inc(LConditionI);
    //  ConfigurationFile.WriteToBloc(LConditionI, _Name, 'B2|10 a 50 tentativas|50% reforço|90% de acerto|sem viés');
    //  ConfigurationFile.WriteToBloc(LConditionI, _CrtHitPorcentage, '90');
    //  ConfigurationFile.WriteToBloc(LConditionI, _NextBlocOnCriteria, LNextBlocOnCriteria);
    //  if i = 4 then begin
    //    ConfigurationFile.WriteToBloc(LConditionI, _AutoEndSession, BoolToStr(True));
    //  end else begin
    //    ConfigurationFile.WriteToBloc(LConditionI, _AutoEndSession, BoolToStr(False));
    //  end;
    //  WriteTraining2(LConditionI, '0.5 0.5');
    //end;

    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem B | pre-teste');
    WriteMSG(LConditionI, 'M2', MessageE2C);

    // testing
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'B|teste|sem viés');
    WriteTrainingTest(LConditionI);
  end;


  procedure WriteCCondition; //viés longo
  var
    i : integer;
  begin
    SetITIForBCD;
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem C|viés longo');
    ConfigurationFile.WriteToBloc(
      LConditionI, 'EndTable',
      'DemandTable_A_Bloco'+LTable.ToString);
    WriteMSG(LConditionI, 'M1', MessageE2B);

    // 10 a 40
    LNextBlocOnCriteria := (LConditionI+4).ToString;
    for i := 0 to 3 do
    begin
      Inc(LConditionI);
      ConfigurationFile.WriteToBloc(LConditionI, _Name, 'C1|10 a 40 tentativas|100% reforço|sem viés');
      ConfigurationFile.WriteToBloc(LConditionI, _CrtHitPorcentage, '100');
      ConfigurationFile.WriteToBloc(LConditionI, _NextBlocOnCriteria, LNextBlocOnCriteria);
      if i = 3 then begin
        ConfigurationFile.WriteToBloc(LConditionI, _AutoEndSession, BoolToStr(True));
      end else begin
        ConfigurationFile.WriteToBloc(LConditionI, _AutoEndSession, BoolToStr(False));
      end;
      WriteTraining1(LConditionI, '1.0 1.0');
    end;

    // 60 no mínimo
    for i := 0 to 5 do
    begin
      Inc(LConditionI);
      if i = 0 then begin
        LTable := LConditionI;
        ConfigurationFile.WriteToBloc(
          LConditionI, 'BeginTable',
          'Experiment2Table_'+LCondition+'_Bloco'+LTable.ToString);
      end;
      ConfigurationFile.WriteToBloc(LConditionI, _Name, 'C1|60 tentativas|80%/20% reforço|viés longo');
      WriteTraining2(LConditionI, '0.8 0.2');
    end;

    //// 100 no máximo, critério de 90% de acerto
    //LNextBlocOnCriteria := (LConditionI+5).ToString;
    //for i := 0 to 4 do
    //begin
    //  Inc(LConditionI);
    //  ConfigurationFile.WriteToBloc(LConditionI, _Name, 'C1|10 a 50 tentativas|80%/20% reforço|90% de acerto|viés longo');
    //  ConfigurationFile.WriteToBloc(LConditionI, _CrtHitPorcentage, '90');
    //  ConfigurationFile.WriteToBloc(LConditionI, _NextBlocOnCriteria, LNextBlocOnCriteria);
    //  if i = 4 then begin
    //    ConfigurationFile.WriteToBloc(LConditionI, _AutoEndSession, BoolToStr(True));
    //  end else begin
    //    ConfigurationFile.WriteToBloc(LConditionI, _AutoEndSession, BoolToStr(False));
    //  end;
    //  WriteTraining2(LConditionI, '0.8 0.2');
    //end;
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem C | pre-teste');
    WriteMSG(LConditionI, 'M2', MessageE2C);

    // testing
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'C|teste|viés longo');
    WriteTrainingTest(LConditionI);
  end;

  procedure WriteDCondition; //viés curto
  var
    i : integer;
  begin
    SetITIForBCD;
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem D|viés curto');
    ConfigurationFile.WriteToBloc(
      LConditionI, 'EndTable',
      'DemandTable_A_Bloco'+LTable.ToString);
    WriteMSG(LConditionI, 'M1', MessageE2B);

    // 10 a 40
    LNextBlocOnCriteria := (LConditionI+4).ToString;
    for i := 0 to 3 do
    begin
      Inc(LConditionI);
      ConfigurationFile.WriteToBloc(LConditionI, _Name, 'D1|10 a 40 tentativas|100% reforço|sem viés');
      ConfigurationFile.WriteToBloc(LConditionI, _CrtHitPorcentage, '100');
      ConfigurationFile.WriteToBloc(LConditionI, _NextBlocOnCriteria, LNextBlocOnCriteria);
      if i = 3 then begin
        ConfigurationFile.WriteToBloc(LConditionI, _AutoEndSession, BoolToStr(True));
      end else begin
        ConfigurationFile.WriteToBloc(LConditionI, _AutoEndSession, BoolToStr(False));
      end;
      WriteTraining1(LConditionI, '1.0 1.0');
    end;

    // 60 no mínimo
    for i := 0 to 5 do
    begin
      Inc(LConditionI);
      ConfigurationFile.WriteToBloc(LConditionI, _Name, 'D1|60 tentativas|20%/80% reforço|viés curto');
      if i = 0 then begin
        LTable := LConditionI;
        ConfigurationFile.WriteToBloc(
          LConditionI, 'BeginTable',
          'Experiment2Table_'+LCondition+'_Bloco'+LTable.ToString);
      end;
      WriteTraining2(LConditionI, '0.2 0.8');
    end;

    //// 100 no máximo, critério de 90% de acerto
    //LNextBlocOnCriteria := (LConditionI+5).ToString;
    //for i := 0 to 4 do
    //begin
    //  Inc(LConditionI);
    //  ConfigurationFile.WriteToBloc(LConditionI, _Name, 'D1|10 a 50 tentativas|20%/80% reforço|90% de acerto|viés curto');
    //  ConfigurationFile.WriteToBloc(LConditionI, _CrtHitPorcentage, '90');
    //  ConfigurationFile.WriteToBloc(LConditionI, _NextBlocOnCriteria, LNextBlocOnCriteria);
    //  if i = 4 then begin
    //    ConfigurationFile.WriteToBloc(LConditionI, _AutoEndSession, BoolToStr(True));
    //  end else begin
    //    ConfigurationFile.WriteToBloc(LConditionI, _AutoEndSession, BoolToStr(False));
    //  end;
    //  WriteTraining2(LConditionI, '0.2 0.8');
    //end;

    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem D | pre-teste');
    WriteMSG(LConditionI, 'M2', MessageE2C);

    // testing
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'D|teste|viés curto');
    WriteTrainingTest(LConditionI);
  end;

begin
  SetupStimuli;
  if (ADesign = 'ABA')  or (ADesign = 'A') then
  begin
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem A0');
    WriteMSG(LConditionI, 'M0', MessageA0);
  end;

  for LCondition in ADesign do
  case LCondition of
    'A': WriteACondition(LConditionI, LTable, '2', ADesign);
    'B': WriteBCondition;
    'C': WriteCCondition;
    'D': WriteDCondition;
  end;
end;

end.

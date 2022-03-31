unit Experiments.Arrasta.Circular;

{$mode objfpc}{$H+}

interface

procedure WriteToConfigurationFile(ADesign : string);

implementation

uses Classes, SysUtils
   , Constants
   , StrUtils
   , LazFileUtils
   , Experiments.Vinicius.Comum
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   ;

var
  StringListPositions : TStringList;

procedure WriteOperantTask(ABlc : integer;
  AName: string; ADelay: string; ASample: string;
  AVisual: Boolean; AIsTestTrial: Boolean);
var
  i : integer;
  r: LongInt;
begin
    for i := 0 to StringListPositions.Count-1 do
    begin
      r := Random(StringListPositions.Count);
      StringListPositions.Exchange(i, r);
    end;
    i := ConfigurationFile.TrialCount[ABlc]+1;
    with ConfigurationFile do
    begin
      WriteToTrial(i, ABlc, _Name, AName);
      WriteToTrial(i, ABlc, _Cursor, '0');
      WriteToTrial(i, ABlc, _Kind, T_DMTS);
      if AVisual then begin
        WriteToTrial(i, ABlc, _Style, 'Visual');
      end else begin
        WriteToTrial(i, ABlc, _Style, 'Auditivo');
      end;
      WriteToTrial(i, ABlc, _Consequence, AIsTestTrial.ToString);
      WriteToTrial(i, ABlc, _Delay, ADelay);
      WriteToTrial(i, ABlc, _Samp, ASample);
      WriteToTrial(i, ABlc, _Samp+_cBnd, '4');
      WriteToTrial(i, ABlc, _ITI, ITIBase.ToString);
      WriteToTrial(i, ABlc, _Comp+'1', ASample);
      WriteToTrial(i, ABlc, _Comp+'1'+_cBnd, StringListPositions[0]);
      case ASample of
        'A1' : begin
          WriteToTrial(i, ABlc, _Comp+'2', 'A2');
          WriteToTrial(i, ABlc, _Comp+'2'+_cBnd, StringListPositions[1]);
          WriteToTrial(i, ABlc, _Comp+'3', 'A3');
          WriteToTrial(i, ABlc, _Comp+'3'+_cBnd, StringListPositions[2]);
        end;
        'A2' : begin
          WriteToTrial(i, ABlc, _Comp+'2', 'A1');
          WriteToTrial(i, ABlc, _Comp+'2'+_cBnd, StringListPositions[1]);
          WriteToTrial(i, ABlc, _Comp+'3', 'A3');
          WriteToTrial(i, ABlc, _Comp+'3'+_cBnd, StringListPositions[2]);
        end;
        'A3' : begin
          WriteToTrial(i, ABlc, _Comp+'2', 'A1');
          WriteToTrial(i, ABlc, _Comp+'2'+_cBnd, StringListPositions[1]);
          WriteToTrial(i, ABlc, _Comp+'3', 'A2');
          WriteToTrial(i, ABlc, _Comp+'3'+_cBnd, StringListPositions[2]);
        end;
        'S1' : begin
          WriteToTrial(i, ABlc, _Comp+'2', 'S2');
          WriteToTrial(i, ABlc, _Comp+'2'+_cBnd, StringListPositions[1]);
          WriteToTrial(i, ABlc, _Comp+'3', 'S3');
          WriteToTrial(i, ABlc, _Comp+'3'+_cBnd, StringListPositions[2]);
        end;
        'S2' : begin
          WriteToTrial(i, ABlc, _Comp+'2', 'S1');
          WriteToTrial(i, ABlc, _Comp+'2'+_cBnd, StringListPositions[1]);
          WriteToTrial(i, ABlc, _Comp+'3', 'S3');
          WriteToTrial(i, ABlc, _Comp+'3'+_cBnd, StringListPositions[2]);
        end;
        'S3' : begin
          WriteToTrial(i, ABlc, _Comp+'2', 'S1');
          WriteToTrial(i, ABlc, _Comp+'2'+_cBnd, StringListPositions[1]);
          WriteToTrial(i, ABlc, _Comp+'3', 'S2');
          WriteToTrial(i, ABlc, _Comp+'3'+_cBnd, StringListPositions[2]);
        end;
      end;
    end;
end;

procedure WriteA(var ABlc: integer);
var
  i : integer;
  j : integer;
  r : integer;
  LStringList : TStringList;
begin
  // 0s, 2s, 4s, 6s, 8s ou 10s
  // 72 tentativas
  LStringList := TStringList.Create;
  try
    LStringList.Append(SP00000.ToString+#32+'A1');
    LStringList.Append(SP02000.ToString+#32+'A1');
    LStringList.Append(SP04000.ToString+#32+'A1');
    LStringList.Append(SP06000.ToString+#32+'A1');
    LStringList.Append(SP08000.ToString+#32+'A1');
    LStringList.Append(SP10000.ToString+#32+'A1');

    LStringList.Append(SP00000.ToString+#32+'A2');
    LStringList.Append(SP02000.ToString+#32+'A2');
    LStringList.Append(SP04000.ToString+#32+'A2');
    LStringList.Append(SP06000.ToString+#32+'A2');
    LStringList.Append(SP08000.ToString+#32+'A2');
    LStringList.Append(SP10000.ToString+#32+'A2');

    LStringList.Append(SP00000.ToString+#32+'A3');
    LStringList.Append(SP02000.ToString+#32+'A3');
    LStringList.Append(SP04000.ToString+#32+'A3');
    LStringList.Append(SP06000.ToString+#32+'A3');
    LStringList.Append(SP08000.ToString+#32+'A3');
    LStringList.Append(SP10000.ToString+#32+'A3');
    for j := 0 to 4 do begin
      for i := 0 to LStringList.Count-1 do
      begin
        r := Random(LStringList.Count);
        LStringList.Exchange(i, r);
      end;

      for i := 0 to LStringList.Count -1 do
      begin
        WriteOperantTask(ABlc,
          'DMTS' + LStringList[i],
           ExtractDelimited(1,LStringList[i],[#32]),
           ExtractDelimited(2,LStringList[i],[#32]),
           False, True);
      end;
    end;
  finally
    LStringList.Free;
  end;
end;

procedure WriteB(ABlc : integer);
var
  i : integer;
  j : integer;
  r : integer;
  LStringList : TStringList;
begin
    // 0s, 2s, 4s ou 6s,
    // 18 tentativas
    LStringList := TStringList.Create;
    try
      LStringList.Append(SP00000.ToString+#32+'A1');
      LStringList.Append(SP00000.ToString+#32+'A2');
      LStringList.Append(SP00000.ToString+#32+'A3');

      for j := 0 to 5 do begin
        for i := 0 to LStringList.Count-1 do
        begin
          r := Random(LStringList.Count);
          LStringList.Exchange(i, r);
        end;

        for i := 0 to LStringList.Count -1 do
        begin
          WriteOperantTask(ABlc,
            'DMTS' + LStringList[i],
             ExtractDelimited(1,LStringList[i],[#32]),
             ExtractDelimited(2,LStringList[i],[#32]),
             False, False);
        end;
      end;
    finally
      LStringList.Free;
    end;
end;

procedure WriteC(ABlc : integer);
var
  i : integer;
  j : integer;
  r : integer;
  LStringList : TStringList;
  LTrainingDelay : string;
  LDelay : string;
  LIsTestTrial : Boolean;
begin
  // 0s, 2s, 4s ou 6s,
  // 18 tentativas
  LTrainingDelay := SP06000.ToString;

  LStringList := TStringList.Create;
  try
    LStringList.Append(SP00000.ToString+#32+'A1');
    LStringList.Append(SP02000.ToString+#32+'A1');
    LStringList.Append(SP04000.ToString+#32+'A1');
    LStringList.Append(SP06000.ToString+#32+'A1');
    LStringList.Append(SP08000.ToString+#32+'A1');
    LStringList.Append(SP10000.ToString+#32+'A1');

    LStringList.Append(SP00000.ToString+#32+'A2');
    LStringList.Append(SP02000.ToString+#32+'A2');
    LStringList.Append(SP04000.ToString+#32+'A2');
    LStringList.Append(SP06000.ToString+#32+'A2');
    LStringList.Append(SP08000.ToString+#32+'A2');
    LStringList.Append(SP10000.ToString+#32+'A2');

    LStringList.Append(SP00000.ToString+#32+'A3');
    LStringList.Append(SP02000.ToString+#32+'A3');
    LStringList.Append(SP04000.ToString+#32+'A3');
    LStringList.Append(SP06000.ToString+#32+'A3');
    LStringList.Append(SP08000.ToString+#32+'A3');
    LStringList.Append(SP10000.ToString+#32+'A3');
    for j := 0 to 5 do begin
      for i := 0 to LStringList.Count-1 do
      begin
        r := Random(LStringList.Count);
        LStringList.Exchange(i, r);
      end;

      for i := 0 to LStringList.Count -1 do
      begin
        LDelay := ExtractDelimited(1,LStringList[i],[#32]);
        LIsTestTrial := LDelay <> LTrainingDelay;
        WriteOperantTask(ABlc,
          'DMTS' + LStringList[i],
          LDelay,
          ExtractDelimited(2,LStringList[i],[#32]),
          False, LIsTestTrial);
      end;
    end;
  finally
    LStringList.Free;
  end;
end;



procedure WriteToConfigurationFile(ADesign: string);
var
  i : integer;
  LCondition : string = '';
  LConditionI : integer = 0;
  procedure WriteA1Condition;
  begin
    // message
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem A1 - Visual');
    WriteMSG(LConditionI, 'A1-Visual', MessageE1A1);

    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'A1 - Visual');

    WriteA(LConditionI);
  end;

  procedure WriteB1Condition;
  begin
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem B1 - Visual');
    WriteMSG(LConditionI, 'B1-Visual', MessageE1B1);

    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'B1 - Visual');

    ConfigurationFile.WriteToBloc(LConditionI, _CrtHitValue, '18');
    ConfigurationFile.WriteToBloc(LConditionI, _MaxBlcRepetition, '4');
    WriteB(LConditionI);
  end;

  procedure WriteA2Condition;
  begin
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem A2 - Visual');
    WriteMSG(LConditionI, 'A2-Visual', MessageE1C1);

    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'A2 - Visual');

    WriteA(LConditionI);
  end;

  procedure WriteC1Condition;
  begin
    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem C1 - Visual');
    WriteMSG(LConditionI, 'C1-Visual', MessageE1C1);

    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'C1 - Visual');

    WriteC(LConditionI);

    Inc(LConditionI);
    ConfigurationFile.WriteToBloc(LConditionI, _Name, 'Mensagem Final 1');
    WriteMSG(LConditionI, 'M1-Final', MessageE1Final1);

  end;

begin
  SetupStimuli;
  for i := 0 to WordCount(ADesign, [#32]) -1 do begin
    LCondition := ExtractDelimited(i+1, ADesign, [#32]);
    case LCondition of
      'A1': WriteA1Condition;
      'B1': WriteB1Condition;
      'A2': WriteA2Condition;
      'C1': WriteC1Condition;
    end;
  end;
end;

initialization
  StringListPositions := TStringList.Create;
  StringListPositions.Append('0');
  StringListPositions.Append('1');
  StringListPositions.Append('2');
  StringListPositions.Append('3');
  //StringListPositions.Append('4'); reserved for sample
  StringListPositions.Append('5');
  StringListPositions.Append('6');
  StringListPositions.Append('7');
  StringListPositions.Append('8');

finalization
  StringListPositions.Free;

end.

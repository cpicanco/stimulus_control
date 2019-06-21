unit ReinforcementProbability;

{$mode objfpc}{$H+}

{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, Grids, fgl;

type

  { TVirtualTrial }

  TVirtualTrial = record
    Index : integer;
    HasConsequence : Boolean;
    class operator =(a,b : TVirtualTrial) : Boolean;
  end;

  TTrialList = specialize TFPGList<TVirtualTrial>;

  procedure RemoveConsequence(StringGrid : TStringGrid;
    ATargetCol: Integer; APorcentage: ShortInt = 25);

implementation

uses math;

procedure RemoveConsequence(StringGrid: TStringGrid;
  ATargetCol: Integer; APorcentage: ShortInt);
var
  TrialCandidate : TVirtualTrial;
  PositiveTrials : TTrialList;
  TrialsToRemove : TTrialList;

  TrialsToRemoveConsequence : integer;
  i: Integer;

  function NotInList(Item :TVirtualTrial) : Boolean;
  var
    Trial : TVirtualTrial;
  begin
    Result := False;
    for Trial in TrialsToRemove do
      if Trial = Item then Exit;
    Result := True;
  end;

  function HasConsequence(StringGrid: TStringGrid; Row: integer): Boolean;
  begin
    case StringGrid.Cells[1, Row] of
      'Positiva' : Result := True;
      'Negativa' : Result := False;
    else
      Result := False;
    end;
  end;

begin
  PositiveTrials := TTrialList.Create;
  TrialsToRemove := TTrialList.Create;
  try
    for i := 1 to StringGrid.RowCount-1 do
    begin
      StringGrid.Cells[ATargetCol, i] := '';
      TrialCandidate.Index := i;
      TrialCandidate.HasConsequence := HasConsequence(StringGrid, i);

      if TrialCandidate.HasConsequence then
        PositiveTrials.Add(TrialCandidate);
    end;

    TrialsToRemoveConsequence := (PositiveTrials.Count*APorcentage) div 100;
    for i := 0 to TrialsToRemoveConsequence-1 do
    begin
      repeat
        TrialCandidate := PositiveTrials[RandomRange(1, PositiveTrials.Count)-1];
      until NotInList(TrialCandidate);
      TrialsToRemove.Add(TrialCandidate);
      StringGrid.Cells[ATargetCol, TrialCandidate.Index] := 'False';
    end;

  finally
    PositiveTrials.Free;
    TrialsToRemove.Free;
  end;
end;

{ TVirtualTrial }

class operator TVirtualTrial.=(a, b: TVirtualTrial): Boolean;
begin
  Result := (a.Index = b.Index);
end;

end.


unit Keyboard.FrameResponse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Keyboard.KeySequence;

type
  TRelation = (Equality, Difference);

  { TFakeStimulus }

  TFakeStimulus = class
  public
    ID : integer;
    IndexLeft: integer;
    IndexRight: integer;
    Key: word;
    constructor Create(AID: integer; AIndex: array of integer; AKey: word = 0);
  end;

  TStimuli = array of TFakeStimulus;
  TRelations = array [0..7] of TRelation;

  { TFrameResponse }

  TFrameResponse = class
  private
    FAllBefore : boolean;
    FStimuli: TStimuli;
    FRelations: TRelations;
    procedure LoadFromDefault;
    function WordRangeFromStimuli(AStimuli: TStimuli): TWordRange;
  public
    constructor Create; overload;
    constructor Create(AParameters: TStringList); overload;
    destructor Destroy; override;
    procedure LoadFromParameters(AParameters: TStringList);
    procedure Parameters(var AContainer : TStringList);
    function EClassBrothers(AStimulus: TFakeStimulus; AStimuli: TStimuli): TStimuli;
    //function EClassMissingBrothers(AStimuli : TStimuli) : TStimuli;
    function Operate(var A: TFakeStimulus; Relations: array of TRelation): TStimuli;
    function KeySequenceFromFrame(AOwner: TComponent): TKeySequence;
    function FrameToStr : string;
  end;

implementation

{ TFakeStimulus }

uses Experiments.Augusto, Dialogs;

constructor TFakeStimulus.Create(AID: integer; AIndex: array of integer;
  AKey: word);
begin
  ID := AID;
  IndexLeft := AIndex[0];
  IndexRight := AIndex[1];
  Key := AKey;
end;

{ TFrameResponse }

constructor TFrameResponse.Create;
begin
  inherited Create;
  LoadFromDefault;
end;

constructor TFrameResponse.Create(AParameters: TStringList);
begin
  inherited Create;
  LoadFromParameters(AParameters);
end;

destructor TFrameResponse.Destroy;
var
  i: integer;
begin
  for i := Low(FStimuli) to High(FStimuli) do
    FStimuli[i].Free;
  SetLength(FStimuli, 0);
  inherited Destroy;
end;

procedure TFrameResponse.LoadFromParameters(AParameters: TStringList);
var
  i: integer;
  C : char;
  CL : integer;
  CR : integer;
  S : string;
begin
  S := AParameters.Values['BottomOrientation'];
  FAllBefore := StrToBool(S);
  SetLength(FStimuli, High(IndexRange));
  for i := Low(FStimuli) to High(FStimuli) do
  begin
    C := AParameters.Values['KC'+(i+1).ToString][1];
    CL := AParameters.Values['KL'+(i+1).ToString].ToInteger-1;
    CR := AParameters.Values['KR'+(i+1).ToString].ToInteger-1;
    FStimuli[i] := TFakeStimulus.Create(i,[CL, CR], Ord(C));
  end;

  for i := Low(FRelations) to High(FRelations) do
  begin
    //ShowMessage(AParameters.Values['Cue'+(i+1).ToString]);
    case AParameters.Values['Cue'+(i+1).ToString] of
      'Diffe': FRelations[i] := Difference;
      'Equal': FRelations[i] := Equality;
    end;
  end;
end;

procedure TFrameResponse.Parameters(var AContainer: TStringList);
var
  i: integer;
begin
  with AContainer do
  begin
    AContainer.Values['BottomOrientation'] := BoolToStr(FAllBefore);
    for i := Low(FStimuli) to High(FStimuli) - 1 do
    begin
      AContainer.Values['KC'+(i+1).ToString] := Char(FStimuli[i].Key);
      AContainer.Values['KL'+(i+1).ToString] := IntToStr(FStimuli[i].IndexLeft+1);
      AContainer.Values['KR'+(i+1).ToString] := IntToStr(FStimuli[i].IndexRight+1);
    end;

    for i := Low(FRelations) to High(FRelations) do
      case FRelations[i] of
        Difference : AContainer.Values['Cue'+(i+1).ToString] := 'Diffe';
        Equality   : AContainer.Values['Cue'+(i+1).ToString] := 'Equal';
      end;
  end;
end;

procedure TFrameResponse.LoadFromDefault;
var
  i: integer;
begin
  FAllBefore := True;
  SetLength(FStimuli, High(IndexRange));
  for i := Low(IndexRange) to High(IndexRange) - 1 do
    case i of
      0: FStimuli[i] := TFakeStimulus.Create(i,[i, i], Ord('Z'));
      1: FStimuli[i] := TFakeStimulus.Create(i,[i, i], Ord('C'));
      2: FStimuli[i] := TFakeStimulus.Create(i,[i, i], Ord('B'));
      3: FStimuli[i] := TFakeStimulus.Create(i,[i, i], Ord('M'));
    end;
  for i := Low(FRelations) to High(FRelations) do
    case i of
      0, 1, 4, 6 : FRelations[i] := Difference;
      2, 3, 5, 7 : FRelations[i] := Equality;
    end;
end;

function TFrameResponse.WordRangeFromStimuli(AStimuli: TStimuli): TWordRange;
var
  i: integer;
begin
  for i := Low(IndexRange) to High(IndexRange) do
  begin
    Result[i] := Ord(#24);
    if i <= High(AStimuli) then
      Result[i] := AStimuli[i].Key;
  end;
end;

{
  AStimulus.Index1 refers to 0, 1, 2 or 3 (left equal/different relations)
  AStimulus.Index2 refers to 4, 5, 6 or 7 (right equal/different relations)
  AStimulus.ID is an unique identifier
}
function TFrameResponse.EClassBrothers(AStimulus: TFakeStimulus;
  AStimuli: TStimuli): TStimuli;
var
  i: integer;
begin
  SetLength(EClassBrothers, 0);
  for i := Low(AStimuli) to High(AStimuli) do
  begin
    if (AStimulus.ID <> AStimuli[i].ID) then
    begin
      SetLength(EClassBrothers, Length(EClassBrothers) + 1);
      EClassBrothers[High(EClassBrothers)] := AStimuli[i];
    end;
  end;
end;

function TFrameResponse.Operate(var A: TFakeStimulus;
  Relations: array of TRelation): TStimuli;
var
  LRelation: TRelation;
  LFirstResult: boolean = True;
  i : integer;
begin
  for LRelation in Relations do
  begin
    if LRelation = Difference then
    begin
      LFirstResult := not LFirstResult;
      Break;
    end;
  end;

  if LFirstResult then begin
    SetLength(Result, 1);
    Result[0] := A;
  end else begin
    Result := EClassBrothers(A, FStimuli);
  end;
end;

function TFrameResponse.KeySequenceFromFrame(AOwner: TComponent): TKeySequence;
var
  i : integer;
  v: TWordMatrix;
  LStimuli: TStimuli;
  //LRelation : TRelation;
begin
  Result := TKeySequence.Create(AOwner);
  if FAllBefore then
    for i := Low(v) to High(v)-1 do
    begin
      LStimuli := Operate(FStimuli[i],
        [FRelations[FStimuli[i].IndexLeft], FRelations[FStimuli[i].IndexRight]]);
      v[i] := WordRangeFromStimuli(LStimuli);
    end
  else
    for i := Low(v) to High(v)-1 do
    begin
      LStimuli := Operate(FStimuli[i],
        [FRelations[FStimuli[i].IndexLeft], FRelations[FStimuli[i].IndexRight]]);
      v[High(v)-1 - i] := WordRangeFromStimuli(LStimuli);
    end;
  v[High(v)][0] := 13;
  v[High(v)][1] := 24;
  v[High(v)][2] := 24;
  v[High(v)][3] := 24;
  v[High(v)][4] := 24;
  Result.LoadFromWords(v);
  //for LRelation in FRelations do
  //begin
  //  if LRelation = Difference then
  //  begin
  //    Result.UseNegation:=True;
  //  end;
  //end;
end;

function TFrameResponse.FrameToStr: string;
var
  i: integer;
  L: string;
begin
  Result := 'Orientation: '+ BoolToStr(FAllBefore, 'AllBefore', 'AllAfter') + #10;
  for i := Low(FStimuli) to High(FStimuli) do
  begin
    Result := Result + 'KC'+(i+1).ToString+': '+ Char(FStimuli[i].Key) + #10;
    Result := Result + 'KL'+(i+1).ToString+': '+ IntToStr(FStimuli[i].IndexLeft) + #10;
    Result := Result + 'KR'+(i+1).ToString+': '+ IntToStr(FStimuli[i].IndexRight) + #10;
  end;

  for i := Low(FRelations) to High(FRelations) do
  begin
    WriteStr(L, FRelations[i]);
    Result := Result + 'R'+(i+1).ToString+': '+ L + #10;
  end;
end;

end.

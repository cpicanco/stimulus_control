unit Experiments.Eduardo.Experimento3.Tables;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Loggers.Tables;

type

  TTableTrial = record
    Tag       : integer;
    Result    : integer;
    IsTest    : Boolean;
    Latency   : Extended;
    VTDelays  : TFloatColumn;
    ResponsesTime : Extended;
    Responses : TFloatColumn;
  end;

  { TExperiment3Table }

  TExperiment3Table = class(TComponent)
  private
    FIsFirstTrial : Boolean;
    FFirstTrial : integer;
    FTrials : array of TTableTrial;
    function GetFilename : string;
    function GetTrial(ATrial: integer) : integer;
    procedure WriteTable;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  public
    procedure AddRow(ATrial: integer; ALatency : Extended;
      AResponsesTime : Extended;
      AResult: integer; IsTestTrial: Boolean);
    procedure AddResponse(ATrial : integer;
      AResponse : Extended);
    procedure AddDelay(ATrial : integer;
      ADelay : Extended);
  end;

implementation

uses Math, Session.Configuration.GlobalContainer;

{ TExperiment3Table }

constructor TExperiment3Table.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsFirstTrial := True;
  FFirstTrial := -1;
end;

destructor TExperiment3Table.Destroy;
var
  i: Integer;
begin
  WriteTable;
  for i := Low(FTrials) to High(FTrials) do begin
    FTrials[i].Responses.Free;
    FTrials[i].VTDelays.Free;
  end;
  inherited Destroy;
end;

procedure TExperiment3Table.AddRow(ATrial: integer; ALatency: Extended;
  AResponsesTime: Extended; AResult: integer; IsTestTrial: Boolean);
var
  i: Integer;
begin
  i := GetTrial(ATrial);
  FTrials[i].IsTest := IsTestTrial;
  FTrials[i].Latency := ALatency;
  FTrials[i].ResponsesTime := AResponsesTime;
  FTrials[i].Result := AResult;
end;

procedure TExperiment3Table.AddResponse(ATrial: integer; AResponse: Extended);
var
  i : integer;
begin
  i := GetTrial(ATrial);
  FTrials[i].Responses.Add(AResponse);
end;

procedure TExperiment3Table.AddDelay(ATrial: integer; ADelay: Extended);
var
  i : integer;
begin
  i := GetTrial(ATrial);
  FTrials[i].VTDelays.Add(ADelay);
end;

function TExperiment3Table.GetFilename: string;
begin
  Result := GlobalContainer.BaseFilename + #95 + Self.name + '.data';
end;

function TExperiment3Table.GetTrial(ATrial: integer): integer;
begin
  if FIsFirstTrial then begin
    FIsFirstTrial := False;
    FFirstTrial := ATrial;
  end;
  Result := ATrial - FFirstTrial;
  if (Result > High(FTrials)) or (Length(FTrials) = 0) then begin
    SetLength(FTrials, Length(FTrials)+1);
    FTrials[Result].Tag := Result+1;
    FTrials[Result].Responses := TFloatColumn.Create;
    FTrials[Result].VTDelays  := TFloatColumn.Create;
  end;
end;

procedure TExperiment3Table.WriteTable;
const
  NONE = -1;
  MISS = 0;
  HIT  = 1;
var
  Table : TTabDelimitedReport;
  i: Integer;
  Result : string;
  function CalculatePorcentage(A, B : integer):string;
  begin
    Result := ((A*100)/B).ToString;
  end;
begin
  Table := TTabDelimitedReport.Create;
  Table.Filename := GetFilename;
  try
    Table.WriteRow([
      'Tentativa',
      'Latência',
      'Respostas (n)',
      'Respostas (s)',
      'Pausa Pós Reforço (n)',
      'Pausa Pós Reforço (s)',
      'Resultado']);
    for i := Low(FTrials) to High(FTrials) do begin
      case FTrials[i].Result of
         NONE : Result := 'TESTE';
         MISS : Result := '0';
         HIT  : Result := '1';
      end;
      Table.WriteRow([
        FTrials[i].Tag.ToString,
        FTrials[i].Latency.ToString,
        FTrials[i].Responses.Count.ToString,
        FTrials[i].ResponsesTime.ToString,
        FTrials[i].VTDelays.Count.ToString,
        FTrials[i].VTDelays.Sum.ToString,
        Result
      ]);
    end;

    //Table.WriteRow(['']);
    //Table.WriteRow(['Média da Latência', 'Porcentagem de Acertos']);
    //
  finally
    Table.Free;
  end;
end;

end.


unit Experiments.Eduardo.Experimento1.Tables;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Loggers.Tables, Loggers.Helpers;

type

  { TExperiment1Table }

  TExperiment1Table = class(TComponent)
  private
    FCategories : TIntegerColumn;
    FLatencies  : TFloatColumn;
    function GetFilename : string;
    procedure WriteTable;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  public
    procedure AddRow(ALatency : Extended; ACategory: integer);

  end;

implementation

uses Math, Session.Configuration.GlobalContainer;

{ TExperiment1Table }

constructor TExperiment1Table.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCategories := TIntegerColumn.Create;
  FLatencies  := TFloatColumn.Create;
end;

destructor TExperiment1Table.Destroy;
begin
  WriteTable;
  FCategories.Free;
  FLatencies.Free;
  inherited Destroy;
end;

procedure TExperiment1Table.AddRow(ALatency: Extended; ACategory: integer);
begin
  FCategories.Add(ACategory);
  FLatencies.Add(ALatency);
end;

function TExperiment1Table.GetFilename: string;
begin
  Result := GlobalContainer.BaseFilename + #95 + Self.name + '.data';
end;

procedure TExperiment1Table.WriteTable;
var
  Table : TTabDelimitedReport;
  Result  : string;
  Results   : array of Extended;
  Latencies : array of Extended;
  Porcentage : Extended;
  i: Integer;

  procedure Append(ALatency : Extended);
  begin
    if ALatency > 0 then begin
      SetLength(Latencies, Length(Latencies)+1);
      Latencies[High(Latencies)] := ALatency;
    end;
  end;

begin
  Results := nil;
  Latencies := nil;
  SetLength(Results, FCategories.Count);

  Table := TTabDelimitedReport.Create;
  Table.Filename := GetFilename;
  try
    Table.WriteRow(['Latência', 'Resultado']);
    for i := 0 to FCategories.Count -1 do begin
      Append(FLatencies[i]);
      case FCategories[i] of
        -1 : begin
          Result := 'NA';
          Results[i] := 0;
        end;

        0 : begin
          Result := '0';
          Results[i] := 0;
        end;

        1 : begin
          Result := '1';
          Results[i] := 1;
        end;
      end;
      Table.WriteRow([FLatencies[i].ToString, Result]);
    end;
    Porcentage := (Sum(Results)*100)/Length(Results);
    Table.WriteRow(['']);
    Table.WriteRow(['Média da Latência', 'Porcentagem de Acertos']);
    Table.WriteRow([Mean(Latencies).ToString, Porcentage.ToString]);
  finally
    Table.Free;
  end;
end;

end.


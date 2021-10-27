unit Experiments.Eduardo.Experimento3.Tables;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Loggers.Tables;

type
  TDelays = array of Extended;

  TTableTrial = record
    Tag            : integer;
    Result         : integer;
    IsTest         : Boolean;
    Latency        : Extended;
    ResponsesTime  : Extended;
    Responses      : TFloatColumn;
    VTDelaysBegin  : TFloatColumn;
    VTDelaysEnd    : TFloatColumn;
    Delays         : TDelays;
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
    procedure CreateTrial(ATrial: integer);
    procedure AddRow(ATrial: integer; ALatency : Extended;
      AResponsesTime : Extended;
      AResult: integer; IsTestTrial: Boolean);
    procedure AddResponse(ATrial : integer;
      AResponse : Extended);
    procedure AddDelayBegin(ATrial : integer;
      ADelay : Extended);
    procedure AddDelayEnd(ATrial : integer;
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
    FTrials[i].VTDelaysBegin.Free;
    FTrials[i].VTDelaysEnd.Free;
    FTrials[i].Delays := nil;
  end;
  inherited Destroy;
end;

procedure TExperiment3Table.CreateTrial(ATrial: integer);
begin
  if FIsFirstTrial then begin
    FTrials := nil;
    SetLength(FTrials, 0);
    FIsFirstTrial := False;
    FFirstTrial := ATrial;
  end;
  SetLength(FTrials, Length(FTrials)+1);
  FTrials[High(FTrials)].Tag := High(FTrials)+1;
  FTrials[High(FTrials)].Responses := TFloatColumn.Create;
  FTrials[High(FTrials)].VTDelaysBegin := TFloatColumn.Create;
  FTrials[High(FTrials)].VTDelaysEnd := TFloatColumn.Create;
end;

procedure TExperiment3Table.AddRow(ATrial: integer; ALatency: Extended;
  AResponsesTime: Extended; AResult: integer; IsTestTrial: Boolean);
var
  i: Integer;
begin
  i := GetTrial(ATrial);
  try
    FTrials[i].IsTest := IsTestTrial;
    FTrials[i].Latency := ALatency;
    FTrials[i].ResponsesTime := AResponsesTime;
    FTrials[i].Result := AResult;
  except
    on E : Exception do begin
      i := 0;
    end;
  end;
end;

procedure TExperiment3Table.AddResponse(ATrial: integer; AResponse: Extended);
var
  i : integer;
begin
  i := GetTrial(ATrial);
  try
    FTrials[i].Responses.Add(AResponse);
  except
    on E : Exception do begin
      i := 0;
    end;
  end;
end;

procedure TExperiment3Table.AddDelayBegin(ATrial: integer; ADelay: Extended);
var
  i : integer;
begin
  i := GetTrial(ATrial);
  try
    FTrials[i].VTDelaysBegin.Add(ADelay);
  except
    on E : Exception do begin
      i := 0;
    end;
  end;
end;

procedure TExperiment3Table.AddDelayEnd(ATrial: integer; ADelay: Extended);
var
  i : integer;
begin
  i := GetTrial(ATrial);
  try
    FTrials[i].VTDelaysEnd.Add(ADelay);
  except
    on E : Exception do begin
      i := 0;
    end;
  end;
end;

function TExperiment3Table.GetFilename: string;
begin
  Result := GlobalContainer.BaseFilename + #95 + Self.name + '.data';
end;

function TExperiment3Table.GetTrial(ATrial: integer): integer;
begin
  Result := ATrial - FFirstTrial;
end;

procedure TExperiment3Table.WriteTable;
const
  NONE = -1;
  MISS = 0;
  HIT  = 1;
var
  Table : TTabDelimitedReport;
  i, j : Integer;
  Result : string;
  DelaysCount  : integer;
  DelaysTime   : Extended;
  Rate         : Extended;
  RunningRate  : Extended;
  FirstQuartil : Extended;
  TrialsWithReinforcement : integer = 0;
  QuarterLife : Extended;
  Quarters : array of Extended = nil;

  function GetFirstQuartil(ATrial: TTableTrial) : Extended;
  var
    Q1 : Integer;
    i : integer;
  begin
    if ATrial.Responses.Count > 0 then begin
      Q1 := Round(ATrial.Responses.Count*0.25);
      Result := ATrial.Responses[Q1];
      for i := Low(ATrial.Delays) to High(ATrial.Delays) do begin
        if ATrial.VTDelaysBegin[i] < ATrial.Responses[Q1] then begin
          Result := Result - ATrial.Delays[i];
        end;
      end;
    end else begin
      Result := 0
    end;
  end;

  function GetDelays(ATrial : TTableTrial):  TDelays;
  var i : integer;
  begin
    Result := nil;
    if ATrial.VTDelaysBegin.Count > 0 then begin
      SetLength(Result, ATrial.VTDelaysBegin.Count);
      if ATrial.VTDelaysBegin.Count = ATrial.VTDelaysEnd.Count then begin
        for i := 0 to ATrial.VTDelaysBegin.Count-1 do begin
          Result[i] := ATrial.VTDelaysEnd[i] - ATrial.VTDelaysBegin[i];
        end;
      end else begin
        Exception.Create('VTDelay error');
      end;
    end else begin
      SetLength(Result, 0);
    end;
  end;
begin
  Table := TTabDelimitedReport.Create;
  Table.Filename := GetFilename;
  try
    Table.WriteRow([
      'Tentativa',
      'Latência',
      'Frequência de Respostas- n',
      'Tempo das Respostas- s',
      'Taxa de Respostas- n/s',
      'Taxa de Respostas- n/(s-s2)',
      'Q1',
      'Q1/(s-s2)',
      'Pausa Pós Reforço- n2',
      'Pausa Pós Reforço- s2',
      'Resultado']);
    for i := Low(FTrials) to High(FTrials) do begin
      case FTrials[i].Result of
        NONE : Result := 'TESTE';
        MISS : Result := '0';
        HIT  : Result := '1';
      end;
      FTrials[i].Delays := GetDelays(FTrials[i]);
      DelaysCount := Length(FTrials[i].Delays);
      if DelaysCount > 0 then begin
        DelaysTime := Sum(FTrials[i].Delays);
      end else begin
        DelaysTime := 0;
      end;

      if FTrials[i].Responses.Count > 0 then begin
        Rate := FTrials[i].Responses.Count/FTrials[i].ResponsesTime;
        RunningRate :=
          FTrials[i].Responses.Count/(FTrials[i].ResponsesTime-DelaysTime);
      end else begin
        Rate := 0;
        RunningRate := 0;
      end;
      FirstQuartil := GetFirstQuartil(FTrials[i]);
      QuarterLife := FirstQuartil/(FTrials[i].ResponsesTime-DelaysTime);
      Table.WriteRow([
        FTrials[i].Tag.ToString,
        FTrials[i].Latency.ToString,
        FTrials[i].Responses.Count.ToString,
        FTrials[i].ResponsesTime.ToString,
        Rate.ToString,
        RunningRate.ToString,
        FirstQuartil.ToString,
        QuarterLife.ToString,
        DelaysCount.ToString,
        DelaysTime.ToString,
        Result
      ]);
      if FTrials[i].IsTest then begin
        { do nothing }
      end else begin
        Inc(TrialsWithReinforcement);
        SetLength(Quarters, Length(Quarters)+1);
        Quarters[High(Quarters)] := QuarterLife;
      end;
    end;
    QuarterLife := Mean(Quarters);
    Table.WriteRow(['']);
    Table.WriteRow([
      'Tentativas com reforço',
      'Média de Q1/(s-s2)']);
    Table.WriteRow([
      TrialsWithReinforcement.ToString,
      QuarterLife.ToString]);

    for i := Low(FTrials) to High(FTrials) do begin
      if FTrials[i].IsTest then begin
        Table.WriteRow(['']);
        Table.WriteRow(['Tentativa de Teste '+FTrials[i].Tag.ToString]);
        Table.WriteRow(['Tempo', 'Frequência Acumulada']);
        for j := 0 to FTrials[i].Responses.Count-1 do begin
          Table.WriteRow([FTrials[i].Responses[j].ToString, (j+1).ToString]);
        end;
      end else begin
        { do nothing }
      end;
    end;
  finally
    Table.Free;
  end;
end;

end.


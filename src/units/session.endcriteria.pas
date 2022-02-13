unit Session.EndCriteria;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  , Session.Configuration
  , CounterManager
  ;

type

  { TEndCriteria }

  TEndCriteria = class(TComponent)
  private
    FBloc : TCfgBlc;
    //FTrial : TCfgTrial;
    procedure EndBlocOnEndTrial;
    procedure EndSessionOnEndBloc;
    function HitPorcentageInBloc : real;
  public
    constructor Create(AOwner : TComponent); override;
    procedure Invalidate;
    function OfSession : Boolean;
    function OfBloc : Boolean;
  end;

var
  EndCriteria : TEndCriteria;

implementation

uses
  Session.Configuration.GlobalContainer,
  Session.ConfigurationFile;

{ TEndCriteria }

constructor TEndCriteria.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TEndCriteria.Invalidate;
begin
  FBloc := ConfigurationFile.CurrentBloc;
  //LCurrentTrial := Counters.CurrentTrial;
end;

function TEndCriteria.OfSession: Boolean;
begin
  EndSessionOnEndBloc;
  Result := Counters.CurrentBlc >= ConfigurationFile.BlocCount;
end;

function TEndCriteria.OfBloc: Boolean;
begin
  EndBlocOnEndTrial;
  Result := Counters.CurrentTrial >= FBloc.TotalTrials;
end;

procedure TEndCriteria.EndBlocOnEndTrial;
  procedure EndBloc;
  begin
    Counters.CurrentTrial := FBloc.TotalTrials;
  end;

begin
  if FBloc.CrtConsecutiveHit > 0 then begin
    if Counters.BlcCscHits >= FBloc.CrtConsecutiveHit then begin
      EndBloc;
      Exit;
    end;
  end;

  if FBloc.CrtMaxTrials > 0 then begin
    if Counters.BlcTrials >= FBloc.CrtMaxTrials then begin
      EndBloc;
      Exit;
    end;
  end;
end;

procedure TEndCriteria.EndSessionOnEndBloc;
  procedure EndSession;
  begin
    if FBloc.MaxBlcRepetition > 0 then begin
      if (Counters.BlcRepetitions < FBloc.MaxBlcRepetition) then begin
        Counters.OnRepeatBlc(Self);
        Exit;
      end;
    end;

    if FBloc.AutoEndSession then begin
      { End session }
    end else begin
      Exit;
    end;

    Counters.CurrentBlc := ConfigurationFile.BlocCount;
  end;
  procedure NextBlocOnCriteria;
  begin
    if FBloc.NextBlocOnCriteria > 0 then begin
      Counters.CurrentBlc := FBloc.NextBlocOnCriteria-1;
    end;
  end;
begin
  if (FBloc.CrtHitValue > 0) then begin
    if (Counters.BlcHits < FBloc.CrtHitValue) then begin
      EndSession;
    end else begin
      NextBlocOnCriteria;
    end;
  end;

  if (FBloc.CrtHitPorcentage > 0) and
     (FBloc.CrtHitPorcentage <= 100) then begin
    if (HitPorcentageInBloc < FBloc.CrtHitPorcentage) then begin
      EndSession;
    end else begin
      NextBlocOnCriteria;
    end;
  end;
end;


function TEndCriteria.HitPorcentageInBloc: real;
begin
  Result := (Counters.BlcHits * 100)/FBloc.TotalTrials;
end;

end.


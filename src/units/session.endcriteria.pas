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
    function OfBlocOnEndTrial : Boolean;
    function OfSessionOnEndBloc : Boolean;
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
  Result := OfSessionOnEndBloc;
  Result := Result or (Counters.CurrentBlc >= ConfigurationFile.BlocCount);
end;

function TEndCriteria.OfBloc: Boolean;
begin
  Result := OfBlocOnEndTrial;
  Result := Result or (Counters.CurrentTrial >= FBloc.TotalTrials);
end;

function TEndCriteria.OfBlocOnEndTrial: Boolean;
begin
  Result := False;
  if FBloc.CrtConsecutiveHit > 0 then begin
    Result := Counters.BlcCscHits >= FBloc.CrtConsecutiveHit;
    Exit;
  end;

  if FBloc.CrtMaxTrials > 0 then begin
    Result := Counters.BlcTrials >= FBloc.CrtMaxTrials;
    Exit;
  end;
end;

function TEndCriteria.OfSessionOnEndBloc: Boolean;
begin
  Result := False;
  if (FBloc.CrtHitValue > 0) and
     (Counters.BlcHits < FBloc.CrtHitValue) then begin
    if FBloc.MaxBlcRepetition > 0 then begin
      if (Counters.BlcRepetitions < FBloc.MaxBlcRepetition) then begin
        Counters.OnRepeatBlc(Self);
        Exit;
      end else begin
        Counters.CurrentBlc := ConfigurationFile.BlocCount;
      end;
    end else begin
      Counters.OnRepeatBlc(Self);
      Exit;
    end;
  end;

  if (FBloc.CrtHitPorcentage > 0) and
     (FBloc.CrtHitPorcentage <= 100) and
     (HitPorcentageInBloc >= FBloc.CrtHitPorcentage) then begin
      if FBloc.NextBlocOnCriteria > 0 then begin
        Counters.CurrentBlc := FBloc.NextBlocOnCriteria-1;
      end else begin
        Counters.CurrentBlc := ConfigurationFile.BlocCount;
      end;
    end;
  end;

function TEndCriteria.HitPorcentageInBloc: real;
begin
  Result := (Counters.BlcHits * 100)/FBloc.TotalTrials;
end;

end.


//
// Validation Project (PCRF) - Stimulus Control App
// Copyright (C) 2014-2016,  Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.
//
// cpicanco@ufpa.br
//
// This file is part of Validation Project (PCRF).
//
// Validation Project (PCRF) is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Validation Project (PCRF) is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Validation Project (PCRF).  If not, see <http://www.gnu.org/licenses/>.
//
unit counter;

{$mode objfpc}{$H+}

interface

type
  TCounter = class (TObject)
    private
      FCounter : integer;
    public
      procedure Plus (Increment : integer);
      procedure Minus (Decrement : integer);
      procedure Mult (Multiplier : integer);
      procedure Divide (Dividend : integer);
      procedure Clear;
      property Counter : integer read FCounter write FCounter;
  end;

implementation

{ TCounter }

procedure TCounter.Clear;
begin
  FCounter := 0;
end;

procedure TCounter.Divide(Dividend: integer);
begin
  if FCounter <> 0 then FCounter := FCounter div Dividend;
end;

procedure TCounter.Minus(Decrement: integer);
begin
  Dec(FCounter, Decrement);
end;

procedure TCounter.Plus(Increment: integer);
begin
  Inc(FCounter, Increment);
end;

procedure TCounter.Mult(Multiplier: integer);
begin
  FCounter := FCounter * Multiplier;
end;

end.


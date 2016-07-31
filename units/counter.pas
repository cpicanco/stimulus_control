{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
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


{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Session.CastleUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ExtCtrls;

type

  { TSessionUpdater }

  TSessionUpdater = class(TObject)
  private
    FTimer : TTimer;
    procedure UpdateForever(Sender : TObject);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  end;

var
  SessionUpdater : TSessionUpdater;

implementation

uses CastleApplicationProperties;

{ TSessionUpdater }

procedure TSessionUpdater.UpdateForever(Sender: TObject);
begin
  ApplicationProperties._Update;
end;

constructor TSessionUpdater.Create;
begin
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 1;
  FTimer.OnTimer := @UpdateForever;
end;

destructor TSessionUpdater.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

procedure TSessionUpdater.Start;
begin
  FTimer.Enabled := True;
end;

procedure TSessionUpdater.Stop;
begin
  FTimer.Enabled := False;
end;

initialization
  SessionUpdater := TSessionUpdater.Create;

finalization
  SessionUpdater.Free;

end.


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
    FStarted : Extended;
    FSeconds : Extended;
    procedure UpdateForever(Sender : TObject);
    procedure UpdateForSomeTime(Sender : TObject);
  public
    constructor Create(AUpdateForever : Boolean); reintroduce;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property Seconds : Extended read FSeconds write FSeconds;
  end;

var
  SessionUpdater : TSessionUpdater;

implementation

uses CastleApplicationProperties, Forms, Timestamps.Helpers;

{ TSessionUpdater }

procedure TSessionUpdater.UpdateForever(Sender: TObject);
begin
  ApplicationProperties._Update;
end;

procedure TSessionUpdater.UpdateForSomeTime(Sender: TObject);
begin
  ApplicationProperties._Update;
  if (GetCustomTick - FStarted) > Seconds then begin
    FTimer.Enabled := False;
  end;
end;

constructor TSessionUpdater.Create(AUpdateForever : Boolean);
begin
  FTimer := TTimer.Create(nil);
  FSeconds := 1;
  FTimer.Enabled := False;
  FTimer.Interval := 1;
  if AUpdateForever then begin
    FTimer.OnTimer := @UpdateForever;
  end else begin
    FTimer.OnTimer := @UpdateForSomeTime;
  end;
end;

destructor TSessionUpdater.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

procedure TSessionUpdater.Start;
begin
  FTimer.Enabled := True;
  FStarted := GetCustomTick;
end;

procedure TSessionUpdater.Stop;
begin
  FTimer.Enabled := False;
end;

initialization
  SessionUpdater := TSessionUpdater.Create(True);

finalization
  SessionUpdater.Free;

end.


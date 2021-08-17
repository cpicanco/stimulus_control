{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Cheats;

{$mode ObjFPC}{$H+}

interface

uses Classes, Controls, ExtCtrls;

type

  { TParticipantBot }

  TParticipantBot = class(TComponent)
  private
    FData  : PtrInt;
    FTimer : TTimer;
    FTargetControl : TControl;
    procedure Click(Sender : TObject);
    procedure Async(Data : PtrInt);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure SetTargetControl(AControl : TControl);
    procedure Start;
    procedure Stop;
  end;

var
  ParticipantBot : TParticipantBot;
  CheatsModeOn : Boolean;

resourcestring
  RSErrorUnknownTarget = 'TParticipantBot.Async: target not assigned';

implementation

uses Forms, SysUtils;

{ TBot }

procedure TParticipantBot.Click(Sender : TObject);
begin
  Application.QueueAsyncCall(@Async, FData);
end;

procedure TParticipantBot.Async(Data : PtrInt);
var
  LX : integer;
  LY : integer;
begin
  if Assigned(FTargetControl) then begin
    LX := FTargetControl.BoundsRect.CenterPoint.X;
    LY := FTargetControl.BoundsRect.CenterPoint.Y;
    FTargetControl.OnClick(Self);
  end else begin
    raise Exception.Create(RSErrorUnknownTarget) at
        get_caller_addr(get_frame),
        get_caller_frame(get_frame);
  end;
end;

constructor TParticipantBot.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(AOwner);
  FTimer.Enabled := False;
  FTimer.Interval := Random(101) + 500;
  FTimer.OnTimer := @Click;
end;

destructor TParticipantBot.Destroy;
begin
  inherited Destroy;
end;

procedure TParticipantBot.SetTargetControl(AControl : TControl);
begin
  FTargetControl := AControl;
end;

procedure TParticipantBot.Start;
begin
  FTimer.Enabled := True;
end;

procedure TParticipantBot.Stop;
begin
  FTimer.Enabled := False;
end;

initialization
  ParticipantBot := TParticipantBot.Create(nil);

finalization
  ParticipantBot.Free;

end.


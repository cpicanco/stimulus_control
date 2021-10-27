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

uses Classes, SysUtils, Controls, ExtCtrls, Stimuli;

type

  { TParticipantBot }

  TParticipantBot = class(TComponent)
  private
    FBias: TBytes;
    FData  : PtrInt;
    FFakeExpectedResponseTimer : TTimer;
    FTargetStimulus : IStimuli;
    procedure Click(Sender : TObject);
    procedure Async(AData : PtrInt);
    procedure SetBias(AValue: TBytes);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Start(AStimulus : IStimuli);
    procedure Stop;
    property Bias : TBytes read FBias write SetBias;
  end;

var
  ParticipantBot : TParticipantBot;
  CheatsModeOn : Boolean;

resourcestring
  RSErrorUnknownTarget = 'TParticipantBot.Async: target not assigned';

{ Two Choice Porcentage Bias
r := Random;
if r < (TrackBarRandomBias.Position/100) then
  repeat
    ri := Random(10);
  until not Odd(ri)
else
  repeat
    ri := Random(10);
  until Odd(ri);
choice := choices[ri];
}

implementation

uses Forms;

{ TBot }

procedure TParticipantBot.Click(Sender : TObject);
begin
  if FFakeExpectedResponseTimer.Enabled then
    Application.QueueAsyncCall(@Async, FData);
end;

procedure TParticipantBot.Async(AData : PtrInt);
begin

    FTargetStimulus.DoExpectedResponse;
end;

procedure TParticipantBot.SetBias(AValue: TBytes);
begin
  if FBias=AValue then Exit;
  FBias:=AValue;
end;

constructor TParticipantBot.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FData := 0;
  FFakeExpectedResponseTimer := TTimer.Create(AOwner);
  FFakeExpectedResponseTimer.Enabled := False;
  FFakeExpectedResponseTimer.Interval := 100;
  FFakeExpectedResponseTimer.OnTimer := @Click;
end;

destructor TParticipantBot.Destroy;
begin
  inherited Destroy;
end;

procedure TParticipantBot.Start(AStimulus : IStimuli);
begin
  FTargetStimulus := AStimulus;
  FFakeExpectedResponseTimer.Enabled := True;
end;

procedure TParticipantBot.Stop;
begin
  FData := 0;
  FFakeExpectedResponseTimer.Enabled := False;
end;

initialization
  ParticipantBot := TParticipantBot.Create(Application);

end.


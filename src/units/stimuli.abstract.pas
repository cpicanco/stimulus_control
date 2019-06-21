unit Stimuli.Abstract;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type

  { TStimulus }

  TStimulus = class(TComponent)
  private
    FOnStop: TNotifyEvent;
    procedure SetOnStop(AValue: TNotifyEvent);
  public
    constructor Create(AOwner : TComponent); override;
    procedure LoadFromFile(AFilename : string); virtual; abstract;
    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;
    property OnStop : TNotifyEvent read FOnStop write SetOnStop;
  end;

implementation

{ TStimulus }

procedure TStimulus.SetOnStop(AValue: TNotifyEvent);
begin
  if FOnStop=AValue then Exit;
  FOnStop:=AValue;
end;

constructor TStimulus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

end.


unit Forms.CheckStimuli;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type

  { TFormCheckStimuli }

  TFormCheckStimuli = class(TForm)
  private

  public
    procedure ShowStimuli(AStimuli : array of string);

  end;

var
  FormCheckStimuli: TFormCheckStimuli;

implementation

{$R *.lfm}

uses Controls.Stimuli.Key;

{ TFormCheckStimuli }

procedure TFormCheckStimuli.ShowStimuli(AStimuli: array of string);
var
  i : integer;
begin
  while ComponentCount > 0 do
    if Components[0] is TKey then Components[0].Free;
  for i := Low(AStimuli) to High(AStimuli) do
    with TKey.Create(Self) do
    begin
      Parent := Self;
      Filename := AStimuli[i];
      Width := 100;
      Height := 100;
      Top := 20+(i * (Height+5));
      Left:= Width;
      Show;
    end;
end;

end.


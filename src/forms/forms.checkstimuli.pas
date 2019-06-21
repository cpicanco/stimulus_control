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
    procedure DeleteStimuli;
    procedure ShowStimuli(AStimuli : array of string; ACol : integer);

  end;

var
  FormCheckStimuli: TFormCheckStimuli;

implementation

{$R *.lfm}

uses Controls.Stimuli.Key;

{ TFormCheckStimuli }

procedure TFormCheckStimuli.DeleteStimuli;
begin
  while ComponentCount > 0 do
    if Components[0] is TKey then Components[0].Free;
end;

procedure TFormCheckStimuli.ShowStimuli(AStimuli: array of string; ACol : integer);
var
  i : integer;
begin
  for i := Low(AStimuli) to High(AStimuli) do
    with TKey.Create(Self) do
    begin
      Parent := Self;
      Filename := AStimuli[i];
      Width := 100;
      Height := 100;
      Top := 20+(i * (Height+5));
      Left:= Width + ACol*120;
      Show;
    end;
end;

end.


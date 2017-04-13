unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LvlGraphCtrl, Forms, Controls, Graphics, Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    LvlGraphControl1: TLvlGraphControl;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  LvlGraphControl1.Graph.LevelCount := 1;
  LvlGraphControl1.Graph.CreateHiddenNode.Caption:='A';
  LvlGraphControl1.Graph.CreateHiddenNode.Caption:='B';
  LvlGraphControl1.Graph.CreateHiddenNode(1).Caption:='C';
  LvlGraphControl1.Graph.CreateHiddenNode(1).Caption:='D';
  for i := 0 to LvlGraphControl1.Graph.NodeCount-1 do
    begin
      LvlGraphControl1.Graph.Nodes[i].Visible:=True;
    end;
  //LvlGraphControl1.Graph.CreateTopologicalLevels(True);
  LvlGraphControl1.InvalidateAutoLayout;
end;

end.


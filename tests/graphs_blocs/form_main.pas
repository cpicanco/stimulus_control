unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LvlGraphCtrl, Forms, Controls, Graphics, Dialogs,
  Menus;

type

  { TForm1 }

  TForm1 = class(TForm)
    LvlGraphControl1: TLvlGraphControl;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
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
  j: Integer;
begin
  with LvlGraphControl1.Graph do
   begin
     GetEdge('A','C',True);
     GetEdge('A','B',True);
     GetEdge('H','A',True);
     GetEdge('B','D',True);
     GetEdge('C','F',True);
     GetEdge('C','G',True);
     GetEdge('D','H',True);


     for i := 0 to NodeCount-1 do
       begin
         WriteLn(Nodes[i].Caption);
         for j := 0 to Nodes[i].OutEdgeCount-1 do
           begin
             WriteLn(Nodes[i].OutEdges[j].AsString);
             WriteLn(Nodes[i].OutEdges[j].Target.Caption);
           end;
       end;

  //LvlGraphControl1.Graph.LevelCount := 1;
  //LvlGraphControl1.Graph.CreateHiddenNode.Caption:='A';
  //LvlGraphControl1.Graph.CreateHiddenNode.Caption:='B';
  //LvlGraphControl1.Graph.CreateHiddenNode(1).Caption:='C';
  //LvlGraphControl1.Graph.CreateHiddenNode(1).Caption:='D';
  //for i := 0 to LvlGraphControl1.Graph.NodeCount-1 do
  //  begin
  //    LvlGraphControl1.Graph.Nodes[i].Visible:=True;
  //  end;
  ////LvlGraphControl1.Graph.CreateTopologicalLevels(True);
  //LvlGraphControl1.InvalidateAutoLayout;
  //
   end;
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin

end;

end.


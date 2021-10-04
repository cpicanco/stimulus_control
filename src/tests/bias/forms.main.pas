unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, IniPropStorage;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1 : TButton;
    Button2 : TButton;
    IniPropStorage1 : TIniPropStorage;
    procedure Button1Click(Sender : TObject);
    procedure Button2Click(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure IniPropStorage1RestoreProperties(Sender : TObject);
    procedure IniPropStorage1SavingProperties(Sender : TObject);
  private

  public

  end;

var
  Form1 : TForm1;

implementation

{$R *.lfm}

uses Controls.Bias;

var TrackBias1 : TTrackBias;
{ TForm1 }

procedure TForm1.FormCreate(Sender : TObject);
begin
  TrackBias1 := TTrackBias.Create(Self);
  TrackBias1.Name := 'TrackBias1';
  TrackBias1.Parent := Self;
end;

procedure TForm1.IniPropStorage1RestoreProperties(Sender : TObject);
begin
  TrackBias1.Load;
end;

procedure TForm1.IniPropStorage1SavingProperties(Sender : TObject);
begin
  TrackBias1.Save;
end;

procedure TForm1.Button1Click(Sender : TObject);
begin
  TrackBias1.AddSpot;
end;

procedure TForm1.Button2Click(Sender : TObject);
begin
  TrackBias1.RemoveSpot;
end;

end.


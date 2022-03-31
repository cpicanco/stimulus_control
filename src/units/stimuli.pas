unit Stimuli;

{$mode ObjFPC}{$H+}

{$INTERFACES CORBA}

interface

uses Classes;

type

  IStimuli = interface
  ['{6B18F44A-7450-4871-A2BB-A109FC2ED005}']
    function AsInterface : IStimuli;
    procedure DoExpectedResponse;
    procedure LoadFromParameters(AParameters : TStringList);
    procedure Start;
    procedure Stop;
  end;

  IDragDropable = interface
  ['{11E4209B-68FD-4866-80C6-CBF74E22F41C}']
    procedure DragDrop(Sender, Source: TObject; X, Y: Integer);
  end;

implementation

end.


unit Stimuli;

{$mode ObjFPC}{$H+}

{$INTERFACES CORBA}

interface

type

  IStimuli = interface
  ['{6B18F44A-7450-4871-A2BB-A109FC2ED005}']
    procedure Start;
    procedure Stop;
    procedure LoadFromParameters;
    procedure DoExpectedResponse;
  end;

implementation

end.


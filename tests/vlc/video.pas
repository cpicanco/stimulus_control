unit Video;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

type

  { IVideoPlayer }

  IVideoPlayer = interface
  ['{C0978598-E70C-4E1B-A430-FD94DDE49594}']
    //function Exist : Boolean;
    procedure LoadFromFile(AFilename: string);
    procedure Play;
    procedure Stop;
    //procedure FullScreen(AValue : Boolean);
  end;


implementation


end.


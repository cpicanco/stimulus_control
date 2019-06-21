unit Stimuli.Image;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Stimuli.Abstract;

type

  { TStimulusFigure }

  TStimulusFigure = class(TStimulus)
  private
    FBackground : TControl;
    FBitmap : TBitmap;
    FLeft: integer;
    FTop: integer;
    function GetWidth: integer;
    function GetHeight: integer;
    procedure SetHeight(AValue: integer);
    procedure SetWidth(AValue: integer);
    procedure SetLeft(AValue: integer);
    procedure SetTop(AValue: integer);
  public
    constructor Create(AOwner : TControl); override; reintroduce;
    destructor Destroy; override;
    procedure LoadFromFile(AFilename: string); override;
    property Width : integer read GetWidth write SetWidth;
    property Height : integer read GetWidth write SetHeight;
    property Top : integer read FTop write SetTop;
    property Left: integer read FLeft write SetLeft;
  end;

implementation

{ TStimulusFigure }

procedure TStimulusFigure.SetHeight(AValue: integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
end;

function TStimulusFigure.GetWidth: integer;
begin

end;

function TStimulusFigure.GetHeight: integer;
begin

end;

procedure TStimulusFigure.SetLeft(AValue: integer);
begin
  if FLeft=AValue then Exit;
  FLeft:=AValue;
  FBackground.Invalidate;
end;

procedure TStimulusFigure.SetTop(AValue: integer);
begin
  if FTop=AValue then Exit;
  FTop:=AValue;
  FBackground.Invalidate;
end;

procedure TStimulusFigure.SetWidth(AValue: integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
  FBackground.Invalidate;
end;

constructor TStimulusFigure.Create(AOwner: TControl);
begin
  inherited Create(AOwner);
  FBitmap.Create;
  FBackground := AOwner;
  FBackground.onp;
end;

destructor TStimulusFigure.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

procedure TStimulusFigure.LoadFromFile(AFilename: string);
begin
  FBitmap.LoadFromFile(AFilename);

end;

end.


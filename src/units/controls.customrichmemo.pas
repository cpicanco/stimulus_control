unit Controls.CustomRichMemo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, RichMemo, LCLType, LMessages;

type

  { TARichMemo }

  TARichMemo = class(TRichMemo)
  protected
    procedure EraseBackground(DC: HDC); override;
    procedure WndProc(var Message: TLMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses Controls, Graphics, LCLIntf, StdCtrls;

{ TARichMemo }

procedure TARichMemo.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);
end;

procedure TARichMemo.WndProc(var Message: TLMessage);
  procedure ColorBackground;
  begin
    SetTextColor(Message.WParam, ColorToRGB(Font.Color));
    SetBkColor(Message.WParam, ColorToRGB(Brush.Color));
    Message.Result := LRESULT(Brush.Handle);
  end;
begin
  case Message.Msg of
    CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC:
      ColorBackground;

    CM_ENABLEDCHANGED:
      ColorBackground;
  else;
    inherited WndProc(Message);
  end;
end;

constructor TARichMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWhite;
  Brush.Color := clWhite;
  //Enabled := False;
  BorderStyle := bsNone;
  ScrollBars := ssAutoVertical;
  ReadOnly := True;
end;

end.


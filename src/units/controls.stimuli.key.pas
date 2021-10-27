{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Stimuli.Key;  // media_key may be a better name

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, SysUtils, Variants, Classes,
     Graphics, Controls, Forms, ExtCtrls, LazFileUtils

    , Dialogs
    , Schedules
     {$IFDEF AUDIO}, Audio.Bass_nonfree {$ENDIF}
     {$IFDEF VIDEO}, Video.VLC {$ENDIF}
    ;

type
  TImage = (stmNone, stmPicture, stmAnimation, stmVideo, stmText);

  TKind = record
    stmAudio : boolean;
    stmImage : TImage;
  end;

  { TKey }

  TKey = class(TGraphicControl)
  private
    {$IFDEF AUDIO}FAudioPlayer : TBassStream;{$ENDIF}
    {$IFDEF VIDEO}FVideoPlayer : TVLCVideoPlayer;{$ENDIF}
    FSchedule: TSchedule;
    FStimulus: TBitmap;
    //FGifImage: TJvGIFAnimator;
    FEdge: TColor;
    FFileName: string;
    FKind: TKind;
    FEditMode: Boolean;
    FResponseCount: Integer;
    FLoopNumber: Integer;
    FLastResponseLog : string;
    procedure SetFileName(AFilename: string);
  private
    FOnConsequence: TNotifyEvent;
    FOnResponse: TNotifyEvent;
    FOnEndMedia: TNotifyEvent;
    FPenWidth: integer;
    procedure AutoDestroy(Sender: TObject);
    procedure Consequence(Sender: TObject);
    function GetShortName: string;
    procedure Response(Sender: TObject);
    procedure KeyMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetPenWidth(AValue: integer);
    procedure VideoResponse(Sender : TObject);
    procedure VideoClick(Sender : TObject);
  protected
    procedure SetVisible(AValue:Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BoundsAsString : string;
    procedure Paint; override;
    procedure Play;
    procedure Stop;
    procedure FullScreen;
    procedure Centralize(AControl : TControl = nil);
    procedure CentralizeLeft;
    procedure CentralizeRight;
    procedure AutoDestroyIn(AInterval : integer);
    procedure SetOriginalSize;
    property Schedule : TSchedule read FSchedule;
    property Edge : TColor read FEdge write FEdge;
    property EditMode: Boolean read FEditMode write FEditMode;
    property Filename : string read FFileName write SetFileName;
    property Loops : Integer read FLoopNumber write FLoopNumber;
    property Kind: TKind read FKind;
    property LastResponseLog : string read FLastResponseLog;
    property ResponseCount : integer read FResponseCount;
    property ShortName : string read GetShortName;
    property PenWidth : integer read FPenWidth write SetPenWidth;
  public
    property OnConsequence: TNotifyEvent read FOnConsequence write FOnConsequence;
    property OnResponse: TNotifyEvent read FOnResponse write FOnResponse;
    property OnEndMedia: TNotifyEvent read FOnEndMedia write FOnEndMedia;
  public
    property Color;
    property Caption;
    property Font;
  end;

implementation

uses FileUtil
   , Session.Configuration.GlobalContainer
   ;

constructor TKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Visible := False;
  Height:= 45;
  Width:= 45;
  EditMode := False;
  Edge:= clInactiveCaption;
  OnMouseDown := @KeyMouseDown;
  Loops := -1;

  FSchedule := TSchedule.Create(Self);
  with FSchedule do
    begin
      OnConsequence:= @Consequence;
      OnResponse:= @Response;
    end;
end;

destructor TKey.Destroy;
begin
  {$IFDEF VIDEO}if Assigned(FVideoPlayer) then FVideoPlayer.Stop;{$ENDIF}
  {$IFDEF AUDIO}if Assigned(FAudioPlayer) then FAudioPlayer.Free;{$ENDIF}
  if Assigned(FStimulus) then FStimulus.Free;
  //if Assigned(FGifImage) then FreeAndNil (FGifImage);

  inherited Destroy;
end;

function TKey.BoundsAsString: string;
begin
  Result :=
   IntToStr(Left) + #32 +
   IntToStr(Top) + #32 +
   IntToStr(Width) + #32 +
   IntToStr(Height);
end;

procedure TKey.FullScreen;
begin
  if FKind.stmImage  = stmPicture then
    begin
      Width := ClientWidth;
      Height := ClientHeight;
      FStimulus.SetSize(Width,Height)
    end;
  {$IFDEF VIDEO}
  if FKind.stmImage = stmVideo then
    FVideoPlayer.FullScreen;
  {$ENDIF}
  Invalidate;
end;

procedure TKey.Centralize(AControl: TControl);
var
  LMonitor : Byte;
  LWidth,LHeight:integer;
begin
  if Assigned(AControl) then
    begin
      LWidth :=  AControl.Width;
      LHeight := AControl.Height;
    end
  else
    begin
      LMonitor := GlobalContainer.MonitorToShow;
      LWidth :=  Screen.Monitors[LMonitor].Width;
      LHeight := Screen.Monitors[LMonitor].Height;
    end;
  Left := (LWidth  div 2) - (Width  div 2);
  Top  := (LHeight div 2) - (Height div 2);
end;

procedure TKey.CentralizeLeft;
var
  LOwner : TCustomControl;
begin
  LOwner := TCustomControl(Parent);
  Left := (LOwner.Width div 4) - (Width div 2);
  Top := (LOwner.Height div 2) - (Height div 2);
end;

procedure TKey.CentralizeRight;
var
  LOwner : TCustomControl;
begin
  LOwner := TCustomControl(Parent);
  Left := LOwner.Width - (LOwner.Width div 4) - (Width div 2);
  Top := LOwner.Height - (LOwner.Height div 2) - (Height div 2);
end;


procedure TKey.AutoDestroyIn(AInterval: integer);
begin
  FSchedule.Load('FT '+IntToStr(AInterval));
  FSchedule.OnConsequence:=@AutoDestroy;
  FSchedule.Start;
end;

procedure TKey.SetOriginalSize;
begin
  case FKind.stmImage of
    stmPicture:
      begin
        Width := FStimulus.Width;
        Height := FStimulus.Height;
      end;
    //stmAnimation:PaintGIF;
    stmNone: { do nothing };
    stmVideo: { do nothing };
  end;
end;

procedure TKey.Paint;
  procedure PaintKey(Color : TColor);
  begin
    with Canvas do
      begin
        Font.Size:= 48;
        Font.Color:= clWhite xor Color;
        Pen.Width := FPenWidth;
        Pen.Color := Edge;
        Brush.Color:= Color;//FBorderColor;
        //FillRect(Rect(0, 0, Width, Height));
        Rectangle(ClientRect);
        TextRect(ClientRect,
                 (((ClientRect.Right-ClientRect.Left) div 2) - (TextWidth(Caption)div 2)),
                 (((ClientRect.Bottom-ClientRect.Top) div 2) - (TextHeight(Caption)div 2)),
                 Caption);
      end;
  end;
begin
  case FKind.stmImage of
    stmPicture:Canvas.StretchDraw(ClientRect, FStimulus);
    //stmAnimation:PaintGIF;
    stmNone:PaintKey(Color);
    stmVideo: { do nothing };
  end;
end;

procedure TKey.Play;
begin
  {$IFDEF AUDIO}
  if FKind.stmAudio then
    if Assigned(FAudioPlayer) then
      FAudioPlayer.Play;
  {$ENDIF}

  {$IFDEF VIDEO}
  case FKind.stmImage of
    //stmAnimation : ;
    //stmNone : ;
    //stmPicture : ;
    stmVideo: if Assigned(FVideoPlayer) then FVideoPlayer.Play;
  end;
  {$ENDIF}
end;

procedure TKey.Stop;
begin
  {$IFDEF AUDIO}
  if FKind.stmAudio then
    if Assigned(FAudioPlayer) then
      FAudioPlayer.Stop;
  {$ENDIF}

  {$IFDEF VIDEO}
  case FKind.stmImage of
    //stmAnimation : ;
    //stmNone : ;
    //stmPicture : ;
    stmVideo: if Assigned(FVideoPlayer) then FVideoPlayer.Stop;
  end;
  {$ENDIF}
end;

procedure TKey.SetFileName(AFilename: string);
var
  s1 : String;

  procedure CreateBitmap;
  begin
    if Assigned(FStimulus) then
      FStimulus.Free;
    FStimulus := TBitmap.Create;
    with FStimulus do
      begin
        Width := Self.Width;
        Height := Self.Height;
        Canvas.Brush.Color := Self.Color;
        Canvas.Rectangle(ClientRect);
      end;
  end;

  procedure SetKind(Audio : boolean; Image : TImage);
  begin
    FKind.stmAudio := Audio;
    FKind.stmImage := Image;
  end;

  procedure Load_PNG(AF:string; Audio: Boolean=False);
  var LPNG : TPortableNetworkGraphic;
  begin
    CreateBitmap;
    LPNG := TPortableNetworkGraphic.Create;
    LPNG.LoadFromFile(AF);
    Width := LPNG.Width;
    Height := LPNG.Height;
    FStimulus.Assign(LPNG);
    FStimulus.Transparent:=True;
    FStimulus.TransparentColor:=clFuchsia;
    LPNG.Free;
    SetKind(Audio, stmPicture);
  end;

  procedure Load_BMP(AF:string; Audio: Boolean=False);
  begin
    CreateBitmap;
    FStimulus.LoadFromFile(AF);
    SetKind(Audio, stmPicture);
  end;

  procedure Load_JPG(AF:string; Audio: Boolean=False);
  var LJPG : TJPEGImage;
  begin
    CreateBitmap;
    LJPG := TJPEGImage.Create;
    LJPG.LoadFromFile(AF);
    FStimulus.Assign(LJPG);
    LJPG.Free;
    SetKind(Audio, stmPicture);
  end;

begin
  if FFileName = AFilename then Exit;
  FFileName := '';
  if FileExists(AFilename) then
    begin
      s1:= UpperCase(ExtractFileExt(AFilename));
      case s1 of
        // images
        '.BMP' : Load_BMP(AFilename);
        '.JPG' : Load_JPG(AFilename);
        '.PNG' : Load_PNG(AFilename);
      end;

      FFileName := AFilename;
      Invalidate;
    end;
end;

procedure TKey.AutoDestroy(Sender: TObject);
begin
  Free;
end;

procedure TKey.KeyMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FLastResponseLog := IntToStr(X + Left) + #9 + IntToStr(Y + Top);
  Inc(FResponseCount);
  if FSchedule.Loaded then
     FSchedule.DoResponse;
end;

procedure TKey.SetPenWidth(AValue: integer);
begin
  if FPenWidth=AValue then Exit;
  FPenWidth:=AValue;
end;

procedure TKey.SetVisible(AValue: Boolean);
begin
  inherited SetVisible(AValue);

end;

function TKey.GetShortName: string;
begin
  if FFilename = '' then
    Result := 'NA'
  else
    Result := ExtractFileNameWithoutExt(ExtractFileNameOnly(FFilename));
end;

procedure TKey.VideoResponse(Sender: TObject);
begin
  if Assigned(Self) then
  begin
    Inc(FResponseCount);
    FSchedule.DoResponse;
  end;
end;

//******
//**  **
//******
// TKey must send its info to OnClick events.
//******
//**  **
//******

procedure TKey.VideoClick(Sender: TObject);
begin
  if Assigned(OnClick) then OnClick(Self); //must be Self
end;


//******
//**  **
//******
// TKey must send its info to TTrial descendents.
//******
//**  **
//******

procedure TKey.Consequence(Sender: TObject);
begin
  if Assigned(OnConsequence) then FOnConsequence(Self);   //must be Self
end;

procedure TKey.Response(Sender: TObject);
begin
  if Assigned(OnResponse) then FOnResponse(Self);   //must be Self
end;


end.

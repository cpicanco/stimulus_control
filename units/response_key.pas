{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit response_key;  // media_key may be a better name

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, SysUtils, Variants, Classes,
     Graphics, Controls, Forms, ExtCtrls, LazFileUtils

    , Dialogs
    , bass_player
    , schedules_main
    , config_session_global_container
    ;

type
  TImage = (stmNone, stmPicture, stmAnimation, stmVideo);

  TKind = record
    stmAudio : boolean;
    stmImage : TImage;
  end;

  { TKey }

  TKey = class(TGraphicControl)
  private
    FAudioPlayer : TBassStream;
    FSchedule: TSchedule;
    FStimulus: TBitmap;
    //FGifImage: TJvGIFAnimator;
    //FMedia : TWindowsMediaPlayer;
    FEdge: TColor;
    FFileName: string;
    FKind: TKind;
    FEditMode: Boolean;
    FResponseCount: Integer;
    FLoopNumber: Integer;
    FLastResponseLog : string;
    procedure SetFileName(Path: string);
  private
    FOnConsequence: TNotifyEvent;
    FOnResponse: TNotifyEvent;
    FOnEndMedia: TNotifyEvent;
    procedure AutoDestroy(Sender: TObject);
    procedure Consequence(Sender: TObject);
    function GetShortName: string;
    procedure Response(Sender: TObject);
    procedure KeyMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Play;
    procedure Stop;
    procedure FullScreen;
    procedure Centralize(AControl : TControl = nil);
    procedure AutoDestroyIn(AInterval : integer);
    property Schedule : TSchedule read FSchedule;
    property Edge : TColor read FEdge write FEdge;
    property EditMode: Boolean read FEditMode write FEditMode;
    property FullPath: string read FFileName write SetFileName;
    property Loops : Integer read FLoopNumber write FLoopNumber;
    property Kind: TKind read FKind;
    property LastResponseLog : string read FLastResponseLog;
    property ResponseCount : integer read FResponseCount;
    property ShortName : string read GetShortName;
  public
    property OnConsequence: TNotifyEvent read FOnConsequence write FOnConsequence;
    property OnResponse: TNotifyEvent read FOnResponse write FOnResponse;
    property OnEndMedia: TNotifyEvent read FOnEndMedia write FOnEndMedia;
  public
    property Color;
    property Caption;
    property Font;
    property OnMouseDown;
  end;

implementation

uses FileUtil;

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
  {if Assigned(FMedia) then
    begin
      Stop;
      FreeAndNil(FMedia);
    end;}
  if Assigned(FAudioPlayer) then FAudioPlayer.Free;
  if Assigned(FStimulus) then FStimulus.Free;
  //if Assigned(FGifImage) then FreeAndNil (FGifImage);

  inherited Destroy;
end;

procedure TKey.FullScreen;
begin
  if FKind.stmImage  = stmPicture then
    begin
      Width := ClientWidth;
      Height := ClientHeight;
      FStimulus.SetSize(Width,Height)
    end;
  {if (FKind.stmImage = stmVideo) then
    begin
      repeat Application.ProcessMessages until FMedia.playState = 3;
      if (FMedia.playState = 3) then
      FMedia.fullScreen := True;
    end;  }
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
      if Assigned(GGlobalContainer) then
        LMonitor := GGlobalContainer.MonitorToShow
      else
        LMonitor := 0;
      LWidth :=  Screen.Monitors[LMonitor].Width;
      LHeight := Screen.Monitors[LMonitor].Height;
    end;
  Left := (LWidth  div 2) - (Width  div 2);
  Top  := (LHeight div 2) - (Height div 2);
end;

procedure TKey.AutoDestroyIn(AInterval: integer);
begin
  FSchedule.Kind := 'FT '+IntToStr(AInterval);
  FSchedule.OnConsequence:=@AutoDestroy;
  FSchedule.StartClock;
end;

procedure TKey.Paint;
    procedure PaintKey(Color : TColor);
    begin
      with Canvas do
        begin
          Font.Size:= 48;
          Font.Color:= clWhite;
          Pen.Width := 3;
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
    stmVideo:;
  end;
end;

procedure TKey.Play;
begin
  if FKind.stmAudio then
    FAudioPlayer.Play;
end;

procedure TKey.Stop;
begin
  if FKind.stmAudio then
    if Assigned(FAudioPlayer) then
      FAudioPlayer.Stop;
end;

procedure TKey.SetFileName(Path: string);                 //Review required
var
  s1 : String;

  LGuessedExtensions : array [0..5] of string = (
    '.BMP', '.JPG', '.PNG',
    '.bmp', '.jpg', '.png'
  );

  LExtension : string;

  procedure CreateBitmap;
  begin
    if Assigned(FStimulus) then
      FStimulus.Free;
    FStimulus := TBitmap.Create;
    FStimulus.Width:=Width;
    FStimulus.Height:=Height;
  end;

  procedure SetKind(Audio : boolean; Image : TImage);
  begin
    FKind.stmAudio := Audio;
    FKind.stmImage := Image;
  end;

  // Create_GIF;

    {FGifImage := TJvGIFAnimator.Create (Self);
    FGifImage.Parent := Self;
    FGifImage.Threaded := False;
    FGifImage.Visible := False;
    FGifImage.OnMouseDown := MouseDown;  //2
    FGifImage.Stretch := True;
    FGifImage.Align := alClient;
    FGifImage.Top := 0;
    FGifImage.Left := 0;
    FGifImage.Width := Width;
    FGifImage.Height := Height;
    FGifImage.Cursor := Self.Cursor;
    FGifImage.Animate := True; }



  // Create_VID;
    {
    FMedia := TWindowsMediaPlayer.Create(self);
    if OnlyAudio then FMedia.ParentWindow := Application.Handle
    else FMedia.Parent := Self;
    FMedia.Align:= alClient;
    FMedia.stretchToFit := True;
    //FMedia.enableContextMenu := False;
    //FMedia.windowlessVideo := True;
    //FMedia.ControlInterface.stretchToFit := True;
    //FMedia.DefaultInterface.stretchToFit := True;
    //FMedia.Top := 0;
    //FMedia.Left := 0;
    //FMedia.Width  := Width;
    //FMedia.Height := Height;
    FMedia.settings.autoStart := False;
    FMedia.settings.setMode('loop', false);
    FMedia.settings.setMode('autoRewind', false);
    FMedia.settings.invokeURLs := False;
    FMedia.ControlInterface.enableContextMenu := False;
    FMedia.ControlInterface.windowlessVideo := True;
    FMedia.Cursor := Self.Cursor;
    FMedia.OnMouseDown := MouseDown; //3
    FMedia.uiMode := 'none';
    }

  function Load_PNG(AFilename:string; Audio: Boolean=False) : boolean;
  var LPNG : TPortableNetworkGraphic;
  begin
    Result := False;
    try
      CreateBitmap;
      LPNG := TPortableNetworkGraphic.Create;
      LPNG.LoadFromFile(AFilename);
      FStimulus.Assign(LPNG);
      FStimulus.Transparent:=True;
      FStimulus.TransparentColor:=clFuchsia;
      LPNG.Free;
      SetKind(Audio, stmPicture);
    except
      on Exception do
        Exit;

    end;
    Result := True;
  end;

  function Load_BMP(AFilename:string; Audio: Boolean=False) : boolean;
  begin
    Result := False;
    try
      CreateBitmap;
      FStimulus.LoadFromFile(AFilename);
      SetKind(Audio, stmPicture);
    except
      on Exception do
        Exit;
    end;
    Result := True;
  end;

  function Load_JPG(AFilename:string; Audio: Boolean=False) : boolean;
  var LJPG : TJPEGImage;
  begin
    Result := False;
    try
      CreateBitmap;
      LJPG := TJPEGImage.Create;
      LJPG.LoadFromFile(AFilename);
      FStimulus.Assign(LJPG);
      LJPG.Free;
      SetKind(Audio, stmPicture);
    except
      on Exception do
        Exit;
    end;
    Result := True;
  end;

  //function Load_GIF(Audio: boolean) : boolean;
  //begin
  //  Result := False;
  //
  //
  //  try
  //    FGifImage.Image.LoadFromFile (s2);
  //  except
  //    on Exception do Exit;
  //  end;
  //
  //  SetKind (Audio, stmAnimation);
  //  Result := True;
  //
  //
  //end;

  function Load_AUD(AFilename : string): boolean;
  begin
    Result := False;
    if Loops > 0 then
      FAudioPlayer := TBassStream.Create(AFilename,Loops)
    else
      FAudioPlayer := TBassStream.Create(AFilename);
    SetKind(True, TImage.stmNone);
    Result := True;
  end;

  //function Load_VID (Audio : boolean) : boolean;
  //begin
  //  Result := False;
  //
  //  try
  //    if EditMode then FMedia.settings.playCount := 1
  //    else if FLoopNumber = 0 then FMedia.settings.playCount := MaxInt
  //    else if FLoopNumber > 0 then FMedia.settings.playCount := FLoopNumber
  //    else if FLoopNumber < 0 then FMedia.settings.playCount := Abs(FLoopNumber);
  //    FMedia.URL := s2;
  //  except
  //    on Exception do Exit;
  //
  //  end;
  //  FMPlayer.OnNotify:= MPlayerLoopNotify;
  //  SetKind (Audio, Image);
  //  Result := True;
  //end;
begin
  if FFileName = Path then Exit;
  FFileName := '';
  if FileExists(Path) then
    begin
      s1:= UpperCase(ExtractFileExt(Path));
      case s1 of
        // images
        '.BMP' : Load_BMP(Path);
        '.JPG' : Load_JPG(Path);
        '.PNG' : Load_PNG(Path);

        // animation
        // '.GIF': Load_GIF(Path);

        // video
        //'.MPG', '.AVI',
        //'.MOV', '.FLV',
        //'.WMV', '.MP4': Load_VID;
        '.WAV','.AIFF','.MP3','.OGG':
          if Load_AUD(Path) then
            begin
              // note that at this point we already loaded the audio file
              // the user can associate an image with each audio file sound
              // the user can place an image file with the same name as the audio file inside rootmedia
              // we load the image here
              s1:= Path;
              Delete(s1, pos(Copy(Path,Length(Path)- 3,4),s1), 4);

              for LExtension in LGuessedExtensions do
                if FileExists(s1 + LExtension) then
                  begin
                    case UpperCase(LExtension) of
                      // images
                      '.BMP' : Load_BMP(s1 + LExtension, True);
                      '.JPG' : Load_JPG(s1 + LExtension, True);
                      '.PNG' : Load_PNG(s1 + LExtension, True);
                    end;
                    Break;
                  end;
            end;
      end;

      FFileName := Path;
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
  Inc(FResponseCount);
  FLastResponseLog := IntToStr(X + Left) + #9 + IntToStr(Y + Top);
  FSchedule.DoResponse;
end;

//******
//**  **
//******
// Sender is irrelevant at this point, TKey needs to send its info to TTrial descendents.
//******
//**  **
//******
procedure TKey.Consequence(Sender: TObject);
begin
  if Assigned(OnConsequence) then FOnConsequence(Self);   //Necessariamente Self
end;

function TKey.GetShortName: string;
begin
  if FullPath = '' then
    Result := 'NA'
  else
    Result := ExtractFileNameWithoutExt(ExtractFileNameOnly(FullPath));
end;

procedure TKey.Response(Sender: TObject);
begin
  if Assigned(OnResponse) then FOnResponse(Self);   //Necessariamente  SELF
end;


end.

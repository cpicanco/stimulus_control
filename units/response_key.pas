{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit response_key;  // media_key may be a good name

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, SysUtils, Variants, Classes,
     Graphics, Controls, Forms, ExtCtrls, LazFileUtils

    , Dialogs
    , bass_player
    , schedules_main
    ;

type

  TImage = (stmNone, stmPicture, stmAnimation, stmVideo);

  TKind = record
    stmAudio : boolean;
    stmImage : TImage;
  end;

  { TKey }

  TKey = class(TCustomControl)
  private
    FAudioChannel : TBassChannel;
    FSchMan: TSchMan;

    FBitMap: TBitMap;
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
    procedure Consequence(Sender: TObject);
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
    property Schedule : TSchMan read FSchMan;
    property Edge : TColor read FEdge write FEdge;
    property EditMode: Boolean read FEditMode write FEditMode;
    property FullPath: string read FFileName write SetFileName;
    property Loops : Integer read FLoopNumber write FLoopNumber;
    property Kind: TKind read FKind;
    property LastResponseLog : string read FLastResponseLog;
    property ResponseCount : integer read FResponseCount;
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

constructor TKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Visible := False;
  Height:= 45;
  Width:= 45;
  EditMode := False;
  Color:= clRed;
  Edge:= clInactiveCaption;
  OnMouseDown := @KeyMouseDown;

  FSchMan := TSchMan.Create(self);
  with FSchMan do
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
  if Assigned(FAudioChannel) then FAudioChannel.Free;
  if Assigned(FBitMap) then FBitMap.Free;
  //if Assigned(FGifImage) then FreeAndNil (FGifImage);

  inherited Destroy;
end;

procedure TKey.FullScreen;
begin
  if FKind.stmImage  = stmPicture then
    begin
      Width := ClientWidth;
      Height := ClientHeight;
      FBitMap.SetSize(Width, Height);
    end;
  {if (FKind.stmImage = stmVideo) then
    begin
      repeat Application.ProcessMessages until FMedia.playState = 3;
      if (FMedia.playState = 3) then
      FMedia.fullScreen := True;
    end;  }
  Invalidate;
end;

procedure TKey.Paint;
    procedure PaintKey (Color : TColor);
    begin
      with Canvas do
        begin
          Font.Size:= 48;
          Font.Color:= clWhite;
          Pen.Width := 3;
          Pen.Color := Edge;
          Brush.Color:= Color;//FBorderColor;
          //FillRect(Rect(0, 0, Width, Height));
          Rectangle(0, 0, Width, Height);
          TextRect(Rect(3, 3, Width -3, Height -3),
                   ((Width div 2) - (TextWidth(Caption)div 2)),
                   ((Height div 2) - (TextHeight(Caption)div 2)),
                   Caption);
        end;
    end;

    procedure PaintFBitMap;
    begin
      with Canvas do
        begin
          //Draw(0,0, FBitMap);
          StretchDraw(Rect(0, 0, Width, Height), FBitMap);
        end;
    end;

begin
  case FKind.stmImage of
    stmPicture:PaintFBitMap;
    //stmAnimation:PaintGIF;
    stmNone:PaintKey(Color);
    stmVideo:;
  end;
end;

procedure TKey.Play;
begin
  if FKind.stmAudio then
    try
      FAudioChannel.Play;
    except
      on E:Exception do
        WriteLn('TKey Could not play audio.');
    end;
end;

procedure TKey.Stop;
begin
  {if Assigned(FMedia) then
  if FMedia.playState = wmppsPlaying then
    begin
      FMedia.controls.stop;
      FMedia.close();
    end;}
end;

procedure TKey.SetFileName(Path: string);                 //Review required
var
    s1, s2 : String;

  procedure RpSave;
  begin
    FFileName := Path;
  end;

  procedure SetKind (Audio : boolean; Image : TImage);
  begin
    FKind.stmAudio := Audio;
    FKind.stmImage := Image;
  end;

  procedure Create_IMG;
  begin
    FBitMap := TBitMap.Create;
    FBitMap.Width:= Width;
    FBitMap.Height:= Height;
  end;

  procedure Create_GIF;
  begin
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
  end;
  {
  procedure Create_AUD;
  begin
    //it is not necessary for audio

  end;
  }

  procedure Create_VID;
  begin
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

    if not EditMode then
      begin

      end
    else
      begin

      end;

  end;

  function Load_BMP (Audio: boolean) : boolean;
  begin
    Result := False;
    try
      FBitMap.LoadFromFile(s2);
      SetKind(Audio, stmPicture);
    except
      on Exception do Exit;
    end;
    Result := True;
  end;

  function Load_JPG (Audio: boolean) : boolean;
  var jpg : TJPEGImage;
  begin
    Result := False;
    try
      jpg:= TJPEGImage.Create;
      jpg.LoadFromFile(s2);
      FBitMap.Assign(jpg);
      jpg.Free;
      SetKind(Audio, stmPicture);
    except
      on Exception do Exit;
    end;
    Result := True;
  end;

  function Load_GIF(Audio: boolean) : boolean;
  begin
    Result := False;

    {
    try
      FGifImage.Image.LoadFromFile (s2);
    except
      on Exception do Exit;
    end;

    SetKind (Audio, stmAnimation);
    Result := True;
    }

  end;

  function Load_AUD: boolean;
  begin
    Result := False;
    s2 := path;
    try
      FAudioChannel := TBassChannel.LoadFromFile(s2, FLoopNumber);
    except
      on E : Exception do
        begin
          WriteLn('TKey.SetFileName: ' + E.Message);
          Application.Terminate;
        end;
    end;
    SetKind (True, TImage.stmNone);
    Result := True;
  end;

  function Load_VID (Audio : boolean) : boolean;
  begin
    Result := False;
    {
    try
      if EditMode then FMedia.settings.playCount := 1
      else if FLoopNumber = 0 then FMedia.settings.playCount := MaxInt
      else if FLoopNumber > 0 then FMedia.settings.playCount := FLoopNumber
      else if FLoopNumber < 0 then FMedia.settings.playCount := Abs(FLoopNumber);
      FMedia.URL := s2;
    except
      on Exception do Exit;

    end;
    FMPlayer.OnNotify:= MPlayerLoopNotify;
    SetKind (Audio, Image);
    Result := True;
    }
  end;
begin
  if FileExistsUTF8(Path) { *Converted from FileExists*  } then
    begin
      s1:= UpperCase(ExtractFileExt(Path));

      // create section

      // image
      if   (s1 = '.BMP')
        or (s1 = '.JPG') then Create_IMG;

      // animation
      if   (s1 = '.GIF') then Create_GIF;

      // audio
      {
      if   (s1 = '.WAV')
        or (s1 = '.AIFF')
        or (s1 = '.MP3')
        or (s1 = '.MP2')
        or (s1 = '.MP1')
        or (s1 = '.OGG') then Create_AUD;
      }

      // video
      if   (s1 = '.MPG')
        or (s1 = '.AVI')
        or (s1 = '.MOV')
        or (s1 = '.FLV')
        or (s1 = '.WMV')
        or (s1 = '.MP4') then Create_VID;

      //load section
      s2 := Path;
      if ( (s1 = '.MPG')
        or (s1 = '.AVI')
        or (s1 = '.MOV')
        or (s1 = '.FLV')
        or (s1 = '.WMV')
        or (s1 = '.MP4') ) and Load_VID (False) then RpSave;

      if (s1 = '.BMP') and Load_BMP(False) then RpSave;
      if (s1 = '.JPG') and Load_JPG(False) then RpSave;
      if (s1 = '.GIF') and Load_GIF (False) then RpSave;

      if (  (s1 = '.WAV')
         or (s1 = '.AIFF')
         or (s1 = '.MP3')
         or (s1 = '.MP2')
         or (s1 = '.MP1')
         or (s1 = '.OGG')  )
         and Load_AUD then
        begin
          // note that at this point we already loaded the audio file
          // the user can associate an image with each audio file sound
          // the user can place an image file with the same name as the audio file inside rootmedia
          // we will load the image here
          s1:= Path;
          Delete(s1, pos(Copy(Path,Length(Path)- 3,4),s1), 4);
          if FileExists(s1 + '.BMP') { *Converted from FileExists*  } then
            begin
              Create_IMG;
              s2:= s1 + '.BMP';
              Load_BMP(True)
            end;

          if FileExists(s1 + '.JPG') { *Converted from FileExists*  } then
            begin
              Create_IMG;
              s2:= s1 + '.JPG';
              Load_JPG(True)
            end;

          if FileExists(s1 + '.GIF') { *Converted from FileExists*  } then
            begin
              Create_GIF;
              s2:= s1 + '.GIF';
              Load_GIF(True)
            end;
          RpSave;
        end;
    end
  else FFileName := '';
  Invalidate;
end;

procedure TKey.KeyMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Inc(FResponseCount);
  FLastResponseLog := IntToStr(X + Left) + #9 + IntToStr(Y + Top);
  FSchMan.DoResponse;
end;

//******
//**  **
//******
// Sender is irrelevant at this point, TKey needs to send its info to TTrial decendents.
//******
//**  **
//******
procedure TKey.Consequence(Sender: TObject);
begin
  if Assigned(OnConsequence) then FOnConsequence(Self);   //Necessariamente Self
end;

procedure TKey.Response(Sender: TObject);
begin
  if Assigned(OnResponse) then FOnResponse(Self);   //Necessariamente  SELF
end;


end.

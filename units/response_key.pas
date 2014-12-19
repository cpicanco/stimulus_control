//
// Validation Project (PCRF) - Stimulus Control App
// Copyright (C) 2014,  Carlos Rafael Fernandes Picanço, cpicanco@ufpa.br
//
// This file is part of Validation Project (PCRF).
//
// Validation Project (PCRF) is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Validation Project (PCRF) is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Validation Project (PCRF).  If not, see <http://www.gnu.org/licenses/>.
//
unit response_key;

{$mode objfpc}{$H+}
//{$MODE Delphi}

interface

uses Dialogs,
     LCLIntf, LCLType, LMessages, Messages,
     SysUtils, Variants, Classes, Graphics, Controls, Forms,
     {Registry,} StdCtrls, ExtCtrls,
     FileUtil, {ShellAnimations, OleCtrls,}
     schedules_abstract, counter
     {, JvExControls, JvAnimatedImage, JvGIFCtrl, WMPLib_TLB};

type

  TImage = (stmNone, stmPicture, stmAnimation, stmVideo);

  TResponsePoint = array [0..1] of Integer;

  TKind = record
    stmAudio : boolean;
    stmImage : TImage;
  end;

  TKey = class(TCustomControl)
  private
    FSchMan: TSchMan;
    FCounterResponse : TCounter;
    FOnConsequence: TNotifyEvent;
    FOnConsequence2: TNotifyEvent;
    FOnResponse: TNotifyEvent;
    FOnEndMedia: TNotifyEvent;
    FBitMap: TBitMap;
    //FGifImage: TJvGIFAnimator;
    //FMedia : TWindowsMediaPlayer;
    FColor: TColor;
    FEdge: TColor;
    FFileName: String;
    FKind: TKind;
    FEditMode: Boolean;
    FLoopNumber: Integer;
    FLRespPnt: TResponsePoint;
    procedure SetFileName(Path: string);
    procedure Consequence (Sender: TObject);
    procedure Consequence2 (Sender: TObject);
    procedure Response (Sender: TObject);
    //procedure CMMouseEnter(var msg: TMessage); message CM_MOUSEENTER;
    //procedure CMMouseLeave(var msg: TMessage); message CM_MOUSELEAVE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); overload; override;
    procedure MouseDown(Sender : TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);reintroduce; overload;
    procedure MouseDown(Sender : TObject; Button: Smallint; ShiftState: Smallint; X: Integer; Y: Integer); reintroduce; overload;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ResponseCount : integer;
    procedure Paint; override;
    procedure Play;
    procedure Stop;
    procedure FullScreen;
    procedure IncCounterResponse;
    property Caption;
    property Color : TColor read FColor write FColor;
    property Edge : TColor read FEdge write FEdge;
    property EditMode: Boolean read FEditMode write FEditMode;
    property Font;
    property FullPath: String read FFileName write SetFileName;
    property HowManyLoops : Integer read FLoopNumber write FLoopNumber;
    property Kind: TKind read FKind;
    property LastResponsePoint : TResponsePoint read FLRespPnt;
    //property FileName2: String read FFileName2 write SetFileName;
    property OnClick;
    //property OnGesture;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnConsequence: TNotifyEvent read FOnConsequence write FOnConsequence;
    property OnConsequence2: TNotifyEvent read FOnConsequence2 write FOnConsequence2;
    property OnResponse: TNotifyEvent read FOnResponse write FOnResponse;
    property OnEndMedia: TNotifyEvent read FOnEndMedia write FOnEndMedia;
    property SchMan: TSchMan read FSchMan;
    //property Touch;
  end;

implementation

constructor TKey.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  Height:= 45;
  Width:= 60;
  EditMode := False;
  FLRespPnt[0] := 0;
  FLRespPnt[1] := 0;
  Color:= clRed;
  Edge:= clInactiveCaption;

  FCounterResponse := TCounter.Create;
  FSchMan:= TSchMan.Create(self);
  with FSchMan do
    begin
      OnConsequence2 := @Consequence2;
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

  if Assigned(FBitMap) then FBitMap.Free;
  //if Assigned(FGifImage) then FreeAndNil (FGifImage);

  if Assigned(FCounterResponse) then FCounterResponse.Free;
  Inherited Destroy;
end;

procedure TKey.FullScreen;
begin
  {if (FKind.stmImage = stmVideo) then
    begin
      repeat Application.ProcessMessages until FMedia.playState = 3;
      if (FMedia.playState = 3) then
      FMedia.fullScreen := True;
    end;  }
end;

procedure TKey.IncCounterResponse;
begin
  FCounterResponse.Plus(1);
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
          //Interpolation := TFPSharpInterpolation.Create;
          Draw(0,0, FBitMap);
          //StretchDraw(Rect(0, 0, Width, Height), FBitMap);
        end;
    end;
begin
  if FileExistsUTF8(Self.FullPath) { *Converted from FileExists*  } then
    begin
      if (FKind.stmImage = stmPicture) then PaintFBitMap;
      if (FKind.stmImage = stmAnimation) then ;//FGifImage.Visible := True;
    end
  else
    begin
      PaintKey(Color);
    end;
end;

procedure TKey.Play;
begin
  {if Assigned(FMedia) then
    begin
      if (FKind.stmAudio) or (FKind.stmImage = stmVideo) then
        try
          //FMPlayer.Notify:= True;
          if not (FMedia.playState = wmppsPlaying) then
            FMedia.controls.play
          else
            FMedia.controls.stop;
        except
          on E:Exception do
            begin
              //MPlayerNotify(nil);
            end;
        end;
    end
  else;// MPlayerNotify(nil); }
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

procedure TKey.SetFileName(Path: String);                 //REVISAR
var s1, s2 : String; //jpg : TJpegImage;
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

    procedure Create_MediaPlayer (OnlyAudio : Boolean);
    begin
      {FMedia := TWindowsMediaPlayer.Create(self);
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
      FMedia.uiMode := 'none'; }

      if not EditMode then
        begin

        end
      else
        begin

        end;

    end;

    procedure RpSave;
    begin
      //Repaint;
      FFileName := Path;
      // Exit;
    end;

    procedure SetKind (Audio : boolean; Image : TImage);
    begin
      FKind.stmAudio := Audio;
      FKind.stmImage := Image;
    end;

    function Load_BMP (Sound: boolean) : boolean;
    begin
      try
        FBitMap.LoadFromFile(s2);
        SetKind (Sound, stmPicture);
      except
        on Exception do
          begin
            Result := False;
            Exit;
          end;
      end;
      Result := True;
    end;

    function Load_JPG (Sound: boolean) : boolean;
    var jpg : TJPEGImage;
    begin
      try
        jpg:= TJPEGImage.Create;
        jpg.LoadFromFile(s2);
        FBitMap.Assign(jpg);
        jpg.Free;
        SetKind (Sound, stmPicture);
      except
        on Exception do
          begin
            Result := False;
            Exit;
          end;
      end;
      Result := True;
    end;

    function Load_GIF(Sound: boolean) : boolean;
    begin
      {
      try
        FGifImage.Image.LoadFromFile (s2);
        SetKind (Sound, stmAnimation);
        Result := True;
      except
        on Exception do
          begin
            Result := False;
            Exit;
          end;
      end;}

    end;

    function Load_Media (Audio : boolean; Image : TImage) : boolean;
    begin
      {try
        if EditMode then FMedia.settings.playCount := 1
        else if FLoopNumber = 0 then FMedia.settings.playCount := MaxInt
        else if FLoopNumber > 0 then FMedia.settings.playCount := FLoopNumber
        else if FLoopNumber < 0 then FMedia.settings.playCount := Abs(FLoopNumber);
        FMedia.URL := s2;
      except
        on Exception do
          begin
            Result := False;
            Exit;
          end;
      end;
      //FMPlayer.OnNotify:= MPlayerLoopNotify; }
      SetKind (Audio, Image);
      Result := True;
    end;
begin
  {if Assigned(FMedia) then begin
    FMedia.Close;
    FreeAndNil(FMedia);
  end; }

  if FileExistsUTF8(Path) { *Converted from FileExists*  } then begin
    s1:= UpperCase(ExtractFileExt(Path));
    if (s1 = '.BMP') or (s1 = '.JPG') then Create_IMG;               //image

    if (s1 = '.GIF') then Create_GIF;                                //animation

    if (s1 = '.WAV') or (s1 = '.MID') then  Create_MediaPlayer(True);//audio

    if (s1 = '.MPG') or (s1 = '.AVI') or                             //v\EDdeo
       (s1 = '.MOV') or (s1 = '.FLV') or
       (s1 = '.WMV') or (s1 = '.MP4') then Create_MediaPlayer(False);

    s2:= Path;
    if ( (s1 = '.MPG') or (s1 = '.AVI') or
         (s1 = '.MOV') or (s1 = '.FLV') or
         (s1 = '.WMV') or (s1 = '.MP4') ) and Load_Media (False,stmVideo) then RpSave;

    if (s1 = '.BMP') and Load_BMP(False) then RpSave;
    if (s1 = '.JPG') and Load_JPG(False) then RpSave;
    if (s1 = '.GIF') and Load_GIF (False) then RpSave;
    if ((s1 = '.WAV') or (s1 = '.MID')) and Load_Media(True,stmNone) then
      begin
        s1:= Path;
        Delete(s1, pos(Copy(Path,Length(Path)- 3,4),s1), 4);
        if FileExistsUTF8(s1 + '.BMP') { *Converted from FileExists*  } then
          begin
            Create_IMG;
            s2:= s1 + '.BMP';
            if Load_BMP(True) then RpSave;
          end;
        if FileExistsUTF8(s1 + '.JPG') { *Converted from FileExists*  } then
          begin
            Create_IMG;
            s2:= s1 + '.JPG';
            if Load_JPG(True) then RpSave;
          end;
        if FileExistsUTF8(s1 + '.GIF') { *Converted from FileExists*  } then
          begin
            Create_GIF;
            s2:= s1 + '.GIF';
            if Load_GIF(True) then RpSave;
          end;
      end;
  end
  else FFileName := '';
  Repaint;
end;

procedure TKey.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FLRespPnt[0] := X;
  FLRespPnt[1] := Y;
  FSchMan.DoResponse;
end;

procedure TKey.MouseDown(Sender : TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FLRespPnt[0] := X;
  FLRespPnt[1] := Y;
  FSchMan.DoResponse;
end;

procedure TKey.MouseDown(Sender : TObject; Button: Smallint; ShiftState: Smallint; X: Integer; Y: Integer);
var Shift : TShiftState;
    aButton : TMouseButton;
begin
  case ShiftState of
    0: Shift := [ssShift];
    1: Shift := [ssAlt];
    2: Shift := [ssCtrl];
    3: Shift := [ssLeft];
    4: Shift := [ssRight];
    5: Shift := [ssMiddle];
    6: Shift := [ssDouble];
    //7: Shift := [ssTouch];
    //8: Shift := [ssPen];
    //9: Shift := [ssCommand];
      else Shift := [];
  end;
  case Button of
    0: aButton := mbLeft;
    1: aButton := mbRight;
    2: aButton := mbMiddle;
    else aButton := mbLeft;
  end;

  inherited MouseDown(aButton, Shift, X, Y);
  FLRespPnt[0] := X;
  FLRespPnt[1] := Y;
  FSchMan.DoResponse;
end;
//******
//**  **
//******
// Necessariamente Self, pois o objeto TKey deve enviar suas informa\E7\F5es de localiza\E7\E3o aos descendentes de TTrial
// Sender is irrelevant at this point, TKey needs to send its info to TTrial decendents.
//******
//**  **
//******
procedure TKey.Consequence(Sender: TObject);
begin
  If Assigned(OnConsequence) then FOnConsequence (Self);   //Necessariamente Self
end;

procedure TKey.Consequence2(Sender: TObject);
begin
  if Assigned(OnConsequence2) then FOnConsequence2 (Self);    //Necessariamente Self, sender is irrelevant at this point
end;

procedure TKey.Response(Sender: TObject);
begin
  If Assigned(OnResponse) then FOnResponse (Self);   //Necessariamente  SELF
end;

function TKey.ResponseCount: integer;
begin
  Result := FCounterResponse.Counter;
end;

end.

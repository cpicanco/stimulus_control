{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit bass_player;

{$mode objfpc}{$H+}

interface

uses LCLType, Classes, SysUtils, Forms
  , BASS
  ;

 // http://www.un4seen.com/doc/#bass/

type

  { TBassAudioDevice }

  TBassAudioDevice = class
    private
      { private declarations }
    public
      constructor Create (WindowHandle : HWND); overload;
      constructor Create; overload;
      destructor Destroy; override;
      //procedure SetVolume; virtual; abstract;
      //procedure SetDevice; virtual; abstract;
  end;

  { TBassChannel }

  TBassChannel = class
    private
      FChannel : HCHANNEL;
      FLoops: integer;
      FSample : HSAMPLE;
    public
      constructor Create(FileName : AnsiString); overload;
      constructor Create(FileName : AnsiString; Loops : integer); overload;
      destructor Destroy;override;
      procedure Play;
      //procedure Pause; virtual; abstract;
      property Loops : integer read FLoops write FLoops;
  end;

  { TBassStream }
  PBassStream = ^TBassStream;

  TBassStream = class
    private
      FLoops : integer;
      FSample : HSAMPLE;
      FSyncProcedure: SYNCPROC;
      procedure SetSyncProcedure(AValue: SYNCPROC);
    public
      constructor Create(FileName : AnsiString; Loops : integer = -1); overload;
      procedure Play;
      procedure Stop;
      property SyncProcedure : SYNCPROC read FSyncProcedure write SetSyncProcedure;
  end;


implementation

{ TBassStream }

procedure TBassStream.SetSyncProcedure(AValue: SYNCPROC);
begin
  if FSyncProcedure=AValue then Exit;
    FSyncProcedure:=AValue;

  if Assigned(FSyncProcedure) then
    BASS_ChannelSetSync(FSample, BASS_SYNC_END, 0, SyncProcedure, nil);
end;

constructor TBassStream.Create(FileName: AnsiString; Loops: integer);
begin
  FLoops := Loops;
  case FLoops of
   -MaxInt..0: FSample := BASS_StreamCreateFile(FALSE, PChar(FileName), 0, 0, BASS_STREAM_AUTOFREE);
   else
     FSample := BASS_StreamCreateFile(FALSE, PChar(FileName), 0, 0, BASS_SAMPLE_LOOP);
  end;
end;

procedure TBassStream.Play;
begin
  BASS_ChannelPlay(FSample, FLoops > 0);
end;

procedure TBassStream.Stop;
begin
  BASS_ChannelStop(FSample);
  if FLoops > 0 then
    BASS_StreamFree(FSample);
end;

{ TBassAudioDevice }

  // on session start
constructor TBassAudioDevice.Create(WindowHandle: HWND);
{$IFDEF MSWINDOWS}

{$ELSE}
  var awindow : Pointer;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    BASS_Init(-1, 44100, 0, WindowHandle, nil);
  {$ELSE}
    awindow := @WindowHandle;
    BASS_Init(-1, 44100, 0, awindow, nil);
  {$ENDIF}
end;

// on session start
constructor TBassAudioDevice.Create;
begin
  {$IFDEF MSWINDOWS}
    // need testing
  {$ELSE}
    BASS_Init(-1, 44100, 0, nil, nil);
  {$ENDIF}
end;

// on before program closes
destructor TBassAudioDevice.Destroy;
begin
  BASS_Free;
  inherited Destroy;
end;

  { TBassChannel }

// on response_key create, if an audio extension was found
constructor TBassChannel.Create(FileName: AnsiString);
  var
    aLength, aMax, aFlags: DWORD;
    aOffSet : QWORD;
  begin

    aOffSet := 0;
    aLength := 0;
    aMax := 1;
    aFlags := 0;

    FSample := BASS_SampleLoad( False, PAnsiChar(FileName), aOffSet, aLength, aMax, aFlags );
    if FSample <> 0 then
      FChannel := BASS_SampleGetChannel(FSample, False);
  end;

constructor TBassChannel.Create(FileName: AnsiString; Loops: integer);
var
  aLength, aMax, aFlags: DWORD;
  aOffSet : QWORD;
begin
  FLoops := Loops;

  aOffSet := 0;
  aLength := 0;
  aMax := 1;
  aFlags := 0;

  FSample := BASS_SampleLoad( False, PAnsiChar(FileName), aOffSet, aLength, aMax, aFlags );
  if FSample <> 0 then
    FChannel := BASS_SampleGetChannel(FSample, False);
end;

destructor TBassChannel.Destroy;
begin
  BASS_SampleFree(FSample);
  BASS_ChannelStop(FChannel);
  inherited Destroy;
end;

procedure TBassChannel.Play;
var  i, aLoops : integer;
begin
  if FLoops > 0 then aLoops := FLoops
  else aLoops := 0;

  for i:= 0 to aLoops do
    BASS_ChannelPlay(FChannel, True);
end;

end.


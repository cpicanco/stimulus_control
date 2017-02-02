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

uses LCLType, Classes, SysUtils, Forms, BASS;

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
      constructor LoadFromFile(FileName : AnsiString); overload;
      constructor LoadFromFile(FileName : AnsiString; Loops : integer); overload;
      destructor Stop;
      procedure Play;
      //procedure Pause; virtual; abstract;
      property Loops : integer read FLoops write FLoops;
  end;

implementation

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
constructor TBassChannel.LoadFromFile(FileName: AnsiString);
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

constructor TBassChannel.LoadFromFile(FileName: AnsiString; Loops: integer);
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

destructor TBassChannel.Stop;
begin
  BASS_ChannelStop(FChannel);
end;

procedure TBassChannel.Play;
var  i, aLoops : integer;
begin

  if FLoops > 0 then aLoops := FLoops
  else aLoops := 0;

  for i:= 0 to aLoops do
    begin
      BASS_ChannelPlay (FChannel, True);
      //while BASS_ChannelIsActive(FChannel) = BASS_ACTIVE_PLAYING do Application.ProcessMessages;
    end;

end;

end.


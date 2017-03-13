{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit bass_player_callbacks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , BASS
  , background
  ;

// hello world callback
procedure EndOfFileCallBack(AHandle: HSYNC; Channel, Data: DWORD; User: Pointer);{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

implementation

{$IFDEF MSWINDOWS}
procedure EndOfFileCallBack(AHandle: HSYNC; Channel, Data: DWORD; User: Pointer); stdcall;
begin
  // todo
end;
{$ELSE}
procedure EndOfFileCallBack(AHandle: HSYNC; Channel, Data: DWORD; User: Pointer); cdecl;
begin
  FrmBackground.DrawMask := True;
end;
{$ENDIF}

end.


unit ini_helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , config_session
  , constants
  ;

function GetNumTrials(ASessionIniFile : TCIniFile) : integer;
function GetSection(ATrial: integer; ABloc : integer) : string; overload;
function GetSection(ABloc : integer) : string; overload;
function IncSection(ASection : string; ABloc : integer) : string; overload;
function IncSection(ABloc : integer) : string; overload;
function IsTrialSection(aSection : string) : integer;

implementation

const
  BegStr = '[Blc ';
  Separator = #32 + '-' + #32;
  EndStr = ']';

function GetNumTrials(ASessionIniFile: TCIniFile): integer;
var
  s1 : string;
begin
  s1:= ASessionIniFile.ReadString(_Blc + #32 + IntToStr(i), _NumTrials, '0 0') + #32;
  Result := StrToIntDef(Copy(s1, 0, Pos(#32, s1) - 1), 0);
end;

{.
 . For Trial Sections: GetSection(1, 2); // Trial 1, Bloc 2
 .}
function GetSection(ATrial: integer; ABloc: integer): string;
begin
  Result := BegStr + IntToStr(ABloc) + Separator + 'T' + IntToStr(ATrial) + EndStr;
end;

{.
 . For Bloc Sections: GetSection(1); // Bloc 1
 .}
function GetSection(ABloc: integer): string;
begin
  Result := BegStr + IntToStr(ABloc) + EndStr;
end;

function IncSection(aSection : string; iBloc : integer) : string;
var
  i : integer;
  BegStrT : string;
begin
  BegStrT := BegStr + IntToStr(ABloc) + Separator + 'T';
  Delete(  aSection, Pos(BegStrT, aSection), Length(BegStrT)  );
  Delete(  aSection, Pos(EndStr, aSection), Length(EndStr)  );

  i := StrToInt(aSection);
  Inc(i);
  Result := BegStrT + IntToStr(i) + EndStr;
end;

function IncSection(ABloc: integer): string;
var
  i : integer;
begin
  Delete(  aSection, Pos(BegStr, aSection), Length(BegStr)  );
  Delete(  aSection, Pos(EndStr, aSection), Length(EndStr)  );

  i := StrToInt(aSection);
  Inc(i);
  Result := BegStr + IntToStr(i) + EndStr;
end;

{
.
. If aSection is a Trial then Result is the trial number else Result is 0.
.
.}
function IsTrialSection(aSection : string) : integer;
var BeginStr, EndStr, Separator, iBloc : string;
begin
  if Pos('T', aSection) <> 0 then
    begin
      Separator := #32 + '-' + #32;
      iBloc := Copy(aSection, Pos('Blc', aSection) + 4, 1);
      BeginStr := BegStr + iBloc + Separator + 'T';
      EndStr := ']';

      Delete(  aSection, Pos(BeginStr, aSection), Length(BeginStr)  );
      Delete(  aSection, Pos(EndStr, aSection), Length(EndStr)  );

      Result := StrToInt(aSection);
    end
  else Result := 0;
end;

end.


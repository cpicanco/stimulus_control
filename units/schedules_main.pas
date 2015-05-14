//
// Validation Project (PCRF) - Stimulus Control App
// Copyright (C) 2014-2015,  Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.
//
// cpicanco@ufpa.br
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
unit schedules_main;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils

     , schedules
     ;

type

  { TSchMan }

  TSchMan = class(TComponent)
  private
    FAbsSch: TAbsSch;
    FAbsSchLoaded: Boolean;
    FKind: string;
    FOnConsequence: TNotifyEvent;
    FOnResponse: TNotifyEvent;
    procedure Consequence(Sender: TObject);
    procedure Response(Sender: TObject);
    procedure SetKind(Kind: String);
  public
    procedure DoResponse;
    function StartMethod : TThreadMethod;
    property Kind: string read FKind write SetKind;
    property Loaded : Boolean read FAbsSchLoaded;
    property OnConsequence: TNotifyEvent read FOnConsequence write FOnConsequence;
    property OnResponse: TNotifyEvent read FOnResponse write FOnResponse;
  end;

implementation

{$ifdef DEBUG}
  uses debug_logger;
{$endif}

procedure TSchMan.SetKind(Kind: String);
var
  aSchedule,
  Parameter1, Parameter2 : String;
  //Parameter3, Parameter4 : String;

  procedure NextSpaceDelimitedParamater;
  begin
    Delete(Kind, 1, pos(#32, Kind));
    if Length(Kind) > 0 then While Kind[1] = ' ' do Delete(Kind, 1, 1);
  end;

begin
  if FAbsSchLoaded then
    begin
      FreeAndNil(FAbsSch);
      FAbsSchLoaded := Assigned(FAbsSch);
    end;

  Kind := Kind + #32;
  aSchedule := Copy(Kind, 0, Pos(#32, Kind) - 1);

  NextSpaceDelimitedParamater;
  Parameter1 := Copy(Kind, 0, Pos(#32, Kind) - 1);

  NextSpaceDelimitedParamater;
  Parameter2 := Copy(Kind, 0, Pos(#32, Kind) - 1);

  //NextSpaceDelimitedParamater;
  //Parameter3 := Copy(Kind, 0, Pos(#32, Kind) - 1);

  //NextSpaceDelimitedParamater;
  //Parameter4 := Copy(Kind, 0, Pos(#32, Kind) - 1);

  if aSchedule = 'CRF' then
    begin
      FAbsSch:= TSchRR.Create(Self);
      FAbsSch.Parameter1 := 1;
      FAbsSch.Parameter2 := 0;
    end;

  if aSchedule = 'EXT' then
    begin
      FAbsSch:= TSchRR.Create(Self);
      FAbsSch.Parameter1 := MaxInt;
      FAbsSch.Parameter2 := 0;
    end;

  if (aSchedule = 'FR') or (aSchedule = 'FI') or (aSchedule = 'FT') then
    begin
      if aSchedule = 'FR' then FAbsSch:= TSchRR.Create(Self);
      if aSchedule = 'FI' then FAbsSch:= TSchRI.Create(Self);
      if aSchedule = 'FT' then FAbsSch:= TSchRT.Create(Self);

      FAbsSch.Parameter1 := StrToIntDef(Parameter1 , 0);
      FAbsSch.Parameter2 := 0;
    end;

  if (aSchedule = 'VR') or (aSchedule = 'VI') or (aSchedule = 'VT') then
    begin
      if aSchedule = 'VR' then FAbsSch:= TSchRR.Create(Self);
      if aSchedule = 'VI' then FAbsSch:= TSchRI.Create(Self);
      if aSchedule = 'VT' then FAbsSch:= TSchRT.Create(Self);

      FAbsSch.Parameter1 := StrToIntDef(Parameter1, 0);
      FAbsSch.Parameter2 := StrToIntDef(Parameter2, 0);
    end;

  if aSchedule = 'DRL' then
    begin
      FAbsSch:= TSchDRL.Create(Self);
      FAbsSch.Parameter1 := StrToIntDef(Parameter1, 0);
      //FAbsSch.Parameter2 := StrToIntDef(Parameter2, 0);
    end;

  if aSchedule = 'DRH' then
    begin
      FAbsSch:= TSchDRH.Create(Self);
      FAbsSch.Parameter1 := StrToIntDef(Parameter1, 0);
      FAbsSch.Parameter2 := StrToIntDef(Parameter2, 0);
      //FAbsSch.Parameter3 := StrToIntDef(Parameter3, 0);
      //FAbsSch.Parameter4 := StrToIntDef(Parameter4, 0);
    end;

  FAbsSchLoaded := Assigned(FAbsSch);
  {$ifdef DEBUG}
    DebugLn(mt_Debug + 'FAbsSchLoaded:'+ BoolToStr(FAbsSchLoaded, 'True', 'False') );
  {$endif}

  if FAbsSchLoaded then
    begin
      {$ifdef DEBUG}
        DebugLn(mt_Debug + 'Schedule:' + aSchedule + #32 +
          Parameter1  + #32 + Parameter2  + #32
          //+ Parameter3 + #32 + Parameter4
          );
      {$endif}
      FAbsSch.OnConsequence := @Consequence;
      FAbsSch.OnResponse := @Response;
      FAbsSch.AssignParameters;
      FAbsSch.Reset;
      FKind := aSchedule;
    end;
end;

procedure TSchMan.Consequence(Sender: TObject);
begin
  if Assigned(OnConsequence) then FOnConsequence(Self);
end;

procedure TSchMan.Response(Sender: TObject);
begin
  if Assigned(OnResponse) then FOnResponse (Self);
end;

procedure TSchMan.DoResponse;
begin
  if FAbsSchLoaded then FAbsSch.DoResponse;
end;

function TSchMan.StartMethod : TThreadMethod;
begin
  Result := FAbsSch.StartMethod;
end;

end.


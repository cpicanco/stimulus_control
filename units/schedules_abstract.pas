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
unit schedules_abstract;

{$MODE Delphi}

interface

uses Classes, SysUtils,
     schedules;

type
  TSchMan = class(TComponent)
  protected
    FAbsSch: TAbsSch;
    FAbsSchLoaded: Boolean;
    FOnConsequence: TNotifyEvent;
    FOnConsequence2: TNotifyEvent;
    FOnResponse: TNotifyEvent;
    procedure AbsSchConsequence(Sender: TObject);
    procedure AbsSchConsequence2(Sender: TObject);
    procedure AbsSchResponce(Sender: TObject);
    procedure SetKind(Kind: String);
    function GetTime : integer;
  public
    procedure Clock;
    procedure DoResponse;
    procedure Play;
    property Kind: String write SetKind;
    property OnConsequence: TNotifyEvent read FOnConsequence write FOnConsequence;
    property OnConsequence2: TNotifyEvent read FOnConsequence2 write FOnConsequence2;
    property OnResponse: TNotifyEvent read FOnResponse write FOnResponse;
    property Time : integer read GetTime;
  end;

implementation

procedure TSchMan.SetKind(Kind: String);
var s1, s2, s3, s4, s5: String;
begin
  if FAbsSchLoaded then begin
    FAbsSch.Free;
    FAbsSchLoaded:= False;
  end;

  Kind:= Kind+#32;                            //[' FR 2' + ' ']
  s1:= Copy(Kind, 0, Pos(#32, Kind) - 1);       //

  Delete(Kind, 1, pos(#32, Kind));
  if Length(Kind) > 0 then While Kind[1] = ' ' do Delete(Kind, 1, 1);
  s2:= Copy(Kind, 0, Pos(#32, Kind) - 1);

  Delete(Kind, 1, pos(#32, Kind));
  if Length(Kind) > 0 then While Kind[1] = ' ' do Delete(Kind, 1, 1);
  s3:= Copy(Kind, 0, Pos(#32, Kind) - 1);

  Delete(Kind, 1, pos(#32, Kind));
  if Length(Kind) > 0 then While Kind[1] = ' ' do Delete(Kind, 1, 1);
  s4:= Copy(Kind, 0, Pos(#32, Kind) - 1);

  Delete(Kind, 1, pos(#32, Kind));
  if Length(Kind) > 0 then While Kind[1] = ' ' do Delete(Kind, 1, 1);
  s5:= Copy(Kind, 0, Pos(#32, Kind) - 1);

  if s1 = 'CRF' then begin
    FAbsSch:= TSchRR.Create(Self);
    FAbsSch.Value:= 1;
    FAbsSch.Variation:= 0;
  end;

  if s1 = 'EXT' then begin
    FAbsSch:= TSchRR.Create(Self);
    FAbsSch.Value:= MaxInt;
    FAbsSch.Variation:= 0;
  end;

  if (s1 = 'FR') or (s1 = 'FI') or (s1 = 'FT') then begin
    if s1 = 'FR' then FAbsSch:= TSchRR.Create(Self);
    if s1 = 'FI' then FAbsSch:= TSchRI.Create(Self);
    if s1 = 'FT' then FAbsSch:= TSchRT.Create(Self);

    FAbsSch.Value:= StrToIntDef(s2, 0);
    FAbsSch.Variation:= 0;
  end;

  if (s1 = 'VR') or (s1 = 'VI') or (s1 = 'VT') then begin
    if s1 = 'VR' then FAbsSch:= TSchRR.Create(Self);
    if s1 = 'VI' then FAbsSch:= TSchRI.Create(Self);
    if s1 = 'VT' then FAbsSch:= TSchRT.Create(Self);

    FAbsSch.Value:= StrToIntDef(s2, 0);
    FAbsSch.Variation:= StrToIntDef(s3, 0);
  end;

  if s1 = 'DRL' then begin
    FAbsSch:= TSchDRL.Create(Self);
    FAbsSch.Value:= StrToIntDef(s2, 0);
    FAbsSch.Variation:= StrToIntDef(s3, 0);
  end;

  if s1 = 'DRH' then begin
    FAbsSch:= TSchDRH.Create(Self);
    FAbsSch.Value   := StrToIntDef(s2, 0);
    FAbsSch.Value2  := StrToIntDef(s3, 0);
    FAbsSch.Variation   := StrToIntDef(s4, 0);
    FAbsSch.Variation2  := StrToIntDef(s5, 0);
  end;
  if (s1 = 'FRFT') or (s1 = 'VRFT') or
     (s1 = 'FRVT') or (s1 = 'VRVT') then begin
    FAbsSch:= TSchRRRT.Create(Self);
    if s1 = 'FRFT' then
      begin
        FAbsSch.Value   := StrToIntDef(s2, 0);
        FAbsSch.Value2  := StrToIntDef(s3, 0);
        FAbsSch.Variation   := 0;
        FAbsSch.Variation2  := 0;
      end;
    if s1 = 'VRFT' then
      begin
        FAbsSch.Value   := StrToIntDef(s2, 0);
        FAbsSch.Value2  := StrToIntDef(s3, 0);
        FAbsSch.Variation   := StrToIntDef(s4, 0);
        FAbsSch.Variation2  := 0;
      end;
    if s1 = 'FRVT' then
      begin
        FAbsSch.Value   := StrToIntDef(s2, 0);
        FAbsSch.Value2  := StrToIntDef(s3, 0);
        FAbsSch.Variation   := 0;
        FAbsSch.Variation2  := StrToIntDef(s5, 0);
      end;
    if s1 = 'VRVT' then
      begin
        FAbsSch.Value   := StrToIntDef(s2, 0);
        FAbsSch.Value2  := StrToIntDef(s3, 0);
        FAbsSch.Variation   := StrToIntDef(s4, 0);
        FAbsSch.Variation2  := StrToIntDef(s5, 0);
      end;
  end;

  FAbsSchLoaded:= Assigned(FAbsSch); // Assigned (FAbsSch) Testa se FAbsSch é nulo ou não. Retorna False se nulo.

  if FAbsSchLoaded then begin
    FAbsSch.OnConsequence2:= AbsSchConsequence2;
    FAbsSch.OnConsequence:= AbsSchConsequence;
    FAbsSch.OnResponse:= AbsSchResponce;
    FAbsSch.Reset;
    FAbsSch.Reset2;
  end;
end;

procedure TSchMan.AbsSchConsequence(Sender: TObject);
begin
  if Assigned(OnConsequence) then FOnConsequence(Self);
end;

procedure TSchMan.AbsSchConsequence2(Sender: TObject);
begin
  if Assigned(OnConsequence2) then FOnConsequence2(Self);
end;

procedure TSchMan.AbsSchResponce(Sender: TObject);
begin
  if Assigned(OnResponse) then FOnResponse (Self);
end;

procedure TSchMan.Clock;
begin
  if FAbsSchLoaded then
    FAbsSch.Clock;
end;

procedure TSchMan.DoResponse;
begin
  if FAbsSchLoaded then
    FAbsSch.DoResponse;
end;

function TSchMan.GetTime: integer;
begin
  if FAbsSchLoaded then Result:= FAbsSch.Time
  else Result:= -1;
end;

procedure TSchMan.Play;
begin
  if FAbsSchLoaded then
    begin
      FAbsSch.Reset;
      FAbsSch.Reset2;
    end;
end;

end.


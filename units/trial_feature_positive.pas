{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit trial_feature_positive;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils

    //, counter
    , dialogs
    , config_session
    //, countermanager
    , trial_abstract
    //, custom_timer
    //, client
    , draw_methods
    , schedules_main
    , response_key
    ;

type

  { TMRD }

  TDataSupport = record
    Responses : integer;
    Latency,
    StmBegin,
    StmEnd : Extended;

    {
      CSQHIT occurs at the trial ending, after the trial is destroyed, see units/blocs.pas IETconsequence,
      so it may not be always contingent to the subject's response
    }
    CSQHIT : string;

    {
      CSQMISS occurs at the trial ending, after the trial is destroyed, see units/blocs.pas IETconsequence,
      so it mey not be always contingent to the subject's response
    }
    CSQMISS : string;

    {
      CSQ2 occurs as soon as the subject's response meets the response schedule, i.e., always contingent.
      CSQ2 is only available for TSchRRRT instances, see units/schedules_main.
    }
    CSQ2 : string;

  end;

  { TFPE }

  TFPE = Class(TTrial)
  private
    FConsequenceFired : Boolean;
    FCurrTrial: TCurrentTrial;
    FDataSupport : TDataSupport;
    FFirstResp : Boolean;
    FNumComp : integer;
    FSchedule : TSchMan;
    FUseMedia : Boolean;
    procedure TrialPaint;
    procedure Consequence(Sender: TObject);
    procedure Response(Sender: TObject);
    procedure TrialBeforeEnd(Sender: TObject);
    procedure TrialStart(Sender: TObject);
    procedure TrialResult(Sender: TObject);
    procedure TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected { TTrial }
    procedure WriteData(Sender: TObject); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Play(ACorrection : Boolean); override;
    //procedure DispenserPlusCall; override;

  end;

implementation

uses constants, timestamps;

{ TFPE }


constructor TFPE.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialKeyUp := @TrialKeyUp;
  OnTrialStart := @TrialStart;
  OnTrialPaint := @TrialPaint;

  Header :=  Header +
             'StmBegin' + #9 +
             '_Latency' + #9 +
             '__StmEnd' + #9 +
             'RespFreq' + #9 +
             'ExpcResp' + #9 +
             '__Result';

  FDataSupport.Responses:= 0;
end;

procedure TFPE.TrialResult(Sender: TObject);
begin
  //FDataSupport.StmDuration := GetCustomTick;
  LogEvent('C');

  if FConsequenceFired then
    begin
      if FCurrTrial.response = 'Positiva' then  Result := T_HIT else Result := T_MISS;
      IETConsequence := FDataSupport.CSQHIT;
    end
  else
    begin
      if FCurrTrial.response = 'Negativa' then  Result := T_HIT else Result := T_MISS;
      IETConsequence := FDataSupport.CSQMISS;
    end;

  if FCurrTrial.NextTrial = T_CRT then NextTrial := T_CRT
  else NextTrial := FCurrTrial.NextTrial;

  FCurrTrial.Result := Result;
end;

procedure TFPE.TrialBeforeEnd(Sender: TObject);
begin
  FDataSupport.StmEnd := TickCount;
  TrialResult(Sender);
  WriteData(Sender);
end;

procedure TFPE.Consequence(Sender: TObject);
var
  LConsequence : TKey;
begin
  if FConsequenceFired = False then FConsequenceFired := True;
  LConsequence := TKey.Create(Self);
  with LConsequence do
    begin
      Cursor:= Self.Cursor;
      Parent:= Self;
      Loops := 0;
      Color := 255;
      FullPath := RootMedia + FDataSupport.CSQ2;
    end;
  LConsequence.Play;
  if Assigned(CounterManager.OnConsequence) then CounterManager.OnConsequence(Self);
end;

procedure TFPE.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 32 then
    begin
      //if FUseMedia then ... not implemented yet
      LogEvent('R');
      FSchedule.DoResponse;
    end;
end;


procedure TFPE.TrialPaint;
var i : integer;
begin
  if not FUseMedia then
    for i := Low(FCurrTrial.C) to High(FCurrTrial.C) do
      with FCurrTrial.C[i] do
        DrawCircle(Canvas, o.X, o.Y, size, gap, gap_degree, gap_length);
end;

procedure TFPE.Play(ACorrection: Boolean);
var
  s1: string;
  R : TRect;
  a1 : Integer;

  procedure NextCommaDelimitedParameter;
  begin
    Delete(s1, 1, pos(#44, s1));
    if Length(s1) > 0 then while s1[1] = #44 do Delete(s1, 1, 1);
  end;

  {
  procedure KeyConfig(var aKey : TKey);
  begin
    with aKey do
      begin
        BoundsRect := R;
        //Color := StrToIntDef(sColor, $0000FF {clRed} );
        //HowManyLoops:= StrToIntDef(sLoop, 0);
        //FullPath:= sName;
        Schedule.Kind:= CfgTrial.SList.Values[_Schedule];
        //Visible := False;
      end;
  end;
}
begin
  inherited Play(ACorrection);

  FUseMedia := StrToBoolDef(CfgTrial.SList.Values[_UseMedia], False);
  FNumComp := StrToIntDef(CfgTrial.SList.Values[_NumComp], 1);

  if FUseMedia then
    // not implemented yet
  else
    begin
      SetLength(FCurrTrial.C, FNumComp);
    end;

  FSchedule := TSchMan.Create(self);
  with FSchedule do
    begin
      OnConsequence := @Consequence;
      OnResponse:= @Response;
      Kind := CfgTrial.SList.Values[_Schedule];
      Enabled := False;
    end;
  AddToClockList(FSchedule);
  // allow user defined differential consequences
  // we expect something like:

  // PositiveHIT, PositiveMISS, PositiveCSQ2
  // NegativeHIT, NegativeMISS, NegativeCSQ2

  // Positive
  // NONE,MISS,HIT

  // Negative
  // NONE,HIT,MISS

  // Custom
  // S1.wav 0 -1 1000,S2.wav 0 -1 1000, S3.wav 0 -1 1000,

  s1 := CfgTrial.SList.Values[_Trial + _cIET] + #44;

  FDataSupport.CSQHIT := Copy(s1, 0, pos(#44, s1) - 1);
  NextCommaDelimitedParameter;

  FDataSupport.CSQMISS := Copy(s1, 0, pos(#44, s1) - 1);
  NextCommaDelimitedParameter;

  FDataSupport.CSQ2 := Copy(s1, 0, pos(#44, s1) - 1);

  // Alias to a default media name.ext
  FCurrTrial.response := CfgTrial.SList.Values[_ExpectedResponse];
  if FCurrTrial.response = 'Positiva' then
    if FDataSupport.CSQ2 = T_HIT then FDataSupport.CSQ2 := 'CSQ1.wav';

  if FCurrTrial.response = 'Negativa' then
    if FDataSupport.CSQ2 = T_MISS then FDataSupport.CSQ2 := 'CSQ2.wav';


  FCurrTrial.Result := T_NONE;

  for a1 := 0 to FNumComp -1 do
    begin
        s1:= CfgTrial.SList.Values[_Comp + IntToStr(a1 + 1) + _cBnd];

        R.Left:= StrToIntDef(Copy(s1, 0, pos(#32, s1) - 1), 0);
        NextSpaceDelimitedParameter(s1);

        R.Top:= StrToIntDef(Copy(s1, 0, pos(#32, s1) - 1), 0);
        NextSpaceDelimitedParameter(s1);

        R.Right := StrToIntDef(s1, 100);
        R.Bottom := R.Right;

       if FUseMedia then //allow mouse input
         begin
           //not implemented yet
           {s1:= CfgTrial.SList.Values[_Comp + IntToStr(a1+1)+_cStm] + #32;

           sName := RootMedia + Copy(s1, 0, pos(#32, s1)-1);
           NextSpaceDelimitedParameter(s1);

           sLoop := Copy(s1, 0, pos(#32, s1)-1);
           NextSpaceDelimitedParameter(s1);

           sColor := s1;

           if a1 = 0 then KeyConfig(FKey1) else KeyConfig(FKey2)}

         end
       else  // keyboard input only
          with FCurrTrial.C[a1] do
            begin
              o := Point( R.Top, R.Left );
              size := R.Right;
              gap := StrToBoolDef( CfgTrial.SList.Values[_Comp + IntToStr(a1+1) + _cGap], False );
              if gap then
                  begin
                    gap_degree := StrToIntDef( CfgTrial.SList.Values[_Comp + IntToStr(a1 + 1) + _cGap_Degree], Random(360));
                    gap_length := StrToIntDef( CfgTrial.SList.Values[_Comp + IntToStr(a1 + 1) + _cGap_Length], 5 );
                  end;
            end;
      end;

  if Self.ClassType = TFPE then Config(Self);
end;

procedure TFPE.TrialStart(Sender: TObject);
begin
  {
  if FUseMedia then
    begin
      // not implemented yet
    end;
  }

  FConsequenceFired := False;
  FDataSupport.Latency := TimeStart;
  FDataSupport.StmBegin := TickCount;
end;

procedure TFPE.WriteData(Sender: TObject);  //
var LLatency : string;
begin
  inherited WriteData(Sender);

  if FDataSupport.Latency = TimeStart then
    LLatency := 'NA'
  else LLatency := TimestampToStr(FDataSupport.Latency - TimeStart);

  Data :=  Data +
           TimestampToStr(FDataSupport.StmBegin - TimeStart) + #9 +
           LLatency + #9 +
           TimestampToStr(FDataSupport.StmEnd - TimeStart) + #9 +
           Format('%-*.*d', [4,8, FDataSupport.Responses]) + #9 +
           FCurrTrial.response + #9 +
           FCurrTrial.Result;

  if Assigned(OnTrialWriteData) then OnTrialWriteData(Self);
end;

procedure TFPE.Paint;
begin
  inherited Paint;
end;

procedure TFPE.Response(Sender: TObject);
begin
  Inc(FDataSupport.Responses);
  if FDataSupport.Latency = TimeStart then
      FDataSupport.Latency := TickCount;

  // not implemented yet
  //if FUseMedia then
  //  if Sender is TKey then TKey(Sender).IncCounterResponse
  //    else;

  if Assigned(CounterManager.OnStmResponse) then CounterManager.OnStmResponse(Sender);
  if Assigned(OnStmResponse) then OnStmResponse (Self);
end;

end.

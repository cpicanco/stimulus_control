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
    , constants
    //, client
    , draw_methods
    , schedules_main
    , response_key
    , timestamps
    ;

type

  { TMRD }

  TDataSupport = record
    Responses : integer;
    StarterLatency,
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
    FResponseEnabled : Boolean;
    FSchedule : TSchMan;
    FShowStarter : Boolean;
    FUseMedia : Boolean;
    procedure BeginCorrection(Sender: TObject);
    procedure BeginStarter;
    procedure Consequence(Sender: TObject);
    procedure EndCorrection (Sender: TObject);
    procedure Hit(Sender: TObject);
    procedure Miss(Sender: TObject);
    procedure None(Sender: TObject);
    procedure Response(Sender: TObject);
    procedure TrialResult(Sender: TObject);
  private { TCustomControl }
    procedure TrialKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected { TTrial }
    procedure BeforeEndTrial(Sender: TObject); override;
    procedure StartTrial(Sender: TObject); override;
    procedure WriteData(Sender: TObject); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Play(TestMode: Boolean; Correction : Boolean); override;
    //procedure DispenserPlusCall; override;

  end;

implementation


{ TFPE }


constructor TFPE.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnBeforeEndTrial := @BeforeEndTrial;
  OnKeyDown := @TrialKeyDown;
  OnKeyUp := @TrialKeyUp;

  Header :=  'StartLat' + #9 +
             'StmBegin' + #9 +
             '_Latency' + #9 +
             '__StmEnd' + #9 +
             'RespFreq' + #9 +
             'ExpcResp' + #9 +
             '__Result'
             //'__DBegin' + #9 +
             //'DLatency' + #9 +
             ;

  FDataSupport.Responses:= 0;
end;

procedure TFPE.TrialResult(Sender: TObject);
begin
  LogEvent('C');

  //FDataSupport.StmDuration := GetCustomTick;

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

  if Result = T_HIT then  Hit(Sender);
  if Result = T_MISS then  Miss(Sender);
  if Result = T_NONE then  None(Sender);
end;

procedure TFPE.BeforeEndTrial(Sender: TObject);
begin
  FResponseEnabled := False;
  FDataSupport.StmEnd := TickCount;
  TrialResult(Sender);
  WriteData(Sender);
end;

procedure TFPE.Consequence(Sender: TObject);
var
  aConsequence : TKey;

begin
  if FConsequenceFired = False then FConsequenceFired := True;
  aConsequence := TKey.Create(Self);
  aConsequence.Cursor:= Self.Cursor;
  aConsequence.Parent:= Self;
  //aConsequence.OnConsequence2:=@Consequence2;
  //aConsequence.OnConsequence:= @Consequence;
  //aConsequence.OnResponse:= @Response;
  aConsequence.HowManyLoops := 0;
  aConsequence.Color := 255;
  aConsequence.FullPath := RootMedia + FDataSupport.CSQ2;
  aConsequence.Play;
  if Assigned(CounterManager.OnConsequence) then CounterManager.OnConsequence(Self);
  //aConsequence.FullScreen;
end;


procedure TFPE.TrialKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 27 {ESC}) and (FResponseEnabled = True) then
    begin
      FResponseEnabled:= False;
      Invalidate;
    end;

   if (ssCtrl in shift) and (Key = 81) {q} then
      begin
        Data := Data + LineEnding + '(Sessão cancelada)' + #9#9#9#9#9#9#9#9#9 + LineEnding;
        Result := 'NONE';
        IETConsequence := 'NONE';
        NextTrial := 'END';
        EndTrial(Self);
      end;
end;


procedure TFPE.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var LTickCount : Extended;
begin
  LTickCount := TickCount;

  if Key = 27 {ESC} then
    begin
      FResponseEnabled:= True;
      Invalidate;
    end;

  if FResponseEnabled then
  begin
    if Key = 32 then
      if FShowStarter then
        begin
          //if FUseMedia then ... not implemented yet
          FDataSupport.StarterLatency := LTickCount;
          LogEvent('*R');
          FShowStarter := False;
          Invalidate;
          StartTrial(Self);
        end
      else
        begin
          //if FUseMedia then ... not implemented yet
          LogEvent('R');
          FSchedule.DoResponse;
        end;
  end;

end;

procedure TFPE.Paint;
var i : integer;
begin
  inherited Paint;
  if FResponseEnabled then
    begin
      if FShowStarter then
        begin
          DrawCenteredCircle (Canvas, Width, Height, 6);
        end
      else
        if FUseMedia then
          begin
            // do nothing, TKey draws itself
            // use of custom TKey was not implemented yet
          end
        else
          begin
            for i := Low(FCurrTrial.C) to High(FCurrTrial.C) do
              with FCurrTrial.C[i] do DrawCircle(Canvas, o.X, o.Y, size, gap, gap_degree, gap_length);
          end;
    end;
end;

procedure TFPE.Play(TestMode: Boolean; Correction : Boolean);
var
  s1{, sName, sLoop, sColor, sGap, sGapDegree, sGapLength} : string;
  R : TRect;
  a1 : Integer;


  procedure NextCommaDelimitedParameter;
  begin
    Delete(s1, 1, pos(#44, s1));
    if Length(s1) > 0 then while s1[1] = #44 do Delete(s1, 1, 1);
  end;

  procedure NextSpaceDelimitedParameter;
  begin
    Delete(s1, 1, pos(#32, s1));
    if Length(s1) > 0 then while s1[1] = #32 do Delete(s1, 1, 1);
  end;

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

begin
  FResponseEnabled:= False;
  NextTrial:= '-1';
  Randomize;

  FUseMedia := StrToBoolDef(CfgTrial.SList.Values[_UseMedia], False);
  FShowStarter := StrToBoolDef(CfgTrial.SList.Values[_ShowStarter], False);
  FLimitedHold := StrToIntDef(CfgTrial.SList.Values[_LimitedHold], 0);
  FNumComp := StrToIntDef(CfgTrial.SList.Values[_NumComp], 1);
  RootMedia := CfgTrial.SList.Values[_RootMedia];

  if FUseMedia then
    // not implemented yet
  else
    begin
      SetLength(FCurrTrial.C, FNumComp);
    end;

  if Correction then FIsCorrection := True
  else FIsCorrection := False;


  Color:= StrToIntDef(CfgTrial.SList.Values[_BkGnd], 0);
  if TestMode then Cursor:= 0
  else Cursor:= StrToIntDef(CfgTrial.SList.Values[_Cursor], 0);

  //self descends from TCustomControl
  FSchedule := TSchMan.Create(self);
  with FSchedule do
    begin
      OnConsequence := @Consequence;
      OnResponse:= @Response;
      Kind := CfgTrial.SList.Values[_Schedule];
      if Loaded then
        begin
          SetLength(FClockList, Length(FClockList) +1);
          FClockList[Length(FClockList) -1] := StartMethod;
        end
      else raise Exception.Create(ExceptionNoScheduleFound);
    end;

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
  FCurrTrial.NextTrial := CfgTrial.SList.Values[_NextTrial];

  for a1 := 0 to FNumComp -1 do
    begin
        s1:= CfgTrial.SList.Values[_Comp + IntToStr(a1 + 1) + _cBnd];

        R.Left:= StrToIntDef(Copy(s1, 0, pos(#32, s1) - 1), 0);
        NextSpaceDelimitedParameter;

        R.Top:= StrToIntDef(Copy(s1, 0, pos(#32, s1) - 1), 0);
        NextSpaceDelimitedParameter;

        R.Right := StrToIntDef(s1, 100);
        R.Bottom := R.Right;

       if FUseMedia then
         begin
           //not implemented yet
           {s1:= CfgTrial.SList.Values[_Comp + IntToStr(a1+1)+_cStm] + #32;

           sName := RootMedia + Copy(s1, 0, pos(#32, s1)-1);
           NextSpaceDelimitedParameter;

           sLoop := Copy(s1, 0, pos(#32, s1)-1);
           NextSpaceDelimitedParameter;

           sColor := s1;

           if a1 = 0 then KeyConfig(FKey1) else KeyConfig(FKey2)}

         end
       else
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

  if FShowStarter then BeginStarter else StartTrial(Self);
  //showmessage(BoolToStr(FShowStarter));
end;

procedure TFPE.StartTrial(Sender: TObject);
//var a1 : integer;
  {
  procedure KeyStart(var aKey : TKey);
  begin
    aKey.Play;
    aKey.Visible:= True;
  end;
  }
begin
  if FIsCorrection then
    begin
      BeginCorrection(Self);
    end;
  {
  if FUseMedia then
    begin
      // not implemented yet
    end;
  }

  FConsequenceFired := False;
  FFirstResp := True;
  FResponseEnabled:= True;
  FDataSupport.StmBegin := TickCount;

  inherited StartTrial(Sender);
end;


procedure TFPE.BeginStarter;
begin
  LogEvent('S');
  FResponseEnabled:= True;
  Invalidate;
end;

procedure TFPE.WriteData(Sender: TObject);  //
var Latency : string;
begin
  if not FFirstResp then
    Latency := FloatToStrF(FDataSupport.Latency - TimeStart, ffFixed, 0,9)
  else Latency := 'NA';

  {
  Header :=  'StartLat' + #9 +
             'StmBegin' + #9 +
             '_Latency' + #9 +
             '__StmEnd' + #9 +
             'RespFreq' + #9 +
             'ExpcResp' + #9 +
             '__Result'
  }

  Data :=  FloatToStrF(FDataSupport.StarterLatency - TimeStart, ffFixed, 0,9) + #9 +
           FloatToStrF(FDataSupport.StmBegin - TimeStart, ffFixed, 0,9) + #9 +
           Latency + #9 +
           FloatToStrF(FDataSupport.StmEnd - TimeStart, ffFixed, 0,9) + #9 +
           Format('%-*.*d', [4,8, FDataSupport.Responses]) + #9 +
           FCurrTrial.response + #9 +
           FCurrTrial.Result + #9 +
           Data // forced session end data
           ;
  if Assigned(OnWriteTrialData) then OnWriteTrialData(Self);
end;

procedure TFPE.Response(Sender: TObject);
var LTickCount : Extended;
begin
  LTickCount := TickCount;
  Inc(FDataSupport.Responses);
  if FFirstResp then
    begin
      FDataSupport.Latency := LTickCount;
      FFirstResp := False;
    end
  else;

  // not implemented yet
  //if FUseMedia then
  //  if Sender is TKey then TKey(Sender).IncCounterResponse
  //    else;

  //Invalidate;
  if Assigned(CounterManager.OnStmResponse) then CounterManager.OnStmResponse(Sender);
  if Assigned(OnStmResponse) then OnStmResponse (Self);
end;

procedure TFPE.BeginCorrection(Sender: TObject);
begin
  if Assigned (OnBeginCorrection) then OnBeginCorrection (Sender);
end;

procedure TFPE.EndCorrection(Sender: TObject);
begin
  if Assigned (OnEndCorrection) then OnEndCorrection (Sender);
end;

procedure TFPE.Hit(Sender: TObject);
begin
  if Assigned(OnHit) then OnHit(Sender);
end;

procedure TFPE.Miss(Sender: TObject);
begin
  if Assigned(OnMiss) then OnMiss(Sender);
end;

procedure TFPE.None(Sender: TObject);
begin
  if Assigned(OnNone) then OnNone(Sender);
end;

end.

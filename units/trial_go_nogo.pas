{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit trial_go_nogo;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils, LazFileUtils

    //, counter
    , dialogs
    , config_session
    //, countermanager
    , trial_abstract
    //, custom_timer
    //, client
    , schedules_main
    , response_key
    , bass_player
    ;

type

  { TDataSupport }

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
      CSQ occurs as soon as the subject's response meets the response schedule, i.e., always contingent.
      CSQ is only available for TSchRRRT instances, see units/schedules_main.
    }
    CSQ : string;

  end;

  TGNGCurrTrial = record
    i : integer;
    NextTrial : string;
    Response : string;
    Result : string;
  end;

  { TGNG }

  TGNG = Class(TTrial)
  private
    FConsequenceFired : Boolean;
    FCurrTrial: TGNGCurrTrial;
    FDataSupport : TDataSupport;
    FStimulus : TKey;

    FSchedule : TSchMan;
    procedure TrialPaint;
    procedure Consequence(Sender: TObject);
    procedure Response(Sender: TObject);
    procedure TrialBeforeEnd(Sender: TObject);
    procedure TrialStart(Sender: TObject);
    procedure TrialResult(Sender: TObject);
    procedure TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected { TTrial }
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Play(ACorrection : Boolean); override;
    //procedure DispenserPlusCall; override;

  end;

implementation

uses constants, timestamps;

{ TGNG }


constructor TGNG.Create(AOwner: TComponent);
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

procedure TGNG.TrialResult(Sender: TObject);
begin
  //FDataSupport.StmDuration := GetCustomTick;
  if FConsequenceFired then
    begin
      case UpperCase(FCurrTrial.Response) of
        'POSITIVA': Result := T_HIT;
        'INDIFERENTE': Result := T_NONE;
        else Result := T_MISS;
      end;
      IETConsequence := FDataSupport.CSQHIT;
    end
  else
    begin
      case UpperCase(FCurrTrial.Response) of
        'NEGATIVA': Result := T_HIT;
        'INDIFERENTE': Result := T_NONE;
        else Result := T_MISS;
      end;
      IETConsequence := FDataSupport.CSQMISS;
    end;

  case NextTrial of
    T_CRT:NextTrial := T_CRT;
    'IF_HIT_JUMP_NEXT_TRIAL':
        if Result = T_HIT then
          NextTrial := IntToStr(GlobalContainer.CounterManager.CurrentTrial+3);
  end;

  LogEvent(Result);
  FCurrTrial.Result := Result;
end;

procedure TGNG.TrialBeforeEnd(Sender: TObject);
begin
  FDataSupport.StmEnd := TickCount;
  TrialResult(Sender);
  WriteData(Sender);
end;

procedure TGNG.Consequence(Sender: TObject);
var
  LSound : TBassStream;
  LSoundFile : string;
  LPopUp : TKey;
  LPopUpFile : string;
begin
  LogEvent(LeftStr(FDataSupport.CSQ, 4));
  if FConsequenceFired = False then FConsequenceFired := True;
  LPopUpFile := '';
  LSoundFile := '';

  case FDataSupport.CSQ of
    T_HIT :
      begin
        LSoundFile := RootMedia+'CSQ1.wav';
        LPopUpFile := RootMedia+'CSQ1.png';
      end;
    T_MISS :
      begin
        LSoundFile := RootMedia+'CSQ2.wav';
        LPopUpFile := RootMedia+'CSQ2.png';
      end;
    else
      begin
        LSoundFile := RootMedia+FDataSupport.CSQ;
        LPopUpFile := ExtractFileNameOnly(RootMedia+FDataSupport.CSQ);
        LPopUpFile := RootMedia+LPopUpFile+'.png';
      end
  end;

  if FileExists(LSoundFile) then
    begin
      LSound := TBassStream.Create(LSoundFile);
      LSound.Play;
    end;

  if FileExists(LPopUpFile) then
    begin
      LPopUp := TKey.Create(Parent);
      with LPopUp do
        begin
          Top := 0;
          Left := 0;
          Width := 200;
          Height := 200;
          FullPath := LPopUpFile;
          Parent := TCustomControl(Owner);
          Show;
          AutoDestroyIn(1000);
        end;

    end;

  if Assigned(CounterManager.OnConsequence) then CounterManager.OnConsequence(Self);
end;

procedure TGNG.TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 32 then
    begin
      LogEvent('R');
      FSchedule.DoResponse;
    end;
end;


procedure TGNG.TrialPaint;
begin
 // do nothing, FStimulus paints it self;
end;

procedure TGNG.Play(ACorrection: Boolean);
var
  s1
  , LColor
  , LLoop
  , LName, LWidth, LHeight: string;
  procedure NextCommaDelimitedParameter;
  begin
    Delete(s1, 1, pos(#44, s1));
    if Length(s1) > 0 then while s1[1] = #44 do Delete(s1, 1, 1);
  end;
begin
  inherited Play(ACorrection);
  {
    Minimum configs

    Name=	Positiva 1
    Kind=	GNG
    Cursor=	-1

    LimitedHold=	2000
    Schedule=	FR 3 0
    Consequence=	Positiva
    # Consequence=	Negativa

    C1Stm=A1.png
  }
  s1:= CfgTrial.SList.Values[_Comp + IntToStr(1) +_cStm] + #32;
  LName := RootMedia + Copy(s1, 0, pos(#32, s1)-1);
  NextSpaceDelimitedParameter(s1);
  LColor := Copy(s1, 0, pos(#32, s1)-1);
  NextSpaceDelimitedParameter(s1);
  LLoop := s1;

  s1:= CfgTrial.SList.Values[_Comp + IntToStr(1) +_cBnd] + #32;
  LWidth := Copy(s1, 0, pos(#32, s1)-1);
  NextSpaceDelimitedParameter(s1);
  LHeight := Copy(s1, 0, pos(#32, s1)-1);

  FStimulus := TKey.Create(Parent);
  with FStimulus do
    begin
      Width := StrToIntDef(LWidth,300);
      Height:= StrToIntDef(LHeight,Width);
      Color := StrToIntDef(LColor, $FFFFFF); //clWhite
      Loops:= StrToIntDef(LLoop, 0);
      FullPath:= LName;
      //Schedule.Kind:= CfgTrial.SList.Values[_Schedule];
      Visible := False;
      Parent := TCustomControl(Owner);
    end;

  FSchedule := TSchMan.Create(Self);
  with FSchedule do
    begin
      OnConsequence := @Consequence;
      OnResponse:= @Response;
      Kind := CfgTrial.SList.Values[_Schedule];
      Enabled := False;
    end;
  AddToClockList(FSchedule);

  FCurrTrial.response := CfgTrial.SList.Values[_Consequence];
  if UpperCase(FCurrTrial.Response) = 'POSITIVA' then
    begin
      FDataSupport.CSQHIT := 'NONE';
      FDataSupport.CSQMISS := 'MISS';
      FDataSupport.CSQ := 'HIT';
    end;

  if UpperCase(FCurrTrial.Response) = 'NEGATIVA' then
    begin
      FDataSupport.CSQHIT := 'NONE';
      FDataSupport.CSQMISS := 'HIT';
      FDataSupport.CSQ := 'MISS';
    end;

  FCurrTrial.Result := T_NONE;

  if Self.ClassType = TGNG then Config(Self);
end;

procedure TGNG.TrialStart(Sender: TObject);
begin
  FStimulus.Centralize;
  FStimulus.Show;
  FConsequenceFired := False;
  FDataSupport.Latency := TimeStart;
  FDataSupport.StmBegin := TickCount;
end;

procedure TGNG.WriteData(Sender: TObject);  //
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
           FCurrTrial.Response + #9 +
           FCurrTrial.Result;

  if Assigned(OnTrialWriteData) then OnTrialWriteData(Self);
end;

procedure TGNG.Response(Sender: TObject);
begin
  Inc(FDataSupport.Responses);
  if FDataSupport.Latency = TimeStart then
      FDataSupport.Latency := TickCount;

  if Assigned(CounterManager.OnStmResponse) then CounterManager.OnStmResponse(Sender);
  if Assigned(OnStmResponse) then OnStmResponse (Self);
end;

end.

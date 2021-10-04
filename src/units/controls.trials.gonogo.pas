{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.GoNoGo;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils, LazFileUtils

  , Controls.Trials.Abstract
  , Controls.Trials.Helpers
  , Schedules
  , Controls.Stimuli.Key
  //, Audio.Bass_nonfree
  ;

type

  { TGNG }

  TGNG = Class(TTrial)
  private
    FPresentConsequenceJustOnce : Boolean;
    FGoNoGoStyle : TGoNoGoStyle;
    FConsequenceFired : Boolean;
    FDataSupport : TDataSupport;
    FStimulus : TKey;
    //FSound : TBassStream;
    FPopUpTime : integer;
    FSchedule : TSchedule;
    FContingency : string;
    procedure TrialPaint;
    procedure Consequence(Sender: TObject);
    procedure Response(Sender: TObject);
    procedure TrialBeforeEnd(Sender: TObject);
    procedure TrialStart(Sender: TObject);
    procedure TrialResult(Sender: TObject);
    procedure PlayConsequence;
    procedure TrialKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    { TTrial }
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TCustomControl); override;
    destructor Destroy; override;
    procedure Play(ACorrection : Boolean); override;
    //procedure DispenserPlusCall; override;

  end;

implementation

uses strutils, constants, Timestamps, Session.Configuration.GlobalContainer;

{ TGNG }

constructor TGNG.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialKeyUp := @TrialKeyUp;
  OnTrialStart := @TrialStart;
  OnTrialPaint := @TrialPaint;

  Header :=  Header + #9 +
             rsReportStmBeg + #9 +
             rsReportRspLat + #9 +
             rsReportStmEnd + #9 +
             rsReportRspFrq + #9 +
             rsReportRspExp;

  FDataSupport.Responses:= 0;
  FContingency := '';
end;

destructor TGNG.Destroy;
begin
  //if Assigned(FSound) then
  //  FSound.Free;
  inherited Destroy;
end;

procedure TGNG.TrialResult(Sender: TObject);
begin
  if Result = T_NONE then
    begin
      if FConsequenceFired then
        case UpperCase(FContingency) of
          'POSITIVA': Result := T_HIT;
          'NEGATIVA': Result := T_MISS;
        end
      else
        case UpperCase(FContingency) of
          'POSITIVA': Result := T_MISS;
          'NEGATIVA': Result := T_HIT;
        end;

      case NextTrial of
        T_CRT:NextTrial := T_CRT;
        'IF_HIT_JUMP_NEXT_TRIAL':
            if Result = T_HIT then
              NextTrial := IntToStr(Counters.CurrentTrial+3);
      end;
    end;
end;

procedure TGNG.PlayConsequence;
var
  LSoundFile : string;
  LPopUp : TKey;
  LPopUpFile : string;
begin
  case Result of
    T_HIT  :
      begin
        LSoundFile := RootMedia+'CSQ1.wav';
        LPopUpFile := RootMedia+'CSQ1.png';
      end;
    T_MISS :
      begin
        LSoundFile := RootMedia+'CSQ2.wav';
        LPopUpFile := RootMedia+'CSQ2.png';
      end;
    //else
    //  begin
    //    LSoundFile := RootMedia+FDataSupport.CSQ;
    //    LPopUpFile := ExtractFileNameOnly(RootMedia+FDataSupport.CSQ);
    //    LPopUpFile := RootMedia+LPopUpFile+'.png';
    //  end
  end;

  if FileExists(LSoundFile) then
    begin
      LogEvent('CS');
      //if Assigned(FSound) then
      //  begin
      //    FSound.Free;
      //    FSound := TBassStream.Create(LSoundFile);
      //  end
      //else
      //  FSound := TBassStream.Create(LSoundFile);
      //FSound.Play;
    end;

  if FileExists(LPopUpFile) then
    begin
      LogEvent('CI');
      LPopUp := TKey.Create(Parent);
      with LPopUp do
        begin
          Top := FStimulus.Top;
          Left := FStimulus.Left+FStimulus.Width+5;
          Width := 200;
          Height := 200;
          Filename := LPopUpFile;
          Parent := TCustomControl(Owner);
          Show;
          AutoDestroyIn(FPopUpTime);
        end;
    end;
end;

procedure TGNG.TrialBeforeEnd(Sender: TObject);
begin
  FDataSupport.StmEnd := TickCount;
  TrialResult(Sender);
  if gngPlayGoOnBeforeEnd in FGoNoGoStyle then
    begin
      if FConsequenceFired then
        PlayConsequence;
    end
  else
    begin
      if gngPlayNoGo in FGoNoGoStyle then
        if not FConsequenceFired then
          PlayConsequence;
    end;

  LogEvent(Result);
  WriteData(Sender);
end;

procedure TGNG.Consequence(Sender: TObject);
  procedure DoConsequence;
  begin
    FConsequenceFired := True;
    TrialResult(Sender);

    if gngPlayGoOnBeforeEnd in FGoNoGoStyle then
      // do nothing
    else
      if gngPlayGo in FGoNoGoStyle then
        PlayConsequence;

    if Assigned(Counters.OnConsequence) then Counters.OnConsequence(Self);
  end;

begin
  if FPresentConsequenceJustOnce then
    begin
      if not FConsequenceFired then
        DoConsequence;
    end
  else
    DoConsequence;
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
  , LName, LWidth, LHeight
  , LPopUpTime : string;
  i : integer;
  Parameters : TStringList;
begin
  inherited Play(ACorrection);
  {
    Minimum configs

    Name=	Positiva 1
    Kind=	GNG
    Cursor=	-1

    Style= GO NOGO
    PopUpTime= 1000
    LimitedHold=	2000
    Schedule=	FR 3 0
    Consequence=	Positiva
    # Consequence=	Negativa

    C1Stm=A1.png
  }
  Parameters := Configurations.Parameters;
  FPresentConsequenceJustOnce := StrToBoolDef(Parameters.Values[_PresentConsequenceJustOnce],True);
  FGoNoGoStyle := [];
  s1 := Parameters.Values[_Style];
  for i := 1 to WordCount(s1,[#32]) do
    FGoNoGoStyle += [StringToStyle(ExtractDelimited(i,s1,[#32]))];

  if FGoNoGoStyle = [] then
    FGoNoGoStyle := [gngPlayGo,gngPlayNoGo];

  s1 := Parameters.Values[_PopUpTime] + #32;
  LPopUpTime := ExtractDelimited(1,s1,[#32]);
  FPopUpTime := StrToIntDef(LPopUpTime,1000);

  s1:= Parameters.Values[_Comp + IntToStr(1) +_cStm] + #32;
  LName := RootMedia + ExtractDelimited(1,s1,[#32]);
  LColor := ExtractDelimited(2,s1,[#32]);
  LLoop := s1;

  s1:= Parameters.Values[_Comp + IntToStr(1) +_cBnd] + #32;
  LWidth := ExtractDelimited(1,s1,[#32]);
  LHeight := ExtractDelimited(2,s1,[#32]);

  FStimulus := TKey.Create(Self);
  with FStimulus do
    begin
      Width := StrToIntDef(LWidth,300);
      Height:= StrToIntDef(LHeight,Width);
      Color := StrToIntDef(LColor, $FFFFFF); //clWhite
      Loops:= StrToIntDef(LLoop, 0);
      Filename:= LName;
      //Schedule.Kind:= CfgTrial.SList.Values[_Schedule];
      Parent := TCustomControl(Self.Parent);
    end;

  FSchedule := TSchedule.Create(Self);
  with FSchedule do
    begin
      OnConsequence := @Consequence;
      OnResponse:= @Response;
      Load(Parameters.Values[_Schedule]);
      Enabled := False;
    end;
  //AddToClockList(FSchedule);

  FContingency := Parameters.Values[_Consequence];
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

procedure TGNG.WriteData(Sender: TObject);
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
           FContingency;

  //if Assigned(OnTrialWriteData) then OnTrialWriteData(Self);
end;

procedure TGNG.Response(Sender: TObject);
begin
  Inc(FDataSupport.Responses);
  if FDataSupport.Latency = TimeStart then
    FDataSupport.Latency := TickCount;

  if Assigned(Counters.OnStmResponse) then Counters.OnStmResponse(Sender);
  if Assigned(OnStmResponse) then OnStmResponse (Self);
end;

end.

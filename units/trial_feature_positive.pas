{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit trial_feature_positive;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils, ExtCtrls, Graphics

    //, counter
    , dialogs
    , config_session
    //, countermanager
    , trial_abstract
    //, custom_timer
    //, client
    , draw_methods
    , schedules_main
    //, response_key
    , bass_player
    ;

type

  { TDataSupport }

  TFPEDrawingType = (fpeClearCircles, fpeFullOuterInnerCircles);

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
      so it may not be always contingent to the subject's response
    }
    CSQMISS : string;

    {
      CSQ occurs as soon as the subject's response meets the response schedule, i.e., always contingent.
      CSQ is only available for TSchRRRT instances, see units/schedules_main.
    }
    CSQ : string;

  end;

  { TFPE }

  TFPE = Class(TTrial)
  private
    FFPEDrawingType : TFPEDrawingType;
    FConsequenceFired : Boolean;
    FCurrTrial: TCurrentTrial;
    FDataSupport : TDataSupport;
    //FFirstResp : Boolean;
    FNumComp : integer;
    FSchedule : TSchedule;
    FForeground : TBitmap;
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
    destructor Destroy; override;
    procedure Play(ACorrection : Boolean); override;
    //procedure DispenserPlusCall; override;
  end;

implementation

uses background, strutils, constants, timestamps;

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

  FForeGround := TBitmap.Create;
  FDataSupport.Responses:= 0;
end;

destructor TFPE.Destroy;
begin
  FForeGround.Free;
  inherited Destroy;
end;

procedure TFPE.TrialResult(Sender: TObject);
begin
  //FDataSupport.StmDuration := GetCustomTick;
  if FConsequenceFired then
    case UpperCase(FCurrTrial.response) of
      'POSITIVA':
        begin
          Result := T_HIT;
          IETConsequence := T_HIT;
        end;
      'NEGATIVA':
        begin
          Result := T_MISS;
          IETConsequence := T_MISS;
        end;
      else
        begin
          Result := T_NONE;
          IETConsequence := T_NONE;
        end;
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

procedure TFPE.TrialBeforeEnd(Sender: TObject);
begin
  FDataSupport.StmEnd := TickCount;
  TrialResult(Sender);
  WriteData(Sender);
end;

procedure TFPE.Consequence(Sender: TObject);
var
  LSound : TBassStream;
  LSoundFile : string;
begin
  LogEvent('C');
  if FConsequenceFired = False then FConsequenceFired := True;
  LSoundFile := '';

  case FDataSupport.CSQ of
    T_HIT  : LSoundFile := RootMedia+'CSQ1.wav';
    T_MISS : LSoundFile := RootMedia+'CSQ2.wav';
    else     LSoundFile := RootMedia+FDataSupport.CSQ;
  end;

  if FileExists(LSoundFile) then
    begin
      LSound := TBassStream.Create(LSoundFile);
      LSound.Play;
    end;

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
begin
  Canvas.Draw(0,0,FForeground);
end;

procedure TFPE.Play(ACorrection: Boolean);
var
  s1: string;
  LOuterR , LR: TRect;
  i, LWidth, LHeight : Integer;
begin
  inherited Play(ACorrection);
  FFPEDrawingType := TFPEDrawingType(StrToIntDef(CfgTrial.SList.Values[_DrawingType], 0));
  FNumComp := StrToIntDef(CfgTrial.SList.Values[_NumComp], 1);
  SetLength(FCurrTrial.C, FNumComp);

  FSchedule := TSchedule.Create(self);
  with FSchedule do
    begin
      OnConsequence := @Consequence;
      OnResponse:= @Response;
      Kind := CfgTrial.SList.Values[_Schedule];
      Enabled := False;
    end;
  AddToClockList(FSchedule);

  // Use alias as defaults
  FCurrTrial.response := CfgTrial.SList.Values[_Contingency];
  //if UpperCase(FCurrTrial.Response) = 'POSITIVA' then
  //  begin
  //    FDataSupport.CSQHIT := 'HIT';
  //    FDataSupport.CSQMISS := 'NONE';
  //    // FDataSupport.CSQ := 'NONE';
  //  end;
  //
  //if UpperCase(FCurrTrial.Response) = 'NEGATIVA' then
  //  begin
  //    FDataSupport.CSQHIT := 'NONE';
  //    FDataSupport.CSQMISS := 'MISS';
  //    // FDataSupport.CSQ := 'NONE';
  //  end;

  // allow user defined differential consequences
  //s1 := CfgTrial.SList.Values[_Trial + _cIET] + #44;
  //
  //FDataSupport.CSQHIT := Copy(s1, 0, pos(#44, s1) - 1);
  //NextCommaDelimitedParameter;
  //
  //FDataSupport.CSQMISS := Copy(s1, 0, pos(#44, s1) - 1);
  //NextCommaDelimitedParameter;
  //
  //FDataSupport.CSQ := Copy(s1, 0, pos(#44, s1) - 1);
  //
  //// Alias to a default media name.ext
  //FCurrTrial.response := CfgTrial.SList.Values[_Consequence];
  //if UpperCase(FCurrTrial.response) = 'POSITIVA' then
  //  if FDataSupport.CSQ = T_HIT then FDataSupport.CSQ := 'CSQ1.wav';
  //
  //if UpperCase(FCurrTrial.response) = 'NEGATIVA' then
  //  if FDataSupport.CSQ = T_MISS then FDataSupport.CSQ := 'CSQ2.wav';

  FCurrTrial.Result := T_NONE;

  for i := 0 to FNumComp -1 do
    begin
        s1:= CfgTrial.SList.Values[_Comp + IntToStr(i + 1) + _cBnd];
        LOuterR.Top:= StrToInt(ExtractDelimited(1,s1,[#32]));
        LOuterR.Left:= StrToInt(ExtractDelimited(2,s1,[#32]));
        LWidth := StrToInt(ExtractDelimited(3,s1,[#32]));
        LOuterR.Right := LOuterR.Left + LWidth;
        LHeight := StrToInt(ExtractDelimited(4,s1,[#32]));
        LOuterR.Bottom := LOuterR.Top + LHeight;

        with FCurrTrial.C[i] do
          begin
            OuterRect := LOuterR;
            case FFPEDrawingType of
              fpeClearCircles: {do nothing};
              fpeFullOuterInnerCircles:InnerRect := GetInnerRect(LOuterR, LWidth, LHeight);
            end;
            gap := StrToBoolDef(CfgTrial.SList.Values[_Comp + IntToStr(i+1) + _cGap], False );
            gap_degree := 16 * StrToIntDef(CfgTrial.SList.Values[_Comp + IntToStr(i + 1) + _cGap_Degree], 1+Random(360));
            gap_length := 16 * StrToIntDef(CfgTrial.SList.Values[_Comp + IntToStr(i + 1) + _cGap_Length], 5 );
          end;
      end;

  if not FrmBackground.DrawMask then
    FrmBackground.DrawMask:=True;
  FrmBackground.UpdateMask;

  FForeground := TBitmap.Create;
  FForeground.Width:= Width;
  FForeground.Height:= Height;
  FForeground.TransparentColor:=clFuchsia;
  FForeground.Transparent:=True;
  LR := Rect(0,0,Width, Height);
  with FForeground.Canvas do
    begin
      // Transparent part of foreground
      Pen.Color:=clFuchsia;
      Brush.Style:=bsSolid;
      Brush.Color:=clFuchsia;
      Rectangle(LR);
    end;

  // FForeground do not change, we need to draw only once
  for i := Low(FCurrTrial.C) to High(FCurrTrial.C) do
    with FCurrTrial.C[i] do
      case FFPEDrawingType of
        fpeClearCircles:
          DrawCustomEllipse(FForeground.Canvas, OuterRect, gap, gap_degree, gap_length);
        fpeFullOuterInnerCircles:
          DrawCustomEllipse(FForeground.Canvas, OuterRect, InnerRect, gap, gap_degree, gap_length);
      end;

  if Self.ClassType = TFPE then Config(Self);
end;

procedure TFPE.TrialStart(Sender: TObject);
begin
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

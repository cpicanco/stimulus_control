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
unit blocs;

{$mode objfpc}{$H+}

interface

uses Classes, Controls, LCLIntf, LCLType,
     ExtCtrls, SysUtils, Graphics, Forms,
     Dialogs,StdCtrls

     , config_session
     , regdata
     , constants
     , response_key
     , countermanager
     , custom_timer
     , trial_abstract

        , trial_message
        , trial_simple
          , trial_mirrored_stm
          , trial_feature_positive
          , trial_calibration
        , trial_matching
     ;

type

  TFakeTimer = record
    Interval : LongInt;
    Enabled : Boolean;
  end;

  { TBlc }

  TBlc = class(TComponent)
  private
    //FOnBeginTrial: TNotifyEvent;
    //FClientThread : TClientThread;

    FBlcHeader: String;
    FLastHeader: String;
    FNextBlc: String;

    // session/global parameters
    FServerAddress: string;
    FShowCounter : Boolean;
    FTestMode: Boolean;
    FIsCorrection : Boolean;

    // Trial data
    FData : string;
    FTimeStart: cardinal;
    FFirstTrialBegin: cardinal;
    FITIBegin : cardinal;
    FITIEnd : cardinal;
    FDataTicks : string;

    // Clock System
    FTimer : TClockThread;
    FTimerCsq : TFakeTimer;
    FTimerITI: TFakeTimer;
    FTimerTO : TFakeTimer;

    // main objects/components
    FBackGround: TWinControl;
    FBlc: TCfgBlc;
    FTrial: TTrial;
    FRegData: TRegData;
    FRegDataTicks: TRegData;
    FCounterManager : TCounterManager;
    FCounterLabel : TLabel;
    FIETMedia : TKey;

    // events
    FOnBeginCorrection : TNotifyEvent;
    FOnBkGndResponse: TNotifyEvent;
    FOnConsequence: TNotifyEvent;
    FOnCriteria: TNotifyEvent;
    FOnEndBlc: TNotifyEvent;
    FOnEndCorrection : TNotifyEvent;
    FOnEndTrial: TNotifyEvent;
    FOnHit: TNotifyEvent;
    FOnMiss: TNotifyEvent;
    FOnStmResponse: TNotifyEvent;
    procedure CreateIETMedia(FileName, HowManyLoops, Color : String);
    procedure DebugStatus(msg : string);
    procedure PlayTrial;
    //procedure ShowCounterPlease (Kind : String);
    // events
    procedure BkGndResponse(Sender: TObject);
    procedure EndBlc(Sender: TObject);
    procedure Hit(Sender: TObject);
    procedure IETConsequence(Sender: TObject);
    procedure IETResponse(Sender: TObject);
    procedure Miss(Sender: TObject);
    procedure StmResponse(Sender: TObject);
    procedure ClockThread(Sender: Tobject);
    procedure TrialTerminate(Sender: TObject);
    procedure WriteTrialData(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Play(CfgBlc: TCfgBlc; Manager : TCountermanager; IndTent: Integer; TestMode: Boolean);
    property RegDataTicks: TRegData read FRegDataTicks write FRegDataTicks;
    property BackGround: TWinControl read FBackGround write FBackGround;
    property NextBlc: String read FNextBlc write FNextBlc;
    property RegData: TRegData read FRegData write FRegData;
    property ServerAddress : string read FServerAddress write FServerAddress;
    property ShowCounter : Boolean read FShowCounter write FShowCounter;
    property TimeStart : cardinal read FTimeStart write FTimeStart;
  public
    property OnBeginCorrection: TNotifyEvent read FOnBeginCorrection write FOnBeginCorrection;
    property OnBkGndResponse : TNotifyEvent read FOnBkGndResponse write FOnBkGndResponse;
    property OnConsequence : TNotifyEvent read FOnConsequence write FOnConsequence;
    property OnCriteria: TNotifyEvent read FOnCriteria write FOnCriteria;
    property OnEndBlc: TNotifyEvent read FOnEndBlc write FOnEndBlc;
    property OnEndCorrection: TNotifyEvent read FOnEndCorrection write FOnEndCorrection;
    property OnEndTrial : TNotifyEvent read FOnEndTrial write FOnEndTrial;
    property OnHit: TNotifyEvent read FOnHit write FOnHit;
    property OnMiss: TNotifyEvent read FOnMiss write FOnMiss;
    property OnStmResponse : TNotifyEvent read FOnStmResponse write FOnStmResponse;
  end;

implementation

{$ifdef DEBUG}
uses debug_logger;
{$endif}

procedure TBlc.EndBlc(Sender: TObject);
begin
  FRegData.SaveData(#13#10);

  if Assigned(RegDataTicks) then
    RegDataTicks.SaveData(#13#10);

  if Assigned(OnEndBlc) then FOnEndBlc(Sender);
end;


procedure TBlc.Hit(Sender: TObject);
begin
  FCounterManager.OnHit(Sender);
  if FBlc.CrtKCsqHit > 0 then
    if FBlc.CrtKCsqHit = FCounterManager.BlcCsqHits.Counter then       //Procedimento da Ana Paula, acertos consecutivos produzindo csq
      begin
        FCounterManager.OnCsqCriterion(Sender);
        FTrial.DispenserPlusCall;
      end;
  if Assigned(OnHit) then FOnHit(Sender);
end;

procedure TBlc.IETConsequence(Sender: TObject);
begin
  //
end;

procedure TBlc.IETResponse(Sender: TObject);
begin
  BkGndResponse(Sender);
end;

procedure TBlc.Miss(Sender: TObject);
begin
  FCounterManager.OnMiss(Sender);
  if Assigned(OnMiss) then FOnMiss(Sender);
end;

procedure TBlc.TrialTerminate(Sender: TObject);
var s0, s1, s2, s3, s4 : string;
    csqDuration : Integer;
    //TimestampsData : TRegData;

  procedure NextSpaceDelimitedParameter;
  begin
    Delete(s0, 1, pos(#32, s0));
    if Length(s0) > 0 then while s0[1] = #32 do Delete(s0, 1, 1);
  end;

  procedure SetValuesToStrings (var as1, as2, as3, as4 : string);
  var
      Values : string;
  begin
    if FTrial.IETConsequence = T_HIT then
      begin
        // FileName, HowManyLoops, Color, MediaDuration
        Values := 'CSQ1.wav 1 255 0';

      end
    else
      if FTrial.IETConsequence = T_MISS then
        begin
          // FileName, HowManyLoops, Color, MediaDuration
          Values := 'CSQ2.wav 1 0 0';

        end
      else
        if FTrial.IETConsequence = T_NONE then
          begin
            Values := 'NONE 1 -1 0';
          end
      else Values := FTrial.IETConsequence;

    s0 := Values + #32;
    as1:= FTrial.RootMedia + Copy(s0, 0, pos(#32, s0)-1);
    NextSpaceDelimitedParameter;
    as2:= Copy(s0, 0, pos(#32, s0)-1);
    NextSpaceDelimitedParameter;
    as3:= Copy(s0, 0, pos(#32, s0)-1);
    NextSpaceDelimitedParameter;
    as4:= Copy(s0, 0, pos(#32, s0)-1);
  end;
begin
  FRegData.SaveData (FData);

  if Assigned(RegDataTicks) then
    RegDataTicks.SaveData(FDataTicks);

  FData := '';
  FDataTicks := '';

  s1 := '';
  s2 := '';
  s3 := '';
  s4 := '';

  if  (FTrial.NextTrial = 'END') then //end bloc
    FCounterManager.CurrentTrial.Counter := FBlc.NumTrials
  else // continue
    if (FTrial.NextTrial = 'CRT') or             // FTrial.NextTrial base 1, FCounterManager.CurrentTrial.Counter base 0)
       (FTrial.NextTrial = (IntToStr(FCounterManager.CurrentTrial.Counter + 1))) then
      begin //correction trials were on
        if ((FBlc.MaxCorrection) = FCounterManager.BlcCscCorrections.Counter) and
           (FBlc.MaxCorrection <> 0) then
          begin //correction
            FCounterManager._VirtualTrialFix;
            FCounterManager.OnNotCorrection(Sender);
            FCounterManager.OnEndTrial (Sender);
            FIsCorrection := False;
          end
        else
          begin // not correction
            FCounterManager.OnCorrection(Sender);
            FIsCorrection := True;
          end;
      end
    else  //correction trials were off
      if StrToIntDef(FTrial.NextTrial, 0) > 0 then
        begin //go to the especified trial
          if FTrial.Result = 'MISS' then
            FCounterManager.VirtualTrialLoop.Counter := FCounterManager.VirtualTrialValue;

          FCounterManager.OnNotCorrection(Sender);
          FCounterManager.CurrentTrial.Counter := StrToIntDef(FTrial.NextTrial, 0) - 1;
          FCounterManager.OnNxtTrial (Sender);
          FIsCorrection := False;
        end
      else // go to the next trial,
        begin
          if FTrial.Result = 'MISS' then
            FCounterManager.VirtualTrialLoop.Counter := FCounterManager.VirtualTrialValue;

          FCounterManager.OnNotCorrection(Sender);
          FCounterManager.OnEndTrial (Sender);
          FIsCorrection := False;
        end;

  //Critérios de ACERTO atingido
  if  ((FBlc.CrtConsecutiveHit > 0) and (FBlc.CrtConsecutiveHit = FCounterManager.BlcCscHits.Counter))
   //or ((FCfgBlc.CrtConsecutiveMiss > 0) and (FCfgBlc.CrtConsecutiveMiss = FCounterManager.BlcCscMisses.Counter))
   or ((FTrial.NextTrial = IntToStr(FBlc.CrtMaxTrials)) and (FBlc.CrtMaxTrials > 0))
  then
      begin
        if Assigned(OnCriteria) then FOnCriteria(Sender);
        FCounterManager.CurrentTrial.Counter := FBlc.NumTrials
      end;
  // FileName, HowManyLoops, Color, MediaDuration

  SetValuesToStrings(s1, s2, s3, s4);

  if (StrToIntDef(s4, 0) > 0) then
  begin
    if StrToIntDef(s2, 1) > 1 then csqDuration := StrToIntDef(s4, 0) * StrToIntDef(s2, 1)
    else csqDuration := StrToIntDef(s4, 0);
  end else csqDuration := 0;


  if FTestMode then FTimerITI.Interval:= 0
  else
    begin
      FTimerITI.Interval:= FBlc.ITI;
      FTimerTO.Interval:= FTrial.TimeOut;
      FTimerCsq.Interval:= csqDuration;

      FITIBegin := GetTickCount;
    end;

  if FTrial.TimeOut > 0 then
    if FBackGround is TForm then TForm(FBackGround).Color:= 0;

  if Assigned(OnEndTrial) then FOnEndTrial(Sender);

  FTrial.Hide;

  {$ifdef DEBUG}
    DebugLn(mt_Debug + 'ITI - inter trial interval');
    DebugLn(mt_Debug + '[Blc - ' + FBlc.Name + 'T - ' + IntToStr(FCounterManager.CurrentTrial.Counter)+ ']');
    DebugLn(mt_Debug + 'Timer Intervals -> ITI:' + IntToStr(FTimerITI.Interval) + ' TO:' + IntToStr(FTimerTO.Interval) + 'Csq:' + IntToStr(FTimerCsq.Interval));
  {$endif}

  //SetFocus;

  {

    From now on screen color should be equal to the Bloc background or Black (time out).

  }

  //showmessage(s0 + #13#10 +s1 + #13#10 +s2 + #13#10 +s3 + #13#10);
  if    (FTimerITI.Interval = 0)
    and (FTimerTO.Interval  = 0)
    and (FTimerCsq.Interval = 0) then
   begin
     {$ifdef DEBUG}
       DebugLn(mt_Debug +  'Time Condition 1');
     {$endif}
     PlayTrial;
     Exit;
   end;

  if    (FTimerITI.Interval > 0)
    and (FTimerTO.Interval  > 0)
    and (FTimerCsq.Interval > 0) then
      begin // take ITI and ignore the rest
        {$ifdef DEBUG}
          DebugLn(mt_Debug +  'Time Condition 2');
        {$endif}

        //if ShowCounter then ShowCounterPlease('IET');
        FTimerITI.Enabled := True;
        FTimer := TClockThread.Create(True);
        FTimer.Interval := FTimerITI.Interval;
        FTimer.OnTimer := @ClockThread;
        FTimer.Start;
        Exit;
      end
    else
      begin
        // fixed ITI, time out and consequence off
        if    (FTimerITI.Interval > 0)
          and (FTimerTO.Interval  < 0)
          and (FTimerCsq.Interval < 0) then
            begin  // take ITI and ignore the rest
              {$ifdef DEBUG}
                DebugLn(mt_Debug +  'Time Condition 2');
              {$endif}

              //if ShowCounter then ShowCounterPlease('IET');
              FTimerITI.Enabled:= True;
              FTimer := TClockThread.Create(True);
              FTimer.Interval := FTimerITI.Interval;
              FTimer.OnTimer := @ClockThread;
              FTimer.Start;
              Exit;
            end;

        if  ( (FTimerITI.Interval > 0)
          or  (FTimerCsq.Interval > 0) )

          and (FTimerTO.Interval  = 0) then
            begin
              {$ifdef DEBUG}
                DebugLn(mt_Debug +  'Time Condition 3');
              {$endif}

              CreateIETMedia(s1, s2, s3); // FileName, HowManyLoops, Color
              //BlockInput(True);
              FTimerCsq.Enabled:= True;
              FTimer := TClockThread.Create(True);
              FTimer.Interval := FTimerCsq.Interval;
              FTimer.OnTimer := @ClockThread;
              FTimer.Start;
              Exit;
            end;

        if  ( (FTimerITI.Interval > 0)
          or  (FTimerTO.Interval  > 0) )

          and (FTimerCsq.Interval = 0) then
          begin
            {$ifdef DEBUG}
              DebugLn(mt_Debug +  'Time Condition 4');
            {$endif}

            FTimerTO.Enabled:= True;
            FTimer := TClockThread.Create(True);
            FTimer.Interval := FTimerTO.Interval;
            FTimer.OnTimer := @ClockThread;
            FTimer.Start;
          end;
      end

end;

procedure TBlc.ClockThread(Sender: Tobject);
begin
  if FTimerCsq.Enabled then
    begin
      {$ifdef DEBUG}
        DebugLn(mt_Debug +  'FTimerCsq.Enabled');
      {$endif}
      FTimerCsq.Enabled := False;

      if Assigned(FIETMedia) then
        begin
          FreeAndNil(FIETMedia);
        end;

      if (FTimerITI.Interval > 0) then
        begin
          //if ShowCounter then ShowCounterPlease('IET');
          FTimer.Interval := FTimerITI.Interval;
          FTimerITI.Enabled:= True;
          Exit;
        end
      else
        begin
          FITIEnd := GetTickCount;
          FTimer.Running := False;
          PlayTrial;
          Exit;
        end;
    end;

  if FTimerTO.Enabled then
    begin
      {$ifdef DEBUG}
        DebugLn(mt_Debug +  'FTimerTO.Enabled');
      {$endif}
      FTimerTO.Enabled := False;

      if FBackGround is TForm then TForm(FBackGround).Color:= FBlc.BkGnd;

      if (FTimerITI.Interval > 0) then
        begin
          //if ShowCounter then ShowCounterPlease('IET');
          FTimer.Interval := FTimerITI.Interval;
          FTimerITI.Enabled:= True;
          Exit;
        end
      else
        begin
          FITIEnd := GetTickCount;
          FTimer.Running := False;
          PlayTrial;
          Exit;
        end;
    end;

  if FTimerITI.Enabled then
    begin
      {$ifdef DEBUG}
        DebugLn(mt_Debug +  'FTimerITI.Enabled');
      {$endif}

      FTimerITI.Enabled:= False;

      if Assigned(FCounterLabel) then
        begin
          FCounterLabel.Free;
        end;

      FITIEnd := GetTickCount;
      FTimer.Running := False;
      PlayTrial;
    end;
end;

procedure TBlc.WriteTrialData(Sender: TObject);
var CountTr, NumTr, NameTr: String;
begin
  if FTrial.Header <> FLastHeader then
    begin
      FData:= FData + #13#10 + FBlcHeader + #9 + 'ITIBegin' + #9 + '__ITIEnd' + #9 + FTrial.Header + #13#10;
      FDataTicks:= FDataTicks + #13#10 + FBlcHeader + FTrial.HeaderTicks + #13#10;
    end;
  FLastHeader:= FTrial.Header;
  //FBlcHeader:= #32#32#32#32#32#32#32#32#9 #32#32#32#32#32#32#32#32#9 #32#32#32#32#32#32#32#32#9;

  CountTr := IntToStr(FCounterManager.Trials.Counter + 1);
  NumTr:= IntToStr(FCounterManager.CurrentTrial.Counter + 1);
  if FBlc.Trials[FCounterManager.CurrentTrial.Counter].Name = '' then NameTr:= '--------'
  else NameTr:= FBlc.Trials[FCounterManager.CurrentTrial.Counter].Name;

  if (FCounterManager.CurrentTrial.Counter + 1) = 1 then
    begin
      FData:= FData +
          CountTr + #9 +
          NumTr + #9 +
          NameTr + #9 +
          #32#32#32#32#32#32 + 'NA' + #9 +
          FormatFloat('00000000;;00000000', FFirstTrialBegin - FTimeStart) + #9 +
          FTrial.Data + #9 +
          #13#10;
    end
  else
    begin
      FData:= FData +
          CountTr + #9 +
          NumTr + #9 +
          NameTr + #9 +
          FormatFloat('00000000;;00000000', FITIBegin - FTimeStart) + #9 +
          FormatFloat('00000000;;00000000', FITIEND - FTimeStart) + #9 +
          FTrial.Data + #9 +
          #13#10;
    end;

  FDataTicks:= FDataTicks + CountTr + #9 + NumTr + #9 + FTrial.DataTicks +  #13#10;

  {$ifdef DEBUG}
    DebugLn(mt_Debug + 'ITI:' + FormatFloat('00000000;;00000000', (FITIEND - FTimeStart) - (FITIBegin - FTimeStart)));
  {$endif}
  if (FTrial is TMsg) then else
    if Assigned(OnConsequence) then FOnConsequence (Sender);
end;

procedure TBlc.BkGndResponse(Sender: TObject);
begin
  if Assigned(OnBkGndResponse) then FOnBkGndResponse (Sender);
end;

constructor TBlc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FData := '';

  with FTimerITI do begin
    Interval := 0;
    Enabled := False;
  end;

  with FTimerTO do begin
    Interval := 0;
    Enabled := False;
  end;

  with FTimerCsq do begin
    Interval := 0;
    Enabled := False;
  end;

end;

procedure TBlc.DebugStatus(msg: string);
begin
  //do nothing
end;

procedure TBlc.CreateIETMedia(FileName, HowManyLoops, Color: String);
//var
//  MediaPath : string;
begin

  //BlockInput(true);
  FIETMedia := TKey.Create(FBackGround);
  FIETMedia.Cursor:= FBackGround.Cursor;
  FIETMedia.Parent:= FBackGround;
  FIETMedia.OnConsequence:= @IETConsequence;
  FIETMedia.OnResponse:= @IETResponse;
  FIETMedia.HowManyLoops := StrToIntDef(HowManyLoops, 1) - 1;
  FIETMedia.Color := StrToIntDef(Color, 0);
  FIETMedia.Width := Screen.Width;
  FIETMedia.Height := Screen.Height;
  {
  FIETMedia.Width := (Screen.Width div 5) * 4;
  FIETMedia.Height := (Screen.Height div 5) * 4;
  FIETMedia.Top := (Screen.Height div 2) - (FIETMedia.Height div 2);
  FIETMedia.Left := (Screen.Width div 2) - (FIETMedia.Width div 2);
  }
  FIETMedia.FullPath := FileName;
  FIETMedia.Play;
  FIETMedia.FullScreen;
  //FTrial.IETConsequence := '';
  //FIETMedia.Show;
end;

destructor TBlc.Destroy;
begin

  inherited Destroy;
end;

procedure TBlc.Play(CfgBlc: TCfgBlc; Manager : TCountermanager; IndTent: Integer; TestMode: Boolean);
//var
//  aFileName : string;

begin
  FBlc:= CfgBlc;
  FCounterManager := Manager;

  FTestMode:= TestMode;

  FLastHeader:= '';

  FCounterManager.CurrentTrial.Counter := IndTent;
  FIsCorrection := False;

  FBlcHeader:= 'Trial_No'+ #9 + 'Trial_Id'+ #9 + 'TrialNam' + #9;
  FRegData.SaveData(FBlc.Name);

  PlayTrial
end;

procedure TBlc.PlayTrial;
var IndTrial : integer;
begin
  if Assigned(FTrial) then
    begin
      FreeAndNil(FTrial);
      //FTrial.Free;
    end;

  if FBackGround is TForm then TForm(FBackGround).Color:= FBlc.BkGnd;

  IndTrial := FCounterManager.CurrentTrial.Counter;

  if IndTrial < FBlc.NumTrials then
    begin

      if FBlc.Trials[IndTrial].Kind = T_CLB then FTrial := TCLB.Create(Self);
      if FBlc.Trials[IndTrial].Kind = T_FPE then FTrial := TFPE.Create(Self);
      if FBlc.Trials[IndTrial].Kind = T_MRD then FTrial:= TMRD.Create(Self);
      if FBlc.Trials[IndTrial].Kind = T_MSG then FTrial:= TMSG.Create(Self);
      if FBlc.Trials[IndTrial].Kind = T_MTS then FTrial:= TMTS.Create(Self);
      if FBlc.Trials[IndTrial].Kind = T_Simple then FTrial:= TSimpl.Create(Self);

      if Assigned(FTrial) then
        begin
          FTrial.CounterManager := FCounterManager;
          FTrial.ServerAddress := FServerAddress;
          FTrial.TimeStart := FTimeStart;
          FTrial.Parent := FBackGround;
          FTrial.Align := AlClient;
          FTrial.OnEndTrial := @TrialTerminate;
          FTrial.OnWriteTrialData := @WriteTrialData;
          FTrial.OnStmResponse := @StmResponse;
          FTrial.OnBkGndResponse := @BkGndResponse;
          FTrial.OnHit := @Hit;
          FTrial.OnMiss := @Miss;
          FTrial.CfgTrial := FBlc.Trials[IndTrial];
          FTrial.Visible := False;
          FTrial.Play(FTestMode, FIsCorrection);
          FTrial.Visible := True;
          FTrial.SetFocus;
          if IndTrial = 0 then FFirstTrialBegin := GetTickCount;
        end else EndBlc(Self);

      FIsCorrection := False;
    end
      else
        begin
          if Assigned(FTrial) then FreeAndNil(FTrial);
          EndBlc(Self);
        end;

end;

  {
procedure TBlc.ShowCounterPlease(Kind: String);
begin
  if Kind = 'IET' then
  begin
    FCounterLabel := TLabel.Create(FBackGround);
    with FCounterLabel do
    begin
      Parent:= FBackGround;
      Cursor:= FBackGround.Cursor;
      AutoSize := True;
      WordWrap := True;

      Caption := IntToStr(FCounterManager.OnHitResult);
      //Color := clBtnFace;
      Font.Size := 100;
      Font.Color := clWhite;
      Top := (Screen.Height div 2) - (Height div 2);
      Left := (Screen.Width div 2) - (Width div 2);
    end
  end;
end;   }

procedure TBlc.StmResponse(Sender: TObject);
begin
  if Assigned(OnStmResponse) then FOnStmResponse (Sender);
end;


end.

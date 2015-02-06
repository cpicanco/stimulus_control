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

uses Classes, {IdGlobal,} Controls, LCLIntf, LCLType,
     ExtCtrls, SysUtils, Graphics, Forms,
     Dialogs,StdCtrls, FileUtil,

     session_config, regdata,
     constants, response_key,
     countermanager,
     client,
     trial_abstract,
       trial_message,

       trial_simple,
         trial_mirrored_stm,
           trial_feature_positive,

       trial_matching
     ;

type

  { TBlc }

  TBlc = class(TComponent)
  private
    FCounterLabel : TLabel;
    FTimeStart: cardinal;
    FITIBegin : cardinal;
    FITIEnd : cardinal;
    FTrial: TTrial;
    FIETMedia : TKey;
    FManager : TCounterManager;
    FData : String;
    FDataTicks : String;
    FBlcHeader: String;
    FTimestampsPath : string;
    FRegData: TRegData;
    //FRegDataTicks: TRegData;
    FNextBlc: String;
    FOnEndBlc: TNotifyEvent;
    FBackGround: TWinControl;
    FBlc: TCfgBlc;
    FLastHeader: String;
    FTimerITI: TTimer;
    FTimerTO : TTimer;
    FTimerCsq : TTimer;
    FShowCounter : Boolean;
    FTestMode: Boolean;
    FIsCorrection : Boolean;
    FOnBeginCorrection : TNotifyEvent;
    FOnEndCorrection : TNotifyEvent;
    FOnStmResponse: TNotifyEvent;
    FOnEndTrial: TNotifyEvent;
    FOnBkGndResponse: TNotifyEvent;
    //FOnBeginTrial: TNotifyEvent;
    FOnConsequence: TNotifyEvent;
    FOnHit: TNotifyEvent;
    FOnMiss: TNotifyEvent;
    FOnCriteria: TNotifyEvent;
    FClientThread : TClientThread;
    procedure CreateClientThread(Code : string);
    procedure DebugStatus(msg : string);
    procedure CreateIETMedia(FileName, HowManyLoops, Color : String);
    procedure ShowCounterPlease (Kind : String);
    procedure IETResponse(Sender: TObject);
    procedure IETConsequence(Sender: TObject);
    procedure EndBlc(Sender: TObject);
    procedure PlayTrial;
    procedure WriteTrialData(Sender: TObject);
    procedure TrialTerminate(Sender: TObject);
    procedure TimerITITimer(Sender: TObject);
    procedure TimerTOTimer(Sender: TObject);
    procedure TimerCsqTimer(Sender: TObject);
    procedure StmResponse(Sender: TObject);
    procedure BkGndResponse(Sender: TObject);
    procedure Hit(Sender: TObject);
    procedure Miss(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Play(CfgBlc: TCfgBlc; Manager : TCountermanager; IndTent: Integer; TestMode: Boolean);
    property ShowCounter : Boolean read FShowCounter write FShowCounter;
    property RegData: TRegData read FRegData write FRegData;
    //property RegDataTicks: TRegData read FRegDataTicks write FRegDataTicks;
    property BackGround: TWinControl read FBackGround write FBackGround;
    property NextBlc: String read FNextBlc write FNextBlc;
    property OnEndBlc: TNotifyEvent read FOnEndBlc write FOnEndBlc;
    property OnStmResponse : TNotifyEvent read FOnStmResponse write FOnStmResponse;
    property OnBkGndResponse : TNotifyEvent read FOnBkGndResponse write FOnBkGndResponse;
    property OnConsequence : TNotifyEvent read FOnConsequence write FOnConsequence;
    property OnEndTrial : TNotifyEvent read FOnEndTrial write FOnEndTrial;
    property OnHit: TNotifyEvent read FOnHit write FOnHit;
    property OnMiss: TNotifyEvent read FOnMiss write FOnMiss;
    property OnBeginCorrection: TNotifyEvent read FOnBeginCorrection write FOnBeginCorrection;
    property OnEndCorrection: TNotifyEvent read FOnEndCorrection write FOnEndCorrection;
    property OnCriteria: TNotifyEvent read FOnCriteria write FOnCriteria;
    property TimeStart : cardinal read FTimeStart write FTimeStart;
  end;

implementation

//uses fUnit6;

procedure TBlc.EndBlc(Sender: TObject);
begin
  FRegData.SaveData(#13#10);
  //FRegDataTicks.SaveData(#13#10);
  if Assigned(OnEndBlc) then FOnEndBlc(Sender);
end;


procedure TBlc.Hit(Sender: TObject);
begin
  FManager.OnHit(Sender);
  if FBlc.CrtKCsqHit > 0 then
    if FBlc.CrtKCsqHit = FManager.BlcCsqHits.Counter then       //Procedimento da Ana Paula, acertos consecutivos produzindo csq
      begin
        FManager.OnCsqCriterion(Sender);
        FTrial.DispenserPlusCall;
      end;
  if Assigned(OnHit) then FOnHit(Sender);
end;

procedure TBlc.IETConsequence(Sender: TObject);
begin
  BkGndResponse(Sender);
end;

procedure TBlc.IETResponse(Sender: TObject);
begin
  BkGndResponse(Sender);
end;

procedure TBlc.Miss(Sender: TObject);
begin
  FManager.OnMiss(Sender);
  if Assigned(OnMiss) then FOnMiss(Sender);
end;

procedure TBlc.TrialTerminate(Sender: TObject);
var s0, s1, s2, s3, s4 : string;
    csqDuration : Integer;

  procedure SetValuesToStrings (var as1, as2, as3, as4 : string);
  var
      Values : string;
  begin
    if FTrial.IETConsequence = 'CSQ+' then
      begin
        // FileName, HowManyLoops, Color, MediaDuration
        Values := 'CSQ1.wav 1 255 1000';

      end
    else
      if FTrial.IETConsequence = 'CSQ-' then
        begin
          // FileName, HowManyLoops, Color, MediaDuration
          Values := 'CSQ2.wav 1 0 1000';

        end
      else Values := FTrial.IETConsequence;

    s0 := Values + #32;
    as1:= FTrial.RootMedia + Copy(s0, 0, pos(#32, s0)-1);
    Delete(s0, 1, pos(#32, s0)); If Length(s0)>0 then While s0[1]=#32 do Delete(s0, 1, 1);
    as2:= Copy(s0, 0, pos(#32, s0)-1);
    Delete(s0, 1, pos(#32, s0)); If Length(s0)>0 then While s0[1]=#32 do Delete(s0, 1, 1);
    as3:= Copy(s0, 0, pos(#32, s0)-1);
    Delete(s0, 1, pos(#32, s0)); If Length(s0)>0 then While s0[1]=#32 do Delete(s0, 1, 1);
    as4:= Copy(s0, 0, pos(#32, s0)-1);
  end;
begin
  FRegData.SaveData (FData);
  //FRegDataTicks.SaveData(FDataTicks);
  FData := '';
  FDataTicks := '';

  if  (FTrial.NextTrial = 'END') then //end session
    FManager.CurrentTrial.Counter := FBlc.NumTrials
  else //continue
    if (FTrial.NextTrial = 'CRT') or             // FTrial.NextTrial base 1, FManager.CurrentTrial.Counter base 0)
       (FTrial.NextTrial = (IntToStr(FManager.CurrentTrial.Counter + 1))) then
      begin //correction trials were on
        if ((FBlc.MaxCorrection) = FManager.BlcCscCorrections.Counter) and
           (FBlc.MaxCorrection <> 0) then
          begin //correction
            FManager._VirtualTrialFix;
            FManager.OnNotCorrection(Sender);
            FManager.OnEndTrial (Sender);
            FIsCorrection := False;
          end
        else
          begin // not correction
            FManager.OnCorrection(Sender);
            FIsCorrection := True;
          end;
      end
    else  //correction trials were off
      if StrToIntDef(FTrial.NextTrial, 0) > 0 then
        begin //go to the especified trial
          if FTrial.Result = 'MISS' then
            FManager.VirtualTrialLoop.Counter := FManager.VirtualTrialValue;

          FManager.OnNotCorrection(Sender);
          FManager.CurrentTrial.Counter := StrToIntDef(FTrial.NextTrial, 0) - 1;
          FManager.OnNxtTrial (Sender);
          FIsCorrection := False;
        end
      else // go to the next trial,
        begin
          if FTrial.Result = 'MISS' then
            FManager.VirtualTrialLoop.Counter := FManager.VirtualTrialValue;

          FManager.OnNotCorrection(Sender);
          FManager.OnEndTrial (Sender);
          FIsCorrection := False;
        end;

  //Critérios de ACERTO atingido
  if  ((FBlc.CrtConsecutiveHit > 0) and (FBlc.CrtConsecutiveHit = FManager.BlcCscHits.Counter))
   //or ((FCfgBlc.CrtConsecutiveMiss > 0) and (FCfgBlc.CrtConsecutiveMiss = FManager.BlcCscMisses.Counter))
   or ((FTrial.NextTrial = IntToStr(FBlc.CrtMaxTrials)) and (FBlc.CrtMaxTrials > 0))
  then
      begin
        if Assigned(OnCriteria) then FOnCriteria(Sender);
        FManager.CurrentTrial.Counter := FBlc.NumTrials
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
      if FBlc.ITI > 0 then FTimerITI.Interval:= FBlc.ITI
      else FTimerITI.Interval:= 0;

      if FTrial.TimeOut > 0 then FTimerTO.Interval:= FTrial.TimeOut
      else FTimerTO.Interval:= 0;

      if csqDuration > 0 then FTimerCsq.Interval:= csqDuration
      else FTimerCsq.Interval:= 0;
      FITIBegin := GetTickCount;
    end;

  if FTrial.TimeOut > 0 then
    if FBackGround is TForm then TForm(FBackGround).Color:= 0;

  if Assigned(FTrial) then
    begin
      FreeAndNil(FTrial);
    end;
  //SetFocus;
  if Assigned(OnEndTrial) then FOnEndTrial(Sender);


  //showmessage(s0 + #13#10 +s1 + #13#10 +s2 + #13#10 +s3 + #13#10);
    if (FTimerITI.Interval = 0)
     and (FTimerTO.Interval = 0)
     and (FTimerCsq.Interval = 0) then
     begin
       PlayTrial;
     end else
    begin
      if (FTimerITI.Interval > 0)
       and (FTimerTO.Interval > 0)
       and (FTimerCsq.Interval > 0) then
        begin
          if ShowCounter then ShowCounterPlease ('IET');
          FTimerITI.Enabled:= True;
        end
      else
        begin
          if (FTimerITI.Interval > 0)
             and (FTimerTO.Interval = 0)
             and (FTimerCsq.Interval = 0) then
            begin
              if ShowCounter then ShowCounterPlease ('IET');
              FTimerITI.Enabled:= True;
            end;
          if ((FTimerITI.Interval > 0) or (FTimerCsq.Interval > 0))
             and (FTimerTO.Interval = 0) then
            begin
              // FileName, HowManyLoops, Color

              CreateIETMedia(s1, s2, s3);
              //BlockInput(True);
              FTimerCsq.Enabled:= True;
            end;

          if ((FTimerITI.Interval > 0) or (FTimerTO.Interval > 0))
             and (FTimerCsq.Interval = 0) then
            begin
              FTimerTO.Enabled:= True;
            end;
        end
    end;

end;

procedure TBlc.TimerCsqTimer(Sender: TObject);
begin
  FTimerCsq.Enabled := False;
  if Assigned(FIETMedia) then
    begin
      FreeAndNil(FIETMedia);
      //BlockInput(False);
    end;
  if (FTimerITI.Interval > 0) then
    begin
      if ShowCounter then ShowCounterPlease ('IET');
      FTimerITI.Enabled:= True;
    end
  else PlayTrial;
end;

procedure TBlc.TimerITITimer(Sender: TObject);
begin
  if Assigned(FCounterLabel) then
    begin
      FreeAndNil(FCounterLabel);
    end;
  FTimerITI.Enabled:= False;
  FITIEnd := GetTickCount;
  PlayTrial;
end;

procedure TBlc.TimerTOTimer(Sender: TObject);
begin
  FTimerTO.Enabled := False;
  if FBackGround is TForm then TForm(FBackGround).Color:= FBlc.BkGnd;

  if (FTimerITI.Interval > 0) then
    begin
      if ShowCounter then ShowCounterPlease ('IET');
      FTimerITI.Enabled:= True;
    end
  else PlayTrial;
end;

procedure TBlc.WriteTrialData(Sender: TObject);
var CountTr, NumTr, NameTr: String;
begin
  if FTrial.Header <> FLastHeader then
    begin
      FData:= FData + #13#10 + FBlcHeader + FTrial.Header + #9 + 'ITIBegin' + #9 + '--ITIEnd' + #13#10;
      //FDataTicks:= FDataTicks + #13#10 + FBlcHeader + FTrial.HeaderTicks + #13#10;
    end;
  FLastHeader:= FTrial.Header;
  FBlcHeader:= #32#32#32#32#32#32#32#32#9#32#32#32#32#32#32#32#32#9;

  CountTr := IntToStr(FManager.Trials.Counter + 1);
  NumTr:= IntToStr(FManager.CurrentTrial.Counter + 1);
  if FBlc.Trials[FManager.CurrentTrial.Counter].Name = '' then NameTr:= '--------'
  else NameTr:= FBlc.Trials[FManager.CurrentTrial.Counter].Name;

  if (FManager.CurrentTrial.Counter + 1) = 1 then
    begin
      FData:= FData + CountTr + #9 +
          NumTr + #9 +
          NameTr + #9 +
          FTrial.Data + #9 +
          #32#32#32#32#32#32 + 'NA' + #9 +
          #32#32#32#32#32#32 + 'NA' +
          #13#10;
    end
  else
    begin
      FData:= FData + CountTr + #9 +
          NumTr + #9 +
          NameTr + #9 +
          FTrial.Data + #9 +
          FormatFloat('00000000;;00000000',FITIBegin - FTimeStart) + #9 +
          FormatFloat('00000000;;00000000',FITIEND - FTimeStart) +
          #13#10;

    end;
  //FDataTicks:= FDataTicks + CountTr + #9 + NumTr + #9 + FTrial.DataTicks +  #13#10;

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
  FTimerITI:= TTimer.Create(Self);
  with FTimerITI do begin
    Enabled:= False;
    OnTimer:= @TimerITITimer;
  end;

  FTimerTO:= TTimer.Create(Self);
  with FTimerTO do begin
    Enabled:= False;
    OnTimer:= @TimerTOTimer;
  end;

  FTimerCsq:= TTimer.Create(Self);
  with FTimerCsq do begin
    Enabled:= False;
    OnTimer:= @TimerCsqTimer;
  end;

end;

procedure TBlc.CreateClientThread(Code: string);
begin
  FClientThread := TClientThread.Create( True, FManager.CurrentTrial.Counter, Code, FTimestampsPath);
  FClientThread.OnShowStatus := @DebugStatus;
  FClientThread.Start;
end;

procedure TBlc.DebugStatus(msg: string);
begin
  //do nothing
end;

procedure TBlc.CreateIETMedia(FileName, HowManyLoops, Color : string);
var
    MediaPath : string;
begin
  if FileExistsUTF8(FileName) { *Converted from FileExists*  } then
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
end;

destructor TBlc.Destroy;
begin
  Inherited Destroy;
end;

procedure TBlc.Play(CfgBlc: TCfgBlc; Manager : TCountermanager; IndTent: Integer; TestMode: Boolean);
var
  aFileName : string;
begin
  FBlc:= CfgBlc;
  FManager := Manager;

  FTestMode:= TestMode;

  FLastHeader:= '';

  FManager.CurrentTrial.Counter := IndTent;
  FIsCorrection := False;

  FBlcHeader:= 'Trial_No'+ #9 + 'Trial_Id'+ #9 + 'TrialNam' + #9;
  FRegData.SaveData(FBlc.Name);

  aFileName := ExtractFileNameWithoutExt(FRegData.FileName);
  FTimestampsPath := aFileName + '.timestamps';
  //FRegDataTicks.SaveData(FBlc.Name);
  PlayTrial;
end;

procedure TBlc.PlayTrial;
var IndTrial : integer;
begin
  if FBackGround is TForm then TForm(FBackGround).Color:= FBlc.BkGnd;

  IndTrial := FManager.CurrentTrial.Counter;

  if IndTrial < FBlc.NumTrials then
    begin

      if FBlc.Trials[IndTrial].Kind = T_FPE then FTrial := TFPE.Create(Self);
      if FBlc.Trials[IndTrial].Kind = T_MRD then FTrial:= TMRD.Create(Self);
      if FBlc.Trials[IndTrial].Kind = T_MSG then FTrial:= TMSG.Create(Self);
      if FBlc.Trials[IndTrial].Kind = T_MTS then FTrial:= TMTS.Create(Self);
      if FBlc.Trials[IndTrial].Kind = T_Simple then FTrial:= TSimpl.Create(Self);


      if Assigned(FTrial) then
        begin
          FTrial.TimeStart := FTimeStart;
          FTrial.FileName := FTimestampsPath;
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
          FTrial.Play(FManager, FTestMode, FIsCorrection);
          FTrial.Visible := True;
          FTrial.SetFocus;
        //BlockInput(False);
        end else EndBlc(Self);

      FIsCorrection := False;
    end
      else
        begin
          if Assigned(FTrial) then Ftrial.Free;
          CreateClientThread('EndBloc');
          EndBlc(Self);
        end;
  end;


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

      Caption := IntToStr(FManager.OnHitResult);
      //Color := clBtnFace;
      Font.Size := 100;
      Font.Color := clWhite;
      Top := (Screen.Height div 2) - (Height div 2);
      Left := (Screen.Width div 2) - (Width div 2);
    end
  end;
end;

procedure TBlc.StmResponse(Sender: TObject);
begin
  if Assigned(OnStmResponse) then FOnStmResponse (Sender);
end;

end.

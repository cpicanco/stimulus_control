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
unit background;

{$mode objfpc}{$H+}

interface

uses
  LCLType, LCLIntf, Math,
  {$IFDEF LCLGTK2}
  gtk2, gdk2, //glib2,
  {$ENDIF}
  Classes, SysUtils, FileUtil, Forms
, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls
, regdata
, client
, draw_methods
;

type

  { Tbkgnd }

  Tbkgnd = class(TForm)
    debugconsole: TMemo;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormPaint(Sender: TObject);
  private
    FFullScreen : Boolean;
    FOriginalBounds: TRect;
    FOriginalWindowState: TWindowState;
    FScreenBounds: TRect;
    //FLabel : TLabel;
    //FTimer : TTimer;
    //FTimeStart : DWord; //cardinal
    //FTimeEvent : DWord;
    //FSessionConfig : TConfig;
    //FVisibleTrial : Boolean;
    //FClientThread : TClientThread;
    //FSimpleClient: TSyncSub;
    //procedure CreateClientThread(Code : String);
    //procedure DebugStatus(msg : string);
    //procedure DoSaveTrialData;
    //procedure DoVisibleTrial(aVisible : Boolean);
    //procedure DoVisibleLabel(aName : String);
    //procedure DoVisibleImages(aVisible : Boolean);
    //procedure TimerOnTimer(Sender : TObject);
  public
    { public declarations }
    //FImage1 : TImage;
    //FImage2 : TImage;
    //FData : TRegData;
    procedure SetFullScreen(TurnOn : Boolean);
    //procedure RunSession(Step, Circle_Size, ITI : integer; Rect : TRect; Img1 :TImage = nil; Img2 : TImage = nil);

    //property VisibleTrial : Boolean read FVisibleTrial write DoVisibleTrial;
  end;

var
  bkgnd: Tbkgnd;

implementation

{$R background.lfm}

{ Tbkgnd }

procedure Tbkgnd.FormCreate(Sender: TObject);
begin
  FFullScreen := False;
  //ITI timer
  //FTimer := TTimer.Create(Self);
  //FTimer.Enabled := False;
  //FTimer.OnTimer := @TimerOnTimer;


  //FVisibleTrial := False;
  //FImage1 := nil;
  //FImage2 := nil;

  //FLabel := TLabel.Create(Self);
  //FLabel.Parent := Self;
  //FLabel.Align:=alClient;
  //FLabel.Alignment:=taCenter;
  //FLabel.AutoSize:= True;
  //FLabel.Layout:= tlCenter;
  //FLabel.Font.Size := 50;
  //FLabel.Font.Color:= clBlack;
  //Randomize;
end;

procedure Tbkgnd.FormActivate(Sender: TObject);
begin

end;

procedure Tbkgnd.FormDestroy(Sender: TObject);
begin
 // if Assigned(FClientThread) then FClientThread.Terminate;
  //DeleteCriticalSection(FCriticalSection);
end;

procedure Tbkgnd.FormKeyPress(Sender: TObject; var Key: char);
begin
  //FTimeEvent := GetTickCount;
  ////if key in ['a', 'A'] then SetFullScreen(not FFullScreen);
  //
  //if key in [#32] then
  //  begin
  //    if FLabel.Visible then
  //      begin
  //        CreateClientThread('LEnd');
  //        FData.LatencyLblResponse := FTimeEvent - FTimeStart ;
  //        FLabel.Visible:= False;
  //        //Sleep(500);
  //        VisibleTrial := True;
  //        Exit;
  //      end;
  //
  //    if VisibleTrial then
  //      begin
  //        CreateClientThread('DEnd');
  //        FData.LatencyStmResponse := FTimeEvent - FTimeStart ;
  //
  //        VisibleTrial := False;
  //
  //        Invalidate;
  //        FTimer.Enabled := True;
  //        CreateClientThread('ITIBEGIN');
  //        FData.ITIBEGIN := GetTickCount - FTimeStart;
  //        Exit;
  //      end;
  //
  //
  //  end;
  //
  //if key in ['s', 'S'] then
  //  begin
  //    CreateClientThread('press_s');
  //    FClientThread.Start;
  //    Exit;
  //  end;


    //if key in ['d', 'D'] then FClientThread.Terminate;

    //
    //if key in ['t', 'T'] then ShowMessage(FSimpleClient.LastLog);
    //
end;

procedure Tbkgnd.FormPaint(Sender: TObject);
//var i : integer;
begin
  //CenteredMarker (Canvas, width, height, 6);

  //if Assigned(FImage1) and Assigned(FImage2) and VisibleTrial then
  //  begin
  //    for i := 0 to ComponentCount -1 do
  //      begin
  //        if Components[i] is TImage then
  //          begin
  //              with Canvas do
  //                begin
  //                  Brush.Style:= bsClear;
  //                  Pen.Width := 10;
  //                  Pen.Color:= clBlack;
  //                  with TImage(Components[i]) do
  //                    Rect(Left,Top, Width, Height);
  //                end;
  //          end;
  //      end;
  //  end
  //else
  //  if VisibleTrial then
  //  with FSessionConfig do
  //  with CT do
  //  begin
  //    if (X.axis[i] = 'd1') then
  //      begin
  //        with C[0] do DrawCircle(Canvas, o.X, o.Y, size, gap, gap_degree, gap_length);
  //        with C[1] do DrawCircle(Canvas, o.X - size, o.Y - size, size, gap, gap_degree, gap_length);
  //      end;
  //    if (X.axis[i] = 'd2') then
  //      begin
  //        with C[0] do DrawCircle(Canvas, o.X - size, o.Y, size, gap, gap_degree, gap_length);
  //        with C[1] do DrawCircle(Canvas, o.X, o.Y - size, size, gap, gap_degree, gap_length);
  //      end;
  //    if (X.axis[i] = 'h') then
  //      begin
  //        with C[0] do DrawCircle(Canvas, o.X, o.Y + 13, size, gap, gap_degree, gap_length);
  //        with C[1] do DrawCircle(Canvas, o.X - size, o.Y + 13, size, gap, gap_degree, gap_length);
  //      end;
  //    if (X.axis[i] = 'v') then
  //      begin
  //        with C[0] do DrawCircle(Canvas, o.X + 13, o.Y, size, gap, gap_degree, gap_length);
  //        with C[1] do DrawCircle(Canvas, o.X + 13, o.Y - size, size, gap, gap_degree, gap_length);
  //      end;
  //  end;
end;

//procedure Tbkgnd.DoVisibleLabel(aName : String);
//begin
//  FLabel.Caption:= aName;
//  //FLabel.Left := Round((Screen.Width/2) - (FLabel.Width/2));
//  //FLabel.Top := Round((Screen.Height/2) - (FLabel.Height/2));
//  FLabel.Visible := True;
//  FData.LatencyLblBegin:= GetTickCount - FTimeStart;
//  CreateClientThread('LBegin');
//end;
//
//procedure Tbkgnd.DoVisibleTrial(aVisible: Boolean);
//begin
//  FVisibleTrial := aVisible;
//  DoVisibleImages(FVisibleTrial);
//  Invalidate;
//  CreateClientThread('DBegin');
//  if aVisible then FData.LatencyStmBegin := GetTickCount - FTimeStart;
//end;
//
//procedure Tbkgnd.DoVisibleImages(aVisible: Boolean);
//begin
//  if Assigned(FImage1) and Assigned(FImage2) then
//    begin
//      if aVisible then
//        begin
//          with FSessionConfig.CT.C[0] do
//            begin
//              FImage1.Left := o.X;
//              FImage1.Top := o.Y;
//            end;
//
//          with FSessionConfig.CT.C[1] do
//            begin
//              FImage2.Left := o.X;
//              FImage2.Top := o.Y;
//            end;
//        end;
//
//      FImage1.Visible := aVisible;
//      FImage2.Visible := aVisible;
//  end;
//end;
//
//procedure Tbkgnd.CreateClientThread(Code : string);
//begin
//  FClientThread := TClientThread.Create( True, FSessionConfig.CT.i, Code );
//  FClientThread.OnShowStatus := @DebugStatus;
//  FClientThread.Start;
//end;
//
//procedure Tbkgnd.DebugStatus(msg: string);
//begin
//  debugconsole.Lines.Append(msg);
//  //Exit;
//end;
//
//procedure Tbkgnd.DoSaveTrialData;
//begin
//  //FData.SaveData('___Trial' + #9 +
//  //               '____Axis' + #9 +
//  //               '______X1' + #9 +
//  //               '______Y1' + #9 +
//  //               '______X2' + #9 +
//  //               '______Y2' + #9 +
//  //               '___Label' + #9 +
//  //               '__LBegin' + #9 +
//  //               'LLatency' + #9 +
//  //               '__DBegin' + #9 +
//  //               'DLatency' + #9 +
//  //               'ITIBEGIN' + #9 +
//  //               '_ITI_END' + #9 +
//  //               //'' + #9 +
//  //               #13#10);
//  with FSessionConfig do
//    FData.SaveData(
//                   Format('%-*.*d', [4,8,CT.i + 1]) + #9 +
//                   Format('%0:-8s', [X.axis[CT.i]]) + #9 +
//                   Format('%-*.*d', [4,8,CT.C[0].o.X]) + #9 +
//                   Format('%-*.*d', [4,8,CT.C[0].o.Y]) + #9 +
//                   Format('%-*.*d', [4,8,CT.C[1].o.X]) + #9 +
//                   Format('%-*.*d', [4,8,CT.C[1].o.Y]) + #9 +
//                   Format('%0:-8s', [CT.response]) + #9 +
//                   FormatFloat('00000000;;00000000',FData.LatencyLblBegin) + #9 +
//                   FormatFloat('00000000;;00000000',FData.LatencyLblResponse) + #9 +
//                   FormatFloat('00000000;;00000000',FData.LatencyStmBegin) + #9 +
//                   FormatFloat('00000000;;00000000',FData.LatencyStmResponse) + #9 +
//                   FormatFloat('00000000;;00000000',FData.ITIBEGIN) + #9 +
//                   FormatFloat('00000000;;00000000',FData.ITIEND) + #9 +
//                   //'' + #9 +
//                   #13#10
//                   );
//end;
//
//procedure Tbkgnd.TimerOnTimer(Sender: TObject);
//begin
//  CreateClientThread('ITIEND');
//
//  FTimer.Enabled:= False;
//  FData.ITIEND:=  GetTickCount - FTimeStart;
//  DoSaveTrialData;
//  if not FSessionConfig.NextTrial then
//    begin
//      //ShowMessage('End.');
//      FData.SaveData('Fim:' + #9 + TimeToStr(Time)+ #13#10 + #13#10);
//      CreateClientThread('TheEnd');
//      Close;
//      Exit;
//    end;
//  DoVisibleLabel(FSessionConfig.CT.response);
//end;

procedure Tbkgnd.SetFullScreen(TurnOn: Boolean);
begin
  if TurnOn then
    begin
      // To full screen
      FOriginalWindowState := WindowState;
      FOriginalBounds := BoundsRect;

      BorderStyle := bsNone;
      FScreenBounds := Screen.MonitorFromWindow(Handle).BoundsRect;
    with FScreenBounds do
      SetBounds(Left, Top, Right - Left, Bottom - Top);
      {$IFDEF WINDOWS}
        Application.ShowInTaskBar := False;
      {$ENDIF}

      {$IFDEF LCLGTK2}
      gdk_window_fullscreen(PGtkWidget(Self.Handle)^.window);
      {$ENDIF}
    end
  else
    begin
      {$IFDEF MSWINDOWS}
      BorderStyle := bsSizeable;
      {$ENDIF}

      if FOriginalWindowState = wsMaximized then
        WindowState := wsMaximized
      else
        with FOriginalBounds do
          SetBounds(Left, Top, Right - Left, Bottom - Top);

      {$IFDEF LINUX}
      BorderStyle := bsSizeable;
      {$ENDIF}

      {$IFDEF LCLGTK2}
      gdk_window_unfullscreen(PGtkWidget(Self.Handle)^.window);
      {$ENDIF}
    end;
  FFullScreen := TurnOn;
end;

//procedure Tbkgnd.RunSession(Step, Circle_Size, ITI: integer; Rect : TRect; Img1 :TImage = nil; Img2 : TImage = nil);
//begin
//  FData.SaveData('Início:' + #9 + TimeToStr(Time) + #13#10 + #13#10);
//  FSessionConfig := TConfig.Create(Self, Step, Circle_Size, Rect);
//  FTimer.Interval := ITI;
//
//  FData.SaveData('___Trial' + #9 +
//                 '____Axis' + #9 +
//                 '______X1' + #9 +
//                 '______Y1' + #9 +
//                 '______X2' + #9 +
//                 '______Y2' + #9 +
//                 '___Label' + #9 +
//                 '__LBegin' + #9 +
//                 'LLatency' + #9 +
//                 '__DBegin' + #9 +
//                 'DLatency' + #9 +
//                 'ITIBEGIN' + #9 +
//                 '_ITI_END' + #9 +
//                 //'' + #9 +
//                 #13#10
//                 );
//
//  if Assigned(Img1) then FImage1 := Img1;
//  if Assigned(Img2) then FImage2 := Img2;
//
//  FTimeStart := GetTickCount;
//  DoVisibleLabel(FSessionConfig.CT.response);
//
//end;




end.


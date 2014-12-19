unit trial;

{$mode objfpc}{$H+}

//{$MODE Delphi}

interface

uses LCLIntf, LCLType, LMessages, Controls, Classes, SysUtils,
     session_config, countermanager;
type

  { TTrial }

  TTrial = class(TCustomControl)
  private
    FTimeStart: cardinal;
  protected
    FRootMedia: string;
    FIETConsequence: string;
    FTimeOut : Integer;
    FManager : TCounterManager;
    FResult: String;
    FHeader: String;
    FHeaderTicks: String;
    FData: String;
    FDataTicks: String;
    FNextTrial: String;
    FIsCorrection : Boolean;
    FCfgTrial: TCfgTrial;
    FOnBeginCorrection : TNotifyEvent;
    FOnEndCorrection : TNotifyEvent;
    FOnBkGndResponse: TNotifyEvent;
    FOnHit: TNotifyEvent;
    FOnMiss: TNotifyEvent;
    FOnNone: TNotifyEvent;
    FOnStmResponse: TNotifyEvent;
    FOnConsequence: TNotifyEvent;
    FOnEndTrial: TNotifyEvent;
    procedure WriteData(Sender: TObject); virtual; abstract;
  public
    procedure Play(Manager : TCounterManager; TestMode: Boolean; Correction : Boolean); virtual; abstract;
    procedure DispenserPlusCall; virtual; abstract;
    property CfgTrial: TCfgTrial read FCfgTrial write FCfgTrial;
    property RootMedia : string read FRootMedia;
    property IETConsequence : string read FIETConsequence;
    property Result: String read FResult;
    property Header: String read FHeader;
    property HeaderTicks: String read FHeaderTicks;
    property Data: String read FData;
    property DataTicks: String read FDataTicks;
    property NextTrial: String read FNextTrial;
    property OnEndTrial: TNotifyEvent read FOnEndTrial write FOnEndTrial;
    property OnConsequence: TNotifyEvent read FOnConsequence write FOnConsequence;
    property OnStmResponse: TNotifyEvent read FOnStmResponse write FOnStmResponse;
    property OnBkGndResponse: TNotifyEvent read FOnBkGndResponse write FOnBkGndResponse;
    property OnHit: TNotifyEvent read FOnHit write FOnHit;
    property OnMiss: TNotifyEvent read FOnMiss write FOnMiss;
    property OnNone: TNotifyEvent read FOnNone write FOnNone;
    property OnBeginCorrection : TNotifyEvent read FOnBeginCorrection write FOnBeginCorrection;
    property OnEndCorrection : TNotifyEvent read FOnEndCorrection write FOnEndCorrection;
    property TimeOut : Integer read FTimeOut;
    property TimeStart : cardinal read FTimeStart write FTimeStart;
  end;

implementation



{ TTrial }


end.

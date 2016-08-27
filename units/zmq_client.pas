{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit zmq_client;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Process

  , zmqapi
  ;

type
  { TResponseReceivedEvent }

  TPupilMultiPartMessage = record
    MsgPackage : TMemoryStream;
    MsgTopic : string;
  end;

  { TResponseReceivedEvent }

  TMultiPartMessRecvE = procedure(AResponse: TPupilMultiPartMessage) of object;

  { TZMQSubThread }

  TZMQSubThread = class(TThread)
  private
    FMultipartMessage : TPupilMultiPartMessage;
    FContext : TZMQContext;
    FSubscriber : TZMQSocket;
    //FPoller : TZMQPoller;
    //FRTLEvent: PRTLEvent;
    FOnMultipartMessageReceived: TMultiPartMessRecvE;
    procedure MultipartMessageReceived;
  protected
    procedure Execute; override;
  public
    constructor Create(ASubHost : string; CreateSuspended: Boolean = True);
    destructor Destroy; override;
    procedure Subscribe(AFilter : UTF8string);
    procedure Unsubscribe(AFilter : UTF8string);
    property OnMultiPartMessageReceived: TMultiPartMessRecvE read FOnMultipartMessageReceived write FOnMultipartMessageReceived;

  end;

  { TResponseReceivedEvent }

  TResponseReceivedEvent = procedure(ARequest, AResponse: String) of object;

  { TZMQThread }

  // Descendents must override ResponseReceived
  TZMQThread = class(TThread)
  private
    FResponse,
    FRequest : UTF8string;
    FContext : TZMQContext;
    FRequester : TZMQSocket;
    FRTLEvent: PRTLEvent;
    FOnResponseReceived: TResponseReceivedEvent;
    procedure ResponseReceived;
  protected
    procedure Execute; override;
    procedure SendRequest(ARequest : UTF8string);
    property OnResponseReceived: TResponseReceivedEvent read FOnResponseReceived write FOnResponseReceived;
  public
    constructor Create(AHost : string; CreateSuspended: Boolean = True);
    destructor Destroy; override;
  end;

implementation

{ TZMQSubThread }

procedure TZMQSubThread.MultipartMessageReceived;
begin
  if Assigned(FOnMultipartMessageReceived) then FOnMultipartMessageReceived(FMultipartMessage);
end;

procedure TZMQSubThread.Execute;
var
  ZMQMessages : TZMQMsg;
  LCount : integer;
begin
  while not Terminated do
    begin
      FMultipartMessage.MsgPackage := TMemoryStream.Create;
      ZMQMessages := TZMQMsg.Create;
      try
        // wait for multipart message
        LCount := FSubscriber.recv( ZMQMessages );

        if LCount = 2 then
          begin
            FMultipartMessage.MsgTopic := ZMQMessages.item[0].asUtf8String;
            FMultipartMessage.MsgPackage.WriteBuffer( ZMQMessages.item[1].data^, ZMQMessages.item[1].size );
            Synchronize( @MultipartMessageReceived );
          end;

      finally
        ZMQMessages.Free;
      end;
    end;
end;

constructor TZMQSubThread.Create(ASubHost: string; CreateSuspended: Boolean);
begin
  FreeOnTerminate := True;
  //FRTLEvent := RTLEventCreate;

  FContext := TZMQContext.Create;
  FSubscriber := FContext.Socket( stSub );
  FSubscriber.connect( 'tcp://' + ASubHost );
  inherited Create(CreateSuspended);
end;

destructor TZMQSubThread.Destroy;
begin
  FSubscriber.Free;
  FContext.Free;
  //RTLEventDestroy(FRTLEvent);
  inherited Destroy;
end;

procedure TZMQSubThread.Subscribe(AFilter: UTF8string);
begin
  FSubscriber.Subscribe(AFilter);
end;

procedure TZMQSubThread.Unsubscribe(AFilter: UTF8string);
begin
  FSubscriber.unSubscribe(AFilter);
end;

{$ifdef DEBUG}
  uses debug_logger;
{$endif}

constructor TZMQThread.Create(AHost: string; CreateSuspended: Boolean);
begin
  FResponse := '';
  FRequest := '';
  FreeOnTerminate := True;
  FRTLEvent := RTLEventCreate;

  FContext := TZMQContext.Create;
  FRequester := FContext.Socket( stReq );
  FRequester.connect( 'tcp://' + AHost );
  inherited Create(CreateSuspended);
end;

destructor TZMQThread.Destroy;
begin
  FRequester.Free;
  FContext.Free;
  RTLEventDestroy(FRTLEvent);
  inherited Destroy;
end;

procedure TZMQThread.ResponseReceived;
begin
  if Assigned(FOnResponseReceived) then FOnResponseReceived(FRequest, FResponse);
end;

procedure TZMQThread.Execute;
var
  ARequest,
  AResponse : UTF8String;
begin
  while not Terminated do
    begin
      AResponse := '';
      RTLeventWaitFor(FRTLEvent);

      // make a local copy to synchronize shared variables
      ARequest := FRequest;
      FRequester.send( ARequest );

      // wait for response in a blocking
      FRequester.recv( AResponse );

      FResponse := AResponse;
      Synchronize( @ResponseReceived );

      {$ifdef DEBUG}
        FResponse := mt_Debug + 'TClientThread instance with ThreadID:' + IntToStr(Self.ThreadID);
        Synchronize( @ResponseReceived );
      {$endif}
    end;
end;

procedure TZMQThread.SendRequest(ARequest: UTF8string);
begin
  FRequest := ARequest;
  RTLeventSetEvent(FRTLEvent);
end;

end.

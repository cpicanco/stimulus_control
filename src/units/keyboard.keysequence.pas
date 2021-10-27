unit Keyboard.KeySequence;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  IndexRange = 0..4;
  TCharRange = array [IndexRange] of char;
  TWordRange = array [IndexRange] of word;

  TCharMatrix = array [IndexRange] of TCharRange;
  TWordMatrix = array [IndexRange] of TWordRange;

  { TKeySequence }

  TKeySequence = class(TComponent)
    private
      FIndex : integer;
      FOnEndCapturing: TNotifyEvent;
      FRecIndex : integer;
      FRecordedSequence : TWordRange;
      FKeys : TWordMatrix;
      FOnEndSequence: TNotifyEvent;
      FOnNonTargetKey: TNotifyEvent;
      FOnTargetKey: TNotifyEvent;
      //FUseNegation: Boolean;
      function FinalKey : word;
      procedure SetOnEndCapturing(AValue: TNotifyEvent);
      procedure SetOnEndSequence(AValue: TNotifyEvent);
      procedure SetOnNonTargetKey(AValue: TNotifyEvent);
      procedure SetOnTargetKey(AValue: TNotifyEvent);
      //procedure SetUseNegation(AValue: Boolean);
    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;
      function NextKeys(AKey : word;
        RestartOnNonTargetKey : Boolean = True;
        RestartOnEndSequence: Boolean = True): TWordRange;
      function TargetKeys : TWordRange;
      function ContainsRecordedSequence : Boolean;
      function SeqToString : string;
      function RecSeqToString : string;
      procedure StartFromBegin;
      procedure StartNewRecording;
      procedure RecordKey(AKey : Word);
      procedure LoadFromConfiguration(AConfiguration : TStringList);
      procedure LoadFromChars(AChars : TCharMatrix);
      procedure LoadFromWords(AWords : TWordMatrix);
      //property UseNegation : Boolean read FUseNegation write SetUseNegation;
      property OnEndSequence : TNotifyEvent read FOnEndSequence write SetOnEndSequence;
      property OnEndCapturing: TNotifyEvent read FOnEndCapturing write SetOnEndCapturing;
      property OnTargetKey : TNotifyEvent read FOnTargetKey write SetOnTargetKey;
      property OnNonTargetKey : TNotifyEvent read FOnNonTargetKey write SetOnNonTargetKey;
  end;

implementation

{ TKeySequence }

uses StrUtils, Dialogs;

function TKeySequence.FinalKey: word;
begin
  Result := FKeys[High(IndexRange)][Low(IndexRange)];
end;

procedure TKeySequence.SetOnEndCapturing(AValue: TNotifyEvent);
begin
  if FOnEndCapturing=AValue then Exit;
  FOnEndCapturing:=AValue;
end;

procedure TKeySequence.SetOnEndSequence(AValue: TNotifyEvent);
begin
  if FOnEndSequence=AValue then Exit;
  FOnEndSequence:=AValue;
end;

procedure TKeySequence.SetOnNonTargetKey(AValue: TNotifyEvent);
begin
  if FOnNonTargetKey=AValue then Exit;
  FOnNonTargetKey:=AValue;
end;

procedure TKeySequence.SetOnTargetKey(AValue: TNotifyEvent);
begin
  if FOnTargetKey=AValue then Exit;
  FOnTargetKey:=AValue;
end;
//
//procedure TKeySequence.SetUseNegation(AValue: Boolean);
//begin
//  if FUseNegation=AValue then Exit;
//  FUseNegation:=AValue;
//end;

function TKeySequence.TargetKeys: TWordRange;
begin
  TargetKeys := FKeys[FIndex];
end;

function TKeySequence.ContainsRecordedSequence: Boolean;
var
  i : integer;
  function Contains(AIndex : integer; AKey: Word) : Boolean;
  var
    LTargetKeys : TWordRange;
    i : integer;
  begin
    Contains := False;
    LTargetKeys := FKeys[AIndex];
    //if UseNegation then
    //begin
    //  case AIndex of
    //    0..3 :
    //      if AKey <> LTargetKeys[0] then
    //        Contains := True;
    //
    //    4 :
    //      if AKey = 13 then
    //        Contains := True;
    //
    //  end
    //end else
      for i := Low(IndexRange) to High(IndexRange) do
      begin
        if AKey = LTargetKeys[i] then
        begin
          Contains := True;
          Break;
        end;
      end;
  end;

begin
  //ShowMessage(SeqToString);
  //ShowMessage(RecSeqToString);
  ContainsRecordedSequence := True;
  for i := Low(IndexRange) to High(IndexRange) do
    if Contains(i, FRecordedSequence[i]) then
      Continue
    else begin
      ContainsRecordedSequence := False;
      Break
    end;
end;


function TKeySequence.SeqToString: string;
var
  i : integer;
  j : integer;
begin
  Result := '';
  for i in IndexRange do
  begin
    Result := Result + '[';
    for j in IndexRange do
    begin
      case FKeys[i][j] of
        0..31 : { do nothing };
        32..126 :
        begin
          Result := Result + Char(FKeys[i][j]) + #32;
        end;
      end;
    end;
    Result := Result + ']' + #10;
  end;
end;

function TKeySequence.RecSeqToString: string;
var
  i : integer;
begin
  Result := '[';
  for i in IndexRange do
  begin
    case FRecordedSequence[i] of
      0..31 : { do nothing };
      32..126 :
      begin
        Result := Result + Char(FRecordedSequence[i]) + #32;
      end;
    end;
  end;
  Result := Result + ']' + #10;
end;

procedure TKeySequence.StartFromBegin;
begin
  FIndex := Low(IndexRange);
end;

procedure TKeySequence.StartNewRecording;
var
  i : integer;
begin
  for i := Low(IndexRange) to High(IndexRange) do
    FRecordedSequence[i] := 0;

  FRecIndex := Low(IndexRange);
end;

procedure TKeySequence.RecordKey(AKey: Word);
begin
  FRecordedSequence[FRecIndex] := AKey;
  if FRecIndex < High(IndexRange) then
    Inc(FRecIndex)
  else
    if Assigned(OnEndCapturing) then OnEndCapturing(Self);
end;

constructor TKeySequence.Create(AOwner: TComponent);
var
  i, j : integer;
begin
  inherited Create(AOwner);
  //UseNegation:=False;
  StartFromBegin;
  StartNewRecording;
  for i in IndexRange do
    for j in IndexRange do
        FKeys[i][j] := 0;
end;

destructor TKeySequence.Destroy;
begin
  { do nothing }
  inherited Destroy;
end;

function TKeySequence.NextKeys(AKey: word; RestartOnNonTargetKey: Boolean;
  RestartOnEndSequence: Boolean): TWordRange;
var
  LTargetKeys : TWordRange;
  LTargetKey : Word;
  LTargetKeyFlag : Boolean = False;
  i : integer;
begin
  LTargetKeys := TargetKeys;
  for i := Low(IndexRange) to High(IndexRange) do
    if AKey = LTargetKeys[i] then
    begin
      LTargetKeyFlag := True;
      LTargetKey := AKey;
      Inc(FIndex);
      if Assigned(OnTargetKey) then OnTargetKey(Self);
      Break;
    end;

  if LTargetKey = FinalKey then
  begin
    if RestartOnEndSequence then StartFromBegin;
    if Assigned(OnEndSequence) then OnEndSequence(Self);
  end;

  if not LTargetKeyFlag then
  begin
    if RestartOnNonTargetKey then StartFromBegin;
    if Assigned(OnNonTargetKey) then OnNonTargetKey(Self);
  end;
  NextKeys := TargetKeys;
end;

procedure TKeySequence.LoadFromConfiguration(AConfiguration: TStringList);
var
  i : integer;
  j: integer;
  LWordCount : integer;
  LLetters : string;
begin
  for i in IndexRange do
  begin
    LLetters := AConfiguration.Values['K'+(i+1).ToString];
    LWordCount := WordCount(LLetters,[#32]);
    for j := 1 to LWordCount do
      FKeys[i][j] := Ord(ExtractDelimited(j,LLetters,[#32])[1]);
  end;
end;

procedure TKeySequence.LoadFromChars(AChars: TCharMatrix);
var
  i : integer;
  j : integer;
begin
  for i in IndexRange do
    for j in IndexRange do
      FKeys[i][j] := Ord(AChars[i][j]);
end;

procedure TKeySequence.LoadFromWords(AWords: TWordMatrix);
var
  i : integer;
  j : integer;
begin
  for i in IndexRange do
    for j in IndexRange do
      FKeys[i][j] := AWords[i][j];
end;

end.


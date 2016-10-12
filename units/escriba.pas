{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit escriba;

{$MODE Delphi}

interface

uses Dialogs, Classes, StdCtrls, Graphics, SysUtils, Forms, LazFileUtils
    , config_session
    , constants
    ;

type

       { TEscriba }

       TEscriba = class(TCfgSes)
       private
        //FRegData: TRegData;
        FWriteK : Boolean;
        FMemo : TMemo;
        FText : TStringList;
        FIndBlc : Integer;
        FIndTrial : Integer;
        FIndComp : Integer;
        function GetBlc (BlcIndex : Integer; Closed : Boolean) : string;
        function GetBlcTrial (BlcIndex, TrialIndex : Integer) : string;
        function GetBnd (S : Boolean): string;
        function GetComparison (CompIndex : Integer) : string;
        function GetPosition (PosIndex : Integer) : string;
        procedure UpDateMemoMain;
        procedure UpDateMemoBlc;
        procedure UpDateMemoTrial;
        procedure UpDateMemoComp;
        procedure UpDateMemoPos;
        procedure WriteMain;
        procedure WriteBlc;
        procedure WriteTrial;
        procedure WriteKind (kind : string);
        procedure WriteComp;
        procedure WritePos;
        procedure WriteSupportKeyboard;
        procedure WriteToMemo;
        procedure LoadMemo (Memo : TMemo);
       public
        constructor Create (AOwner : TComponent); override;
        destructor Destroy; override;
        procedure CleanMemoPlease;
        procedure SaveTextToTXT (Name : string);
        procedure SaveMemoTextToTxt(FileName : string);
        procedure SetVariables;
        procedure SetMain;
        procedure SetBlc (n : Integer; Write : Boolean);
        procedure SetTrial (n : integer);
        procedure SetComp (n : Integer);
        procedure SetK(CanWrite : Boolean);
        procedure SetPositions (ArrayofPositions : array of TCoordenates);
        procedure SetPosWriteOnly;
        procedure SetKplusAfterPositiveTrialOnly;
        property Memo : TMemo read FMemo write LoadMemo;
       end;

implementation

//uses formMain, formTextEditor, formPositionGetter;

{Writer}

procedure TEscriba.CleanMemoPlease;
begin
  FMemo.Lines.BeginUpdate;
  FMemo.Lines.Clear;
  FMemo.Lines.EndUpdate;
end;

constructor TEscriba.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetVariables;
end;

destructor TEscriba.Destroy;
begin
  FText.Free;
  FMemo := nil;
  inherited Destroy;
end;

function TEscriba.GetBlc (BlcIndex : Integer; Closed : Boolean) : string;
begin
  if Closed then
    Result := '[' + _Blc + #32 + IntToStr (BlcIndex) + ']'
  else Result := _Blc + #32 + IntToStr (BlcIndex);
end;

function TEscriba.GetBlcTrial(BlcIndex, TrialIndex: Integer): string;
begin
  Result := '[' + GetBlc(BlcIndex, false) + #32 + '-' + #32 + _Trial + IntToStr (TrialIndex) + ']';
end;

function TEscriba.GetBnd (S : Boolean) : string;
var position : integer; bnd : string;
begin
  if S then position := StrToIntDef (FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [_Samp + _cBnd], 1)
  else position := StrToIntDef (FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [GetComparison (FIndComp + 1) + 'Bnd'],1);

  bnd := IntToStr (FPositions[position -1].Top) + #32 +
         IntToStr (FPositions[position -1].Left) + #32 +
         IntToStr (FPositions[position -1].Width) + #32 +
         IntToStr (FPositions[position -1].Height);

  Result := bnd;
end;

function TEscriba.GetComparison(CompIndex: Integer): string;
begin
  Result := _Comp + IntToStr(CompIndex);
end;

function TEscriba.GetPosition(PosIndex: Integer): string;
begin
  Result := _Pos + IntToStr(PosIndex);
end;

procedure TEscriba.UpDateMemoMain;
begin

end;

procedure TEscriba.UpDateMemoBlc;
begin

end;

procedure TEscriba.UpDateMemoTrial;
begin

end;

procedure TEscriba.UpDateMemoComp;
begin

end;

procedure TEscriba.UpDateMemoPos;
begin

end;

procedure TEscriba.LoadMemo(Memo: TMemo);
begin
  FMemo := Memo;
end;

procedure TEscriba.SaveTextToTXT(Name : String);
var a1: integer;s1,s2: string;// Memo : TMemo;
begin
  ForceDirectoriesUTF8(ExtractFilePath(Name)); { *Converted from ForceDirectories*  }
  a1 := 0;
  s1:= Copy(Name, 0, Length(Name)-4);
  s2:= Copy(Name, Length(Name)-3, 4);
  while FileExistsUTF8(name) { *Converted from FileExists*  } do
    begin
      Inc(a1);
      name := s1 + StringOfChar(#48, 3 - Length(IntToStr(a1))) + IntToStr(a1) + s2;
    end;
  //Memo := TMemo.Create(Self);
  //FText.Text := Memo.Text;
  FText.SaveToFile(Name);
  FText.Clear;
end;

procedure TEscriba.SaveMemoTextToTxt(FileName: string);
var a1 : integer;
    s1, s2 : string;
begin
  ForceDirectoriesUTF8(ExtractFilePath(Filename)); { *Converted from ForceDirectories*  }
  a1 := 0;
  s1:= Copy(Filename, 0, Length(Filename)-4);
  s2:= Copy(Filename, Length(Filename)-3, 4);
  while FileExistsUTF8(Filename) { *Converted from FileExists*  } do
    begin
      Inc(a1);
      name := s1 + StringOfChar(#48, 3 - Length(IntToStr(a1))) + IntToStr(a1) + s2;
    end;
  //Memo := TMemo.Create(Self);
  //FText.Text := Memo.Text;
  FMemo.Lines.SaveToFile(Filename);
end;

procedure TEscriba.SetBlc(n: Integer; Write : Boolean);
begin
  FIndBlc := n;
  if Write then
    begin
      WriteBlc;
      WriteToMemo;
    end;
end;

procedure TEscriba.SetComp(n: Integer);
begin
  FIndComp := n;
  WriteComp;
end;

procedure TEscriba.SetK(CanWrite: Boolean);
begin
  FWriteK := CanWrite;
end;

procedure TEscriba.SetKplusAfterPositiveTrialOnly;
var OldString, NewString, OldPattern, NewPattern : string;
    Flags : TReplaceFlags;
begin
  OldPattern := _Comp + '1'+ KRes + T_HIT;
  NewPattern := OldPattern + KEnter +
                _Kplus + KUsb + '1';
  Flags := [rfReplaceAll];
  OldString := FMemo.Text;

  NewString := StringReplace (OldString, OldPattern, NewPattern, Flags);
  Memo.Text := NewString;
end;

procedure TEscriba.SetMain;
begin
  WriteMain;
  WriteToMemo;
end;

procedure TEscriba.SetPosWriteOnly;
begin
  WritePos;
  WriteToMemo;
end;

procedure TEscriba.SetPositions(ArrayofPositions: array of TCoordenates);
var i, size : integer;
begin
      size := Length (ArrayofPositions);
      FNumPos := size;
      SetLength (FPositions, size);
      for i := 0 to size - 1 do
        begin
          FPositions[i] := ArrayofPositions[i];
        end;

  WritePos;
  WriteToMemo;
end;

procedure TEscriba.SetTrial(n: integer);
begin
  FIndTrial := n;
  WriteTrial;
  WriteToMemo;
end;

procedure TEscriba.SetVariables;
begin
  FText := TStringList.Create;
  FText.Sorted := False;
  FText.Duplicates := dupIgnore;
  //FRegData := TRegData.Create(Self, GetCurrentDir + PathDelim + 'Files Settings' + PathDelim + 'Sessão_000.txt');
end;

procedure TEscriba.WriteBlc;
begin
  FText.Add (
             GetBlc(FIndBlc + 1, True) + KEnter + KEnter +
             KName        + FBlcs[FIndBlc].Name + KEnter +
             KBackGround  + IntToStr(FBlcs[FIndBlc].BkGnd) + KEnter +
             KITInterval  + IntToStr (FBlcs[FIndBlc].ITI) + KEnter +
             //KMaxCorrection      + IntToStr (FVetCfgBlc[FIndBlc].MaxCorrection) + KEnter +
             KNumTrial    + IntToStr(FBlcs[FIndBlc].NumTrials) + #32 + IntToStr(FBlcs[FIndBlc].VirtualTrialValue) + KEnter
             //KCrtConsecutiveHit   + IntToStr (FVetCfgBlc[FIndBlc].CrtConsecutiveHit) + KEnter +
             //KCrtConsecutiveMiss   + IntToStr (FVetCfgBlc[FIndBlc].CrtConsecutiveMiss) + KEnter +
             //KCrtMaxTrials   + IntToStr (FVetCfgBlc[FIndBlc].CrtMaxTrials) + KEnter
             );
end;

procedure TEscriba.WriteComp;
var
    i : integer;
    //aType : integer;
    aComp : string;

const
  MirroredStimuli = 0;
  FeaturePositive = 1;
  SimpleDiscrimin = 2;
  ConditDiscrimin = 3;

  function GetTrialType (Kind : string) : integer;
  begin
    Result := -1;
    if Kind = T_MRD then Result := 0;
    if Kind = T_FPE then Result := 1;
    if Kind = T_Simple then Result := 2;
    if Kind = T_MTS then Result := 3;
  end;

begin
  i := FIndComp + 1;
  aComp := GetComparison (i);

  case GetTrialType(FBlcs[FIndBlc].Trials[FIndTrial].Kind) of
    MirroredStimuli..FeaturePositive:
                      FText.Add(
                        aComp + KBnd +
                          FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[aComp + _cBnd] + KEnter +
                        aComp + KStm +
                          FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[aComp + _cStm] + KEnter +
                        aComp + KcGap +
                          FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[aComp + _cGap] + KEnter +
                        aComp + KcGap_Degree +
                          FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[aComp + _cGap_Degree] + KEnter +
                        aComp + KcGap_Length +
                          FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[aComp + _cGap_Length] + KEnter
                        );

    SimpleDiscrimin..ConditDiscrimin :
                      FText.Add(
                        aComp + KBnd + GetBnd (false) + KEnter +
                        aComp + KStm +
                          FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [aComp + _cStm] + KEnter +
                        aComp + KSch +
                          FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [aComp + _cSch] + KEnter +
                        aComp + KIET +
                          FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [aComp + _cIET] + KEnter +
                        aComp + KUsb +
                          FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [aComp + _cUsb] + KEnter +
                        aComp + KCsq +
                          FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [aComp + _cCsq] + KEnter +
                        aComp + KMsg +
                          FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [aComp + _cMsg] + KEnter +
                        aComp + KRes +
                          FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [aComp + _cRes] + KEnter +
                        aComp + KNxt +
                          FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [aComp + _cNxt] + KEnter +
                        aComp + KTO +
                          FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [aComp + _cTO] + KEnter
                        );

  end;
end;

procedure TEscriba.WriteKind(kind: string);
var i : Integer; NumComp: String;
begin

  if kind = T_FPE then
    begin
      NumComp := IntToStr(FBlcs[FIndBlc].Trials[FIndTrial].NumComp);
      FText.Add(
                KNumComp + NumComp + KEnter +
                KUseMedia +
                  FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[_UseMedia] + KEnter +
                KShowStarter +
                  FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[_ShowStarter] + KEnter +
                KLimitedHold +
                    FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[_LimitedHold] + KEnter +
                KSchedule +
                  FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[_Schedule] + KEnter +
                KExpectedResponse +
                  FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[_ExpectedResponse] + KEnter +
                _Trial + KIET +
                  FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[_Trial + _cIET] + KEnter +
                KNextTrial +
                  FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[_NextTrial] + KEnter
                );
      for i := 0 to StrToInt (NumComp) - 1 do SetComp (i);
    end;

  if kind = T_MRD then
    begin
      NumComp := IntToStr(FBlcs[FIndBlc].Trials[FIndTrial].NumComp);
      FText.Add(
                KNumComp + NumComp + KEnter +
                KUseMedia +
                  FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[_UseMedia] + KEnter +
                KShowStarter +
                  FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[_ShowStarter] + KEnter +
                KAngle +
                  FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[_Angle] + KEnter +
                KSchedule +
                  FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[_Schedule] + KEnter +
                KNextTrial +
                  FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[_NextTrial] + KEnter
                );
      for i := 0 to StrToInt (NumComp) - 1 do SetComp (i);
    end;

  if kind = T_Msg then
    begin
        FText.Add(
                KMsg +
                  FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[_cMsg] + KEnter +
                KWidth +
                  FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[_MsgWidth] + KEnter +
                KFontSize +
                  FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[_MsgFontSize]  + KEnter +
                KFontColor +
                  FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[_MsgFontColor] + KEnter +
                KBackGround +
                  FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[_BkGnd] + KEnter +
                KPrompt +
                  FBlcs[FIndBlc].Trials[FIndTrial].SList.Values[_Prompt] + KEnter);
    end;

  if kind = T_Simple then
    begin
      NumComp := IntToStr(FBlcs[FIndBlc].Trials[FIndTrial].NumComp);
      FText.Add(KNumComp + NumComp + KEnter);
      for i := 0 to StrToInt (NumComp) - 1 do SetComp (i);
      if FWriteK then WriteSupportKeyboard;
    end;

  if kind = T_MTS then
    begin
      NumComp := IntToStr(FBlcs[FIndBlc].Trials[FIndTrial].NumComp);
      FText.Add(
                KComAtraso +
                  FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [_Delayed] + KEnter +
                KAtraso +
                  FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [_Delay] + KEnter +
                _Samp + KBnd + GetBnd(True) + KEnter +
                _Samp + KStm +
                  FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [_Samp + _cStm] + KEnter +
                _Samp + KSch +
                  FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [_Samp + _cSch] + KEnter +
                _Samp + KMsg +
                  FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [_Samp + _cMsg] + KEnter + KEnter +
                KNumComp + NumComp + KEnter
                );
      for i := 0 to StrToInt (NumComp) - 1 do SetComp (i);
      if FWriteK then WriteSupportKeyboard;
    end;
end;

procedure TEscriba.WriteMain;
begin
  FText.Add(
            KMain  + KEnter +
            KName  + SessionName + KEnter +
            KSubject + SessionSubject + KEnter +
            KType  + SessionType + KEnter +
            KRootMedia + GlobalContainer.RootMedia + KEnter +
            KRootData  + GlobalContainer.RootData + KEnter +
            KServer + SessionServer + KEnter +
            KNumBlc  + IntToStr(FNumBlc) + KEnter
            );
end;

procedure TEscriba.WritePos;
var i : Integer;
begin
  FText.Add(KPositions + KEnter +
            KRows + IntToStr (FRow)+ KEnter +
            KCols + IntToStr (FCol)+ KEnter +
            KNumPos + IntToStr(FNumPos) + KEnter);

  for i := 0 to FNumPos - 1 do
    begin
      FText.Add(GetPosition (i + 1) + '=' + #9 + IntToStr(FPositions[i].Top) + #32 +
                                                 IntToStr(FPositions[i].Left) + #32 +
                                                 IntToStr(FPositions[i].Width) + #32 +
                                                 IntToStr(FPositions[i].Height));
    end;
end;

procedure TEscriba.WriteSupportKeyboard;
begin
  FText.Add(
            KEnter +
            _Kplus + KMsg + rmKeyPlus + KEnter +
            _Kplus + KCsq +
              FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [_Kplus + _cCsq] + KEnter +
            _Kplus + KUsb +
              FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [_Kplus + _cUsb] + KEnter +
            _Kplus + KRes + T_NONE + KEnter +
            _Kplus + KNxt +
              FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [_Kplus + _cNxt] + KEnter +
            KEnter +
            _Kminus + KMsg + rmKeyMinus + KEnter +
            _Kminus + KCsq +
              FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [_Kminus + _cCsq] + KEnter +
            _Kminus + KUsb +
              FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [_Kminus + _cUsb] + KEnter +
            _Kminus + KRes + T_NONE + KEnter +
            _Kminus + KNxt +
              FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [_Kminus + _cNxt]
            );
end;

procedure TEscriba.WriteTrial;
begin
  FText.Add(
            GetBlcTrial(FIndBlc + 1, FIndTrial + 1) + KEnter + KEnter +
            KName +
              FBlcs[FIndBlc].Trials[FIndTrial].Name + KEnter +
            KKind +
              FBlcs[FIndBlc].Trials[FIndTrial].Kind + Kenter +
            KBackGround +
              FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [_BkGnd] + KEnter +
            KCursor +
              FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [_Cursor] + KEnter
            //KAutoNext + FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [_AutoNxt] + KEnter +
            //KCustomNext + FBlcs[FIndBlc].Trials[FIndTrial].SList.Values [_CustomNxtValue] + KEnter
            );
  WriteKind(FBlcs[FIndBlc].Trials[FIndTrial].Kind);
end;

procedure TEscriba.WriteToMemo;
begin
 FMemo.Lines.BeginUpdate;
 FMemo.Lines.AddStrings(FText);
 FMemo.Lines.EndUpdate;
 FText.Clear;
end;

end.

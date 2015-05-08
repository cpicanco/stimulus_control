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
unit config_session;

{$mode objfpc}{$H+}

interface

uses Classes, ComCtrls, IniFiles, SysUtils,  Dialogs, Forms
    , constants
    ;

type

  TCircle = record
    o : TPoint;  //Left/Top
    size : integer;
    gap : Boolean;
    gap_degree : integer;
    gap_length : integer;
  end;

  TCurrentTrial = record
    C : array of TCircle; //circles
    //K : array of TKey; // not implemented yet
    i : integer; //trial index
    NextTrial : string;
    angle : Extended; // angle from "userconfigs_trial_mirrored" form,
    response : string;
    Result : string;
  end;

  TCfgTrial = record
    Id : integer;
    Name: string;
    Kind: string;
    NumComp: integer;
    SList: TStringList;
  end;

  TVetCfgTrial = array of TCfgTrial;

  TCfgBlc = record
    Name: string;
    ITI: Integer;
    BkGnd: Integer;

    NumTrials: Integer;
    VirtualTrialValue: Integer;

    MaxCorrection: Integer;
    DefNextBlc: string;

    CrtConsecutiveHit: Integer;
    CrtConsecutiveMiss : Integer;
    CrtMaxTrials : Integer;
    CrtKCsqHit : Integer;
    Trials: TVetCfgTrial;

    //VetCrtBlc: array of TCrtBlc;
  end;

  TVetCfgBlc = array of TCfgBlc;

  TCoordenates = record
    Index  : Integer;
    Top    : Integer;
    Left   : Integer;
    Width  : Integer;
    Height : Integer;
  end;

  TCIniFile = class (TIniFile)
  public
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
  end;

  { TCfgSes }

  TCfgSes = class(TComponent)
  protected
    //Fddd: Cardinal;
    FBlcs: TVetCfgBlc;
    FCol: integer;
    FData: string;
    FEdtMode : Boolean;
    FIniFile: TCIniFile;
    FLoaded : Boolean;
    FMedia: string;
    FName: string;
    FNumBlc: Integer;
    FNumPos: Integer;
    FPositions : array of TCoordenates;
    FRootData: string;
    FRootMedia: string;
    FRow: integer;
    FSubject: string;
    FType : string;
    function GetCfgBlc(Ind: Integer): TCfgBlc;
  published
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadFromFile(FileName: string; EdtMode : Boolean): Boolean;
    function LoadTreeFromFile(FileName:string; aTree : TTreeView):Boolean;
    procedure SetLengthVetBlc;
    procedure SetLengthVetTrial (Blc : Integer);
    property Blcs : TVetCfgBlc read FBlcs write FBlcs;
    property CfgBlc[Ind: Integer]: TCfgBlc read GetCfgBlc;
    property Col : integer read FCol write FCol;
    property Data : string read FData write FData;
    property EditMode : Boolean read FEdtMode write FEdtMode;
    property IsLoaded : Boolean read FLoaded write FLoaded;
    property Media: string read FMedia write FMedia;
    property Name: string read FName write FName;
    property NumBlc: Integer read FNumBlc write FNumBlc;
    property NumPos: Integer read FNumPos write FNumPos;
    property RootData: string read FRootData write FRootData;
    property RootMedia: string read FRootMedia write FRootMedia;
    property Row : integer read FRow write FRow;
    property SesType : string read FType write FType;
    property Subject : string read FSubject write FSubject;
  end;
resourcestring
  messuCfgSessionMainError = 'Valor não encontrado: Main, Name or NumBlc.';
  messLoading = 'Carregando...';
  messReady = 'Pronto.';
  messRoot = 'Estrutura';
  messLevel_01 = 'Sessão 1';
  messLevel_04_M = 'Mensagem';
  DefName = 'No name';
  DefSubject = 'No subject';

implementation

//uses formMainLaz;

function TCfgSes.GetCfgBlc(Ind: Integer): TCfgBlc;
begin
  Result:= FBlcs[Ind];
end;

constructor TCfgSes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLoaded := False;
end;

destructor TCfgSes.Destroy;
var a1, a2 : Integer;
begin
  if Length(FBlcs) > 0 then
    for a1:= Low(FBlcs) to High(FBlcs) do
      begin
        if Length(FBlcs[a1].Trials) > 0 then
          for a2:= Low(FBlcs[a1].Trials) to High(FBlcs[a1].Trials) do
             FBlcs[a1].Trials[a2].SList.Free;
      end;
  inherited Destroy;
end;

procedure TCIniFile.ReadSectionValues(const Section: string; Strings: TStrings);
var
  KeyList: TStringList;
  I: Integer;
begin
  KeyList := TStringList.Create;
  //KeyList.Sorted := False;
  KeyList.CaseSensitive := False;
  KeyList.Duplicates := dupIgnore;
  try
    ReadSection(Section, KeyList);
    //showmessage(Keylist.Text);
    //Strings.BeginUpdate;
    //try
      for I := 0 to KeyList.Count - 1 do
        Strings.Add(KeyList[I] + '=' + ReadString(Section, KeyList[I], ''))
    //finally
    //  Strings.EndUpdate;
    //end;
  finally
    KeyList.Free;
  end;
end;

procedure TCfgSes.SetLengthVetBlc;
begin
  SetLength(FBlcs, FNumBlc);
end;

procedure TCfgSes.SetLengthVetTrial(Blc: Integer);
var trial : integer;
begin
  SetLength(FBlcs[Blc].Trials, FBlcs[Blc].NumTrials);
  for trial := 0 to FBlcs[blc].NumTrials - 1 do
    begin
      if not Assigned(FBlcs[blc].Trials[trial].SList)then
        begin
          FBlcs[blc].Trials[trial].SList:= TStringList.Create;
          FBlcs[blc].Trials[trial].SList.Sorted := False;
          FBlcs[blc].Trials[trial].SList.Duplicates := dupIgnore;
          FBlcs[blc].Trials[trial].SList.BeginUpdate;
          FBlcs[blc].Trials[trial].SList.Add('new=true');
          FBlcs[blc].Trials[trial].SList.EndUpdate;
        end;
    end;
end;

function TCfgSes.LoadFromFile(FileName: string; EdtMode : Boolean): Boolean;
var
  a1, a2, a3, a4: Integer; s1: string; NumList : TStringList;

    function GetBarPosition(NowNumber, MaxNumber : integer):integer;
    begin
      Result := (100 * NowNumber) div MaxNumber;
    end;

    procedure ListCreate;
    begin
      NumList := TStringList.Create;
      //NumList.Sorted := False;
      NumList.Duplicates := dupIgnore;
    end;
begin
  if FileExists(FileName) then begin
    FIniFile:= TCIniFile.Create(FileName);
    with FIniFile do begin
      //frmMain.Statusbar1.Panels[0].Text := messLoading;
      if  SectionExists(_Main) and
          ValueExists(_Main, _Name) and
          ValueExists(_Main, _NumBlc) then begin

        FName := ReadString(_Main, _Name, DefName);
        FSubject := ReadString(_Main, _Subject, DefSubject);
        FNumBlc := ReadInteger(_Main, _NumBlc, 0);
        FType := ReadString(_Main, _Type, T_CIC);

        if (FType = T_CIC) or (FType = T_CRT) then
        else FType := T_CIC;

        // set media and data paths
        s1 := ReadString(_Main, _RootMedia, '');
        while Pos(PathDelim, s1) <> 0 do Delete(s1, 1, Pos(PathDelim, s1));
        FRootMedia:= ExtractFilePath(FileName) + s1;
        if not (FRootMedia[Length(FRootMedia)] = PathDelim) then FRootMedia:= FRootMedia + PathDelim;

        s1 := ReadString(_Main, _RootData, '');
        while Pos(PathDelim, s1) <> 0 do Delete(s1, 1, Pos(PathDelim, s1));
        FRootData:= ExtractFilePath(FileName) + s1;
        if not (FRootData[Length(FRootData)] = PathDelim) then FRootData:= FRootData + PathDelim;

        //Fddd := GetTickCount;
        SetLength(FBlcs, FNumBlc);
        for a1:= Low(FBlcs) to High(FBlcs) do
          begin
            with FBlcs[a1] do
              begin
                Name:= ReadString(_Blc + #32 + IntToStr(a1+1), _Name, '');
                BkGnd:= ReadInteger(_Blc + #32 + IntToStr(a1+1), _BkGnd, 0);
                ITI:= ReadInteger(_Blc + #32 + IntToStr(a1+1), _ITI, 0);
                CrtConsecutiveHit := ReadInteger(_Blc + #32 + IntToStr(a1+1), _CrtConsecutiveHit, -1);
                CrtConsecutiveMiss := ReadInteger(_Blc + #32 + IntToStr(a1+1), _CrtConsecutiveMiss, -1);
                CrtMaxTrials:= ReadInteger(_Blc + #32 + IntToStr(a1+1), _CrtMaxTrials, -1);
                CrtKCsqHit := ReadInteger(_Blc + #32 + IntToStr(a1+1), _CsqCriterion, -1);

                s1:= ReadString(_Blc + #32 + IntToStr(a1+1), _NumTrials, '0 0');
                NumTrials:= StrToIntDef(Copy(s1, 0, pos(' ', s1)-1), 0);
                Delete(s1, 1, pos(' ', s1)); If Length(s1)>0 then While s1[1]=' ' do Delete(s1, 1, 1);
                VirtualTrialValue:= StrToIntDef(s1, 0);
                DefNextBlc:= ReadString(_Blc + #32 + IntToStr(a1+1), _DefNextBlc, '');
                MaxCorrection:= ReadInteger(_Blc + #32 + IntToStr(a1+1), _MaxCorrection, 0);
              end;
            SetLength(FBlcs[a1].Trials, FBlcs[a1].NumTrials);
            for a2:= Low(FBlcs[a1].Trials) to High(FBlcs[a1].Trials) do
              begin
                with FBlcs[a1].Trials[a2] do
                  begin
                    Id :=  a2 + 1;
                    Name:=
                      ReadString(_Blc + #32 + IntToStr(a1+1) + ' - ' + _Trial + IntToStr(a2+1),_Name, '');
                    Kind:=
                      ReadString(_Blc + #32 + IntToStr(a1+1) + ' - ' + _Trial + IntToStr(a2+1), _Kind, '');
                    NumComp :=
                      ReadInteger(_Blc + #32 + IntToStr(a1+1) + ' - ' + _Trial + IntToStr(a2+1), _NumComp, 0);
                    SList:= TStringList.Create;
                    with SList do
                      begin
                        //SList.Sorted := False;
                        CaseSensitive := False;
                        Duplicates := dupIgnore;
                        BeginUpdate;
                        ReadSectionValues(_Blc + #32 + IntToStr(a1+1) + ' - ' + _Trial + IntToStr(a2+1), SList);
                        Add(_RootMedia + '=' + FRootMedia);
                        EndUpdate;
                      end;
                  end;
                //frmMain.Statusbar1.Panels[1].Text := '[' + _Blc + IntToStr(a1+1) + ' - ' + _Trial +IntToStr(a2+1) + ']';
                //frmMain.ProgressBar1.Position := GetBarPosition(a2, FVetCfgBlc[a1].NumTrials-1);
                Application.ProcessMessages;
              end;
          end;
        //ShowMessage(FormatFloat('#####,###',GetTickCount - Fddd));
        if EdtMode then
          begin
            If  SectionExists(_Positions) and
                ValueExists(_Positions, _NumPos) then begin
              FMedia := ExtractFileName(ExcludeTrailingPathDelimiter(FRootMedia));
              FData := ExtractFileName(ExcludeTrailingPathDelimiter(FRootData));
              FNumPos := ReadInteger(_Positions, _NumPos, 0);
              FRow := ReadInteger(_Positions, _Rows, 0);
              FCol := ReadInteger(_Positions, _Cols, 0);
              SetLength(FPositions, FNumPos);
              for a4 := 0 to FNumPos - 1 do
                begin
                  s1:= ReadString(_Positions, _Pos + IntToStr(a4 + 1), '0');
                  with FPositions[a4] do
                    begin
                      Index := a4 + 1;
                      Top:= StrToIntDef(Copy(s1, 0, pos(' ', s1)-1), 0); Delete(s1, 1, pos(' ', s1)); If Length(s1)>0 then While s1[1]=' ' do Delete(s1, 1, 1);
                      Left:= StrToIntDef(Copy(s1, 0, pos(' ', s1)-1), 0); Delete(s1, 1, pos(' ', s1)); If Length(s1)>0 then While s1[1]=' ' do Delete(s1, 1, 1);
                      Width:= StrToIntDef(Copy(s1, 0, pos(' ', s1)-1), 0); Delete(s1, 1, pos(' ', s1)); If Length(s1)>0 then While s1[1]=' ' do Delete(s1, 1, 1);
                      Height:= StrToIntDef(s1, 0);
                    end;
                end;
              ListCreate;
              for a4 := 0 to FNumPos - 1 do NumList.Add(ReadString(_Positions, _Pos + IntToStr(a4 + 1), '0'));

              for a1:= Low(FBlcs) to High(FBlcs) do
                for a2:= Low(FBlcs[a1].Trials) to High(FBlcs[a1].Trials) do
                  begin
                    FBlcs[a1].Trials[a2].SList.BeginUpdate;
                    with FBlcs[a1].Trials[a2].SList do
                      begin
                        if NumList.IndexOf(Values[_Samp + _cBnd]) = -1 then
                          Values[_Samp + _cBnd] := '1'
                        else
                          begin
                            Values[_Samp + _cBnd] :=
                            IntToStr(NumList.IndexOf(Values[_Samp + _cBnd]) + 1);
                          end;
                      end;
                    if FBlcs[a1].Trials[a2].NumComp > 0 then
                      for a3 := 0 to (FBlcs[a1].Trials[a2].NumComp - 1) do
                          with FBlcs[a1].Trials[a2].SList do
                            begin
                              if NumList.IndexOf(Values[_Comp + IntToStr(a3 + 1) + _cBnd]) = -1 then
                                Values[_Comp+IntToStr(a3+1)+_cBnd]:= '1'
                              else
                                Values[_Comp+IntToStr(a3+1)+_cBnd]:= IntToStr(NumList.IndexOf(Values[_Comp+IntToStr(a3+1)+_cBnd])+1);
                            end;

                    FBlcs[a1].Trials[a2].SList.EndUpdate;
                  end;
              NumList.Free;
            end;
          end;
        FLoaded := True; //Diferencia "Abrir Sessão" de "Nova sessão".
        //frmMain.Statusbar1.Panels[0].Text := messReady;
        //frmMain.Statusbar1.Panels[1].Text := FName;
        FIniFile.Free;
        Result:= True;
      end else Result:= False;
    end;
  end else Result:= False;
end;

function TCfgSes.LoadTreeFromFile(FileName: string; aTree: TTreeView): Boolean;
//from INI like files,  encoded as UTF-8 without BOM (Ansi as UTF-8)
//this functionality is not finished yet
{TODO -oRafael -cFunctionality: Load Config File From Tree List View Component}
var

  i1, i2, i3, aNumBlc, aNumTrials, aNumComp : Integer;
  ANode : TTreeNode;
  CurrStr, aux: string;
begin
  Result:= False;
  if FileExists(FileName) then
    begin
      FIniFile:= TCIniFile.Create(FileName);
      aTree.BeginUpdate;
      with FIniFile do
        try
          if  SectionExists(_Main) and
              ValueExists(_Main, _Name) and
              ValueExists(_Main, _NumBlc) then
            begin
              aTree.Items.Clear;
              CurrStr := messRoot;
              ANode := aTree.Items.AddChild(nil, CurrStr);   //Level 0

              CurrStr := ReadString(_Main, _Name, messLevel_01);
              ANode := aTree.Items.AddChild(ANode, CurrStr); //Level 1

              aNumBlc := ReadInteger(_Main, _NumBlc, 0);
              for i1 := 0 to aNumBlc - 1 do
                begin
                  aux := _Blc + #32 + IntToStr(i1 + 1);
                  CurrStr :=  ReadString(aux, _Name, aux);
                  while ANode.Level > 1 do ANode := ANode.Parent; //Level 2
                  ANode := aTree.Items.AddChild(ANode, CurrStr);

                  aux:= ReadString(_Blc + #32 + IntToStr(i1 + 1), _NumTrials, '0 0');
                  aNumTrials:= StrToIntDef(Copy(aux, 0, pos(' ', aux)-1), 0);
                  for i2 := 0 to aNumTrials -1 do
                    begin
                       aux := _Blc + #32 + IntToStr(i1 + 1) + ' - ' + _Trial + IntToStr(i2 + 1);
                       CurrStr := ReadString(aux,_Name, aux);
                       while ANode.Level > 2 do ANode := ANode.Parent; //Level 3
                           ANode := aTree.Items.AddChild(ANode, CurrStr);

                       if ReadString(aux,_Kind, T_NONE) = T_Simple then
                          begin
                            aNumComp := ReadInteger(_Blc + #32 + IntToStr(i1 + 1) + ' - ' + _Trial + IntToStr(i2 + 1), _NumComp, 0);
                            for i3 := 0 to aNumComp -1 do
                              begin
                                CurrStr:= ReadString(aux,_Comp + IntToStr(i3 + 1) + _cStm,_Comp + IntToStr(i3 + 1) + _cStm);
                                while ANode.Level > 3 do ANode := ANode.Parent;
                                ANode := aTree.Items.AddChild(ANode,CurrStr); //Level 4
                              end;
                          end
                        else if ReadString(aux,_Kind, T_NONE) = T_MTS then
                          begin
                            aNumComp := ReadInteger(_Blc + #32 + IntToStr(i1 + 1) + ' - ' + _Trial + IntToStr(i2 + 1), _NumComp, 0);
                            for i3 := 0 to aNumComp do
                              begin
                                if i3 = 0 then
                                  begin
                                    CurrStr:= ReadString(aux,_Samp + _cStm, _Samp + _cStm);
                                    while ANode.Level > 3 do ANode := ANode.Parent;  //Level 4
                                    ANode := aTree.Items.AddChild(ANode,CurrStr);
                                  end
                                else
                                  begin
                                    CurrStr:= ReadString(aux,_Comp + IntToStr(i3) + _cStm,_Comp + IntToStr(i3) + _cStm);
                                    while ANode.Level > 3 do ANode := ANode.Parent; //Level 4
                                    ANode := aTree.Items.AddChild(ANode.Parent,CurrStr);
                                  end;
                              end;
                          end
                        else if ReadString(aux,_Kind, T_NONE) = T_MSG then
                          begin
                            CurrStr:= messLevel_04_M;
                            while ANode.Level > 3 do ANode := ANode.Parent;    //Level 4
                            ANode := aTree.Items.AddChild(ANode,CurrStr);
                          end
                        else if ReadString(aux,_Kind, T_NONE) = T_NONE then
                          begin
                            CurrStr:= T_NONE;
                            while ANode.Level > 3 do ANode := ANode.Parent;   //Level 4
                            ANode := aTree.Items.AddChild(ANode,CurrStr);
                          end
                    end;
                end;
              Result := True;
            end
          else ShowMessage(messuCfgSessionMainError);
        finally
          FIniFile.Free;
          aTree.EndUpdate;
        end;
  end;
end;

end.








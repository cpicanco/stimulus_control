unit userconfigs_blocs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, LvlGraphCtrl, Grids;

type

  TBlcAddress = record
    SessionIndex : integer;
    BlocIndex : integer;
    Path : string;
  end;

  { TSimpleBlc }

  TSimpleBlc = class(TComponent)
  strict private
    FName: string;
    FNextBlcOnCriteria: integer;
    FNextBlcOnNotCriteria: integer;
    function AddressToString:string;
    function GetAsSection : TStrings;
    function GetOriginalIndex: integer;
    function GetTrialSection(Index : integer): TStrings;
    procedure SetBlocName(AValue: string);
    procedure SetNextBlcOnCriteria(AValue: integer);
    procedure SetNextBlcOnNotCriteria(AValue: integer);
    procedure StringToAddress(AValue : string);
  public
    Address : TBlcAddress;
    MenuItem : TMenuItem;
    property OriginalIndex : integer read GetOriginalIndex;
    property BlocName : string read FName write SetBlocName;
    property NextBlcOnCriteria : integer read FNextBlcOnCriteria write SetNextBlcOnCriteria;
    property NextBlcOnNotCriteria : integer read FNextBlcOnNotCriteria write SetNextBlcOnNotCriteria;
    property AddressAsString : string read AddressToString write StringToAddress;
    property AsSection : TStrings read GetAsSection;
    property TrialSection[Index : integer] : TStrings read GetTrialSection;
  end;

  TSimpleBlcs = array of TSimpleBlc;

  TSimpleSession = record
    Filename : string;
    MenuItem : TMenuItem;
    Blocs : TSimpleBlcs;
  end;

  TSessions = array of TSimpleSession;

  { TFormBlocs }

  TFormBlocs = class(TForm)
    LvlGraphControl1: TLvlGraphControl;
    PopupMenuBlocs: TPopupMenu;
  private
    FBlocsPath: string;
    FChoosenBlocs : TStringList;
    FOnBlocItemClick: TNotifyEvent;
    FSessions : TSessions;
    FStringGrid: TStringGrid;
    function GetCount: integer;
    procedure BlocItemClick(Sender : TObject);
    function GetItems(BlocIndex : integer): TSimpleBlc; overload;
    function GetItems(AAddress : string): TSimpleBlc; overload;
    procedure SetAddressHelper(var i, j : integer; AAddress : string; SN : integer = 1; BN : integer = 2);
    procedure SetBlocsPath(AValue: string);
    //procedure SetItems(SessionIndex, BlocIndex : integer; AValue: TSimpleBlc);
    procedure SetOnBlocItemClick(AValue: TNotifyEvent);
    { private declarations }
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure AppendBlocToStringGrid(AShortAddress : string); overload;
    procedure AppendBlocToStringGrid(ABlc : integer); overload;
    procedure InvalidateGraph;
    procedure WriteToDisk(ADefaultMainSection: TStrings; ADefaultBlocSection:TStrings);
    property BlocsPath : string read FBlocsPath write SetBlocsPath;
    property Count : integer read GetCount;
    property Items[BlocIndex : integer] : TSimpleBlc read GetItems; default;
    property OnBlocItemClick : TNotifyEvent read FOnBlocItemClick write SetOnBlocItemClick;
    property StringGrid : TStringGrid read FStringGrid write FStringGrid;
  end;

var
  FormBlocs: TFormBlocs;

operator = (B1, B2 : TBlcAddress) B : Boolean;

operator in (const A: TSimpleSession; const B: TSessions): boolean;

operator in (const A: TSimpleBlc; const B: TSimpleBlcs): boolean;


implementation

uses config_session, config_session_fileutils, strutils,FileUtil, LazFileUtils, constants;

operator=(B1, B2: TBlcAddress)B: Boolean;
begin
  if (B1.Path=B2.Path) and (B1.BlocIndex=B2.BlocIndex) then
    Result := True
  else
    Result := False;
end;

operator in(const A: TSimpleSession; const B: TSessions): boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(B) to High(B) do
    begin
      if B[i].Filename = A.Filename then
        Result := True;
      Break;
    end;
end;

operator in(const A: TSimpleBlc; const B: TSimpleBlcs): boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(B) to High(B) do
    begin
      if B[i].Address = A.Address then
        Result := True;
      Break;
    end;
end;

{$R *.lfm}

{ TSimpleBlc }

procedure TSimpleBlc.StringToAddress(AValue: string);
begin
  if WordCount(AValue,[#32]) < 2 then
    raise Exception.Create('StringToAddress failed with value:'+AValue);

  Address.Path:=ExtractDelimited(1,AValue,[#32]);
  Address.SessionIndex:=StrToInt(ExtractDelimited(2,AValue,[#32]));;
  Address.BlocIndex := StrToInt(ExtractDelimited(3,AValue,[#32]));
end;

function TSimpleBlc.AddressToString: string;
begin
  Result := Address.Path + #32 + IntToStr(Address.SessionIndex) + #32 + IntToStr(Address.BlocIndex);
end;

function TSimpleBlc.GetAsSection: TStrings;
begin
  Result := nil;
  AbstractError;
end;

function TSimpleBlc.GetOriginalIndex: integer;
begin
  Result := Address.BlocIndex+1;
end;

function TSimpleBlc.GetTrialSection(Index : integer): TStrings;
begin
  Result := nil;
  AbstractError;
end;

procedure TSimpleBlc.SetBlocName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TSimpleBlc.SetNextBlcOnCriteria(AValue: integer);
begin
  if FNextBlcOnCriteria=AValue then Exit;
  FNextBlcOnCriteria:=AValue;
end;

procedure TSimpleBlc.SetNextBlcOnNotCriteria(AValue: integer);
begin
  if FNextBlcOnNotCriteria=AValue then Exit;
  FNextBlcOnNotCriteria:=AValue;
end;

{ TFormBlocs }

function TFormBlocs.GetCount: integer;
begin
  Result := FChoosenBlocs.Count;
end;

procedure TFormBlocs.SetBlocsPath(AValue: string);
var
  LSession : string;
  LSessionFiles : TStringList;
  LConfigurationFile : TConfigurationFile;
  s,
  i: Integer;
  LCfgBloc : TCfgBlc;
  M : TMenuItem;
begin
  if FBlocsPath=AValue then Exit;
  FBlocsPath:=AValue;

  s := 0;
  LSessionFiles := FindAllFiles(FBlocsPath,'*.txt');
  for LSession in LSessionFiles do
    begin
      LConfigurationFile := TConfigurationFile.Create(LSession);
      if LConfigurationFile.BlocCount > 0 then
        begin
          SetLength(FSessions,s+1);
          FSessions[s].Filename := LSession;
          FSessions[s].MenuItem := TMenuItem.Create(PopupMenuBlocs);
          FSessions[s].MenuItem.Caption := IntToStr(s+1)+': '+ExtractFileNameOnly(LSession);
          PopupMenuBlocs.Items.Add(FSessions[s].MenuItem);
          SetLength(FSessions[s].Blocs,LConfigurationFile.BlocCount);
          for i := 0 to LConfigurationFile.BlocCount -1 do
            begin
              LCfgBloc := LConfigurationFile.Bloc[i+1];
              FSessions[s].Blocs[i] := TSimpleBlc.Create(FSessions[s].MenuItem);
              FSessions[s].Blocs[i].Address.SessionIndex:=s;
              FSessions[s].Blocs[i].Address.BlocIndex := i;
              FSessions[s].Blocs[i].Address.Path := LConfigurationFile.FileName;
              FSessions[s].Blocs[i].Name := 'B'+IntToStr(i+1);
              FSessions[s].Blocs[i].BlocName:= LCfgBloc.Name;
              FSessions[s].Blocs[i].NextBlcOnCriteria:= LCfgBloc.NextBlocOnCriteria;
              FSessions[s].Blocs[i].NextBlcOnNotCriteria:= LCfgBloc.NextBlocOnNotCriteria;
              FSessions[s].Blocs[i].MenuItem := TMenuItem.Create(FSessions[s].Blocs[i]);
              FSessions[s].Blocs[i].MenuItem.Caption := IntToStr(i+1)+': '+ LCfgBloc.Name;
              FSessions[s].Blocs[i].MenuItem.OnClick := OnBlocItemClick;
              PopupMenuBlocs.Items[s].Add(FSessions[s].Blocs[i].MenuItem);
            end;
            M := TMenuItem.Create(PopupMenuBlocs);
            M.Caption:='Blocos';
            M.Enabled:=False;
            PopupMenuBlocs.Items[s].Insert(0,M);
            Inc(s);
         end;
     LConfigurationFile.Free;
    end;
    M := TMenuItem.Create(PopupMenuBlocs);
    M.Caption:='Sessões';
    M.Enabled:=False;
    PopupMenuBlocs.Items.Insert(0,M);
end;

procedure TFormBlocs.BlocItemClick(Sender: TObject);
var
  LBlocIndex : integer;
  LSessionIndex : integer;
  LShortAddress: string;
begin
  LBlocIndex := StrToInt(ExtractDelimited(1,TMenuItem(Sender).Caption,[':']));
  LSessionIndex := StrToInt(ExtractDelimited(1,TMenuItem(Sender).Parent.Caption,[':']));
  LShortAddress := IntToStr(LSessionIndex-1)+#32+IntToStr(LBlocIndex-1);
  FChoosenBlocs.Append(LShortAddress);
  AppendBlocToStringGrid(LShortAddress);
end;

function TFormBlocs.GetItems(BlocIndex : integer): TSimpleBlc;
var
  i : integer = 0;
  j : integer = 0;
begin
  SetAddressHelper(i,j, FChoosenBlocs[BlocIndex]);
  Result := FSessions[i].Blocs[j];
end;

function TFormBlocs.GetItems(AAddress: string): TSimpleBlc;
var
  i : integer = 0;
  j : integer = 0;
begin
  SetAddressHelper(i,j, AAddress, 2, 3);
  Result := FSessions[i].Blocs[j];
end;

procedure TFormBlocs.SetAddressHelper(var i, j: integer; AAddress: string;
  SN: integer; BN: integer);
begin
  i := StrToInt(ExtractDelimited(SN,AAddress,[#32]));
  j := StrToInt(ExtractDelimited(BN,AAddress,[#32]));
end;

procedure TFormBlocs.SetOnBlocItemClick(AValue: TNotifyEvent);
begin
  if FOnBlocItemClick=AValue then Exit;
  FOnBlocItemClick:=AValue;
end;

constructor TFormBlocs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChoosenBlocs := TStringList.Create;
  OnBlocItemClick:=@BlocItemClick;
end;

destructor TFormBlocs.Destroy;
begin
  FChoosenBlocs.Free;
  inherited Destroy;
end;

procedure TFormBlocs.AppendBlocToStringGrid(AShortAddress: string);
var
  i : integer = 0;
  j : integer = 0;
begin
  SetAddressHelper(i, j,AShortAddress);
  with StringGrid do
    begin
      if Cells[0,RowCount-1] <> '' then RowCount := RowCount+1;
      Cells[0, RowCount-1] := IntToStr(RowCount-1);
      Cells[1, RowCount-1] := FSessions[i].Blocs[j].BlocName;
      Cells[2, RowCount-1] := IntToStr(FSessions[i].Blocs[j].NextBlcOnCriteria);
      Cells[3, RowCount-1] := IntToStr(FSessions[i].Blocs[j].NextBlcOnNotCriteria);
      Cells[4, RowCount-1] := FSessions[i].Blocs[j].AddressAsString;
    end;
end;

procedure TFormBlocs.AppendBlocToStringGrid(ABlc: integer);
begin
  AppendBlocToStringGrid(FChoosenBlocs[ABlc]);
end;

procedure TFormBlocs.InvalidateGraph;
var
  LSource : string;
  LTarget1 : string;
  LTarget2 : string;
  LRow, n : integer;

  function SourceNode : string;
  begin
    with StringGrid do
      Result := Cells[0, LRow] +'|'+ Cells[1, LRow];
  end;

  function TargetEdge(ACol : integer) : string;
  begin
    with StringGrid do
      begin
        n := StrToInt(Cells[ACol, LRow]);
        if n > 0 then
          // custom bloc
          Result := Cells[ACol, LRow] +'|'+ Cells[1,n]
        else
          begin
            // next bloc
            n := LRow+1;
            if n <= RowCount-1 then
              Result := Cells[0, n] +'|'+Cells[1, n];
          end;

        // end bloc
        if n > RowCount-1 then
          Result := 'FIM';
      end;
  end;

begin
  LvlGraphControl1.Graph.Clear;
  with StringGrid do
    for LRow := 1 to RowCount-1 do
      begin
        // number | name
        LSource := SourceNode;
        LTarget1 := TargetEdge(2);
        LTarget2 := TargetEdge(3);
        if LRow = 1 then
          LvlGraphControl1.Graph.GetEdge('INÍCIO', LSource, True);
        LvlGraphControl1.Graph.GetEdge(LSource, LTarget1, True);
        LvlGraphControl1.Graph.GetEdge(LSource, LTarget2, True);
      end;
end;

procedure TFormBlocs.WriteToDisk(ADefaultMainSection: TStrings;
  ADefaultBlocSection: TStrings);
var
  LRow : integer;
  i : integer = 0;
  j : integer = 0;
  FNewSession : TConfigurationFile;
  FTargetSession : TConfigurationFile;
  LDestination : string;
begin
  LDestination := BlocsPath+DirectorySeparator+LAST_BLOCS_INI_FILENAME;
  if FileExistsUTF8(LDestination) then
    DeleteFileUTF8(LDestination);
  FNewSession := TConfigurationFile.Create(LDestination);
  FNewSession.CacheUpdates:=True;
  FNewSession.WriteMain(ADefaultMainSection);
  with StringGrid do
    for LRow := 1 to RowCount-1 do
      begin
        try
          // ReadFromAddress
          SetAddressHelper(i, j,Cells[4, LRow], 2, 3);
          FTargetSession := TConfigurationFile.Create(FSessions[i].Blocs[j].Address.Path);

          // Keep changes in memory and do not call FTargetSession.UpdateFile
          FTargetSession.CacheUpdates:=True;

          // Check for empty keys and apply defaults
          FTargetSession.WriteBlocIfEmpty(FSessions[i].Blocs[j].OriginalIndex, ADefaultBlocSection);

          // Override some keys in memory
          with FTargetSession do
            begin
              WriteToBloc(FSessions[i].Blocs[j].OriginalIndex,_Name,Cells[1, LRow]);
              WriteToBloc(FSessions[i].Blocs[j].OriginalIndex,_NextBlocOnCriteria,Cells[2, LRow]);
              WriteToBloc(FSessions[i].Blocs[j].OriginalIndex,_NextBlocOnNotCriteria,Cells[3, LRow])
            end;

          // copy from target
          FNewSession.WriteBlocFromTarget(FSessions[i].Blocs[j].OriginalIndex,FTargetSession);
        finally
          FTargetSession.Free;
        end;
      end;

  // Save changes to disk
  FNewSession.UpdateFile;
  FNewSession.Free;
end;


end.


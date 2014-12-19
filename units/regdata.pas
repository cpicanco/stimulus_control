unit regdata;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, FileUtil;

type

  { TRegData }

  TRegData = class(TComponent)
  private
    FITIBEGIN: DWord;
    FITIEND: DWord;
    FLatencyLblBegin: Dword;
    FLatencyLblResponse: Dword;
    FLatencyStmBegin: Dword;
    FLatencyStmResponse: Dword;
    FFile: TextFile;
    FSessionNumber: integer;
  public
    constructor Create(AOwner: TComponent; FileName: String); reintroduce;
    destructor Destroy; override;
    procedure SaveData(Data: string);
    property SessionNumber : integer read FSessionNumber write FSessionNumber;
    property LatencyLblBegin : Dword read FLatencyLblBegin write FLatencyLblBegin;
    property LatencyLblResponse : Dword read FLatencyLblResponse write FLatencyLblResponse;
    property LatencyStmBegin : Dword read FLatencyStmBegin write FLatencyStmBegin;
    property LatencyStmResponse : Dword read FLatencyStmResponse write FLatencyStmResponse;
    property ITIBEGIN : DWord read FITIBEGIN write FITIBEGIN;
    property ITIEND : DWord read FITIEND write FITIEND;
  end;

implementation

constructor TRegData.Create(AOwner: TComponent; FileName: string);
var a1: Integer; s1, s2: string;
begin
  inherited Create(AOwner);

  ForceDirectoriesUTF8(ExtractFilePath(FileName)); { *Converted from ForceDirectories*  }

  a1:= 1;
  s1:= Copy(FileName, 0, Length(FileName)-8);
  s2:= Copy(FileName, Length(FileName)-3, 4);
  while FileExistsUTF8(FileName) { *Converted from FileExists*  } do begin
    Inc(a1);
    FileName:= s1 + '_' + StringOfChar(#48, 3 - Length(IntToStr(a1))) + IntToStr(a1) + s2;
  end;
  FSessionNumber := a1;
  AssignFile(FFile, FileName);
  Rewrite(FFile);
end;

destructor TRegData.Destroy;
begin
  CloseFile(FFile);
  inherited Destroy;
end;

procedure TRegData.SaveData(Data: string);
begin
  Write(FFile, Data);
end;

end.


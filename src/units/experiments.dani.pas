unit Experiments.Dani;

{$mode objfpc}{$H+}

interface

procedure MakeConfigurationFile(ACondition, ASessionBlocs : integer);

var
  ConfigurationFilename : string;

const
  SessionBlocs = 1;

implementation

uses Classes, SysUtils, Forms
   , Constants
   , FileMethods
   , LazFileUtils
   , Session.ConfigurationFile
   , Session.Configuration.GlobalContainer
   ;

type

  TResponseStyle = (Go, NoGo);
  //TScreenSide = (ssLeft, ssRight);
  TExperimentalCategory = (Sex, Rape, Control);

  TGoNoGoTrial = record
    SampleFilename : string;
    ComparFilename : string;
    ResponseStyle: TResponseStyle;
    Category : TExperimentalCategory;
    ID : byte;
  end;

  TGoNoGoTrials = array of TGoNoGoTrial;
  TGoNoGoTrialsArray = array of TGoNoGoTrials;

var
  // rape
  Class1AStimuli : TStringArray;
  Class1BStimuli : TStringArray;
  Class1CStimuli : TStringArray;

  // sex
  Class2AStimuli : TStringArray;
  Class2BStimuli : TStringArray;
  Class2CStimuli : TStringArray;

  TrialsRapeABGo : TGoNoGoTrials;
  TrialsRapeABNoGo : TGoNoGoTrials;
  TrialsRapeBCGo : TGoNoGoTrials;
  TrialsRapeBCNoGo : TGoNoGoTrials;
  TrialsRapeACGo : TGoNoGoTrials;
  TrialsRapeACNoGo : TGoNoGoTrials;

  TrialsSexABGo : TGoNoGoTrials;
  TrialsSexABNoGo : TGoNoGoTrials;
  TrialsSexBCGo : TGoNoGoTrials;
  TrialsSexBCNoGo : TGoNoGoTrials;
  TrialsSexACGo : TGoNoGoTrials;
  TrialsSexACNoGo : TGoNoGoTrials;

  Message1 : string;

const
  FolderClass1A =
   'classe-1-estupro'+DirectorySeparator+'A'+DirectorySeparator;

  FolderClass1B =
   'classe-1-estupro'+DirectorySeparator+'B'+DirectorySeparator;

  FolderClass1C =
   'classe-1-estupro'+DirectorySeparator+'C'+DirectorySeparator;

  FolderClass2A =
   'classe-2-sexo'+DirectorySeparator+'A'+DirectorySeparator;

  FolderClass2B =
   'classe-2-sexo'+DirectorySeparator+'B'+DirectorySeparator;

  FolderClass2C =
   'classe-2-sexo'+DirectorySeparator+'C'+DirectorySeparator;

  Folders : array [0..5] of string = (
    FolderClass1A,
    FolderClass1B,
    FolderClass1C,
    FolderClass2A,
    FolderClass2B,
    FolderClass2C);

  LimitedHold = 0;

  ITI = 1250;

procedure RandomizeStimuli(var Rand : array of integer);
var
  i, t, r, l : integer;
begin
  l := Length(Rand);
  for i := Low(Rand) to High(Rand) do
  begin
    r := Random(l);
    t := Rand[r];
    Rand[r] := Rand[i];
    Rand[i] := t;
  end;
end;

procedure SetupStimuli;
var
  Folder : String;

  procedure MountTrialsFor(out AGoNoGoTrials: TGoNoGoTrials;
    AStimuli1, AStimuli2 : TStringArray;
    AExperimentalCategory : TExperimentalCategory;
    AResponseStyle : TResponseStyle;
    AID : byte);
  var
    i, j, t : integer;
  begin
    i := Length(AStimuli1);
    j := Length(AStimuli2);
    SetLength(AGoNoGoTrials, i*j);
    t := 0;
    for i := Low(AStimuli1) to High(AStimuli1) do
      for j := Low(AStimuli2) to High(AStimuli2) do
      begin
        AGoNoGoTrials[t].SampleFilename := AStimuli1[i];
        AGoNoGoTrials[t].ComparFilename := AStimuli2[j];
        AGoNoGoTrials[t].Category := AExperimentalCategory;
        AGoNoGoTrials[t].ResponseStyle := AResponseStyle;
        AGoNoGoTrials[t].ID := AID;
        Inc(t);
      end;
  end;

  procedure LoadMessageFromFile(var AMessage : string; AFilename : string);
  var
    LStringList : TStringList;
  begin
    LStringList := TStringList.Create;
    try
      LStringList.LoadFromFile(AFilename);
      AMessage := LStringList.Text;
    finally
      LStringList.Free;
    end;
  end;

begin
  LoadMessageFromFile(Message1, GlobalContainer.RootMedia+'Mensagem1.txt');
  for Folder in Folders do
    ForceDirectories(GlobalContainer.RootMedia+Folder);

  FindFilesFor(Class1AStimuli, GlobalContainer.RootMedia+FolderClass1A);
  FindFilesFor(Class1BStimuli, GlobalContainer.RootMedia+FolderClass1B);
  FindFilesFor(Class1CStimuli, GlobalContainer.RootMedia+FolderClass1C);
  FindFilesFor(Class2AStimuli, GlobalContainer.RootMedia+FolderClass2A);
  FindFilesFor(Class2BStimuli, GlobalContainer.RootMedia+FolderClass2B);
  FindFilesFor(Class2CStimuli, GlobalContainer.RootMedia+FolderClass2C);

  MountTrialsFor(TrialsRapeABGo,   Class1AStimuli, Class1BStimuli, Rape, Go, 0);
  MountTrialsFor(TrialsRapeABNoGo, Class1AStimuli, Class2BStimuli, Rape, NoGo, 1);
  MountTrialsFor(TrialsSexABGo,    Class2AStimuli, Class2BStimuli, Sex,  Go, 2);
  MountTrialsFor(TrialsSexABNoGo,  Class2AStimuli, Class1BStimuli, Sex,  NoGo, 3);

  MountTrialsFor(TrialsRapeBCGo,   Class1BStimuli, Class1CStimuli, Rape, Go, 4);
  MountTrialsFor(TrialsRapeBCNoGo, Class1BStimuli, Class2CStimuli, Rape, NoGo, 5);
  MountTrialsFor(TrialsSexBCGo,    Class2BStimuli, Class2CStimuli, Sex,  Go, 6);
  MountTrialsFor(TrialsSexBCNoGo,  Class2BStimuli, Class1CStimuli, Sex,  NoGo, 7);

  MountTrialsFor(TrialsRapeACGo,   Class1AStimuli, Class1CStimuli, Rape, Go, 8);
  MountTrialsFor(TrialsRapeACNoGo, Class1AStimuli, Class2CStimuli, Rape, NoGo, 9);
  MountTrialsFor(TrialsSexACGo,    Class2AStimuli, Class2CStimuli, Sex,  Go, 10);
  MountTrialsFor(TrialsSexACNoGo,  Class2AStimuli, Class1CStimuli, Sex,  NoGo, 11);
end;

procedure NewConfigurationFile;
begin
  ConfigurationFilename := ExtractFilePath(
    Application.ExeName) + DirectorySeparator + 'last_session.ini';
  if FileExists(ConfigurationFilename) then
    DeleteFile(ConfigurationFilename);
  ConfigurationFile := TConfigurationFile.Create(ConfigurationFilename);
  ConfigurationFile.CacheUpdates := True;
  ConfigurationFile.WriteString(_Main, _NumBlc, '3');
  ConfigurationFile.WriteToBloc(1, _Name, 'SS');
  ConfigurationFile.Invalidate;
end;

// https://stackoverflow.com/questions/20190110/2d-int-array-shuffle
function RandomizeTrials(var ATrials : TGoNoGoTrialsArray) : TGoNoGoTrials;
var
  r, t, j, i :  integer;
  temp : TPoint;
  LTrialsMask : array of array of TPoint;
begin
  // initialize
  SetLength(LTrialsMask, Length(ATrials));
  for i := Low(LTrialsMask) to High(LTrialsMask) do
  begin
    SetLength(LTrialsMask[i], Length(ATrials[i]));
    for j := Low(LTrialsMask[i]) to High(LTrialsMask[i]) do
      LTrialsMask[i][j] := Point(i,j);
  end;

  for i := Low(LTrialsMask) to High(LTrialsMask) do
  begin
    for j := Low(LTrialsMask[i]) to High(LTrialsMask[i]) do
    begin
        r := Random(i+1);
        t := Random(j+1);
        temp := LTrialsMask[i, j];
        LTrialsMask[i, j] := LTrialsMask[r, t];
        LTrialsMask[r, t] := temp;
    end;
  end;

  SetLength(Result, 0);
  for i := Low(LTrialsMask) to High(LTrialsMask) do
  begin
    for j := Low(LTrialsMask[i]) to High(LTrialsMask[i]) do
    begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)] := ATrials[LTrialsMask[i][j].x][LTrialsMask[i][j].y];
    end;
  end;
  //for i := Low(Result) to High(Result) do
  //  WriteLn(Result[i].ID);
end;

procedure MakeConfigurationFile(ACondition, ASessionBlocs: integer);
{
  bloco 1
  1 mensagem
  18 tentativas 8go/8nogo
  18 tentativas 8go/8nogo
  1 contadores de acerto e erro do bloco
}
  procedure WriteMSGTrial(ABlc : integer);
  var
    i : integer;
  begin
    i := ConfigurationFile.TrialCount[1]+1;
    with ConfigurationFile do
    begin
      WriteToTrial(i, ABlc, _Name, 'Mensagem 1');
      WriteToTrial(i, ABlc, _Kind, T_MSG);
      WriteToTrial(i, ABlc, _ITI, ITI.ToString);
      WriteToTrial(i, ABlc, _Msg, Message1)
    end;
  end;

  procedure WriteBloc(Experimental : Boolean; ABlc : integer);
    procedure WriteReviewTrial;
    var
      i : integer;
    begin
      i := ConfigurationFile.TrialCount[ABlc]+1;
      with ConfigurationFile do
      begin
        WriteToTrial(i, ABlc,_Name, 'Review');
        WriteToTrial(i, ABlc, _Cursor, '-1');
        WriteToTrial(i, ABlc, _Kind, T_PFR);
      end;
    end;

    procedure WriteTrial(ATrial : TGoNoGoTrial);
    var
      i : integer;
      SampleStimulus, ComparStimulus, Category, ResponseStyle : string;
      function ExtractMediaName(AFilename: string): string;
      begin
        Result := AFilename;
        Delete(Result,1,Pos('media', Result)+Length('media'));
      end;

    begin
      i := ConfigurationFile.TrialCount[ABlc]+1;
      with ConfigurationFile do
      begin
        WriteStr(Category, ATrial.Category);
        WriteStr(ResponseStyle, ATrial.ResponseStyle);
        SampleStimulus := ExtractFileNameWithoutExt(ExtractFileName(ATrial.SampleFilename));
        ComparStimulus := ExtractFileNameWithoutExt(ExtractFileName(ATrial.ComparFilename));
        WriteToTrial(i, ABlc, _Name,
          ('').Join(#32,
            [
              SampleStimulus+'|'+ComparStimulus,
              Category,
              ResponseStyle
            ]));
        WriteToTrial(i, ABlc, _Cursor, '1');
        WriteToTrial(i, ABlc, _Kind, T_GNG);
        WriteToTrial(i, ABlc, _ResponseStyle, ResponseStyle);
        WriteToTrial(i, ABlc, _Category, Category);
        WriteToTrial(i, ABlc, _LimitedHold, LimitedHold.ToString);
        WriteToTrial(i, ABlc, _ITI, ITI.ToString);
        WriteToTrial(i, ABlc, _Samp+_cStm, ExtractMediaName(ATrial.SampleFilename));
        WriteToTrial(i, ABlc, _Comp+'1'+_cStm, ExtractMediaName(ATrial.ComparFilename));
      end;
    end;

    procedure WriteGoNoGoTrials;
    var
      i : integer;
      LStructuredTrials : TGoNoGoTrialsArray;
      LTrialsToWrite : TGoNoGoTrials;
    begin
      LStructuredTrials := TGoNoGoTrialsArray.Create(
        TrialsRapeABGo,
        TrialsRapeABNoGo,
        TrialsSexABGo,
        TrialsSexABNoGo);
      LTrialsToWrite := RandomizeTrials(LStructuredTrials);
      for i := Low(LTrialsToWrite) to High(LTrialsToWrite) do
        WriteTrial(LTrialsToWrite[i]);

      LStructuredTrials := TGoNoGoTrialsArray.Create(
        TrialsRapeBCGo,
        TrialsRapeBCNoGo,
        TrialsSexBCGo,
        TrialsSexBCNoGo);
      LTrialsToWrite := RandomizeTrials(LStructuredTrials);
      for i := Low(LTrialsToWrite) to High(LTrialsToWrite) do
        WriteTrial(LTrialsToWrite[i]);

      LStructuredTrials := TGoNoGoTrialsArray.Create(
        TrialsRapeACGo,
        TrialsRapeACNoGo,
        TrialsSexACGo,
        TrialsSexACNoGo);
      LTrialsToWrite := RandomizeTrials(LStructuredTrials);
      for i := Low(LTrialsToWrite) to High(LTrialsToWrite) do
        WriteTrial(LTrialsToWrite[i]);
    end;
  begin
    WriteGoNoGoTrials;
    WriteReviewTrial;
  end;

var
  i: Integer;
begin
  NewConfigurationFile;
  case ACondition of
    0 :
      for i := 1 to ASessionBlocs do
      begin
        ConfigurationFile.WriteToBloc(i, _Name, 'Experimental '+i.ToString);
        if i = 1 then WriteMSGTrial(i);
        WriteBloc(True, i);
      end;
    1 :
      for i := 1 to ASessionBlocs do
      begin
        ConfigurationFile.WriteToBloc(i, _Name, 'Controle '+i.ToString);
        if i = 1 then WriteMSGTrial(i);
        WriteBloc(False, i);
      end;
  end;
  ConfigurationFile.Invalidate;
  ConfigurationFile.UpdateFile;
end;

operator = (A, B: TGoNoGoTrial) : Boolean;
begin
  if (A.ID = B.ID) or (A.ResponseStyle = B.ResponseStyle) then
    Result := True
  else
    Result := False;
end;

initialization
  Randomize;
  SetupStimuli;

end.

{
  Stimulus Control
  Copyright (C) 2014-2016 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit git_vertioning;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils

  , Process
  , Pipes
  , FileUtil
  ;

function ReadProcessOutput(Output : TInputPipeStream) : TStringList;
function GetCommitTag(UseTagsOption : Boolean) : TStringList;

function CurrentVersion(CommitTag : TStringList) : string;
function LastCommitShortCode(IncludeLocalCommits: Boolean) : string;

implementation

function GetCommitTag(UseTagsOption: Boolean): TStringList;
var
  GitProcess : TProcess;
  GitPath : string;
  procedure EmptyResult;
  begin
    Result := TStringList.Create;
    Result.Append('v0.0.0.0-0-0000000');
  end;

begin

  {$ifdef UNIX}
    GitPath := FindDefaultExecutablePath('git');
  {$endif}

  {$ifdef WINDOWS}
    GitPath := FindDefaultExecutablePath('git');
  {$endif}

  GitProcess := TProcess.Create(nil);
  try
    if not (GitPath = '') then
      begin
        GitProcess.Executable := GitPath;
        GitProcess.Parameters.Append('describe');
        GitProcess.Parameters.Append('--always');
        if UseTagsOption then GitProcess.Parameters.Append('--tags');
        GitProcess.Options := [poUsePipes];
        GitProcess.Execute;
        Result := ReadProcessOutput(GitProcess.Output);
      end
    else EmptyResult;
  finally
    GitProcess.Free;
  end;
end;

// http://wiki.freepascal.org/Executing_External_Programs#Reading_large_output
function ReadProcessOutput(Output: TInputPipeStream): TStringList;
var
  NumBytes, BytesRead: LongInt;
  MemStream: TMemoryStream;
const
  READ_BYTES = 2048;
begin
  BytesRead := 0;
  MemStream := TMemoryStream.Create;
  try
    while True do
      begin
        // make sure we have room
        MemStream.SetSize(BytesRead + READ_BYTES);

        // try reading it
        NumBytes := Output.Read((MemStream.Memory + BytesRead)^, READ_BYTES);
        if NumBytes > 0 then
          begin
            Inc(BytesRead, NumBytes);
          end
        else Break;
      end;
    MemStream.SetSize(BytesRead);
    Result := TStringList.Create;
    Result.LoadFromStream(MemStream);
  finally
    // clean the room
    MemStream.Free;
  end;
end;

{
  [version tag]-[commits since last tag]-[last commit code]

  CommitTag[0] :  inline output, e.g.: v0.0.0.0-0-0000000
  VersionLines[0] : version tag, e.g.: v0.0.0.0
  VersionLines[1] : commits since last tag
  VersionLines[2] : leading 7 digit tag commit code
}
function CurrentVersion(CommitTag: TStringList): string;
var VersionLines : TStringList;
begin
  VersionLines := TStringList.Create;
  try
    with VersionLines do
      begin
        StrictDelimiter := True;
        Delimiter := '-';
        DelimitedText := CommitTag.Text;
      end;
    Result := #32 + VersionLines[0] + #32 + 'c' + VersionLines[1];
  finally
    VersionLines.Free;
  end;
end;

function LastCommitShortCode(IncludeLocalCommits: Boolean): string;
var VersionLines : TStringList;
begin
  if IncludeLocalCommits then
    Result := GetCommitTag(False).Text
  else
    begin
      VersionLines := TStringList.Create;
      try
        with VersionLines do
          begin
            StrictDelimiter := True;
            Delimiter := '-';
            DelimitedText := GetCommitTag(True).Text;
          end;
        Result := VersionLines[2];
      finally
        VersionLines.Free;
      end;
    end;
end;

end.


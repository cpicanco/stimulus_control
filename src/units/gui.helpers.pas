{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit GUI.Helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Grids, Forms, Spin, Dialogs;

type

  TGuiMain = record
    SessionType : TComboBox;
    SessionName : TEdit;
    Participant : TEdit;
    RootMedia : TEdit;
    RootData : TEdit;
    PupilAddress : TEdit;
    Blocs : TStringGrid;
  end;

  TGuiBloc = record
    BlocName : TEdit;
    BlocITI : TSpinEdit;
    Color : TColorButton;
    VirtualTrial : TSpinEdit;
    Trials : TStringGrid;
    CrtHitPorcentage : TSpinEdit;
    CrtConsecutiHits : TSpinEdit;
    MaxCorrection : TSpinEdit;
    MaxRepetition : TSpinEdit;
    Counter : TComboBox;
  end;

procedure SetGridHeader(AGrid : TStringGrid; ATrialType:string);
procedure AddTrialsToGui(ATrialForm : TForm; ATrialGrid : TStringGrid);
procedure TrialFromGui(var ATrialSection : TStrings; ATrialType : string; ATrialIndex : integer; ATrialGrid : TStringGrid);
procedure BlocFromGui(var ABlocSection : TStringList; AGuiBloc : TGuiBloc);
procedure MainFromGui(var AMainSection : TStringList; AGuiMain : TGuiMain);

resourcestring
  rsFillTypeAxes = 'Eixos';
  rsFillTypeMatriz = 'FP/FN';
  rsFillTypeGoNoGo = 'Go/No-Go';
  rsFillTypeMTS = 'MTS e Simples';
  rsFillTypeBlocChaining = 'Encadear Blocos';

  rsMTSRelation = 'Relação';
  rsTrials = 'Tentativas';
  rsConsequence = 'Consequência';
  rsAngle = 'Ângulo';
  rsExpectedResponse = 'Resposta';
  rsContingency = 'Contingência';

  rsSize = 'Tamanho';
  rsSchedule = 'Esquema';
  rsStimulus = 'Figura';
  rsLimitedHold = 'Tempo/Estímulo';


  rsBlocs = 'Bloco';
  rsBlocName = 'Nome';
  rsBlocNextBlocOnCriteria = 'Bloco seguinte se critério for atingido';
  rsBlocNextBlocOnNotCriteria = 'Bloco seguinte se critério não for atingido';
  rsBlocAddress = 'Endereço';
implementation

uses constants, Graphics;

procedure SetGridHeader(AGrid: TStringGrid; ATrialType: string);
begin
  with AGrid do
    begin
      Clean;
      RowCount := 2;
      if ATrialType = rsFillTypeAxes then
        begin
          ColCount := 9;
          Cells[0, 0] := rsTrials;
          Cells[1, 0] := rsAngle;
          Cells[2, 0] := 'x0';
          Cells[3, 0] := 'y0';
          Cells[4, 0] := 'x1';
          Cells[5, 0] := 'y1';
          Cells[6, 0] := rsExpectedResponse;
          Cells[7, 0] := rsSize;
          Cells[8, 0] := rsSchedule;
        end;

      if ATrialType = rsFillTypeMatriz then
        begin
          ColCount := 2;
          Cells[0, 0] := rsTrials;
          Cells[1, 0] := rsContingency;
        end;

      if ATrialType = rsFillTypeGoNoGo then
        begin
          ColCount := 6;
          Cells[0, 0] := rsTrials;
          Cells[1, 0] := rsConsequence;
          Cells[2, 0] := rsSchedule;
          Cells[3, 0] := rsLimitedHold;
          Cells[4, 0] := rsStimulus;
          Cells[5, 0] := rsSize;
        end;

      if ATrialType = rsFillTypeMTS then
        begin
          ColCount := 3;
          Cells[0, 0] := rsTrials;
          Cells[1, 0] := rsMTSRelation;
          Cells[2, 0] := rsConsequence;
        end;

      if ATrialType = rsFillTypeBlocChaining then
        begin
          ColCount := 5;
          Cells[0, 0] := rsBlocs;
          Cells[1, 0] := rsBlocName;
          Cells[2, 0] := rsBlocNextBlocOnCriteria;
          Cells[3, 0] := rsBlocNextBlocOnNotCriteria;
          Cells[4, 0] := rsBlocAddress;
        end;
    end;
end;

procedure AddTrialsToGui(ATrialForm: TForm; ATrialGrid: TStringGrid);
begin

end;

procedure TrialFromGui(var ATrialSection: TStrings; ATrialType: string;
  ATrialIndex: integer; ATrialGrid: TStringGrid);
begin

end;

procedure BlocFromGui(var ABlocSection: TStringList; AGuiBloc: TGuiBloc);
var
  LVirtualTrial : string;
  LNumTrials : string;

const
  PERFORMANCE_COUNTER = 1;

  function GetCounterType(Index : integer) : string;
  begin
    case Index of
      PERFORMANCE_COUNTER:Result := _Counter_Performance;
      else
        Result := T_NONE;
    end;
  end;

begin
  ABlocSection.BeginUpdate;
  with AGuiBloc do
    begin
      if Assigned(BlocName) then
        ABlocSection.Values[_Name] := BlocName.Text
      else
        ABlocSection.Values[_Name] := 'Bloco';

      if Assigned(Color) then
        ABlocSection.Values[_BkGnd] := IntToStr(Color.ButtonColor)
      else
        ABlocSection.Values[_BkGnd] := IntToStr(clWhite);

      if Assigned(BlocITI) then
        ABlocSection.Values[_ITI] :=  IntToStr(BlocITI.Value)
      else
        ABlocSection.Values[_ITI] := '0';

      if Assigned(Counter) then
        ABlocSection.Values[_Counter] := GetCounterType(Counter.ItemIndex)
      else
        ABlocSection.Values[_Counter] := T_NONE;

      if Assigned(CrtConsecutiHits) then
        ABlocSection.Values[_CrtConsecutiveHit] := IntToStr(CrtConsecutiHits.Value)
      else
        ABlocSection.Values[_CrtConsecutiveHit] := '0';

      if Assigned(CrtHitPorcentage) then
        ABlocSection.Values[_CrtHitPorcentage] := IntToStr(CrtHitPorcentage.Value)
      else
        ABlocSection.Values[_CrtHitPorcentage] := '0';

      if Assigned(MaxCorrection) then
        ABlocSection.Values[_MaxCorrection] := IntToStr(MaxCorrection.Value)
      else
        ABlocSection.Values[_MaxCorrection] := '0';

      if Assigned(MaxRepetition) then
        ABlocSection.Values[_MaxBlcRepetition] := IntToStr(MaxRepetition.Value)
      else
        ABlocSection.Values[_MaxBlcRepetition] := '0';

      if Assigned(VirtualTrial) then
        LVirtualTrial := IntToStr(VirtualTrial.Value)
      else
        LVirtualTrial := '1';

      if Assigned(Trials) then
        LNumTrials := IntToStr(Trials.RowCount -1)
      else
        LNumTrials:= '1';

      ABlocSection.Values[_NumTrials] := LNumTrials+#32+LVirtualTrial;
    end;
  ABlocSection.EndUpdate;
end;

procedure MainFromGui(var AMainSection: TStringList; AGuiMain: TGuiMain);
const
  styCRT = 1;
  styCND = 2;

  function GetType(Index:integer):string;
  begin
    case Index of
      styCRT : Result := T_CRT;
      styCND : Result := T_CND;
      else
        Result := T_CIC;
      end;
  end;
begin
  AMainSection.BeginUpdate;
  with AGuiMain do
    begin
      if Assigned(SessionName) then
        AMainSection.Values[_Name] := SessionName.Text
      else
        AMainSection.Values[_Name] := 'Sessão 1';

      if Assigned(Participant) then
        AMainSection.Values[_Subject] := Participant.Text
      else
        AMainSection.Values[_Subject] := 'Participant 1';

      if Assigned(SessionType) then
        AMainSection.Values[_Type] := GetType(SessionType.ItemIndex)
      else
        AMainSection.Values[_Type] := GetType(-1);

      if Assigned(RootMedia) then
        AMainSection.Values[_RootMedia] := RootMedia.Text
      else
        AMainSection.Values[_RootMedia] := 'Media';

      if Assigned(RootData) then
        AMainSection.Values[_RootData] := RootData.Text
      else
        AMainSection.Values[_RootData] := 'Data';

      if Assigned(Blocs) then
        AMainSection.Values[_NumBlc] := IntToStr(Blocs.RowCount -1)
      else
        AMainSection.Values[_NumBlc] := '';

      if Assigned(PupilAddress) then
        AMainSection.Values[_ServerAddress] := PupilAddress.Text
      else
        AMainSection.Values[_ServerAddress] := DefaultAddress;
    end;
  AMainSection.EndUpdate;
end;


end.


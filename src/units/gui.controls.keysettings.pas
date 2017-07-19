{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit GUI.Controls.KeySettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, Graphics, Dialogs
  , Controls.Stimuli.Key
  ;

type

  { TGUIKeySettings }

  TGUIKeySettings = class(TGroupBox)
    GBStimulus : TGroupBox;
      LabelSFilename : TLabel;
      EditSFilename : TEdit;

      LabelSRepeat : TLabel;
      ComboSRepeat : TComboBox;

      LabelSColor : TLabel;
      ButtonSColor : TColorButton;

    GBResponse : TGroupBox;
      LabelSchedule : TLabel;
      EditSchedule : TEdit;

    GBConseque : TGroupBox;
      LabelSampleStyle : TLabel;
      ComboSampleStyle : TComboBox;

      LabelSampleDelay : TLabel;
      EditSampleDelay : TEdit;

      LabelResult : TLabel;
      ComboResult : TComboBox;

      LabelCFilename : TLabel;
      EditCFilename : TEdit;

      LabelCRepeat : TLabel;
      ComboCRepeat : TComboBox;

      LabelCColor : TLabel;
      ButtonCColor : TColorButton;

      LabelUSB : TLabel;
      RadioGroupUSB : TRadioGroup;

      LabelNextTrial : TLabel;
      EditNextTrial : TEdit;

      LabelConsequenceDuration : TLabel;
      EditConsequenceDuration : TEdit;

      LabelTimeOut : TLabel;
      EditTimeOut : TEdit;
  private
    FConfigurationList: TStringList;
    FIsSample: Boolean;
    FOnShow: TNotifyEvent;
    FOpenDialog: TOpenDialog;
    FResponseKey: TKey;
    function GetHeader: string;
    procedure SetConfigurationList(AValue: TStringList);
    procedure FileNameDblClick(Sender : TObject);
    procedure SetHeader(AValue: string); overload;
    procedure SetHeader(ATag: integer); overload;
    procedure SetResponseKey(AValue: TKey);
    procedure ControlEditingDone(Sender : TObject);
    procedure UpdateSFilename;
    procedure UpdateCFilename;
  public
    constructor Create(AOwner: TComponent; ASample : Boolean = False); reintroduce;
    procedure Show; reintroduce;
    procedure UpdateToConfigList;
    procedure UpdateFromConfigList;
    property IsSample : Boolean read FIsSample;
    property Header : string read GetHeader write SetHeader;
    property OpenDialog : TOpenDialog read FOpenDialog write FOpenDialog;
    property ResponseKey : TKey read FResponseKey write SetResponseKey;
    property ConfigList : TStringList read FConfigurationList write SetConfigurationList;
    property OnShow : TNotifyEvent read FOnShow write FOnShow;
  end;

implementation

uses strutils, constants;

{ TGUIKey }

procedure TGUIKeySettings.SetConfigurationList(AValue: TStringList);
begin
  if FConfigurationList=AValue then Exit;
  FConfigurationList:=AValue;
end;

function TGUIKeySettings.GetHeader: string;
begin
  Result := Caption;
end;

procedure TGUIKeySettings.FileNameDblClick(Sender: TObject);
begin
  if Sender is TEdit then
    begin
      if OpenDialog.Execute then
        begin
          TEdit(sender).Text := ExtractFileName(OpenDialog.FileName);
          if Sender = EditSFilename then
            begin
              if Assigned(FResponseKey) then
                begin
                  FResponseKey.FullPath := OpenDialog.FileName;
                  if FResponseKey.Kind.stmAudio then
                    ComboSRepeat.Enabled:=True
                  else
                    ComboSRepeat.Enabled:=False;
                end;
              UpdateSFilename;
              Exit;
            end;

          if Sender = EditCFilename then
            begin
              if Assigned(FResponseKey) then
                begin
                  FResponseKey.FullPath := OpenDialog.FileName;
                  if FResponseKey.Kind.stmAudio then
                    ComboCRepeat.Enabled:=True
                  else
                    ComboCRepeat.Enabled:=False;
                end;
              UpdateCFilename;
            end;
        end;
      Exit;
    end;

  if Sender is TKey then
    begin
      if OpenDialog.Execute then
        begin
          TKey(Sender).FullPath := OpenDialog.FileName;
          EditSFilename.Text := ExtractFileName(OpenDialog.FileName);
          if TKey(Sender).Kind.stmAudio then
            ComboSRepeat.Enabled:=True
          else
            ComboSRepeat.Enabled:=False;
          UpdateSFilename;
        end;
    end;
end;

procedure TGUIKeySettings.SetHeader(AValue: string);
begin
  if Caption=AValue then Exit;
  Caption:=AValue;
end;

procedure TGUIKeySettings.SetHeader(ATag: integer);
begin
  if IsSample then
  {$IFDEF WINDOWS}
    Caption := ''
  {$ELSE}
    Caption := rsSample
  {$ENDIF}
  else
    Caption := rsComparison + IntToStr(Tag);
end;

procedure TGUIKeySettings.SetResponseKey(AValue: TKey);
begin
  if FResponseKey=AValue then Exit;
  FResponseKey:=AValue;
  FResponseKey.OnDblClick:=@FileNameDblClick;
end;

procedure TGUIKeySettings.ControlEditingDone(Sender: TObject);
  function SampleStyleToStr(I:integer) : string;
  begin
    case I of
      0 : Result := 'FALSE';
      1 : Result := 'TRUE';
    end;
  end;

  function ResultToStr(I:integer) : string;
  begin
    case I of
      0:Result := T_NONE;
      1:Result := T_HIT;
      2:Result := T_MISS;
    end;
  end;
begin
  //  GBStimulus : TGroupBox;
  if Assigned(ConfigList) then
    begin
      if (Sender = EditSFilename) or (Sender = ComboSRepeat) or (Sender = ButtonSColor) then
        UpdateSFilename;

      if IsSample then
        begin
          //  GBResponse : TGroupBox;
          if Sender = EditSchedule then
            ConfigList.Values[_Samp+_cSch] := EditSchedule.Text;

          //  GBConseque : TGroupBox;
          if Sender = ComboSampleStyle then
            begin
              case ComboSampleStyle.ItemIndex of
                0 {simultaneous}:
                  EditSampleDelay.Enabled:=False;
                1 {successive}  :
                  EditSampleDelay.Enabled:=True;
              end;
              ConfigList.Values[_Delayed] := SampleStyleToStr(ComboSampleStyle.ItemIndex);
            end;

          if Sender = EditSampleDelay then
            ConfigList.Values[_Delay] := EditSampleDelay.Text;
        end
      else
        begin
          //  GBResponse : TGroupBox;
          if Sender = EditSchedule then
            ConfigList.Values[_Comp+IntToStr(Tag)+_cSch] := EditSchedule.Text;

          //  GBConseque : TGroupBox;
          if Sender = ComboResult then
            ConfigList.Values[_Comp+IntToStr(Tag)+_cRes] := ResultToStr(ComboResult.ItemIndex);

          if (Sender = EditCFilename) or (Sender = ComboCRepeat) or (Sender = ButtonCColor) or (Sender = EditConsequenceDuration) then
            UpdateCFilename;

          if Sender = RadioGroupUSB then
            if RadioGroupUSB.ItemIndex > 0 then
              ConfigList.Values[_Comp+IntToStr(Tag)+_cUsb] := IntToStr(RadioGroupUSB.ItemIndex);

          if Sender = EditNextTrial then
            ConfigList.Values[_Comp+IntToStr(Tag)+_cNxt] := EditNextTrial.Text;

          if Sender = EditTimeOut then
            ConfigList.Values[_Comp+IntToStr(Tag)+_cTO] := EditTimeOut.Text;
        end;
    end;
end;

procedure TGUIKeySettings.UpdateSFilename;
var
  S: String;
begin
  if Assigned(ConfigList) then
    begin
      S := EditSFilename.Text+#32+IntToStr(ComboSRepeat.ItemIndex)+#32+IntToStr(ButtonSColor.ButtonColor);
      if IsSample then
        ConfigList.Values[_Samp+_cStm] := S
      else
        ConfigList.Values[_Comp+IntToStr(Tag)+_cStm] := S;
    end;
end;

procedure TGUIKeySettings.UpdateCFilename;
var
  S: String;
begin
  if Assigned(ConfigList) then
    begin
      S := EditCFilename.Text+#32+IntToStr(ComboCRepeat.ItemIndex)+#32+IntToStr(ButtonCColor.ButtonColor)+#32+EditConsequenceDuration.Text;
      if IsSample then
        // do nothing
      else
        ConfigList.Values[_Comp+IntToStr(Tag)+_cIET] := S;
    end;
end;

constructor TGUIKeySettings.Create(AOwner: TComponent; ASample: Boolean);
var
  LBox : TGroupBox;
  LBoxes : array [0..2] of TGroupBox;
begin
  inherited Create(AOwner);
  FIsSample:=ASample;
  SetHeader(0);
  AutoSize := True;
  ShowHint:=True;
  {$IFDEF WINDOWS}
    ParentColor:=False;
    Color := clBtnFace;
  {$ENDIF}
  with ChildSizing do
    begin
      ControlsPerLine := 1;
      EnlargeHorizontal := crsAnchorAligning;
      EnlargeVertical := crsAnchorAligning;
      Layout := cclLeftToRightThenTopToBottom;
      LeftRightSpacing := 0;
      TopBottomSpacing := 0;
      VerticalSpacing := 20;
    end;
  {
  ..............................    STIMULUS    ..............................
  }
  GBStimulus := TGroupBox.Create(Self);
  GBStimulus.Caption:=rsStimulus;

  LabelSFilename := TLabel.Create(Self);
  with LabelSFilename do
    begin
      Caption := rsFilename;
      Hint:='Qual o arquivo de estímulo?';
      Parent := GBStimulus;
    end;

  EditSFilename := TEdit.Create(Self);
  with EditSFilename do
    begin
      Hint:='Clique duas vezes para abrir o arquivo.';
      Parent := GBStimulus;
      OnDblClick:=@FileNameDblClick;
      OnEditingDone:=@ControlEditingDone;
    end;

  //////////////////////////////////////////////////////////////////////////////

  LabelSRepeat := TLabel.Create(Self);
  with LabelSRepeat do
    begin
      Caption := rsMediaSound;
      Hint:='Como o arquivo (som) deve ser apresentado?';
      Parent := GBStimulus;
    end;

  ComboSRepeat := TComboBox.Create(Self);
  with ComboSRepeat do
    begin
      Items.Append('Tocar uma vez'); // 0
      Items.Append('Tocar em loop'); // 1
      ItemIndex:=0;
      Enabled:=False;
      ReadOnly:=True;
      OnSelect:=@ControlEditingDone;
      Parent := GBStimulus;
    end;

  //////////////////////////////////////////////////////////////////////////////

  LabelSColor := TLabel.Create(Self);
  with LabelSColor do
    begin
      Caption := rsColor;
      Hint:='Cor da janela do estímulo caso o arquivo de estímulo não seja encontrado.';
      Parent := GBStimulus;
    end;

  ButtonSColor := TColorButton.Create(Self);
  with ButtonSColor do
    begin
      ButtonColor:=clRed;
      Parent := GBStimulus;
      OnColorChanged:=@ControlEditingDone;
    end;

  {
  ..............................    RESPONSE    ..............................
  }

  GBResponse := TGroupBox.Create(Self);
  GBResponse.Caption := rsExpectedResponse;

  LabelSchedule := TLabel.Create(Self);
  with LabelSchedule do
    begin
      Caption := rsSchedule;
      Hint:='Qual a exigência da resposta ao estimulo?';
      Parent := GBResponse;
    end;

  EditSchedule := TEdit.Create(Self);
  with EditSchedule do
    begin
      ShowHint:=True;
      Hint:='CRF, FI, FR, VI, VR, DRL, DRH';
      Parent := GBResponse;
      OnEditingDone:=@ControlEditingDone;
    end;

  {
  ..............................    CONSEQUENCE    ..............................
  }

  GBConseque := TGroupBox.Create(Self);
  GBConseque.Caption:=rsConsequence;

  if ASample then
    begin
      LabelSampleStyle := TLabel.Create(Self);
      with LabelSampleStyle do
        begin
          Caption := rsStyle;
          Hint:='Qual o formato de apresentação do modelo?';
          Parent := GBConseque;
        end;

      ComboSampleStyle := TComboBox.Create(Self);
      with ComboSampleStyle do
        begin
          Items.Append('Simultâneo'); // 0
          Items.Append('Sucessivo'); // 1
          ItemIndex:=0;
          Enabled:=True;
          ReadOnly:=True;
          OnSelect:=@ControlEditingDone;
          Parent := GBConseque;
        end;

      //////////////////////////////////////////////////////////////////////////////

      LabelSampleDelay := TLabel.Create(Self);
      with LabelSampleDelay do
        begin
          Caption := rsDelay;
          Hint:='Tempo entre a remoção do modelo e a apresentação das comparações, em milisegundos';
          Parent := GBConseque;
        end;

      EditSampleDelay := TEdit.Create(Self);
      with EditSampleDelay do
        begin
          ShowHint:=True;
          Enabled:=False;
          Hint:='Tempo entre a remoção do modelo e a apresentação das comparações, em milisegundos';
          Parent := GBConseque;
          OnEditingDone:=@ControlEditingDone;
        end;

    end
  else
    begin
      LabelResult := TLabel.Create(Self);
      with LabelResult do
        begin
          Caption := rsCount;
          Hint := 'O que a resposta ao estímulo contabilizará?';
          Parent := GBConseque;
        end;

      ComboResult := TComboBox.Create(Self);
      with ComboResult do
        begin
          Items.Append('Nada'); // 0
          Items.Append('Acerto'); // 1
          Items.Append('Erro'); // 2
          ItemIndex:=0;
          Enabled:=True;
          ReadOnly:=True;
          Parent := GBConseque;
          OnSelect:=@ControlEditingDone;
        end;

      //////////////////////////////////////////////////////////////////////////////

      LabelCFilename := TLabel.Create(Self);
      with LabelCFilename do
        begin
          Caption := rsFilename;
          Hint := 'Apresentar uma consequência após a resposta ao estímulo?';
          Parent := GBConseque;
        end;

      EditCFilename := TEdit.Create(Self);
      with EditCFilename do
        begin
          Hint:='Deixe vazio para usar o padrão para acerto e erro. Clique duas vezes para customizar';
          OnDblClick:=@FileNameDblClick;
          OnEditingDone:=@ControlEditingDone;
          Parent := GBConseque;
        end;

      //////////////////////////////////////////////////////////////////////////////

      LabelCRepeat := TLabel.Create(Self);
      with LabelCRepeat do
        begin
          Caption := rsMediaSound;
          Hint := 'Como o arquivo (som) deve ser apresentado?';
          Parent := GBConseque;
        end;

      ComboCRepeat := TComboBox.Create(Self);
      with ComboCRepeat do
        begin
          Items.Append('Tocar uma vez'); // 0
          Items.Append('Tocar em loop'); // 1
          ItemIndex:=0;
          Enabled:=False;
          ReadOnly:=True;
          Parent := GBConseque;
          OnSelect:=@ControlEditingDone;
        end;

      //////////////////////////////////////////////////////////////////////////////

      LabelCColor := TLabel.Create(Self);
      with LabelCColor do
        begin
          Caption := rsColor;
          Hint := 'Cor da janela da consequência caso o arquivo não seja encontrado';
          Parent := GBConseque;
        end;

      ButtonCColor := TColorButton.Create(Self);
      with ButtonCColor do
        begin
          ButtonColor:=clRed;
          Parent := GBConseque;
          OnColorChanged:=@ControlEditingDone;
        end;

      //////////////////////////////////////////////////////////////////////////////

      LabelConsequenceDuration := TLabel.Create(Self);
      with LabelConsequenceDuration do
        begin
          Caption := rsDuration;
          Hint:='Duração da consequência (antes do IET) em milisegundos';
          Parent := GBConseque;
        end;

      EditConsequenceDuration := TEdit.Create(Self);
      with EditConsequenceDuration do
        begin
          ShowHint:=True;
          Hint:='Duração da consequência (antes do IET) em milisegundos';
          Parent := GBConseque;
          OnEditingDone:=@ControlEditingDone;
        end;

      //////////////////////////////////////////////////////////////////////////////
      LabelUSB := TLabel.Create(Self);
      with LabelUSB do
        begin
          Caption := rsUSBPort;
          Hint := 'Qual porta USB ativar?';
          Parent := GBConseque;
        end;

      RadioGroupUSB := TRadioGroup.Create(Self);
      with RadioGroupUSB do
        begin
          Caption := '';
          Columns:=4;
          Items.Append('0');
          Items.Append('1');
          Items.Append('2');
          Items.Append('3');
          Items.Append('4');
          Hint := 'Qual porta USB ativar?';
          Parent := GBConseque;
          OnClick:=@ControlEditingDone;
        end;

      //////////////////////////////////////////////////////////////////////////////

      LabelNextTrial := TLabel.Create(Self);
      with LabelNextTrial do
        begin
          Caption := rsNextTrial;
          Hint := 'Qual a tentativa seguinte?';
          Parent := GBConseque;
        end;

      EditNextTrial := TEdit.Create(Self);
      with EditNextTrial do
        begin
          ShowHint:=True;
          Hint:='vazio, 0 ou NXT=seguinte; CRT=correção; 1..N: tentativa especificada';
          Parent := GBConseque;
          OnEditingDone:=@ControlEditingDone;
        end;

      //////////////////////////////////////////////////////////////////////////////

      LabelTimeOut := TLabel.Create(Self);
      with LabelTimeOut do
        begin
          Caption := rsTimeOut;
          Hint := 'Apresentar Time-Out em tela preta';
          Parent := GBConseque;
        end;

      EditTimeOut := TEdit.Create(Self);
      with EditTimeOut do
        begin
          ShowHint:=True;
          Hint:='Tempo em milisegundos';
          Parent := GBConseque;
          OnEditingDone:=@ControlEditingDone;
        end;

    end;

  {
  ..............................    CHILD SIZING    ..............................
  }

  LBoxes[0] := GBStimulus;
  LBoxes[1] := GBResponse;
  LBoxes[2] := GBConseque;

  for LBox in LBoxes do
    begin
      LBox.AutoSize := True;
      with LBox.ChildSizing do
        begin
          ControlsPerLine := 2;
          EnlargeHorizontal := crsHomogenousChildResize;
          HorizontalSpacing := 0;
          Layout := cclLeftToRightThenTopToBottom;
          LeftRightSpacing := 20;
          TopBottomSpacing := 0;
          VerticalSpacing := 0;
        end;
      LBox.Parent := Self;
    end;
  AutoSize:=False;
  AutoSize:=True;
end;

procedure TGUIKeySettings.Show;
begin
  inherited Show;
  SetHeader(Tag);
  UpdateFromConfigList;
  if Assigned(OnShow) then OnShow(Self);
end;

procedure TGUIKeySettings.UpdateFromConfigList;
var
  S : string;

  function StrToSampleStyle(S:string) : integer;
  begin
    case UpperCase(S) of
      'FALSE':Result := 0;
      'TRUE':Result := 1;
      else
        Result := 0;
    end;
  end;

  function StrToResult(S:string) : integer;
  begin
    case UpperCase(S) of
      T_NONE:Result := 0;
      T_HIT:Result := 1;
      T_MISS:Result := 2;
      '':
        if Tag = 1 then
          Result := 1
        else
          Result := 2;
      else
        Result := 0;
    end;
  end;

begin
  if IsSample then
    begin
      // GBStimulus : TGroupBox;
      S := ConfigList.Values[_Samp+_cStm];
      EditSFilename.Text := ExtractDelimited(1,S,[#32]);
      ComboSRepeat.ItemIndex := StrToIntDef(ExtractDelimited(2,S,[#32]),0);
      ButtonSColor.ButtonColor := StrToIntDef(ExtractDelimited(3,S,[#32]),clRed);

      //GBResponse : TGroupBox;
      EditSchedule.Text:=ConfigList.Values[_Samp+_cSch];

      //GBConseque : TGroupBox;
      ComboSampleStyle.ItemIndex := StrToSampleStyle(ConfigList.Values[_Delayed]);
      EditSampleDelay.Text := ConfigList.Values[_Delay];
    end
  else
    begin
      // GBStimulus : TGroupBox;
      S := ConfigList.Values[_Comp+IntToStr(Tag)+_cStm];
      EditSFilename.Text := ExtractDelimited(1,S,[#32]);
      ComboSRepeat.ItemIndex := StrToIntDef(ExtractDelimited(2,S,[#32]),0);
      ButtonSColor.ButtonColor := StrToIntDef(ExtractDelimited(3,S,[#32]),clRed);

      //GBResponse : TGroupBox;
      EditSchedule.Text:=ConfigList.Values[_Comp+IntToStr(Tag)+_cSch];

      //GBConseque : TGroupBox;
      ComboResult.ItemIndex := StrToResult(ConfigList.Values[_Comp+IntToStr(Tag)+_cRes]);

      S := ConfigList.Values[_Comp+IntToStr(Tag)+_cIET];
      EditCFilename.Text := ExtractDelimited(1,S,[#32]);
      if UpperCase(RightStr(EditCFilename.Text,3)) = 'WAV' then
        ComboCRepeat.Enabled:=True
      else
        ComboCRepeat.Enabled:=False;

      ComboCRepeat.ItemIndex := StrToIntDef(ExtractDelimited(2,S,[#32]),0);
      ButtonCColor.ButtonColor := StrToIntDef(ExtractDelimited(3,S,[#32]),clRed);
      EditConsequenceDuration.Text := ExtractDelimited(4,S,[#32]);

      RadioGroupUSB.ItemIndex := StrToIntDef(ConfigList.Values[_Comp+IntToStr(Tag)+_cUsb],-1);
      EditNextTrial.Text := ConfigList.Values[_Comp+IntToStr(Tag)+_cNxt];
      EditTimeOut.Text := ConfigList.Values[_Comp+IntToStr(Tag)+_cTO];
    end;
end;

procedure TGUIKeySettings.UpdateToConfigList;
var
  i: Integer;
begin
  for i := 0 to ComponentCount-1 do
    if Components[i] is TLabel then
      ControlEditingDone(Components[i]);
end;

end.


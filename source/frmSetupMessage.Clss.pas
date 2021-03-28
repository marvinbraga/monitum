unit frmSetupMessage.Clss;
{
   Monitum Project, 2021.
   Marcus Vinicius Braga, mvbraga@gmail.com.
   https://github.com/marvinbraga/monitum
   GNU General Public License v3.0
   See all terms in:
   https://github.com/marvinbraga/monitum/blob/main/LICENSE
}

interface

uses
  uShowMessage,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.StdCtrls,
  FMX.ListBox,
  FMX.Controls.Presentation,
  FMX.Edit;

type
  IMessageData = interface
    ['{A4554DED-DBB1-43CA-B202-5E9804668B11}']
    function Title: string;
    function Text: string;
    function MessageSize: TMessageSize;
    function SetTitle(const AValue: string): IMessageData;
    function SetText(const AValue: string): IMessageData;
    function SetMessageSize(const AValue: TMessageSize): IMessageData;
  end;

  TMessageData = class(TInterfacedObject, IMessageData)
  private
    FTitle: string;
    FText: string;
    FMessageSize: TMessageSize;
  protected
    function Title: string;
    function Text: string;
    function MessageSize: TMessageSize;
    function SetTitle(const AValue: string): IMessageData;
    function SetText(const AValue: string): IMessageData;
    function SetMessageSize(const AValue: TMessageSize): IMessageData;
  public
    class function New(const ATitle: string; const AText: string; const AMessageSize: TMessageSize = Small): IMessageData; static;
    constructor Create(const ATitle: string; const AText: string; const AMessageSize: TMessageSize = Small);
    destructor Destroy; override;
  end;

  ISetupMessage = interface
    ['{B4E9D730-3DAB-4180-AF7B-242219A1EA2B}']
    function MessageData: IMessageData;
  end;

  TFormSetupMessage = class(TForm, ISetupMessage)
    LayoutMain: TLayout;
    EditTitle: TEdit;
    EditMessage: TEdit;
    ComboBoxMessageSize: TComboBox;
    LayoutButtons: TLayout;
    LayoutControls: TLayout;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    LabelTitle: TLabel;
    LabelMessage: TLabel;
    LabelMessageSize: TLabel;
    procedure ButtonOkClick(Sender: TObject);
  private
    FMessageData: IMessageData;
    procedure DataToControls;
    procedure ControlsToData;
  protected
    function MessageData: IMessageData;
  public
    class function New(const AOwner: TComponent; const APosition: TPoint; const AMessageData: IMessageData): ISetupMessage; static;
    constructor Create(const AOwner: TComponent; const APosition: TPoint; const AMessageData: IMessageData); reintroduce;
    destructor Destroy; override;
  end;

var
  FormSetupMessage: TFormSetupMessage;

implementation

{$R *.fmx}

{ TMessageData }

class function TMessageData.New(const ATitle: string; const AText: string; const AMessageSize: TMessageSize = Small): IMessageData;
begin
  Result := TMessageData.Create(ATitle, AText, AMessageSize);
end;

constructor TMessageData.Create(const ATitle: string; const AText: string; const AMessageSize: TMessageSize = Small);
begin
  inherited Create;
  FTitle := ATitle;
  FText := AText;
  FMessageSize := AMessageSize;
end;

destructor TMessageData.Destroy;
begin
  inherited;
end;

function TMessageData.MessageSize: TMessageSize;
begin
  Result := FMessageSize;
end;

function TMessageData.Text: string;
begin
  Result := FText;
end;

function TMessageData.Title: string;
begin
  Result := FTitle;
end;

function TMessageData.SetMessageSize(const AValue: TMessageSize): IMessageData;
begin
  Result := Self;
  FMessageSize := AValue;
end;

function TMessageData.SetText(const AValue: string): IMessageData;
begin
  Result := Self;
  FText := AValue;
end;

function TMessageData.SetTitle(const AValue: string): IMessageData;
begin
  Result := Self;
  FTitle := AValue;
end;

{ TFormSetupMessage }

class function TFormSetupMessage.New(const AOwner: TComponent; const APosition: TPoint; const AMessageData: IMessageData): ISetupMessage;
begin
  Result := TFormSetupMessage.Create(AOwner, APosition, AMessageData);
end;

procedure TFormSetupMessage.ButtonOkClick(Sender: TObject);
begin
  Self.ControlsToData;
end;

procedure TFormSetupMessage.ControlsToData;
begin
  FMessageData
    .SetTitle(EditTitle.Text)
    .SetText(EditMessage.Text)
    .SetMessageSize(TMessageSize(ComboBoxMessageSize.ItemIndex));
end;

constructor TFormSetupMessage.Create(const AOwner: TComponent; const APosition: TPoint; const AMessageData: IMessageData);
begin
  inherited Create(AOwner);
  Self.Left := APosition.X;
  Self.Top := APosition.Y;
  FMessageData := AMessageData;
  Self.DataToControls;
end;

destructor TFormSetupMessage.Destroy;
begin
  FMessageData := nil;
  inherited;
end;

function TFormSetupMessage.MessageData: IMessageData;
begin
  Result := FMessageData;
end;

procedure TFormSetupMessage.DataToControls;
begin
  EditTitle.Text := FMessageData.Title;
  EditMessage.Text := FMessageData.Text;
  ComboBoxMessageSize.ItemIndex := Integer(FMessageData.MessageSize);
end;

end.


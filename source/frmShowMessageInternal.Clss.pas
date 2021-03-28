unit frmShowMessageInternal.Clss;
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
  FMX.Objects,
  FMX.Layouts,
  FMX.Effects, FMX.Ani;

type
  TMessageBoxInternal = class
    class function New(const AOwner: TComponent; const ATitle, AText: string; const ASize: TMessageSize = Small): IFormMessage; static;
  end;

  TFormMessageInternal = class(TForm, IFormMessage)
    LayoutMain: TLayout;
    RectangleMain: TRectangle;
    LayoutTitle: TLayout;
    LayoutMessage: TLayout;
    TextTitle: TText;
    TextMessage: TText;
    ShadowTitle: TShadowEffect;
    ShadowPanel: TShadowEffect;
    FloatAnimationRectMain: TFloatAnimation;
    procedure RectangleMainClick(Sender: TObject);
    procedure RectangleMainDblClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FTitle, FText: string;
    FMessageSize: TMessageSize;
    procedure UpdateInfo;
    procedure SetFMessageSize(const Value: TMessageSize);
  protected
    procedure ShowMessage;
    procedure CloseMessage;
    property MessageSize: TMessageSize  read FMessageSize write SetFMessageSize;
  public
    class function New(const AOwner: TComponent; const ATitle, AText: string; const ASize: TMessageSize = Small): IFormMessage; static;
    constructor Create(const AOwner: TComponent; const ATitle, AText: string; const ASize: TMessageSize = Small); reintroduce;
    destructor Destroy; override;
  end;

var
  FormMessageInternal: TFormMessageInternal;

implementation

{$R *.fmx}

{ TMessageDialog }

class function TMessageBoxInternal.New(const AOwner: TComponent; const ATitle, AText: string; const ASize: TMessageSize = Small): IFormMessage;
begin
  Result := TFormMessageInternal.New(AOwner, ATitle, AText, ASize);
end;

{ TFormMessage }

class function TFormMessageInternal.New(const AOwner: TComponent; const ATitle, AText: string; const ASize: TMessageSize = Small): IFormMessage;
begin
  Result := TFormMessageInternal.Create(AOwner, ATitle, AText, ASize);
end;

constructor TFormMessageInternal.Create(const AOwner: TComponent; const ATitle, AText: string; const ASize: TMessageSize = Small);
begin
  inherited Create(AOwner);
  FTitle := ATitle;
  FText := AText;
  Self.MessageSize := ASize;
  Self.UpdateInfo;
end;

destructor TFormMessageInternal.Destroy;
begin
  inherited;
end;

procedure TFormMessageInternal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TFormMessageInternal.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key in [vkReturn, vkEscape, vkSpace] then
    Self.CloseMessage;
end;

procedure TFormMessageInternal.CloseMessage;
begin
  Self.Close;
end;

procedure TFormMessageInternal.RectangleMainClick(Sender: TObject);
begin
  Self.CloseMessage;
end;

procedure TFormMessageInternal.RectangleMainDblClick(Sender: TObject);
begin
  Self.CloseMessage;
end;

procedure TFormMessageInternal.SetFMessageSize(const Value: TMessageSize);
begin
  FMessageSize := Value;
  case FMessageSize of
    Small: Self.Height := 150;
    Medium: Self.Height := 190;
    Large: Self.Height := 250;
  end;
end;

procedure TFormMessageInternal.ShowMessage;
begin
  Self.Show;
end;

procedure TFormMessageInternal.UpdateInfo;
begin
  TextTitle.Text := FTitle;
  TextMessage.Text := FText;
end;

end.


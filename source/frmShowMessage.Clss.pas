unit frmShowMessage.Clss;
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
  FMX.Dialogs, FMX.Ani, FMX.Objects, FMX.Layouts;

type
  TMessageBox = class
    class procedure Show(const ATitle: string; const AText: string; const ASize: TMessageSize = Small); static;
  end;

  TFormBlock = class(TForm)
    TimerClose: TTimer;
    LayoutMain: TLayout;
    RectangleMain: TRectangle;
    FloatAnimationRectMain: TFloatAnimation;
    procedure TimerCloseTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  private
    FFormMessage: IFormMessage;
    procedure CloseMessage;
  public
    function FormMessage: IFormMessage;
    procedure SetFormMessage(const AValue: IFormMessage);
  end;

var
  FormBlock: TFormBlock;

implementation

{$R *.fmx}

uses
  frmShowMessageInternal.Clss;

procedure TFormBlock.CloseMessage;
begin
  TimerClose.Enabled := False;
  FFormMessage.CloseMessage;
  Self.Close;
end;

procedure TFormBlock.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FFormMessage := nil;
  Action := TCloseAction.caFree;
end;

procedure TFormBlock.FormDestroy(Sender: TObject);
begin
  if TimerClose.Enabled then
    TimerClose.Enabled := False;
end;

procedure TFormBlock.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key in [vkReturn, vkEscape, vkSpace] then
    Self.CloseMessage;
end;

function TFormBlock.FormMessage: IFormMessage;
begin
  Result := FFormMessage;
end;

procedure TFormBlock.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  Self.CloseMessage;
end;

procedure TFormBlock.SetFormMessage(const AValue: IFormMessage);
begin
  FFormMessage := AValue;
end;

procedure TFormBlock.TimerCloseTimer(Sender: TObject);
begin
  Self.CloseMessage;
end;

{ TMessageBox }

class procedure TMessageBox.Show(const ATitle: string; const AText: string; const ASize: TMessageSize = Small);
begin
  FormBlock := TFormBlock.Create(Application);
  FormBlock.SetFormMessage(TMessageBoxInternal.New(FormBlock, ATitle, AText, ASize));
  FormBlock.Show;
  FormBlock.FormMessage.ShowMessage;
end;

end.


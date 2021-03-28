unit frmStart.Clss;
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
  frmSetupMessage.Clss,
  frmScreenShot.Clss,
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
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Objects,
  FMX.Colors;

type
  TFormStart = class(TForm)
    ButtonStart: TButton;
    ButtonTravar: TButton;
    RectTools: TRectangle;
    LayoutMain: TLayout;
    ButtonColor1: TComboColorBox;
    PanelComponents: TPanel;
    ButtonColor2: TComboColorBox;
    ButtonColor3: TComboColorBox;
    ButtonColor4: TComboColorBox;
    ButtonColor5: TComboColorBox;
    ButtonColor: TRectangle;
    ButtonCreateRect: TButton;
    ButtonShowMessage: TButton;
    ButtonShowZoom: TButton;
    ButtonMessageBox: TButton;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonTravarClick(Sender: TObject);
    procedure ButtonCreateRectClick(Sender: TObject);
    procedure ButtonShowMessageClick(Sender: TObject);
    procedure ButtonShowZoomClick(Sender: TObject);
    procedure ButtonColorChange(Sender: TObject);
    procedure ButtonMessageBoxClick(Sender: TObject);
  private
    FActiveLoupe: Boolean;
    FFormScreenshot: TFormScreenshot;
    FMessageData: IMessageData;
    procedure StartView;
  public
  end;

var
  FormStart: TFormStart;

implementation

uses
  uComponentsTypes.Clss,
  frmShowMessage.Clss,
  uShowMessage;

{$R *.fmx}

procedure TFormStart.ButtonColorChange(Sender: TObject);
begin
  ButtonColor.Fill.Color := TComboColorBox(Sender).Color;
end;

procedure TFormStart.ButtonCreateRectClick(Sender: TObject);
begin
  if not Assigned(FFormScreenshot) then
    Exit;

  FFormScreenshot.CreateRectangle(TShape.New(TFill.New(ButtonColor.Fill.Color), TStroke.New(TAlphaColors.Black, TStrokeDash.Solid, TBrushKind.None, 1)));
end;

procedure TFormStart.ButtonStartClick(Sender: TObject);
begin
  Self.StartView;
end;

procedure TFormStart.ButtonMessageBoxClick(Sender: TObject);
var
  LForm: ISetupMessage;
  LPosition: TPoint;

  function GetTop: Integer;
  begin
    Result := Self.Top + Round(PanelComponents.Position.Y) + Round(ButtonMessageBox.Position.Y) + 100;
  end;

  function GetLeft: Integer;
  begin
    Result := Self.Left + Round(Self.Width) + 25;
  end;

begin
  if not Assigned(FMessageData) then
    FMessageData := TMessageData.New(EmptyStr, EmptyStr);

  LPosition.Y := GetTop;
  LPosition.X := GetLeft;
  LForm := TFormSetupMessage.New(Self, LPosition, FMessageData);
  if TFormSetupMessage(LForm).ShowModal = mrOk then
  begin
    TMessageBox.Show(FMessageData.Title, FMessageData.Text, FMessageData.MessageSize);
  end;
end;

procedure TFormStart.StartView;
begin
  if not Assigned(FFormScreenshot) then
  begin
    Self.Visible := False;
    try
      ButtonColor.Fill.Color := ButtonColor1.Color;
      FFormScreenshot := TFormScreenshot.Create(nil);
      FFormScreenshot.Show;
      ButtonStart.Text := 'Terminar';
    finally
      Self.Visible := True;
    end;
  end
  else
  begin
    FreeAndNil(FFormScreenshot);
    ButtonStart.Text := 'Iniciar';
  end;
end;

procedure TFormStart.ButtonTravarClick(Sender: TObject);
begin
  if not Assigned(FFormScreenshot) then
    Exit;

  FFormScreenshot.ActiveMoveControls := not FFormScreenshot.ActiveMoveControls;
  if FFormScreenshot.ActiveMoveControls then
    ButtonTravar.Text := 'Travar'
  else
    ButtonTravar.Text := 'Destravar';
end;

procedure TFormStart.ButtonShowMessageClick(Sender: TObject);
begin
  if not Assigned(FFormScreenshot) then
    Exit;

  FFormScreenshot.ToastMessageShow('Aqui está a mensagem.');
end;

procedure TFormStart.ButtonShowZoomClick(Sender: TObject);
begin
  if not Assigned(FFormScreenshot) then
    Exit;

  FActiveLoupe := not FActiveLoupe;
  if FActiveLoupe then
  begin
    FreeAndNil(FFormScreenshot);
    Self.StartView;
  end;
  FFormScreenshot.ActivateLoupe(FActiveLoupe);
end;

end.


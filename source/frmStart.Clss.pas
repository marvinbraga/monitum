unit frmStart.Clss;

// Monitum Projet, 2021.
// Marcus Vinicius Braga, all rights reserved.

interface

uses
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
  frmScreenShot.Clss,
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
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonTravarClick(Sender: TObject);
    procedure ButtonCreateRectClick(Sender: TObject);
    procedure ButtonShowMessageClick(Sender: TObject);
    procedure ButtonShowZoomClick(Sender: TObject);
    procedure ButtonColorChange(Sender: TObject);
  private
    FActiveLoupe: Boolean;
    FFormScreenshot: TFormScreenshot;
  public
  end;

var
  FormStart: TFormStart;

implementation

uses
  uComponentsTypes.Clss;

{$R *.fmx}

procedure TFormStart.ButtonColorChange(Sender: TObject);
begin
  ButtonColor.Fill.Color := TComboColorBox(Sender).Color;
end;

procedure TFormStart.ButtonCreateRectClick(Sender: TObject);
begin
  if not Assigned(FFormScreenshot) then
    Exit;

  FFormScreenshot.CreateRectangle(
    TShape.New(
      TFill.New(ButtonColor.Fill.Color),
      TStroke.New(TAlphaColors.Black, TStrokeDash.Solid, TBrushKind.None, 1)));
end;

procedure TFormStart.ButtonStartClick(Sender: TObject);
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
  FFormScreenshot.ActivateLoupe(FActiveLoupe);
end;

end.


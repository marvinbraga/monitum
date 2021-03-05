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
    ButtonColor: TComboColorBox;
    PanelComponents: TPanel;
    ButtonCreateRect: TRectangle;
    Rectangle1: TRectangle;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonTravarClick(Sender: TObject);
    procedure ButtonCreateRectClick(Sender: TObject);
    procedure Rectangle1Click(Sender: TObject);
  private
    FFormScreenshot: TFormScreenshot;
  public
  end;

var
  FormStart: TFormStart;

implementation

uses
  uComponentsTypes.Clss;

{$R *.fmx}

procedure TFormStart.ButtonCreateRectClick(Sender: TObject);
begin
  if not Assigned(FFormScreenshot) then
    Exit;

  FFormScreenshot.CreateRectangle(
    TShape.New(
      TFill.New(ButtonColor.Color),
      TStroke.New(TAlphaColors.Black, TStrokeDash.Solid, TBrushKind.None, 1)));
end;

procedure TFormStart.ButtonStartClick(Sender: TObject);
begin
  if not Assigned(FFormScreenshot) then
  begin
    Self.Visible := False;
    try
      FFormScreenshot := TFormScreenshot.Create(nil);
      FFormScreenshot.Show;
      FFormScreenshot.ActivateLoupe(True);
      ButtonStart.Text := 'Terminar';
    finally
      Self.Visible := True;
    end;
  end
  else
  begin
    FFormScreenshot.ActivateLoupe(False);
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

procedure TFormStart.Rectangle1Click(Sender: TObject);
begin
  if not Assigned(FFormScreenshot) then
    Exit;

  FFormScreenshot.ToastMessageShow('Aqui est� a mensagem.');
end;

end.


unit frmScreenShot.Clss;

// Monitum Projet, 2021.
// Marcus Vinicius Braga, all rights reserved.

interface

uses
  uComponentsManager,
  Marvin.UI.ToastMessage,
  FMX.Objects,
  FMX.Layouts,
  System.Classes,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  uComponentsTypes, uMagnifierGlass;

type
  TFormScreenshot = class(TForm)
    LayoutMain: TLayout;
    SlaveRect: TRectangle;
    ImageScreenshot: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
  private
    FComponentsManager: IComponentsManager;
    FMagnifierGlass: IMagnifierGlassManager;
    FActiveMoveControls: Boolean;
    FToast: IToast;
    procedure SetActiveMoveControls(const Value: Boolean);
  protected
  public
    procedure ToastMessageShow(const AMessage: string; const ATextAction: string = '');
    procedure CreateRectangle(const AShape: IShape);
    procedure ActivateLoupe(const AValue: Boolean);
  public
    property ActiveMoveControls: Boolean read FActiveMoveControls write SetActiveMoveControls;
  end;

var
  FormScreenshot: TFormScreenshot;

implementation

uses
  System.UITypes,
  Marvin.UI.ToastMessage.Clss,
  uScreenShot.Clss,
  uComponentsManager.Clss,
  uComponentsFactory.Clss, uMagnifierGlass.Clss;

{$R *.fmx}

procedure TFormScreenshot.ActivateLoupe(const AValue: Boolean);
begin
  FMagnifierGlass.SetVisible(AValue);
end;

procedure TFormScreenshot.CreateRectangle(const AShape: IShape);
begin
  FComponentsManager.Deactivate;
  try
    TComponentsFactory.New(LayoutMain).CreateRectangle(AShape);
  finally
    FComponentsManager.Activate;
  end;
end;

procedure TFormScreenshot.FormCreate(Sender: TObject);
begin
  TScreenShot.New(ImageScreenshot.Bitmap).Execute;
  FComponentsManager := TComponentsManager.New(Self, LayoutMain, SlaveRect);
  FActiveMoveControls := FComponentsManager.IsActivated;
  Self.Top := 0;
  Self.Left := 0;
  FMagnifierGlass := TMagnifierGlassManager.New(LayoutMain, ImageScreenshot).SetVisible(True);
end;

procedure TFormScreenshot.FormDestroy(Sender: TObject);
begin
  FMagnifierGlass := nil;
  FComponentsManager := nil;
  FToast := nil;
end;

procedure TFormScreenshot.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkDelete then
  begin
    FComponentsManager.DeleteObject;
  end;
end;

procedure TFormScreenshot.SetActiveMoveControls(const Value: Boolean);
begin
  FActiveMoveControls := Value;
  if FActiveMoveControls <> FComponentsManager.IsActivated then
  begin
    FToast := nil;
    if FActiveMoveControls then
      FComponentsManager.Activate
    else
      FComponentsManager.Deactivate;
  end;
end;

procedure TFormScreenshot.ToastMessageShow(const AMessage, ATextAction: string);
begin
  if Assigned(FToast) then
    FToast := nil;
  FToast := TToast.New(LayoutMain, AMessage, ATextAction).Show;
end;

end.


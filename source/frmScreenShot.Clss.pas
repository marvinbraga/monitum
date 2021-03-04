unit frmScreenShot.Clss;

interface

uses
  uComponentsManager,
  FMX.Objects, FMX.Layouts, System.Classes, FMX.Types, FMX.Controls,
  FMX.Forms;

type
  TFormScreenshot = class(TForm)
    LayoutMain: TLayout;
    SlaveRect: TRectangle;
    ImageScreenshot: TImage;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
  private
    FComponentsManager: IComponentsManager;
    FActiveMoveControls: Boolean;
    procedure SetActiveMoveControls(const Value: Boolean);
  protected
  public
    property ActiveMoveControls: Boolean read FActiveMoveControls write SetActiveMoveControls;
  end;

var
  FormScreenshot: TFormScreenshot;

implementation

uses
  System.UITypes,
  uScreenShot.Clss,
  uComponentsManager.Clss;

{$R *.fmx}

procedure TFormScreenshot.FormCreate(Sender: TObject);
begin
  TScreenShot.New(ImageScreenshot.Bitmap).Execute;
  FComponentsManager := TComponentsManager.New(Self, LayoutMain, SlaveRect);
  FActiveMoveControls := FComponentsManager.IsActivated;
  Self.Top := 0;
  Self.Left := 0;
end;

procedure TFormScreenshot.FormDestroy(Sender: TObject);
begin
  FComponentsManager := nil;
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
    if FActiveMoveControls then
      FComponentsManager.Activate
    else
      FComponentsManager.Deactivate;
  end;
end;

end.


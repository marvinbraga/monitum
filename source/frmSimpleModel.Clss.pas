unit frmSimpleModel.Clss;

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
  FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TFormSimpleModel = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  private
    FOffset: TPoint;
    FIsChangePosition: Boolean;
  public
  end;

var
  FormSimpleModel: TFormSimpleModel;

implementation

{$R *.fmx}

procedure TFormSimpleModel.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TFormSimpleModel.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FIsChangePosition := True;
  FOffset.X := Round(X) + Self.Left;
  FOffset.Y := Round(Y) + Self.Top;
end;

procedure TFormSimpleModel.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if FIsChangePosition then
  begin
    Label1.Text := 'Top: ' + Self.Top.ToString + ', OffY: ' + FOffset.Y.ToString + ', Y: ' + Y.ToString;
    Label2.Text := 'Left: ' + Self.Left.ToString + ', OffX: ' + FOffset.X.ToString + ', X: ' + X.ToString;
    Self.Top := FOffset.Y + Round(Y);
    Self.Left := FOffset.X + Round(X);
  end;
end;

procedure TFormSimpleModel.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FIsChangePosition := False;
end;

end.


unit frmStart.Clss;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, frmScreenShot.Clss;

type
  TFormStart = class(TForm)
    ButtonStart: TButton;
    ButtonFinish: TButton;
    ButtonTravar: TButton;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonFinishClick(Sender: TObject);
    procedure ButtonTravarClick(Sender: TObject);
  private
    FFormScreenshot: TFormScreenshot;
  public
  end;

var
  FormStart: TFormStart;

implementation

{$R *.fmx}

procedure TFormStart.ButtonFinishClick(Sender: TObject);
begin
  if not Assigned(FFormScreenshot) then
    Exit;
  FreeAndNil(FFormScreenshot);
end;

procedure TFormStart.ButtonStartClick(Sender: TObject);
begin
  if Assigned(FFormScreenshot) then
    Exit;

  FFormScreenshot := TFormScreenshot.Create(nil);
  FFormScreenshot.Show;
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

end.

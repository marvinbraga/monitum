program Monitum;

uses
  System.StartUpCopy,
  FMX.Forms,
  frmStart.Clss in '..\source\frmStart.Clss.pas' {FormStart},
  uScreenShot.Clss in '..\source\uScreenShot.Clss.pas',
  uScreenShot in '..\source\uScreenShot.pas',
  uComponentsManager.Clss in '..\source\uComponentsManager.Clss.pas',
  uComponentsManager in '..\source\uComponentsManager.pas',
  frmScreenShot.Clss in '..\source\frmScreenShot.Clss.pas' {FormScreenshot};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormStart, FormStart);
  Application.Run;
end.

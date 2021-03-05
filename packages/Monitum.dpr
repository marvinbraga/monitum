program Monitum;

uses
  System.StartUpCopy,
  FMX.Forms,
  frmStart.Clss in '..\source\frmStart.Clss.pas' {FormStart},
  uScreenShot.Clss in '..\source\uScreenShot.Clss.pas',
  uScreenShot in '..\source\uScreenShot.pas',
  uComponentsManager.Clss in '..\source\uComponentsManager.Clss.pas',
  uComponentsManager in '..\source\uComponentsManager.pas',
  frmScreenShot.Clss in '..\source\frmScreenShot.Clss.pas' {FormScreenshot},
  uComponentsFactory in '..\source\uComponentsFactory.pas',
  uComponentsFactory.Clss in '..\source\uComponentsFactory.Clss.pas',
  uComponentsTypes in '..\source\uComponentsTypes.pas',
  uComponentsTypes.Clss in '..\source\uComponentsTypes.Clss.pas',
  Marvin.UI.ToastMessage.Clss in '..\source\Marvin.UI.ToastMessage.Clss.pas',
  Marvin.UI.ToastMessage.Frame in '..\source\Marvin.UI.ToastMessage.Frame.pas' {FrameToast: TFrame},
  Marvin.UI.ToastMessage in '..\source\Marvin.UI.ToastMessage.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormStart, FormStart);
  Application.Run;
end.

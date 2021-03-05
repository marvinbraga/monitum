unit Marvin.UI.ToastMessage.Clss;

interface

uses
  Marvin.UI.ToastMessage,
  FMX.Types,
  FMX.Controls;

type
  TToast = class(TInterfacedObject, IToast)
  private
    FToast: IToastMessage;
    FTimer: TTimer;
    procedure InitTimer;
    procedure OnTimer(Sender: TObject);
  protected
    function Show: IToast;
    procedure Hide;
  public
    class function New(const AParent: TControl; AMessage: string; const ATextAction: string): IToast; static;
    constructor Create(const AParent: TControl; AMessage: string; const ATextAction: string);
    destructor Destroy; override;
  end;

implementation

uses
  Marvin.UI.ToastMessage.Frame,
  System.Classes,
  System.SysUtils;

{ TToast }

class function TToast.New(const AParent: TControl; AMessage: string; const ATextAction: string): IToast;
begin
  Result := TToast.Create(AParent, AMessage, ATextAction);
end;

constructor TToast.Create(const AParent: TControl; AMessage: string; const ATextAction: string);
begin
  FToast := TFrameToast.New(AParent, AMessage, ATextAction);
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 5000;
  FTimer.OnTimer := Self.OnTimer;
end;

destructor TToast.Destroy;
var
  LComponent: TComponent;
begin
  FTimer.Enabled := False;
  FreeAndNil(FTimer);

  LComponent := TComponent(FToast);
  FToast := nil;
  FreeAndNil(LComponent);
  inherited;
end;

procedure TToast.InitTimer;
begin
  FTimer.Enabled := True;
end;

procedure TToast.OnTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
  Self.Hide;
end;

procedure TToast.Hide;
begin
  FTimer.Enabled := False;
  FToast.HideFrame;
end;

function TToast.Show: IToast;
begin
  Result := Self;
  FToast.ShowFrame;
  Self.InitTimer;
end;

end.


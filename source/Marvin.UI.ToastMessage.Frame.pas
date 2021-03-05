unit Marvin.UI.ToastMessage.Frame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  { marvin }
  Marvin.UI.ToastMessage,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Objects,
  FMX.Ani;

type
  TFrameToast = class(TFrame, IToastMessage)
    Layout: TLayout;
    RectToast: TRoundRect;
    TXT_Mensagem: TText;
    FloatAnimationMensagem: TFloatAnimation;
    TXT_Acao: TText;
    FloatAnimationAcao: TFloatAnimation;
    FloatAnimationRect: TFloatAnimation;
  private
    FOwner: TComponent;
  protected
    function ShowFrame: IToastMessage;
    function HideFrame: IToastMessage;
  public
    class function New(const AOwner: TComponent; const AMensagem: string; const ATextoAcao: string): IToastMessage;
    constructor Create(const AOwner: TComponent; const AMensagem: string; const ATextoAcao: string); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

{ TFrameToast }

class function TFrameToast.New(const AOwner: TComponent; const AMensagem: string; const ATextoAcao: string): IToastMessage;
begin
  { instancia o objeto }
  Result := TFrameToast.Create(AOwner, AMensagem, ATextoAcao);
end;

constructor TFrameToast.Create(const AOwner: TComponent; const AMensagem, ATextoAcao: string);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
  Self.Parent := TFmxObject(AOwner);
  Self.BringToFront;
  { posiciona }
  Self.Align := TAlignLayout.Horizontal;
  Self.Position.Y := TControl(AOwner).Height - Self.Height;
  Self.Layout.Visible := False;
end;

destructor TFrameToast.Destroy;
begin
  TFmxObject(FOwner).RemoveObject(Self);
  inherited;
end;

function TFrameToast.ShowFrame: IToastMessage;
begin
  Result := Self;
  { escondo o retângulo }
  RectToast.Align := TAlignLayout.None;
  RectToast.Position.Y := Self.Height + 1;
  { animações }
  FloatAnimationRect.StartValue := RectToast.Position.Y;
  { esconde o layout }
  Layout.Visible := True;
  { inicializa as animações }
  FloatAnimationRect.Start;
  FloatAnimationAcao.Start;
  FloatAnimationMensagem.Start;
end;

function TFrameToast.HideFrame: IToastMessage;
begin
  TAnimator.AnimateFloatWait(RectToast, 'Position.Y', Layout.Height, 0.5);
end;

end.


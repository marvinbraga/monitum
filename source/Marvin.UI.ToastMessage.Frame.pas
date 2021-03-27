unit Marvin.UI.ToastMessage.Frame;
{
   Monitum Project, 2021.
   Marcus Vinicius Braga, mvbraga@gmail.com.
   https://github.com/marvinbraga/monitum
   GNU General Public License v3.0
   See all terms in:
   https://github.com/marvinbraga/monitum/blob/main/LICENSE
}

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
    TextMensagem: TText;
    FloatAnimationMensagem: TFloatAnimation;
    TextAcao: TText;
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
  { texto }
  TextMensagem.Text := AMensagem;
  TextAcao.Text := ATextoAcao;
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


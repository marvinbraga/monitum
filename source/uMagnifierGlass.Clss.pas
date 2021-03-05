unit uMagnifierGlass.Clss;

interface

uses
  uMagnifierGlass,
  FMX.Types,
  FMX.MagnifierGlass,
  FMX.Controls,
  FMX.Graphics,
  System.Classes,
  System.UITypes,
  System.Types;

type
  TMagnifierGlassManager = class(TInterfacedObject, IMagnifierGlassManager)
  private
    const
      FC_MAX_ZOOM = 10;
      FC_MIN_ZOOM = 1;
  private
    FMagnifierGlass: TMagnifierGlass;
    FZoom: Single;
    FLoupeMode: TLoupeMode;
    FIsVisible: Boolean;
    FIsAbsolutePosition: Boolean;
    FTarget: TControl;
    FImage: TControl;
    FExternalTargetResize: TNotifyEvent;
    FExternalMouseDown: TMouseEvent;
    FExternalMouseMove: TMouseMoveEvent;
    procedure TargetResize(Sender: TObject);
    procedure TargetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure TargetMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure MagnifierGlassPainting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
  private
    FMousePos: TPointF;
    procedure SetMousePos(const AValue: TPointF);
    procedure MagnifierShowUpdate;
    procedure MagnifierChangeMode;
    property MousePos: TPointF read FMousePos write SetMousePos;
  protected
    function IsVisible: Boolean;
    function SetVisible(const AValue: Boolean): IMagnifierGlassManager;
    function IsAbsolutePosition: Boolean;
    function SetAbsolutePosition(const AValue: Boolean): IMagnifierGlassManager;
    function Zoom: Single;
    function SetZoom(const AValue: Single): IMagnifierGlassManager;
    function IncrementZoom: IMagnifierGlassManager;
    function DecrementZoom: IMagnifierGlassManager;
    function LoupeMode: TLoupeMode;
    function SetLoupeMode(const AValue: TLoupeMode): IMagnifierGlassManager;
  public
    class function New(const ATarget: TControl; const AImage: TControl): IMagnifierGlassManager; static;
    constructor Create(const ATarget: TControl; const AImage: TControl);
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils;

{ TMagnifierGlassManager }

class function TMagnifierGlassManager.New(const ATarget: TControl; const AImage: TControl): IMagnifierGlassManager;
begin
  Result := TMagnifierGlassManager.Create(ATarget, AImage);
end;

constructor TMagnifierGlassManager.Create(const ATarget: TControl; const AImage: TControl);
begin
  inherited Create;
  FTarget := ATarget;
  FImage := AImage;
  // recupera os eventos
  FExternalTargetResize := FTarget.OnResize;
  FExternalMouseDown := FTarget.OnMouseDown;
  FExternalMouseMove := FTarget.OnMouseMove;
  // atualiza os eventos
  FTarget.OnResize := Self.TargetResize;
  FTarget.OnMouseDown := Self.TargetMouseDown;
  FTarget.OnMouseMove := Self.TargetMouseMove;
  // inicializa
  FIsVisible := False;
  FIsAbsolutePosition := False;
  FMagnifierGlass := TMagnifierGlass.Create(FImage);
  FImage.AddObject(FMagnifierGlass);
  FMagnifierGlass.OnPaint := Self.MagnifierGlassPainting;
  Self.SetLoupeMode(TLoupeMode.Rectangle);

  Self.MagnifierShowUpdate;
  Self.SetZoom(2);
  Self.MousePos := TPointF.Zero;
  Self.MagnifierChangeMode;
end;

destructor TMagnifierGlassManager.Destroy;
begin
  FImage.RemoveObject(FMagnifierGlass);
  FreeAndNil(FMagnifierGlass);
  FTarget.OnResize := FExternalTargetResize;
  FTarget.OnMouseDown := FExternalMouseDown;
  FTarget.OnMouseMove := FExternalMouseMove;
  FTarget := nil;
  inherited;
end;

function TMagnifierGlassManager.DecrementZoom: IMagnifierGlassManager;
begin
  Result := Self;
  Self.SetZoom(FZoom - 1);
end;

function TMagnifierGlassManager.IncrementZoom: IMagnifierGlassManager;
begin
  Result := Self;
  Self.SetZoom(FZoom + 1);
end;

function TMagnifierGlassManager.IsAbsolutePosition: Boolean;
begin
  Result := FIsAbsolutePosition;
end;

function TMagnifierGlassManager.IsVisible: Boolean;
begin
  Result := FIsVisible;
end;

function TMagnifierGlassManager.LoupeMode: TLoupeMode;
begin
  Result := FLoupeMode;
end;

procedure TMagnifierGlassManager.MagnifierChangeMode;
var
  LPosition: TPointF;
begin
  if FIsAbsolutePosition then
  begin
    FMagnifierGlass.ZoomMode := TZoomMode.absolute;
    LPosition := FTarget.AbsoluteRect.BottomRight;
    LPosition := LPosition - FMagnifierGlass.LocalRect.BottomRight;
    FMagnifierGlass.Position.Point := LPosition;
  end
  else
  begin
    FMagnifierGlass.ZoomMode := TZoomMode.Relative;
    LPosition := FMagnifierGlass.LocalRect.CenterPoint *  - 1;
    FMagnifierGlass.ZoomRegionCenter.Point := LPosition;
  end;
end;

procedure TMagnifierGlassManager.MagnifierGlassPainting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  LCanvasState: TCanvasSaveState;
  LCenter, LP1, LP2: TPointF;
begin
  LCanvasState := Canvas.SaveState;
  try
    Canvas.Stroke.Color := TAlphaColors.Red;
    Canvas.Stroke.Thickness := 1;

    // Linha horizontal
    LCenter := ARect.CenterPoint + PointF(1, 1);
    LP1 := LCenter - PointF(10, 0);
    LP2 := LCenter + PointF(10, 0);
    Canvas.DrawLine(LP1, LP2, 0.8);

    // Linha vertical
    LP1 := LCenter - PointF(0, 10);
    LP2 := LCenter + PointF(0, 10);
    Canvas.DrawLine(LP1, LP2, 0.8);
  finally
    Canvas.RestoreState(LCanvasState);
  end;
end;

procedure TMagnifierGlassManager.MagnifierShowUpdate;
begin
  FMagnifierGlass.Visible := FIsVisible;
end;

function TMagnifierGlassManager.SetAbsolutePosition(const AValue: Boolean): IMagnifierGlassManager;
begin
  Result := Self;
  FIsAbsolutePosition := AValue;
  Self.MagnifierChangeMode;
end;

function TMagnifierGlassManager.SetLoupeMode(const AValue: TLoupeMode): IMagnifierGlassManager;
begin
  Result := Self;
  FLoupeMode := AValue;
  FMagnifierGlass.LoupeMode := FLoupeMode;
  if FLoupeMode = TLoupeMode.Rectangle then
  begin
    FMagnifierGlass.Height := 150;
    FMagnifierGlass.Width := 300;
  end;
end;

procedure TMagnifierGlassManager.SetMousePos(const AValue: TPointF);
var
  LPosition: TPointF;
begin
  if not FMousePos.EqualsTo(AValue) then
    FMousePos := AValue;

  if FMagnifierGlass.Visible then
  begin
    LPosition := FTarget.LocalToAbsolute(AValue);
    if FIsAbsolutePosition then
      FMagnifierGlass.ZoomRegionCenter.Point := LPosition
    else
      FMagnifierGlass.Position.Point := LPosition;
  end;
end;

function TMagnifierGlassManager.SetVisible(const AValue: Boolean): IMagnifierGlassManager;
begin
  Result := Self;
  FIsVisible := AValue;
  Self.MagnifierShowUpdate;
end;

function TMagnifierGlassManager.SetZoom(const AValue: Single): IMagnifierGlassManager;
begin
  Result := Self;
  if (AValue >= FC_MIN_ZOOM) and (AValue <= FC_MAX_ZOOM) then
    FZoom := AValue;
  FMagnifierGlass.LoupeScale := FZoom;
end;

procedure TMagnifierGlassManager.TargetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  Self.MousePos := PointF(X, Y);
  if Assigned(FExternalMouseDown) then
    FExternalMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TMagnifierGlassManager.TargetMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  Self.MousePos := PointF(X, Y);
  if Assigned(FExternalMouseMove) then
    FExternalMouseMove(Sender, Shift, X, Y);
end;

procedure TMagnifierGlassManager.TargetResize(Sender: TObject);
begin
  Self.MagnifierChangeMode;
  if Assigned(FExternalTargetResize) then
  begin
    FExternalTargetResize(Sender);
  end;
end;

function TMagnifierGlassManager.Zoom: Single;
begin
  Result := FZoom;
end;

end.


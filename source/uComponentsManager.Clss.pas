unit uComponentsManager.Clss;

// Monitum Projet, 2021.
// Marcus Vinicius Braga, all rights reserved.

interface

uses
  uComponentsManager,
  System.Classes,
  FMX.Objects,
  FMX.Layouts,
  FMX.Types,
  System.Types,
  System.UITypes,
  FMX.Graphics, FMX.Forms;

type
  TComponentsManager = class(TInterfacedObject, IComponentsManager)
  private
    FOwner: TForm;
    FLayoutMain: TLayout;
    FSlaveRect: TRectangle;
  private
    FMoveMode: Integer;
    FPosD, FPosM: TPointF;
    FMoveRec: TRectF;
    FSelected: TObject;
    FIsActivated: Boolean;
  private
    procedure LayoutMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure LayoutMainPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure SlaveRectMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure SlaveRectMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure SlaveRectMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure SlaveRectPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    function IsTarget(APoint: TPointF; ARect: TRectF): Integer;
    procedure CalcMode(var ARect: TRectF);
  protected
    function IsActivated: Boolean;
    function Activate: IComponentsManager;
    function Deactivate: IComponentsManager;
    function DeleteObject: IComponentsManager;
  public
    class function New(const AOwner: TForm; const ALayoutMain: TLayout; const ASlaveRect: TRectangle): IComponentsManager; static;
    constructor Create(const AOwner: TForm; const ALayoutMain: TLayout; const ASlaveRect: TRectangle);
    destructor Destroy; override;
  end;

implementation

uses
  FMX.Controls;

const
  FC_CHECK_POINT: array[1..8] of TPointF = ((
    X: 0;
    Y: 0
  ), (
    X: 0.5;
    Y: 0
  ), (
    X: 1;
    Y: 0
  ), (
    X: 0;
    Y: 0.5
  ), (
    X: 1;
    Y: 0.5
  ), (
    X: 0;
    Y: 1
  ), (
    X: 0.5;
    Y: 1
  ), (
    X: 1;
    Y: 1
  ));
  FC_CHECK_POINT_SIZE: Single = 4;

{ TComponentsManager }

class function TComponentsManager.New(const AOwner: TForm; const ALayoutMain: TLayout; const ASlaveRect: TRectangle): IComponentsManager;
begin
  Result := TComponentsManager.Create(AOwner, ALayoutMain, ASlaveRect);
end;

constructor TComponentsManager.Create(const AOwner: TForm; const ALayoutMain: TLayout; const ASlaveRect: TRectangle);
begin
  inherited Create;
  FOwner := AOwner;
  // conecta os objetos
  FLayoutMain := ALayoutMain;
  FSlaveRect := ASlaveRect;
  // conecta os eventos
  FLayoutMain.OnMouseDown := Self.LayoutMainMouseDown;
  FLayoutMain.OnPaint := Self.LayoutMainPaint;
  FSlaveRect.OnMouseDown := Self.SlaveRectMouseDown;
  FSlaveRect.OnMouseMove := Self.SlaveRectMouseMove;
  FSlaveRect.OnMouseUp := Self.SlaveRectMouseUp;
  FSlaveRect.OnPaint := Self.SlaveRectPaint;

  FSelected := nil;
  FSlaveRect.Visible := False;
  Self.Activate;
end;

destructor TComponentsManager.Destroy;
begin
  // desconecta os eventos
  FLayoutMain.OnMouseDown := nil;
  FLayoutMain.OnPaint := nil;
  FSlaveRect.OnMouseDown := nil;
  FSlaveRect.OnMouseMove := nil;
  FSlaveRect.OnMouseUp := nil;
  FSlaveRect.OnPaint := nil;
  // desconecta os objetos
  FOwner := nil;
  FLayoutMain := nil;
  FSlaveRect := nil;
  inherited;
end;

procedure TComponentsManager.CalcMode(var ARect: TRectF);
begin
  case FMoveMode of
    1:
      ARect.TopLeft := FPosM - FPosD;
    2:
      ARect.Top := FPosM.Y - FPosD.Y;
    3:
      begin
        ARect.Top := FPosM.Y - FPosD.Y;
        ARect.Right := FPosM.X - FPosD.X;
      end;
    4:
      ARect.Left := FPosM.X - FPosD.X;
    5:
      ARect.Right := FPosM.X - FPosD.X;
    6:
      begin
        ARect.Left := FPosM.X - FPosD.X;
        ARect.Bottom := FPosM.Y - FPosD.Y;
      end;
    7:
      ARect.Bottom := FPosM.Y - FPosD.Y;
    8:
      ARect.BottomRight := FPosM - FPosD;
  else
    ARect.Location := FPosM - FPosD;
  end;
end;

function TComponentsManager.IsTarget(APoint: TPointF; ARect: TRectF): Integer;
var
  LRect: TRectF;
  LLine: Single;
  LPoint: TPointF;
  LIndex: Integer;
begin
  Result := 0;
  LLine := FC_CHECK_POINT_SIZE * 2;
  LRect := ARect;
  LRect.Inflate(-LLine, -LLine);
  LPoint := LRect.BottomRight;
  for LIndex := Low(FC_CHECK_POINT) to High(FC_CHECK_POINT) do
  begin
    LRect := TRectF.Create(0, 0, LLine, LLine);
    LRect.Offset(FC_CHECK_POINT[LIndex] * LPoint);
    if LRect.Contains(APoint) then
    begin
      Exit(LIndex);
    end;
  end;
end;

function TComponentsManager.IsActivated: Boolean;
begin
  Result := FIsActivated;
end;

function TComponentsManager.Activate: IComponentsManager;
var
  LIndex, LEnd: Integer;
  LComp: TRectangle;
  LRect: TRectF;
  LContent: TContent;
begin
  Result := Self;
  FIsActivated := True;

  FSelected := nil;
  LEnd := FLayoutMain.ChildrenCount - 1;
  for LIndex := LEnd downto 0 do
  begin
    if FLayoutMain.Children[LIndex] = FSlaveRect then
      Continue;

    LComp := TRectangle(FSlaveRect.Clone(nil));
    LComp.Visible := True;
    LContent := TContent(FLayoutMain.Children[LIndex]);
    LRect := LContent.AbsoluteRect;
    LRect.Offset(0, 0 - FLayoutMain.Position.Y);
    LComp.SetBounds(LRect.Left, LRect.Top, LRect.Width, LRect.Height);
    LContent.Position.Point := TPointF.Zero;
    LContent.HitTest := False;
    LContent.Locked := True;
    LContent.Align := TAlignLayout.Client;
    LComp.HitTest := True;
    LComp.AddObject(LContent);
    LComp.OnMouseDown := SlaveRectMouseDown;
    LComp.OnMouseMove := SlaveRectMouseMove;
    LComp.OnMouseUp := SlaveRectMouseUp;
    LComp.OnPaint := SlaveRectPaint;
    FLayoutMain.AddObject(LComp);
  end;
end;

function TComponentsManager.Deactivate: IComponentsManager;
var
  LIndex, LEnd: Integer;
  LComp: TRectangle;
  LRect: TRectF;
  LContent: TContent;
begin
  Result := Self;
  FIsActivated := False;
  FSelected := nil;
  LEnd := FLayoutMain.ChildrenCount - 1;
  for LIndex := LEnd downto 0 do
  begin
    if FLayoutMain.Children[LIndex] = FSlaveRect then
      Continue;

    LComp := TRectangle(FLayoutMain.Children[LIndex]);
    LContent := TContent(LComp.Children[0]);
    LRect := LComp.AbsoluteRect;
    LRect.Offset(0, 0 - FLayoutMain.Position.Y);
    LContent.HitTest := True;
    LContent.Locked := False;
    LContent.Align := TAlignLayout.None;
    FLayoutMain.RemoveObject(LComp);
    FLayoutMain.AddObject(LContent);
    LContent.SetBounds(LRect.Left, LRect.Top, LRect.Width, LRect.Height);
  end;
end;

function TComponentsManager.DeleteObject: IComponentsManager;
begin
  Result := Self;
  if Assigned(FSelected) then
  begin
    FLayoutMain.RemoveObject(TFmxObject(FSelected));
    FSelected.DisposeOf;
    FSelected := nil;
    FLayoutMain.Repaint;
  end;
end;

procedure TComponentsManager.LayoutMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if ssLeft in Shift then
  begin
    FSelected := nil;
    FLayoutMain.Repaint;
  end;
end;

procedure TComponentsManager.LayoutMainPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  LCanvasState: TCanvasSaveState;
  LRect: TRectF;
  LStroke: TStrokeBrush;
  LLine: Single;
begin
  if not Assigned(FSelected) then
    Exit;

  LCanvasState := FOwner.Canvas.SaveState;
  try
    // pinta a linha de seleção no objeto clicado
    LStroke := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColors.Blue);
    try
      LRect := FMoveRec;
      LLine := TRectangle(FSelected).Stroke.Thickness * 0.5;
      LRect.Inflate(-LLine, -LLine);
      LStroke.Dash := TStrokeDash.Dash;
      CalcMode(LRect);
      FOwner.Canvas.DrawRect(LRect, 0, 0, [], 1, LStroke);
    finally
      LStroke.Free;
    end;
  finally
    FOwner.Canvas.RestoreState(LCanvasState);
  end;
end;

procedure TComponentsManager.SlaveRectMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if ssLeft in Shift then
  begin
    // seleciona o retangulo
    FMoveRec := TRectangle(Sender).AbsoluteRect;
    FMoveRec.Offset(0, 0 - FLayoutMain.Position.Y);
    FMoveMode := Self.IsTarget(TPointF.Create(X, Y), TRectangle(Sender).LocalRect);
    FPosM := TRectangle(Sender).LocalToAbsolute(TPointF.Create(X, Y));
    FPosD := TPointF.Zero;
    case FMoveMode of
      1:
        FPosD := FPosM - FMoveRec.TopLeft;
      2:
        FPosD.Y := FPosM.Y - FMoveRec.Top;
      3:
        FPosD := FPosM - TPointF.Create(FMoveRec.Right, FMoveRec.Top);
      4:
        FPosD.X := FPosM.X - FMoveRec.Left;
      5:
        FPosD.X := FPosM.X - FMoveRec.Right;
      6:
        FPosD := FPosM - TPointF.Create(FMoveRec.Left, FMoveRec.Bottom);
      7:
        FPosD.Y := FPosM.Y - FMoveRec.Bottom;
      8:
        FPosD := FPosM - FMoveRec.BottomRight;
    else
      FPosD := FPosM - FMoveRec.Location;
    end;
    FSelected := Sender;
    TRectangle(FSelected).AutoCapture := True;
    FLayoutMain.Repaint;
  end;
end;

procedure TComponentsManager.SlaveRectMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if (ssLeft in Shift) and (FSelected = Sender) then
  begin
    // altera a posição do retangulo
    FPosM := TRectangle(Sender).LocalToAbsolute(TPointF.Create(X, Y));
    FLayoutMain.Repaint;
  end;
end;

procedure TComponentsManager.SlaveRectMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  LRect: TRectF;
begin
  // retira a borda de seleção do retangulo
  LRect := TRectangle(FSelected).AbsoluteRect;
  LRect.Offset(0, 0 - FLayoutMain.Position.Y);
  CalcMode(LRect);
  LRect.NormalizeRect;
  TRectangle(FSelected).SetBounds(LRect.Left, LRect.Top, LRect.Width, LRect.Height);
  TRectangle(FSelected).AutoCapture := False;
  FLayoutMain.Repaint;
end;

procedure TComponentsManager.SlaveRectPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  LRect: TRectF;
  LLine: Single;
  LCanvasState: TCanvasSaveState;
  LBrush: TBrush;
  LIndex: Integer;
  LPoint: TPointF;
begin
  // pintando os quadrados pretos no retangulos
  if Sender <> FSelected then
    Exit;

  LCanvasState := FOwner.Canvas.SaveState;
  try
    LBrush := TBrush.Create(TBrushKind.Solid, TAlphaColors.Red);
    try
      LLine := FC_CHECK_POINT_SIZE * 2;
      LRect := ARect;
      LRect.Inflate(-LLine, -LLine);
      LPoint := LRect.BottomRight;

      for LIndex := Low(FC_CHECK_POINT) to High(FC_CHECK_POINT) do
      begin
        LBrush.Color := TAlphaColors.Black;
        if FMoveMode = LIndex then
        begin
          LBrush.Color := TAlphaColors.Yellow;
        end;
        LRect := TRectF.Create(0, 0, LLine, LLine);
        LRect.Offset(FC_CHECK_POINT[LIndex] * LPoint);
        FOwner.Canvas.FillRect(LRect, 0, 0, [], 1, LBrush);
      end;
    finally
      LBrush.Free;
    end;
  finally
    FOwner.Canvas.RestoreState(LCanvasState);
  end;
end;

end.


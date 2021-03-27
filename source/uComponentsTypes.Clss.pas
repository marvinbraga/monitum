unit uComponentsTypes.Clss;
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
  uComponentsTypes,
  System.UITypes,
  FMX.Graphics;

type
  TFill = class(TInterfacedObject, IFill)
  private
    FColor: TAlphaColor;
  protected
    function Color: TAlphaColor;
  public
    class function New(const AColor: TAlphaColor): IFill; static;
    constructor Create(const AColor: TAlphaColor);
    destructor Destroy; override;
  end;

  TStroke = class(TInterfacedObject, IStroke)
  private
    FColor: TAlphaColor;
    FDash: TStrokeDash;
    FKind: TBrushKind;
    FThickness: Single;
  protected
    function Color: TAlphaColor;
    function Dash: TStrokeDash;
    function Kind: TBrushKind;
    function Thickness: Single;
  public
    class function New(const AColor: TAlphaColor; const ADash: TStrokeDash; const AKind: TBrushKind; const AThickness: Single): IStroke; static;
    constructor Create(const AColor: TAlphaColor; const ADash: TStrokeDash; const AKind: TBrushKind; const AThickness: Single);
    destructor Destroy; override;
  end;

  TShape = class(TInterfacedObject, IShape)
  private
    FFill: IFill;
    FStroke: IStroke;
  protected
    function Fill: IFill;
    function Stroke: IStroke;
    function Clone: IShape;
  public
    class function New(const AFill: IFill; const AStroke: IStroke): IShape; static;
    constructor Create(const AFill: IFill; const AStroke: IStroke);
    destructor Destroy; override;
  end;

implementation

{ TFill }

class function TFill.New(const AColor: TAlphaColor): IFill;
begin
  Result := TFill.Create(AColor);
end;

constructor TFill.Create(const AColor: TAlphaColor);
begin
  inherited Create;
  FColor := AColor;
end;

destructor TFill.Destroy;
begin
  inherited;
end;

function TFill.Color: TAlphaColor;
begin
  Result := FColor;
end;

{ TStroke }

class function TStroke.New(const AColor: TAlphaColor; const ADash: TStrokeDash; const AKind: TBrushKind; const AThickness: Single): IStroke;
begin
  Result := TStroke.Create(AColor, ADash, AKind, AThickness);
end;

constructor TStroke.Create(const AColor: TAlphaColor; const ADash: TStrokeDash; const AKind: TBrushKind; const AThickness: Single);
begin
  inherited Create;
  FColor := AColor;
  FDash := ADash;
  FKind := AKind;
  FThickness := AThickness;
end;

destructor TStroke.Destroy;
begin
  inherited;
end;

function TStroke.Color: TAlphaColor;
begin
  Result := FColor;
end;

function TStroke.Dash: TStrokeDash;
begin
  Result := FDash;
end;

function TStroke.Kind: TBrushKind;
begin
  Result := FKind;
end;

function TStroke.Thickness: Single;
begin
  Result := FThickness;
end;

{ TShape }

class function TShape.New(const AFill: IFill; const AStroke: IStroke): IShape;
begin
  Result := TShape.Create(AFill, AStroke);
end;

constructor TShape.Create(const AFill: IFill; const AStroke: IStroke);
begin
  inherited Create;
  FFill := AFill;
  FStroke := AStroke;
end;

destructor TShape.Destroy;
begin
  FFill := nil;
  FStroke := nil;
  inherited;
end;

function TShape.Clone: IShape;
begin
  Result := TShape.New(TFill.New(FFill.Color), TStroke.New(FStroke.Color, FStroke.Dash, FStroke.Kind, FStroke.Thickness));
end;

function TShape.Fill: IFill;
begin
  Result := FFill;
end;

function TShape.Stroke: IStroke;
begin
  Result := FStroke;
end;

end.


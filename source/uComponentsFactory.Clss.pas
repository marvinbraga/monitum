unit uComponentsFactory.Clss;
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
  uComponentsFactory,
  uComponentsTypes,
  FMX.Objects,
  FMX.Layouts;

type
  TComponentsFactory = class(TInterfacedObject, IComponentsFactory)
  private
    FLayout: TLayout;
  protected
    function CreateRectangle(const AShape: IShape): TRectangle;
  public
    class function New(const ALayout: TLayout): IComponentsFactory; static;
    constructor Create(const ALayout: TLayout);
    destructor Destroy; override;
  end;

implementation

{ TComponentsFactory }

class function TComponentsFactory.New(const ALayout: TLayout): IComponentsFactory;
begin
  Result := TComponentsFactory.Create(ALayout);
end;

constructor TComponentsFactory.Create(const ALayout: TLayout);
begin
  inherited Create;
  FLayout := ALayout;
end;

destructor TComponentsFactory.Destroy;
begin
  FLayout := nil;
  inherited;
end;

function TComponentsFactory.CreateRectangle(const AShape: IShape): TRectangle;
begin
  Result := TRectangle.Create(FLayout);
  FLayout.AddObject(Result);
  Result.Fill.Color := AShape.Fill.Color;
  Result.Stroke.Color := AShape.Stroke.Color;
  Result.Stroke.Kind := AShape.Stroke.Kind;
  Result.Stroke.Dash := AShape.Stroke.Dash;
  Result.Stroke.Thickness := AShape.Stroke.Thickness;
end;

end.


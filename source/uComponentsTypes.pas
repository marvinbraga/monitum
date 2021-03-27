unit uComponentsTypes;
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
  System.UITypes,
  FMX.Graphics;

type
  IFill = interface
    ['{C4E103C1-295A-40D1-9ED0-9A0B594AA310}']
    function Color: TAlphaColor;
  end;

  IStroke = interface
    ['{35E9B06C-A33D-4122-958C-FC846D1BF849}']
    function Color: TAlphaColor;
    function Dash: TStrokeDash;
    function Kind: TBrushKind;
    function Thickness: Single;
  end;

  IShape = interface
    ['{6CEEA219-5731-47E1-81C8-805E0FC49638}']
    function Fill: IFill;
    function Stroke: IStroke;
    function Clone: IShape;
  end;

implementation

end.


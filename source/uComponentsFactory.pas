unit uComponentsFactory;
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
  FMX.Objects;

type
  IComponentsFactory = interface
    ['{1FC25698-FC3E-4914-A45C-6BFF775D172C}']
    function CreateRectangle(const AShape: IShape): TRectangle;
  end;

implementation

end.


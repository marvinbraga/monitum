unit uComponentsManager;
{
   Monitum Project, 2021.
   Marcus Vinicius Braga, mvbraga@gmail.com.
   https://github.com/marvinbraga/monitum
   GNU General Public License v3.0
   See all terms in:
   https://github.com/marvinbraga/monitum/blob/main/LICENSE
}

interface

type
  IComponentsManager = interface
    ['{A2C55EA7-5899-4258-9B33-3D4E19D8948A}']
    function IsActivated: Boolean;
    function Activate: IComponentsManager;
    function Deactivate: IComponentsManager;
    function DeleteObject: IComponentsManager;
  end;

implementation

end.

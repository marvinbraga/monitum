unit uComponentsManager;

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

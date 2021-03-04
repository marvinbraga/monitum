unit uComponentsFactory;

// Monitum Projet, 2021.
// Marcus Vinicius Braga, all rights reserved.

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


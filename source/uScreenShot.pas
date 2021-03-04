unit uScreenShot;

interface

uses
  FMX.Graphics;

type
  IScreenShot = interface
    ['{026A7002-AD30-47FD-A7B8-D633F3FAB130}']
    function Execute: IScreenShot;
    function Bitmap: TBitmap;
  end;

implementation

end.

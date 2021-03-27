unit uScreenShot;
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
  FMX.Graphics;

type
  IScreenShot = interface
    ['{026A7002-AD30-47FD-A7B8-D633F3FAB130}']
    function Execute: IScreenShot;
    function Bitmap: TBitmap;
  end;

implementation

end.

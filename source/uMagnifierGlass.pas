unit uMagnifierGlass;
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
  FMX.MagnifierGlass;

type
  IMagnifierGlassManager = interface
    ['{55A9F8AE-222B-4542-A9C5-B60D82D668AC}']
    function IsVisible: Boolean;
    function SetVisible(const AValue: Boolean): IMagnifierGlassManager;
    function IsAbsolutePosition: Boolean;
    function SetAbsolutePosition(const AValue: Boolean): IMagnifierGlassManager;
    function Zoom: Single;
    function SetZoom(const AValue: Single): IMagnifierGlassManager;
    function IncrementZoom: IMagnifierGlassManager;
    function DecrementZoom: IMagnifierGlassManager;
    function LoupeMode: TLoupeMode;
    function SetLoupeMode(const AValue: TLoupeMode): IMagnifierGlassManager;
  end;

implementation

end.

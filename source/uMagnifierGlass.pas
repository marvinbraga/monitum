unit uMagnifierGlass;

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

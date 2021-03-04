unit uScreenShot.Clss;

{ =====================================================================
   How to use:
     TScreenShot.New(YourBitmapObject).Execute;
     TScreenShot.New.Execute.Bitmap;
  ====================================================================== }

interface

uses
  Vcl.Graphics,
  FMX.Graphics,
  uScreenShot;

type
  TVclBitmap = Vcl.Graphics.TBitmap;

  TScreenShot = class(TInterfacedObject, IScreenShot)
  private
    FBitmap: TBitmap;
    FTarget: TBitmap;
    class procedure TakeScreenshot(const ATarget: TBitmap); static;
    class procedure CopyBitmap(const ASource: TVclBitmap; const ATarget: TBitmap); static;
  protected
    function Execute: IScreenShot;
    function Bitmap: TBitmap;
  public
    class function New: IScreenShot; overload; static;
    class function New(const ATarget: TBitmap): IScreenShot; overload; static;
    constructor Create; overload;
    constructor Create(const ATarget: TBitmap); overload;
    destructor Destroy; override;
  end;

implementation

uses
  FMX.Forms,
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Types;

{ TScreenShot }

class function TScreenShot.New: IScreenShot;
begin
  Result := TScreenShot.Create;
end;

class function TScreenShot.New(const ATarget: FMX.Graphics.TBitmap): IScreenShot;
begin
  Result := TScreenShot.Create(ATarget);
end;

constructor TScreenShot.Create;
begin
  inherited;
  FTarget := nil;
  FBitmap := FMX.Graphics.TBitmap.Create;
end;

constructor TScreenShot.Create(const ATarget: FMX.Graphics.TBitmap);
begin
  Self.Create;
  FTarget := ATarget;
end;

destructor TScreenShot.Destroy;
begin
  FTarget := nil;
  FreeAndNil(FBitmap);
  inherited;
end;

function TScreenShot.Bitmap: FMX.Graphics.TBitmap;
begin
  Result := FBitmap;
end;

function TScreenShot.Execute: IScreenShot;
begin
  Result := Self;
  TScreenShot.TakeScreenshot(FBitmap);
  if Assigned(FTarget) then
  begin
    FTarget.Size := FBitmap.Size;
    FTarget.CopyFromBitmap(FBitmap);
  end;
end;

class procedure TScreenShot.TakeScreenshot(const ATarget: FMX.Graphics.TBitmap);
var
  LDc: HDC;
  LSize: TPoint;
  LVclBitmap: TVclBitmap;
begin
  LSize.X := Screen.Size.Width;
  LSize.Y := Screen.Size.Height;
  // Getting Screenshot
  LDc := GetDC(0);
  LVclBitmap := TVclBitmap.Create;
  try
    LVclBitmap.PixelFormat := pf32bit;
    LVclBitmap.SetSize(LSize.X, LSize.Y);
    BitBlt(LVclBitmap.Canvas.Handle, 0, 0, LVclBitmap.Width, LVclBitmap.Height, LDc, 0, 0, SRCCOPY);
    ATarget.SetSize(LVclBitmap.Width, LVclBitmap.Height);
    // Saving Screenshot
    TScreenShot.CopyBitmap(LVclBitmap, ATarget);
  finally
    ReleaseDC(0, LDc);
    LVclBitmap.Free;
  end;
end;

class procedure TScreenShot.CopyBitmap(const ASource: TVclBitmap; const ATarget: FMX.Graphics.TBitmap);
var
  LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    ASource.SaveToStream(LStream);
    LStream.Position := 0;
    ATarget.LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
end;

end.


{
 * u_BitmapCursor.pas
 * Copyright (C) 2012- Nick Bonnière
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

// tried - works OK, but limited to 32x32 pixels
// for an 8x8 brush, scale must be 4 or less

//----------------------------------------------------------------------------
UNIT u_BitmapCursor;

{----------------------------------------------------------------------------
White mask -> keep screen bits, non-white mask -> clear screen bits
Color mask -> XOR with masked screen bits
 ----------------------------------------------------------------------------}

{============================================================================}
INTERFACE

uses Forms, Windows, Graphics;

const
  crMyCursor = 1; //any number > 0

var
  CC_bmpMask : TBitmap;
  CC_bmpColor : TBitmap;

procedure CC_Create(Sender: TObject);

{============================================================================}
IMPLEMENTATION

{----------------------------------------------------------------------------}
procedure CC_Create(Sender: TObject);
var
  iconInfo : TIconInfo;

begin
//  CC_bmpMask.LoadFromFile('CircleMask.bmp');
//  CC_bmpColor.LoadFromFile('Circle.bmp');
//  CC_bmpMask.LoadFromFile('Mask.bmp');
//  CC_bmpColor.LoadFromFile('Color.bmp');
  with iconInfo do
  begin
    fIcon := false;
    xHotspot := 15;
    yHotspot := 15;
    hbmMask := CC_bmpMask.Handle;    // AND with screen bits
    hbmColor := CC_bmpColor.Handle;  // then XOR with screen bits
  end;

  Screen.Cursors[crMyCursor] := CreateIconIndirect(iconInfo);

end;

{----------------------------------------------------------------------------}
procedure CC_Destroy;
begin
  DestroyIcon(Screen.Cursors[crMyCursor]);
end;

{----------------------------------------------------------------------------}
begin { Initialization }
  CC_bmpMask := TBitmap.Create;
  CC_bmpColor := TBitmap.Create;
end.

{--- End of File ------------------------------------------------------------}


{
 * u_ReduceColors.pas
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

//----------------------------------------------------------------------------
UNIT u_ReduceColors;

{============================================================================}
INTERFACE

uses Windows, Graphics,
    PaletteLibrary;

var
  ColorsReduced : boolean;
  NumColors : integer;
  RGBQuadArray   : TRGBQuadArray;

function ReduceColors(Source: TBitmap; MaxColors: Integer):TBitmap;

{============================================================================}
IMPLEMENTATION

 uses ColorQuantizationLibrary;

//----------------------------------------------------------------------------
function ReduceColors(Source: TBitmap; MaxColors: Integer):TBitmap;
const
  ColorBits = 8;
//  RGBblank : TRGBquad = (rgbBlue:0;rgbGreen:0;rgbRed:0;rgbReserved:0);

var
  ColorQuantizer : TColorQuantizer;
//  RGBQuadArray   : TRGBQuadArray;
  i:integer;

begin
  NumColors := MaxColors;
  Result := TBitmap.Create;
  Result.PixelFormat := pf8bit;
  Result.Width := Source.Width;
  Result.Height := Source.Height;
  ColorQuantizer := TColorQuantizer.Create(MaxColors, 8);
  try
    ColorQuantizer.ProcessImage(Source.Handle);
    ColorQuantizer.GetColorTable(RGBQuadArray);
    for i := MaxColors to 256-1 do begin
      RGBQuadArray[i] := RGBQuadArray[MaxColors-1];
    end;
    SetDIBColorTable(Result.Canvas.Handle, 0, 256, RGBQuadArray);
    Result.Canvas.Draw(0, 0, Source);
    ColorsReduced := true;
  finally
    ColorQuantizer.Free
  end;
//  Result.PixelFormat := pf24bit; //change back to 24 bit format
  Source.Canvas.Draw(0, 0, Result);
  Result.Free;
end;

//----------------------------------------------------------------------------
begin
  ColorsReduced := false;
end.

//--- End of File ------------------------------------------------------------

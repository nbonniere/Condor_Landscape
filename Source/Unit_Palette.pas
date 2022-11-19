{
 * Unit_Palette.pas
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

//---------------------------------------------------------------------------
unit Unit_Palette;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, ExtCtrls;

type
  TForm_Palette = class(TForm)
    Image_Palette: TImage;
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form_Palette: TForm_Palette;

//---------------------------------------------------------------------------
implementation

{$R *.DFM}

uses u_BMP, u_ReduceColors;

//---------------------------------------------------------------------------
procedure TForm_Palette.FormActivate(Sender: TObject);
var
  i,j : integer;
  ColorRows, ColorColumns : integer;
  x : real;

begin
  if (ColorsReduced) then begin
    x := sqrt(NumColors);
    ColorColumns := trunc(x);
    if (Frac(x) <> 0) then begin
      INC(ColorColumns);
    end;
    x := NumColors/ColorColumns;
    ColorRows := trunc(x);
    if (Frac(x) <> 0) then begin
      INC(ColorRows);
    end;
    with Image_Palette do begin
      Canvas.Brush.Color := clBlack;
      Canvas.FillRect(rect(0,0,Width,Height));
      for i := 0 to ColorRows-1 do begin
        for j := 0 to ColorColumns-1 do begin
          if (i*ColorColumns+j < NumColors) then begin
//            Canvas.Brush.Color := Tcolor(RGBQuadArray[i*ColorColumns+j]);
            Canvas.Brush.Color := ByteSwapColor(Tcolor(RGBQuadArray[i*ColorColumns+j]));
            Picture.Bitmap.Canvas.FillRect(rect(j*16,i*16,j*16+15,i*16+15));
          end;
        end;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
end.

//--- End of File -----------------------------------------------------------

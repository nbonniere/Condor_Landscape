{
 * u_RGB_CIE.pas
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
unit u_RGB_CIE;

{----------------------------------------------------------------------------

----------------------------------------------------------------------------}

{============================================================================}
interface
  uses
    Math;

procedure RGBtoCIElab(R, G, B: Single; out cie_L, cie_a, cie_b: Single);

{============================================================================}
implementation

//---------------------------------------------------------------------------
procedure RGBtoCIElab(R, G, B: Single; out cie_L, cie_a, cie_b: Single);
var
  X, Y, Z : single;
begin
// Step 1: RGB to XYZ
//         http://www.easyrgb.com/index.php?X=MATH&H=02#text2
// Step 2: XYZ to Lab
//         http://www.easyrgb.com/index.php?X=MATH&H=07#text7

    // first RGB to XYZ
    R := R /255;
    if (R > 0.04045) then begin
      R := Power((R +0.055) /1.055, 2.4);
    end else begin
     R := R /12.92;
    end;
    G := G /255;
    if (G > 0.04045) then begin
      G := Power((G +0.055) /1.055, 2.4);
    end else begin
     G := G /12.92;
    end;
    B := B /255;
    if (B > 0.04045) then begin
      B := Power((B +0.055) /1.055, 2.4);
    end else begin
     B := B /12.92;
    end;

    X := R * 41.24 + G * 35.76 + B * 18.05;
    Y := R * 21.26 + G * 71.52 + B * 07.22;
    Z := R * 01.93 + G * 11.92 + B * 95.04;

    // then XYZ to cieLAB
    // Observer = 2°, Illuminant= D65
    X := X / 95.047;         // ref_X =  95.047
    Y := Y / 100.0;          // ref_Y = 100.000
    Z := Z / 108.883 ;       // ref_Z = 108.883

    if (X > 0.008856) then begin
      X := Power(X, 0.3333333333333333);
    end else begin
      X := (7.787 * X) + (16.0 / 116.0);
    end;
    if (Y > 0.008856) then begin
      Y := Power(Y, 0.3333333333333333);
    end else begin
      Y := (7.787 * Y) + (16.0 / 116.0);
    end;
    if (Z > 0.008856) then begin
      Z := Power(Z, 0.3333333333333333);
    end else begin
      Z := (7.787 * Z) + (16.0 / 116.0);
    end;

    cie_L := (116 * Y) - 16;   // 0..100
    cie_a := 500 * (X - Y);    // typically +/-150
    cie_b := 200 * (Y - Z);    // typically +/-150
end;

//---------------------------------------------------------------------------
var
  LL, AA, BB : single;
begin
   RGBtoCIElab(64,128,192,LL, AA, BB);
end.


{
 * u_Convolve.pas
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
unit u_Convolve;

//----------------------------------------------------------------------------
interface

uses
  Windows, SysUtils, Graphics, Controls, Forms,
  StdCtrls, Grids, comctrls;

{Some edge detection filters:

laplace      hipass     find edges   sharpen    edge enhance  color emboss
                        (top down)                            (well, kinda)
-1 -1 -1    -1 -1 -1     1  1  1     -1 -1 -1     0 -1  0       1  0  1
-1  8 -1    -1  9 -1     1 -2  1     -1 16 -1    -1  5 -1       0  0  0
-1 -1 -1    -1 -1 -1    -1 -1 -1     -1 -1 -1     0 -1  0       1  0 -2

    1           1           1            8           1             1

 Soften        blur    Soften (less)   lowpass  Gaussian

 2  2  2     3  3  3     0  1  0      1  1  1    1  2  1
 2  0  2     3  8  3     1  2  1      1  1  1    2  4  2
 2  2  2     3  3  3     0  1  0      1  1  1    1  2  1

   16          32           6            9          16
}

var
  Memo_Message : TMemo;  // external TMemo for messages
  ProgressBar_Status : TProgressBar;
  StringGrid1 : TStringGrid;
  Edit_Divisor : TEdit;

procedure Convolve_Swell(aBmp : TBitmap; Match : TRGBTriple);
procedure Convolve_Shrink(aBmp : TBitmap; Match : TRGBTriple);
procedure Convolve_Matrix(aBmp : TBitmap);
Procedure BitmapShrink_File(Filename : string; Match : TRGBTriple);
Procedure ForestBitmapShrink_File(FileSource, FileDestination : string);

//----------------------------------------------------------------------------
implementation

uses u_BMP, u_Util;

//----------------------------------------------------------------------------
procedure Convolve_Filter(ray : array of integer; z : word; aBmp : TBitmap);
var
  O, T, C, B : pRGBArray;  // Scanlines
  x, y : integer;
  xp1, yp1 : integer;
  xm1, ym1 : integer;
  tBufr : TBitmap; // temp bitmap
begin
  tBufr := TBitmap.Create;
  BMP_CopyMe(tBufr,aBmp);
  ProgressBar_Status.Max := aBmp.Height;

  for x := 0 to aBmp.Height - 1 do begin  // Walk scanlines
    if (x = 0) then begin
      xm1 := x;
      xp1 := x+1;
    end else begin
      if (x = aBmp.Height-1) then begin
        xp1 := x;
        xm1 := x-1;
      end
      else begin
        xm1 := x-1;
        xp1 := x+1;
      end;
    end;
    O := aBmp.ScanLine[x];      // New Target (Original)
    T := tBufr.ScanLine[xm1];     //old x-1  (Top)
    C := tBufr.ScanLine[x];   //old x    (Center)
    B := tBufr.ScanLine[xp1];   //old x+1  (Bottom)
  // Now do the main piece
    for y := 0 to (tBufr.Width - 1) do begin  // Walk pixels
      if (y = 0) then begin
        ym1 := y;
        yp1 := y+1;
      end else begin
        if (y = tBufr.Width-1) then begin
          yp1 := y;
          ym1 := y-1;
        end
        else begin
          ym1 := y-1;
          yp1 := y+1;
        end;
      end;
//      O[y].rgbtRed := Set255(
      O[y].rgbtRed := ClampByte(
          ((T[ym1].rgbtRed*ray[0]) +
          (T[y].rgbtRed*ray[1]) + (T[yp1].rgbtRed*ray[2]) +
          (C[ym1].rgbtRed*ray[3]) +
          (C[y].rgbtRed*ray[4]) + (C[yp1].rgbtRed*ray[5])+
          (B[ym1].rgbtRed*ray[6]) +
          (B[y].rgbtRed*ray[7]) + (B[yp1].rgbtRed*ray[8])) div z
          );
//      O[y].rgbtBlue := Set255(
      O[y].rgbtBlue := ClampByte(
          ((T[ym1].rgbtBlue*ray[0]) +
          (T[y].rgbtBlue*ray[1]) + (T[yp1].rgbtBlue*ray[2]) +
          (C[ym1].rgbtBlue*ray[3]) +
          (C[y].rgbtBlue*ray[4]) + (C[yp1].rgbtBlue*ray[5])+
          (B[ym1].rgbtBlue*ray[6]) +
          (B[y].rgbtBlue*ray[7]) + (B[yp1].rgbtBlue*ray[8])) div z
          );
//      O[y].rgbtGreen := Set255(
      O[y].rgbtGreen := ClampByte(
          ((T[ym1].rgbtGreen*ray[0]) +
          (T[y].rgbtGreen*ray[1]) + (T[yp1].rgbtGreen*ray[2]) +
          (C[ym1].rgbtGreen*ray[3]) +
          (C[y].rgbtGreen*ray[4]) + (C[yp1].rgbtGreen*ray[5])+
          (B[ym1].rgbtGreen*ray[6]) +
          (B[y].rgbtGreen*ray[7]) + (B[yp1].rgbtGreen*ray[8])) div z
          );
    end;
    ProgressBar_Status.StepIt;
    Application.ProcessMessages;
  end;
  ProgressBar_Status.Position := 0;
  tBufr.Free;
end;

//----------------------------------------------------------------------------
procedure Convolve_Swell(aBmp : TBitmap; Match : TRGBTriple);
var
  O, T, C, B : pRGBArray;  // Scanlines
  x, y : integer;
  xp1, yp1 : integer;
  xm1, ym1 : integer;
  tBufr : TBitmap; // temp bitmap

begin
  Screen.Cursor := crHourGlass;  // Let user know we're busy...
  tBufr := TBitmap.Create;
  BMP_CopyMe(tBufr,aBmp);
  ProgressBar_Status.Max := aBmp.Height;

  for x := 0 to aBmp.Height - 1 do begin  // Walk scanlines
    if (x = 0) then begin
      xm1 := x;
      xp1 := x+1;
    end else begin
      if (x = aBmp.Height-1) then begin
        xp1 := x;
        xm1 := x-1;
      end
      else begin
        xm1 := x-1;
        xp1 := x+1;
      end;
    end;
    O := aBmp.ScanLine[x];      // New Target (Original)
    T := tBufr.ScanLine[xm1];     //old x-1  (Top)
    C := tBufr.ScanLine[x];   //old x    (Center)
    B := tBufr.ScanLine[xp1];   //old x+1  (Bottom)
  // Now do the main piece
    for y := 0 to (tBufr.Width - 1) do begin  // Walk pixels
      if (y = 0) then begin
        ym1 := y;
        yp1 := y+1;
      end else begin
        if (y = tBufr.Width-1) then begin
          yp1 := y;
          ym1 := y-1;
        end
        else begin
          ym1 := y-1;
          yp1 := y+1;
        end;
      end;
      if (
          (CompareMem(@T[ym1],@Match,3)) OR
          (CompareMem(@T[y  ],@Match,3)) OR
          (CompareMem(@T[yp1],@Match,3)) OR
          (CompareMem(@C[ym1],@Match,3)) OR
//          (CompareMem(@C[y  ],@Match,3)) OR
          (CompareMem(@C[yp1],@Match,3)) OR
          (CompareMem(@B[ym1],@Match,3)) OR
          (CompareMem(@B[y  ],@Match,3)) OR
          (CompareMem(@B[yp1],@Match,3))
{          ((T[ym1].rgbtRed = Match.rgbtRed) AND (T[ym1].rgbtGreen = Match.rgbtGreen) AND (T[ym1].rgbtBlue = Match.rgbtBlue)) OR
          ((T[y  ].rgbtRed = Match.rgbtRed) AND (T[y  ].rgbtGreen = Match.rgbtGreen) AND (T[y  ].rgbtBlue = Match.rgbtBlue)) OR
          ((T[yp1].rgbtRed = Match.rgbtRed) AND (T[yp1].rgbtGreen = Match.rgbtGreen) AND (T[yp1].rgbtBlue = Match.rgbtBlue)) OR
          ((C[ym1].rgbtRed = Match.rgbtRed) AND (C[ym1].rgbtGreen = Match.rgbtGreen) AND (C[ym1].rgbtBlue = Match.rgbtBlue)) OR
//          ((C[y  ].rgbtRed = Match.rgbtRed) AND (C[y  ].rgbtGreen = Match.rgbtGreen) AND (C[y  ].rgbtBlue = Match.rgbtBlue)) OR
          ((C[yp1].rgbtRed = Match.rgbtRed) AND (C[yp1].rgbtGreen = Match.rgbtGreen) AND (C[yp1].rgbtBlue = Match.rgbtBlue)) OR
          ((B[ym1].rgbtRed = Match.rgbtRed) AND (B[ym1].rgbtGreen = Match.rgbtGreen) AND (B[ym1].rgbtBlue = Match.rgbtBlue)) OR
          ((B[y  ].rgbtRed = Match.rgbtRed) AND (B[y  ].rgbtGreen = Match.rgbtGreen) AND (B[y  ].rgbtBlue = Match.rgbtBlue)) OR
          ((B[yp1].rgbtRed = Match.rgbtRed) AND (B[yp1].rgbtGreen = Match.rgbtGreen) AND (B[yp1].rgbtBlue = Match.rgbtBlue))
}          ) then begin
            O[y] := Match;
          end;
    end;
    ProgressBar_Status.StepIt;
    Application.ProcessMessages;
  end;
  ProgressBar_Status.Position := 0;
  tBufr.Free;
  Screen.Cursor := crDefault;  // no longer busy
end;

//----------------------------------------------------------------------------
procedure Convolve_Shrink(aBmp : TBitmap; Match : TRGBTriple);
var
  O, T, C, B : pRGBArray;  // Scanlines
  x, y : integer;
  xp1, yp1 : integer;
  xm1, ym1 : integer;
  tBufr : TBitmap; // temp bitmap

begin
  Screen.Cursor := crHourGlass;  // Let user know we're busy...
  tBufr := TBitmap.Create;
  BMP_CopyMe(tBufr,aBmp);
  ProgressBar_Status.Max := aBmp.Height;

  for x := 0 to aBmp.Height - 1 do begin  // Walk scanlines
    if (x = 0) then begin
      xm1 := x;
      xp1 := x+1;
    end else begin
      if (x = aBmp.Height-1) then begin
        xp1 := x;
        xm1 := x-1;
      end
      else begin
        xm1 := x-1;
        xp1 := x+1;
      end;
    end;
    O := aBmp.ScanLine[x];      // New Target (Original)
    T := tBufr.ScanLine[xm1];     //old x-1  (Top)
    C := tBufr.ScanLine[x];   //old x    (Center)
    B := tBufr.ScanLine[xp1];   //old x+1  (Bottom)
  // Now do the main piece
    for y := 0 to (tBufr.Width - 1) do begin  // Walk pixels
      if (y = 0) then begin
        ym1 := y;
        yp1 := y+1;
      end else begin
        if (y = tBufr.Width-1) then begin
          yp1 := y;
          ym1 := y-1;
        end
        else begin
          ym1 := y-1;
          yp1 := y+1;
        end;
      end;
      if (
          (CompareMem(@C[y  ],@Match,3)) AND NOT(
          (CompareMem(@T[ym1],@Match,3)) AND
          (CompareMem(@T[y  ],@Match,3)) AND
          (CompareMem(@T[yp1],@Match,3)) AND
          (CompareMem(@C[ym1],@Match,3)) AND
          (CompareMem(@C[yp1],@Match,3)) AND
          (CompareMem(@B[ym1],@Match,3)) AND
          (CompareMem(@B[y  ],@Match,3)) AND
          (CompareMem(@B[yp1],@Match,3))
{          ((C[y  ].rgbtRed = Match.rgbtRed) AND (C[y  ].rgbtGreen = Match.rgbtGreen) AND (C[y  ].rgbtBlue = Match.rgbtBlue)) AND NOT(
          ((T[ym1].rgbtRed = Match.rgbtRed) AND (T[ym1].rgbtGreen = Match.rgbtGreen) AND (T[ym1].rgbtBlue = Match.rgbtBlue)) AND
          ((T[y  ].rgbtRed = Match.rgbtRed) AND (T[y  ].rgbtGreen = Match.rgbtGreen) AND (T[y  ].rgbtBlue = Match.rgbtBlue)) AND
          ((T[yp1].rgbtRed = Match.rgbtRed) AND (T[yp1].rgbtGreen = Match.rgbtGreen) AND (T[yp1].rgbtBlue = Match.rgbtBlue)) AND
          ((C[ym1].rgbtRed = Match.rgbtRed) AND (C[ym1].rgbtGreen = Match.rgbtGreen) AND (C[ym1].rgbtBlue = Match.rgbtBlue)) AND
          ((C[yp1].rgbtRed = Match.rgbtRed) AND (C[yp1].rgbtGreen = Match.rgbtGreen) AND (C[yp1].rgbtBlue = Match.rgbtBlue)) AND
          ((B[ym1].rgbtRed = Match.rgbtRed) AND (B[ym1].rgbtGreen = Match.rgbtGreen) AND (B[ym1].rgbtBlue = Match.rgbtBlue)) AND
          ((B[y  ].rgbtRed = Match.rgbtRed) AND (B[y  ].rgbtGreen = Match.rgbtGreen) AND (B[y  ].rgbtBlue = Match.rgbtBlue)) AND
          ((B[yp1].rgbtRed = Match.rgbtRed) AND (B[yp1].rgbtGreen = Match.rgbtGreen) AND (B[yp1].rgbtBlue = Match.rgbtBlue))
}          )) then begin
            O[y] := tNone.cRGB;
          end;
    end;
    ProgressBar_Status.StepIt;
    Application.ProcessMessages;
  end;
  ProgressBar_Status.Position := 0;
  tBufr.Free;
  Screen.Cursor := crDefault;  // no longer busy
end;

{This applies the filter settings to the image}
//----------------------------------------------------------------------------
procedure Convolve_Matrix(aBmp : TBitmap);
var
  ray : array [0..8] of integer;  // Filter settings array
  z : word;                       // Divisor
begin
  Screen.Cursor := crHourGlass;  // Let user know we're busy...
  ray[0] := StrToInt(StringGrid1.Cells[0,0]);  // Set filter values
  ray[1] := StrToInt(StringGrid1.Cells[1,0]);  //  from string grid.
  ray[2] := StrToInt(StringGrid1.Cells[2,0]);
  ray[3] := StrToInt(StringGrid1.Cells[0,1]);
  ray[4] := StrToInt(StringGrid1.Cells[1,1]);
  ray[5] := StrToInt(StringGrid1.Cells[2,1]);
  ray[6] := StrToInt(StringGrid1.Cells[0,2]);
  ray[7] := StrToInt(StringGrid1.Cells[1,2]);
  ray[8] := StrToInt(StringGrid1.Cells[2,2]);
  z := StrToInt(Edit_Divisor.Text);  //   Set divisor value from edit1
  if z = 0 then z := 1;       //   Prevent divide by zero
  Convolve_Filter(ray,z,aBmp);
  Screen.Cursor := crDefault;      // Let user know we're done
end;

{----------------------------------------------------------------------------}
Procedure BitmapShrink_File(Filename : string; Match : TRGBTriple);
var
 BitMap1,BitMap2 : TBitMap;
 MyFormat : Word;
begin
   BitMap2 := TBitMap.Create;
   BitMap1 := TBitMap.Create;
try
   BitMap1.LoadFromFile(Filename);
   BitMap2.Assign(BitMap1);     // Copy BitMap1 into BitMap2
   BitMap2.Dormant;             // Free up GDI resources
   BitMap2.FreeImage;           // Free up Memory.
   Convolve_Shrink(BitMap2, Match);
   BitMap2.SaveToFile(Filename+'s.bmp');
   BitMap2.ReleaseHandle;       // This will actually lose the bitmap;
 finally
   BitMap1.Free;
   BitMap2.Free;
 end
end;

{----------------------------------------------------------------------------}
Procedure ForestBitmapShrink_File(FileSource, FileDestination : string);
var
 BitMap1 : TBitMap;
 MyFormat : Word;
begin
   BitMap1 := TBitMap.Create;
try
   BitMap1.LoadFromFile(FileSource);
//   BitMap1.Dormant;             // Free up GDI resources
//   BitMap1.FreeImage;           // Free up Memory.
   Convolve_Shrink(BitMap1, tDeciduous.cRGB);
   Convolve_Shrink(BitMap1, tConiferous.cRGB);
   BitMap1.SaveToFile(FileDestination);
   BitMap1.ReleaseHandle;       // This will actually lose the bitmap;
 finally
   BitMap1.Free;
 end
end;

//----------------------------------------------------------------------------
end.

{--- End of File ------------------------------------------------------------}

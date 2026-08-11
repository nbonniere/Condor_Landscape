{
 * u_Perspective.pas
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
unit u_Perspective;

// Google Gemini
// two-step stratification, first affine, then metric, for perspective correction
// with 4 points with cropping in delphi-4 with example
// with scanline instead of pixel

//===========================================================================
INTERFACE

uses
  Windows, Graphics, Classes, Math, Dialogs, SysUtils;

type
  TVector3D = record
    X, Y, Z: Double;
  end;

  TPoint2D = record
    X, Y: Double;
  end;

  TMatrix3x3 = array[0..2, 0..2] of Double;

  PRGBTriple = ^TRGBTriple;
  TRGBTriple = packed record
    rgbtBlue: Byte;
    rgbtGreen: Byte;
    rgbtRed: Byte;
  end;

  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..32767] of TRGBTriple;

var
  H, InvH: TMatrix3x3;

//---------------------------------------------------------------------------
procedure PrepareDLTHomography(
  const bWidth, bHeight : integer;
  const P1, P2, P3, P4: TPoint2D;
  var H, Hinv : TMatrix3x3);

procedure RectifyBitmapDLT(
  SrcBitmap, DstBitmap: TBitmap;
  const H, InvH: TMatrix3x3);

procedure RectifyBitmapDLTBilinear(
  SrcBitmap, DstBitmap: TBitmap;
  const H, InvH: TMatrix3x3);

function TransformPoint(
  const M: TMatrix3x3;
  const Pt: TPoint2D): TPoint2D;

//===========================================================================
IMPLEMENTATION

// Solves an 8x8 system of linear equations using Gaussian Elimination
//---------------------------------------------------------------------------
function Solve8x8(A: array of Double; B: array of Double; var X: array of Double): Boolean;
var
  i, j, k, maxRow: Integer;
  temp, factor: Double;
  Mat: array[0..7, 0..8] of Double;
begin
  Result := False;
  for i := 0 to 7 do begin
    for j := 0 to 7 do begin
      Mat[i, j] := A[i * 8 + j];
    end;
    Mat[i, 8] := B[i];
  end;

  for i := 0 to 7 do begin
    maxRow := i;
    for k := i + 1 to 7 do begin
      if (Abs(Mat[k, i]) > Abs(Mat[maxRow, i])) then begin
        maxRow := k;
      end;
    end;
    for k := 0 to 8 do begin
      temp := Mat[i, k];
      Mat[i, k] := Mat[maxRow, k];
      Mat[maxRow, k] := temp;
    end;

    if (Abs(Mat[i, i]) < 1e-11) then begin
      Exit;
    end;
    for k := i + 1 to 7 do begin
      factor := Mat[k, i] / Mat[i, i];
      for j := i to 8 do begin
        Mat[k, j] := Mat[k, j] - factor * Mat[i, j];
      end;
    end;
  end;

  for i := 7 downto 0 do begin
    temp := Mat[i, 8];
    for j := i + 1 to 7 do begin
      temp := temp - Mat[i, j] * X[j];
    end;
    X[i] := temp / Mat[i, i];
  end;

  Result := True;
end;

// Direct 4-Point Homography computation using DLT
//---------------------------------------------------------------------------
function ComputeDLTHomography(
  const SrcP1, SrcP2, SrcP3, SrcP4: TPoint2D;
  DstWidth, DstHeight: Double;
  var H: TMatrix3x3): Boolean;
var
  A: array[0..63] of Double;
  B: array[0..7] of Double;
  X: array[0..7] of Double;
  Src: array[0..3] of TPoint2D;
  Dst: array[0..3] of TPoint2D;
  i: Integer;
begin
  Src[0] := SrcP1; Src[1] := SrcP2; Src[2] := SrcP3; Src[3] := SrcP4;

  Dst[0].X := 0;         Dst[0].Y := 0;
  Dst[1].X := DstWidth;  Dst[1].Y := 0;
  Dst[2].X := DstWidth;  Dst[2].Y := DstHeight;
  Dst[3].X := 0;         Dst[3].Y := DstHeight;

  FillChar(A, SizeOf(A), 0);

  for i := 0 to 3 do begin
    // Equation 1: x_dst
    A[(i*2)*8 + 0] := Src[i].X;
    A[(i*2)*8 + 1] := Src[i].Y;
    A[(i*2)*8 + 2] := 1.0;
    A[(i*2)*8 + 6] := -Dst[i].X * Src[i].X;
    A[(i*2)*8 + 7] := -Dst[i].X * Src[i].Y;
    B[i*2]         := Dst[i].X;

    // Equation 2: y_dst
    A[(i*2+1)*8 + 3] := Src[i].X;
    A[(i*2+1)*8 + 4] := Src[i].Y;
    A[(i*2+1)*8 + 5] := 1.0;
    A[(i*2+1)*8 + 6] := -Dst[i].Y * Src[i].X;
    A[(i*2+1)*8 + 7] := -Dst[i].Y * Src[i].Y;
    B[i*2+1]       := Dst[i].Y;
  end;

  if ( not Solve8x8(A, B, X) ) then begin
    Result := False;
    Exit;
  end;

  H[0,0] := X[0]; H[0,1] := X[1]; H[0,2] := X[2];
  H[1,0] := X[3]; H[1,1] := X[4]; H[1,2] := X[5];
  H[2,0] := X[6]; H[2,1] := X[7]; H[2,2] := 1.0;

  Result := True;
end;

//---------------------------------------------------------------------------
function Invert3x3(const M: TMatrix3x3; var Inv: TMatrix3x3): Boolean;
var
  Det: Double;
begin
  Det := M[0,0]*(M[1,1]*M[2,2] - M[1,2]*M[2,1]) -
         M[0,1]*(M[1,0]*M[2,2] - M[1,2]*M[2,0]) +
         M[0,2]*(M[1,0]*M[2,1] - M[1,1]*M[2,0]);

  if (Abs(Det) < 1e-11) then begin
    Result := False;
    Exit;
  end;

  Inv[0,0] :=  (M[1,1]*M[2,2] - M[1,2]*M[2,1]) / Det;
  Inv[0,1] := -(M[0,1]*M[2,2] - M[0,2]*M[2,1]) / Det;
  Inv[0,2] :=  (M[0,1]*M[1,2] - M[0,2]*M[1,1]) / Det;

  Inv[1,0] := -(M[1,0]*M[2,2] - M[1,2]*M[2,0]) / Det;
  Inv[1,1] :=  (M[0,0]*M[2,2] - M[0,2]*M[2,0]) / Det;
  Inv[1,2] := -(M[0,0]*M[1,2] - M[0,2]*M[1,0]) / Det;

  Inv[2,0] :=  (M[1,0]*M[2,1] - M[1,1]*M[2,0]) / Det;
  Inv[2,1] := -(M[0,0]*M[2,1] - M[0,1]*M[2,0]) / Det;
  Inv[2,2] :=  (M[0,0]*M[1,1] - M[0,1]*M[1,0]) / Det;

  Result := True;
end;

// initially for debugging
//---------------------------------------------------------------------------
function HomogeneousToPoint(const V: TVector3D): TPoint2D;
begin
  if (Abs(V.Z) > 1e-9) then begin
    Result.X := V.X / V.Z;
    Result.Y := V.Y / V.Z;
  end else begin
    Result.X := V.X;
    Result.Y := V.Y;
  end;
end;

// initially for debugging
//---------------------------------------------------------------------------
function PointToHomogeneous(const Pt: TPoint2D): TVector3D;
begin
  Result.X := Pt.X;
  Result.Y := Pt.Y;
  Result.Z := 1.0;
end;

// initially for debugging
//---------------------------------------------------------------------------
function TransformPoint(const M: TMatrix3x3; const Pt: TPoint2D): TPoint2D;
var
  HIn, HOut: TVector3D;
begin
  HIn := PointToHomogeneous(Pt);
  HOut.X := M[0,0]*HIn.X + M[0,1]*HIn.Y + M[0,2]*HIn.Z;
  HOut.Y := M[1,0]*HIn.X + M[1,1]*HIn.Y + M[1,2]*HIn.Z;
  HOut.Z := M[2,0]*HIn.X + M[2,1]*HIn.Y + M[2,2]*HIn.Z;
  Result := HomogeneousToPoint(HOut);
end;

//---------------------------------------------------------------------------
procedure DebugCheckCorners(const InvH: TMatrix3x3; DstW, DstH: Integer);
var
  CornerTL, CornerTR, CornerBR, CornerBL: TPoint2D;
begin
  // Transform destination corners back to source space
  CornerTL.x := 0; CornerTL.y := 0;
  CornerTL := TransformPoint(InvH, CornerTL{Point2D(0, 0)});
  CornerTR.x := DstW; CornerTR.y := 0;
  CornerTR := TransformPoint(InvH, CornerTR{Point2D(DstW, 0)});
  CornerBR.x := DstW; CornerBR.y := DstH;
  CornerBR := TransformPoint(InvH, CornerBR{Point2D(DstW, DstH)});
  CornerBL.x := 0; CornerBL.y := DstH;
  CornerBL := TransformPoint(InvH, CornerBL{Point2D(0, DstH)});

  ShowMessage(
    Format('Destination (0,0) maps back to Source: (%.1f, %.1f)'#13#10 +
           'Destination (%d,0) maps back to Source: (%.1f, %.1f)'#13#10 +
           'Destination (%d,%d) maps back to Source: (%.1f, %.1f)'#13#10 +
           'Destination (0,%d) maps back to Source: (%.1f, %.1f)',
           [CornerTL.X, CornerTL.Y,
            DstW, CornerTR.X, CornerTR.Y,
            DstW, DstH, CornerBR.X, CornerBR.Y,
            DstH, CornerBL.X, CornerBL.Y]));
end;

//---------------------------------------------------------------------------
procedure DebugCheckCornersForward(const H: TMatrix3x3; P1, P2, P3, P4 : TPoint2D);
var
  CornerTL, CornerTR, CornerBR, CornerBL: TPoint2D;
begin
  // Transform destination corners back to source space
  CornerTL.x := P1.x; CornerTL.y := P1.y;
  CornerTL := TransformPoint(H, CornerTL{P1});
  CornerTR.x := P2.x; CornerTR.y := P2.y;
  CornerTR := TransformPoint(H, CornerTR{P2});
  CornerBR.x := P3.x; CornerBR.y := P3.y;
  CornerBR := TransformPoint(H, CornerBR{P3});
  CornerBL.x := P4.x; CornerBL.y := P4.y;
  CornerBL := TransformPoint(H, CornerBL{P4});

  ShowMessage(
    Format('Destination (%.1f, %.1f) maps forward to Destination: (%.1f, %.1f)'#13#10 +
           'Destination (%.1f, %.1f) maps forward to Destination: (%.1f, %.1f)'#13#10 +
           'Destination (%.1f, %.1f) maps forward to Destination: (%.1f, %.1f)'#13#10 +
           'Destination (%.1f, %.1f) maps forward to Destination: (%.1f, %.1f)',
           [P1.x, P1.y, CornerTL.X, CornerTL.Y,
            P2.x, P2.y, CornerTR.X, CornerTR.Y,
            P3.x, P3.y, CornerBR.X, CornerBR.Y,
            P4.x, P4.y, CornerBL.X, CornerBL.Y]));
end;

//---------------------------------------------------------------------------
procedure PrepareDLTHomography(
  const bWidth, bHeight : integer;
  const P1, P2, P3, P4: TPoint2D;
  var H, Hinv : TMatrix3x3);
begin
  // Compute forward Homography mapping Source -> Destination
  if (not ComputeDLTHomography(P1, P2, P3, P4, bWidth, bHeight, H)) then begin
    Exit;
  end;

  // We invert H because backward-mapping (Destination -> Source) prevents gaps/holes
  if (not Invert3x3(H, InvH)) then begin
    Exit;
  end;
end;

// nearest pixel, not bilinear
//---------------------------------------------------------------------------
procedure RectifyBitmapDLT(
  SrcBitmap, DstBitmap: TBitmap;
  const H, InvH: TMatrix3x3);
var
  x, y: Integer;
  DstRow: PRGBArray;
  SrcRowPointers: array of PRGBArray;
  SrcWidth, SrcHeight: Integer;
  hx, hy, hz: Double;
  SrcX, SrcY: Integer;
begin
  SrcBitmap.PixelFormat := pf24bit;
  DstBitmap.PixelFormat := pf24bit;

  SrcWidth  := SrcBitmap.Width;
  SrcHeight := SrcBitmap.Height;

  SetLength(SrcRowPointers, SrcHeight);
  for y := 0 to SrcHeight - 1 do
    SrcRowPointers[y] := SrcBitmap.ScanLine[y];

  for y := 0 to DstBitmap.Height - 1 do begin
    DstRow := DstBitmap.ScanLine[y];

    for x := 0 to DstBitmap.Width - 1 do begin
      // Backward projection using InvH
      hx := InvH[0,0] * x + InvH[0,1] * y + InvH[0,2];
      hy := InvH[1,0] * x + InvH[1,1] * y + InvH[1,2];
      hz := InvH[2,0] * x + InvH[2,1] * y + InvH[2,2];

      if (Abs(hz) > 1e-9) then begin
        SrcX := Round(hx / hz);
        SrcY := Round(hy / hz);
      end else begin
        SrcX := -1;
        SrcY := -1;
      end;

      if ( (SrcX >= 0) and (SrcX < SrcWidth) and
           (SrcY >= 0) and (SrcY < SrcHeight) ) then begin
        DstRow[x] := SrcRowPointers[SrcY][SrcX];
      end else begin
        DstRow[x].rgbtRed   := 0;
        DstRow[x].rgbtGreen := 0;
        DstRow[x].rgbtBlue  := 0;
      end;
    end;
  end;

  SetLength(SrcRowPointers, 0);

//  DebugCheckCorners(InvH, DstBitmap.Width, DstBitmap.Height);
//  DebugCheckCornersForward(H, P1, P2, P3, P4);

end;

// bilinear, not nearest pixel
//---------------------------------------------------------------------------
procedure RectifyBitmapDLTBilinear(
  SrcBitmap, DstBitmap: TBitmap;
  const H, InvH: TMatrix3x3);
var
  x, y: Integer;
  DstRow: PRGBArray;
  SrcRowPointers: array of PRGBArray;
  SrcWidth, SrcHeight: Integer;

  // Projection values
  hx, hy, hz: Double;
  u, v: Double;
  x1, y1, x2, y2: Integer;
  dx, dy: Double;
  w00, w10, w01, w11: Integer; // Fixed-point weights (scaled by 256)

  // Neighboring pixels
  p00, p10, p01, p11: TRGBTriple;
begin
  SrcBitmap.PixelFormat := pf24bit;
  DstBitmap.PixelFormat := pf24bit;

  SrcWidth  := SrcBitmap.Width;
  SrcHeight := SrcBitmap.Height;

  // Cache source scanline pointers
  SetLength(SrcRowPointers, SrcHeight);
  for y := 0 to SrcHeight - 1 do begin
    SrcRowPointers[y] := SrcBitmap.ScanLine[y];
  end;
  for y := 0 to DstBitmap.Height - 1 do begin
    DstRow := DstBitmap.ScanLine[y];

    for x := 0 to DstBitmap.Width - 1 do begin
      // Backward projection
      hx := InvH[0,0] * x + InvH[0,1] * y + InvH[0,2];
      hy := InvH[1,0] * x + InvH[1,1] * y + InvH[1,2];
      hz := InvH[2,0] * x + InvH[2,1] * y + InvH[2,2];

      if Abs(hz) > 1e-9 then begin
        u := hx / hz;
        v := hy / hz;
      end else begin
        u := -1.0;
        v := -1.0;
      end;

      // Check bounds (leave 1px margin on right/bottom for 2x2 neighborhood)
      if ( (u >= 0.0) and (u < SrcWidth - 1) and
           (v >= 0.0) and (v < SrcHeight - 1) ) then begin
        x1 := Trunc(u);
        y1 := Trunc(v);
        x2 := x1 + 1;
        y2 := y1 + 1;

        dx := u - x1;
        dy := v - y1;

        // Convert floating weights to 8-bit fixed-point integers (Sum = 256)
        w00 := Round((1.0 - dx) * (1.0 - dy) * 256.0);
        w10 := Round(dx * (1.0 - dy) * 256.0);
        w01 := Round((1.0 - dx) * dy * 256.0);
        w11 := 256 - (w00 + w10 + w01); // Ensure exact sum of 256

        // Fetch 2x2 pixel neighborhood directly from memory
        p00 := SrcRowPointers[y1][x1];
        p10 := SrcRowPointers[y1][x2];
        p01 := SrcRowPointers[y2][x1];
        p11 := SrcRowPointers[y2][x2];

        // Blend Red, Green, and Blue channels via bit-shift division (shr 8)
        DstRow[x].rgbtRed   := (p00.rgbtRed   * w00 + p10.rgbtRed   * w10 +
                                p01.rgbtRed   * w01 + p11.rgbtRed   * w11) shr 8;

        DstRow[x].rgbtGreen := (p00.rgbtGreen * w00 + p10.rgbtGreen * w10 +
                                p01.rgbtGreen * w01 + p11.rgbtGreen * w11) shr 8;

        DstRow[x].rgbtBlue  := (p00.rgbtBlue  * w00 + p10.rgbtBlue  * w10 +
                                p01.rgbtBlue  * w01 + p11.rgbtBlue  * w11) shr 8;
      end else begin
        // Out-of-bounds background color (Black)
        DstRow[x].rgbtRed   := 0;
        DstRow[x].rgbtGreen := 0;
        DstRow[x].rgbtBlue  := 0;
      end;
    end;
  end;

  SetLength(SrcRowPointers, 0);

//  DebugCheckCorners(InvH, DstBitmap.Width, DstBitmap.Height);
//  DebugCheckCornersForward(H, P1, P2, P3, P4);

end;

//---------------------------------------------------------------------------
{var
  Src, Dest: TBitmap;
  P1, P2, P3, P4 : TPoint2D;
begin
  Src := TBitmap.Create;
  Dest := TBitmap.Create;

  Src.LoadFromFile('skewed_document.bmp');

  // Set up 4 input perspective coordinates

//  P1.X :=  44; P1.Y := 249; // TL
//  P2.X := 767; P2.Y := 272; // TR
//  P3.X := 770; P3.Y := 356; // BR
//  P4.X :=  38; P4.Y := 343; // BL

  P1.X := 132; P1.Y := 259; // TL
  P2.X := 867; P2.Y := 144; // TR
  P3.X := 896; P3.Y := 522; // BR
  P4.X := 133; P4.Y := 516; // BL

  // Ready the output canvas size
  Dest.Width :=  1024;
  Dest.Height := 512;

////  RectifyBitmapDLT( Src, Dest, P1, P2, P3, P4);
//  PrepareDLTHomography( Dest.Width, Dest.Height,
//    P1, P2, P3, P4,
//    H, invH);
//  RectifyBitmapDLT(Src, Dest, H, InvH);

//  RectifyBitmapDLTbilinear( Src, Dest, P1, P2, P3, P4);
  PrepareDLTHomography( Dest.Width, Dest.Height,
    P1, P2, P3, P4,
    H, invH);
  RectifyBitmapDLTbilinear(Src, Dest, H, InvH);

  Dest.SaveToFile('corrected.bmp');

  Src.Free;
  Dest.Free;
}
//---------------------------------------------------------------------------
end.

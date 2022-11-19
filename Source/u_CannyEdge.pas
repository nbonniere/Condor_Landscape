{
 * u_CannyEdge.pas
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
UNIT u_CannyEdge;

{============================================================================}
INTERFACE

uses Graphics, StdCtrls, comctrls;

var
  Memo_Message : TMemo;  // external TMemo for messages
  ProgressBar_Status : TProgressBar;

  Prefilter : boolean;
  sigma, thrlow, thrhigh   : real;

function canny( b: TBitmap ): TBitmap;
function gaussian( b: TBitmap; sigma: real ): TBitmap;
function CalcKsize( sigma: real ): integer;

{============================================================================}
IMPLEMENTATION

uses {Windows,} math,
  u_BMP, u_Util;

type
  TWarnaRGB = packed record
    b, g, r : byte;
  end;
  TArrWarnaRGB = array[0..65535] of TWarnaRGB;
  PArrRGB = ^TArrWarnaRGB;

const
  rgb_hitam : TWarnaRGB = (b:0;g:0;r:0);

//----------------------------------------------------------------------------
function BMP_Clone( frbmp: TBitmap ): TBitmap;
begin
  result := TBitmap.Create;
  result.Width := frbmp.Width;
  result.Height := frbmp.Height;
  result.PixelFormat := frbmp.PixelFormat;
//  result.Canvas.Draw(0,0,frbmp);
end;

{----------------------------------------------------------------------------
  http://pebbie.wordpress.com/2009/12/25/implementasi-gaussian-blur/
----------------------------------------------------------------------------}

//----------------------------------------------------------------------------
function CalcKsize( sigma: real ): integer;
var
  norm : real;
  ediv : real;

begin
  norm := 1 / ( sigma * sqrt( 2 * pi ) );
  ediv := 0.5 / ( sigma * sigma );
  result := 1 + 2 * round( 3 * sigma );
  if result < 3 then result := 3;
  if ( ( result and 1 ) <> 1 ) then result := result + 1;
end;

//----------------------------------------------------------------------------
function gaussian( b: TBitmap; sigma: real ): TBitmap;
var
  mid, ksize        : integer;
  ker               : array of real;
  p                 : array of PArrRGB;
  b2                : TBitmap;

//----------------------------------------------------------------------------
  procedure gauss( sigma: real );
  var
    i               : integer;
    sum, w          : real;
    norm            : real;
    ediv            : real;

  begin
    norm := 1 / ( sigma * sqrt( 2 * pi ) );
    ediv := 0.5 / ( sigma * sigma );
    ksize := 1 + 2 * round( 3 * sigma );
    if ksize < 3 then ksize := 3;
    if ( ( ksize and 1 ) <> 1 ) then ksize := ksize + 1;

    setlength( ker, ksize );
    mid := ksize div 2;
    sum := 0;
    for i := 0 to ksize - 1 do begin
      w := norm * exp( -( i - mid ) * ( i - mid ) * ediv );
      ker[i] := w;
      sum := sum + w;
    end;
    for i := 0 to high( ker ) do
      ker[i] := ker[i] / sum;
  end;

//----------------------------------------------------------------------------
  function Hconv( b: TBitmap ): TBitmap;
  var
    j, i, k         : integer;
    corr            : real;
    sum             : real;
    pp              : ParrRGB;

  begin
    result := BMP_Clone( b );
    for j := 0 to high( p ) do
      p[j] := b.ScanLine[j];
    for j := 0 to b.Height - 1 do begin
      pp := result.ScanLine[j];
      for i := 0 to b.Width - 1 do begin
        sum := 0;
        corr := 0;
        for k := -mid to mid do begin
          if ( i + k < 0 ) or ( i + k >= b.Width ) then begin
            corr := corr + ker[mid + k];
            continue;
          end;
          sum := sum + ker[mid + k] * (0.299*p[j][i + k].r + 0.587*p[j][i + k].g + 0.114*p[j][i + k].b);
        end;

        if corr > 0 then sum := sum * ( 1 / ( 1 - corr ) );
//        pp[i] := warna_create( clamp( round( sum ) ), clamp( round( sum ) ), clamp( round( sum ) ) );
          pp[i].b := ClampByte( round( sum ));
          pp[i].g := pp[i].b;
          pp[i].r := pp[i].b;
      end;
    end;
  end;

//----------------------------------------------------------------------------
  function Vconv( b: TBitmap ): TBitmap;
  var
    j, i, k         : integer;
    corr            : real;
    sum             : real;
    pp              : ParrRGB;

  begin
    result := BMP_Clone( b );
    for j := 0 to high( p ) do
      p[j] := b.ScanLine[j];
    for j := 0 to b.Width - 1 do begin
      for i := 0 to b.Height - 1 do begin
        pp := result.ScanLine[i];
        sum := 0;
        corr := 0;
        for k := -mid to mid do begin
          if ( i + k < 0 ) or ( i + k >= b.Height ) then begin
            corr := corr + ker[mid + k];
            continue;
          end;
          sum := sum + ker[mid + k] * (0.299*p[i + k][j].r  + 0.587*p[i + k][j].g + 0.114*p[i + k][j].b);
        end;
        if corr > 0 then sum := sum * ( 1 / ( 1 - corr ) );
//        pp[j] := warna_create( clamp( round( sum ) ), clamp( round( sum ) ), clamp( round( sum ) ) );
          pp[j].b := ClampByte( round( sum ));
          pp[j].g := pp[j].b;
          pp[j].r := pp[j].b;
      end;
    end;
  end;

//----------------------------------------------------------------------------
begin
  setlength( p, b.Height );
  gauss( sigma );
  b2 := hconv( b );
  result := vconv( b2 );
  b2.Free;
end;

//----------------------------------------------------------------------------
function canny( b: TBitmap ): TBitmap;
var
  kv, kh            : array[0..2, 0..2] of real;
  i, j, k, l        : integer;
  p                 : array of PArrRGB;
  mag, gx, gy       : array of array of real;
  bg, br            : TBitmap;
  dir               : real;
  tmp, dx, dy       : integer;
  pr                : ParrRGB;
//  ap                : array of TPoint;
  edge              : array of array of boolean;
//  pt                : array of Tpoint;
//  thrlow, thrhigh   : real;

//----------------------------------------------------------------------------
  function trace( x, y: integer ): boolean;
  var
    i, j            : integer;

//----------------------------------------------------------------------------
    function ok( x, y: integer ): boolean;
    begin
      result := ( x >= 0 ) and ( y >= 0 ) and ( x < b.Width ) and ( y < b.Height );
    end;

//----------------------------------------------------------------------------
  begin
    result := false;
    if not edge[y][x] then exit;

    edge[y][x] := false;
    result := mag[y][x] >= thrlow;
    for j := -1 to 1 do
      for i := -1 to 1 do
        if ok( x + i, y + j ) then
          result := trace( x + i, y + j ) or result;

    if result then begin
      pr := br.ScanLine[y];
      pr[x] := rgb_hitam;
    end;
  end;

begin
  if (PreFilter) then begin
    bg := gaussian( b, sigma );
  end else begin
    bg := b;
  end;
//  result := citra_create( b.Width, b.Height );
  result := TBitmap.Create;
  result.Width := b.Width;
  result.Height := b.Height;
  result.PixelFormat := pf24bit;

  setlength( p, b.Height );
  for j := 0 to high( p ) do
    p[j] := bg.ScanLine[j];

  setlength( gx, b.Height );
  for j := 0 to high( gx ) do
    setlength( gx[j], b.Width );

  setlength( gy, b.Height );
  for j := 0 to high( gx ) do
    setlength( gy[j], b.Width );

  setlength( mag, b.Height );
  for j := 0 to high( mag ) do
    setlength( mag[j], b.Width );

  setlength( edge, b.Height );
  for j := 0 to high( edge ) do
    setlength( edge[j], b.Width );

  //Sobel operator
  for j := 1 to b.Height - 2 do begin
    for i := 1 to b.Width - 2 do begin
      gy[j][i] := ( p[j + 1][i - 1].r - p[j - 1][i - 1].r )
        + 2 * ( p[j + 1][i].r - p[j - 1][i].r )
        + ( p[j + 1][i + 1].r - p[j - 1][i + 1].r );
      gx[j][i] := ( p[j - 1][i + 1].r - p[j - 1][i - 1].r )
        + 2 * ( p[j][i + 1].r - p[j][i - 1].r )
        + ( p[j + 1][i + 1].r - p[j + 1][i - 1].r );
      mag[j][i] := sqrt( gx[j][i] * gx[j][i] + gy[j][i] * gy[j][i] );
    end;
  end;

  //non-maximal suppression
  for j := 1 to b.Height - 2 do begin
    for i := 1 to b.Width - 2 do begin
      dir := arctan2( gy[j][i], gx[j][i] );
      dx := round( cos( dir ) );
      dy := round( sin( dir ) );

      edge[j][i] := ( mag[j][i] >= mag[j + dy][i + dx] ) and ( mag[j][i] >= mag[j - dy][i - dx] );
      if edge[j][i] then begin
        edge[j + dy][i + dx] := false;
        edge[j - dy][i - dx] := false;
      end;
    end;
  end;

  //apply hysteresis
  thrlow := 10;
  thrhigh := 90;
  br := result;
  for j := 1 to b.Height - 2 do begin
    for i := 1 to b.Width - 2 do begin
// ???      if edge[j][i] and (mag[y][x] >= thrhigh) then trace( i, j );//trace edge-linking
      if edge[j][i] and (mag[j][i] >= thrhigh) then trace( i, j );//trace edge-linking
    end;
  end;
end;

//----------------------------------------------------------------------------
end.

{--- End of File ------------------------------------------------------------}


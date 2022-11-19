{
 * u_Tile_XYZ.pas
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
UNIT u_Tile_XYZ;

{----------------------------------------------------------------------------
// (epsg:4326) coordinates to wgs84/pseudo mercator (epsg:3857)
// smRadius = 6378136.98
// smRange = smRadius * pi * 2.0
// smLonToX = smRange / 360.0
// smRadiansOverDegrees = pi / 180.0

// X = longitude *smLonToX

// Y = latitude

// if (Y > 86.0) begin
//   Y = smRange
// end else if (Y < -86.0) begin
//   Y = -smRange
// end else begin
//   Y = Y * smRadiansOverDegrees
//   Y = ln(tan(Y) + (1.0 / cos(Y)))
//   Y = Y * smRadius
// end
----------------------------------------------------------------------------}

{----------------------------------------------------------------------------
// tiles XYZ - 0,0 is at top left
// https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Zoom_levels
// for TMS - 0,0 is at bottom left
// to convert - y' = (2^zoom)-1 - Y
// zoom level 0 - 1 tile for whole world
// zoom level 1 - 2^1 x 2^1 tiles  (long -180..+180, lat -85.0511..+85.0511)
// zoom level n	- 2^n × 2^n tiles, 2^2*n tiles,	width 360/2n° x height[variable]
----------------------------------------------------------------------------}

{============================================================================}
INTERFACE

var
  zoom : integer;
  xtile, ytile : integer;
  lat_deg, lon_deg : double;
  TMS : boolean;

Procedure LatLong_To_Tile;
Procedure Tile_To_LatLong;

{============================================================================}
IMPLEMENTATION

uses
  Math;

{----------------------------------------------------------------------------}
Procedure LatLong_To_Tile;
var
  lat_rad, n: Real;
begin
  lat_rad := DegToRad(lat_deg);
  n := Power(2, zoom);
  xtile := Trunc(((lon_deg + 180) / 360) * n);
  ytile := Trunc((1 - (ln(Tan(lat_rad) + (1 /Cos(lat_rad))) / Pi)) / 2 * n);
  if (TMS) then begin
    ytile := trunc(n-1) - ytile;
  end;
end;

// tile lat/long top left corner
{----------------------------------------------------------------------------}
Procedure Tile_To_LatLong;
var
  lat_rad, n: Real;
begin
  n := Power(2, zoom);
  if (TMS) then begin
    lat_rad := Arctan (Sinh (Pi * (1 - 2 * (trunc(n-1) - ytile) / n)));
  end else begin
    lat_rad := Arctan (Sinh (Pi * (1 - 2 * ytile / n)));
  end;  
  lat_deg := RadtoDeg (lat_rad);
  lon_deg := xtile / n * 360.0 - 180.0;
end;

{----------------------------------------------------------------------------}
begin { Initialization }
//  Memo_Message := nil;
{
  zoom := 10;
  TMS := true;

  lat_deg :=   42.5;
  lon_deg := -120.5;
  LatLong_To_Tile;

  xtile := 169;
//  ytile := 379;  // XYZ
  ytile := 645;  // TMS
  Tile_To_LatLong;
}
end.

{--- End of File ------------------------------------------------------------}


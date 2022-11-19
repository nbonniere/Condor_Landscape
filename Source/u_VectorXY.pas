{
 * u_VectorXY.pas
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
UNIT u_VectorXY;

{----------------------------------------------------------------------------
----------------------------------------------------------------------------}

//===========================================================================
INTERFACE

type
  CoordXY = record
    X, Y : single;
  end;

  CoordXY_Array = array of CoordXY;

//var

procedure Offset(var PointXY : CoordXY; OffsetXY : CoordXY);
procedure Offset_Array(var PointXY : CoordXY_Array; OffsetXY : CoordXY);
procedure Rotate(var PointXY : CoordXY; Angle : double);
procedure Rotate_Array(var PointXY : CoordXY_Array; Angle : double);

//===========================================================================
IMPLEMENTATION

//uses

//---------------------------------------------------------------------------
procedure Offset(var PointXY : CoordXY; OffsetXY : CoordXY);
begin
  PointXY.X := PointXY.X + OffsetXY.X;
  PointXY.Y := PointXY.Y + OffsetXY.Y;
end;

//---------------------------------------------------------------------------
procedure Offset_Array(var PointXY : CoordXY_Array; OffsetXY : CoordXY);
var
  i : integer;
begin
  for i := 0 to length(PointXY)-1 do begin
    PointXY[i].X := PointXY[i].X + OffsetXY.X;
    PointXY[i].Y := PointXY[i].Y + OffsetXY.Y;
  end;
end;

//---------------------------------------------------------------------------
procedure Rotate(var PointXY : CoordXY; Angle : double);
var
  Temp : double;
  cCOS, cSIN : double;
begin
  cSIN := sin(Angle/180*PI);
  cCOS := cos(Angle/180*PI);
  Temp := PointXY.X * cCOS + PointXY.Y * -cSIN;
  PointXY.Y := PointXY.X * cSIN + PointXY.Y * cCOS;
  PointXY.X := Temp;
end;

//---------------------------------------------------------------------------
procedure Rotate_Array(var PointXY : CoordXY_Array; Angle : double);
var
  i : integer;
  Temp : double;
  cCOS, cSIN : double;
begin
  cSIN := sin(Angle/180*PI);
  cCOS := cos(Angle/180*PI);
  for i := 0 to length(PointXY)-1 do begin
    Temp := PointXY[i].X * cCOS + PointXY[i].Y * -cSIN;
    PointXY[i].Y := PointXY[i].X * cSIN + PointXY[i].Y * cCOS;
    PointXY[i].X := Temp;
  end;
end;

//---------------------------------------------------------------------------
begin { Initialization }
end.

//-- End of File ------------------------------------------------------------


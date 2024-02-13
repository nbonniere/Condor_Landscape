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

//===========================================================================
INTERFACE

type
  TCoordXY = record
    X, Y : single;
  end;

  TCoordXY_Array = array of TCoordXY;
  TArray_CoordXY_Array = array of TCoordXY_Array;

  TFunction = function(const A, B : TCoordXY): Integer;

procedure Offset(var PointXY : TCoordXY; OffsetXY : TCoordXY);
procedure Offset_Array(var PointXY : TCoordXY_Array; OffsetXY : TCoordXY);
procedure Rotate(var PointXY : TCoordXY; Angle : double);
procedure Rotate_Array(var PointXY : TCoordXY_Array; Angle : double);

//function GrahamScan(const Points : TCoordXY_Array): TCoordXY_Array;
function FindConvexHull(var APoints: TCoordXY_Array): Boolean;

//===========================================================================
IMPLEMENTATION

uses
  Math {for hypot()} {,
  u_StackXY};

//---------------------------------------------------------------------------
procedure Offset(var PointXY : TCoordXY; OffsetXY : TCoordXY);
begin
  PointXY.X := PointXY.X + OffsetXY.X;
  PointXY.Y := PointXY.Y + OffsetXY.Y;
end;

//---------------------------------------------------------------------------
procedure Offset_Array(var PointXY : TCoordXY_Array; OffsetXY : TCoordXY);
var
  i : integer;
begin
  for i := 0 to length(PointXY)-1 do begin
    PointXY[i].X := PointXY[i].X + OffsetXY.X;
    PointXY[i].Y := PointXY[i].Y + OffsetXY.Y;
  end;
end;

//---------------------------------------------------------------------------
procedure Rotate(var PointXY : TCoordXY; Angle : double);
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
procedure Rotate_Array(var PointXY : TCoordXY_Array; Angle : double);
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

//https://www.swissdelphicenter.ch/en/showcode.php?id=1616
//---------------------------------------------------------------------------
Procedure Sort_Shell(var A: TCoordXY_Array; Angles: array of Real);
var
  bis, i, j, k: LongInt;
  h: real;
  hA : TCoordXY;
begin
  bis := High(Angles);
  k := bis shr 1; // div 2
  while (k > 0) do begin
    for i := 0 to bis - k do begin
      j := i;
      while (j >= 0) and (Angles[j] > Angles[j + k]) do begin
        h := Angles[j];
        Angles[j] := Angles[j + k];
        Angles[j + k] := h;
        // also do A
        hA := A[j];
        A[j] := A[j + k];
        A[j + k] := hA;
        if (j > k) then begin
          Dec(j, k);
        end else begin
          j := 0;
        end;
      end; // {end while]
    end; // { end for}
    k := k shr 1; // div 2
  end;  // {end while}

end;

// https://www.swissdelphicenter.ch/en/showcode.php?id=2230
// sort an array of points by angle
//---------------------------------------------------------------------------
procedure QuickSortAngle(var A: TCoordXY_Array; Angles: array of Real; iLo, iHi: Integer);
var
  Lo, Hi: Integer;
  Mid: Real;
  TempPoint: TCoordXY;
  TempAngle: Real;
begin
  Lo  := iLo;
  Hi  := iHi;
  Mid := Angles[(Lo + Hi) div 2];
  repeat
    while (Angles[Lo] < Mid) do begin
      Inc(Lo);
    end;
    while (Angles[Hi] > Mid) do begin
      Dec(Hi);
    end;
    if (Lo <= Hi) then begin
      // swap points
      TempPoint := A[Lo];
      A[Lo] := A[Hi];
      A[Hi] := TempPoint;
      // swap angles
      TempAngle := Angles[Lo];
      Angles[Lo] := Angles[Hi];
      Angles[Hi] := TempAngle;
      Inc(Lo);
      Dec(Hi);
    end;
  until (Lo > Hi);
  // perform quicksorts on subsections
  if (Hi > iLo) then begin
    QuickSortAngle(A, Angles, iLo, Hi);
  end;
  if (Lo < iHi) then begin
    QuickSortAngle(A, Angles, Lo, iHi);
  end;
end;

// TBD - possible improvements
// find way to avoid all the setlengths by using pointers instead of moving data ?
// keep pivot as first point in array as opposed to removing it and re-inserting it ?
// replace hypot(sqrt(a^2+b^2)) by something faster ?
// find a way to sort array without generating an Angles array and using a compare
// function instead ?

// return the boundary points of the convex hull of a set of points using Grahams scan
// over-writes the input array - so make a copy first
//---------------------------------------------------------------------------
function FindConvexHull(var APoints: TCoordXY_Array): Boolean;
var
  LAngles: array of Real;
  Lindex, LPivotIndex: Integer;
  LMinY, LMaxX : single;
  L : single;
  LPivot: TCoordXY;
  LBehind, LInfront: TCoordXY;
  LRightTurn: Boolean;
  LVecPoint: TCoordXY;
begin
  // assume success
  Result := True;

  if Length(APoints) = 3 then Exit; // already a convex hull
  if Length(APoints) < 3 then
  begin // not enough points
    Result := False;
    Exit;
  end;

  // find pivot point, which is known to be on the hull
  // point with lowest y - if there are multiple, point with highest x
  LMinY := 1000;
  LMaxX := 1000;
  LPivotIndex := 0;
  for Lindex := 0 to High(APoints) do begin
    if (APoints[Lindex].Y = LMinY) then begin
      if (APoints[Lindex].X > LMaxX) then begin
        LMaxX := APoints[Lindex].X;
        LPivotIndex := Lindex;
      end;
    end else begin
      if (APoints[Lindex].Y < LMinY) then begin
        LMinY := APoints[Lindex].Y;
        LMaxX := APoints[Lindex].X;
        LPivotIndex := Lindex;
      end;
    end;
  end;
  // put pivot into separate variable and remove from array
  LPivot := APoints[LPivotIndex];
  APoints[LPivotIndex] := APoints[High(APoints)];
  SetLength(APoints, High(APoints));

  // calculate angle to pivot for each point in the array
  // quicker to calculate dot product of point with a horizontal comparison vector
  SetLength(LAngles, Length(APoints));
  for Lindex := 0 to High(APoints) do begin
    LVecPoint.X := LPivot.X - APoints[Lindex].X; // point vector
    LVecPoint.Y := LPivot.Y - APoints[Lindex].Y;
    // reduce to a unit-vector - length 1
    L := Hypot(LVecPoint.X, LVecPoint.Y);
    if (L = 0) then begin  // avoid DIV by ZERO
      LAngles[Lindex] := 0;
    end else begin
      LAngles[Lindex] := LVecPoint.X / L;
    end;
  end;

  // sort the points by angle
//  QuickSortAngle(APoints, LAngles, 0, High(APoints)); // stack overflow due to recursion
  Sort_Shell(APoints, LAngles);

  // step through array to remove points that are not part of the convex hull
  Lindex := 1;
  repeat
    // assign points behind and infront of current point
    if (Lindex = 0) then begin
      LRightTurn := True
    end else begin
      LBehind := APoints[Lindex - 1];
      if (Lindex = High(APoints)) then begin
        LInfront := LPivot
      end else begin
        LInfront := APoints[Lindex + 1];
      end;

      // work out if we are making a right or left turn using vector product
      if ((LBehind.X - APoints[Lindex].X) * (LInfront.Y - APoints[Lindex].Y)) -
        ((LInfront.X - APoints[Lindex].X) * (LBehind.Y - APoints[Lindex].Y)) < 0 then begin
        LRightTurn := True;
      end else begin
        LRightTurn := False;
      end;
    end;

    if (LRightTurn) then begin
      // point is currently considered part of the hull
      Inc(Lindex); // go to next point
    end else begin
      // point is not part of the hull
      // remove point from convex hull
      if (Lindex = High(APoints)) then begin
        SetLength(APoints, High(APoints));
      end else begin
        Move(APoints[Lindex + 1], APoints[Lindex],
        (High(APoints) - Lindex) * SizeOf(TCoordXY) + 1);
        SetLength(APoints, High(APoints));
      end;
      Dec(Lindex); // backtrack to previous point
    end;
  until Lindex = High(APoints);

  // add pivot back into points array
  SetLength(APoints, Length(APoints) + 1);
  APoints[High(APoints)] := LPivot;
end;

//---------------------------------------------------------------------------
begin { Initialization }
end.

//-- End of File ------------------------------------------------------------


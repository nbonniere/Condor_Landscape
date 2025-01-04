{
 * u_UTM.pas
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
UNIT u_UTM;

{----------------------------------------------------------------------------
Based on University of Wisconsin UTM conversion routines
----------------------------------------------------------------------------}

//===========================================================================
INTERFACE

//---------------------------------------------------------------------------
// for compile options
{$I Define.pas}

const
  earthRadius = 6371.0;  // WGS84 mean radius

var
  uLatitude : double;
  uLongitude : double;

  uNorthing : double;
  uEasting : double;

  uZone : integer;
  uGrid : char;

//function ReadUTMzoneNS(sZone : string) : NorthSouthFlag;
function ReadUTMzoneNS(sZone : string) : string;
//Procedure CalcUTMzone(lat,long:double);
Procedure CalcUTMzone(lat,long:double;ZoneOffset:integer);
function UTMgridConvert(grid: char) : char;
//Procedure LatLongToUTM(lat,long:double;zone:string;NorthSouth:NorthSouthFlag);
//Procedure LatLongToUTM(lat,long:double;zone,grid:string);
Procedure LatLongToUTM(lat,long:double;zone:integer;grid:char);
//Procedure UTMtoLatLong(northing,easting:double;zone:string;NorthSouth:NorthSouthFlag);
//Procedure UTMtoLatLong(northing,easting:double;zone,grid:string);
Procedure UTMtoLatLong(northing,easting:double;zone:integer;grid:char);
Function WrapUTMzone(Zone:integer) : integer;
Function DifferenceUTMzone(Zone1,Zone2:integer) : integer;

//===========================================================================
IMPLEMENTATION

uses Math,
     SysUtils {format};

const
  a = 6378137.0;    // equatorial radius in metres
  b = 6356752.3142; // polar radius
  k0 = 0.9996;
  Ea0 = 500000.0;      // easting offset = 500 km
  No0_North = 0.0;
  No0_South = 10000000.0;

var
  e, e1sq, n : double;

// convert from UTM grid to north/south
{----------------------------------------------------------------------------}
function UTMgridConvert(grid: char) : char;
begin
  if ( ord(grid) < ord('O') ) then begin
    result := 'S';
  end else begin
    result := 'N';
  end;
end;

{----------------------------------------------------------------------------}
function GetZoneValue(sZone:string):integer;
var
  i,j: integer;
  ErrorCode : integer;

begin
  //skip non-digits
  i := 1;
  while ((length(sZone) >= i) AND NOT (sZone[i] in ['0'..'9'])) do begin
    INC(i);
  end;
  //accumulate digits
  j := i;
//  while sZone[j] in ['0'..'9'] do begin
  while ((length(sZone) >= j) AND (sZone[j] in ['0'..'9'])) do begin
    INC(j);
  end;
  //skip rest
  VAL(copy(sZone,i,j-i),Result,ErrorCode);
end;

{----------------------------------------------------------------------------}
Function DifferenceUTMzone(Zone1,Zone2:integer) : integer;
begin
  result := Zone1-Zone2;
  if (result >= 30) then begin
    result := Result - 60;
  end else begin
    if (result < -30) then begin
      result := result + 60;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Function WrapUTMzone(Zone:integer) : integer;
begin
  if (Zone > 60) then begin
    result := Zone - 60;
  end else begin
    if (Zone < 1) then begin
      result := Zone + 60;
    end else begin
      result := Zone;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure CalcUTMzone(lat,long:double;ZoneOffset:integer);
var
  UTMlong : integer;
begin
  if (Lat >= 0) then begin
//    uNorthSouth := uNorth;
    uGrid := 'N';
  end else begin
//    uNorthSouth := uSouth;
    uGrid := 'S';
  end;
  // deg zones starting at -180 deg.
  UTMlong := trunc((long + 180) / 6) + 1;
//  uZone := format('%d',[WrapUTMzone(UTMlong+ZoneOffset)]); // need to check wrap-around ???
  uZone := WrapUTMzone(UTMlong+ZoneOffset); // need to check wrap-around ???
end;

{----------------------------------------------------------------------------}
//function ReadUTMzoneNS(sZone : string) : NorthSouthFlag;
function ReadUTMzoneNS(sZone : string) : string;
var
  i : integer;
begin
  //skip all but N and S
  i := 1;
  while ((length(sZone) >= i) AND NOT (sZone[i] in ['n','N','s','S'])) do begin
    INC(i);
  end;
  if (UpperCase(sZone[i]) = 'N') then begin
//    result := uNorth;
    result := 'N';
  end else begin
//    result := uSouth;
    result := 'S';
  end;
  //skip rest
end;

{----------------------------------------------------------------------------}
//Procedure LatLongToUTM(lat,long:double;zone:string;NorthSouth:NorthSouthFlag);
Procedure LatLongToUTM(lat,long:double;zone:integer;grid:char);
var
  No0, long0, nu, p, S : double;
  A0, B0, C0, D0, E0 : double;
  K1, K2, K3, K4, K5 : double;
  COS1, COS2, COS3, COS4, TAN2 : double;

begin
  lat := lat * PI/180.0; // convert to radians
//  long := long * PI/180.0; // convert to radians
//  if ((zone = '1') AND (long > 90)) then begin
  if ((zone = 1) AND (long > 90)) then begin
    long := long-360;
  end else begin
//    if ((zone = '60') AND (long < -90)) then begin
    if ((zone = 60) AND (long < -90)) then begin
      long := long-360;
    end;
  end;
  long := long * PI/180.0; // convert to radians

  // every 6 degrees starting at -180 degrees and centered on 6 degree zone
//  long0 := ((GetZoneValue(zone)-1)*6-180+3) *PI/180.0;
  long0 := ((zone-1)*6-180+3) *PI/180.0;

  //rho := =a*(1-e*e)/((1-(e*SIN(lat))^2)^(3/2));
  //nu := a/((1-(e*SIN(lat))^2)^(1/2));
  nu := e*SIN(lat);
  nu := a/SQRT(1-nu*nu);
  p  := long-long0;

  A0 := a*(1-n+(5*n*n/4)*(1-n) +(81*n*n*n*n/64)*(1-n));
  B0 := (3*a*n/2)*(1 - n - (7*n*n/8)*(1-n) + 55*n*n*n*n/64);
  C0 := (15*a*n*n/16)*(1 - n +(3*n*n/4)*(1-n));
  D0 := (35*a*n*n*n/48)*(1 - n + 11*n*n/16);
  E0 := (315*a*n*n*n*n/51)*(1-n);

  S  := A0*lat - B0*SIN(2*lat) + C0*SIN(4*lat) - D0*SIN(6*lat) + E0*SIN(8*lat);

  COS1 := COS(lat);
  COS2 := COS1*COS1;
  COS3 := COS2*COS1;
  COS4 := COS3*COS1;
  TAN2 := TAN(lat);
  TAN2 := TAN2*TAN2;
  K1 := S*k0;
  K2 := nu*SIN(lat)*COS1*k0/2;
  K3 := ((nu*SIN(lat)*COS3)/24)*(5-TAN2+9*e1sq*COS2+4*e1sq*e1sq*COS4)*k0;
  K4 := nu*COS1*k0;
  K5 := COS3*(nu/6)*(1-TAN2+e1sq*COS2)*k0;

  if (grid = 'N') then begin
    No0 := No0_North;
  end else begin
    No0 := No0_South;
  end;

  uNorthing := No0 + (K1+K2*p*p+K3*p*p*p*p);
  uEasting  := Ea0 + (K4*p+K5*p*p*p);
end;

{----------------------------------------------------------------------------}
//Procedure UTMtoLatLong(northing,easting:double;zone:string;NorthSouth:NorthSouthFlag);
Procedure UTMtoLatLong(northing,easting:double;zone:integer;grid:char);
var
  No0, M, mu, e1, fp, Long0 : double;
  J1, J2, J3, J4 : double;
  C1, T1, R1, N1, D : double;
  Q1, Q2, Q3, Q4, Q5, Q6, Q7 : double;
  eSIN2, COS2, TAN2 : double;

begin
  // every 6 degrees starting at -180 degrees and centered on 6 degree zone
//  long0 := ((GetZoneValue(zone)-1)*6-180+3) *PI/180.0;
  long0 := ((zone-1)*6-180+3) *PI/180.0;

  if (grid = 'N') then begin
    No0 := northing - No0_North;
  end else begin
    No0 := northing - No0_South;
  end;

  M := No0/k0;
  mu := M/(a*(1-e*e/4-3*e*e*e*e/64-5*e*e*e*e*e*e/256));
  e1 := (1-SQRT(1-e*e))/(1+SQRT(1-e*e));

  J1 := 3*e1/2-27*e1*e1*e1/32;
  J2 := 21*e1*e1/16-55*e1*e1*e1*e1/32;
  J3 := 151*e1*e1*e1/96;
  J4 := 1097*e1*e1*e1*e1/512;

  fp := mu+J1*SIN(2*mu)+J2*SIN(4*mu)+J3*SIN(6*mu)+J4*SIN(8*mu);

  COS2 := COS(fp);
  COS2 := COS2*COS2;
  C1 := e1sq*COS2;
  TAN2 := TAN(fp);
  TAN2 := TAN2*TAN2;
  T1 := TAN2;
  eSIN2 := SIN(fp);
  eSIN2 := 1-e*e*eSIN2*eSIN2;
  R1 := a*(1-e*e)/(SQRT(eSIN2*eSIN2*eSIN2));
  N1 := a/SQRT(eSIN2);
  D  := (easting-Ea0)/(N1*k0);

  Q1 := N1*TAN(fp)/R1;
  Q2 := D*D/2;
  Q3 := (5+3*T1+10*C1-4*C1*C1-9*e1sq)*D*D*D*D/24;
  Q4 := (61+90*T1+298*C1+45*T1*T1-252*e1sq-3*C1*C1)*D*D*D*D*D*D/720;
  Q5 := D;
  Q6 := (1+2*T1+C1)*D*D*D/6;
  Q7 := (5-2*C1+28*T1-3*C1*C1+8*e1sq+24*T1*T1)*D*D*D*D*D/120;

  uLatitude  := fp-Q1*(Q2-Q3+Q4);
  uLongitude := long0 + (Q5-Q6+Q7)/COS(fp);

  uLatitude := uLatitude *180/PI;
  uLongitude := uLongitude *180/PI;
end;

{----------------------------------------------------------------------------}
begin { Initialization }
  e := SQRT(1-(b/a)*(b/a)); //eccentricity
  e1sq := e*e/(1-e*e);
  n := (a-b)/(a+b);
//  LatLongToUTM(43.64256667,-79.38713889,17,uNorth);
//  UTMtoLatLong(4833439,630084,17,uNorth);
end.

{--- End of File ------------------------------------------------------------}

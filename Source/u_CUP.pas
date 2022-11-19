{
 * u_CUP.pas
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

{============================================================================}
{                                                                            }
{ Module:     u_CUP.PAS                                                      }
{ Created by: D. Bonniere                                                    }
{ Abstract:   Provides CUP file functions.                                   }
{                                                                            }
{============================================================================}

//---------------------------------------------------------------------------
UNIT u_CUP;

{$S-,R-,I-,V-,F+,D+}

{============================================================================}
INTERFACE

uses
  u_Terrain;

procedure MakeDummyCUP(FilePath,Filename:string);
procedure Append_CUP_File(UTM_Limits : Extents;
                          FilePath,Filename,
                          FilePath_a,Filename_a : string);

{============================================================================}

IMPLEMENTATION
uses SysUtils,
  u_TileList, u_UTM;

//---------------------------------------------------------------------------
procedure MakeDummyCUP(FilePath,Filename:string);
const
  sS = 'SNWE';
var
  CUP_File : TextFile;
  LatString : string;
  LongString : string;
  Index : integer;
  Degrees : double;
begin
  Degrees := scCentre.TileLatBottom;
  if (Degrees >= 0) then begin
    Index := 2;
  end else begin
    Index := 1;
    Degrees := -Degrees;
  end;
  LatString :=  formatfloat('00',trunc(Degrees))+
                formatfloat('00.000',frac(Degrees)*60)+
                copy(sS,Index,1);

  Degrees := scCentre.TileLongRight;
  if (Degrees >= 0) then begin
    Index := 4;
  end else begin
    Index := 3;
    Degrees := -Degrees;
  end;
  LongString := formatfloat('000',trunc(Degrees))+
                formatfloat('00.000',frac(Degrees)*60)+
                copy(sS,Index,1);

  AssignFile(CUP_File,FilePath+'\'+Filename);
  Rewrite(CUP_File);
  writeln(CUP_File,'name,code,country,lat,lon,elev,style,rwdir,rwlen,freq,desc');
  writeln(CUP_File,format('"Centre",Centre,,%s,%s,0.0m,1,,,,',[LatString,LongString]));
  CloseFile(CUP_File);
end;

{----------------------------------------------------------------------------}
procedure Append_CUP_File(UTM_Limits : Extents;
                          FilePath,Filename,
                          FilePath_a,Filename_a : string);
var
  CUP_File : TextFile;
  CUP_File_a : TextFile;
  LineString : string;
  cupString : string;
  apLatitude, apLongitude : double;
  CommaPos : integer;
begin
  AssignFile(CUP_File,FilePath+'\'+Filename+'.cup');
  if (NOT FileExists(FilePath+'\'+Filename+'.cup')) then begin
    Rewrite(CUP_File);
    writeln(CUP_File,'name,code,country,lat,lon,elev,style,rwdir,rwlen,freq,desc');
  end else begin
    Append(CUP_File);
  end;

  AssignFile(CUP_File_a,FilePath_a+'\'+Filename_a+'.cup');
  Reset(CUP_File_a);
  readln(CUP_File_a,LineString);  // skip first line
  While not EOF(CUP_File_a) do begin
    readln(CUP_File_a,LineString);
    // parse lat and long fields and compare to limits
    // kludge, replace first two commas and look for third as an index
    cupString := StringReplace(LineString,',','|',[]);
    cupString := StringReplace(cupString,',','|',[]);
    CommaPos := pos(',',cupString);
    if (CommaPos <> 0) then begin
      apLatitude := strtofloat(copy(cupString,CommaPos+1,2));
      apLatitude := apLatitude + strtofloat(copy(cupString,CommaPos+1+2,6))/60;
      if (uppercase(cupString[CommaPos+1+2+6]) = 'S') then begin
        apLatitude := -apLatitude;
      end;
      apLongitude := strtofloat(copy(cupString,CommaPos+11,3));
      apLongitude := apLongitude + strtofloat(copy(cupString,CommaPos+11+3,6))/60;
      if (uppercase(cupString[CommaPos+11+3+6]) = 'W') then begin
        apLongitude := -apLongitude;
      end;
    end else begin
      continue;
    end;

    LatLongToUTM(apLatitude, apLongitude, IntToStr(u_Terrain.TerrainHeader.tUTMzone), uGrid);
    if (uEasting > UTM_Limits.xMax) then begin
      continue;
    end;
    if (uEasting < UTM_Limits.xMin) then begin
      continue;
    end;
    if (uNorthing > UTM_Limits.yMax) then begin
      continue;
    end;
    if (uNorthing < UTM_Limits.yMin) then begin
      continue;
    end;

    writeln(CUP_File,LineString);
  end;
  CloseFile(CUP_File_a);
  CloseFile(CUP_File);
end;

{----------------------------------------------------------------------------}
begin
end.

{=== end of file ============================================================}


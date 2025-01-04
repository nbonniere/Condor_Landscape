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
  StdCtrls,
  u_Terrain;

procedure MakeDummyCUP(FilePath,Filename:string);
procedure Append_CUP_File(UTM_Limits : Extents;
                          FilePath,Filename,
                          FilePath_a,Filename_a : string);
var
  Memo_Message : TMemo;  // external TMemo for messages

{============================================================================}

IMPLEMENTATION
uses
  SysUtils,
  u_TileList, u_UTM;

{----------------------------------------------------------------------------}
Procedure MessageShow(Info : string);
begin
  if assigned(Memo_Message) then begin
    Memo_Message.lines.add(Info);
  end;
end;

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
function SkipAfield(var cString : string) : boolean;
var
  CommaPos : integer;
begin
  CommaPos := pos(',',cString);
  if (CommaPos = 0) then begin
    result := false;
  end else begin
    cString := copy(cString,CommaPos+1,
      length(cString)-CommaPos);
    result := true;
  end;
end;
{----------------------------------------------------------------------------}
function GetAfield(var cString : string) : string;
var
  CommaPos : integer;
begin
  CommaPos := pos(',',cString);
  if (CommaPos = 0) then begin
    result := trim(cString);
    cString := '';
  end else begin
    result := trim(copy(cString,1,CommaPos-1));
    cString := copy(cString,CommaPos+1,
      length(cString)-CommaPos);
  end;
end;

{----------------------------------------------------------------------------}
function GetLatitude(var cString : string) : single;
begin
  try
    result := strtofloat(copy(cString,1,2));
    result := result + strtofloat(copy(cString,3,
      length(cString)-2-1))/60;
    if (uppercase(cString[length(cString)]) = 'S') then begin
      result := -result;
    end;
  except
    // IO error
    On E : EInOutError do begin
    // Division by zero
    end;
    On E : EDivByZero do begin
    // Catch other errors
    end;
    else begin
    end;
  end;
end;

{----------------------------------------------------------------------------}
function GetLongitude(var cString : string) : single;
begin
  try
    result := strtofloat(copy(cString,1,3));
    result := result + strtofloat(copy(cString,4,
      length(cString)-3-1))/60;
    if (uppercase(cString[length(cString)]) = 'W') then begin
      result := -result;
    end;
  except
    // IO error
    On E : EInOutError do begin
    // Division by zero
    end;
    On E : EDivByZero do begin
    // Catch other errors
    end;
    else begin
    end;
  end;
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
  LatLong : string;
  apLatitude, apLongitude : double;

begin
  AssignFile(CUP_File,FilePath+'\'+Filename+'.cup');
  if (NOT FileExists(FilePath+'\'+Filename+'.cup')) then begin
    Rewrite(CUP_File);
    writeln(CUP_File,'name,code,country,lat,lon,elev,style,rwdir,rwlen,freq,desc');
  end else begin
    Append(CUP_File);
  end;

  if (NOT FileExists(FilePath_a+'\'+Filename_a+'.cup')) then begin
//    MessageShow('Warning: '+Filename_a+'.obj file not found');
    Beep;
  end else begin
    AssignFile(CUP_File_a,FilePath_a+'\'+Filename_a+'.cup');
    Reset(CUP_File_a);
    readln(CUP_File_a,LineString);  // skip first line
    While not EOF(CUP_File_a) do begin
      readln(CUP_File_a,LineString);
//  MessageShow(LineString);
      cupString := Linestring;
      // parse lat and long fields and compare to limits
      if (Not SkipAfield(CupString)) then begin
        continue; // problem - move on
      end else begin
        if (Not SkipAfield(CupString)) then begin
          continue; // problem - move on
        end else begin
          if (Not SkipAfield(CupString)) then begin
            continue; // problem - move on
          end else begin
            LatLong := GetAfield(CupString);
            apLatitude := GetLatitude(LatLong);
            LatLong := GetAfield(CupString);
            apLongitude := GetLongitude(LatLong);
	  end;
        end;
      end;
      // now check if within scenery limits
//      LatLongToUTM(apLatitude, apLongitude, IntToStr(u_Terrain.TerrainHeader.tUTMzone), uGrid);
      LatLongToUTM(apLatitude, apLongitude, u_Terrain.TerrainHeader.tUTMzone, uGrid);
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
  end;
  CloseFile(CUP_File);
end;

{----------------------------------------------------------------------------}
end.

{=== end of file ============================================================}


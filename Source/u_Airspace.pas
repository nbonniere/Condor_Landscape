{
 * u_Airspace.pas
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
{ Module:     u_Airspace.PAS                                                 }
{ Created by: D. Bonniere                                                    }
{ Abstract:   Provides Airspace file functions.                              }
{                                                                            }
{============================================================================}

//---------------------------------------------------------------------------
UNIT u_Airspace;

{$S-,R-,I-,V-,F+,D+}

{============================================================================}
INTERFACE

uses
  u_Terrain;

procedure MakeDummy_AIR(FilePath,Filename:string);
procedure MakeDummy_AHA(FilePath,Filename:string);
procedure Append_AIR_File(UTM_Limits : Extents;
                          FilePath,Filename,
                          FilePath_a,Filename_a : string);

{============================================================================}

IMPLEMENTATION
uses SysUtils,
  u_TileList, u_UTM;

//---------------------------------------------------------------------------
procedure MakeDummy_AIR(FilePath,Filename:string);
var
  AIR_File : TextFile;
begin
  AssignFile(AIR_File,FilePath+'\'+Filename);
  Rewrite(AIR_File);
  // empty file
  CloseFile(AIR_File);
end;

//---------------------------------------------------------------------------
procedure MakeDummy_AHA(FilePath,Filename:string);
var
  AHA_File : TextFile;
begin
  AssignFile(AHA_File,FilePath+'\'+Filename);
  Rewrite(AHA_File);
  writeln(AHA_File,'0'); // just a zero since file is empty
  CloseFile(AHA_File);
end;

{----------------------------------------------------------------------------}
procedure Append_AIR_File(UTM_Limits : Extents;
                          FilePath,Filename,
                          FilePath_a,Filename_a : string);
var
  AIR_File : TextFile;
  AIR_File_a : TextFile;
  LineString : string;
  airString : string;
  apLatitude, apLongitude : double;
  CommaPos : integer;
begin
  AssignFile(AIR_File,FilePath+'\'+Filename+'.air');
  if (NOT FileExists(FilePath+'\'+Filename+'.air')) then begin
    Rewrite(AIR_File);
  end else begin
    Append(AIR_File);
  end;

  if (NOT FileExists(FilePath_a+'\'+Filename_a+'.air')) then begin
//    MessageShow('Warning: '+Filename_a+'.obj file not found');
    Beep;
  end else begin
    AssignFile(AIR_File_a,FilePath_a+'\'+Filename_a+'.air');
    Reset(AIR_File_a);
    readln(AIR_File_a,LineString);  // skip first line
    While not EOF(AIR_File_a) do begin
      readln(AIR_File_a,LineString);
      // parse lat and long fields and compare to limits
      // TBD ???
      writeln(AIR_File,LineString);
    end;
    CloseFile(AIR_File_a);
  end;
  CloseFile(AIR_File);
end;

{----------------------------------------------------------------------------}
begin
end.

{=== end of file ============================================================}


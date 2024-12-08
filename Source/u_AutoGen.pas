{
 * u_AutoGen.pas
 * Copyright (C) 2024 - Nick Bonnière
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
{ Module:     u_AutoGen.pas                                                  }
{ Created by: D. Bonniere                                                    }
{ Date:       28 Nov 2024                                                    }
{ Abstract:   Provides AutoGen functions.                                    }
{                                                                            }
{============================================================================}

//---------------------------------------------------------------------------
UNIT u_AutoGen;

{$S-,R-,I-,V-,F+,D+}

{============================================================================}
INTERFACE

procedure MakeDummy_OHA(Column, Row : integer; FilePath, Filename:string);

{============================================================================}
IMPLEMENTATION

uses
  SysUtils,
  u_TileList;

// AutoGen Objects HASH
//---------------------------------------------------------------------------
procedure MakeDummy_OHA(Column, Row : integer; FilePath, Filename:string);
var
  i, j : integer;
  OHA_File : TextFile;

begin
  AssignFile(OHA_File,FilePath+'\'+Filename);
  Rewrite(OHA_File);
  for i := 0 to Column-1 do begin
    for j := 0 to Row-1 do begin
      writeln(OHA_File,format('%s',[MakeTileName(i,j, xxxyyy)])); // just the index since .air file is empty
    end;
  end;
  CloseFile(OHA_File);
end;

//---------------------------------------------------------------------------
begin
end.

{=== end of file ============================================================}


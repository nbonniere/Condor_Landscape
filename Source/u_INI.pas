{
 * u_INI.pas
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
{ Module:     u_INI.PAS                                                      }
{ Created by: D. Bonniere                                                    }
{ Abstract:   Provides INI file functions.                                   }
{                                                                            }
{============================================================================}

//---------------------------------------------------------------------------
UNIT u_INI;

{$S-,R-,I-,V-,F+,D+}

{============================================================================}
INTERFACE

//uses stdctrls;

procedure MakeDummyINI(FilePath,Filename:string);


{============================================================================}

IMPLEMENTATION
//uses SysUtils;

//---------------------------------------------------------------------------
procedure MakeDummyINI(FilePath,Filename:string);
var
  INI_File : TextFile;
begin
  AssignFile(INI_File,FilePath+'\'+Filename);
  Rewrite(INI_File);
  writeln(INI_File,'[General]');
  writeln(INI_File,'Version=1.0');
  writeln(INI_File,'RealtimeShading=1');
  CloseFile(INI_File);
end;

{----------------------------------------------------------------------------}
begin
end.

{=== end of file ============================================================}


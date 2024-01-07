{
 * u_QuarterTile.pas
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
{ Module:     u_QuarterTile.PAS                                                      }
{ Created by: D. Bonniere                                                    }
{ Abstract:   Provides Quarter Tile functions.                                   }
{                                                                            }
{============================================================================}

//---------------------------------------------------------------------------
UNIT u_QuarterTile;

{$S-,R-,I-,V-,F+,D+}

{============================================================================}
INTERFACE

uses
  stdctrls;

var
  Memo_Message : TMemo;  // external TMemo for messages
  QT_Folder : string;    // external path for file output

  // saved true corners
  Tile_RB_Lat_save  : real;
  Tile_RB_Long_save : real;
  Tile_LB_Lat_save  : real;
  Tile_LB_Long_save : real;
  Tile_RT_Lat_save  : real;
  Tile_RT_Long_save : real;
  Tile_LT_Lat_save  : real;
  Tile_LT_Long_save : real;

Procedure Make_QT_All_BatchFile(CurrentRow, CurrentColumn, offset_Row, offset_Column : Integer);

Procedure CalcCorners(CurrentRow, CurrentColumn, offset_Row, offset_Column : Integer);

{============================================================================}

IMPLEMENTATION
uses
  {Windows, StdCtrls,} SysUtils, FileCtrl,
  u_TileList, u_UTM, u_Terrain, u_SceneryHDR;

{----------------------------------------------------------------------------}
Procedure MessageShow(Info : string);
begin
  if (Memo_Message <> nil) then begin
    Memo_Message.lines.add(Info);
  end;
end;

//---------------------------------------------------------------------------
Procedure CalcCorners(CurrentRow, CurrentColumn, offset_Row, offset_Column : Integer);
var
  Easting, Northing : double;

begin
//  Easting := UTM_Right+Resolution/2;    // extra 1/2 of 90 metres all sides
  Easting := UTM_Right+Legacy_Offset;    // extra 1/2 of 90 metres all sides
//  Northing := UTM_Bottom-Resolution/2;  // i.e. from tile centre
  Northing := UTM_Bottom-Legacy_Offset;  // i.e. from tile centre

  // tile corners
  UTMtoLatLong(Northing+CurrentRow*Resolution*tRows + offset_Row*Resolution*tRows/4,
    Easting-CurrentColumn*Resolution*tRows - offset_Column*Resolution*tColumns/4,
    UTM_Zone,UTM_ZoneNS);
  Tile_RB_Lat_save  := uLatitude;
  Tile_RB_Long_save := uLongitude;

  UTMtoLatLong(Northing+CurrentRow*Resolution*tRows + offset_Row*Resolution*tRows/4,
    Easting-CurrentColumn*Resolution*tRows - (offset_Column+1)*Resolution*tColumns/4,
    UTM_Zone,UTM_ZoneNS);
  Tile_LB_Lat_save  := uLatitude;
  Tile_LB_Long_save := uLongitude;

  UTMtoLatLong(Northing+CurrentRow*Resolution*tRows + (offset_Row+1)*Resolution*tRows/4,
    Easting-CurrentColumn*Resolution*tRows - offset_Column*Resolution*tColumns/4,
    UTM_Zone,UTM_ZoneNS);
  Tile_RT_Lat_save  := uLatitude;
  Tile_RT_Long_save := uLongitude;

  UTMtoLatLong(Northing+CurrentRow*Resolution*tRows + (offset_Row+1)*Resolution*tRows/4,
    Easting-CurrentColumn*Resolution*tRows - (offset_Column+1)*Resolution*tColumns/4,
    UTM_Zone,UTM_ZoneNS);
  Tile_LT_Lat_save  := uLatitude;
  Tile_LT_Long_save := uLongitude;
end;

//-------------------------------------------------------------------------------------
Procedure Make_QT_All_BatchFile(CurrentRow, CurrentColumn, offset_Row, offset_Column : Integer);
var
  i,j : integer;
  FileName : string;
  FilePath : string;
  TileIndex : integer;
  TileName : string;
  QT_file : TextFile;

begin
  TileIndex := CurrentRow * (TileColumnCount+1) + CurrentColumn;

  FilePath := QT_folder +'\SourceTiles\'+ TileList[TileIndex].TileName +'\QuarterTiles';
  // create path
  ForceDirectories(FilePath);

  TileName := TileList[TileIndex].TileName+format('_%2.2d_%2.2d',[offset_Column,offset_Row]);
//  TextureName := format('t%2.2d%2.2d',[CurrentColumn*4+offset_Column,CurrentRow*4+offset_Row]);

  //open the file
  FileName := 'MAKE_ALL_'+TileName+'.bat';
  AssignFile(QT_file, FilePath +'\'+ FileName);
  Rewrite(QT_file);

  writeln(QT_file,'@echo off');
  // use || to execute next command if previous one failed
  writeln(QT_file,'call Batch_Download_'+TileName+'.bat || exit /b 9');
  writeln(QT_file,'call Batch_Combine_'+TileName+'.bat || exit /b 9');
  writeln(QT_file,'call GDAL_'+TileName+'_3857.bat || exit /b 9');
  writeln(QT_file,'call DDS_'+TileName+'.bat || exit /b 9');

  // close the file
  Close(QT_file);
  MessageShow(FileName+' done.');
end;

{----------------------------------------------------------------------------}
begin
end.

{=== end of file ============================================================}


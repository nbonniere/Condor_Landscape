{
 * u_MakeGDAL.pas
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

// EPSG:4326 versus EPSG:3857 see u_Tile_XYZ

{
 -co tfw=yes  not really needed since the .tfw is not used

 simpler to use -a_ullr instead of -gcp if corners are used anyway

 -s_srs xxxx with gdalwarp not needed when -a_srs specified in gdal_translate  

 geid (Google Earth) is 4326, while gmid (Google Satellite Map) is 3857 !
   tested and confirmed - with geid, do not convert from 4326 to 3857 !
   which means AllmapSoft geid TIF out is ok.
}

//----------------------------------------------------------------------------
unit u_MakeGDAL;

{----------------------------------------------------------------------------
Create batch files that use GDAL functions to convert a lat/long bitmap
to a UTM projected bitmap.
- first convert the bitmap to GeoTIFF to embed the corner coordinates
  - coordinates come from GMID log file
- second warp the bitmap to UTM, crop it to the proper UTM extent, and
  re-size it to the desired final size
  - UTM extents come from the header file
  - final size can be 1024, 2048, 4096, 8192
----------------------------------------------------------------------------}
interface

uses StdCtrls, FileCtrl, SysUtils;

var
  Memo_Message : TMemo;       // external TMemo for messages
  GDALFolder : string;        // external path for file output
  GDALlibraryFolder : string; // external path for library
//  CondorFolder : string;      // external path for Condor Folder
  CompressorFolder : string;  // external path for Texture Compressor Folder
  ZoomLevel : string;
  OutputTileSize : string;
  File_Destination : string;
  DXT_Type : string;

Procedure xMakeGDALbatchFile(TileIndex : integer);     // 4326
Procedure xMakeAutoGDALbatchFile(TileIndex : integer); // 4326
Procedure MakeAutoGDALbatchFile(DetectTree : boolean; epsg : integer; TileIndex : integer);  // 3857 or 4326
Procedure MakeGDALbatchTiffFile(TileIndex : integer);  // based on GEOtiff
//Procedure MakeDDSbatchFile(TileIndex : integer);

Procedure MakeGDALoverallBatchFile(Name : string);
Procedure MakeAutoGDALoverallBatchFile(Name : string);
Procedure xxMakeGDALoverallBatchFile(Name : string);
Procedure MakeGDALoverallTiffBatchFile(Name : string);  // based on GEOtiff

Procedure MakeLandsatGDALbatchFile(TileIndex : integer);

Procedure MakeGDALquarterTile_MC(CurrentRow, CurrentColumn, offset_Row, offset_Column : Integer);
Procedure MakeAutoGDALquarterTile(epsg : Integer; CurrentRow, CurrentColumn, offset_Row, offset_Column : Integer);
//Procedure MakeDDSquarterTile(CurrentRow, CurrentColumn, offset_Row, offset_Column : Integer);

//Procedure MakeGDAL_All_BatchFile(DetectTree : boolean; TIFF : boolean);
Procedure MakeGDAL_All_BatchFile(DetectTree, TIFF : boolean; epsg : integer);
Procedure Make_DetectTree_to_ForestMaps_BatchFile;

// for HiResRunway
Procedure MakeAutoGDAL_Generic(epsg : Integer; FileName, FilePath : string;
  Name, Zoom_Level : string;
  UTM_Left, UTM_Right, UTM_Bottom, UTM_Top : single);

//----------------------------------------------------------------------------
implementation

uses Math,
  u_Terrain, u_TileList, u_GMIDlog, u_SceneryHDR, u_BMP;

var
  GDALfile : TextFile;

{----------------------------------------------------------------------------}
Procedure MessageShow(Info : string);
begin
  if (Memo_Message <> nil) then begin
    Memo_Message.lines.add(Info);
  end;
end;

// right now it is based on EPSG:4326 coordinates (WGS84) which is not accurate
//-------------------------------------------------------------------------------------
Procedure xMakeGDALbatchFile(TileIndex : integer);

var
//  i : integer;
  FileName : string;
  FilePath : string;

  Tile_B_Lat : double;
  Tile_T_Lat : double;
  Tile_L_Long : double;
  Tile_R_Long : double;

begin
  FilePath := GDALfolder +'\SourceTiles\'+ TileList[TileIndex].TileName;
  ForceDirectories(FilePath);

  //open the file
  FileName := 'GDAL_'+TileList[TileIndex].TileName+'.bat';
  AssignFile(GDALfile, FilePath +'\'+ FileName);
  Rewrite(GDALfile);

  writeln(GDALfile,'@echo off');
  writeln(GDALfile,'setlocal');
  writeln(GDALfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
  // suppress generation of .xml file
  writeln(GDALfile,'set GDAL_PAM_ENABLED=NO');
  writeln(GDALfile,'rem goto directory where batch file is');
  writeln(GDALfile,'cd /d %~dp0');

// for old sceneries
//  // if file in a separate combined subfolder move it into the main folder
//  writeln(GDALfile,'move '+TileList[TileIndex].TileName+'_combined\'+TileList[TileIndex].TileName+'.bmp'+' .\' );

  writeln(GDALfile,'rem convert bitmap to GeoTiff to embed the lat/long coordinates');
  writeln(GDALfile,format('set real_left=%1.8f',[SourceLeftLongitude]));
  writeln(GDALfile,format('set real_top=%1.8f',[SourceTopLatitude]));
  writeln(GDALfile,format('set real_right=%1.8f',[SourceRightLongitude]));
  writeln(GDALfile,format('set real_bottom=%1.8f',[SourceBottomLatitude]));
  writeln(GDALfile,format('set image_width=%d',[BitmapWidth]));
  writeln(GDALfile,format('set image_height=%d',[BitmapHeight]));
  writeln(GDALfile,'set sourcebmp='+TileList[TileIndex].TileName+'_combined\'+TileList[TileIndex].TileName+'.bmp');
  writeln(GDALfile,'set destinationtiff='+TileList[TileIndex].TileName+'\'+'bigmap.tif');

  writeln(GDALfile,'gdal_translate -gcp 0 0 %real_left% %real_top% -gcp %image_width% 0 %real_right% %real_top% -gcp %image_width% %image_height% %real_right% %real_bottom% -gcp 0 %image_height% %real_left% %real_bottom% %sourcebmp% %destinationtiff%');
  writeln(GDALfile,'del %sourcebmp%');

  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - Legacy_Offset;
  Tile_T_Lat  := Tile_B_Lat + 23040;
  Tile_L_Long := UTM_Right + Legacy_Offset - TileList[TileIndex+1].TileUTMRight;
  Tile_R_Long := Tile_L_Long + 23040;

  writeln(GDALfile,'rem crop to UTM coordinates');
//  writeln(GDALfile,'set utm_zone='+UTM_Zone);
  writeln(GDALfile,format('set utm_zone=%d',[UTM_Zone]));
  if (UTM_ZoneNS = 'N') then begin
    writeln(GDALfile,'set utm_grid=north');
  end else begin
    writeln(GDALfile,'set utm_grid=south');
  end;
  writeln(GDALfile,format('set utm_wanted_left=%1.1f',[Tile_L_Long]));
  writeln(GDALfile,format('set utm_wanted_bottom=%1.1f',[Tile_B_Lat]));
  writeln(GDALfile,format('set utm_wanted_right=%1.1f',[Tile_R_Long]));
  writeln(GDALfile,format('set utm_wanted_top=%1.1f',[Tile_T_Lat]));

  writeln(GDALfile,'rem re-size');
  writeln(GDALfile,'set image_width='+OutputTileSize);
  writeln(GDALfile,'set image_height='+OutputTileSize);

  writeln(GDALfile,'set sourcetiff='+TileList[TileIndex].TileName+'\'+'bigmap.tif');
  writeln(GDALfile,'set destinationtiff='+TileList[TileIndex].TileName+'\'+'UTMmap.tif');

  writeln(GDALfile,'rem convert, with cropping, and re-sizing');
  writeln(GDALfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
  writeln(GDALfile,'gdalwarp.exe -r lanczos -of GTiff -s_srs WGS84 -t_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -tps -ts %image_width% %image_height% -te %utm_wanted_left% %utm_wanted_bottom% %utm_wanted_right% %utm_wanted_top% %sourcetiff% %destinationtiff%');
  writeln(GDALfile,'del %sourcetiff%');

  writeln(GDALfile,'rem convert to bitmap');
  writeln(GDALfile,'set sourcetiff='+TileList[TileIndex].TileName+'\'+'UTMmap.tif');
  writeln(GDALfile,'set destinationbmp='+TileList[TileIndex].TileName+'.bmp');
  writeln(GDALfile,'gdal_translate -of BMP %sourcetiff% %destinationbmp%');
  writeln(GDALfile,'del %sourcetiff%');
  writeln(GDALfile,'move %destinationbmp% ' + File_Destination);

  writeln(GDALfile,'endlocal');

  // close the file
  Close(GDALfile);

  MessageShow(FileName+' done.');
end;

// try getting data from gmid file directly
// right now it is based on EPSG:4326 coordinates (WGS84) which is not accurate
//-------------------------------------------------------------------------------------
Procedure xMakeAutoGDALbatchFile(TileIndex : integer);

var
//  i : integer;
  TileName : string;
  FileName : string;
  FilePath : string;

  Tile_B_Lat : double;
  Tile_T_Lat : double;
  Tile_L_Long : double;
  Tile_R_Long : double;

begin
  FilePath := GDALfolder +'\SourceTiles\'+ TileList[TileIndex].TileName;
  ForceDirectories(FilePath);

  TileName := TileList[TileIndex].TileName;
  //open the file
  FileName := 'GDAL_'+TileName+'.bat';
  AssignFile(GDALfile, FilePath +'\'+ FileName);
  Rewrite(GDALfile);

  writeln(GDALfile,'@echo off');
  writeln(GDALfile,'setlocal');
  writeln(GDALfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
  writeln(GDALfile,'rem goto directory where batch file is');
  writeln(GDALfile,'cd /d %~dp0');

// for old sceneries
//  // if file in a separate combined subfolder move it into the main folder
//  writeln(GDALfile,'move '+TileName+'_combined\'+TileName+'.bmp'+' .\' );

  writeln(GDALfile,'rem convert bitmap to GeoTiff to embed the lat/long coordinates');
//  writeln(GDALfile,format('set real_left=%1.8f',[SourceLeftLongitude]));
//  writeln(GDALfile,format('set real_top=%1.8f',[SourceTopLatitude]));
//  writeln(GDALfile,format('set real_right=%1.8f',[SourceRightLongitude]));
//  writeln(GDALfile,format('set real_bottom=%1.8f',[SourceBottomLatitude]));
//  writeln(GDALfile,format('set image_width=%d',[BitmapWidth]));
//  writeln(GDALfile,format('set image_height=%d',[BitmapHeight]));

  writeln(GDALfile,'set FileName='+TileName+'.gmid');
  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "Left_Longitude_download=" %FileName%'') do set real_left=%%a');
  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "Top_Latitude_download=" %FileName%'') do set real_top=%%a');
  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "Right_Longitude_download=" %FileName%'') do set real_right=%%a');
  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "Bottom_Latitude_download=" %FileName%'') do set real_bottom=%%a');
  // trim spaces
//  writeln(GDALfile,'set real_left=%real_left: =%');
//  writeln(GDALfile,'set real_top=%real_top: =%');
//  writeln(GDALfile,'set real_right=%real_right: =%');
//  writeln(GDALfile,'set real_bottom=%real_bottom: =%');

  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "MinX=" %FileName%'') do set MinX=%%a');
  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "MinY=" %FileName%'') do set MinY=%%a');
  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "MaxX=" %FileName%'') do set MaxX=%%a');
  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "MaxY=" %FileName%'') do set MaxY=%%a');

  writeln(GDALfile,'set /a image_width=(MaxX-MinX+1)*256');
  writeln(GDALfile,'set /a image_height=(MaxY-MinY+1)*256');
//  writeln(GDALfile,'echo %image_width% %image_height%');

  writeln(GDALfile,'set sourcebmp='+TileName+'_combined\'+TileName+'.bmp');
  writeln(GDALfile,'set sourcetiff='+TileName+'_combined\'+TileName+'.tif');
  writeln(GDALfile,'set destinationtiff='+TileName+'\'+'bigmap.tif');

  writeln(GDALfile,'gdal_translate -gcp 0 0 %real_left% %real_top% -gcp %image_width% 0 %real_right% %real_top% -gcp %image_width% %image_height% %real_right% %real_bottom% -gcp 0 %image_height% %real_left% %real_bottom% %sourcebmp% %destinationtiff%');
  writeln(GDALfile,'del %sourcebmp%');
  writeln(GDALfile,'if exist %sourcetiff% del %sourcetiff%');

  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - Legacy_Offset;
  Tile_T_Lat  := Tile_B_Lat + 23040;
  Tile_L_Long := UTM_Right + Legacy_Offset - TileList[TileIndex+1].TileUTMRight;
  Tile_R_Long := Tile_L_Long + 23040;

  writeln(GDALfile,'rem crop to UTM coordinates');
//  writeln(GDALfile,'set utm_zone='+UTM_Zone);
  writeln(GDALfile,format('set utm_zone=%d',[UTM_Zone]));
  if (UTM_ZoneNS = 'N') then begin
    writeln(GDALfile,'set utm_grid=north');
  end else begin
    writeln(GDALfile,'set utm_grid=south');
  end;
  writeln(GDALfile,format('set utm_wanted_left=%1.1f',[Tile_L_Long]));
  writeln(GDALfile,format('set utm_wanted_bottom=%1.1f',[Tile_B_Lat]));
  writeln(GDALfile,format('set utm_wanted_right=%1.1f',[Tile_R_Long]));
  writeln(GDALfile,format('set utm_wanted_top=%1.1f',[Tile_T_Lat]));

  writeln(GDALfile,'rem re-size');
  writeln(GDALfile,'set image_width='+OutputTileSize);
  writeln(GDALfile,'set image_height='+OutputTileSize);

  writeln(GDALfile,'set sourcetiff='+TileName+'\'+'bigmap.tif');
  writeln(GDALfile,'set destinationtiff='+TileName+'\'+'UTMmap.tif');

  writeln(GDALfile,'rem convert, with cropping, and re-sizing');
  writeln(GDALfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
  writeln(GDALfile,'gdalwarp.exe -r lanczos -of GTiff -s_srs WGS84 -t_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -tps -ts %image_width% %image_height% -te %utm_wanted_left% %utm_wanted_bottom% %utm_wanted_right% %utm_wanted_top% %sourcetiff% %destinationtiff%');
  writeln(GDALfile,'del %sourcetiff%');

  writeln(GDALfile,'rem convert to bitmap');
  writeln(GDALfile,'set sourcetiff='+TileName+'\'+'UTMmap.tif');
  writeln(GDALfile,'set destinationbmp='+TileName+'.bmp');
  writeln(GDALfile,'gdal_translate -of BMP %sourcetiff% %destinationbmp%');
  writeln(GDALfile,'del %sourcetiff%');
  writeln(GDALfile,'move %destinationbmp% ' + File_Destination);

  writeln(GDALfile,'endlocal');

  // close the file
  Close(GDALfile);

  MessageShow(FileName+' done.');
end;

// try getting data from gmid file directly
// more accurate version to avoid latitude distortion
// based on EPSG:3857 conversion
//-------------------------------------------------------------------------------------
Procedure MakeAutoGDALbatchFile(DetectTree : boolean; epsg : integer; TileIndex : integer);

var
//  i : integer;
  TileName : string;
  FileName : string;
  Ext_Name : string;
  FilePath : string;
  Zoom_Suffix : string;

  Tile_B_Lat : double;
  Tile_T_Lat : double;
  Tile_L_Long : double;
  Tile_R_Long : double;

begin
  FilePath := GDALfolder +'\SourceTiles\'+ TileList[TileIndex].TileName;
  ForceDirectories(FilePath);

  TileName := TileList[TileIndex].TileName;
  //open the file
  Ext_Name := '';
  if (DetectTree) then begin
    Ext_Name := Ext_Name + '_DetectTree';
  end;
  FileName := 'GDAL_';
  //open the file
  if (epsg = 4326) then begin
    FileName := format('%s_%d.bat',[FileName+TileName+Ext_Name,epsg]);
  end else begin
    FileName := FileName+TileName+Ext_Name+'.bat';
  end;
  AssignFile(GDALfile, FilePath +'\'+ FileName);
  Rewrite(GDALfile);

  writeln(GDALfile,'@echo off');
  writeln(GDALfile,'rem Enable Delayed Expansion for !Variables! ');
  writeln(GDALfile,'setlocal EnableDelayedExpansion');
  writeln(GDALfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
  writeln(GDALfile,'set GDAL_DATA='+GDALlibraryFolder+'\..\share\epsg_csv');
  // suppres generation of .xml file
  writeln(GDALfile,'set GDAL_PAM_ENABLED=NO');
  writeln(GDALfile,'rem goto directory where batch file is');
  writeln(GDALfile,'cd /d %~dp0');

// for old sceneries
//  // if file in a separate combined subfolder move it into the main folder
//  writeln(GDALfile,'move '+TileName+'_combined\'+TileName+'.bmp'+' .\' );

  writeln(GDALfile,'rem convert bitmap to GeoTiff to embed the lat/long coordinates');

  if (epsg = 4326) then begin
    Zoom_Suffix := format('_%d',[strtoint(ZoomLevel)+1]); // for geid, zoom level is 1 step higher
//    writeln(GDALfile,'set sourcebmp='+TileName+'_geid_combined\'+TileName+'_zoom'+Zoom_Suffix+'.bmp');
    writeln(GDALfile,'set sourcebmp='+TileName+'_combined\'+TileName+'_zoom'+Zoom_Suffix+'.bmp');
    writeln(GDALfile,'set FileName='+TileName+'.geid');
  end else begin // (epsg = 3857)
    Zoom_Suffix := '';
    writeln(GDALfile,'set sourcebmp='+TileName+'_combined\'+TileName+'.bmp');
    writeln(GDALfile,'set FileName='+TileName+'.umd');
    writeln(GDALfile,'if NOT exist %FileName% set FileName='+TileName+'.gmid');
  end;
  writeln(GDALfile,'if NOT exist %sourcebmp% (echo ERROR: %sourcebmp% NOT found & pause & exit /b 9)');
  writeln(GDALfile,'if NOT exist %FileName% (echo ERROR: %FileName% NOT found & pause & exit /b 9)');

  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "Left_Longitude_download'+Zoom_Suffix+'=" %FileName%'') do set real_left=%%a');
  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "Top_Latitude_download'+Zoom_Suffix+'=" %FileName%'') do set real_top=%%a');
  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "Right_Longitude_download'+Zoom_Suffix+'=" %FileName%'') do set real_right=%%a');
  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "Bottom_Latitude_download'+Zoom_Suffix+'=" %FileName%'') do set real_bottom=%%a');
  // trim spaces
//  writeln(GDALfile,'set real_left=%real_left: =%');
//  writeln(GDALfile,'set real_top=%real_top: =%');
//  writeln(GDALfile,'set real_right=%real_right: =%');
//  writeln(GDALfile,'set real_bottom=%real_bottom: =%');

//  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "MinX=" %FileName%'') do set MinX=%%a');
//  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "MinY=" %FileName%'') do set MinY=%%a');
//  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "MaxX=" %FileName%'') do set MaxX=%%a');
//  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "MaxY=" %FileName%'') do set MaxY=%%a');

  writeln(GDALfile,'set destinationtiff='+TileName+'\'+'bigmap.tif');
  writeln(GDALfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
  if (epsg = 4326) then begin
//    writeln(GDALfile,'set sourcebmp='+TileName+'_combined\'+TileName+'.bmp');
//    writeln(GDALfile,'set sourcetiff='+TileName+'_combined\'+TileName+'.tif');
    writeln(GDALfile,'gdal_translate -of Gtiff -a_ullr %real_left% %real_top% %real_right% %real_bottom% -a_srs EPSG:4326 %sourcebmp% %destinationtiff%');
  end else begin
    writeln(GDALfile,'rem convert EPSG:4326 coords to EPSG:3857');
    writeln(GDALfile,'rem convert lat/long coordinates to meters');
    writeln(GDALfile,'(echo %real_left% %real_top%)>Coord_In.txt');
    writeln(GDALfile,'(echo %real_right% %real_bottom%)>>Coord_In.txt');
    writeln(GDALfile,'gdaltransform -s_srs EPSG:4326 -t_srs EPSG:3857 <Coord_In.txt >Coord_Out.txt');
    writeln(GDALfile,'rem Get file contents and store them per variable ex: var1, var2, var3, var4');
    writeln(GDALfile,'set VarList=0');
    writeln(GDALfile,'for /F "tokens=1,2" %%A in (Coord_Out.txt) do (');
    writeln(GDALfile,'    SET /A VarList=!VarList! + 1');
    writeln(GDALfile,'    set "var!VarList!=%%A"');
    writeln(GDALfile,'    SET /A VarList=!VarList! + 1');
    writeln(GDALfile,'    set "var!VarList!=%%B"');
    writeln(GDALfile,')');

//    writeln(GDALfile,'set sourcebmp='+TileName+'_combined\'+TileName+'.bmp');
//    writeln(GDALfile,'set sourcetiff='+TileName+'_combined\'+TileName+'.tif');
//    writeln(GDALfile,'set destinationtiff='+TileName+'\'+'bigmap.tif');

    writeln(GDALfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
//    writeln(GDALfile,'gdal_translate -of Gtiff -co tfw=yes -a_ullr %var1% %var2% %var3% %var4% -a_srs EPSG:3857 %sourcebmp% %destinationtiff%');
    writeln(GDALfile,'gdal_translate -of Gtiff -a_ullr %var1% %var2% %var3% %var4% -a_srs EPSG:3857 %sourcebmp% %destinationtiff%');
  end;

  writeln(GDALfile,'del %sourcebmp%');
//  writeln(GDALfile,'if exist %sourcetiff% del %sourcetiff%');

  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - Legacy_Offset;
  Tile_T_Lat  := Tile_B_Lat + 23040;
  Tile_L_Long := UTM_Right + Legacy_Offset - TileList[TileIndex+1].TileUTMRight;
  Tile_R_Long := Tile_L_Long + 23040;

  writeln(GDALfile,'rem crop to UTM coordinates');
//  writeln(GDALfile,'set utm_zone='+UTM_Zone);
  writeln(GDALfile,format('set utm_zone=%d',[UTM_Zone]));
  if (UTM_ZoneNS = 'N') then begin
    writeln(GDALfile,'set utm_grid=north');
  end else begin
    writeln(GDALfile,'set utm_grid=south');
  end;
  writeln(GDALfile,format('set utm_wanted_left=%1.1f',[Tile_L_Long]));
  writeln(GDALfile,format('set utm_wanted_bottom=%1.1f',[Tile_B_Lat]));
  writeln(GDALfile,format('set utm_wanted_right=%1.1f',[Tile_R_Long]));
  writeln(GDALfile,format('set utm_wanted_top=%1.1f',[Tile_T_Lat]));

 if (DetectTree) then begin
  writeln(GDALfile,'rem re-size for forest tile size');
  writeln(GDALfile,'set image_width=2048');
  writeln(GDALfile,'set image_height=2048');
 end else begin
  writeln(GDALfile,'rem re-size');
  writeln(GDALfile,'set image_width='+OutputTileSize);
  writeln(GDALfile,'set image_height='+OutputTileSize);
 end;

  writeln(GDALfile,'set sourcetiff='+TileName+'\'+'bigmap.tif');
//  writeln(GDALfile,'set destinationtiff='+TileName+'\'+'UTMmap.tif');
  writeln(GDALfile,'set destinationtiff='+TileName+'\'+TileName+'.tif');

  writeln(GDALfile,'rem convert, with cropping, and re-sizing');
  writeln(GDALfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
//  writeln(GDALfile,'gdalwarp.exe -r lanczos -of GTiff -s_srs EPSG:3857 -t_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -ts %image_width% %image_height% -te %utm_wanted_left% %utm_wanted_bottom% %utm_wanted_right% %utm_wanted_top% %sourcetiff% %destinationtiff%');
  writeln(GDALfile,'gdalwarp.exe -r lanczos -of GTiff -t_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -ts %image_width% %image_height% -te %utm_wanted_left% %utm_wanted_bottom% %utm_wanted_right% %utm_wanted_top% %sourcetiff% %destinationtiff%');
  writeln(GDALfile,'del %sourcetiff%');

 if (DetectTree) then begin
  writeln(GDALfile,'move %destinationTIFF% ' + File_Destination + '_DetectTree');
 end else begin
  writeln(GDALfile,'rem convert to bitmap');
//  writeln(GDALfile,'set sourcetiff='+TileName+'\'+'UTMmap.tif');
  writeln(GDALfile,'set sourcetiff='+TileName+'\'+TileName+'.tif');
  writeln(GDALfile,'set destinationbmp='+TileName+'.bmp');
  writeln(GDALfile,'gdal_translate -of BMP %sourcetiff% %destinationbmp%');
  writeln(GDALfile,'del %sourcetiff%');
  writeln(GDALfile,'move %destinationbmp% ' + File_Destination);
 end;
  writeln(GDALfile,'endlocal');

  // close the file
  Close(GDALfile);

  MessageShow(FileName+' done.');
end;

// more accurate version to avoid latitude distortion
// based on EPSG:3857 TIFF file, but
// right now it is based on EPSG:4326 TIFF file which is not accurate
//-------------------------------------------------------------------------------------
Procedure MakeGDALbatchTiffFile(TileIndex : integer);

var
//  i : integer;
  FileName : string;
  FilePath : string;

  Tile_B_Lat  : double;
  Tile_T_Lat  : double;
  Tile_L_Long : double;
  Tile_R_Long : double;

begin
  FilePath := GDALfolder +'\SourceTiles\'+ TileList[TileIndex].TileName;
  ForceDirectories(FilePath);

  //open the file
  FileName := 'GDAL_'+TileList[TileIndex].TileName+'_TIF.bat';
  AssignFile(GDALfile, FilePath +'\'+ FileName);
  Rewrite(GDALfile);

  writeln(GDALfile,'@echo off');
  writeln(GDALfile,'setlocal');
  writeln(GDALfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
  writeln(GDALfile,'set GDAL_DATA='+GDALlibraryFolder+'\..\share\epsg_csv');
  // suppres generation of .xml file
  writeln(GDALfile,'set GDAL_PAM_ENABLED=NO');
  writeln(GDALfile,'rem goto directory where batch file is');
  writeln(GDALfile,'cd /d %~dp0');

// for old sceneries
//  // if file in a separate combined subfolder move it into the main folder
//  writeln(GDALfile,'move '+TileList[TileIndex].TileName+'_combined\'+TileList[TileIndex].TileName+'.tif'+' .\' );

  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - Legacy_Offset;
  Tile_T_Lat  := Tile_B_Lat + 23040;
  Tile_L_Long := UTM_Right + Legacy_Offset - TileList[TileIndex+1].TileUTMRight;
  Tile_R_Long := Tile_L_Long + 23040;

  writeln(GDALfile,'rem crop to UTM coordinates');
//  writeln(GDALfile,'set utm_zone='+UTM_Zone);
  writeln(GDALfile,format('set utm_zone=%d',[UTM_Zone]));
  if (UTM_ZoneNS = 'N') then begin
    writeln(GDALfile,'set utm_grid=north');
  end else begin
    writeln(GDALfile,'set utm_grid=south');
  end;
  writeln(GDALfile,format('set utm_wanted_left=%1.1f',[Tile_L_Long]));
  writeln(GDALfile,format('set utm_wanted_bottom=%1.1f',[Tile_B_Lat]));
  writeln(GDALfile,format('set utm_wanted_right=%1.1f',[Tile_R_Long]));
  writeln(GDALfile,format('set utm_wanted_top=%1.1f',[Tile_T_Lat]));

  writeln(GDALfile,'rem re-size');
  writeln(GDALfile,'set image_width='+OutputTileSize);
  writeln(GDALfile,'set image_height='+OutputTileSize);

  writeln(GDALfile,'set sourcebmp='+TileList[TileIndex].TileName+'_combined\'+TileList[TileIndex].TileName+'.bmp');
  writeln(GDALfile,'set sourcetiff='+TileList[TileIndex].TileName+'_combined\'+TileList[TileIndex].TileName+'.tif');
  writeln(GDALfile,'set destinationtiff='+TileList[TileIndex].TileName+'\'+'UTMmap.tif');

  writeln(GDALfile,'rem convert, with cropping, and re-sizing');
  writeln(GDALfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
//  writeln(GDALfile,'gdalwarp.exe -r lanczos -of GTiff -s_srs EPSG:3857 -t_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -ts %image_width% %image_height% -te %utm_wanted_left% %utm_wanted_bottom% %utm_wanted_right% %utm_wanted_top% %sourcetiff% %destinationtiff%');
  writeln(GDALfile,'gdalwarp.exe -r lanczos -of GTiff -s_srs EPSG:4326 -t_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -ts %image_width% %image_height% -te %utm_wanted_left% %utm_wanted_bottom% %utm_wanted_right% %utm_wanted_top% %sourcetiff% %destinationtiff%');
  writeln(GDALfile,'del %sourcetiff%');
  writeln(GDALfile,'if exist %sourcebmp% del %sourcebmp%');

  writeln(GDALfile,'rem convert to bitmap');
  writeln(GDALfile,'set sourcetiff='+TileList[TileIndex].TileName+'\'+'UTMmap.tif');
  writeln(GDALfile,'set destinationbmp='+TileList[TileIndex].TileName+'.bmp');
  writeln(GDALfile,'gdal_translate -of BMP %sourcetiff% %destinationbmp%');
  writeln(GDALfile,'del %sourcetiff%');
  writeln(GDALfile,'move %destinationbmp% ' + File_Destination);

  writeln(GDALfile,'endlocal');

  // close the file
  Close(GDALfile);

  MessageShow(FileName+' done.');
end;

// 4326 google-earth manual screen captur
//-------------------------------------------------------------------------------------
Procedure MakeGDALquarterTile_MC(CurrentRow, CurrentColumn, offset_Row, offset_Column : Integer);

var
//  i : integer;
  FileName : string;
  FilePath : string;

  TileIndex : integer;
  TileName : string;
  TextureName : string;
  Tile_B_Lat  : double;
  Tile_T_Lat  : double;
  Tile_L_Long : double;
  Tile_R_Long : double;

begin
  TileIndex := CurrentRow * (TileColumnCount+1) + CurrentColumn;

  FilePath := GDALfolder +'\SourceTiles\'+ TileList[TileIndex].TileName +'\QuarterTiles';
  // create path
  ForceDirectories(FilePath);

  TileName := TileList[TileIndex].TileName+format('_%2.2d_%2.2d',[offset_Column,offset_Row]);
//  TextureName := format('t%2.2d%2.2d',[CurrentColumn*4+offset_Column,CurrentRow*4+offset_Row]);
  TextureName := 't'+MakeTileName(CurrentColumn*4+offset_Column,CurrentRow*4+offset_Row, TileNameMode);
  //open the file
  FileName := 'GDAL_'+TileName+'_MC.bat';
  AssignFile(GDALfile, FilePath +'\'+ FileName);
  Rewrite(GDALfile);

  writeln(GDALfile,'@echo off');
  writeln(GDALfile,'setlocal');
  writeln(GDALfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
  writeln(GDALfile,'rem goto directory where batch file is');
  writeln(GDALfile,'cd /d %~dp0');

  writeln(GDALfile,'rem convert bitmap to GeoTiff to embed the lat/long coordinates');
  writeln(GDALfile,format('set real_left=%1.8f',[SourceLeftLongitude]));
  writeln(GDALfile,format('set real_top=%1.8f',[SourceTopLatitude]));
  writeln(GDALfile,format('set real_right=%1.8f',[SourceRightLongitude]));
  writeln(GDALfile,format('set real_bottom=%1.8f',[SourceBottomLatitude]));
  writeln(GDALfile,format('set image_width=%d',[BitmapWidth]));
  writeln(GDALfile,format('set image_height=%d',[BitmapHeight]));
  writeln(GDALfile,'set sourcebmp='+TileName+'.bmp');
  writeln(GDALfile,'set destinationtiff='+'bigmap.tif');

  writeln(GDALfile,'gdal_translate -gcp 0 0 %real_left% %real_top% -gcp %image_width% 0 %real_right% %real_top% -gcp %image_width% %image_height% %real_right% %real_bottom% -gcp 0 %image_height% %real_left% %real_bottom% %sourcebmp% %destinationtiff%');
  writeln(GDALfile,'rem del %sourcebmp%');

//  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - Resolution/2
  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - Legacy_Offset
    + offset_Row*Resolution*tRows/4;
  Tile_T_Lat  := Tile_B_Lat + 23040 /4;
//  Tile_R_Long  := UTM_Right +Resolution/2 - TileList[TileIndex].TileUTMRight
  Tile_R_Long  := UTM_Right + Legacy_Offset - TileList[TileIndex].TileUTMRight
   - offset_Column*Resolution*tColumns/4;
  Tile_L_Long  := Tile_R_Long - 23040 /4;

  writeln(GDALfile,'rem crop to UTM coordinates');
//  writeln(GDALfile,'set utm_zone='+UTM_Zone);
  writeln(GDALfile,format('set utm_zone=%d',[UTM_Zone]));
  if (UTM_ZoneNS = 'N') then begin
    writeln(GDALfile,'set utm_grid=north');
  end else begin
    writeln(GDALfile,'set utm_grid=south');
  end;
  writeln(GDALfile,format('set utm_wanted_left=%1.1f',[Tile_L_Long]));
  writeln(GDALfile,format('set utm_wanted_bottom=%1.1f',[Tile_B_Lat]));
  writeln(GDALfile,format('set utm_wanted_right=%1.1f',[Tile_R_Long]));
  writeln(GDALfile,format('set utm_wanted_top=%1.1f',[Tile_T_Lat]));

  writeln(GDALfile,'rem re-size');
  writeln(GDALfile,'set image_width='+OutputTileSize);
  writeln(GDALfile,'set image_height='+OutputTileSize);

  writeln(GDALfile,'set sourcetiff='+'bigmap.tif');
  writeln(GDALfile,'set destinationtiff='+'UTMmap.tif');

  writeln(GDALfile,'rem convert, with cropping, and re-sizing');
  writeln(GDALfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
  writeln(GDALfile,'gdalwarp.exe -r lanczos -of GTiff -s_srs WGS84 -t_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -tps -ts %image_width% %image_height% -te %utm_wanted_left% %utm_wanted_bottom% %utm_wanted_right% %utm_wanted_top% %sourcetiff% %destinationtiff%');
  writeln(GDALfile,'del %sourcetiff%');

  writeln(GDALfile,'rem convert to bitmap');
  writeln(GDALfile,'set sourcetiff='+'UTMmap.tif');
  writeln(GDALfile,'set destinationbmp='+TextureName+'.bmp');
  writeln(GDALfile,'gdal_translate -of BMP %sourcetiff% %destinationbmp%');
  writeln(GDALfile,'del %sourcetiff%');
  writeln(GDALfile,'rem move %destinationbmp% ' + File_Destination);

  writeln(GDALfile,'endlocal');

  // close the file
  Close(GDALfile);

  MessageShow(FileName+' done.');
end;

// 3857 for gmid and 4326 for geid
//-------------------------------------------------------------------------------------
Procedure MakeAutoGDALquarterTile(epsg : Integer; CurrentRow, CurrentColumn, offset_Row, offset_Column : Integer);

var
//  i : integer;
  FileName : string;
  FilePath : string;
  Zoom_Suffix : string;

  TileIndex : integer;
  TileName : string;
  TextureName : string;
  Tile_B_Lat  : double;
  Tile_T_Lat  : double;
  Tile_L_Long : double;
  Tile_R_Long : double;

begin
  TileIndex := CurrentRow * (TileColumnCount+1) + CurrentColumn;

  FilePath := GDALfolder +'\SourceTiles\'+ TileList[TileIndex].TileName +'\QuarterTiles';
  // create path
  ForceDirectories(FilePath);

  TileName := TileList[TileIndex].TileName+format('_%2.2d_%2.2d',[offset_Column,offset_Row]);
//  TextureName := format('t%2.2d%2.2d',[CurrentColumn*4+offset_Column,CurrentRow*4+offset_Row]);
  TextureName := 't'+MakeTileName(CurrentColumn*4+offset_Column,CurrentRow*4+offset_Row, TileNameMode);
  //open the file
//  FileName := 'GDAL_'+TileName+'.bat';
  FileName := format('GDAL_%s_%d.bat',[TileName,epsg]);
  AssignFile(GDALfile, FilePath +'\'+ FileName);
  Rewrite(GDALfile);

  writeln(GDALfile,'@echo off');
  writeln(GDALfile,'rem Enable Delayed Expansion for !Variables! ');
  writeln(GDALfile,'setlocal EnableDelayedExpansion');
  writeln(GDALfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
  writeln(GDALfile,'set GDAL_DATA='+GDALlibraryFolder+'\..\share\epsg_csv');
  // suppres generation of .xml file
  writeln(GDALfile,'set GDAL_PAM_ENABLED=NO');
  writeln(GDALfile,'rem goto directory where batch file is');
  writeln(GDALfile,'cd /d %~dp0');

  writeln(GDALfile,'rem convert bitmap to GeoTiff to embed the lat/long coordinates');

  if (epsg = 4326) then begin
    Zoom_Suffix := format('_%d',[strtoint(ZoomLevel)+1]); // for geid, zoom level is 1 step higher
//    writeln(GDALfile,'set sourcebmp='+TileName+'_geid_combined\'+TileName+'_zoom'+Zoom_Suffix+'.bmp');
    writeln(GDALfile,'set sourcebmp='+TileName+'_combined\'+TileName+'_zoom'+Zoom_Suffix+'.bmp');
    writeln(GDALfile,'set FileName='+TileName+'.geid');
  end else begin // (epsg = 3857)
    Zoom_Suffix := '';
    writeln(GDALfile,'set sourcebmp='+TileName+'_combined\'+TileName+'.bmp');
    writeln(GDALfile,'set FileName='+TileName+'.umd');
    writeln(GDALfile,'if NOT exist %FileName% set FileName='+TileName+'.gmid');
  end;
  writeln(GDALfile,'if NOT exist %FileName% (echo ERROR: %FileName% NOT found & pause & exit /b 9)');

  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "Left_Longitude_download'+Zoom_Suffix+'=" %FileName%'') do set real_left=%%a');
  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "Top_Latitude_download'+Zoom_Suffix+'=" %FileName%'') do set real_top=%%a');
  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "Right_Longitude_download'+Zoom_Suffix+'=" %FileName%'') do set real_right=%%a');
  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "Bottom_Latitude_download'+Zoom_Suffix+'=" %FileName%'') do set real_bottom=%%a');
  // trim spaces
//  writeln(GDALfile,'set real_left=%real_left: =%');
//  writeln(GDALfile,'set real_top=%real_top: =%');
//  writeln(GDALfile,'set real_right=%real_right: =%');
//  writeln(GDALfile,'set real_bottom=%real_bottom: =%');

  writeln(GDALfile,'set destinationtiff='+'bigmap.tif');
  writeln(GDALfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present

  if (epsg = 4326) then begin
    writeln(GDALfile,'gdal_translate -of Gtiff -a_ullr %real_left% %real_top% %real_right% %real_bottom% -a_srs EPSG:4326 %sourcebmp% %destinationtiff%');
  end else begin // (epsg = 3857)
    // convert coordinates
    writeln(GDALfile,'rem convert EPSG:4326 coords to EPSG:3857');
    writeln(GDALfile,'rem convert lat/long coordinates to meters');
    writeln(GDALfile,'(echo %real_left% %real_top%)>Coord_In.txt');
    writeln(GDALfile,'(echo %real_right% %real_bottom%)>>Coord_In.txt');
    writeln(GDALfile,'gdaltransform -s_srs EPSG:4326 -t_srs EPSG:3857 <Coord_In.txt >Coord_Out.txt');
    writeln(GDALfile,'rem Get file contents and store them per variable ex: var1, var2, var3, var4');
    writeln(GDALfile,'set VarList=0');
    writeln(GDALfile,'for /F "tokens=1,2" %%A in (Coord_Out.txt) do (');
    writeln(GDALfile,'    SET /A VarList=!VarList! + 1');
    writeln(GDALfile,'    set "var!VarList!=%%A"');
    writeln(GDALfile,'    SET /A VarList=!VarList! + 1');
    writeln(GDALfile,'    set "var!VarList!=%%B"');
    writeln(GDALfile,')');

    writeln(GDALfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
    writeln(GDALfile,'gdal_translate -of Gtiff -a_ullr %var1% %var2% %var3% %var4% -a_srs EPSG:3857 %sourcebmp% %destinationtiff%');
  end;
  writeln(GDALfile,'rem del %sourcebmp%');

//  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - Resolution/2
  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - Legacy_Offset
    + offset_Row*Resolution*tRows/4;
  Tile_T_Lat  := Tile_B_Lat + 23040 /4;
//  Tile_R_Long  := UTM_Right +Resolution/2 - TileList[TileIndex].TileUTMRight
  Tile_R_Long  := UTM_Right + Legacy_Offset - TileList[TileIndex].TileUTMRight
   - offset_Column*Resolution*tColumns/4;
  Tile_L_Long  := Tile_R_Long - 23040 /4;

  writeln(GDALfile,'rem crop to UTM coordinates');
//  writeln(GDALfile,'set utm_zone='+UTM_Zone);
  writeln(GDALfile,format('set utm_zone=%d',[UTM_Zone]));
  if (UTM_ZoneNS = 'N') then begin
    writeln(GDALfile,'set utm_grid=north');
  end else begin
    writeln(GDALfile,'set utm_grid=south');
  end;
  writeln(GDALfile,format('set utm_wanted_left=%1.1f',[Tile_L_Long]));
  writeln(GDALfile,format('set utm_wanted_bottom=%1.1f',[Tile_B_Lat]));
  writeln(GDALfile,format('set utm_wanted_right=%1.1f',[Tile_R_Long]));
  writeln(GDALfile,format('set utm_wanted_top=%1.1f',[Tile_T_Lat]));

  writeln(GDALfile,'rem re-size');
  writeln(GDALfile,'set image_width='+OutputTileSize);
  writeln(GDALfile,'set image_height='+OutputTileSize);

  writeln(GDALfile,'set sourcetiff='+'bigmap.tif');
  writeln(GDALfile,'set destinationtiff='+'UTMmap.tif');

  writeln(GDALfile,'rem convert, with cropping, and re-sizing');
  writeln(GDALfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
//  writeln(GDALfile,'gdalwarp.exe -r lanczos -of GTiff -s_srs WGS84 -t_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -tps -ts %image_width% %image_height% -te %utm_wanted_left% %utm_wanted_bottom% %utm_wanted_right% %utm_wanted_top% %sourcetiff% %destinationtiff%');
  writeln(GDALfile,'gdalwarp.exe -r lanczos -of GTiff -t_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -ts %image_width% %image_height% -te %utm_wanted_left% %utm_wanted_bottom% %utm_wanted_right% %utm_wanted_top% %sourcetiff% %destinationtiff%');
  writeln(GDALfile,'del %sourcetiff%');

  writeln(GDALfile,'rem convert to bitmap');
  writeln(GDALfile,'set sourcetiff='+'UTMmap.tif');
  writeln(GDALfile,'set destinationbmp='+TextureName+'.bmp');
  writeln(GDALfile,'gdal_translate -of BMP %sourcetiff% %destinationbmp%');
  writeln(GDALfile,'del %sourcetiff%');
  writeln(GDALfile,'rem move %destinationbmp% ' + File_Destination);

  writeln(GDALfile,'endlocal');

  // close the file
  Close(GDALfile);

  MessageShow(FileName+' done.');
end;

// 2^(n-1)
//-------------------------------------------------------------------------------------
function xConvertNumMips(BitmapWidth : integer) : string;
begin
  case BitmapWidth of
    256: begin
      result := '9';
    end;
    512: begin
      result := '10';
    end;
    1024: begin
      result := '11';
    end;
    2048: begin
      result := '12';
    end;
    4096: begin
      result := '13';
    end;
    8192: begin
      result := '14';
    end;
    else begin
      result := '1';
    end;
  end;
end;

//-------------------------------------------------------------------------------------
function ConvertNumMips(BitmapWidth : integer) : string;
begin
  result := IntToStr(round(log2(BitmapWidth)) + 1);
end;

//-------------------------------------------------------------------------------------
Procedure MakeDDSbatchFile(TileIndex : integer);

var
  TileName : string;
  ErrorCode : integer;
  TileRow, TileColumn : integer;
  i, j : integer;
//  NumMips : string;
  FileName : string;
  FilePath : string;
  SourcePath : string;
//  Algo : string;  // nmip image shrink algorithm

begin
  TileName := TileList[TileIndex].TileName;
  FilePath := GDALfolder +'\SourceTiles\'+ TileName;
  ForceDirectories(FilePath);

  //open the file
  FileName := 'DDS_'+TileName+'.bat';
  AssignFile(GDALfile, FilePath +'\'+ FileName);
  Rewrite(GDALfile);

  writeln(GDALfile,'@echo off');
  writeln(GDALfile,'setlocal');
  writeln(GDALfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
//  writeln(GDALfile,'set PATH=%PATH%;"'+CondorFolder+'\Tools"');
  writeln(GDALfile,'set PATH=%PATH%;"'+CompressorFolder+'"');
//set PATH=%PATH%;"C:\Compressonator_4.1.5083\bin\CLI"
  writeln(GDALfile,'rem goto directory where batch file is');
  writeln(GDALfile,'cd /d %~dp0');

  SourcePath := '..\..\Terragen\Textures\';
//  writeln(GDALfile,'set sourcebmp='+SourcePath+TileName+'.bmp');
  writeln(GDALfile,'set sourcebmp='+SourcePath+TileName);
  writeln(GDALfile,'if exist %sourcebmp%.tif (set fext=.tif) else (set fext=.bmp)');
  writeln(GDALfile,'set sourcebmp=%sourcebmp%%fext%');
{
  // read bitmap size
  BMPfolder := FilePath;
  BitmapWidth := 1; // dummy to for manual insert later.
  Bitmap_GetWidthHeight(SourcePath+TileName+'.bmp');
  if (BitmapSuccess) then begin
    BitmapWidth := BitmapWidth div 4;
  end else begin
    BitmapWidth := strtoint(OutputTileSize) div 4;
  end;
}
  // find bitmap size
  writeln(GDALfile,'gdalinfo -nomd -norat -noct %sourcebmp% >info.txt');
  writeln(GDALfile,'set FileName=info.txt');
  writeln(GDALfile,'for /f "tokens=2  delims=," %%a in (''find "Size is " %FileName%'') do set bSize=%%a');
  writeln(GDALfile,'set /a BitmapWidth=bSize/4');
  writeln(GDALfile,'set /a b0=bSize/4*0');
  writeln(GDALfile,'set /a b1=bSize/4*1');
  writeln(GDALfile,'set /a b2=bSize/4*2');
  writeln(GDALfile,'set /a b3=bSize/4*3');

  // use -triangle for 16k bitmap as -cubic will fail
  writeln(GDALfile,'set algo=-cubic');
  writeln(GDALfile,'if %bSize% EQU 16384 set algo=-triangle');

{
  // convert size to NumMips
//  NumMips := 'nmips';
  NumMips := ConvertNumMips(BitmapWidth);
 // if not specified, nvdxt defaults to max, so no need
}
  GetTileIndex(TileName,TileColumn, TileRow);
//  Val(copy(TileName,1,2),TileColumn,ErrorCode);
//  Val(copy(TileName,3,2),TileRow,ErrorCode);
// if errorcode or not in range -> error

  writeln(GDALfile,'mkdir TEMP');

  for i := 0 to 4-1 do begin
    for j := 0 to 4-1 do begin
//      TileName := format('t%2.2d%2.2d',[TileColumn*4+i,TileRow*4+j]);
      TileName := 't'+MakeTileName(TileColumn*4+i,TileRow*4+j, TileNameMode);
      writeln(GDALfile,'set destinationbmp='+'TEMP\'+TileName+'%fext%');
      writeln(GDALfile,'set destinationdds='+'TEMP\'+TileName+'.dds');
//      writeln(GDALfile,'gdal_translate -of BMP -srcwin '+
      writeln(GDALfile,'gdal_translate -srcwin '+
//       inttostr((3-i)*BitmapWidth) +' '+ inttostr((3-j)*BitmapWidth) +' '+
//       inttostr(BitmapWidth) +' '+ inttostr(BitmapWidth) +' '+
        format('%%b%d%% %%b%d%% %%BitmapWidth%% %%BitmapWidth%% ',[3-i,3-j]) +
        '%sourcebmp% %destinationbmp%');
//      writeln(GDALfile,'nvDXT.exe -quality_highest %algo% -dxt1c -outdir "TEMP" -file %destinationbmp%');
//      writeln(GDALfile,'nvDXT.exe -quality_highest %algo% -dxt3 -outdir "TEMP" -file %destinationbmp%');
      writeln(GDALfile,'CompressonatorCLI.exe -fd '+DXT_Type+' -mipsize 1 -CompressionSpeed 0 %destinationbmp% %destinationdds%');
//      writeln(GDALfile,'CompressonatorCLI.exe -fd '+DXT_Type+' -DXT1UseAlpha 1 -AlphaThreshold 192 -mipsize 1 -CompressionSpeed 0 %destinationbmp% %destinationdds%');
      writeln(GDALfile,'del %destinationbmp%');
      writeln(GDALfile,'move %destinationdds% ' + '..\..\..\Textures');
    end;
  end;

  writeln(GDALfile,'endlocal');

  // close the file
  Close(GDALfile);

  MessageShow(FileName+' done ('+DXT_Type+').');
end;

//-------------------------------------------------------------------------------------
Procedure MakeDDSquarterTile(CurrentRow, CurrentColumn, offset_Row, offset_Column : Integer);

var
//  i : integer;
  FileName : string;
  FilePath : string;
//  NumMips : string;
  TileIndex : integer;
  TileName : string;
  TextureName : string;
//  Tile_B_Lat : double;
//  Tile_T_Lat : double;
//  Tile_L_Long : double;
//  Tile_R_Long : double;

begin
  TileIndex := CurrentRow * (TileColumnCount+1) + CurrentColumn;

  FilePath := GDALfolder +'\SourceTiles\'+ TileList[TileIndex].TileName +'\QuarterTiles';
  // create path
  ForceDirectories(FilePath);

  TileName := TileList[TileIndex].TileName+format('_%2.2d_%2.2d',[offset_Column,offset_Row]);
//  TextureName := format('t%2.2d%2.2d',[CurrentColumn*4+offset_Column,CurrentRow*4+offset_Row]);
  TextureName := 't'+MakeTileName(CurrentColumn*4+offset_Column,CurrentRow*4+offset_Row, TileNameMode);
  //open the file
  FileName := 'DDS_'+TileName+'.bat';
  AssignFile(GDALfile, FilePath +'\'+ FileName);
  Rewrite(GDALfile);

  writeln(GDALfile,'@echo off');
  writeln(GDALfile,'setlocal');
//  writeln(GDALfile,'set PATH=%PATH%;"'+CondorFolder+'\Tools"');
  writeln(GDALfile,'set PATH=%PATH%;"'+CompressorFolder+'"');
  writeln(GDALfile,'rem goto directory where batch file is');
  writeln(GDALfile,'cd /d %~dp0');

  writeln(GDALfile,'rem converts .bmp file into .dds in "dds" folder');
{
//  // 8 mipmaps, but probably should use 2^(n-1). For 1024 n = 11, for 2048 n = 12
//  NumMips := '8';
  // read bitmap size
  BMPfolder := FilePath;
  NumMips := 'nmips';
  Bitmap_GetWidthHeight(TileName+'.bmp');
  if (BitmapSuccess) then begin
    // convert size to NumMips
    NumMips := ConvertNumMips(BitmapWidth);
  end;
} // if not specified, nvdxt defaults to max, so no need
//  writeln(GDALfile,'nvDXT.exe -quality_highest -nmips '+NumMips+' -Cubic -dxt1c -outdir "dds" -file ',TextureName,'.bmp');
//  writeln(GDALfile,'nvDXT.exe -quality_highest -nmips '+NumMips+' -Cubic -dxt3 -outdir "dds" -file ',TextureName,'.bmp');
//  writeln(GDALfile,'nvDXT.exe -quality_highest -Cubic -dxt3 -outdir "dds" -file ',TextureName,'.bmp');
  writeln(GDALfile,'CompressonatorCLI.exe -fd '+DXT_Type+' -mipsize 1 -CompressionSpeed 0 %destinationbmp% %destinationdds%');
//  writeln(GDALfile,'CompressonatorCLI.exe -fd '+DXT_Type+' -DXT1UseAlpha 1 -AlphaThreshold 192 -mipsize 1 -CompressionSpeed 0 %destinationbmp% %destinationdds%');
  writeln(GDALfile,'endlocal');

  // close the file
  Close(GDALfile);

  MessageShow(FileName+' done ('+DXT_Type+').');
end;

// embed EPSG:4326 coordinates - not accurate
//-------------------------------------------------------------------------------------
Procedure xMakeGDALoverallBatchFile;

var
//  i : integer;
  FileName : string;
  FilePath : string;

  Tile_B_Lat  : double;
  Tile_T_Lat  : double;
  Tile_L_Long : double;
  Tile_R_Long : double;

begin
  FilePath := GDALfolder +'\SourceTiles\Overall';
  ForceDirectories(FilePath);

  //open the file
  FileName := 'GDAL_Overall.bat';
  AssignFile(GDALfile, FilePath +'\'+ FileName);
  Rewrite(GDALfile);

  writeln(GDALfile,'@echo off');
  writeln(GDALfile,'setlocal');
  writeln(GDALfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
  writeln(GDALfile,'rem goto directory where batch file is');
  writeln(GDALfile,'cd /d %~dp0');

  writeln(GDALfile,'rem convert bitmap to GeoTiff to embed the lat/long coordinates');
  writeln(GDALfile,format('set real_left=%1.8f',[SourceLeftLongitude]));
  writeln(GDALfile,format('set real_top=%1.8f',[SourceTopLatitude]));
  writeln(GDALfile,format('set real_right=%1.8f',[SourceRightLongitude]));
  writeln(GDALfile,format('set real_bottom=%1.8f',[SourceBottomLatitude]));
  writeln(GDALfile,format('set image_width=%d',[BitmapWidth]));
  writeln(GDALfile,format('set image_height=%d',[BitmapHeight]));
  writeln(GDALfile,'set sourcebmp=Overall_combined\Overall.bmp');
  writeln(GDALfile,'set destinationtiff=Overall\bigmap.tif');

  writeln(GDALfile,'gdal_translate -gcp 0 0 %real_left% %real_top% -gcp %image_width% 0 %real_right% %real_top% -gcp %image_width% %image_height% %real_right% %real_bottom% -gcp 0 %image_height% %real_left% %real_bottom% %sourcebmp% %destinationtiff%');
  writeln(GDALfile,'del %sourcebmp%');

  Tile_B_Lat  := CornerList[0].TileUTMBottom + UTM_Bottom - Legacy_Offset;
//  Tile_T_Lat  := Tile_B_Lat + 23040 * TileRowCount;
  Tile_T_Lat  := Tile_B_Lat + Resolution * RowCount;
  Tile_L_Long := UTM_Right + Legacy_Offset - CornerList[1].TileUTMRight;
//  Tile_R_Long := Tile_L_Long + 23040 * TileColumnCount;
  Tile_R_Long := Tile_L_Long + Resolution * ColumnCount;

  writeln(GDALfile,'rem crop to UTM coordinates');
//  writeln(GDALfile,'set utm_zone='+UTM_Zone);
  writeln(GDALfile,format('set utm_zone=%d',[UTM_Zone]));
  if (UTM_ZoneNS = 'N') then begin
    writeln(GDALfile,'set utm_grid=north');
  end else begin
    writeln(GDALfile,'set utm_grid=south');
  end;
  writeln(GDALfile,format('set utm_wanted_left=%1.1f',[Tile_L_Long]));
  writeln(GDALfile,format('set utm_wanted_bottom=%1.1f',[Tile_B_Lat]));
  writeln(GDALfile,format('set utm_wanted_right=%1.1f',[Tile_R_Long]));
  writeln(GDALfile,format('set utm_wanted_top=%1.1f',[Tile_T_Lat]));

  writeln(GDALfile,'rem re-size');
//  writeln(GDALfile,format('set image_width=%d',[TileColumnCount*256]));
  writeln(GDALfile,format('set image_width=%d',[ColumnCount]));
//  writeln(GDALfile,format('set image_height=%d',[TileRowCount*256]));
  writeln(GDALfile,format('set image_height=%d',[RowCount]));

  writeln(GDALfile,'set sourcetiff=Overall\bigmap.tif');
  writeln(GDALfile,'set destinationtiff=Overall\UTMmap.tif');

  writeln(GDALfile,'rem convert, with cropping, and re-sizing');
  writeln(GDALfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
  writeln(GDALfile,'gdalwarp.exe -r lanczos -of GTiff -s_srs WGS84 -t_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -tps -ts %image_width% %image_height% -te %utm_wanted_left% %utm_wanted_bottom% %utm_wanted_right% %utm_wanted_top% %sourcetiff% %destinationtiff%');
  writeln(GDALfile,'del %sourcetiff%');

  writeln(GDALfile,'rem convert to bitmap');
  writeln(GDALfile,'set sourcetiff=Overall\UTMmap.tif');
  writeln(GDALfile,'set destinationbmp=Overall.bmp');
  writeln(GDALfile,'gdal_translate -of BMP %sourcetiff% %destinationbmp%');
  writeln(GDALfile,'del %sourcetiff%');
  writeln(GDALfile,'rem move %destinationbmp% ' + File_Destination);

  writeln(GDALfile,'endlocal');

  // close the file
  Close(GDALfile);

  MessageShow(FileName+' done.');
end;

// more accurate version for large area to avoid latitude distortion
// embed EPSG:3857 coordinates
//-------------------------------------------------------------------------------------
Procedure MakeGDALoverallBatchFile(Name : string);

var
//  i : integer;
  FileName : string;
  FilePath : string;

  Tile_B_Lat  : double;
  Tile_T_Lat  : double;
  Tile_L_Long : double;
  Tile_R_Long : double;

begin
  FilePath := GDALfolder +'\SourceTiles\'+Name;
  ForceDirectories(FilePath);

  //open the file
  FileName := 'GDAL_'+Name+'.bat';
  AssignFile(GDALfile, FilePath +'\'+ FileName);
  Rewrite(GDALfile);

  writeln(GDALfile,'@echo off');
  writeln(GDALfile,'rem Enable Delayed Expansion for !Variables! ');
  writeln(GDALfile,'setlocal EnableDelayedExpansion');
  writeln(GDALfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
  writeln(GDALfile,'set GDAL_DATA='+GDALlibraryFolder+'\..\share\epsg_csv');
  // suppres generation of .xml file
  writeln(GDALfile,'set GDAL_PAM_ENABLED=NO');
  writeln(GDALfile,'rem goto directory where batch file is');
  writeln(GDALfile,'cd /d %~dp0');

  writeln(GDALfile,'rem convert EPSG:4326 coords to EPSG:3857');
  writeln(GDALfile,format('set real_left=%1.8f',[SourceLeftLongitude]));
  writeln(GDALfile,format('set real_top=%1.8f',[SourceTopLatitude]));
  writeln(GDALfile,format('set real_right=%1.8f',[SourceRightLongitude]));
  writeln(GDALfile,format('set real_bottom=%1.8f',[SourceBottomLatitude]));
  writeln(GDALfile,'(echo %real_left% %real_top%)>Coord_In.txt');
  writeln(GDALfile,'(echo %real_right% %real_bottom%)>>Coord_In.txt');

  writeln(GDALfile,'rem convert lat/long coordinates to meters');
  writeln(GDALfile,'gdaltransform -s_srs EPSG:4326 -t_srs EPSG:3857 <Coord_In.txt >Coord_Out.txt');

  writeln(GDALfile,'rem Get file contents and store them per variable ex: var1, var2, var3, var4');
  writeln(GDALfile,'set VarList=0');
  writeln(GDALfile,'for /F "tokens=1,2" %%A in (Coord_Out.txt) do (');
  writeln(GDALfile,'    SET /A VarList=!VarList! + 1');
  writeln(GDALfile,'    set "var!VarList!=%%A"');
  writeln(GDALfile,'    SET /A VarList=!VarList! + 1');
  writeln(GDALfile,'    set "var!VarList!=%%B"');
  writeln(GDALfile,')');
//  writeln(GDALfile,'rem show variables');
//  writeln(GDALfile,'rem set var');

  writeln(GDALfile,'set sourcebmp='+Name+'_combined\'+Name+'.bmp');
  writeln(GDALfile,'set destinationtiff='+Name+'.tif');

  writeln(GDALfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
  writeln(GDALfile,'gdal_translate -of Gtiff -co tfw=yes -a_ullr %var1% %var2% %var3% %var4% -a_srs EPSG:3857 %sourcebmp% %destinationtiff%');
//  writeln(GDALfile,'rem show information');
//  writeln(GDALfile,'rem gdalinfo %destinationtiff%');
  writeln(GDALfile,'del %sourcebmp%');

  Tile_B_Lat  := CornerList[0].TileUTMBottom + UTM_Bottom - Legacy_Offset;
//  Tile_T_Lat  := Tile_B_Lat + 23040 * TileRowCount;
  Tile_T_Lat  := Tile_B_Lat + Resolution * RowCount;
  Tile_L_Long := UTM_Right + Legacy_Offset - CornerList[1].TileUTMRight;
//  Tile_R_Long := Tile_L_Long + 23040 * TileColumnCount;
  Tile_R_Long := Tile_L_Long + Resolution * ColumnCount;

  writeln(GDALfile,'rem crop to UTM coordinates');
//  writeln(GDALfile,'set utm_zone='+UTM_Zone);
  writeln(GDALfile,format('set utm_zone=%d',[UTM_Zone]));
  if (UTM_ZoneNS = 'N') then begin
    writeln(GDALfile,'set utm_grid=north');
  end else begin
    writeln(GDALfile,'set utm_grid=south');
  end;
  writeln(GDALfile,format('set utm_wanted_left=%1.1f',[Tile_L_Long]));
  writeln(GDALfile,format('set utm_wanted_bottom=%1.1f',[Tile_B_Lat]));
  writeln(GDALfile,format('set utm_wanted_right=%1.1f',[Tile_R_Long]));
  writeln(GDALfile,format('set utm_wanted_top=%1.1f',[Tile_T_Lat]));

  writeln(GDALfile,'rem re-size');
  writeln(GDALfile,format('set image_width=%d',[ColumnCount]));
  writeln(GDALfile,format('set image_height=%d',[RowCount]));

  writeln(GDALfile,'set sourcetiff='+Name+'.tif');
  writeln(GDALfile,'set destinationtiff=UTMmap.tif');

  writeln(GDALfile,'rem convert, with cropping, and re-sizing');
  writeln(GDALfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
  writeln(GDALfile,'gdalwarp.exe -of GTiff -s_srs EPSG:3857 -t_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -ts %image_width% %image_height% -te %utm_wanted_left% %utm_wanted_bottom% %utm_wanted_right% %utm_wanted_top% %sourcetiff% %destinationtiff%');
  writeln(GDALfile,'del %sourcetiff%');

  writeln(GDALfile,'rem convert to bitmap');
  writeln(GDALfile,'set sourcetiff=UTMmap.tif');
  writeln(GDALfile,'set destinationbmp='+Name+'.bmp');
  writeln(GDALfile,'gdal_translate -of BMP %sourcetiff% %destinationbmp%');
  writeln(GDALfile,'del %sourcetiff%');
  writeln(GDALfile,'rem move %destinationbmp% ' + File_Destination);

  writeln(GDALfile,'endlocal');

  // close the file
  Close(GDALfile);

  MessageShow(FileName+' done.');
end;

// more accurate version for large area to avoid latitude distortion
// embed EPSG:3857 coordinates
//-------------------------------------------------------------------------------------
Procedure MakeAutoGDALoverallBatchFile(Name : string);

var
//  i : integer;
  FileName : string;
  FilePath : string;

  Tile_B_Lat  : double;
  Tile_T_Lat  : double;
  Tile_L_Long : double;
  Tile_R_Long : double;

begin
  FilePath := GDALfolder +'\SourceTiles\'+Name;
  ForceDirectories(FilePath);

  //open the file
  FileName := 'GDAL_'+Name+'.bat';
  AssignFile(GDALfile, FilePath +'\'+ FileName);
  Rewrite(GDALfile);

  writeln(GDALfile,'@echo off');
  writeln(GDALfile,'rem Enable Delayed Expansion for !Variables! ');
  writeln(GDALfile,'setlocal EnableDelayedExpansion');
  writeln(GDALfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
  writeln(GDALfile,'set GDAL_DATA='+GDALlibraryFolder+'\..\share\epsg_csv');
  // suppres generation of .xml file
  writeln(GDALfile,'set GDAL_PAM_ENABLED=NO');
  writeln(GDALfile,'rem goto directory where batch file is');
  writeln(GDALfile,'cd /d %~dp0');

//  writeln(GDALfile,format('set real_left=%1.8f',[SourceLeftLongitude]));
//  writeln(GDALfile,format('set real_top=%1.8f',[SourceTopLatitude]));
//  writeln(GDALfile,format('set real_right=%1.8f',[SourceRightLongitude]));
//  writeln(GDALfile,format('set real_bottom=%1.8f',[SourceBottomLatitude]));

  writeln(GDALfile,'set FileName='+Name+'.umd');
  writeln(GDALfile,'if NOT exist %FileName% set FileName='+Name+'.gmid');
  writeln(GDALfile,'if NOT exist %FileName% (echo ERROR: %FileName% NOT found & pause & exit /b 9)');

  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "Left_Longitude_download=" %FileName%'') do set real_left=%%a');
  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "Top_Latitude_download=" %FileName%'') do set real_top=%%a');
  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "Right_Longitude_download=" %FileName%'') do set real_right=%%a');
  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "Bottom_Latitude_download=" %FileName%'') do set real_bottom=%%a');
  // trim spaces
//  writeln(GDALfile,'set real_left=%real_left: =%');
//  writeln(GDALfile,'set real_top=%real_top: =%');
//  writeln(GDALfile,'set real_right=%real_right: =%');
//  writeln(GDALfile,'set real_bottom=%real_bottom: =%');

  writeln(GDALfile,'rem convert EPSG:4326 coords to EPSG:3857');
  writeln(GDALfile,'rem convert lat/long coordinates to meters');
  writeln(GDALfile,'(echo %real_left% %real_top%)>Coord_In.txt');
  writeln(GDALfile,'(echo %real_right% %real_bottom%)>>Coord_In.txt');
  writeln(GDALfile,'gdaltransform -s_srs EPSG:4326 -t_srs EPSG:3857 <Coord_In.txt >Coord_Out.txt');
  writeln(GDALfile,'rem Get file contents and store them per variable ex: var1, var2, var3, var4');
  writeln(GDALfile,'set VarList=0');
  writeln(GDALfile,'for /F "tokens=1,2" %%A in (Coord_Out.txt) do (');
  writeln(GDALfile,'    SET /A VarList=!VarList! + 1');
  writeln(GDALfile,'    set "var!VarList!=%%A"');
  writeln(GDALfile,'    SET /A VarList=!VarList! + 1');
  writeln(GDALfile,'    set "var!VarList!=%%B"');
  writeln(GDALfile,')');
//  writeln(GDALfile,'rem show variables');
//  writeln(GDALfile,'rem set var');

  writeln(GDALfile,'set sourcebmp='+Name+'_combined\'+Name+'.bmp');
  writeln(GDALfile,'set destinationtiff='+Name+'.tif');

  writeln(GDALfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
  writeln(GDALfile,'gdal_translate -of Gtiff -co tfw=yes -a_ullr %var1% %var2% %var3% %var4% -a_srs EPSG:3857 %sourcebmp% %destinationtiff%');
//  writeln(GDALfile,'rem show information');
//  writeln(GDALfile,'rem gdalinfo %destinationtiff%');
  writeln(GDALfile,'del %sourcebmp%');

  Tile_B_Lat  := CornerList[0].TileUTMBottom + UTM_Bottom - Legacy_Offset;
//  Tile_T_Lat  := Tile_B_Lat + 23040 * TileRowCount;
  Tile_T_Lat  := Tile_B_Lat + Resolution * RowCount;
  Tile_L_Long := UTM_Right + Legacy_Offset - CornerList[1].TileUTMRight;
//  Tile_R_Long := Tile_L_Long + 23040 * TileColumnCount;
  Tile_R_Long := Tile_L_Long + Resolution * ColumnCount;

  writeln(GDALfile,'rem crop to UTM coordinates');
//  writeln(GDALfile,'set utm_zone='+UTM_Zone);
  writeln(GDALfile,format('set utm_zone=%d',[UTM_Zone]));
  if (UTM_ZoneNS = 'N') then begin
    writeln(GDALfile,'set utm_grid=north');
  end else begin
    writeln(GDALfile,'set utm_grid=south');
  end;
  writeln(GDALfile,format('set utm_wanted_left=%1.1f',[Tile_L_Long]));
  writeln(GDALfile,format('set utm_wanted_bottom=%1.1f',[Tile_B_Lat]));
  writeln(GDALfile,format('set utm_wanted_right=%1.1f',[Tile_R_Long]));
  writeln(GDALfile,format('set utm_wanted_top=%1.1f',[Tile_T_Lat]));

  writeln(GDALfile,'rem re-size');
  writeln(GDALfile,format('set image_width=%d',[ColumnCount]));
  writeln(GDALfile,format('set image_height=%d',[RowCount]));

  writeln(GDALfile,'set sourcetiff='+Name+'.tif');
  writeln(GDALfile,'set destinationtiff=UTMmap.tif');

  writeln(GDALfile,'rem convert, with cropping, and re-sizing');
  writeln(GDALfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
  writeln(GDALfile,'gdalwarp.exe -of GTiff -s_srs EPSG:3857 -t_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -ts %image_width% %image_height% -te %utm_wanted_left% %utm_wanted_bottom% %utm_wanted_right% %utm_wanted_top% %sourcetiff% %destinationtiff%');
  writeln(GDALfile,'del %sourcetiff%');

  writeln(GDALfile,'rem convert to bitmap');
  writeln(GDALfile,'set sourcetiff=UTMmap.tif');
  writeln(GDALfile,'set destinationbmp='+Name+'.bmp');
  writeln(GDALfile,'gdal_translate -of BMP %sourcetiff% %destinationbmp%');
  writeln(GDALfile,'del %sourcetiff%');
  writeln(GDALfile,'rem move %destinationbmp% ' + File_Destination);

  writeln(GDALfile,'endlocal');

  // close the file
  Close(GDALfile);

  MessageShow(FileName+' done.');
end;

// simpler - just embed EPSG:4326 coordinates, but really needs 3857 for accuracy
//-------------------------------------------------------------------------------------
Procedure xxMakeGDALoverallBatchFile(Name : string);

var
//  i : integer;
  FileName : string;
  FilePath : string;

  Tile_B_Lat  : double;
  Tile_T_Lat  : double;
  Tile_L_Long : double;
  Tile_R_Long : double;

begin
  FilePath := GDALfolder +'\SourceTiles\'+Name;
  ForceDirectories(FilePath);

  //open the file
  FileName := 'GDAL_'+Name+'.bat';
  AssignFile(GDALfile, FilePath +'\'+ FileName);
  Rewrite(GDALfile);

  writeln(GDALfile,'@echo off');
  writeln(GDALfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
  writeln(GDALfile,'set GDAL_DATA='+GDALlibraryFolder+'\..\share\epsg_csv');
  // suppres generation of .xml file
  writeln(GDALfile,'set GDAL_PAM_ENABLED=NO');

  writeln(GDALfile,'rem convert bitmap to GeoTiff to embed the EPSG:4326 lat/long coordinates');
  writeln(GDALfile,format('set real_left=%1.8f',[SourceLeftLongitude]));
  writeln(GDALfile,format('set real_top=%1.8f',[SourceTopLatitude]));
  writeln(GDALfile,format('set real_right=%1.8f',[SourceRightLongitude]));
  writeln(GDALfile,format('set real_bottom=%1.8f',[SourceBottomLatitude]));

  writeln(GDALfile,'set sourcebmp='+Name+'_combined\'+Name+'.bmp');
  writeln(GDALfile,'set destinationtiff='+Name+'.tif');

  writeln(GDALfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
  writeln(GDALfile,'gdal_translate -of GTiff -a_srs EPSG:4326 -a_ullr %real_left% %real_top% %real_right% %real_bottom% %sourcebmp% %destinationtiff%');
//  writeln(GDALfile,'rem show information');
//  writeln(GDALfile,'rem gdalinfo %destinationtiff%');
  writeln(GDALfile,'del %sourcebmp%');

  Tile_B_Lat  := CornerList[0].TileUTMBottom + UTM_Bottom - Legacy_Offset;
//  Tile_T_Lat  := Tile_B_Lat + 23040 * TileRowCount;
  Tile_T_Lat  := Tile_B_Lat + Resolution * RowCount;
  Tile_L_Long := UTM_Right + Legacy_Offset - CornerList[1].TileUTMRight;
//  Tile_R_Long := Tile_L_Long + 23040 * TileColumnCount;
  Tile_R_Long := Tile_L_Long + Resolution * ColumnCount;

  writeln(GDALfile,'rem crop to UTM coordinates');
//  writeln(GDALfile,'set utm_zone='+UTM_Zone);
  writeln(GDALfile,format('set utm_zone=%d',[UTM_Zone]));
  if (UTM_ZoneNS = 'N') then begin
    writeln(GDALfile,'set utm_grid=north');
  end else begin
    writeln(GDALfile,'set utm_grid=south');
  end;
  writeln(GDALfile,format('set utm_wanted_left=%1.1f',[Tile_L_Long]));
  writeln(GDALfile,format('set utm_wanted_bottom=%1.1f',[Tile_B_Lat]));
  writeln(GDALfile,format('set utm_wanted_right=%1.1f',[Tile_R_Long]));
  writeln(GDALfile,format('set utm_wanted_top=%1.1f',[Tile_T_Lat]));

  writeln(GDALfile,'rem re-size');
  writeln(GDALfile,format('set image_width=%d',[ColumnCount]));
  writeln(GDALfile,format('set image_height=%d',[RowCount]));

  writeln(GDALfile,'set sourcetiff='+Name+'.tif');
  writeln(GDALfile,'set destinationtiff=UTMmap.tif');

  writeln(GDALfile,'rem convert, with cropping, and re-sizing');
  writeln(GDALfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
  writeln(GDALfile,'gdalwarp.exe -of GTiff -s_srs EPSG:4326 -t_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -ts %image_width% %image_height% -te %utm_wanted_left% %utm_wanted_bottom% %utm_wanted_right% %utm_wanted_top% %sourcetiff% %destinationtiff%');
  writeln(GDALfile,'del %sourcetiff%');

  writeln(GDALfile,'rem convert to bitmap');
  writeln(GDALfile,'set sourcetiff=UTMmap.tif');
  writeln(GDALfile,'set destinationbmp='+Name+'.bmp');
  writeln(GDALfile,'gdal_translate -of BMP %sourcetiff% %destinationbmp%');
  writeln(GDALfile,'del %sourcetiff%');
  writeln(GDALfile,'rem move %destinationbmp% ' + File_Destination);

  writeln(GDALfile,'endlocal');

  // close the file
  Close(GDALfile);

  MessageShow(FileName+' done.');
end;

// based on EPSG:4326 TIF file, but really needs 3857 for accuracy
//-------------------------------------------------------------------------------------
Procedure MakeGDALoverallTiffBatchFile(Name : string);

var
//  i : integer;
  FileName : string;
  FilePath : string;

  Tile_B_Lat  : double;
  Tile_T_Lat  : double;
  Tile_L_Long : double;
  Tile_R_Long : double;

begin
  FilePath := GDALfolder +'\SourceTiles\'+Name;
  ForceDirectories(FilePath);

  //open the file
  FileName := 'GDAL_'+Name+'_TIF.bat';
  AssignFile(GDALfile, FilePath +'\'+ FileName);
  Rewrite(GDALfile);

  writeln(GDALfile,'@echo off');
//  writeln(GDALfile,'rem Enable Delayed Expansion for !Variables! ');
//  writeln(GDALfile,'setlocal EnableDelayedExpansion');
  writeln(GDALfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
  writeln(GDALfile,'set GDAL_DATA='+GDALlibraryFolder+'\..\share\epsg_csv');
  // suppres generation of .xml file
  writeln(GDALfile,'set GDAL_PAM_ENABLED=NO');
  writeln(GDALfile,'rem goto directory where batch file is');
  writeln(GDALfile,'cd /d %~dp0');

  Tile_B_Lat  := CornerList[0].TileUTMBottom + UTM_Bottom - Legacy_Offset;
//  Tile_T_Lat  := Tile_B_Lat + 23040 * TileRowCount;
  Tile_T_Lat  := Tile_B_Lat + Resolution * RowCount;
  Tile_L_Long := UTM_Right + Legacy_Offset - CornerList[1].TileUTMRight;
//  Tile_R_Long := Tile_L_Long + 23040 * TileColumnCount;
  Tile_R_Long := Tile_L_Long + Resolution * ColumnCount;

  writeln(GDALfile,'rem crop to UTM coordinates');
//  writeln(GDALfile,'set utm_zone='+UTM_Zone);
  writeln(GDALfile,format('set utm_zone=%d',[UTM_Zone]));
  if (UTM_ZoneNS = 'N') then begin
    writeln(GDALfile,'set utm_grid=north');
  end else begin
    writeln(GDALfile,'set utm_grid=south');
  end;
  writeln(GDALfile,format('set utm_wanted_left=%1.1f',[Tile_L_Long]));
  writeln(GDALfile,format('set utm_wanted_bottom=%1.1f',[Tile_B_Lat]));
  writeln(GDALfile,format('set utm_wanted_right=%1.1f',[Tile_R_Long]));
  writeln(GDALfile,format('set utm_wanted_top=%1.1f',[Tile_T_Lat]));

  writeln(GDALfile,'rem re-size');
  writeln(GDALfile,format('set image_width=%d',[ColumnCount]));
  writeln(GDALfile,format('set image_height=%d',[RowCount]));

  writeln(GDALfile,'set sourcebmp='+Name+'_combined\'+Name+'.bmp');
  writeln(GDALfile,'set sourcetiff='+Name+'_combined\'+Name+'.tif');
  writeln(GDALfile,'set destinationtiff=UTMmap.tif');

  writeln(GDALfile,'rem convert, with cropping, and re-sizing');
  writeln(GDALfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
//  writeln(GDALfile,'gdalwarp.exe -of GTiff -s_srs EPSG:3857 -t_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -ts %image_width% %image_height% -te %utm_wanted_left% %utm_wanted_bottom% %utm_wanted_right% %utm_wanted_top% %sourcetiff% %destinationtiff%');
  writeln(GDALfile,'gdalwarp.exe -of GTiff -s_srs EPSG:4326 -t_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -ts %image_width% %image_height% -te %utm_wanted_left% %utm_wanted_bottom% %utm_wanted_right% %utm_wanted_top% %sourcetiff% %destinationtiff%');
  writeln(GDALfile,'del %sourcetiff%');
  writeln(GDALfile,'if exist %sourcebmp% del %sourcebmp%');

  writeln(GDALfile,'rem convert to bitmap');
  writeln(GDALfile,'set sourcetiff=UTMmap.tif');
  writeln(GDALfile,'set destinationbmp=x'+Name+'.bmp');
  writeln(GDALfile,'gdal_translate -of BMP %sourcetiff% %destinationbmp%');
  writeln(GDALfile,'del %sourcetiff%');
  writeln(GDALfile,'rem move %destinationbmp% ' + File_Destination);

  writeln(GDALfile,'endlocal');

  // close the file
  Close(GDALfile);

  MessageShow(FileName+' done.');
end;

//-------------------------------------------------------------------------------------
Procedure MakeLandsatGDALbatchFile(TileIndex : integer);

var
//  i : integer;
  FileName : string;
  FilePath : string;

  Tile_B_Lat  : double;
  Tile_T_Lat  : double;
  Tile_L_Long : double;
  Tile_R_Long : double;

begin
   // check for folder
  if (NOT DirectoryExists(GDALFolder)) then begin
    MessageShow('Destination Folder not found');
    exit;
  end;
  // create a folder if necessary
  FilePath := GDALFolder +'\SourceTiles';
  if (NOT DirectoryExists(FilePath)) then begin
    mkdir(FilePath);
  end;
  // create a folder if necessary
  FilePath := FilePath +'\'+ TileList[TileIndex].TileName;
  if (NOT DirectoryExists(FilePath)) then begin
    mkdir(FilePath);
  end;
  //open the file
  FileName := 'LS_'+TileList[TileIndex].TileName+'.bat';
  AssignFile(GDALfile, FilePath +'\'+ FileName);
  Rewrite(GDALfile);

  writeln(GDALfile,'@echo off');
  writeln(GDALfile,'setlocal');
  writeln(GDALfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
  writeln(GDALfile,'rem goto directory where batch file is');
  writeln(GDALfile,'cd /d %~dp0');

  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - Legacy_Offset;
  Tile_T_Lat  := Tile_B_Lat + 23040;
  Tile_L_Long := UTM_Right + Legacy_Offset - TileList[TileIndex+1].TileUTMRight;
  Tile_R_Long := Tile_L_Long + 23040;

  writeln(GDALfile,'rem crop to UTM coordinates');
//  writeln(GDALfile,'set utm_zone='+UTM_Zone);
//  if (UTM_ZoneNS = 'N') then begin
//    writeln(GDALfile,'set utm_grid=north');
//  end else begin
//    writeln(GDALfile,'set utm_grid=south');
//  end;
  writeln(GDALfile,format('set utm_wanted_left=%1.1f',[Tile_L_Long]));
  writeln(GDALfile,format('set utm_wanted_bottom=%1.1f',[Tile_B_Lat]));
  writeln(GDALfile,format('set utm_wanted_right=%1.1f',[Tile_R_Long]));
  writeln(GDALfile,format('set utm_wanted_top=%1.1f',[Tile_T_Lat]));

  writeln(GDALfile,'rem re-size');
  writeln(GDALfile,'set image_width='+OutputTileSize);
  writeln(GDALfile,'set image_height='+OutputTileSize);

  // "" needed because of potential spaces in the file path
  writeln(GDALfile,'set sourcetiff="'+GDALfolder+'\Landsat\'+'fused.tif"');
  writeln(GDALfile,'set destinationtiff='+TileList[TileIndex].TileName+'\'+'UTMmap.tif');

  writeln(GDALfile,'rem convert, with cropping, and re-sizing');
  writeln(GDALfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
  writeln(GDALfile,'gdalwarp.exe -r lanczos -of GTiff -ts %image_width% %image_height% -te %utm_wanted_left% %utm_wanted_bottom% %utm_wanted_right% %utm_wanted_top% %sourcetiff% %destinationtiff%');

  writeln(GDALfile,'rem convert to bitmap');
  writeln(GDALfile,'set sourcetiff='+TileList[TileIndex].TileName+'\'+'UTMmap.tif');
  writeln(GDALfile,'set destinationbmp='+TileList[TileIndex].TileName+'.bmp');
  writeln(GDALfile,'gdal_translate -of BMP %sourcetiff% %destinationbmp%');
  writeln(GDALfile,'del %sourcetiff%');
//  writeln(GDALfile,'move %destinationbmp% ..\..\Terragen');
  writeln(GDALfile,'move %destinationbmp% ' + File_Destination);

  writeln(GDALfile,'endlocal');

  // close the file
  Close(GDALfile);

  MessageShow(FileName+' done.');
end;

//-------------------------------------------------------------------------------------
Procedure MakeGDAL_All_BatchFile(DetectTree, TIFF : boolean; epsg : integer);
var
  i,j : integer;
  FileName : string;
  FilePath : string;
  TileIndex : integer;
  Name : string;
  Ext_Name : string;

begin
  // create a folder if necessary
  FilePath := GDALFolder +'\SourceTiles';
  ForceDirectories(FilePath);

  //open the file
  Ext_Name := '';
  if (DetectTree) then begin
    Ext_Name := Ext_Name + '_DetectTree';
  end;
  if (TIFF) then begin
    Ext_Name := Ext_Name + '_TIFF';
  end;
  if (epsg = 4326) then begin
    Ext_Name := Ext_Name + '_4326';
  end;
  FileName := 'GDAL_ALL'+Ext_Name+'.bat';
  AssignFile(GDALfile, FilePath +'\'+ FileName);
  Rewrite(GDALfile);

  writeln(GDALfile,'@echo off');
  writeln(GDALfile,'setlocal');
  writeln(GDALfile,'rem goto directory where batch file is');
  writeln(GDALfile,'cd /d %~dp0');
  for j := 0 to TileColumnCount-1 do begin
    for i := 0 to TileRowCount-1 do begin
      TileIndex := i*(TileColumnCount+1)+j;
      Name := TileList[TileIndex].TileName;
      // use || to execute next command if previous one failed
      writeln(GDALfile,'call '+Name+'\GDAL_'+Name+Ext_Name+'.bat || exit /b 9');
    end;
  end;
  writeln(GDALfile,'endlocal');

  // close the file
  Close(GDALfile);
  MessageShow(FileName+' done.');
end;

//-------------------------------------------------------------------------------------
Procedure Make_DetectTree_to_ForestMaps_BatchFile;
var
  i,j : integer;
  FileName : string;
  FilePath : string;
  TileIndex : integer;
  Name : string;

begin
  // create a folder if necessary
  FilePath := GDALFolder +'\Terragen\\Textures_DetectTree\pred_tiles';
  ForceDirectories(FilePath);

  FileName := 'Make_ForestMaps.bat';
  AssignFile(GDALfile, FilePath +'\'+ FileName);
  Rewrite(GDALfile);
  begin
      writeln(GDALfile,'@echo off');
      writeln(GDALfile,'setlocal EnableDelayedExpansion');
//      writeln(GDALfile,'set PATH=%PATH%;"C:\OSGeo4W64\bin"');
//      writeln(GDALfile,'set PATH=%PATH%;"\Tools"');
      writeln(GDALfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
//      writeln(GDALfile,'set GDAL_DATA='+GDALlibraryFolder+'\..\share\epsg_csv');
      // suppress generation of .xml file
      writeln(GDALfile,'set GDAL_PAM_ENABLED=NO');
      writeln(GDALfile,'rem goto directory where batch file is');
      writeln(GDALfile,'cd /d %~dp0');
      writeln(GDALfile,'rem create output folder if necessary');
      writeln(GDALfile,'if NOT exist ..\..\ForestMaps mkdir ..\..\ForestMaps');
      writeln(GDALfile,'rem do all files in folder');
      writeln(GDALfile,'for %%y in (*.tif) DO (');
      writeln(GDALfile,'set sourceTIF=%%y');
      writeln(GDALfile,'echo !sourceTIF!');
      writeln(GDALfile,'set destinationBMP=..\..\ForestMaps\b%%~ny.bmp');
      writeln(GDALfile,'rem convert 24 bit greyscale to 8 bit greyscale by just using one of the three colors');
      writeln(GDALfile,'gdal_translate -b 1 !sourceTIF! !destinationBMP!');
//      writeln(GDALfile,'del !destinationBMP!.aux.xml');
      writeln(GDALfile,'rem create a blank s file');
      writeln(GDALfile,'set destinationBMP=..\..\ForestMaps\s%%~ny.bmp');
      writeln(GDALfile,'gdal_translate -b 1 -scale 0 255 0 0 !sourceTIF! !destinationBMP!');
//      writeln(GDALfile,'rem remove un-wanted files');
//      writeln(GDALfile,'del !destinationBMP!.aux.xml');
      writeln(GDALfile,')');
      writeln(GDALfile,'endlocal');
   end;
  // close the file
  Close(GDALfile);
  MessageShow(FileName+' done.');
end;

// 3857 for gmid and 4326 for geid
//-------------------------------------------------------------------------------------
Procedure MakeAutoGDAL_Generic(epsg : Integer; FileName, FilePath : string;
  Name, Zoom_Level : string;
  UTM_Left, UTM_Right, UTM_Bottom, UTM_Top : single);

var
  Zoom_Suffix : string;

begin
  // create path
  ForceDirectories(FilePath);

  //open the file
  AssignFile(GDALfile, FilePath +'\'+ FileName);
  Rewrite(GDALfile);

  writeln(GDALfile,'@echo off');
  writeln(GDALfile,'rem Enable Delayed Expansion for !Variables! ');
  writeln(GDALfile,'setlocal EnableDelayedExpansion');
  writeln(GDALfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
  writeln(GDALfile,'set GDAL_DATA='+GDALlibraryFolder+'\..\share\epsg_csv');
  // suppres generation of .xml file
  writeln(GDALfile,'set GDAL_PAM_ENABLED=NO');
  writeln(GDALfile,'rem goto directory where batch file is');
  writeln(GDALfile,'cd /d %~dp0');

  writeln(GDALfile,'rem convert bitmap to GeoTiff to embed the lat/long coordinates');

  if (epsg = 4326) then begin
    Zoom_Suffix := format('_%d',[strtoint(Zoom_Level)+1]); // for geid, zoom level is 1 step higher
//    writeln(GDALfile,'set sourcebmp='+TileName+'_geid_combined\'+TileName+'_zoom'+Zoom_Suffix+'.bmp');
    writeln(GDALfile,'set sourcebmp='+Name+'_combined\'+Name+'_zoom'+Zoom_Suffix+'.bmp');
    writeln(GDALfile,'set FileName='+Name+'.geid');
  end else begin // (epsg = 3857)
    Zoom_Suffix := '';
    writeln(GDALfile,'set sourcebmp='+Name+'_combined\'+Name+'.bmp');
    writeln(GDALfile,'set FileName='+Name+'.umd');
    writeln(GDALfile,'if NOT exist %FileName% set FileName='+Name+'.gmid');
  end;
  writeln(GDALfile,'if NOT exist %FileName% (echo ERROR: %FileName% NOT found & pause & exit /b 9)');

  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "Left_Longitude_download'+Zoom_Suffix+'=" %FileName%'') do set real_left=%%a');
  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "Top_Latitude_download'+Zoom_Suffix+'=" %FileName%'') do set real_top=%%a');
  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "Right_Longitude_download'+Zoom_Suffix+'=" %FileName%'') do set real_right=%%a');
  writeln(GDALfile,'for /f "tokens=2 delims==" %%a in (''find "Bottom_Latitude_download'+Zoom_Suffix+'=" %FileName%'') do set real_bottom=%%a');
  // trim spaces
//  writeln(GDALfile,'set real_left=%real_left: =%');
//  writeln(GDALfile,'set real_top=%real_top: =%');
//  writeln(GDALfile,'set real_right=%real_right: =%');
//  writeln(GDALfile,'set real_bottom=%real_bottom: =%');

  writeln(GDALfile,'set destinationtiff='+'bigmap.tif');
  writeln(GDALfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present

  if (epsg = 4326) then begin
    writeln(GDALfile,'gdal_translate -of Gtiff -a_ullr %real_left% %real_top% %real_right% %real_bottom% -a_srs EPSG:4326 %sourcebmp% %destinationtiff%');
  end else begin // (epsg = 3857)
    // convert coordinates
    writeln(GDALfile,'rem convert EPSG:4326 coords to EPSG:3857');
    writeln(GDALfile,'rem convert lat/long coordinates to meters');
    writeln(GDALfile,'(echo %real_left% %real_top%)>Coord_In.txt');
    writeln(GDALfile,'(echo %real_right% %real_bottom%)>>Coord_In.txt');
    writeln(GDALfile,'gdaltransform -s_srs EPSG:4326 -t_srs EPSG:3857 <Coord_In.txt >Coord_Out.txt');
    writeln(GDALfile,'rem Get file contents and store them per variable ex: var1, var2, var3, var4');
    writeln(GDALfile,'set VarList=0');
    writeln(GDALfile,'for /F "tokens=1,2" %%A in (Coord_Out.txt) do (');
    writeln(GDALfile,'    SET /A VarList=!VarList! + 1');
    writeln(GDALfile,'    set "var!VarList!=%%A"');
    writeln(GDALfile,'    SET /A VarList=!VarList! + 1');
    writeln(GDALfile,'    set "var!VarList!=%%B"');
    writeln(GDALfile,')');

    writeln(GDALfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
    writeln(GDALfile,'gdal_translate -of Gtiff -a_ullr %var1% %var2% %var3% %var4% -a_srs EPSG:3857 %sourcebmp% %destinationtiff%');
  end;
  writeln(GDALfile,'rem del %sourcebmp%');

  writeln(GDALfile,'rem crop to UTM coordinates');
//  writeln(GDALfile,'set utm_zone='+UTM_Zone);
  writeln(GDALfile,format('set utm_zone=%d',[UTM_Zone]));
  if (UTM_ZoneNS = 'N') then begin
    writeln(GDALfile,'set utm_grid=north');
  end else begin
    writeln(GDALfile,'set utm_grid=south');
  end;
  writeln(GDALfile,format('set utm_wanted_left=%1.1f',[UTM_Left]));
  writeln(GDALfile,format('set utm_wanted_bottom=%1.1f',[UTM_Bottom]));
  writeln(GDALfile,format('set utm_wanted_right=%1.1f',[UTM_Right]));
  writeln(GDALfile,format('set utm_wanted_top=%1.1f',[UTM_Top]));

// no size for airport
//  writeln(GDALfile,'rem re-size');
//  writeln(GDALfile,'set image_width='+OutputTileSize);
//  writeln(GDALfile,'set image_height='+OutputTileSize);

  writeln(GDALfile,'set sourcetiff='+'bigmap.tif');
  writeln(GDALfile,'set destinationtiff='+'UTMmap.tif');

  writeln(GDALfile,'rem convert, with cropping, and re-sizing');
  writeln(GDALfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
//  writeln(GDALfile,'gdalwarp.exe -r lanczos -of GTiff -t_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -ts %image_width% %image_height% -te %utm_wanted_left% %utm_wanted_bottom% %utm_wanted_right% %utm_wanted_top% %sourcetiff% %destinationtiff%');
// no size for airport
  writeln(GDALfile,'gdalwarp.exe -r lanczos -of GTiff -t_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -te %utm_wanted_left% %utm_wanted_bottom% %utm_wanted_right% %utm_wanted_top% %sourcetiff% %destinationtiff%');
  writeln(GDALfile,'del %sourcetiff%');

  writeln(GDALfile,'rem convert to bitmap');
  writeln(GDALfile,'set sourcetiff='+'UTMmap.tif');
  writeln(GDALfile,'set destinationbmp='+Name+'.bmp');
  writeln(GDALfile,'gdal_translate -of BMP %sourcetiff% %destinationbmp%');
  writeln(GDALfile,'del %sourcetiff%');
// no move for airport
//  writeln(GDALfile,'rem move %destinationbmp% ' + File_Destination);

  writeln(GDALfile,'endlocal');

  // close the file
  Close(GDALfile);

  MessageShow(FileName+' done.');
end;

{----------------------------------------------------------------------------}
begin
  Memo_Message := nil;
end.

//--- End of file ---------------------------------------------------------------------


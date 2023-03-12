{
 * u_MakeGEO.pas
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

//----------------------------------------------------------------------------
unit u_MakeGEO;

{----------------------------------------------------------------------------
Create batch files that use GDAL functions to create a bitmap from a
GEO database shapefile.
- first create and convert a bitmap to GeoTIFF to embed the corner coordinates
  - UTM extents come from the header file
  - size can be 1024, 2048, 4096, 8192
----------------------------------------------------------------------------}
interface

uses StdCtrls, FileCtrl, SysUtils;

var
  Memo_Message : TMemo;  // external TMemo for messages
  GEOFolder : string;   // external path for file output
  GDALlibraryFolder : string; // external path for library
  OutputTileSize : string;
  GeoDatabaseType : (OSM, CanVec);
  File_Destination : string;

Procedure Init;
//Procedure Init_Colors;
//Procedure Init_Sql;
Procedure MakeGEO_GO_batchFile;
Procedure MakeGEObatchFile(TileIndex : integer);
Procedure MakeGEO_V1_Forest_batchFile(TileIndex : integer);
Procedure MakeGEO_V2_Forest_Deciduous_batchFile(TileIndex : integer);
Procedure MakeGEO_V2_Forest_Coniferous_batchFile(TileIndex : integer);
Procedure MakeGEO_Thermal_batchFile(TileIndex : integer);
Procedure MakeGEO_V2_Water_batchFile(TileIndex : integer);

//----------------------------------------------------------------------------
implementation

uses Graphics, u_TileList, u_SceneryHDR, u_BMP, u_Util, u_UTM;

type
  GeoFeature = record
    fDesc : string;
    fColor : array[0..3-1] of string[3];
    ftColor : ColorConvert;
    fSql : string;
    fOpt : string;
  end;

const
  // for CANVEC Canadian data only
  CanVec_FeatureCount_250K = 2;
  CanVec_FeatureCount_50K = 18;
  CanVec_FeatureCount_Water = 3;

  // for OSM SHP data only
  OSM_FeatureCount_250K = 1;
  OSM_FeatureCount_50K = 12;
  OSM_FeatureCount_Water = 3;

var
  GEOfile : TextFile;
  DBfolderName : string;
  DBfolderExt : string;
  FeatureCount_250K : integer;
  FeatureCount_50K : integer;
  FeatureCount_Water : integer;
  FeatureList_250K : array of GeoFeature;
  FeatureList_50K : array of GeoFeature;
  FeatureList_Water : array of GeoFeature;

  // for CANVEC Canadian data only
  CanVec_FeatureList_250K : array[0..CanVec_FeatureCount_250K-1] of GeoFeature = (
    (fDesc:'wooded_area_2';         fColor:('  0','255','  0')),  // deciduous forest
    (fDesc:'saturated_soil_2';      fColor:('192','224',' 32'))   // coniferous forest
  );
  CanVec_FeatureList_50K : array[0..CanVec_FeatureCount_50K-1] of GeoFeature = (
    (fDesc:'residential_area_2';    fColor:('255','255','  0')),
    (fDesc:'building_2';            fColor:('224','224','  0')),
    (fDesc:'waste_2';               fColor:('  0','255','255')),
    (fDesc:'peat_2';                fColor:('255','  0','255')),
    (fDesc:'aggregate_2';           fColor:('160','128','  0')),
    (fDesc:'track_segment_1';       fColor:(' 64','128','128')),
    (fDesc:'track_structure_1';     fColor:(' 64','128','128')),
    (fDesc:'landmark_feature_1';    fColor:('255','  1','  1')),
    (fDesc:'landmark_feature_2';    fColor:('255','  1','  1')),
    (fDesc:'runway_2';              fColor:('255','  1','  1')),
    (fDesc:'watercourse_1';         fColor:('  0','  0','255')),
    (fDesc:'water_linear_flow_1';   fColor:('  0','  0','255')),
    (fDesc:'waterbody_2';           fColor:('  0','  0','255')),
    (fDesc:'road_segment_1';        fColor:('196','196','  0')),
    (fDesc:'cut_line_1';            fColor:('160','160','160')),
    (fDesc:'trail_1';               fColor:('224','112','  0')),
    (fDesc:'transformer_station_2'; fColor:('255','255','255')),
    (fDesc:'power_line_1';          fColor:('255','255','255'))
  );
  CanVec_FeatureList_Water : array[0..CanVec_FeatureCount_Water-1] of GeoFeature = (
    (fDesc:'waterbody_2';           fColor:('  0','  0','255')),
    (fDesc:'watercourse_1';         fColor:('  0','  0','255')),
    (fDesc:'water_linear_flow_1';   fColor:('  0','  0','255'))
  );

  // for OSM SHP data only
  OSM_FeatureList_250K : array[0..OSM_FeatureCount_250K-1] of GeoFeature = (
    (fDesc:'gis_osm_landuse_a_free_1';   fColor:('  0','128','  0'))  // deciduous forest
  );                                                                  // NO coniferous forest, unfortunately
  OSM_FeatureList_50K : array[0..OSM_FeatureCount_50K-1] of GeoFeature = (
    (fDesc:'gis_osm_landuse_a_free_1';   fColor:('  0','192','  0')),  // green fields
    (fDesc:'gis_osm_railways_free_1';    fColor:(' 64','128','128')),
    (fDesc:'gis_osm_roads_free_1';       fColor:('196','196','  0')),
    (fDesc:'gis_osm_traffic_a_free_1';   fColor:('196','196','  0')),
    (fDesc:'gis_osm_traffic_free_1';     fColor:('196','196','  0')),
    (fDesc:'gis_osm_transport_a_free_1'; fColor:(' 64','128','128')),
    (fDesc:'gis_osm_transport_free_1';   fColor:(' 64','128','128')),
    (fDesc:'gis_osm_landuse_a_free_1';   fColor:('255','  1','  1')),  // quarry
    (fDesc:'gis_osm_buildings_a_free_1'; fColor:('224','224','  0')),
    (fDesc:'gis_osm_landuse_a_free_1';   fColor:('255','255','  0')),  // residential
    (fDesc:'gis_osm_water_a_free_1';     fColor:('  0','  0','255')),
    (fDesc:'gis_osm_waterways_free_1';   fColor:('  0','  0','255'))
  );
  OSM_FeatureList_Water : array[0..OSM_FeatureCount_Water-1] of GeoFeature = (
    (fDesc:'gis_osm_water_a_free_1';     fColor:('  0','  0','255')),
    (fDesc:'gis_osm_waterways_free_1';   fColor:('  0','  0','255')),
    (fDesc:'water_polygons';             fColor:('  0','  0','255'))
  );                                                                  // NO coniferous forest, unfortunately

{----------------------------------------------------------------------------}
Procedure MessageShow(Info : string);
begin
  if (Memo_Message <> nil) then begin
    Memo_Message.lines.add(Info);
  end;
end;

{----------------------------------------------------------------------------}
Procedure Init_Sql;
begin
  case GeoDatabaseType of
    CanVec: begin
      // cor CanVec, no sql
    end;
    OSM: begin
      // for OSM SHP data only
      FeatureList_250K[0].fSql := ' -where "fclass=''forest'' OR fclass=''orchard''"';   // deciduous forest
                                                                                     // NO coniferous forest
      FeatureList_50K[0].fSql :=  ' -where "fclass=''farm'' OR fclass=''grass'' OR fclass=''meadow''"';
      FeatureList_50K[7].fSql :=  ' -where "fclass=''quary''"';
      FeatureList_50K[9].fSql :=  ' -where "fclass=''residential'' OR fclass=''industrial'' OR fclass=''retail'' OR fclass=''commercial''"';
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure Init_Options;
begin
  case GeoDatabaseType of
    CanVec: begin
      // cor CanVec, no options
    end;
    OSM: begin
      // for OSM SHP data only
      FeatureList_Water[2].fOpt := ' --config OGR_ENABLE_PARTIAL_REPROJECTION TRUE';
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure Init_Colors;
begin
  case GeoDatabaseType of
    CanVec: begin
      // colors for direct thermal generation
      // for CANVEC Canadian data only
      FeatureList_250K[0].ftColor := tDeciduous;
      FeatureList_250K[1].ftColor := tConiferous;
      FeatureList_50K[0].ftColor := tSand;
      FeatureList_50K[1].ftColor := tSand;
      FeatureList_50K[2].ftColor := tSand;
      FeatureList_50K[3].ftColor := tSwamp;
      FeatureList_50K[4].ftColor := tSand;
      FeatureList_50K[5].ftColor := tSand;
      FeatureList_50K[6].ftColor := tSand;
      FeatureList_50K[7].ftColor := tSand;
      FeatureList_50K[8].ftColor := tSand;
      FeatureList_50K[9].ftColor := tSand;
      FeatureList_50K[10].ftColor := tWater;
      FeatureList_50K[11].ftColor := tWater;
      FeatureList_50K[12].ftColor := tWater;
      FeatureList_50K[13].ftColor := tSand;
      FeatureList_50K[14].ftColor := tDarkFields;
      FeatureList_50K[15].ftColor := tDarkFields;
      FeatureList_50K[16].ftColor := tSand;
      FeatureList_50K[17].ftColor := tDarkFields;
    end;
    OSM: begin
      // colors for direct thermal generation
      // for OSM SHP data only
      FeatureList_250K[0].ftColor := tDeciduous;     // deciduous forest
      //FeatureList_250K[1].ftColor := tConiferous;  // NO coniferous forest
      FeatureList_50K[0].ftColor :=  tGreenFields;
      FeatureList_50K[1].ftColor :=  tSand;
      FeatureList_50K[2].ftColor :=  tSand;
      FeatureList_50K[3].ftColor :=  tSand;
      FeatureList_50K[4].ftColor :=  tSand;
      FeatureList_50K[5].ftColor :=  tSand;
      FeatureList_50K[6].ftColor :=  tSand;
      FeatureList_50K[7].ftColor :=  tSand;
      FeatureList_50K[8].ftColor :=  tSand;
      FeatureList_50K[9].ftColor :=  tSand;
      FeatureList_50K[10].ftColor := tWater;
      FeatureList_50K[11].ftColor := tWater;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure Init;
var
  i : integer;
begin
  case GeoDatabaseType of
    OSM: begin
      // for generic OSM shape file
      DBfolderName := '';
      DBfolderExt := '*.shp';

      FeatureCount_250K := OSM_FeatureCount_250K;
      FeatureCount_50K  := OSM_FeatureCount_50K;
      FeatureCount_Water  := OSM_FeatureCount_Water;
      SetLength(FeatureList_250K,FeatureCount_250K);
      for i := 0 to FeatureCount_250K-1 do begin
        FeatureList_250K[i]  := OSM_FeatureList_250K[i];
      end;
      SetLength(FeatureList_50K,FeatureCount_50K);
      for i := 0 to FeatureCount_50K-1 do begin
        FeatureList_50K[i]  := OSM_FeatureList_50K[i];
      end;
      SetLength(FeatureList_Water,FeatureCount_Water);
      for i := 0 to FeatureCount_Water-1 do begin
        FeatureList_Water[i]  := OSM_FeatureList_Water[i];
      end;
    end;
    CanVec: begin
      // for Canada data source
      DBfolderName := '/dataset/canvec.gdb';
      DBfolderExt := '*.ogr';

      FeatureCount_250K := CanVec_FeatureCount_250K;
      FeatureCount_50K  := CanVec_FeatureCount_50K;
      FeatureCount_Water  := CanVec_FeatureCount_Water;
      SetLength(FeatureList_250K,FeatureCount_250K);
      for i := 0 to FeatureCount_250K-1 do begin
        FeatureList_250K[i]  := CanVec_FeatureList_250K[i];
      end;
      SetLength(FeatureList_50K,FeatureCount_50K);
      for i := 0 to FeatureCount_50K-1 do begin
        FeatureList_50K[i]  := CanVec_FeatureList_50K[i];
      end;
      SetLength(FeatureList_Water,FeatureCount_Water);
      for i := 0 to FeatureCount_Water-1 do begin
        FeatureList_Water[i]  := CanVec_FeatureList_Water[i];
      end;
    end;
  end;
  Init_Colors;
  Init_Sql;
  Init_Options;
end;

//-------------------------------------------------------------------------------------
Procedure MakeGEO_GO_batchFile;

var
  i,j : integer;
  TileIndex : integer;
  FileName : string;
  FilePath : string;
  BatchFileName : string;


begin
  // create a folder if necessary
  FilePath := GEOFolder +'\GeoDatabase';
  if (NOT DirectoryExists(FilePath)) then begin
    mkdir(FilePath);
  end;

  //create thermal batch file
  BatchFileName := 'GO_t.bat';
  AssignFile(GEOfile, FilePath+'\'+BatchFileName);
  Rewrite(GEOfile);

  writeln(GEOfile,'setlocal');
  writeln(GEOfile,'cd ..\SourceTiles\0000');

  for i := 0 to TileRowCount-1 do begin
    for j := 0 to TileColumnCount-1 do begin
      TileIndex := i*(TileColumnCount+1)+j;
      FileName := 'GEO_t_'+TileList[TileIndex].TileName+'.bat';
      writeln(GEOfile,'cd ..\'+TileList[TileIndex].TileName);
      writeln(GEOfile,'call '+FileName);
    end;
  end;

  writeln(GEOfile,'endlocal');

  // close the file
  Close(GEOfile);
  MessageShow(BatchFileName+' done.');

  //create V1 forest batch file
  BatchFileName := 'GO_f.bat';
  AssignFile(GEOfile, FilePath+'\'+BatchFileName);
  Rewrite(GEOfile);

  writeln(GEOfile,'setlocal');
  writeln(GEOfile,'cd ..\SourceTiles\0000');

  for i := 0 to TileRowCount-1 do begin
    for j := 0 to TileColumnCount-1 do begin
      TileIndex := i*(TileColumnCount+1)+j;
      FileName := 'GEO_V1_f_'+TileList[TileIndex].TileName+'.bat';
      writeln(GEOfile,'cd ..\'+TileList[TileIndex].TileName);
      writeln(GEOfile,'call '+FileName);
    end;
  end;

  writeln(GEOfile,'endlocal');

  // close the file
  Close(GEOfile);
  MessageShow(BatchFileName+' done.');

  //create V2 deciduous forest batch file
  BatchFileName := 'GO_b.bat';
  AssignFile(GEOfile, FilePath+'\'+BatchFileName);
  Rewrite(GEOfile);

  writeln(GEOfile,'setlocal');
  writeln(GEOfile,'cd ..\SourceTiles\0000');

  for i := 0 to TileRowCount-1 do begin
    for j := 0 to TileColumnCount-1 do begin
      TileIndex := i*(TileColumnCount+1)+j;
      FileName := 'GEO_V2_b_'+TileList[TileIndex].TileName+'.bat';
      writeln(GEOfile,'cd ..\'+TileList[TileIndex].TileName);
      writeln(GEOfile,'call '+FileName);
    end;
  end;

  writeln(GEOfile,'endlocal');

  // close the file
  Close(GEOfile);

  MessageShow(BatchFileName+' done.');

  //create V2 Coniferous forest batch file
  BatchFileName := 'GO_s.bat';
  AssignFile(GEOfile, FilePath+'\'+BatchFileName);
  Rewrite(GEOfile);

  writeln(GEOfile,'setlocal');
  writeln(GEOfile,'cd ..\SourceTiles\0000');

  for i := 0 to TileRowCount-1 do begin
    for j := 0 to TileColumnCount-1 do begin
      TileIndex := i*(TileColumnCount+1)+j;
      FileName := 'GEO_V2_s_'+TileList[TileIndex].TileName+'.bat';
      writeln(GEOfile,'cd ..\'+TileList[TileIndex].TileName);
      writeln(GEOfile,'call '+FileName);
    end;
  end;

  writeln(GEOfile,'endlocal');

  // close the file
  Close(GEOfile);

  MessageShow(BatchFileName+' done.');

  //create water tile batch file
  BatchFileName := 'GO_w.bat';
  AssignFile(GEOfile, FilePath+'\'+BatchFileName);
  Rewrite(GEOfile);

  writeln(GEOfile,'setlocal');
  writeln(GEOfile,'cd ..\SourceTiles\0000');

  for i := 0 to TileRowCount-1 do begin
    for j := 0 to TileColumnCount-1 do begin
      TileIndex := i*(TileColumnCount+1)+j;
      FileName := 'GEO_V2_w_'+TileList[TileIndex].TileName+'.bat';
      writeln(GEOfile,'cd ..\'+TileList[TileIndex].TileName);
      writeln(GEOfile,'call '+FileName);
    end;
  end;

  writeln(GEOfile,'endlocal');

  // close the file
  Close(GEOfile);
  MessageShow(BatchFileName+' done.');

end;

//-------------------------------------------------------------------------------------
Procedure MakeGEObatchFile(TileIndex : integer);

var
  i : integer;
  FileName : string;
  FilePath : string;

  Tile_B_Lat : double;
  Tile_T_Lat : double;
  Tile_L_Long : double;
  Tile_R_Long : double;

begin
  // check for folder
  if (NOT DirectoryExists(GEOFolder)) then begin
    MessageShow('Destination Folder not found');
    exit;
  end;
  // create a folder if necessary
  FilePath := GEOFolder +'\SourceTiles';
  if (NOT DirectoryExists(FilePath)) then begin
    mkdir(FilePath);
  end;
  // create a folder if necessary
  FilePath := FilePath +'\'+ TileList[TileIndex].TileName;
  if (NOT DirectoryExists(FilePath)) then begin
    mkdir(FilePath);
  end;

  MessageShow(FilePath +'\'+ FileName);

  FilePath := GEOfolder +'\SourceTiles\'+ TileList[TileIndex].TileName;
  //open the file
  FileName := 'GEO_'+TileList[TileIndex].TileName+'.bat';
  AssignFile(GEOfile, FilePath +'\'+ FileName);
  Rewrite(GEOfile);

  writeln(GEOfile,'echo off');
  // enable delayed expansion for for loops with !sourcevec!; also need ^! instead of %%
  writeln(GEOfile,'setlocal enabledelayedexpansion');
  writeln(GEOfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
  writeln(GEOfile,'set GDAL_DATA='+GDALlibraryFolder+'\..\share\epsg_csv');
  writeln(GEOfile,'gdalinfo --version');

  writeln(GEOfile,'rem create a GeoTiff to embed the UTM easting and northing');

  writeln(GEOfile,'rem set the size');
//  writeln(GEOfile,'set image_width='+OutputTileSize);
  writeln(GEOfile,'set image_width=1024');
//  writeln(GEOfile,'set image_height='+OutputTileSize);
  writeln(GEOfile,'set image_height=1024');

  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - 45;
  Tile_T_Lat  := Tile_B_Lat + 23040;
  Tile_L_Long  := UTM_Right +45 - TileList[TileIndex+1].TileUTMRight;
  Tile_R_Long  := Tile_L_Long + 23040;

  writeln(GEOfile,'rem crop to UTM coordinates');
  writeln(GEOfile,'set utm_zone='+UTM_Zone);
  if (UTM_ZoneNS = 'N') then begin
    writeln(GEOfile,'set utm_grid=north');
  end else begin
    writeln(GEOfile,'set utm_grid=south');
  end;
  writeln(GEOfile,format('set utm_left=%1.1f',[Tile_L_Long]));
  writeln(GEOfile,format('set utm_bottom=%1.1f',[Tile_B_Lat]));
  writeln(GEOfile,format('set utm_right=%1.1f',[Tile_R_Long]));
  writeln(GEOfile,format('set utm_top=%1.1f',[Tile_T_Lat]));
//  writeln(GEOfile,'set destTiff='+TileList[TileIndex].TileName+'\'+'UTMmap.tif');
  writeln(GEOfile,'set destTiff=gUTMmap.tif');

  writeln(GEOfile,'rem it doesn''t work the first time, but the file is created');
//  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
  writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (if not defined sourcevec set "sourcevec=..\..\GeoDatabase\%%f'+DBfolderName+'")');
  writeln(GEOfile,'echo ^!sourcevec^!');
//  writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy %sourcevec% %destTiff%');
  writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy ^!sourcevec^! %destTiff%');

  // loop through all ShapeFiles
  writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (');
//  writeln(GEOfile,'echo %%f');

  // must do Wooded Areas first so that roads and rivers are plotted on top !
  writeln(GEOfile,'rem do Wooded Areas first so that roads and rivers are plotted on top');
//  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
  writeln(GEOfile,'set sourcevec=../../GeoDatabase/%%f'+DBfolderName);
  writeln(GEOfile,'echo ^!sourcevec^!');
  for i := 0 to FeatureCount_250K-1 do begin
    with FeatureList_250K[i] do begin
//      writeln(GEOfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn '+fColor[0]+' -burn '+fColor[1]+' -burn '+fColor[2]+' -l '+fDesc + fSql +' %sourcevec% %destTiff%');
      writeln(GEOfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn '+fColor[0]+' -burn '+fColor[1]+' -burn '+fColor[2]+' -l '+fDesc + fSql +' ^!sourcevec^! %destTiff%');
    end;
  end;
  writeln(GEOfile,'rem add the desired vector features into the raster file');
//  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
  writeln(GEOfile,'set sourcevec=../../GeoDatabase/%%f'+DBfolderName);
  writeln(GEOfile,'echo ^!sourcevec^!');
  for i := 0 to FeatureCount_50K-1 do begin
    with FeatureList_50K[i] do begin
//      writeln(GEOfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn '+fColor[0]+' -burn '+fColor[1]+' -burn '+fColor[2]+' -l '+fDesc + fSql +' %sourcevec% %destTiff%');
      writeln(GEOfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn '+fColor[0]+' -burn '+fColor[1]+' -burn '+fColor[2]+' -l '+fDesc + fSql +' ^!sourcevec^! %destTiff%');
    end;
  end;

  writeln(GEOfile,')'); // end loop

  writeln(GEOfile,'rem convert to bitmap');
//  writeln(GEOfile,'set sourcetiff='+TileList[TileIndex].TileName+'\'+'UTMmap.tif');
  writeln(GEOfile,'set sourcetiff=gUTMmap.tif');
  writeln(GEOfile,'set destinationbmp='+TileList[TileIndex].TileName+'.bmp');
  writeln(GEOfile,'gdal_translate -of BMP %sourcetiff% %destinationbmp%');
  writeln(GEOfile,'del %sourcetiff%');
  writeln(GEOfile,'move %destinationbmp% ..\..\GeoDatabase');

  writeln(GEOfile,'endlocal');

  // close the file
  Close(GEOfile);
  MessageShow(FileName+' done.');
end;

//-------------------------------------------------------------------------------------
Procedure MakeGEO_V1_Forest_batchFile(TileIndex : integer);

const
  V1_Forest_Size = '512';

var
  i : integer;
  FileName : string;
  FilePath : string;

  Tile_B_Lat : double;
  Tile_T_Lat : double;
  Tile_L_Long : double;
  Tile_R_Long : double;

begin
  FilePath := GEOfolder +'\SourceTiles\'+ TileList[TileIndex].TileName;

  //open the file
  FileName := 'GEO_V1_f_'+TileList[TileIndex].TileName+'.bat';
  AssignFile(GEOfile, FilePath +'\'+ FileName);
  Rewrite(GEOfile);

  writeln(GEOfile,'echo off');
//  writeln(GEOfile,'setlocal');
  writeln(GEOfile,'setlocal enabledelayedexpansion');
  writeln(GEOfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
  writeln(GEOfile,'set GDAL_DATA='+GDALlibraryFolder+'\..\share\epsg_csv');
  writeln(GEOfile,'gdalinfo --version');

  writeln(GEOfile,'rem create a GeoTiff to embed the UTM easting and northing');

  writeln(GEOfile,'rem set the size');
  writeln(GEOfile,'set image_width='+V1_Forest_Size);
  writeln(GEOfile,'set image_height='+V1_Forest_Size);

  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - 45;
  Tile_T_Lat  := Tile_B_Lat + 23040;
  Tile_L_Long  := UTM_Right +45 - TileList[TileIndex+1].TileUTMRight;
  Tile_R_Long  := Tile_L_Long + 23040;

  writeln(GEOfile,'rem crop to UTM coordinates');
  writeln(GEOfile,'set utm_zone='+UTM_Zone);
  if (UTM_ZoneNS = 'N') then begin
    writeln(GEOfile,'set utm_grid=north');
  end else begin
    writeln(GEOfile,'set utm_grid=south');
  end;
  writeln(GEOfile,format('set utm_left=%1.1f',[Tile_L_Long]));
  writeln(GEOfile,format('set utm_bottom=%1.1f',[Tile_B_Lat]));
  writeln(GEOfile,format('set utm_right=%1.1f',[Tile_R_Long]));
  writeln(GEOfile,format('set utm_top=%1.1f',[Tile_T_Lat]));
//  writeln(GEOfile,'set destTiff='+TileList[TileIndex].TileName+'\'+'UTMmap.tif');
  writeln(GEOfile,'set destTiff=fUTMmap.tif');

  writeln(GEOfile,'rem it doesn''t work the first time, but the file is created');
//  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
  writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (if not defined sourcevec set "sourcevec=..\..\GeoDatabase\%%f'+DBfolderName+'")');
  writeln(GEOfile,'echo ^!sourcevec^!');
//  writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy %sourcevec% %destTiff%');
  writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy ^!sourcevec^! %destTiff%');

  // loop through all ShapeFiles
  writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (');
//  writeln(GEOfile,'echo %%f');

  // must do Wooded Areas first so that roads and rivers are plotted on top !
  writeln(GEOfile,'rem do Wooded Areas first so that roads and rivers are plotted on top');
//  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
  writeln(GEOfile,'set sourcevec=../../GeoDatabase/%%f'+DBfolderName);
  writeln(GEOfile,'echo ^!sourcevec^!');
  for i := 0 to FeatureCount_250K-1 do begin
    with FeatureList_250K[i] do begin
//      writeln(GEOfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn '+fColor[0]+' -burn '+fColor[1]+' -burn '+fColor[2]+' -l '+fDesc + fSql +' %sourcevec% %destTiff%');
      writeln(GEOfile,'gdal_rasterize -b 1 -b 2 -b 3 ' +
        format('-burn %d -burn %d -burn %d',[ftColor.cRGB.rgbtRed,ftColor.cRGB.rgbtGreen,ftColor.cRGB.rgbtBlue]) +
//        ' -l ' + fDesc + fSql +' %sourcevec% %destTiff%');
        ' -l ' + fDesc + fSql +' ^!sourcevec^! %destTiff%');
    end;
  end;
  writeln(GEOfile,'rem add the desired vector features into the raster file');
//  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
  writeln(GEOfile,'set sourcevec=../../GeoDatabase/%%f'+DBfolderName);
  writeln(GEOfile,'echo ^!sourcevec^!');
  for i := 0 to FeatureCount_50K-1 do begin
    with FeatureList_50K[i] do begin
//      writeln(GEOfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn '+fColor[0]+' -burn '+fColor[1]+' -burn '+fColor[2]+' -l '+fDesc + fSql +' %sourcevec% %destTiff%');
      // overwrite forest with black non-forest features
//      writeln(GEOfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn 0 -burn 0 -burn 0 -l '+fDesc + fSql +' %sourcevec% %destTiff%');
      writeln(GEOfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn 0 -burn 0 -burn 0 -l '+fDesc + fSql +' ^!sourcevec^! %destTiff%');
    end;
  end;

  writeln(GEOfile,')'); // end loop

  writeln(GEOfile,'rem convert to bitmap');
//  writeln(GEOfile,'set sourcetiff='+TileList[TileIndex].TileName+'\'+'UTMmap.tif');
  writeln(GEOfile,'set sourcetiff=fUTMmap.tif');
//  writeln(GEOfile,'set destinationbmp='+TileList[TileIndex].TileName+'.bmp');
  writeln(GEOfile,'set destinationbmp=f'+TileList[TileIndex].TileName+'.bmp');
  writeln(GEOfile,'gdal_translate -of BMP %sourcetiff% %destinationbmp%');
  writeln(GEOfile,'del %sourcetiff%');
//  writeln(GEOfile,'move %destinationbmp% ..\..\GeoDatabase');
  writeln(GEOfile,'rem move %destinationbmp% ..\..\Terragen\ForestMaps');
  writeln(GEOfile,'endlocal');

  // close the file
  Close(GEOfile);

  MessageShow(FileName+' done.');
end;

//-------------------------------------------------------------------------------------
Procedure MakeGEO_V2_Forest_Deciduous_batchFile(TileIndex : integer);

const
  V2_Forest_Size = '2048';

var
  i : integer;
  FileName : string;
  FilePath : string;

  Tile_B_Lat : double;
  Tile_T_Lat : double;
  Tile_L_Long : double;
  Tile_R_Long : double;

begin
  FilePath := GEOfolder +'\SourceTiles\'+ TileList[TileIndex].TileName;

  //open the file
  FileName := 'GEO_V2_b_'+TileList[TileIndex].TileName+'.bat';
  AssignFile(GEOfile, FilePath +'\'+ FileName);
  Rewrite(GEOfile);

  writeln(GEOfile,'echo off');
  writeln(GEOfile,'setlocal enabledelayedexpansion');
  writeln(GEOfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
  writeln(GEOfile,'set GDAL_DATA='+GDALlibraryFolder+'\..\share\epsg_csv');
  writeln(GEOfile,'gdalinfo --version');

  writeln(GEOfile,'rem create a GeoTiff to embed the UTM easting and northing');

  writeln(GEOfile,'rem set the size');
  writeln(GEOfile,'set image_width='+V2_Forest_Size);
  writeln(GEOfile,'set image_height='+V2_Forest_Size);

  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - 45;
  Tile_T_Lat  := Tile_B_Lat + 23040;
  Tile_L_Long  := UTM_Right +45 - TileList[TileIndex+1].TileUTMRight;
  Tile_R_Long  := Tile_L_Long + 23040;

  writeln(GEOfile,'rem crop to UTM coordinates');
  writeln(GEOfile,'set utm_zone='+UTM_Zone);
  if (UTM_ZoneNS = 'N') then begin
    writeln(GEOfile,'set utm_grid=north');
  end else begin
    writeln(GEOfile,'set utm_grid=south');
  end;
  writeln(GEOfile,format('set utm_left=%1.1f',[Tile_L_Long]));
  writeln(GEOfile,format('set utm_bottom=%1.1f',[Tile_B_Lat]));
  writeln(GEOfile,format('set utm_right=%1.1f',[Tile_R_Long]));
  writeln(GEOfile,format('set utm_top=%1.1f',[Tile_T_Lat]));
//  writeln(GEOfile,'set destTiff='+TileList[TileIndex].TileName+'\'+'UTMmap.tif');
  writeln(GEOfile,'set destTiff=bUTMmap.tif');

  writeln(GEOfile,'rem it doesn''t work the first time, but the file is created');
//  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
  writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (if not defined sourcevec set "sourcevec=..\..\GeoDatabase\%%f'+DBfolderName+'")');
  writeln(GEOfile,'echo ^!sourcevec^!');
//  writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy %sourcevec% %destTiff%');
  writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy ^!sourcevec^! %destTiff%');

  // loop through all ShapeFiles
  writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (');
//  writeln(GEOfile,'echo %%f');

  // must do Wooded Areas first so that roads and rivers are plotted on top !
  writeln(GEOfile,'rem do Wooded Areas first so that roads and rivers are plotted on top');
//  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
  writeln(GEOfile,'set sourcevec=../../GeoDatabase/%%f'+DBfolderName);
  writeln(GEOfile,'echo ^!sourcevec^!');
  {for i := 0 to FeatureCount_250K-1 do} i:= 0; begin  // 0 only for deciduaous forest
    with FeatureList_250K[i] do begin
//      writeln(GEOfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn '+fColor[0]+' -burn '+fColor[1]+' -burn '+fColor[2]+' -l '+fDesc + fSql +' %sourcevec% %destTiff%');
      writeln(GEOfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn '+fColor[0]+' -burn '+fColor[1]+' -burn '+fColor[2]+' -l '+fDesc + fSql +' ^!sourcevec^! %destTiff%');
    end;
  end;
  writeln(GEOfile,'rem add the desired vector features into the raster file');
//  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
  writeln(GEOfile,'set sourcevec=../../GeoDatabase/%%f'+DBfolderName);
  writeln(GEOfile,'echo ^!sourcevec^!');
  for i := 0 to FeatureCount_50K-1 do begin
    with FeatureList_50K[i] do begin
//      writeln(GEOfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn '+fColor[0]+' -burn '+fColor[1]+' -burn '+fColor[2]+' -l '+fDesc + fSql +' %sourcevec% %destTiff%');
      // overwrite forest with black non-forest features
      writeln(GEOfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn 0 -burn 0 -burn 0 -l '+fDesc + fSql +' %sourcevec% %destTiff%');
    end;
  end;

  writeln(GEOfile,')'); // end loop

  writeln(GEOfile,'rem convert to bitmap');
//  writeln(GEOfile,'set sourcetiff='+TileList[TileIndex].TileName+'\'+'UTMmap.tif');
  writeln(GEOfile,'set sourcetiff=bUTMmap.tif');
//  writeln(GEOfile,'set destinationbmp='+TileList[TileIndex].TileName+'.bmp');
  writeln(GEOfile,'set destinationbmp=b'+TileList[TileIndex].TileName+'.bmp');
  writeln(GEOfile,'gdal_translate -of BMP %sourcetiff% %destinationbmp%');
  writeln(GEOfile,'del %sourcetiff%');
//  writeln(GEOfile,'move %destinationbmp% ..\..\GeoDatabase');
  writeln(GEOfile,'move %destinationbmp% ..\..\Terragen\ForestMaps');
  writeln(GEOfile,'endlocal');

  // close the file
  Close(GEOfile);

  MessageShow(FileName+' done.');
end;

//-------------------------------------------------------------------------------------
Procedure MakeGEO_V2_Forest_Coniferous_batchFile(TileIndex : integer);

const
  V2_Forest_Size = '2048';

var
  i : integer;
  FileName : string;
  FilePath : string;

  Tile_B_Lat : double;
  Tile_T_Lat : double;
  Tile_L_Long : double;
  Tile_R_Long : double;

begin
  FilePath := GEOfolder +'\SourceTiles\'+ TileList[TileIndex].TileName;

  //open the file
  FileName := 'GEO_V2_s_'+TileList[TileIndex].TileName+'.bat';
  AssignFile(GEOfile, FilePath +'\'+ FileName);
  Rewrite(GEOfile);

  writeln(GEOfile,'echo off');
  writeln(GEOfile,'setlocal enabledelayedexpansion');
  writeln(GEOfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
  writeln(GEOfile,'set GDAL_DATA='+GDALlibraryFolder+'\..\share\epsg_csv');
  writeln(GEOfile,'gdalinfo --version');

  writeln(GEOfile,'rem create a GeoTiff to embed the UTM easting and northing');

  writeln(GEOfile,'rem set the size');
  writeln(GEOfile,'set image_width='+V2_Forest_Size);
  writeln(GEOfile,'set image_height='+V2_Forest_Size);

  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - 45;
  Tile_T_Lat  := Tile_B_Lat + 23040;
  Tile_L_Long  := UTM_Right +45 - TileList[TileIndex+1].TileUTMRight;
  Tile_R_Long  := Tile_L_Long + 23040;

  writeln(GEOfile,'rem crop to UTM coordinates');
  writeln(GEOfile,'set utm_zone='+UTM_Zone);
  if (UTM_ZoneNS = 'N') then begin
    writeln(GEOfile,'set utm_grid=north');
  end else begin
    writeln(GEOfile,'set utm_grid=south');
  end;
  writeln(GEOfile,format('set utm_left=%1.1f',[Tile_L_Long]));
  writeln(GEOfile,format('set utm_bottom=%1.1f',[Tile_B_Lat]));
  writeln(GEOfile,format('set utm_right=%1.1f',[Tile_R_Long]));
  writeln(GEOfile,format('set utm_top=%1.1f',[Tile_T_Lat]));
//  writeln(GEOfile,'set destTiff='+TileList[TileIndex].TileName+'\'+'UTMmap.tif');
  writeln(GEOfile,'set destTiff=sUTMmap.tif');

  writeln(GEOfile,'rem it doesn''t work the first time, but the file is created');
//  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
  writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (if not defined sourcevec set "sourcevec=..\..\GeoDatabase\%%f'+DBfolderName+'")');
  writeln(GEOfile,'echo ^!sourcevec^!');
//  writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy %sourcevec% %destTiff%');
  writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy ^!sourcevec^! %destTiff%');

if (FeatureCount_250K > 1) then begin // if there are any coniferous forests
  // loop through all ShapeFiles
  writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (');
//  writeln(GEOfile,'echo %%f');

  // must do Wooded Areas first so that roads and rivers are plotted on top !
  writeln(GEOfile,'rem do Wooded Areas first so that roads and rivers are plotted on top');
//  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
  writeln(GEOfile,'set sourcevec=../../GeoDatabase/%%f'+DBfolderName);
  writeln(GEOfile,'echo ^!sourcevec^!');
  {for i := 0 to FeatureCount_250K-1 do} i:= 1; begin // 1 only if there are any coniferous forests
    with FeatureList_250K[i] do begin
//      writeln(GEOfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn '+fColor[0]+' -burn '+fColor[1]+' -burn '+fColor[2]+' -l '+fDesc + fSql +' %sourcevec% %destTiff%');
      writeln(GEOfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn '+fColor[0]+' -burn '+fColor[1]+' -burn '+fColor[2]+' -l '+fDesc + fSql +' ^!sourcevec^! %destTiff%');
    end;
  end;
  writeln(GEOfile,'rem add the desired vector features into the raster file');
//  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
  writeln(GEOfile,'set sourcevec=../../GeoDatabase/%%f'+DBfolderName);
  writeln(GEOfile,'echo ^!sourcevec^!');
  for i := 0 to FeatureCount_50K-1 do begin
    with FeatureList_50K[i] do begin
//      writeln(GEOfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn '+fColor[0]+' -burn '+fColor[1]+' -burn '+fColor[2]+' -l '+fDesc + fSql +' %sourcevec% %destTiff%');
      // overwrite forest with black non-forest features
//      writeln(GEOfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn 0 -burn 0 -burn 0 -l '+fDesc + fSql +' %sourcevec% %destTiff%');
      writeln(GEOfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn 0 -burn 0 -burn 0 -l '+fDesc + fSql +' ^!sourcevec^! %destTiff%');
    end;
  end;

  writeln(GEOfile,')'); // end loop
end;


  writeln(GEOfile,'rem convert to bitmap');
//  writeln(GEOfile,'set sourcetiff='+TileList[TileIndex].TileName+'\'+'UTMmap.tif');
  writeln(GEOfile,'set sourcetiff=sUTMmap.tif');
//  writeln(GEOfile,'set destinationbmp='+TileList[TileIndex].TileName+'.bmp');
  writeln(GEOfile,'set destinationbmp=s'+TileList[TileIndex].TileName+'.bmp');
  writeln(GEOfile,'gdal_translate -of BMP %sourcetiff% %destinationbmp%');
  writeln(GEOfile,'del %sourcetiff%');
//  writeln(GEOfile,'move %destinationbmp% ..\..\GeoDatabase');
  writeln(GEOfile,'move %destinationbmp% ..\..\Terragen\ForestMaps');

  writeln(GEOfile,'endlocal');

  // close the file
  Close(GEOfile);

  MessageShow(FileName+' done.');
end;

// For direct generation of thermal values for MakeThermal
//-------------------------------------------------------------------------------------
Procedure MakeGEO_Thermal_batchFile(TileIndex : integer);

const
  Thermal_Size = '256';

var
  i : integer;
  FileName : string;
  FilePath : string;

  Tile_B_Lat : double;
  Tile_T_Lat : double;
  Tile_L_Long : double;
  Tile_R_Long : double;

begin
  FilePath := GEOfolder +'\SourceTiles\'+ TileList[TileIndex].TileName;

  //open the file
  FileName := 'GEO_t_'+TileList[TileIndex].TileName+'.bat';
  AssignFile(GEOfile, FilePath +'\'+ FileName);
  Rewrite(GEOfile);

  writeln(GEOfile,'echo off');
  writeln(GEOfile,'setlocal enabledelayedexpansion');
  writeln(GEOfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
  writeln(GEOfile,'set GDAL_DATA='+GDALlibraryFolder+'\..\share\epsg_csv');
  writeln(GEOfile,'gdalinfo --version');

  writeln(GEOfile,'rem create a GeoTiff to embed the UTM easting and northing');

  writeln(GEOfile,'rem set the size');
  writeln(GEOfile,'set image_width='+Thermal_Size);
  writeln(GEOfile,'set image_height='+Thermal_Size);

  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - 45;
  Tile_T_Lat  := Tile_B_Lat + 23040;
  Tile_L_Long  := UTM_Right +45 - TileList[TileIndex+1].TileUTMRight;
  Tile_R_Long  := Tile_L_Long + 23040;

  writeln(GEOfile,'rem crop to UTM coordinates');
  writeln(GEOfile,'set utm_zone='+UTM_Zone);
  if (UTM_ZoneNS = 'N') then begin
    writeln(GEOfile,'set utm_grid=north');
  end else begin
    writeln(GEOfile,'set utm_grid=south');
  end;
  writeln(GEOfile,format('set utm_left=%1.1f',[Tile_L_Long]));
  writeln(GEOfile,format('set utm_bottom=%1.1f',[Tile_B_Lat]));
  writeln(GEOfile,format('set utm_right=%1.1f',[Tile_R_Long]));
  writeln(GEOfile,format('set utm_top=%1.1f',[Tile_T_Lat]));
//  writeln(GEOfile,'set destTiff='+TileList[TileIndex].TileName+'\'+'UTMmap.tif');
  writeln(GEOfile,'set destTiff=tUTMmap.tif');

  writeln(GEOfile,'rem it doesn''t work the first time, but the file is created');
//  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
  writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (if not defined sourcevec set "sourcevec=../../GeoDatabase/%%f'+DBfolderName+'")');
  writeln(GEOfile,'echo ^!sourcevec^!');
//  writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy %sourcevec% %destTiff%');
  writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy ^!sourcevec^! %destTiff%');

  // loop through all ShapeFiles
  writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (');
//  writeln(GEOfile,'echo %%f');

  // must do Wooded Areas first so that roads and rivers are plotted on top !
  writeln(GEOfile,'rem do Wooded Areas first so that roads and rivers are plotted on top');
//  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
  writeln(GEOfile,'set sourcevec=../../GeoDatabase/%%f'+DBfolderName);
  writeln(GEOfile,'echo ^!sourcevec^!');
  for i := 0 to FeatureCount_250K-1 do begin
    with FeatureList_250K[i] do begin
      writeln(GEOfile,'gdal_rasterize -b 1 -b 2 -b 3 ' +
        format('-burn %d -burn %d -burn %d',[ftColor.cRGB.rgbtRed,ftColor.cRGB.rgbtGreen,ftColor.cRGB.rgbtBlue]) +
//        ' -l ' + fDesc + fSql +' %sourcevec% %destTiff%');
        ' -l ' + fDesc + fSql +' ^!sourcevec^! %destTiff%');
    end;
  end;
  writeln(GEOfile,'rem add the desired vector features into the raster file');
//  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
  writeln(GEOfile,'set sourcevec=../../GeoDatabase/%%f'+DBfolderName);
  writeln(GEOfile,'echo ^!sourcevec^!');
  for i := 0 to FeatureCount_50K-1 do begin
    with FeatureList_50K[i] do begin
      writeln(GEOfile,'gdal_rasterize -b 1 -b 2 -b 3 ' +
        format('-burn %d -burn %d -burn %d',[ftColor.cRGB.rgbtRed,ftColor.cRGB.rgbtGreen,ftColor.cRGB.rgbtBlue]) +
//      ' -l '+fDesc + fSql +' %sourcevec% %destTiff%');
      ' -l '+fDesc + fSql +' ^!sourcevec^! %destTiff%');
    end;
  end;

  writeln(GEOfile,')'); // end loop

  writeln(GEOfile,'rem convert to bitmap');
//  writeln(GEOfile,'set sourcetiff='+TileList[TileIndex].TileName+'\'+'UTMmap.tif');
  writeln(GEOfile,'set sourcetiff=tUTMmap.tif');
//  writeln(GEOfile,'set destinationbmp='+TileList[TileIndex].TileName+'.bmp');
//  writeln(GEOfile,'set destinationbmp=t'+TileList[TileIndex].TileName+'.bmp');
  writeln(GEOfile,'set destinationbmp='+TileList[TileIndex].TileName+'_t.bmp');
  writeln(GEOfile,'gdal_translate -of BMP %sourcetiff% %destinationbmp%');
  writeln(GEOfile,'del %sourcetiff%');
//  writeln(GEOfile,'move %destinationbmp% ..\..\GeoDatabase');

  writeln(GEOfile,'endlocal');

  // close the file
  Close(GEOfile);

  MessageShow(FileName+' done.');
end;

// changed EPSG followed by UTM to work with some databases that didn't work
// with direct UTM rasterize
// not as good since lines are 'warped' to UTM after
//-------------------------------------------------------------------------------------
Procedure MakeGEO_V2_Water_batchFile(TileIndex : integer);

const
  ExtraDist = 0.1;  // extra 100 metres on each edge

var
  i : integer;
  FileName : string;
  FilePath : string;

  Tile_B_Lat : double;
  Tile_T_Lat : double;
  Tile_L_Long : double;
  Tile_R_Long : double;

  Xsize,Ysize : real;

begin
  FilePath := GEOfolder +'\SourceTiles\'+ TileList[TileIndex].TileName;

  //open the file
  FileName := 'GEO_V2_w_'+TileList[TileIndex].TileName+'.bat';
  AssignFile(GEOfile, FilePath +'\'+ FileName);
  Rewrite(GEOfile);

  writeln(GEOfile,'echo off');
  // enable delayed expansion for for loops with !sourcevec!; also need ^! instead of %%
  writeln(GEOfile,'setlocal enabledelayedexpansion');
  writeln(GEOfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
  writeln(GEOfile,'set GDAL_DATA='+GDALlibraryFolder+'\..\share\epsg_csv');
  writeln(GEOfile,'gdalinfo --version');

  writeln(GEOfile,'rem create a GeoTiff to embed the UTM easting and northing');

  writeln(GEOfile,'rem set the size');
  writeln(GEOfile,'set image_width='+OutputTileSize);
  writeln(GEOfile,'set image_height='+OutputTileSize);

  // extra margin to make sure all area is included
  Ysize := arctan(ExtraDist/earthRadius)*180.0/Pi;
  Xsize := Ysize*cos(TileList[TileIndex].TileLatBottom*Pi/180);

  // tile corners
  Tile_B_Lat := f_Minimum(TileList[TileIndex].TileLatBottom,
    TileList[TileIndex+1].TileLatBottom);

  Tile_T_Lat := f_Maximum(TileList[TileIndex+1+TileColumnCount].TileLatBottom,
    TileList[TileIndex+1+TileColumnCount+1].TileLatBottom);

  Tile_L_Long := f_Minimum(TileList[TileIndex+1+TileColumnCount+1].TileLongRight,
    TileList[TileIndex+1].TileLongRight);

  Tile_R_Long := f_Maximum(TileList[TileIndex].TileLongRight,
    TileList[TileIndex+1+TileColumnCount].TileLongRight);

  writeln(GEOfile,'rem set the range');
  writeln(GEOfile,'set real_left='+format('%1.8f',[Tile_L_Long - Xsize]));
  writeln(GEOfile,'set real_top='+format('%1.8f',[Tile_T_Lat + Ysize]));
  writeln(GEOfile,'set real_right='+format('%1.8f',[Tile_R_Long + Xsize]));
  writeln(GEOfile,'set real_bottom='+format('%1.8f',[Tile_B_Lat - Ysize]));
  writeln(GEOfile,'set destTiff=gEPSGmap.tif');

  writeln(GEOfile,'rem it doesn''t work the first time, but the file is created');
  writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (if not defined sourcevec set "sourcevec=..\..\GeoDatabase\%%f'+DBfolderName+'")');
  writeln(GEOfile,'echo ^!sourcevec^!');
  writeln(GEOfile,'gdal_rasterize -init 255 -ot Byte -ts %image_width% %image_height% -te %real_left% %real_bottom% %real_right% %real_top% -of GTiff -a_srs EPSG:4326 -burn 255 -l dummy ^!sourcevec^! %destTiff%');

  // loop through all ShapeFiles
  writeln(GEOfile,'rem loop through all *.SHP folders');
  writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (');
  writeln(GEOfile,'set sourcevec=../../GeoDatabase/%%f'+DBfolderName);
  writeln(GEOfile,'echo ^!sourcevec^!');
  for i := 0 to FeatureCount_Water-1 do begin
    with FeatureList_Water[i] do begin
      writeln(GEOfile,'gdal_rasterize -b 1 -burn 191 -l '+fDesc + fSql + fOpt +' ^!sourcevec^! %destTiff%'); // half transparency
    end;
  end;
//  writeln(GEOfile,'gdal_rasterize -b 1 -burn 191 -l water_polygons ^!sourcevec^! %destTiff% --config OGR_ENABLE_PARTIAL_REPROJECTION TRUE');
  writeln(GEOfile,')'); // end loop

  // now convert to UTM
  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - 45;
  Tile_T_Lat  := Tile_B_Lat + 23040;
  Tile_L_Long  := UTM_Right +45 - TileList[TileIndex+1].TileUTMRight;
  Tile_R_Long  := Tile_L_Long + 23040;

  writeln(GEOfile,'rem convert to UTM coordinates');
  writeln(GEOfile,'set utm_zone='+UTM_Zone);
  if (UTM_ZoneNS = 'N') then begin
    writeln(GEOfile,'set utm_grid=north');
  end else begin
    writeln(GEOfile,'set utm_grid=south');
  end;
  writeln(GEOfile,format('set utm_left=%1.1f',[Tile_L_Long]));
  writeln(GEOfile,format('set utm_bottom=%1.1f',[Tile_B_Lat]));
  writeln(GEOfile,format('set utm_right=%1.1f',[Tile_R_Long]));
  writeln(GEOfile,format('set utm_top=%1.1f',[Tile_T_Lat]));

  writeln(GEOfile,'set sourcetiff=gEPSGmap.tif');
  writeln(GEOfile,'set destinationtiff=gUTMmap.tif');

  writeln(GEOfile,'rem convert, with cropping, and re-sizing');
  writeln(GEOfile,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
  writeln(GEOfile,'gdalwarp.exe -of GTiff -t_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% %sourcetiff% %destinationtiff%');
  writeln(GEOfile,'del %sourcetiff%');

  // finally convert to bitmap for use as an 'alpha' tranparency
  writeln(GEOfile,'rem convert to bitmap');
//  writeln(GEOfile,'set sourcetiff='+TileList[TileIndex].TileName+'\'+'UTMmap.tif');
  writeln(GEOfile,'set sourcetiff=gUTMmap.tif');
  writeln(GEOfile,'set destinationbmp='+TileList[TileIndex].TileName+'.bmp');
  writeln(GEOfile,'gdal_translate -of BMP %sourcetiff% %destinationbmp%');
  writeln(GEOfile,'del %sourcetiff%');
  writeln(GEOfile,'move %destinationbmp% ..\..\Terragen\WaterMaps');

  writeln(GEOfile,'endlocal');

  // close the file
  Close(GEOfile);
  MessageShow(FileName+' done.');
end;

// changed EPSG followed by UTM to work with some databases that didn't work
// with direct UTM rasterize
// not as good since lines are 'warped' to UTM after
//-------------------------------------------------------------------------------------
Procedure xMakeGEO_V2_Water_batchFile(TileIndex : integer);

var
  i : integer;
  FileName : string;
  FilePath : string;

  Tile_B_Lat : double;
  Tile_T_Lat : double;
  Tile_L_Long : double;
  Tile_R_Long : double;

begin
  FilePath := GEOfolder +'\SourceTiles\'+ TileList[TileIndex].TileName;

  //open the file
  FileName := 'GEO_V2_w_'+TileList[TileIndex].TileName+'.bat';
  AssignFile(GEOfile, FilePath +'\'+ FileName);
  Rewrite(GEOfile);

  writeln(GEOfile,'echo off');
  // enable delayed expansion for for loops with !sourcevec!; also need ^! instead of %%
  writeln(GEOfile,'setlocal enabledelayedexpansion');
  writeln(GEOfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
  writeln(GEOfile,'set GDAL_DATA='+GDALlibraryFolder+'\..\share\epsg_csv');
  writeln(GEOfile,'gdalinfo --version');

  writeln(GEOfile,'rem create a GeoTiff to embed the UTM easting and northing');

  writeln(GEOfile,'rem set the size');
  writeln(GEOfile,'set image_width='+OutputTileSize);
  writeln(GEOfile,'set image_height='+OutputTileSize);

  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - 45;
  Tile_T_Lat  := Tile_B_Lat + 23040;
  Tile_L_Long  := UTM_Right +45 - TileList[TileIndex+1].TileUTMRight;
  Tile_R_Long  := Tile_L_Long + 23040;

  writeln(GEOfile,'rem crop to UTM coordinates');
  writeln(GEOfile,'set utm_zone='+UTM_Zone);
  if (UTM_ZoneNS = 'N') then begin
    writeln(GEOfile,'set utm_grid=north');
  end else begin
    writeln(GEOfile,'set utm_grid=south');
  end;
  writeln(GEOfile,format('set utm_left=%1.1f',[Tile_L_Long]));
  writeln(GEOfile,format('set utm_bottom=%1.1f',[Tile_B_Lat]));
  writeln(GEOfile,format('set utm_right=%1.1f',[Tile_R_Long]));
  writeln(GEOfile,format('set utm_top=%1.1f',[Tile_T_Lat]));
//  writeln(GEOfile,'set destTiff='+TileList[TileIndex].TileName+'\'+'UTMmap.tif');
  writeln(GEOfile,'set destTiff=gUTMmap.tif');

  writeln(GEOfile,'rem it doesn''t work the first time, but the file is created');
//  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
  writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (if not defined sourcevec set "sourcevec=..\..\GeoDatabase\%%f'+DBfolderName+'")');
  writeln(GEOfile,'echo ^!sourcevec^!');
//  writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy %sourcevec% %destTiff%');
//  writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy ^!sourcevec^! %destTiff%');
  writeln(GEOfile,'gdal_rasterize -init 255 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -l dummy ^!sourcevec^! %destTiff%');

  // loop through all ShapeFiles
  writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (');
//  writeln(GEOfile,'echo %%f');

//  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
  writeln(GEOfile,'set sourcevec=../../GeoDatabase/%%f'+DBfolderName);
  writeln(GEOfile,'echo ^!sourcevec^!');
  for i := 0 to FeatureCount_Water-1 do begin
    with FeatureList_Water[i] do begin
//      writeln(GEOfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn '+fColor[0]+' -burn '+fColor[1]+' -burn '+fColor[2]+' -l '+fDesc + fSql +' %sourcevec% %destTiff%');
//      writeln(GEOfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn '+fColor[0]+' -burn '+fColor[1]+' -burn '+fColor[2]+' -l '+fDesc + fSql +' ^!sourcevec^! %destTiff%');
//      writeln(GEOfile,'gdal_rasterize -b 1 -burn 0 -l '+fDesc + fSql +' ^!sourcevec^! %destTiff%');  // full transparency
      writeln(GEOfile,'gdal_rasterize -b 1 -burn 191 -l '+fDesc + fSql +' ^!sourcevec^! %destTiff%'); // half transparency
    end;
  end;

  writeln(GEOfile,')'); // end loop

  writeln(GEOfile,'rem convert to bitmap');
//  writeln(GEOfile,'set sourcetiff='+TileList[TileIndex].TileName+'\'+'UTMmap.tif');
  writeln(GEOfile,'set sourcetiff=gUTMmap.tif');
  writeln(GEOfile,'set destinationbmp='+TileList[TileIndex].TileName+'.bmp');
  writeln(GEOfile,'gdal_translate -of BMP %sourcetiff% %destinationbmp%');
  writeln(GEOfile,'del %sourcetiff%');
  writeln(GEOfile,'move %destinationbmp% ..\..\Terragen\WaterMaps');

  writeln(GEOfile,'endlocal');

  // close the file
  Close(GEOfile);
  MessageShow(FileName+' done.');
end;

{----------------------------------------------------------------------------}
begin
  Memo_Message := nil;
end.

//--- End of file ---------------------------------------------------------------------


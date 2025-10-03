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
- for GDAL prior to version 3.x the first run of gdal_rasterize fails but
creates a blank tif filethat is used thesecond time
- an alternate is to create a blank bitmap and use gdal_translate to add the
geo-referenced data
- For GDAL from 3.x, there's a gdal-create function that can be used to create
a geo-referenced balnk tif.
----------------------------------------------------------------------------}
interface

uses
  StdCtrls, FileCtrl, SysUtils;

const
  ggSize : integer = 4096; // generic
  gtSize : integer = 256; // thermal
  gfV1Size : integer = 512; // V1 forest
  gfV2Size : integer = 2048; // V2 forest

  GEO_F_Name : array [0..6-1] of string = ('GEO_', 'GEO_t_', 'GEO_V1_f_',
    'GEO_V2_b_','GEO_V2_s_','GEO_V2_w_');

type
  geoType = (gGeneric, gThermal, gV1forest,
    gV2decideous, gV2coniferous, gWater);

var
  Memo_Message : TMemo;  // external TMemo for messages
  GEOFolder : string;   // external path for file output
  GDALlibraryFolder : string; // external path for library
  OutputTileSize : string;
  GeoDatabaseType : (OSM, CanVec, GLC);
  File_Destination : string;
  ApplicationPath : string;
  WGETfolder : string;               // external path for Wget

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

//Procedure MakeGEO_GLC_Thermal_batchFile(TileIndex : integer);
Procedure MakeGEO_GLC_batchFile(TileIndex: integer;
  gType : geoType; Size : integer);
Procedure MakeGEO_GLC_Wget;
Procedure MakeGEO_Blank_GreyScale(FileName : string; Size : integer; value : byte);
Procedure MakeGEO_Blank_24bit(FileName : string; Width, Height : integer; value : byte);
Procedure MakeGEO_GLC_Blank(TileIndex : integer;
  gType : geoType; Size : integer);

//----------------------------------------------------------------------------
implementation

uses
  Windows, Graphics, Math,
  u_Terrain, u_TileList, u_SceneryHDR, u_BMP, u_Util, u_UTM;

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

//-------------------------------------------------------------------------------------
Procedure Make_OGRinfo;
var
  Name : string;
  FilePath : string;
  FileName : string;

begin
  case GeoDatabaseType of
    CanVec: begin
      Name := 'CanVec'
    end;
    OSM: begin
      Name := 'CanVec'
    end;
    else begin
    end;
  end;
  if (GeoDatabaseType = GLC) then begin
    exit;
  end;
  FilePath := GEOFolder +'\GeoDatabase';
  //open the file
  FileName := 'OGR_info_'+Name+'.bat';
  AssignFile(GEOfile, FilePath +'\'+ FileName);
  Rewrite(GEOfile);

  writeln(GEOfile,'echo off');
  // enable delayed expansion for for loops
  writeln(GEOfile,'setlocal enabledelayedexpansion');
  writeln(GEOfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
//  writeln(GEOfile,'set GDAL_DATA='+GDALlibraryFolder+'\..\share\epsg_csv');

  if (GeoDatabaseType = CanVec) then begin
    writeln(GEOfile,'ogrinfo fme.ogr\dataset\canvec.gdb > OGR.txt');
  end else begin // OSM
    writeln(GEOfile,'del OGR.txt');
    writeln(GEOfile,'for /F %%f in (''dir /b *.shp'') do (');
    writeln(GEOfile,'ogrinfo -so %%f >> OGR.txt');
    writeln(GEOfile,')');
  end;
  writeln(GEOfile,'more OGR.txt');
  writeln(GEOfile,'endlocal');
  // close the file
  Close(GEOfile);
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
  Make_OGRinfo;
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
//  if (NOT DirectoryExists(FilePath)) then begin
//    mkdir(FilePath);
//  end;
//    ForceDirectories(FilePath);

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

{
- using a dummy fail run the first time
  - set destTiff=tUTMmap.tif
  - set sourcevec=../../GeoDatabase/'+DBfolderName
  -gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy %sourcevec% %destTiff%
- using a blank BMP of right size to start
  - set destTiff=tUTMmap.tif
  - set sourcetiff=..\..\Blank_correctsize.bmp
  - gdal_translate.exe -r near -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +datum=WGS84" -a_ullr %utm_left% %utm_top% %utm_right% %utm_bottom% %sourcetiff% %destTiff%
- using a blank BMP of arbitrary size to start - more flexible
  - set destTiff=tUTMmap.tif
  - set sourcetiff=..\..\Blank_256_blk.bmp
  - gdal_translate.exe -r near -of GTiff -outsize %image_width% %image_height% -a_srs "+proj=utm +zone=%utm_zone% +datum=WGS84" -a_ullr %utm_left% %utm_top% %utm_right% %utm_bottom% %sourcetiff% %destTiff%
- using new gdal_create i.e if (fileExists(GDALlibraryFolder+'\gdal_create.exe'))
  - set destTiff=tUTMmap.tif
  - gdal_create -of GTiff -outsize %image_width% %image_height% -bands 3 -burn 0 -a_srs "+proj=utm +zone=%utm_zone% +datum=WGS84" -a_ullr %utm_left% %utm_top% %utm_right% %utm_bottom% %destTiff%
}
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
{  // check for folder
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
}
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
  // suppres generation of .xml file
  writeln(GEOfile,'set GDAL_PAM_ENABLED=NO');
  writeln(GEOfile,'gdalinfo --version');

  writeln(GEOfile,'rem create a GeoTiff to embed the UTM easting and northing');

  writeln(GEOfile,'rem set the size');
//  writeln(GEOfile,'set image_width='+OutputTileSize);
  writeln(GEOfile,'set image_width=1024');
//  writeln(GEOfile,'set image_height='+OutputTileSize);
  writeln(GEOfile,'set image_height=1024');

  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - Legacy_Offset;
  Tile_T_Lat  := Tile_B_Lat + 23040;
  Tile_L_Long  := UTM_Right + Legacy_Offset - TileList[TileIndex+1].TileUTMRight;
  Tile_R_Long  := Tile_L_Long + 23040;

  writeln(GEOfile,'rem crop to UTM coordinates');
//  writeln(GEOfile,'set utm_zone='+UTM_Zone);
  writeln(GEOfile,format('set utm_zone=%d',[UTM_Zone]));
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

  // if new GDAL libary, use gdal_create
  if (fileExists(GDALlibraryFolder+'\gdal_create.exe')) then begin
    writeln(GEOfile,'rem create tif file');
    writeln(GEOfile,'gdal_create -of GTiff -outsize %image_width% %image_height% -bands 3 -burn 0 -a_srs "+proj=utm +zone=%utm_zone% +datum=WGS84" -a_ullr %utm_left% %utm_top% %utm_right% %utm_bottom% %destTiff%');
  end else begin
{
    // use old way with 'dummy' that creates the file
    writeln(GEOfile,'rem it doesn''t work the first time, but the file is created');
  //  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
    writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (if not defined sourcevec set "sourcevec=..\..\GeoDatabase\%%f'+DBfolderName+'")');
    writeln(GEOfile,'echo ^!sourcevec^!');
  //  writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy %sourcevec% %destTiff%');
    writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy ^!sourcevec^! %destTiff%');
}
//    // create a blank bitmap in GEO folder
//    MakeGEO_Blank_24bit(GEOFolder+'\GeoDatabase\Blank_256_blk.bmp', 256);
    writeln(GEOfile,'rem create tif file from a blank bmp file');
    writeln(GEOfile,'set sourcetiff=..\..\GeoDatabase\Blank_256_blk.bmp');
    writeln(GEOfile,'gdal_translate.exe -r near -of GTiff -outsize %image_width% %image_height% -a_srs "+proj=utm +zone=%utm_zone% +datum=WGS84" -a_ullr %utm_left% %utm_top% %utm_right% %utm_bottom% %sourcetiff% %destTiff%');
  end;
//  writeln(GEOfile,'gdalinfo %destTiff% > info.txt');

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
  // suppres generation of .xml file
  writeln(GEOfile,'set GDAL_PAM_ENABLED=NO');
  writeln(GEOfile,'gdalinfo --version');

  writeln(GEOfile,'rem create a GeoTiff to embed the UTM easting and northing');

  writeln(GEOfile,'rem set the size');
  writeln(GEOfile,'set image_width='+V1_Forest_Size);
  writeln(GEOfile,'set image_height='+V1_Forest_Size);

  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - Legacy_Offset;
  Tile_T_Lat  := Tile_B_Lat + 23040;
  Tile_L_Long  := UTM_Right + Legacy_Offset - TileList[TileIndex+1].TileUTMRight;
  Tile_R_Long  := Tile_L_Long + 23040;

  writeln(GEOfile,'rem crop to UTM coordinates');
//  writeln(GEOfile,'set utm_zone='+UTM_Zone);
  writeln(GEOfile,format('set utm_zone=%d',[UTM_Zone]));
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

  // if new GDAL libary, use gdal_create
  if (fileExists(GDALlibraryFolder+'\gdal_create.exe')) then begin
    writeln(GEOfile,'rem create tif file');
    writeln(GEOfile,'gdal_create -of GTiff -outsize %image_width% %image_height% -bands 3 -burn 0 -a_srs "+proj=utm +zone=%utm_zone% +datum=WGS84" -a_ullr %utm_left% %utm_top% %utm_right% %utm_bottom% %destTiff%');
//    writeln(GEOfile,'gdalinfo tUTMmap.tif > info.txt');
  end else begin
{    // use old way with 'dummy' that creates the file
    writeln(GEOfile,'rem it doesn''t work the first time, but the file is created');
  //  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
    writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (if not defined sourcevec set "sourcevec=..\..\GeoDatabase\%%f'+DBfolderName+'")');
    writeln(GEOfile,'echo ^!sourcevec^!');
  //  writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy %sourcevec% %destTiff%');
    writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy ^!sourcevec^! %destTiff%');
}
//    // create a blank bitmap in GEO folder
//    MakeGEO_Blank_24bit(GEOFolder+'\GeoDatabase\Blank_256_blk.bmp', 256);
    writeln(GEOfile,'rem create tif file from a blank bmp file');
    writeln(GEOfile,'set sourcetiff=..\..\GeoDatabase\Blank_256_blk.bmp');
    writeln(GEOfile,'gdal_translate.exe -r near -of GTiff -outsize %image_width% %image_height% -a_srs "+proj=utm +zone=%utm_zone% +datum=WGS84" -a_ullr %utm_left% %utm_top% %utm_right% %utm_bottom% %sourcetiff% %destTiff%');
  end;
//  writeln(GEOfile,'gdalinfo %destTiff% > info.txt');

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
  // suppres generation of .xml file
  writeln(GEOfile,'set GDAL_PAM_ENABLED=NO');
  writeln(GEOfile,'gdalinfo --version');

  writeln(GEOfile,'rem create a GeoTiff to embed the UTM easting and northing');

  writeln(GEOfile,'rem set the size');
  writeln(GEOfile,'set image_width='+V2_Forest_Size);
  writeln(GEOfile,'set image_height='+V2_Forest_Size);

  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - Legacy_Offset;
  Tile_T_Lat  := Tile_B_Lat + 23040;
  Tile_L_Long  := UTM_Right + Legacy_Offset - TileList[TileIndex+1].TileUTMRight;
  Tile_R_Long  := Tile_L_Long + 23040;

  writeln(GEOfile,'rem crop to UTM coordinates');
//  writeln(GEOfile,'set utm_zone='+UTM_Zone);
  writeln(GEOfile,format('set utm_zone=%d',[UTM_Zone]));
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

  // if new GDAL libary, use gdal_create
  if (fileExists(GDALlibraryFolder+'\gdal_create.exe')) then begin
    writeln(GEOfile,'rem create tif file');
    writeln(GEOfile,'gdal_create -of GTiff -outsize %image_width% %image_height% -bands 3 -burn 0 -a_srs "+proj=utm +zone=%utm_zone% +datum=WGS84" -a_ullr %utm_left% %utm_top% %utm_right% %utm_bottom% %destTiff%');
//    writeln(GEOfile,'gdalinfo tUTMmap.tif > info.txt');
  end else begin
{    // use old way with 'dummy' that creates the file
    writeln(GEOfile,'rem it doesn''t work the first time, but the file is created');
  //  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
    writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (if not defined sourcevec set "sourcevec=..\..\GeoDatabase\%%f'+DBfolderName+'")');
    writeln(GEOfile,'echo ^!sourcevec^!');
  //  writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy %sourcevec% %destTiff%');
    writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy ^!sourcevec^! %destTiff%');
}
//    // create a blank bitmap in GEO folder
//    MakeGEO_Blank_24bit(GEOFolder+'\GeoDatabase\Blank_256_blk.bmp', 256);
    writeln(GEOfile,'rem create tif file from a blank bmp file');
    writeln(GEOfile,'set sourcetiff=..\..\GeoDatabase\Blank_256_blk.bmp');
    writeln(GEOfile,'gdal_translate.exe -r near -of GTiff -outsize %image_width% %image_height% -a_srs "+proj=utm +zone=%utm_zone% +datum=WGS84" -a_ullr %utm_left% %utm_top% %utm_right% %utm_bottom% %sourcetiff% %destTiff%');
  end;
//  writeln(GEOfile,'gdalinfo %destTiff% > info.txt');
  
  // loop through all ShapeFiles
  writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (');
//  writeln(GEOfile,'echo %%f');

  // must do Wooded Areas first so that roads and rivers are plotted on top !
  writeln(GEOfile,'rem do Wooded Areas first so that roads and rivers are plotted on top');
//  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
  writeln(GEOfile,'set sourcevec=../../GeoDatabase/%%f'+DBfolderName);
  writeln(GEOfile,'echo ^!sourcevec^!');
  {for i := 0 to FeatureCount_250K-1 do} i:= 0; begin  // 0 only for deciduous forest
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
  // suppres generation of .xml file
  writeln(GEOfile,'set GDAL_PAM_ENABLED=NO');
  writeln(GEOfile,'gdalinfo --version');

  writeln(GEOfile,'rem create a GeoTiff to embed the UTM easting and northing');

  writeln(GEOfile,'rem set the size');
  writeln(GEOfile,'set image_width='+V2_Forest_Size);
  writeln(GEOfile,'set image_height='+V2_Forest_Size);

  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - Legacy_Offset;
  Tile_T_Lat  := Tile_B_Lat + 23040;
  Tile_L_Long  := UTM_Right + Legacy_Offset - TileList[TileIndex+1].TileUTMRight;
  Tile_R_Long  := Tile_L_Long + 23040;

  writeln(GEOfile,'rem crop to UTM coordinates');
//  writeln(GEOfile,'set utm_zone='+UTM_Zone);
  writeln(GEOfile,format('set utm_zone=%d',[UTM_Zone]));
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

  // if new GDAL libary, use gdal_create
  if (fileExists(GDALlibraryFolder+'\gdal_create.exe')) then begin
    writeln(GEOfile,'rem create tif file');
    writeln(GEOfile,'gdal_create -of GTiff -outsize %image_width% %image_height% -bands 3 -burn 0 -a_srs "+proj=utm +zone=%utm_zone% +datum=WGS84" -a_ullr %utm_left% %utm_top% %utm_right% %utm_bottom% %destTiff%');
//    writeln(GEOfile,'gdalinfo tUTMmap.tif > info.txt');
  end else begin
{    // use old way with 'dummy' that creates the file
    writeln(GEOfile,'rem it doesn''t work the first time, but the file is created');
  //  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
    writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (if not defined sourcevec set "sourcevec=..\..\GeoDatabase\%%f'+DBfolderName+'")');
    writeln(GEOfile,'echo ^!sourcevec^!');
  //  writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy %sourcevec% %destTiff%');
    writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy ^!sourcevec^! %destTiff%');
}
//    // create a blank bitmap in GEO folder
//    MakeGEO_Blank_24bit(GEOFolder+'\GeoDatabase\Blank_256_blk.bmp', 256);
    writeln(GEOfile,'rem create tif file from a blank bmp file');
    writeln(GEOfile,'set sourcetiff=..\..\GeoDatabase\Blank_256_blk.bmp');
    writeln(GEOfile,'gdal_translate.exe -r near -of GTiff -outsize %image_width% %image_height% -a_srs "+proj=utm +zone=%utm_zone% +datum=WGS84" -a_ullr %utm_left% %utm_top% %utm_right% %utm_bottom% %sourcetiff% %destTiff%');
  end;
//  writeln(GEOfile,'gdalinfo %destTiff% > info.txt');

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
  // suppres generation of .xml file
  writeln(GEOfile,'set GDAL_PAM_ENABLED=NO');
  writeln(GEOfile,'gdalinfo --version');

  writeln(GEOfile,'rem create a GeoTiff to embed the UTM easting and northing');

  writeln(GEOfile,'rem set the size');
  writeln(GEOfile,'set image_width='+Thermal_Size);
  writeln(GEOfile,'set image_height='+Thermal_Size);

  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - Legacy_Offset;
  Tile_T_Lat  := Tile_B_Lat + 23040;
  Tile_L_Long  := UTM_Right + Legacy_Offset - TileList[TileIndex+1].TileUTMRight;
  Tile_R_Long  := Tile_L_Long + 23040;

  writeln(GEOfile,'rem crop to UTM coordinates');
//  writeln(GEOfile,'set utm_zone='+UTM_Zone);
  writeln(GEOfile,format('set utm_zone=%d',[UTM_Zone]));
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

  // if new GDAL libary, use gdal_create
  if (fileExists(GDALlibraryFolder+'\gdal_create.exe')) then begin
    writeln(GEOfile,'rem create tif file');
    writeln(GEOfile,'gdal_create -of GTiff -outsize %image_width% %image_height% -bands 3 -burn 0 -a_srs "+proj=utm +zone=%utm_zone% +datum=WGS84" -a_ullr %utm_left% %utm_top% %utm_right% %utm_bottom% %destTiff%');
//    writeln(GEOfile,'gdalinfo tUTMmap.tif > info.txt');
  end else begin
{    // use old way with 'dummy' that creates the file
    writeln(GEOfile,'rem it doesn''t work the first time, but the file is created');
  //  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
    writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (if not defined sourcevec set "sourcevec=../../GeoDatabase/%%f'+DBfolderName+'")');
    writeln(GEOfile,'echo ^!sourcevec^!');
  //  writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy %sourcevec% %destTiff%');
    writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy ^!sourcevec^! %destTiff%');
}
//    // create a blank bitmap in GEO folder
//    MakeGEO_Blank_24bit(GEOFolder+'\GeoDatabase\Blank_256_blk.bmp', 256);
    writeln(GEOfile,'rem create tif file from a blank bmp file');
    writeln(GEOfile,'set sourcetiff=..\..\GeoDatabase\Blank_256_blk.bmp');
    writeln(GEOfile,'gdal_translate.exe -r near -of GTiff -outsize %image_width% %image_height% -a_srs "+proj=utm +zone=%utm_zone% +datum=WGS84" -a_ullr %utm_left% %utm_top% %utm_right% %utm_bottom% %sourcetiff% %destTiff%');
  end;
//  writeln(GEOfile,'gdalinfo %destTiff% > info.txt');
  
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
Procedure xMakeGEO_V2_Water_batchFile(TileIndex : integer);

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
  // suppres generation of .xml file
  writeln(GEOfile,'set GDAL_PAM_ENABLED=NO');
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

  // if new GDAL libary, use gdal_create
  if (fileExists(GDALlibraryFolder+'\gdal_create.exe')) then begin
    writeln(GEOfile,'rem create tif file');
    writeln(GEOfile,'gdal_create -of GTiff -outsize %image_width% %image_height% -bands 1 -burn 255 -a_srs EPSG:4326 -a_ullr %real_left% %real_top% %real_right% %real_bottom% %destTiff%');
//    writeln(GEOfile,'gdalinfo tUTMmap.tif > info.txt');
  end else begin
{    // use old way with 'dummy' that creates the file
    writeln(GEOfile,'rem it doesn''t work the first time, but the file is created');
    writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (if not defined sourcevec set "sourcevec=..\..\GeoDatabase\%%f'+DBfolderName+'")');
    writeln(GEOfile,'echo ^!sourcevec^!');
    writeln(GEOfile,'gdal_rasterize -init 255 -ot Byte -ts %image_width% %image_height% -te %real_left% %real_bottom% %real_right% %real_top% -of GTiff -a_srs EPSG:4326 -burn 255 -l dummy ^!sourcevec^! %destTiff%');
}
//    // create a blank bitmap in GEO folder
//    MakeGEO_Blank_24bit(GEOFolder+'\GeoDatabase\Blank_256_blk.bmp', 256);
    writeln(GEOfile,'rem create tif file from a blank bmp file');
    writeln(GEOfile,'set sourcetiff=..\..\GeoDatabase\Blank_256_wht.bmp');
    writeln(GEOfile,'gdal_translate.exe -r near -of GTiff -outsize %image_width% %image_height% -a_srs EPSG:4326 -a_ullr %real_left% %real_top% %real_right% %real_bottom% %sourcetiff% %destTiff%');
  end;
//  writeln(GEOfile,'gdalinfo %destTiff% > info.txt');

  // loop through all ShapeFiles
//  writeln(GEOfile,'rem loop through all database folders');
  writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (');
  writeln(GEOfile,'set sourcevec=../../GeoDatabase/%%f'+DBfolderName);
  writeln(GEOfile,'echo ^!sourcevec^!');
  for i := 0 to FeatureCount_Water-1 do begin
    with FeatureList_Water[i] do begin
//      writeln(GEOfile,'gdal_rasterize -b 1 -burn 191 -l '+fDesc + fSql + fOpt +' ^!sourcevec^! %destTiff%'); // half transparency
      writeln(GEOfile,'gdal_rasterize -b 1 -burn 0 -l '+fDesc + fSql + fOpt +' ^!sourcevec^! %destTiff%'); // half transparency
    end;
  end;
////  writeln(GEOfile,'gdal_rasterize -b 1 -burn 191 -l water_polygons ^!sourcevec^! %destTiff% --config OGR_ENABLE_PARTIAL_REPROJECTION TRUE');
//  writeln(GEOfile,'gdal_rasterize -b 1 -burn 0 -l water_polygons ^!sourcevec^! %destTiff% --config OGR_ENABLE_PARTIAL_REPROJECTION TRUE');
  writeln(GEOfile,')'); // end loop

  // now convert to UTM
  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - Legacy_Offset;
  Tile_T_Lat  := Tile_B_Lat + 23040;
  Tile_L_Long  := UTM_Right + Legacy_Offset - TileList[TileIndex+1].TileUTMRight;
  Tile_R_Long  := Tile_L_Long + 23040;

  writeln(GEOfile,'rem convert to UTM coordinates');
//  writeln(GEOfile,'set utm_zone='+UTM_Zone);
  writeln(GEOfile,format('set utm_zone=%d',[UTM_Zone]));
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
Procedure MakeGEO_V2_Water_batchFile(TileIndex : integer);

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
  // suppres generation of .xml file
  writeln(GEOfile,'set GDAL_PAM_ENABLED=NO');
  writeln(GEOfile,'gdalinfo --version');

  writeln(GEOfile,'rem create a GeoTiff to embed the UTM easting and northing');

  writeln(GEOfile,'rem set the size');
  writeln(GEOfile,'set image_width='+OutputTileSize);
  writeln(GEOfile,'set image_height='+OutputTileSize);

  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - Legacy_Offset;
  Tile_T_Lat  := Tile_B_Lat + 23040;
  Tile_L_Long  := UTM_Right + Legacy_Offset - TileList[TileIndex+1].TileUTMRight;
  Tile_R_Long  := Tile_L_Long + 23040;

  writeln(GEOfile,'rem crop to UTM coordinates');
//  writeln(GEOfile,'set utm_zone='+UTM_Zone);
  writeln(GEOfile,format('set utm_zone=%d',[UTM_Zone]));
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

  // if new GDAL libary, use gdal_create
  if (fileExists(GDALlibraryFolder+'\gdal_create.exe')) then begin
    writeln(GEOfile,'rem create tif file');
    writeln(GEOfile,'gdal_create -of GTiff -outsize %image_width% %image_height% -bands 1 -burn 255 -a_srs "+proj=utm +zone=%utm_zone% +datum=WGS84" -a_ullr %utm_left% %utm_top% %utm_right% %utm_bottom% %destTiff%');
//    writeln(GEOfile,'gdalinfo tUTMmap.tif > info.txt');
  end else begin
{    // use old way with 'dummy' that creates the file
    writeln(GEOfile,'rem it doesn''t work the first time, but the file is created');
  //  writeln(GEOfile,'set sourcevec=../../GeoDatabase/'+DBfolderName);
    writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\'+DBfolderExt+''') do (if not defined sourcevec set "sourcevec=..\..\GeoDatabase\%%f'+DBfolderName+'")');
    writeln(GEOfile,'echo ^!sourcevec^!');
  //  writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy %sourcevec% %destTiff%');
  //  writeln(GEOfile,'gdal_rasterize -init 0 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -burn 255 -burn 255 -l dummy ^!sourcevec^! %destTiff%');
    writeln(GEOfile,'gdal_rasterize -init 255 -ot Byte -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% -of GTiff -a_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -burn 255 -l dummy ^!sourcevec^! %destTiff%');
}
//    // create a blank bitmap in GEO folder
//    MakeGEO_Blank_24bit(GEOFolder+'\GeoDatabase\Blank_256_wht.bmp', 256, 255);
    writeln(GEOfile,'rem create tif file from a blank bmp file');
    writeln(GEOfile,'set sourcetiff=..\..\GeoDatabase\Blank_256_wht.bmp');
    writeln(GEOfile,'gdal_translate.exe -r near -of GTiff -outsize %image_width% %image_height% -a_srs "+proj=utm +zone=%utm_zone% +datum=WGS84" -a_ullr %utm_left% %utm_top% %utm_right% %utm_bottom% %sourcetiff% %destTiff%');
  end;
//  writeln(GEOfile,'gdalinfo %destTiff% > info.txt');

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
      writeln(GEOfile,'gdal_rasterize -b 1 -burn 0 -l '+fDesc + fSql +' ^!sourcevec^! %destTiff%');  // full transparency
//      writeln(GEOfile,'gdal_rasterize -b 1 -burn 191 -l '+fDesc + fSql +' ^!sourcevec^! %destTiff%'); // half transparency
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

Type
  tColorTable = array[0..256-1] of array[0..4-1] of byte;

// include the color tables
{$I Color_Table_Thermal.pas}
{$I Color_Table_Forest.pas}
{$I Color_Table_Water.pas}

// For Modifying the color table
//-------------------------------------------------------------------------------------
Procedure Make_Dummy_VRT_File(FilePath, FileName : string;
  Size : integer; ColorTable : tColorTable);

var
  i : integer;

begin
  //open the file
  FileName := FileName+'.vrt';
  AssignFile(GEOfile, FilePath +'\'+ FileName);
  Rewrite(GEOfile);

  writeln(GEOfile,format('<VRTDataset rasterXSize="%d" rasterYSize="%d">',[Size,Size]));
  writeln(GEOfile,'  <VRTRasterBand dataType="Byte" band="1">');
  writeln(GEOfile,'    <NoDataValue>255</NoDataValue>');
  writeln(GEOfile,'    <ColorInterp>Palette</ColorInterp>');
  writeln(GEOfile,'    <ColorTable>');

  for i := 0 to 256-1 do begin
    writeln(GEOfile,format('      <Entry c1="%d" c2="%d" c3="%d" c4="%d" />',
      [ColorTable[i][0],ColorTable[i][1],ColorTable[i][2],ColorTable[i][3]]));;
  end;

  writeln(GEOfile,'    </ColorTable>');
  writeln(GEOfile,'    <ComplexSource>');
  writeln(GEOfile,format('      <SourceFilename relativeToVRT="1">%s</SourceFilename>',['UTMmap.tif']));
  writeln(GEOfile,'      <SourceBand>1</SourceBand>');
  writeln(GEOfile,format('      <SourceProperties RasterXSize="%d" RasterYSize="%d" DataType="Byte" BlockXSize="%d" BlockYSize="1" />',[Size,Size,Size]));
  writeln(GEOfile,format('      <SrcRect xOff="0" yOff="0" xSize="%d" ySize="%d" />',[Size,Size]));
  writeln(GEOfile,format('      <DstRect xOff="0" yOff="0" xSize="%d" ySize="%d" />',[Size,Size]));
  writeln(GEOfile,'      <NODATA>255</NODATA>');
  writeln(GEOfile,'    </ComplexSource>');
  writeln(GEOfile,'  </VRTRasterBand>');
  writeln(GEOfile,'</VRTDataset>');

  // close the file
  Close(GEOfile);
end;

//-------------------------------------------------------------------------------------
Procedure MakeGEO_GLC_batchFile(TileIndex : integer;
  gType : geoType; Size : integer);

var
  FilePath : string;
  BatchFileName : string;
  FileName : string;
  gName : string;
  bmpPath : string;
  bmpName : string;
  ColorTable : tColorTable;

  Tile_B_Lat : double;
  Tile_T_Lat : double;
  Tile_L_Long : double;
  Tile_R_Long : double;

begin
  FilePath := GEOfolder +'\SourceTiles\'+ TileList[TileIndex].TileName;
  BatchFileName := GEO_F_Name[ord(gType)];
  case gType of
    gGeneric: begin
      gName := 'gUTMmap';
      bmpPath := '..\..\GeoDatabase\';
      bmpName := TileList[TileIndex].TileName+'.bmp';
    end;
    gThermal: begin
      gName := 'tUTMmap';
      bmpPath := '';
      bmpName := TileList[TileIndex].TileName+'_t.bmp';
      ColorTable := Color_Table_Thermal;
    end;
    gV1forest: begin
      gName := 'fUTMmap';
      bmpPath := '..\..\Terragen\ForestMaps\';
      bmpName := 'f'+TileList[TileIndex].TileName+'.bmp';
      ColorTable := Color_Table_Forest;
    end;
    gV2decideous: begin
      gName := 'fbUTMmap';
      bmpPath := '..\..\Terragen\ForestMaps\';
      bmpName := 'b'+TileList[TileIndex].TileName+'.bmp';
      ColorTable := Color_Table_Forest;
    end;
    gV2coniferous: begin
      gName := 'fsUTMmap';
      bmpPath := '..\..\Terragen\ForestMaps\';
      bmpName := 's'+TileList[TileIndex].TileName+'.bmp';
      ColorTable := Color_Table_Forest;
    end;
    gWater: begin
      gName := 'fwUTMmap';
      bmpPath := '..\..\Terragen\WaterMaps\';
      bmpName := TileList[TileIndex].TileName+'.bmp';
      ColorTable := Color_Table_Water;
    end;
    else begin
      Exit;
    end;
  end;

  //open the file
  FileName := BatchFileName+TileList[TileIndex].TileName+'.bat';
  AssignFile(GEOfile, FilePath +'\'+ FileName);
  Rewrite(GEOfile);

  writeln(GEOfile,'echo off');
  writeln(GEOfile,'rem Enable Delayed Expansion for !Variables!');
  writeln(GEOfile,'setlocal enabledelayedexpansion');
  writeln(GEOfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
  writeln(GEOfile,'set GDAL_DATA='+GDALlibraryFolder+'\..\share\epsg_csv');
  writeln(GEOfile,'gdalinfo --version');
  // suppress generation of .xml file
  writeln(GEOfile,'set GDAL_PAM_ENABLED=NO');

  writeln(GEOfile,'rem create a GeoTiff to embed the UTM easting and northing');

  writeln(GEOfile,'rem set the size');
  writeln(GEOfile,format('set image_width=%d',[Size]));
  writeln(GEOfile,format('set image_height=%d',[Size]));

  Tile_B_Lat  := TileList[TileIndex].TileUTMBottom + UTM_Bottom - Legacy_Offset;
  Tile_T_Lat  := Tile_B_Lat + 23040;
  Tile_L_Long  := UTM_Right + Legacy_Offset - TileList[TileIndex+1].TileUTMRight;
  Tile_R_Long  := Tile_L_Long + 23040;

  writeln(GEOfile,'rem crop to UTM coordinates');
//  writeln(GEOfile,'set utm_zone='+UTM_Zone);
  writeln(GEOfile,format('set utm_zone=%d',[UTM_Zone]));
  if (UTM_ZoneNS = 'N') then begin
    writeln(GEOfile,'set utm_grid=north');
  end else begin
    writeln(GEOfile,'set utm_grid=south');
  end;
  writeln(GEOfile,format('set utm_left=%1.1f',[Tile_L_Long]));
  writeln(GEOfile,format('set utm_bottom=%1.1f',[Tile_B_Lat]));
  writeln(GEOfile,format('set utm_right=%1.1f',[Tile_R_Long]));
  writeln(GEOfile,format('set utm_top=%1.1f',[Tile_T_Lat]));

//  writeln(GEOfile,'set sourcetiff=..\..\GeoDatabase\*.tif');
  // cannot do *.tif when re-projection done; need to use for loop
  writeln(GEOfile,'for /F %%f in (''dir /b ..\..\GeoDatabase\*.tif'') do (');
  writeln(GEOfile,'set sourceTiff=..\..\GeoDatabase\%%f');
  writeln(GEOfile,'set destinationTiff='+TileList[TileIndex].TileName+'_%%f');
  writeln(GEOfile,'if exist !destinationTiff! del !destinationTiff!'); // if already present
  writeln(GEOfile,'rem for color table preservation, use - r near or -r min or -r max');
  writeln(GEOfile,'gdalwarp.exe -r near -of GTiff -t_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -ts %image_width% %image_height% -te %utm_left% %utm_bottom% %utm_right% %utm_top% !sourcetiff! !destinationtiff!');
  writeln(GEOfile,')'); // end for loop

  // now combine the files
  writeln(GEOfile,format('set destinationTiff=%s.tif',['UTMmap']));
  writeln(GEOfile,'gdalwarp.exe '+TileList[TileIndex].TileName+'_*.tif %destinationTiff%');
  writeln(GEOfile,'del '+TileList[TileIndex].TileName+'_*.tif');

  writeln(GEOfile,'set destinationbmp='+bmpPath+bmpName);
  case gType of
    gGeneric: begin
      writeln(GEOfile,'gdal_translate -of BMP %destinationTIFF% %destinationbmp%');
    end;
    else begin
      // convert to bitmap with new Color Table from VRT
      writeln(GEOfile,'rem convert to bitmap');
      writeln(GEOfile,format('set sourceVRT=%s.vrt',[gName]));
      writeln(GEOfile,'gdal_translate -of BMP %sourceVRT% %destinationbmp%');
    end;
  end;
  writeln(GEOfile,'del %destinationTIFF%');

  writeln(GEOfile,'endlocal');

  // close the file
  Close(GEOfile);

  // also make the VRT file with the color table
  Make_Dummy_VRT_File(FilePath, gName, Size, ColorTable);

  MessageShow(FileName+' done.');
end;

//-------------------------------------------------------------------------------------
Procedure MakeGEO_GLC_Blank(TileIndex : integer;
  gType : geoType; Size : integer);

var
  FilePath : string;
  BatchFileName : string;
  FileName : string;
  gName : string;
  bmpPath : string;
  bmpName : string;

begin
  FilePath := GEOfolder +'\SourceTiles\'+ TileList[TileIndex].TileName;
  BatchFileName := GEO_F_Name[ord(gType)];
  case gType of
    gV2coniferous: begin
      gName := 'fsUTMmap';
      bmpPath := '..\..\Terragen\ForestMaps\';
      bmpName := 's'+TileList[TileIndex].TileName+'.bmp';
    end;
    else begin
      Exit;
    end;
  end;

  //open the file
  FileName := BatchFileName+TileList[TileIndex].TileName+'.bat';
  AssignFile(GEOfile, FilePath +'\'+ FileName);
  Rewrite(GEOfile);

  writeln(GEOfile,'echo off');
  writeln(GEOfile,'setlocal');

  writeln(GEOfile,'set sourceBMP='+FilePath+'\'+BMPpath+'sBlank.bmp');
  writeln(GEOfile,'if not exist %sourceBMP% (echo ERROR: %sourceBMP% not found & pause & exit /b 9)');
  writeln(GEOfile,'set destinationBMP='+FilePath+'\'+BMPpath+bmpName);
  writeln(GEOfile,'copy %sourceBMP% %destinationBMP%');

  writeln(GEOfile,'endlocal');

  // close the file
  Close(GEOfile);

  MessageShow(FileName+' done.');
end;

// identify list of files to download
//---------------------------------------------------------------------------
Procedure MakeGEO_GLC_Wget;
const
  baseURL = 'https://storage.googleapis.com/earthenginepartners-hansen/GLCLU_2019/map/';

var
  i, j : integer;
  Lat_Min, Lat_Max, Long_Min, Long_Max : double;
  GLC_Lat_Min, GLC_Lat_Max, GLC_Long_Min, GLC_Long_Max : integer;
  GEO_File : TextFile;
  URL_File : TextFile;

//---------------------------------------------------------------------------
function GLC_Name(i,j : integer): string;
var
  S : string;
begin
  S := format('%2.2dN_%3.3dE',[abs(i),abs(j)]);
  if (i < 0) then begin // if south replace N by S
    S[3] := 'S';
  end;
  if (j < 0) then begin // if west replace E by W
    S[8] := 'W';
  end;
  result := S;
end;

//---------------------------------------------------------------------------
begin
    // first, using lat/long extents, determine how many HGT tiles are needed
    UTMtoLatLong(UTM_Top, UTM_Left, UTM_Zone, UTM_ZoneNS);
    Lat_Max := uLatitude;
    Long_Min := uLongitude;
    UTMtoLatLong(UTM_Top, UTM_Right, UTM_Zone, UTM_ZoneNS);
    if (Lat_Max < uLatitude) then begin
      Lat_Max := uLatitude;
    end;
    Long_Max := uLongitude;
    UTMtoLatLong(UTM_Bottom, UTM_Left, UTM_Zone, UTM_ZoneNS);
    Lat_Min := uLatitude;
    if (Long_min > uLongitude) then begin
      Long_min := uLongitude;
    end;
    UTMtoLatLong(UTM_Bottom, UTM_Right, UTM_Zone, UTM_ZoneNS);
    if (Lat_Min > uLatitude) then begin
      Lat_Min := uLatitude;
    end;
    if (Long_Max < uLongitude) then begin
      Long_Max := uLongitude;
    end;

    // every 10 deg
    GLC_Lat_Min  := Ceil(Lat_Min/10);
    GLC_Lat_Max  := Ceil(Lat_Max/10);
    GLC_Long_Min := Floor(Long_Min/10);
    GLC_Long_Max := Floor(Long_Max/10);

    MessageShow('Files needed:');
    AssignFile(URL_file, GEOFolder+'\GeoDatabase'+'\URLs.txt');
    Rewrite(URL_file);
    for i := GLC_Lat_Min to GLC_Lat_Max do begin
      for j := GLC_Long_Min to GLC_Long_Max do begin
        writeln(URL_file, baseURL+GLC_Name(i*10,j*10)+'.tif');
        MessageShow(GLC_Name(i*10,j*10)+'.tif');
      end;
    end;
    CloseFile(URL_file);

    // a file to download the GLC files
    // first check if alternate batch file is available
    if (FileExists(ApplicationPath+'\Batch\GLC_WGET.bat')) then begin
      CopyFile(pchar(ApplicationPath+'\Batch\GLC_Wget.bat'),
        pchar(GEOFolder+'\GeoDatabase'+'\GLC_Wget.bat'),false);
    end else begin
      // create a file to download the HGT files
      AssignFile(GEO_file, GEOFolder+'\GeoDatabase'+'\GLC_WGET.bat');
      Rewrite(GEO_file);

      writeln(GEO_file,'@echo off');
//      writeln(GEO_file,'setlocal');
      writeln(GEO_file,'setlocal EnableDelayedExpansion');
//      writeln(GEO_file,'set PATH=%PATH%;c:\programs\wget');
      writeln(GEO_file,'set PATH=%PATH%;'+WGETfolder);

      writeln(GEO_file,'rem make sure needed programs exist');
      writeln(GEO_file,'where wget 2>nul');
      writeln(GEO_file,'IF %ERRORLEVEL% NEQ 0 (');
      writeln(GEO_file,'   echo ERROR: '+WGETfolder+'\wget.exe not found & pause & exit /b 9)');

      writeln(GEO_file,'rem goto directory where batch file is');
      writeln(GEO_file,'cd /d %~dp0');

      writeln(GEO_file, 'wget -i URLs.txt');

      writeln(GEO_file, 'rem check files OK');
      writeln(GEO_file, 'for /F %%G in (URLs.txt) DO (');
      writeln(GEO_file, '  for /F "tokens=6 delims=/" %%a in ("%%G") do set "tifFile=%%a"');
      writeln(GEO_file, '  if not exist !tifFile! (echo ERROR: !tifFile! is missing & pause & exit /b 9)');
      writeln(GEO_file,')');

//      writeln(GEO_file,'pause');
      writeln(GEO_file,'endlocal');
      // close the file
      CloseFile(GEO_file);
    end;

end;

{----------------------------------------------------------------------------}
Procedure MakeGEO_Blank_GreyScale(FileName : string; Size : integer; value : byte);
const
  BlockSize : integer = 256;

var
  i :integer;
  Greyscale_File : File of byte;
  P : PByteArray;
//  P : pRGBAlphaArray;
  pColor : ColorConvert;
  dSize : LongWord;

begin
    AssignFile(Greyscale_File,FileName);
    Rewrite(Greyscale_File);
    with BitmapHeader_8bitColor do begin
      bDib.bPaletteColors := 256; // 8 bit greyscale
      bDib.bWidth := Size;
      bDib.bHeight := Size;
//      bDib.bImageByteSize := bDib.bWidth*bDib.bHeight*Color8Size div 8;
      bDib.bImageByteSize := bDib.bWidth*bDib.bHeight*Color8Size;
      bH.bFileByteSize := bDib.bImageByteSize+bH.bPixelArrayOffset;
      BlockWrite(Greyscale_File,BitmapHeader_8bitColor,
        sizeof(BitmapHeader_8bitColor));

      try
        P := AllocMem(256 * sizeof(TRGBQuad)); // block transfer
        //write 256 color palette
        //seek(GreyScale_File,sizeof(BMP_Header) + sizeof(BMP_DIB_Header));
        pColor.ByteValue[3]:=0;
        for i := 0 to 256-1 do begin //grey scale
          pColor.ByteValue[0] := i;
          pColor.ByteValue[1] := i;
          pColor.ByteValue[2] := i;
          pRGBAlphaArray(P)^[i] := pColor.cRGBA;
        end;
        BlockWrite(Greyscale_File,P^[0],256*sizeof(TRGBQuad));
      finally
        freemem(P);
      end;

      try
        P := AllocMem(BlockSize); // block transfer
        if (value <> 0) then begin
          for i := 0 to BlockSize-1 do begin
            P^[i] := value;
          end;
        end;
        dSize := bDib.bHeight * bDib.bWidth;
        while (dSize > BlockSize) do begin
          BlockWrite(Greyscale_File,P^,BlockSize);
          DEC(dSize, BlockSize);
        end;
        BlockWrite(Greyscale_File,P^,dSize);
      finally
        freemem(P);
      end;

    end;

    Close(Greyscale_File);
//    MessageShow('Greyscale bitmap created');
end;

{----------------------------------------------------------------------------}
Procedure MakeGEO_Blank_24bit(FileName : string; Width, Height : integer; value : byte);
var
  i :integer;
  Bitmap_File : File of byte;
  P : pRGBArray;

begin
    AssignFile(Bitmap_File,FileName);
    Rewrite(Bitmap_File);
    // create a header
    with xBitmapHeader_24bitColor do begin
      bDib.bWidth := Width;
      bDib.bHeight := Height;
      //bDib.bImageByteSize := bDib.bWidth*bDib.bHeight*xColor24Size div 8;
      bDib.bImageByteSize := bDib.bWidth*bDib.bHeight*Color24Size;
      bH.bFileByteSize := bDib.bImageByteSize+bH.bPixelArrayOffset;
    end;
    BlockWrite(Bitmap_File,xBitmapHeader_24bitColor,
      sizeof(xBitmapHeader_24bitColor));

    try
      P := AllocMem(Width * sizeof(TRGBTriple)); // block of 0's
      if (value <> 0) then begin
        P^[0].rgbtBlue := value;
        P^[0].rgbtGreen := value;
        P^[0].rgbtRed := value;
        for i := 1 to Width-1 do begin
          P^[i] := p^[0];
        end;
      end;
      for i := 0 to Height-1 do begin
        BlockWrite(Bitmap_File,P^,Width * sizeof(TRGBTriple));
      end;
    finally
      freemem(P);
    end;

    Close(Bitmap_File);
//    MessageShow('bitmap created');
end;

{----------------------------------------------------------------------------}
begin
  Memo_Message := nil;
end.

//--- End of file ---------------------------------------------------------------------


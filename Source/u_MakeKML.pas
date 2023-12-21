{
 * u_MakeKML.pas
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
unit u_MakeKML;

{----------------------------------------------------------------------------
creates a Google Earth KML file of task turnpoints

KML file coordinates must not have any spaces in it!
"45.2346,-75.87365,500" NOT "45.2346, -75.87365, 500"
i.e. use 1:6 for format specifier to not have any leading spaces

----------------------------------------------------------------------------}
interface

uses StdCtrls, FileCtrl;

var
  Memo_Message : TMemo;   // external TMemo for messages
  KMLfolder : string;     // external path for file output
  OverallFolder : string; // external path for overall file output

  Tile_RB_Lat  : real;
  Tile_RB_Long : real;
  Tile_LB_Lat  : real;
  Tile_LB_Long : real;
  Tile_RT_Lat  : real;
  Tile_RT_Long : real;
  Tile_LT_Lat  : real;
  Tile_LT_Long : real;

Procedure MakeKML(TileIndex : Integer);
Procedure MakeOverallKML(Index0,Index1,Index2,Index3 : Integer);
Procedure MakeKML_QuarterTile(CurrentRow, CurrentColumn, offset_Row, offset_Column : Integer);

//----------------------------------------------------------------------------
implementation

uses SysUtils,
  {u_UTM,} u_TileList, Unit_Main, u_LandsatMet, {u_SceneryHDR, u_Terrain,}
  u_QuarterTile; //??? to much cross-coupling!

const
  earthRadius = 6371.0;

var
  KMLfile : TextFile;
  Name : string;
{
  Tile_RB_Lat  : real;
  Tile_RB_Long : real;
  Tile_LB_Lat  : real;
  Tile_LB_Long : real;
  Tile_RT_Lat  : real;
  Tile_RT_Long : real;
  Tile_LT_Lat  : real;
  Tile_LT_Long : real;
}
{----------------------------------------------------------------------------}
Procedure MessageShow(Info : string);
begin
  if (Memo_Message <> nil) then begin
    Memo_Message.lines.add(Info);
  end;
end;

//-------------------------------------------------------------------------------------
Procedure MakeKMLheader;
begin
  writeln(KMLfile,'<kml xmlns="http://earth.google.com/kml/2.0">');
  writeln(KMLfile,'<Document>');
  writeln(KMLfile,'  <open>1</open>');
{
  writeln(KMLfile,'  <Style id="blueline">');
  writeln(KMLfile,'    <LineStyle>');
  writeln(KMLfile,'      <color>ffff0000</color>');
  writeln(KMLfile,'      <width>2</width>');
  writeln(KMLfile,'    </LineStyle>');
  writeln(KMLfile,'    <PolyStyle>');
  writeln(KMLfile,'      <color>7fffffff</color>');
  writeln(KMLfile,'    </PolyStyle>');
  writeln(KMLfile,'  </Style>');

  writeln(KMLfile,'  <Style id="Waypoints">');
  writeln(KMLfile,'    <IconStyle>');
  writeln(KMLfile,'      <color>ff0000ff</color>');
  writeln(KMLfile,'      <scale>0.2</scale>');
  writeln(KMLfile,'    </IconStyle>');
  writeln(KMLfile,'    <LabelStyle>');
  writeln(KMLfile,'      <color>ffff00ff</color>');
  writeln(KMLfile,'      <scale>0.7</scale>');
  writeln(KMLfile,'    </LabelStyle>');
  writeln(KMLfile,'  </Style>');
}
  writeln(KMLfile,'  <description> Condor Tile Viewer</description>');
end;

//-------------------------------------------------------------------------------------
Procedure MakeKMLtrailer;
begin
  writeln(KMLfile,'</Document>');
  writeln(KMLfile,'</kml>');
end;

//-------------------------------------------------------------------------------------
// put camera 1000m higher and 30 km back (78 deg tilt)
//-------------------------------------------------------------------------------------
Procedure PutCamera(Name:string;Lat,Long,Alt,Bearing:real);
const
  ViewDist = 33;
  ViewHeight = 22000;
  ViewAngle = '45';

var
  Xsize,Ysize : real;

begin
      Xsize := arctan(ViewDist/earthRadius)*180.0/Pi;
      Ysize := cos(Bearing)*Xsize*cos(Lat*Pi/180);
      Xsize := sin(Bearing)*Xsize;
      writeln(KMLfile,'  <Placemark>');
      writeln(KMLfile,'    <name>Camera '+Name+'</name>');
      writeln(KMLfile,'    <Camera id="Camera ',Name,'">');
      writeln(KMLfile,'        <altitudeMode>absolute</altitudeMode>');

      writeln(KMLfile,'        <longitude>',Long-Xsize:1:6,'</longitude>');
      writeln(KMLfile,'        <latitude>',Lat-Ysize:1:6,'</latitude>');
      writeln(KMLfile,'        <altitude>',ViewHeight+Alt:1:0,'</altitude>');

      writeln(KMLfile,'        <heading>0</heading>');
      writeln(KMLfile,'        <tilt>'+ViewAngle+'</tilt>');
      writeln(KMLfile,'        <roll>0</roll>');
      writeln(KMLfile,'      </Camera>');

      writeln(KMLfile,'  </Placemark>');
end;

//-------------------------------------------------------------------------------------
Procedure MakeKMLtile(xColor : string; TickMarks : boolean);
const
  Elevation = 0; // sea level
  Bearing = 0; // north
  Height = 1000.0;

var
//  xColor : string;
  xCentreLat : real;
  xCentreLong : real;

begin
    writeln(KMLfile,'<Folder>');
    writeln(KMLfile,'<name>Tile '+Name+'</name>');
    writeln(KMLfile,'  <open>1</open>');

//      xColor := '6f00ffff'; // translucent yellow

      writeln(KMLfile,'  <Placemark>');
      writeln(KMLfile,'    <name>Boundary '+Name+'</name>');
      writeln(KMLfile,'    <visibility>1</visibility>');
      // lines, translucent polygon
      writeln(KMLfile,'    <Style><LineStyle><color>'+xColor+'</color></LineStyle><PolyStyle><color>'+xColor+'</color></PolyStyle></Style>');
      // no lines, translucent polygon
//      writeln(KMLfile,'    <Style><LineStyle><color>00000000</color></LineStyle><PolyStyle><color>'+xColor+'</color></PolyStyle></Style>');
      // lines, transparent polygon
//      writeln(KMLfile,'    <Style><LineStyle><width>3</width><color>'+xColor+'</color></LineStyle><PolyStyle><color>00000000</color></PolyStyle></Style>');
      writeln(KMLfile,'    <MultiGeometry>');
      writeln(KMLfile,'    <LineString>');
      writeln(KMLfile,'      <extrude>1</extrude>');
      writeln(KMLfile,'      <altitudeMode>absolute</altitudeMode>');
      writeln(KMLfile,'      <coordinates>');

      writeln(KMLfile,Tile_RB_Long:1:6,',',
                      Tile_RB_Lat:1:6,',', Height:1:0);
      writeln(KMLfile,Tile_LB_Long:1:6,',',
                      Tile_LB_Lat:1:6,',', Height:1:0);
      writeln(KMLfile,Tile_LT_Long:1:6,',',
                      Tile_LT_Lat:1:6,',', Height:1:0);
      writeln(KMLfile,Tile_RT_Long:1:6,',',
                      Tile_RT_Lat:1:6,',', Height:1:0);
      writeln(KMLfile,Tile_RB_Long:1:6,',',
                      Tile_RB_Lat:1:6,',', Height:1:0);

      writeln(KMLfile,'      </coordinates>');
      writeln(KMLfile,'    </LineString>');

      if (TickMarks) then begin
        writeln(KMLfile,'    <LineString>');
        writeln(KMLfile,'      <extrude>1</extrude>');
        writeln(KMLfile,'      <altitudeMode>absolute</altitudeMode>');
        writeln(KMLfile,'      <coordinates>');

        writeln(KMLfile,((Tile_RB_Long+Tile_LB_Long)/2):1:6,',',
                        Tile_RB_Lat:1:6,',', Height:1:0);
        writeln(KMLfile,((Tile_RB_Long+Tile_LB_Long))/2:1:6,',',
                        (Tile_RB_Lat-0.005):1:6,',', Height:1:0);

        writeln(KMLfile,'      </coordinates>');
        writeln(KMLfile,'    </LineString>');

        writeln(KMLfile,'    <LineString>');
        writeln(KMLfile,'      <extrude>1</extrude>');
        writeln(KMLfile,'      <altitudeMode>absolute</altitudeMode>');
        writeln(KMLfile,'      <coordinates>');

        writeln(KMLfile,((Tile_RT_Long+Tile_LT_Long)/2):1:6,',',
                        Tile_RT_Lat:1:6,',', Height:1:0);
        writeln(KMLfile,((Tile_RT_Long+Tile_LT_Long))/2:1:6,',',
                        (Tile_RT_Lat+0.005):1:6,',', Height:1:0);

        writeln(KMLfile,'      </coordinates>');
        writeln(KMLfile,'    </LineString>');

        writeln(KMLfile,'    <LineString>');
        writeln(KMLfile,'      <extrude>1</extrude>');
        writeln(KMLfile,'      <altitudeMode>absolute</altitudeMode>');
        writeln(KMLfile,'      <coordinates>');

        writeln(KMLfile,Tile_RB_Long:1:6,',',
                        ((Tile_RB_Lat+Tile_RT_Lat)/2):1:6,',', Height:1:0);
        writeln(KMLfile,(Tile_RB_Long+0.005):1:6,',',
                        ((Tile_RB_Lat+Tile_RT_Lat)/2):1:6,',', Height:1:0);

        writeln(KMLfile,'      </coordinates>');
        writeln(KMLfile,'    </LineString>');

        writeln(KMLfile,'    <LineString>');
        writeln(KMLfile,'      <extrude>1</extrude>');
        writeln(KMLfile,'      <altitudeMode>absolute</altitudeMode>');
        writeln(KMLfile,'      <coordinates>');

        writeln(KMLfile,Tile_LB_Long:1:6,',',
                        ((Tile_LB_Lat+Tile_LT_Lat)/2):1:6,',', Height:1:0);
        writeln(KMLfile,(Tile_LB_Long-0.005):1:6,',',
                        ((Tile_LB_Lat+Tile_LT_Lat)/2):1:6,',', Height:1:0);

        writeln(KMLfile,'      </coordinates>');
        writeln(KMLfile,'    </LineString>');
      end;

      writeln(KMLfile,'    </MultiGeometry>');
      writeln(KMLfile,'  </Placemark>');

      xCentreLat := (Tile_LT_Lat + Tile_RB_Lat)/2;
      xCentreLong := (Tile_LT_Long + Tile_RB_Long)/2;
      PutCamera(Name,xCentreLat,xCentreLong,Elevation,Bearing);
    writeln(KMLfile,'</Folder>');
end;

//-------------------------------------------------------------------------------------
Procedure MakeOverallKML(Index0,Index1,Index2,Index3 : Integer);
var
  i,j,TileIndex : integer;
  FileName : string;
  FilePath : string;

begin
{
  //check for folder
  if (NOT DirectoryExists(KMLfolder)) then begin
    MessageShow('Destination Folder not found');
    exit;
  end;
  //create a folder if necessary
  FilePath := KMLfolder +'\OverAllMap';
  if (NOT DirectoryExists(FilePath)) then begin
    mkdir(FilePath);
  end;
}
//  FilePath := KMLfolder +'\SourceTiles\Overall';
  FilePath := KMLfolder + '\' + OverallFolder;
  ForceDirectories(FilePath);

  //open the file
  FileName := 'OverAll.kml';
  AssignFile(KMLfile, FilePath +'\'+ FileName);
  Rewrite(KMLfile);

  MakeKMLheader;

  Name := 'Overall';
  Tile_RB_Lat  := CornerList[0].TileLatBottom;
  Tile_RB_Long := CornerList[0].TileLongRight;
  Tile_LB_Lat  := CornerList[1].TileLatBottom;
  Tile_LB_Long := CornerList[1].TileLongRight;
  Tile_RT_Lat  := CornerList[2].TileLatBottom;
  Tile_RT_Long := CornerList[2].TileLongRight;
  Tile_LT_Lat  := CornerList[3].TileLatBottom;
  Tile_LT_Long := CornerList[3].TileLongRight;
  MakeKMLtile('6f00ffff', false);  // translucent yellow

  Name := 'Fly region';
  Tile_RB_Lat  := RangeList[0].TileLatBottom;
  Tile_RB_Long := RangeList[0].TileLongRight;
  Tile_LB_Lat  := RangeList[1].TileLatBottom;
  Tile_LB_Long := RangeList[1].TileLongRight;
  Tile_RT_Lat  := RangeList[2].TileLatBottom;
  Tile_RT_Long := RangeList[2].TileLongRight;
  Tile_LT_Lat  := RangeList[3].TileLatBottom;
  Tile_LT_Long := RangeList[3].TileLongRight;
  MakeKMLtile('6f0000ff', false);  // translucent red

  writeln(KMLfile,'<Folder>');
  writeln(KMLfile,'<name>Tiles</name>');
  writeln(KMLfile,'  <open>0</open>');
  for i := 0 to TileRowCount-1 do begin
    for j := 0 to TileColumnCount-1 do begin
      TileIndex := i*(TileColumnCount+1)+j;
      Name := TileList[TileIndex].TileName;
      // tile corners
      Tile_RB_Lat  := TileList[TileIndex].TileLatBottom;
      Tile_RB_Long := TileList[TileIndex].TileLongRight;
      Tile_LB_Lat  := TileList[TileIndex+1].TileLatBottom;
      Tile_LB_Long := TileList[TileIndex+1].TileLongRight;
      Tile_RT_Lat  := TileList[TileIndex+1+TileColumnCount].TileLatBottom;
      Tile_RT_Long := TileList[TileIndex+1+TileColumnCount].TileLongRight;
      Tile_LT_Lat  := TileList[TileIndex+1+TileColumnCount+1].TileLatBottom;
      Tile_LT_Long := TileList[TileIndex+1+TileColumnCount+1].TileLongRight;
      MakeKMLtile('6f00ffff', false);  // translucent yellow
    end;
  end;
  writeln(KMLfile,'</Folder>');

  // Clean up here too - organize things better - too much cross-coupling between units!
  if ( (Form_Main.ComboBox_Imagery.Text = 'Landsat') AND
    (FileCount > 0) ) then begin
    //add landsat tiles to overall KML
    writeln(KMLfile,'<Folder>');
    writeln(KMLfile,'<name>Landsat tiles</name>');
    writeln(KMLfile,'  <open>0</open>');
    for i := 0 to FileCount-1 do begin
      Name := LandsatMetFiles[i].Folder;
      // tile corners
      Tile_RB_Lat  := LandsatMetFiles[i].SCENE_LR_CORNER_LAT;
      Tile_RB_Long := LandsatMetFiles[i].SCENE_LR_CORNER_LON;
      Tile_LB_Lat  := LandsatMetFiles[i].SCENE_LL_CORNER_LAT;
      Tile_LB_Long := LandsatMetFiles[i].SCENE_LL_CORNER_LON;
      Tile_RT_Lat  := LandsatMetFiles[i].SCENE_UR_CORNER_LAT;
      Tile_RT_Long := LandsatMetFiles[i].SCENE_UR_CORNER_LON;
      Tile_LT_Lat  := LandsatMetFiles[i].SCENE_UL_CORNER_LAT;
      Tile_LT_Long := LandsatMetFiles[i].SCENE_UL_CORNER_LON;
      MakeKMLtile('6fff0000', false);  // translucent blue
    end;
    writeln(KMLfile,'</Folder>');
  end;
{
  // Clean up here too - organize things better - too much cross-coupling between units!
    if (AreaPointsDefined) then begin  // based on desired area or UTM ?
    //add desired area to overall KML
      Name := 'Desired Area';
      // tile corners
      Tile_RB_Lat  := LatSouth;
      Tile_RB_Long := LongEast;
      Tile_LB_Lat  := LatSouth;
      Tile_LB_Long := LongWest;
      Tile_RT_Lat  := LatNorth;
      Tile_RT_Long := LongEast;
      Tile_LT_Lat  := LatNorth;
      Tile_LT_Long := LongWest;
      MakeKMLtile('6fff0000');  // translucent blue
  end;
}

  MakeKMLtrailer;

  // close the file
  Close(KMLfile);

  MessageShow(FileName+' done.');
end;

//-------------------------------------------------------------------------------------
Procedure MakeKML(TileIndex : Integer);
var
  FileName : string;
  FilePath : string;

begin
{  //check for folder
  if (NOT DirectoryExists(KMLfolder)) then begin
    MessageShow('Destination Folder not found');
    exit;
  end;
  //create a folder if necessary
  FilePath := KMLfolder +'\SourceTiles';
  if (NOT DirectoryExists(FilePath)) then begin
    mkdir(FilePath);
  end;
  //create a sub folder if necessary
  FilePath := FilePath +'\'+ TileList[TileIndex].TileName;
  if (NOT DirectoryExists(FilePath)) then begin
    mkdir(FilePath);
  end;
}
  FilePath := KMLfolder +'\SourceTiles\'+ TileList[TileIndex].TileName;
  ForceDirectories(FilePath);

  FileName := TileList[TileIndex].TileName+'.kml';
  //open the file
  AssignFile(KMLfile, FilePath +'\'+ FileName);
  Rewrite(KMLfile);

  MakeKMLheader;

  Name := TileList[TileIndex].TileName;
  // tile corners
  Tile_RB_Lat  := TileList[TileIndex].TileLatBottom;
  Tile_RB_Long := TileList[TileIndex].TileLongRight;
  Tile_LB_Lat  := TileList[TileIndex+1].TileLatBottom;
  Tile_LB_Long := TileList[TileIndex+1].TileLongRight;
  Tile_RT_Lat  := TileList[TileIndex+1+TileColumnCount].TileLatBottom;
  Tile_RT_Long := TileList[TileIndex+1+TileColumnCount].TileLongRight;
  Tile_LT_Lat  := TileList[TileIndex+1+TileColumnCount+1].TileLatBottom;
  Tile_LT_Long := TileList[TileIndex+1+TileColumnCount+1].TileLongRight;
  MakeKMLtile('6f00ffff', false);  // translucent yellow

  MakeKMLtrailer;

  // close the file
  Close(KMLfile);

  MessageShow(FileName+' done.');
end;

//-------------------------------------------------------------------------------------
Procedure MakeKML_QuarterTile(CurrentRow, CurrentColumn, offset_Row, offset_Column : Integer);
var
  FileName : string;
  FilePath : string;
//  CurrentColumn, CurrentRow : integer;
//  Easting, Northing : double;
  hMargin, vMargin : double;
  TileIndex : integer;

begin
{  //check for folder
  if (NOT DirectoryExists(KMLfolder)) then begin
    MessageShow('Destination Folder not found');
    exit;
  end;
  //create a folder if necessary
  FilePath := KMLfolder +'\SourceTiles';
  if (NOT DirectoryExists(FilePath)) then begin
    mkdir(FilePath);
  end;
  //create a sub folder if necessary
  FilePath := FilePath +'\'+ TileList[TileIndex].TileName;
  if (NOT DirectoryExists(FilePath)) then begin
    mkdir(FilePath);
  end;
  //create a sub folder if necessary
  FilePath := FilePath +'\QuarterTiles';
  if (NOT DirectoryExists(FilePath)) then begin
    mkdir(FilePath);
  end;
}
  TileIndex := CurrentRow * (TileColumnCount+1) + CurrentColumn;

  // create path
  FilePath := KMLfolder +'\SourceTiles'+'\'+ TileList[TileIndex].TileName+'\QuarterTiles';
  ForceDirectories(FilePath);

  // KML tile name
  Name := TileList[TileIndex].TileName+format('_%2.2d_%2.2d',[offset_Column,offset_Row]);

  FileName := Name+'.kml';
  //open the file
  AssignFile(KMLfile, FilePath +'\'+ FileName);
  Rewrite(KMLfile);

  MakeKMLheader;

  Tile_RB_Lat  := Tile_RB_Lat_save;
  Tile_RB_Long := Tile_RB_Long_save;
  Tile_LB_Lat  := Tile_LB_Lat_save;
  Tile_LB_Long := Tile_LB_Long_save;
  Tile_RT_Lat  := Tile_RT_Lat_save;
  Tile_RT_Long := Tile_RT_Long_save;
  Tile_LT_Lat  := Tile_LT_Lat_save;
  Tile_LT_Long := Tile_LT_Long_save;

  MakeKMLtile('6f00ffff', false);  // translucent yellow

  // now make an extent tile and add 100 m margin
  vMargin := (0.1 /earthRadius) *180.0 /Pi;;
  hMargin := vMargin / cos (Tile_RB_Lat *Pi /180);

  if (Tile_RB_Lat < Tile_LB_Lat) then begin // bottom
    Tile_RB_Lat := Tile_RB_Lat - vMargin;
    Tile_LB_Lat := Tile_RB_Lat;
  end else begin
    Tile_LB_Lat := Tile_LB_Lat - vMargin;
    Tile_RB_Lat := Tile_LB_Lat;
  end;
  if (Tile_RT_Lat > Tile_LT_Lat) then begin // top
    Tile_RT_Lat := Tile_RT_Lat + vMargin;
    Tile_LT_Lat := Tile_RT_Lat;
  end else begin
    Tile_LT_Lat := Tile_LT_Lat + vMargin;
    Tile_RT_Lat := Tile_LT_Lat;
  end;
  if (Tile_RB_Long > Tile_RT_Long) then begin // right
    Tile_RB_Long := Tile_RB_Long + hMargin;
    Tile_RT_Long := Tile_RB_Long;
  end else begin
    Tile_RT_Long := Tile_RT_Long + hMargin;
    Tile_RB_Long := Tile_RT_Long;
  end;
  if (Tile_LB_Long < Tile_LT_Long) then begin // left
    Tile_LB_Long := Tile_LB_Long - hMargin;
    Tile_LT_Long := Tile_LB_Long;
  end else begin
    Tile_LT_Long := Tile_LT_Long - hMargin;
    Tile_LB_Long := Tile_LT_Long;
  end;

  Name := Name + '_ext';
  MakeKMLtile('7f00ff00', true);  // translucent green

  MakeKMLtrailer;

  // close the file
  Close(KMLfile);

  MessageShow(FileName+' done.');
end;

{----------------------------------------------------------------------------}
begin
  Memo_Message := nil;
end.

//--- End of file ------------------------------------------------------------


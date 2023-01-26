{
 * u_MakeGMID.pas
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

// MKINK notes
{
BOOLEAN CreateSymbolicLinkA(
  LPCSTR lpSymlinkFileName,
  LPCSTR lpTargetFileName,
  DWORD  dwFlags
);

dwFlags
Indicates whether the link target, lpTargetFileName, is a directory.
0x0 The link target is a file.
0x1 The link target is a directory.
0x2 SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE

Nick Note Flag 0x2 doesn't work - need to run as administrator

File reference can be relative
}

//----------------------------------------------------------------------------
unit u_MakeGMID;

{----------------------------------------------------------------------------
creates an AllMapSoft Google Earth Downloader project file

creates a lat long extent and a zoom level

[AREA]
LeftLongitude=-75.35
RightLongitude=-75.05
TopLatitude=45.60
BottomLatitude=45.39

[Zoom]
Zoom=15

Save as .gmid file

NOTE: Since this file also contains actual download extent, to make it work,
this file is only used as an initial project file. You MUST change the project
name after you open this file and change it name to force the creation of a new
project fle with the actual download extent!

----------------------------------------------------------------------------}
interface

uses Windows, StdCtrls, FileCtrl, SysUtils;

type
  Windows_CreateSymbolicLink_Type = function ( lpSymlinkFileName : LPCSTR ;
                                               lpTargetFileName : LPCSTR;
                                               dwFlags : DWORD
                                              ) : boolean; stdcall;

  Type_DC = (td_D, td_C, td_Both);

var
  fHandle : THandle;
  CreateSymbolicLink : Windows_CreateSymbolicLink_Type;

  Memo_Message : TMemo;  // external TMemo for messages
  GMIDFolder : string;   // external path for file output
  GMIDProgramsFolder : string; // external path for library
  GMIDMapID : string;    // external path for mapstype
  ZoomLevel : string;

function OpenDLL : boolean;
function CloseDLL : boolean;

Procedure MakeGMID_All_BatchFile;
Procedure MakeGMIDprojectFile(geid : boolean; TileIndex : integer);
Procedure MakeGMIDoverallProjectFile;
Procedure MakeGMIDquarterTile(geid : boolean; CurrentRow, CurrentColumn, offset_Row, offset_Column : Integer;
      Tile_Top_Lat, Tile_Left_Long, Tile_Bottom_Lat, Tile_Right_Long : double);

// a few extra functions for when you need separate processing
Procedure MakeGMID_All_Combine_BatchFile;

Procedure Make_Batch_DownloadCombine(Which : Type_DC;
                                     Name, ID,
                                     FilePath, FileName : string;
                                     ZoomLevel : string;
                                     XL, XR, YT, YB : real);
Procedure MakeWGET_All_BatchFile;

//----------------------------------------------------------------------------
implementation

uses u_Util, u_TileList, u_UTM;

var
  GMIDfile : TextFile;

{----------------------------------------------------------------------------}
Procedure MessageShow(Info : string);
begin
  if (Memo_Message <> nil) then begin
    Memo_Message.lines.add(Info);
  end;
end;

//-------------------------------------------------------------------------------------
function OpenDLL : boolean;
begin
  fHandle := LoadLibrary('Kernel32');
  if (fHandle <> 0) then begin
    @CreateSymbolicLink := GetProcAddress(fHandle, 'CreateSymbolicLinkA');
    if (@CreateSymbolicLink = nil) then begin
      MessageShow('Unable to find function in DLL');
    end;
  end;
end;

//-------------------------------------------------------------------------------------
function CloseDLL : boolean;
begin
//  if (fHandle <> null) then begin
  if (fHandle <> 0) then begin
    FreeLibrary(fHandle);
  end;
end;

//-------------------------------------------------------------------------------------
Procedure Make_Batch_DownloadCombine(Which : Type_DC;
                                     Name, ID,
                                     FilePath, FileName : string;
                                     ZoomLevel : string;
                                     XL, XR, YT, YB : real);

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure Do_Downloader;
begin
  writeln(GMIDfile,format('%s %s %s %s %1.8f %1.8f %1.8f %1.8f %s', [
                     '"'+GMIDProgramsFolder+'\downloader.exe"',
                     Name,
                     ID,
                     ZoomLevel,
                     XL, XR, YT, YB,
                     '"'+FilePath+'"'
                     ]));
end;

// search (for loop) for file extension instead of hard coding
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure Do_Combiner;
begin
  writeln(GMIDfile,'@echo off');
  writeln(GMIDfile,'IF [%1] NEQ [] (set type=%1 & set option=%2) else (set type=bmp & set option=meters)');

  writeln(GMIDfile,'setlocal enabledelayedexpansion');
  writeln(GMIDfile,'rem goto directory where batch file is');
  writeln(GMIDfile,'cd /d %~dp0');
  writeln(GMIDfile,'for %%f in (*) do (');
  writeln(GMIDfile,'  if "%%~nf"=="'+Name+'" (');
  writeln(GMIDfile,'    set Ext=%%~xf');

  writeln(GMIDfile,format('%s %s %s', [
                     '"'+GMIDProgramsFolder+'\combiner.exe"',
                     '"'+FilePath+'\'+Name+'"!Ext! ',
//                     'bmp meters'
                     '%type% %option% %3'
                     ]));

  writeln(GMIDfile,'    goto :continue');
  writeln(GMIDfile,'   )');
  writeln(GMIDfile,')');
  writeln(GMIDfile,':continue');
  writeln(GMIDfile,'endlocal');
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
begin
  AssignFile(GMIDfile, FilePath +'\'+ FileName);
  Rewrite(GMIDfile);

  if ((which = td_Both) or (which = td_D)) then begin
    Do_Downloader;
  end;
  if ((which = td_Both) or (which = td_C)) then begin
    Do_Combiner;
  end;

  // close the file
  Close(GMIDfile);
  MessageShow(FileName+' done.');
end;

//-------------------------------------------------------------------------------------
Procedure MakeGMIDprojectFile(geid : boolean; TileIndex : Integer);

const
  ExtraDist = 0.1;  // extra 100 metres on each edge

var
  i : integer;
  FileName : string;
  FilePath : string;
  Xsize,Ysize : real;

  Tile_Top_Lat  : real;
  Tile_Left_Long : real;
  Tile_Bottom_Lat  : real;
  Tile_Right_Long : real;

  SymbolicLinkFolder : string;
  CommonFolder : string;

begin
  // extra margin to make sure all area is included
  Ysize := arctan(ExtraDist/earthRadius)*180.0/Pi;
  Xsize := Ysize*cos(TileList[TileIndex].TileLatBottom*Pi/180);

  // tile corners
  Tile_Bottom_Lat := f_Minimum(TileList[TileIndex].TileLatBottom,
    TileList[TileIndex+1].TileLatBottom);

  Tile_Top_Lat := f_Maximum(TileList[TileIndex+1+TileColumnCount].TileLatBottom,
    TileList[TileIndex+1+TileColumnCount+1].TileLatBottom);

  Tile_Left_Long := f_Minimum(TileList[TileIndex+1+TileColumnCount+1].TileLongRight,
    TileList[TileIndex+1].TileLongRight);

  Tile_Right_Long := f_Maximum(TileList[TileIndex].TileLongRight,
    TileList[TileIndex+1+TileColumnCount].TileLongRight);

  FilePath := GMIDFolder +'\SourceTiles\'+ TileList[TileIndex].TileName;
  ForceDirectories(FilePath);

  // create a common folder for downloaded tiles
  CommonFolder := GMIDFolder +'\SourceTiles\Tiles';
  ForceDirectories(CommonFolder);

  // also create a symbolic link to the common folder for downloaded imagery tiles
  SymbolicLinkFolder := FilePath + '\'+ TileList[TileIndex].TileName;
  if (NOT DirectoryExists(SymbolicLinkFolder)) then begin
//    if (NOT CreateSymbolicLink(PCHAR(SymbolicLinkFolder+chr(0)),
//                               PCHAR(CommonFolder+chr(0)),
    if (NOT CreateSymbolicLink(LPCSTR(SymbolicLinkFolder+chr(0)),
                               LPCSTR(CommonFolder+chr(0)),
                               $01)) then begin

      MessageShow('Unable to create Symbolic link');
      MessageShow('Run Condor_Tiles as Administrator');
      Beep; Exit;
    end;
  end;

  //open the file
  FileName := 'Initial_'+TileList[TileIndex].TileName;
  if (geid) then begin
    FileName := FileName + '.geid';
  end else begin
    FileName := FileName + '.gmid';
  end;
  AssignFile(GMIDfile, FilePath +'\'+ FileName);
  Rewrite(GMIDfile);

  writeln(GMIDfile,'[AREA]');
  writeln(GMIDfile,'LeftLongitude='+format('%1.8f',[Tile_Left_Long - Xsize]));
  writeln(GMIDfile,'RightLongitude='+format('%1.8f',[Tile_Right_Long + Xsize]));
  writeln(GMIDfile,'TopLatitude='+format('%1.8f',[Tile_Top_Lat + Ysize]));
  writeln(GMIDfile,'BottomLatitude='+format('%1.8f',[Tile_Bottom_Lat - Ysize]));
  writeln(GMIDfile);
  writeln(GMIDfile,'[Zoom]');
//  writeln(GMIDfile,'Zoom=15');
  if (geid) then begin
//    writeln(GMIDfile,'Zoom1='+Zoomlevel); // for GEID
//    writeln(GMIDfile,'Zoom2='+Zoomlevel); // for GEID
    writeln(GMIDfile, format('Zoom1=%d',[strtoint(ZoomLevel)+1])); // for geid, zoom level is 1 step higher
    writeln(GMIDfile, format('Zoom2=%d',[strtoint(ZoomLevel)+1])); // for geid, zoom level is 1 step higher
  end else begin
    writeln(GMIDfile,'Zoom='+Zoomlevel);
  end;

  if (geid) then begin
    writeln(GMIDfile);
    writeln(GMIDfile,'[HistoryDate]');
//    writeln(GMIDfile,'HistoryDate=2009-07-07');
    writeln(GMIDfile,'HistoryDate='+FormatDateTime('yyyy-mm-dd', Date()));
  end;

  // close the file
  Close(GMIDfile);
  MessageShow(FileName+' done.');

  if (NOT geid) then begin // only for gmid for now
    // now make a batch Download only file
    FileName := 'Batch_Download_'+TileList[TileIndex].TileName+'.bat';
    Make_Batch_DownloadCombine(td_D, TileList[TileIndex].TileName, GMIDMapID,
                               FilePath, FileName,
                               ZoomLevel,
                               Tile_Left_Long - Xsize,
                               Tile_Right_Long + Xsize,
                               Tile_Top_Lat + Ysize,
                               Tile_Bottom_Lat - Ysize
                              );

    // now make a batch combine only file
    FileName := 'Batch_Combine_'+TileList[TileIndex].TileName+'.bat';
    Make_Batch_DownloadCombine(td_C, TileList[TileIndex].TileName, GMIDMapID,
                               FilePath, FileName,
                               ZoomLevel,
                               Tile_Left_Long - Xsize,
                               Tile_Right_Long + Xsize,
                               Tile_Top_Lat + Ysize,
                               Tile_Bottom_Lat - Ysize
                              );
  end else begin //geid
  end;
end;

//-------------------------------------------------------------------------------------
Procedure MakeGMIDquarterTile(geid : boolean; CurrentRow, CurrentColumn, offset_Row, offset_Column : Integer;
      Tile_Top_Lat, Tile_Left_Long, Tile_Bottom_Lat, Tile_Right_Long : double);

var
  i : integer;
  FileName : string;
  FilePath : string;
  TileIndex : integer;
{
  Tile_Top_Lat  : real;
  Tile_Left_Long : real;
  Tile_Bottom_Lat  : real;
  Tile_Right_Long : real;
}
begin
{
  // check for folder
  if (NOT DirectoryExists(GMIDFolder)) then begin
    MessageShow('Destination Folder not found');
    exit;
  end;
  // create a folder if necessary
  FilePath := GMIDFolder +'\SourceTiles';
  if (NOT DirectoryExists(FilePath)) then begin
    mkdir(FilePath);
  end;
  // create a folder if necessary
  FilePath := FilePath +'\'+ TileList[TileIndex].TileName;
  if (NOT DirectoryExists(FilePath)) then begin
    mkdir(FilePath);
  end;
}
  TileIndex := CurrentRow * (TileColumnCount+1) + CurrentColumn;

  // create path
  FilePath := GMIDfolder +'\SourceTiles'+'\'+ TileList[TileIndex].TileName+'\QuarterTiles';
  ForceDirectories(FilePath);

  //open the file
  FileName := TileList[TileIndex].TileName+format('_%2.2d_%2.2d',[offset_Column,offset_Row]);
  if (geid) then begin
    FileName := Filename +'_initial.geid';
  end else begin
    FileName := Filename +'_initial.gmid';
  end;
  AssignFile(GMIDfile, FilePath +'\'+ FileName);
  Rewrite(GMIDfile);

  writeln(GMIDfile,'[AREA]');
  writeln(GMIDfile,'LeftLongitude='+format('%1.8f',[Tile_Left_Long]));
  writeln(GMIDfile,'RightLongitude='+format('%1.8f',[Tile_Right_Long]));
  writeln(GMIDfile,'TopLatitude='+format('%1.8f',[Tile_Top_Lat]));
  writeln(GMIDfile,'BottomLatitude='+format('%1.8f',[Tile_Bottom_Lat]));
  writeln(GMIDfile);
  // assume done manually to exact values based on rectangle drawn on Google-earth
  writeln(GMIDfile,'Left_Longitude_download='+format('%1.8f',[Tile_Left_Long]));
  writeln(GMIDfile,'Right_Longitude_download='+format('%1.8f',[Tile_Right_Long]));
  writeln(GMIDfile,'Top_Latitude_download='+format('%1.8f',[Tile_Top_Lat]));
  writeln(GMIDfile,'Bottom_Latitude_download='+format('%1.8f',[Tile_Bottom_Lat]));
  writeln(GMIDfile);
  writeln(GMIDfile,'[Zoom]');
  if (geid) then begin
//    writeln(GMIDfile,'Zoom1='+Zoomlevel); // for GEID
//    writeln(GMIDfile,'Zoom2='+Zoomlevel); // for GEID
    writeln(GMIDfile, format('Zoom1=%d',[strtoint(ZoomLevel)+1])); // for geid, zoom level is 1 step higher
    writeln(GMIDfile, format('Zoom2=%d',[strtoint(ZoomLevel)+1])); // for geid, zoom level is 1 step higher
  end else begin
    writeln(GMIDfile,'Zoom='+Zoomlevel);
  end;
  if (geid) then begin
    writeln(GMIDfile);
    writeln(GMIDfile,'[HistoryDate]');
//    writeln(GMIDfile,'HistoryDate=2009-07-07');
    writeln(GMIDfile,'HistoryDate='+FormatDateTime('yyyy-mm-dd', Date()));
  end;

  // close the file
  Close(GMIDfile);

  MessageShow(FileName+' done.');
end;

//-------------------------------------------------------------------------------------
Procedure MakeGMIDoverallProjectFile;

const
  ExtraDist = 0.1;  // extra 100 metres on each edge, to acccount for UTM warping
  // with zoom of 10, a small expansion/warp is needed
  // with zoom of 11, a large shrinkage/warp is needed, but quality may be a little better
  Default_Zoom = '11';
var
  i : integer;
  FileName : string;
  FilePath : string;
  Xsize,Ysize : real;

  Tile_Top_Lat  : real;
  Tile_Left_Long : real;
  Tile_Bottom_Lat  : real;
  Tile_Right_Long : real;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
begin
  Ysize := arctan(ExtraDist/earthRadius)*180.0/Pi;
  Xsize := Ysize*cos(CornerList[0].TileLatBottom*Pi/180);

  // tile corners
  Tile_Bottom_Lat := f_Minimum(CornerList[0].TileLatBottom, CornerList[1].TileLatBottom);
  Tile_Top_Lat := f_Maximum(CornerList[2].TileLatBottom, CornerList[3].TileLatBottom);
  Tile_Left_Long := f_Minimum(CornerList[3].TileLongRight, CornerList[1].TileLongRight);
  Tile_Right_Long := f_Maximum(CornerList[0].TileLongRight, CornerList[2].TileLongRight);

  FilePath := GMIDFolder +'\SourceTiles\Overall';
  ForceDirectories(FilePath);

  //open the file
  FileName := 'Initial_Overall.umd';
  AssignFile(GMIDfile, FilePath +'\'+ FileName);
  Rewrite(GMIDfile);

  // Virtual Earth Satellite Maps
  writeln(GMIDfile,'[MapsType]');
  writeln(GMIDfile,'MapsType=6');

  writeln(GMIDfile,'[AREA]');
  writeln(GMIDfile,'LeftLongitude='+format('%1.8f',[Tile_Left_Long - Xsize]));
  writeln(GMIDfile,'RightLongitude='+format('%1.8f',[Tile_Right_Long + Xsize]));
  writeln(GMIDfile,'TopLatitude='+format('%1.8f',[Tile_Top_Lat + Ysize]));
  writeln(GMIDfile,'BottomLatitude='+format('%1.8f',[Tile_Bottom_Lat - Ysize]));
  writeln(GMIDfile);
  writeln(GMIDfile,'[Zoom]');
  writeln(GMIDfile,'Zoom='+Default_Zoom);

  // close the file
  Close(GMIDfile);
  MessageShow(FileName+' done.');

  // now make a batch download & combine file
  FileName := 'Batch_Download.bat';
  Make_Batch_DownloadCombine(td_Both, 'Overall', GMIDMapID,
                             FilePath, FileName,
                             Default_Zoom,
                             Tile_Left_Long - Xsize,
                             Tile_Right_Long + Xsize,
                             Tile_Top_Lat + Ysize,
                             Tile_Bottom_Lat - Ysize
                            );
  // now make a batch combine only file
  FileName := 'Batch_Combine.bat';
  Make_Batch_DownloadCombine(td_C, 'Overall', GMIDMapID,
                             FilePath, FileName,
                             Default_Zoom,
                             Tile_Left_Long - Xsize,
                             Tile_Right_Long + Xsize,
                             Tile_Top_Lat + Ysize,
                             Tile_Bottom_Lat - Ysize
                            );
end;

//-------------------------------------------------------------------------------------
Procedure MakeWGET_All_BatchFile;
var
  i,j : integer;
  FileName : string;
  FilePath : string;
  TileIndex : integer;
  Name : string;

begin
  FilePath := GMIDFolder +'\SourceTiles';
  ForceDirectories(FilePath);

  //open the file
  FileName := 'WGET_ALL.bat';
  AssignFile(GMIDfile, FilePath +'\'+ FileName);
  Rewrite(GMIDfile);

  writeln(GMIDfile,'@echo off');
  writeln(GMIDfile,'setlocal');
  writeln(GMIDfile,'rem goto directory where batch file is');
  writeln(GMIDfile,'cd /d %~dp0');
//  writeln(GMIDfile,'mkdir "Tiles"');
//  writeln(GMIDfile,'rem make a link (Hardlink junction); delete with rmdir');
  for j := 0 to TileColumnCount-1 do begin
    for i := 0 to TileRowCount-1 do begin
      TileIndex := i*(TileColumnCount+1)+j;
      Name := TileList[TileIndex].TileName;
      writeln(GMIDfile,'call '+Name+'\Do_All_'+Name+'.bat');
    end;
  end;
  writeln(GMIDfile,'endlocal');

  // close the file
  Close(GMIDfile);
  MessageShow(FileName+' done.');
end;

//-------------------------------------------------------------------------------------
Procedure MakeGMID_All_BatchFile;
var
  i,j : integer;
  FileName : string;
  FilePath : string;
  TileIndex : integer;
  Name : string;

begin
  FilePath := GMIDFolder +'\SourceTiles';
  ForceDirectories(FilePath);

  //open the file
  FileName := 'GMID_ALL.bat';
  AssignFile(GMIDfile, FilePath +'\'+ FileName);
  Rewrite(GMIDfile);

  writeln(GMIDfile,'@echo off');
  writeln(GMIDfile,'setlocal');
  writeln(GMIDfile,'rem goto directory where batch file is');
  writeln(GMIDfile,'cd /d %~dp0');
//  writeln(GMIDfile,'mkdir "Tiles"');
//  writeln(GMIDfile,'rem make a link (Hardlink junction); delete with rmdir');
//  writeln(GMIDfile,'rem for tif add ",tif" at end of combine call');
  writeln(GMIDfile,'rem for tif use set type="bmp,tif"');
  writeln(GMIDfile,'rem for older combiner use set option="" or rem it out');
  writeln(GMIDfile,'set type="bmp"');
//  writeln(GMIDfile,'set option="degrees"');    // tif with lat/long ?
  writeln(GMIDfile,'set option="meters"');     // tif with meters (EPSG:3857)
  for j := 0 to TileColumnCount-1 do begin
    for i := 0 to TileRowCount-1 do begin
      TileIndex := i*(TileColumnCount+1)+j;
      Name := TileList[TileIndex].TileName;
//      writeln(GMIDfile,'mklink /D "'+Name+'\'+Name+'" "..\Tiles"');
      writeln(GMIDfile,'call '+Name+'\Batch_Download_'+Name+'.bat');
//      writeln(GMIDfile,'call '+Name+'\Batch_Combine_'+Name+'.bat');
      writeln(GMIDfile,'call '+Name+'\Batch_Combine_'+Name+'.bat %type% %option%');
    end;
  end;
  writeln(GMIDfile,'endlocal');

  // close the file
  Close(GMIDfile);
  MessageShow(FileName+' done.');
end;

//-------------------------------------------------------------------------------------
Procedure MakeGMID_All_Combine_BatchFile;
var
  i,j : integer;
  FileName : string;
  FilePath : string;
  TileIndex : integer;
  Name : string;

begin
  FilePath := GMIDFolder +'\SourceTiles';
  ForceDirectories(FilePath);

  //open the file
  FileName := 'GMID_ALL_Combine.bat';
  AssignFile(GMIDfile, FilePath +'\'+ FileName);
  Rewrite(GMIDfile);

  writeln(GMIDfile,'@echo off');
  writeln(GMIDfile,'setlocal');
  writeln(GMIDfile,'rem goto directory where batch file is');
  writeln(GMIDfile,'cd /d %~dp0');
//  writeln(GMIDfile,'mkdir "Tiles"');
//  writeln(GMIDfile,'rem make a link (Hardlink junction); delete with rmdir');
//  writeln(GMIDfile,'rem for tif add ",tif" at end of combine call');
  writeln(GMIDfile,'rem for tif use set type="bmp,tif"');
  writeln(GMIDfile,'rem for older combiner use set option="" or rem it out');
  writeln(GMIDfile,'set type="bmp"');
//  writeln(GMIDfile,'set option="degrees"');    // tif with let/long ?
  writeln(GMIDfile,'set option="meters"');     // tif with meters (EPSG:3857)
  for j := 0 to TileColumnCount-1 do begin
    for i := 0 to TileRowCount-1 do begin
      TileIndex := i*(TileColumnCount+1)+j;
      Name := TileList[TileIndex].TileName;
//      writeln(GMIDfile,'mklink /D "'+Name+'\'+Name+'" "..\Tiles"');
//      writeln(GMIDfile,'call '+Name+'\Batch_Combine_'+Name+'.bat');
      writeln(GMIDfile,'call '+Name+'\Batch_Combine_'+Name+'.bat %type% %option%');
    end;
  end;
  writeln(GMIDfile,'endlocal');

  // close the file
  Close(GMIDfile);
  MessageShow(FileName+' done.');
end;

{----------------------------------------------------------------------------}
begin
  Memo_Message := nil;
end.

//--- End of file ---------------------------------------------------------------------


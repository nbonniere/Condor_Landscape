{
 * u_Terrain.pas
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


//---------------------------------------------------------------------------
UNIT u_Terrain;

{----------------------------------------------------------------------------
The condor .TRN file is structured as follows:
- 36 bit header
  - 4 bytes for width (little-endian) = 64 * number of horizontal patches
  - 4 bytes for height (little-endian) = 64 * number of vertical patches
  - 28 bytes -> UTM information
- n 16 bit integers (little-endian) -> n = 64*patch_Columns * 64*patch_Rows
  - each integer value
    - corresponds to a 90x90 metre square
    - contains an elevation value in metres
    - 0 to 65535 meters of elevation
  - integer order in file
    - by column
    - right column first
    - starting from the bottom
----------------------------------------------------------------------------}

{----------------------------------------------------------------------------
3DEM - use 3DEM for condor
----------------------------------------------------------------------------}

// in Condor V1, UTM zone grid uses grid system A..Z
//   if <= 'N' -> south, if >= 'O' -> north
// in Condor V2, UTM zone grid is 'N' for north and 'S' for south
//===========================================================================
INTERFACE

uses StdCtrls, comctrls;

type
  CondorTerrainHeader = packed record
    tWidth : longword;              // number of columns
    tHeight : longword;             // number of rows
    tResolution : single;           // 90m
    tDeltaX : single;               // actual horizontal resolution in m (calibrated)
    tDeltaY : single;               // actual vertical resolution in m (calibrated)
    tRightMapEasting : single;      // UTM absolute Easting, bottom right
    tBottomMapNorthing : single;    // UTM absolute Northing, bottom right
    tUTMzone : longword;            // UTM zone number
    tUTMgrid : array[0..3] of char; // UTM zone grid (A..Z or N/S) (only first char used (?))
  end;

type
  Extents = record
    xMin : single;
    yMin : single;
    xMax : single;
    yMax : single;
  end;

const
  TerrainSize = 2; // size of value in file in bytes -> integer 0..65535

var
  Memo_Message : TMemo;              // external TMemo for messages
  ProgressBar_Status : TProgressBar; // external
  FileFolder : string;               // external path for file
  TerrainOpen : boolean;
  TerrainHeader : CondorTerrainHeader;
  PatchColumnCount : integer;
  PatchRowCount : integer;

Procedure ReadTerrainHeader(TerrainFileName : string);
Procedure WriteTerrainHeader(TerrainFileName : string);
Procedure ForceTerrainSize(TerrainFileName : string);
procedure Merge_TRN_File(Offset_X, Offset_Y, Min_X, Max_X, Min_Y, Max_Y : LongInt;
                          FilePath,Filename,
                          FilePath_a,Filename_a : string);
procedure Fix_TR3_Seams(Offset_X, Offset_Y : LongInt;
                        FilePath,Filename,
                        FilePath_a,Filename_a : string);
Procedure OverrideTerrainCalibration(TerrainFileName : string);
Procedure CreateColorGradientBitmap(TerrainFileName,ColorGradientFileName : string);
Procedure CreateSlopeGradientBitmap(TerrainFileName,SlopeGradientFileName : string);
Procedure TRN_To_Greyscale_Bitmap(TRN_FileName,Greyscale_FileName : string);
Procedure RAW_To_TRN(RAW_FileName, TRN_FileName : string);
Procedure RAW_To_TR3(RAW_FileName, TR3_FilePath : string);
Procedure TRN_To_RAW(TRN_FileName, RAW_FileName : string);
Procedure RAW_To_RAW3(RAW_FileName, RAW3_FileName : string);
Procedure UpdateTerrainUTMgrid_V2(TerrainFileName : string);

//===========================================================================
IMPLEMENTATION

uses forms, FileCtrl, SysUtils, Graphics, {Windows,} Math, Dialogs,
  u_SceneryHDR, u_TileList, u_BMP;

var
  Terrain_File : File of byte;
  CG_File : File of byte;
  GradientGrid : array[0..tRows-1,0..tColumns-1] of integer;
  HeightValue : WordConvert;

{----------------------------------------------------------------------------}
Procedure MessageShow(Info : string);
begin
  if (Memo_Message <> nil) then begin
    Memo_Message.lines.add(Info);
  end;
end;

{----------------------------------------------------------------------------}
Procedure ReadTerrainHeader(TerrainFileName : string);
begin
  if (NOT FileExists(TerrainFileName)) then begin
    MessageShow('Terrain file not found');
    SysUtils.Beep; // note: there is a windows beep function that requires parameters
  end else begin
    MessageShow('Reading Terrain header...');
    AssignFile(Terrain_File,TerrainFileName);
    reset(Terrain_File);
//    seek(Terrain_File,0);
    BlockRead(Terrain_File,TerrainHeader,sizeof(CondorTerrainHeader));
    with TerrainHeader do begin
      MessageShow(format('UTM Zone:   %d %s',[tUTMzone, tUTMgrid[0]]));
      MessageShow(format('UTM Bottom: %1.3f',[tBottomMapNorthing]));
      MessageShow(format('UTM Right:  %1.3f',[tRightMapEasting]));
//      MessageShow(format('Height:     %d  (256 x %d)',[tHeight,tHeight div 256]));
      MessageShow(format('Height:     %d  (64 x %d, 256 x %1.2f)',[tHeight,tHeight div pRows,tHeight/tRows]));
//      MessageShow(format('Width:      %d  (256 x %d)',[tWidth,tWidth div 256]));
      MessageShow(format('Width:      %d  (64 x %d, 256 x %1.2f)',[tWidth,tWidth div pColumns,tWidth/tColumns]));
      MessageShow(format('Resolution: %1.6f',[tResolution]));
      MessageShow(format('Delta X:    %1.6f',[tDeltaX]));
      MessageShow(format('Delta Y:    %1.6f',[tDeltaY]));
    end;
    TerrainOpen := true;

    Close(Terrain_File);
  end;
end;

{----------------------------------------------------------------------------}
Procedure WriteTerrainHeader(TerrainFileName : string);
const
  ZeroByte : byte = 0;
begin
  AssignFile(Terrain_File,TerrainFileName);
  if (FileExists(TerrainFileName)) then begin
    MessageShow('Updating Terrain header');
//    reset(Terrain_File);
  end else begin
    MessageShow('Writing Terrain header');
//    rewrite(Terrain_File);
  end;
    rewrite(Terrain_File);  // ALWAYS start new file !
//  seek(Terrain_File,0);
  BlockWrite(Terrain_File,TerrainHeader,sizeof(CondorTerrainHeader));
  Close(Terrain_File);
end;

{----------------------------------------------------------------------------}
Procedure ForceTerrainSize(TerrainFileName : string);
const
  ZeroByte : byte = 0;
begin
  AssignFile(Terrain_File,TerrainFileName);
  if (NOT FileExists(TerrainFileName)) then begin
    MessageShow('Terrain file not found');
    beep; Exit;
  end;
  reset(Terrain_File);
  BlockRead(Terrain_File,TerrainHeader,sizeof(CondorTerrainHeader));
  // write dummy last value to force filesize
  with TerrainHeader do begin
    seek(Terrain_File,sizeof(CondorTerrainHeader)+tWidth*tHeight*TerrainSize-1);
  end;
  Write(Terrain_File,ZeroByte);
  Close(Terrain_File);
end;

// initial version without cropping
{----------------------------------------------------------------------------}
procedure xxx_Merge_TRN_File(Offset_X, Offset_Y : LongInt;
                          FilePath,Filename,
                          FilePath_a,Filename_a : string);
var
  TRN_File : File of Byte;      // use Word instead of Byte and terrain-size ???
  TRN_File_a : File of Byte;
  P : PByteArray;
  i : integer;
  FileIndex : LongInt;
  cWidth, cHeight : integer;

begin
  if (NOT FileExists(FilePath+'\'+Filename)) then begin
    MessageShow('Terrain file not found');
    Beep; Exit;
  end;
  AssignFile(TRN_File,FilePath+'\'+Filename);
  Reset(TRN_File);
  BlockRead(TRN_File,TerrainHeader,sizeof(CondorTerrainHeader));
  // keep width and height
  cWidth := TerrainHeader.tWidth;
  cHeight := TerrainHeader.tHeight;

  AssignFile(TRN_File_a,FilePath_a+'\'+Filename_a);
  Reset(TRN_File_a);
  BlockRead(TRN_File_a,TerrainHeader,sizeof(CondorTerrainHeader));

  with TerrainHeader do begin
    // need a buffer
    P := AllocMem(tHeight*2); // one block at a time, 2 bytes for elevation value

    ProgressBar_Status.Max := tWidth;
    for i := 0 to tWidth-1 do begin
      BlockRead(TRN_File_a,P^,tHeight*2);
      FileIndex := sizeof(CondorTerrainHeader) +
        (i + -Offset_X) * cHeight*2 +
        Offset_Y*2;
      seek(TRN_File,FileIndex);
      BlockWrite(TRN_File,P^,tHeight*2);
      ProgressBar_Status.StepIt;
      Application.ProcessMessages;
    end;
  end;

  freemem(P);
  CloseFile(TRN_File_a);
  CloseFile(TRN_File);
  ProgressBar_Status.Position := 0;
end;

{----------------------------------------------------------------------------}
procedure Merge_TRN_File(Offset_X, Offset_Y, Min_X, Max_X, Min_Y, Max_Y : LongInt;
                          FilePath,Filename,
                          FilePath_a,Filename_a : string);
var
  TRN_File : File of Byte;      // use Word instead of Byte and terrain-size ???
  TRN_File_a : File of Byte;
  P : PByteArray;
  i, i_Min, i_Max : integer;
  FileIndex : LongInt;
  cWidth, cHeight : integer;
  gWidth, gHeight : integer;
  j_DeltaL, j_DeltaR, j_Index, j_Width : integer;


begin
  if (NOT FileExists(FilePath+'\'+Filename)) then begin
    MessageShow('Terrain file not found');
    Beep; Exit;
  end;
  AssignFile(TRN_File,FilePath+'\'+Filename);
  Reset(TRN_File);
  BlockRead(TRN_File,TerrainHeader,sizeof(CondorTerrainHeader));
  // keep width and height
  cWidth := TerrainHeader.tWidth;
  cHeight := TerrainHeader.tHeight;


  AssignFile(TRN_File_a,FilePath_a+'\'+Filename_a);
  Reset(TRN_File_a);
  BlockRead(TRN_File_a,TerrainHeader,sizeof(CondorTerrainHeader));

  with TerrainHeader do begin
    gWidth := TerrainHeader.tWidth;
    gHeight := TerrainHeader.tHeight;
  end;

  try
    // need a buffer
    P := AllocMem(gHeight*TerrainSize); // one block at a time, 2 bytes for elevation value

// make limit calc common for BMP, TDM and TRN ???
    // diagonally mirrored for TRN file, i.e. swap width, height, i.e. X=Y' Y'=X
    // calculate vertical crop limits i_Min and i_Max
    if (Min_X > (-Offset_X)) then begin
      i_Min := Min_X - (-Offset_X);
    end else begin
      i_Min := 0;
    end;
    if (Max_X > (-Offset_X) + gWidth) then begin
      i_Max := gWidth;
    end else begin
      i_Max := Max_X - (-Offset_X);
    end;
    // calculate horizontal crop limits j_Delta, j_Index and j_Width
    if ( (Offset_Y+gHeight)< Max_Y) then begin // crop left ?
      j_Index := cHeight - (Offset_Y+gHeight - Min_Y);
      j_DeltaL := 0;
    end else begin
      j_Index := 0;
      j_DeltaL := Offset_Y+gHeight - Max_Y;
    end;
    j_DeltaR := 0;
    j_Width := gHeight - j_DeltaL;
    if (Min_Y > Offset_Y) then begin // crop right ?
      j_DeltaR := Min_Y + Offset_Y;
      j_Width := j_Width - j_DeltaR;
    end;

    ProgressBar_Status.Max := gWidth;
    for i := i_Min to i_Max-1 do begin
      // get input data    // do seek only once and then just sequential ?
      FileIndex := sizeof(CondorTerrainHeader) +
        ((i) * gHeight*TerrainSize);
      seek(TRN_File_a,FileIndex);
      BlockRead(TRN_File_a,P^,gHeight*TerrainSize);
      FileIndex := sizeof(CondorTerrainHeader) +
        (i + (-Offset_X) - Min_X) * cHeight*TerrainSize +
        j_Index*TerrainSize;
      seek(TRN_File,FileIndex);
      BlockWrite(TRN_File,P^[j_DeltaR*TerrainSize],j_Width*TerrainSize);
      ProgressBar_Status.StepIt;
      Application.ProcessMessages;
    end;
  Finally
    freemem(P);
    CloseFile(TRN_File_a);
    CloseFile(TRN_File);
    ProgressBar_Status.Position := 0;
  end;
end;

{----------------------------------------------------------------------------}
procedure Fix_TR3_Seams(Offset_X, Offset_Y : LongInt;
                        FilePath,Filename,
                        FilePath_a,Filename_a : string);
const
  File_Prefix = 'h';
  File_Ext = '.tr3';

var
  Data_File : File of Byte;
  Data_File_a : File of Byte;
  P : PWordArray;
  i,j,k : integer;
  FileIndex : LongInt;
  cWidth, cHeight : integer;
  startX, startY : integer;
  endX, endY : integer;

begin
  // load merged terrain file for reference extents
  if (NOT FileExists(FilePath+'\'+Filename)) then begin
    MessageShow('Terrain file not found');
    Beep; Exit;
  end;
  AssignFile(Data_File,FilePath+'\'+Filename);
  Reset(Data_File);
  BlockRead(Data_File,TerrainHeader,sizeof(CondorTerrainHeader));
  // keep width and height
  cWidth := TerrainHeader.tWidth div 64;
  cHeight := TerrainHeader.tHeight div 64;
  CloseFile(Data_File);

  AssignFile(Data_File_a,FilePath_a+'\'+Filename_a);
  Reset(Data_File_a);
  BlockRead(Data_File_a,TerrainHeader,sizeof(CondorTerrainHeader));
  startX := ((-Offset_X)) div 64;
  endX := (TerrainHeader.tWidth+(-Offset_X)) div 64;
  startY := (Offset_Y) div 64;
  endY := (TerrainHeader.tHeight+Offset_Y) div 64;
  CloseFile(Data_File_a);

  // need a buffer
  P := AllocMem(193*2); // one block at a time, 2 bytes for elevation value

  // check possible top seam
  // if on top edge of new scenery do nothing
  j := endY-1;
  if (j < cHeight-1) then begin  // not on top ?
    ProgressBar_Status.Max := endX-startX+1;
    for i := startX to endX-1 do begin
      // is there a file above ?
      FileName_a := format('%s%2.2d%2.2d%s',[File_Prefix,i,j+1,File_Ext]);
      if (FileExists(FilePath+'\HeightMaps\'+FileName_a)) then begin
        // open both files and copy overlap
        FileName := format('%s%2.2d%2.2d%s',[File_Prefix,i,j,File_Ext]);
        AssignFile(Data_File_a,FilePath+'\HeightMaps\'+Filename_a);
        Reset(Data_File_a);
        AssignFile(Data_File,FilePath+'\HeightMaps\'+Filename);
        Reset(Data_File);
        // copy bottom of top file to top of bottom file
        // i.e. first column, to last column
        for k := 0 to 193-1 do begin
          FileIndex := 0 + k*193*2;
          Seek(Data_File_a,FileIndex);
          BlockRead(Data_File_a,P^,2);
          Seek(Data_File,FileIndex + (193-1)*2);
          BlockWrite(Data_File,P^,2);
        end;
        CloseFile(Data_File);
        CloseFile(Data_File_a);
      end;
      ProgressBar_Status.StepIt;
      Application.ProcessMessages;
    end;
    ProgressBar_Status.Position := 0;
  end;

  // check possible bottom seam
  // if on bottom edge of new scenery do nothing
  j := startY;
  if (j > 0) then begin  // not on bottom ?
    ProgressBar_Status.Max := endX-startX+1;
    for i := startX to endX-1 do begin
      // is there a file below ?
      FileName_a := format('%s%2.2d%2.2d%s',[File_Prefix,i,j-1,File_Ext]);
      if (FileExists(FilePath+'\HeightMaps\'+FileName_a)) then begin
        // open both files and copy overlap
        FileName := format('%s%2.2d%2.2d%s',[File_Prefix,i,j,File_Ext]);
        AssignFile(Data_File_a,FilePath+'\HeightMaps\'+Filename_a);
        Reset(Data_File_a);
        AssignFile(Data_File,FilePath+'\HeightMaps\'+Filename);
        Reset(Data_File);
        // copy bottom of top file to top of bottom file
        // i.e. first column, to last column
        for k := 0 to 193-1 do begin
          FileIndex := 0 + k*193*2;
          Seek(Data_File,FileIndex);
          BlockRead(Data_File,P^,2);
          Seek(Data_File_a,FileIndex + (193-1)*2);
          BlockWrite(Data_File_a,P^,2);
        end;
        CloseFile(Data_File);
        CloseFile(Data_File_a);
      end;
      ProgressBar_Status.StepIt;
      Application.ProcessMessages;
    end;
    ProgressBar_Status.Position := 0;
  end;

  // check possible right seam
  // if on right edge of new scenery do nothing
  i := startX;
  if (i > 0) then begin  // not on right ?
    ProgressBar_Status.Max := endY-startY+1;
    for j := startY to endY-1 do begin
      // is there a file to the right ?
      FileName_a := format('%s%2.2d%2.2d%s',[File_Prefix,i-1,j,File_Ext]);
      if (FileExists(FilePath+'\HeightMaps\'+FileName_a)) then begin
        // open both files and copy overlap
        FileName := format('%s%2.2d%2.2d%s',[File_Prefix,i,j,File_Ext]);
        AssignFile(Data_File_a,FilePath+'\HeightMaps\'+Filename_a);
        Reset(Data_File_a);
        AssignFile(Data_File,FilePath+'\HeightMaps\'+Filename);
        Reset(Data_File);
        // copy right of left file to left of right file
        // i.e. first row, to last row
        FileIndex := 0 + (193-1)*193*2;
        Seek(Data_File,0);
        BlockRead(Data_File,P^,193*2);
        Seek(Data_File_a,FileIndex);
        BlockWrite(Data_File_a,P^,193*2);
        CloseFile(Data_File);
        CloseFile(Data_File_a);
      end;
      ProgressBar_Status.StepIt;
      Application.ProcessMessages;
    end;
    ProgressBar_Status.Position := 0;
  end;

  // check possible left seam
  // if on left edge of new scenery do nothing
  i := endX-1;
  if (i < cWidth-1) then begin  // not on left ?
    ProgressBar_Status.Max := endY-startY+1;
    for j := startY to endY-1 do begin
      // is there a file to the left ?
      FileName_a := format('%s%2.2d%2.2d%s',[File_Prefix,i+1,j,File_Ext]);
      if (FileExists(FilePath+'\HeightMaps\'+FileName_a)) then begin
        // open both files and copy overlap
        FileName := format('%s%2.2d%2.2d%s',[File_Prefix,i,j,File_Ext]);
        AssignFile(Data_File_a,FilePath+'\HeightMaps\'+Filename_a);
        Reset(Data_File_a);
        AssignFile(Data_File,FilePath+'\HeightMaps\'+Filename);
        Reset(Data_File);
        // copy right of left file to left of right file
        // i.e. first row, to last row
        FileIndex := 0 + (193-1)*193*2;
        Seek(Data_File_a,0);
        BlockRead(Data_File_a,P^,193*2);
        Seek(Data_File,FileIndex);
        BlockWrite(Data_File,P^,193*2);
        CloseFile(Data_File);
        CloseFile(Data_File_a);
      end;
      ProgressBar_Status.StepIt;
      Application.ProcessMessages;
    end;
    ProgressBar_Status.Position := 0;
  end;

  freemem(P);
end;

{----------------------------------------------------------------------------}
Procedure OverrideTerrainCalibration(TerrainFileName : string);
begin
  if (NOT FileExists(TerrainFileName)) then begin
    MessageShow('Terrain file not found');
  end else begin
    AssignFile(Terrain_File,TerrainFileName);
    reset(Terrain_File);
//    seek(Terrain_File,0);
    BlockRead(Terrain_File,TerrainHeader,sizeof(CondorTerrainHeader));
    with TerrainHeader do begin
      tDeltaX := -90.0;        // 90m
      tDeltaY :=  90.0;        // 90m
      //extend by 1/2 of 90m all around
      tRightMapEasting :=  UTM_Right +45.0;
      tBottomMapNorthing := UTM_Bottom -45.0;
    end;
    seek(Terrain_File,0);
    BlockWrite(Terrain_File,TerrainHeader,sizeof(CondorTerrainHeader));

    Close(Terrain_File);
    MessageShow('Terrain file modification done');
  end;
end;

{----------------------------------------------------------------------------}
Procedure ReadTerrainTile(TileColumn,TileRow: integer);
var
  i,j : integer;
  FileIndex : longint;

begin
  FileIndex := {TerrainHeaderSize} sizeof(CondorTerrainHeader) +
//               TerrainSize * (TileRow*tRows*(TileColumnCount*tColumns) +
////                              (((TileColumnCount-1)-TileColumn)*tColumns)
//                              (TileColumn*tColumns)
               TerrainSize * (TileColumn*tColumns*(TileRowCount*tRows) +
                              (TileRow*tRows)
                             );
  for i := tColumns-1 downto 0 do begin // columns from right
    seek(Terrain_File,FileIndex);
    for j := tRows-1 downto 0 do begin // rows from bottom
      // read each height element
      read(Terrain_File,HeightValue.ByteValue[0],HeightValue.ByteValue[1]);
      GradientGrid[j,i] := HeightValue.IntegerValue;
    end;
    INC(FileIndex,(TileRowCount*tRows) * TerrainSize);
  end;
end;

{----------------------------------------------------------------------------}
Procedure WriteBitmapGradientTile(TileColumn,TileRow: integer);
var
  i,j : integer;
  FileIndex : longint;
  FileByte : byte;
  gColor : Tcolor;
  cColor : ColorConvert;

begin
  FileIndex := BitmapHeader_24bitColor.Bitmap24PixelOffset +
               Color24Size * (TileRow*tRows*(TileColumnCount*tColumns) +
                              (((TileColumnCount-1)-TileColumn)*tColumns)
                             );
  for i := tRows-1 downto 0 do begin // rows from bottom
    seek(CG_File,FileIndex);
    for j := 0 to tColumns-1 do begin // columns from left
      // write each colorized height
      CalcGradientColor(GradientGrid[i,j],gColor);
//      cColor.ColorValue := ByteReverseOrder32(gColor);
      cColor.ColorValue := ByteSwapColor(gColor);
      BlockWrite(CG_File,cColor.cRGB,sizeof(cColor.cRGB))
//      FileByte := GetBValue(gColor);
//      Write(CG_File,FileByte);
//      FileByte := GetGValue(gColor);
//      Write(CG_File,FileByte);
//      FileByte := GetRValue(gColor);
//      Write(CG_File,FileByte);
    end;
    INC(FileIndex,(TileColumnCount*tColumns) * Color24Size);
  end;
end;

{----------------------------------------------------------------------------}
Procedure CreateColorGradientBitmap(TerrainFileName,ColorGradientFileName : string);
const
  HeaderOffset = 8;
  ZeroByte : byte = 0;

var
//  TileIndex : Integer;
  i, j :integer;
//  FileName : string;
//  FilePath : string;
//  FileByte : byte;
//  TileName : string;
//  BitmapFilePath : string;
//  gColor : Tcolor;
  LIC : LongIntConvert;

begin
  if (NOT FileExists(TerrainFileName)) then begin
    MessageShow('Terrain file not found');
  end else begin
    MessageShow('Creating color Gradient bitmap...');
    AssignFile(CG_File,{FilePath+'\'+}ColorGradientFileName);
    Rewrite(CG_File);
    // create a header
{    for i := 0 to 54-1 do begin
      Write(CG_File,BitmapHeader_24bitColor.Bitmap24[i]);
    end;
    seek(CG_File,BitmapHeader.BitmapImageByteSizeOffset);
    LIC.LongValue := ColumnCount*RowCount*Color24Size; // 3 for 24 bit color
    Write(CG_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
    seek(CG_File,BitmapHeader.BitmapFileByteSizeOffset);
    LIC.LongValue := LIC.LongValue+54;
    Write(CG_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
    seek(CG_File,BitmapHeader.BitmapWidthOffset);
    LIC.LongValue := ColumnCount;
    Write(CG_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
    seek(CG_File,BitmapHeader.BitmapHeightOffset);
    LIC.LongValue := RowCount;
    Write(CG_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
    seek(CG_File,BitmapHeader_24bitColor.Bitmap24PixelOffset);
}
    with xBitmapHeader_24bitColor do begin
      bDib.bWidth := ColumnCount;
      bDib.bHeight := RowCount;
      bDib.bImageByteSize := ColumnCount*RowCount*xColor24Size div 8;
      bH.bFileByteSize := bDib.bImageByteSize+bH.bPixelArrayOffset;
    end;
    BlockWrite(CG_File,xBitmapHeader_24bitColor,
      sizeof(xBitmapHeader_24bitColor));

    AssignFile(Terrain_File,TerrainFileName);
    Reset(Terrain_File);
{
    seek(Terrain_File,36);
    // order is wrong - for testing for now
    for i := 0 to ColumnCount-1 do begin
      for j := 0 to RowCount-1 do begin
        Read(Terrain_File,HeightValue.ByteValue[0],HeightValue.ByteValue[1]);
        CalcGradientColor(HeightValue.IntegerValue, gColor);
        FileByte := GetBValue(gColor);
        Write(CG_File,FileByte);
        FileByte := GetGValue(gColor);
        Write(CG_File,FileByte);
        FileByte := GetRValue(gColor);
        Write(CG_File,FileByte);
      end;
    end;
}
    //because of the row/column order change,
    //open the terrain file in block of tile size
    //and for each value convert height to color
    //and write a block

//  TileRowCount := RowCount div 256;
//  TileColumnCount := ColumnCount div 256;
    ProgressBar_Status.Max := (TileRowCount)*(TileColumnCount);
    for i := 0 to TileColumnCount-1 do begin
      for j := 0 to TileRowCount-1 do begin
        //read the terrain file
        ReadTerrainTile(i,j);
        //write to gradient file
        WriteBitmapGradientTile(i,j);
        ProgressBar_Status.StepIt;
        Application.ProcessMessages;
      end;
    end;

    Close(Terrain_File);
    Close(CG_File);
    MessageShow('Color gradient bitmap created');
    ProgressBar_Status.Position := 0;
  end;
end;

{----------------------------------------------------------------------------}
Procedure CreateSlopeGradient24bitBitmap(TerrainFileName,SlopeGradientFileName : string);
const
  HeaderOffset = 8;
  ZeroByte : byte = 0;
  SunAngle = 30.0/180*PI; //from vertical
  SunMin = 64;

var
  i, j :integer;
  FileByte : byte;
  PreviousHeight : integer;
  Angle : single;
  LIC : LongIntConvert;

begin
  if (NOT FileExists(TerrainFileName)) then begin
    MessageShow('Terrain file not found');
  end else begin
    MessageShow('Creating slope Gradient bitmap...');
    AssignFile(CG_File,{FilePath+'\'+}SlopeGradientFileName);
    Rewrite(CG_File);
    // create a header
    for i := 0 to 54-1 do begin
      Write(CG_File,BitmapHeader_24bitColor.Bitmap24[i]);
    end;
    seek(CG_File,BitmapHeader.BitmapImageByteSizeOffset);
    LIC.LongValue := ColumnCount*RowCount*Color24Size; // 3 for 24 bit color
    Write(CG_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
    seek(CG_File,BitmapHeader.BitmapFileByteSizeOffset);
    LIC.LongValue := LIC.LongValue+54;
    Write(CG_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
    seek(CG_File,BitmapHeader.BitmapWidthOffset);
//    LIC.LongValue := ColumnCount;
    LIC.LongValue := RowCount;
    Write(CG_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
    seek(CG_File,BitmapHeader.BitmapHeightOffset);
//    LIC.LongValue := RowCount;
    LIC.LongValue := ColumnCount;
    Write(CG_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
    seek(CG_File,BitmapHeader_24bitColor.Bitmap24PixelOffset);

    AssignFile(Terrain_File,TerrainFileName);
    Reset(Terrain_File);

    seek(Terrain_File,36);
    // not in bitmap order - need to swap column and rows
    ProgressBar_Status.Max := ColumnCount;
    for i := 0 to ColumnCount-1 do begin
      for j := 0 to RowCount-1 do begin
        BlockRead(Terrain_File,HeightValue.IntegerValue,2);
        if (j <> 0) then begin
          Angle := ARCTAN2(HeightValue.IntegerValue-PreviousHeight,90);
          FileByte := trunc(SunMin + (255-SunMin) * cos(Angle-SunAngle));
          Write(CG_File,FileByte);
          Write(CG_File,FileByte);
          Write(CG_File,FileByte);
        end;
        if (j = RowCount-1) then begin
          Write(CG_File,FileByte);
          Write(CG_File,FileByte);
          Write(CG_File,FileByte);
        end else begin
          PreviousHeight := HeightValue.IntegerValue;
        end;
      end;
      ProgressBar_Status.StepIt;
      Application.ProcessMessages;
    end;

    Close(Terrain_File);
    Close(CG_File);
    MessageShow('Slope gradient bitmap created');
    ProgressBar_Status.Position := 0;
  end;
end;

{----------------------------------------------------------------------------}
Procedure x8CreateSlopeGradientBitmap(TerrainFileName,SlopeGradientFileName : string);
const
  HeaderOffset = 8;
  ZeroByte : byte = 0;
  SunAngle = 30.0/180*PI; //from vertical
  SunMin = 64; // 25% of 256

var
  i, j :integer;
  FileByte : byte;
  PreviousHeight : integer;
  Angle : single;
  LIC : LongIntConvert;

begin
  if (NOT FileExists(TerrainFileName)) then begin
    MessageShow('Terrain file not found');
  end else begin
    MessageShow('Creating thermal sunny slope bitmap...');
    AssignFile(CG_File,{FilePath+'\'+}SlopeGradientFileName);
    Rewrite(CG_File);
    // create a header
    for i := 0 to 54-1 do begin
      Write(CG_File,BitmapHeader_24bitColor.Bitmap24[i]);
    end;
    seek(CG_File,BitmapHeader.BitmapImageByteSizeOffset);
    LIC.LongValue := ColumnCount*RowCount{*Color24Size}; // 8bit color
    Write(CG_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
    seek(CG_File,BitmapHeader.BitmapFileByteSizeOffset);
    LIC.LongValue := LIC.LongValue+54+(256*4);
    Write(CG_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
    seek(CG_File,BitmapHeader.BitmapWidthOffset);
    LIC.LongValue := ColumnCount;
    Write(CG_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
    seek(CG_File,BitmapHeader.BitmapHeightOffset);
    LIC.LongValue := RowCount;
    Write(CG_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
    seek(CG_File,BitmapHeader_24bitColor.Bitmap24PixelOffset);

    AssignFile(Terrain_File,TerrainFileName);
    Reset(Terrain_File);

    seek(CG_File,BitmapHeader.BitmapColorBitsOffset);
    LIC.LongValue := 8;
    Write(CG_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
    seek(CG_File,BitmapHeader.BitmapPixelArrayOffset);
    LIC.LongValue := 54+(256*4);
    Write(CG_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
    //write 256 color palette
    seek(CG_File,54);
    for i := 0 to 256-1 do begin
      Write(CG_File,byte(i),byte(i),byte(i),ZeroByte);
    end;

// create a bitmap and savetofile instead? faster?

    seek(Terrain_File,36);
    ProgressBar_Status.Max := ColumnCount;
    for i := 0 to ColumnCount-1 do begin
      for j := 0 to RowCount-1 do begin
        BlockRead(Terrain_File,HeightValue.IntegerValue,2);
        if (j <> 0) then begin
          Angle := ARCTAN2(HeightValue.IntegerValue-PreviousHeight,90);
          FileByte := trunc(SunMin + (255-SunMin) * cos(Angle-SunAngle));
          seek(CG_File,54+(256*4)+((j-1)*ColumnCount)+(ColumnCount-1-i));
          Write(CG_File,FileByte);
        end;
        if (j = RowCount-1) then begin
          seek(CG_File,54+(256*4)+((j)*ColumnCount)+(ColumnCount-1-i));
          Write(CG_File,FileByte);
        end else begin
          PreviousHeight := HeightValue.IntegerValue;
        end;
      end;
      ProgressBar_Status.StepIt;
      Application.ProcessMessages;
    end;

    Close(Terrain_File);
    Close(CG_File);
    MessageShow('Thermal sunny slope bitmap created');
    ProgressBar_Status.Position := 0;
  end;
end;

{----------------------------------------------------------------------------}
Procedure CreateSlopeGradientBitmap(TerrainFileName,SlopeGradientFileName : string);
const
  ZeroByte : byte = 0;
  SunAngle = 30.0/180*PI; //from vertical
  SunMin = 64; // 25% of 256

var
  i, j :integer;
  FileByte : byte;
  PreviousHeight : integer;
  Angle : single;
  pColor : ColorConvert;

begin
  if (NOT FileExists(TerrainFileName)) then begin
    MessageShow('Terrain file not found');
  end else begin
    MessageShow('Creating thermal sunny slope bitmap...');
    AssignFile(CG_File,{FilePath+'\'+}SlopeGradientFileName);
    Rewrite(CG_File);
    // create a header
    with BitmapHeader_8bitColor do begin
      bDib.bWidth := ColumnCount;
      bDib.bHeight := RowCount;
      bDib.bImageByteSize := ColumnCount*RowCount*Color8Size div 8;
      bH.bFileByteSize := bDib.bImageByteSize+bH.bPixelArrayOffset;
      BlockWrite(CG_File,BitmapHeader_8bitColor,
        sizeof(BitmapHeader_8bitColor));

      //write 256 color palette
      //seek(CG_File,sizeof(BMP_Header) + sizeof(BMP_DIB_Header));
      pColor.ByteValue[3]:=0;
      for i := 0 to 256-1 do begin //grey scale
//        Write(CG_File,byte(i),byte(i),byte(i),ZeroByte);
        pColor.ByteValue[0] := i;
        pColor.ByteValue[1] := i;
        pColor.ByteValue[2] := i;
        BlockWrite(CG_File,pColor.ColorValue,sizeof(pColor));
      end;

// create a bitmap and savetofile instead? faster?

      AssignFile(Terrain_File,TerrainFileName);
      Reset(Terrain_File);
      seek(Terrain_File,sizeof(CondorTerrainHeader));
      ProgressBar_Status.Max := ColumnCount;
      for i := 0 to ColumnCount-1 do begin
        for j := 0 to RowCount-1 do begin
          BlockRead(Terrain_File,HeightValue.IntegerValue,2);
          if (j <> 0) then begin
            Angle := ARCTAN2(HeightValue.IntegerValue-PreviousHeight,90);
            FileByte := trunc(SunMin + (255-SunMin) * cos(Angle-SunAngle));
            seek(CG_File,bH.bPixelArrayOffset+((j-1)*ColumnCount)+(ColumnCount-1-i));
            Write(CG_File,FileByte);
          end;
          if (j = RowCount-1) then begin
            seek(CG_File,bH.bPixelArrayOffset+((j)*ColumnCount)+(ColumnCount-1-i));
            Write(CG_File,FileByte);
          end else begin
            PreviousHeight := HeightValue.IntegerValue;
          end;
        end;
        ProgressBar_Status.StepIt;
        Application.ProcessMessages;
      end;
    end;

    Close(Terrain_File);
    Close(CG_File);
    MessageShow('Thermal sunny slope bitmap created');
    ProgressBar_Status.Position := 0;
  end;
end;

// Note: TRN file has data in columns, instead of rows
{----------------------------------------------------------------------------}
Procedure xTRN_To_Greyscale_Bitmap(TRN_FileName,Greyscale_FileName : string);
const
  ZeroByte : byte = 0;

var
  i, j :integer;
//  FileByte : byte;
  pColor : ColorConvert;
  TRN_File : File of byte;
  Greyscale_File : File of byte;
  TRN_Header : CondorTerrainHeader;
//  ByteCount : longint;
  Q : PWordArray;
  P : PByteArray;
  Value : single;
  WithPadding : integer;

begin
  if (NOT FileExists(TRN_FileName)) then begin
    MessageShow('TRN file not found');
    Exit;
  end;
  begin
    AssignFile(TRN_File,TRN_FileName);
    Reset(TRN_File);
//    seek(TRN_File,0);

    if (UpperCase(ExtractFileExt(TRN_FileName)) = '.TR3') then begin
      TRN_Header.tWidth := 193;
      TRN_Header.tHeight := 193;
    end else begin
      BlockRead(TRN_File,TRN_Header,sizeof(TRN_Header));
      i := TRN_Header.tWidth;
      TRN_Header.tWidth := TRN_Header.tHeight;
      TRN_Header.tHeight := i;
    end;
    MessageShow('Converting TRN file to greyscale bitmap...');
    AssignFile(Greyscale_File,Greyscale_FileName);
    Rewrite(Greyscale_File);
    // create a BMP header
    with BitmapHeader_8bitColor do begin
      bDib.bPaletteColors := 256; // 8 bit greyscale
      bDib.bWidth := TRN_Header.tWidth;
      bDib.bHeight := TRN_Header.tHeight;
      bDib.bImageByteSize := TRN_Header.tWidth*TRN_Header.tHeight*Color8Size div 8;
      bH.bFileByteSize := bDib.bImageByteSize+bH.bPixelArrayOffset;
      BlockWrite(Greyscale_File,BitmapHeader_8bitColor,
        sizeof(BitmapHeader_8bitColor));

      //write 256 color palette
      //seek(CG_File,sizeof(BMP_Header) + sizeof(BMP_DIB_Header));
      pColor.ByteValue[3]:=0;
      for i := 0 to 256-1 do begin //grey scale
//        Write(CG_File,byte(i),byte(i),byte(i),ZeroByte);
        pColor.ByteValue[0] := i;
        pColor.ByteValue[1] := i;
        pColor.ByteValue[2] := i;
        BlockWrite(Greyscale_File,pColor.ColorValue,sizeof(pColor));
      end;

      try
        P := AllocMem(TRN_Header.tWidth*2); // one row at a time
        Q := PWordArray(P);
        ProgressBar_Status.Max := TRN_Header.tHeight;
        for i := 0 to TRN_Header.tHeight-1 do begin
          BlockRead(TRN_File,P^,TRN_Header.tWidth*2);
          // convert integer to byte
          j := 0;
          While (j < TRN_Header.tWidth*2) do begin
                Value := Q^[j div 2];
                if value <= 1 then value := 1;
//                P^[j div 2] := trunc(50*log10(Value));
                P^[j div 2] := trunc(Value/8);      // 6000' max
                INC(j,2);
          end;
          //must be divisible by 4, if not, add 1, 2, or 3 bytes
          WithPadding := ((TRN_Header.tWidth + 3) div 4) * 4;
          BlockWrite(Greyscale_File,P^,WithPadding);

          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;
      finally
        freemem(P);
      end;

    end;

    Close(TRN_File);
    Close(Greyscale_File);
    MessageShow('TRN to greyscale bitmap created (mirrored diagonally)');
    ProgressBar_Status.Position := 0;
  end;
end;

{----------------------------------------------------------------------------}
Procedure RAW_To_Greyscale_Bitmap(RAW_FileName, Greyscale_FileName : string);
const
  ZeroByte : byte = 0;

var
  i, j :integer;
//  FileByte : byte;
  pColor : ColorConvert;
  RAW_File : File of byte;
  RAW_FileSize : longint;
  Greyscale_File : File of byte;
//  TRN_Header : CondorTerrainHeader;
//  ByteCount : longint;
  Q : PWordArray;
  P : PByteArray;
  Value : single;
  WithPadding : integer;
  ByteWidth : integer;
  HDR_Filename : string;
  WidthHeight : string;

{----------------------------------------------------------------------------}
function ParseWidthHeight(WidthHeight : string) : boolean;
var
  i : integer;
begin
  i := pos(',',WidthHeight);
  if (i = 0) then begin
    result := false;
  end else begin
    try
      ColumnCount := strToInt(copy(WidthHeight,1,i-1));
      try
        RowCount := strToInt(copy(WidthHeight,i+1,length(WidthHeight)));
        result := true;
      except
        result := false;
      end;
    except
      result := false;
    end;
  end;
end;

{----------------------------------------------------------------------------}
begin
  if (NOT FileExists(RAW_FileName)) then begin
    MessageShow('Error - File not found');
  end else begin
    AssignFile(RAW_File,RAW_FileName);
    Reset(RAW_File);
//    seek(RAW_File,0);
    // need to read a header to get width and height
    // first look for 'scenery.hdr'
    HeaderOpen := false;
    HDR_Filename := extractFilePath(RAW_FileName) + '\scenery.hdr';
    if (FileExists(HDR_Filename)) then begin // if found calc, size and see if 1x or 3x and select correct
      ReadSceneryHeader(HDR_Filename);
    end else begin  // if not, look for same name but with .hdr extension
      HDR_Filename := copy(RAW_FileName,1,length(RAW_FileName)-length(ExtractFileExt(RAW_FileName))) + '.hdr';
      if (FileExists(HDR_Filename)) then begin
        ReadSceneryHeader(HDR_Filename);
      end;
    end;

    if (NOT HeaderOpen) then begin  // if not, prompt for size
      WidthHeight := InputBox('Dimensions of terrain', 'Width, Height', 'Width, Height');
      if (NOT ParseWidthHeight(WidthHeight)) then begin
        MessageShow('Error - Invalid values');
        Beep;
        Exit;
      end;
    end;

    // validate - file size must be width * height * 2
    RAW_FileSize := FileSize(Raw_File);
    if (NOT (RAW_FileSize = ColumnCount * RowCount * 2) ) then begin
      if (RAW_FileSize = ColumnCount*3 * RowCount*3 * 2) then begin
        ColumnCount := ColumnCount * 3;
        RowCount := RowCount * 3;
      end else begin
        if (RAW_FileSize = (ColumnCount div 3) * (RowCount div 3) * 2) then begin
          ColumnCount := ColumnCount div 3;
          RowCount := RowCount div 3;
        end else begin
          MessageShow('Error - File size');
          Beep;
          Exit;
        end;
      end;
    end;

    // Bitmap byte width must be divisible by 4
    ByteWidth := ((ColumnCount * (Color8Size div 8) +3) div 4) * 4;
    MessageShow('Converting terrain file to greyscale bitmap...');
    AssignFile(Greyscale_File,Greyscale_FileName);
    Rewrite(Greyscale_File);
    // create a BMP header
    with BitmapHeader_8bitColor do begin
      bDib.bPaletteColors := 256; // 8 bit greyscale
      bDib.bWidth := ColumnCount;
      bDib.bHeight := RowCount;
      bDib.bImageByteSize := ColumnCount*RowCount*Color8Size div 8;
//      bH.bFileByteSize := bDib.bImageByteSize+bH.bPixelArrayOffset;
      bH.bFileByteSize := ColumnCount*(Color8Size div 8) *ByteWidth + bH.bPixelArrayOffset;
      BlockWrite(Greyscale_File,BitmapHeader_8bitColor,
        sizeof(BitmapHeader_8bitColor));

      //write 256 color palette
      //seek(CG_File,sizeof(BMP_Header) + sizeof(BMP_DIB_Header));
      pColor.ByteValue[3]:=0;
      for i := 0 to 256-1 do begin //grey scale
//        Write(CG_File,byte(i),byte(i),byte(i),ZeroByte);
        pColor.ByteValue[0] := i;
        pColor.ByteValue[1] := i;
        pColor.ByteValue[2] := i;
        BlockWrite(Greyscale_File,pColor.ColorValue,sizeof(pColor));
      end;
//      // write dummy last byte to force filesize
//      seek(Greyscale_File,bH.bPixelArrayOffset+ByteWidth*RowCount-1);
//      Write(Greyscale_File,pColor.ByteValue[3]);
      try
        P := AllocMem(ColumnCount*2); // one row at a time
        Q := PWordArray(P);
        ProgressBar_Status.Max := RowCount;
        for i := 0 to RowCount-1 do begin
          BlockRead(RAW_File,P^,ColumnCount*2);
          // convert integer to byte
          j := 0;
          While (j < ColumnCount*2) do begin
                Value := Q^[j div 2];
                if value <= 1 then value := 1;
                P^[j div 2] := trunc(60*log10(Value));
//                P^[j div 2] := trunc(Value/8);      // 6000' max
                INC(j,2);
          end;
	  //Now write bitmap row - in bitmap, first row data is bottom pixel row
          seek(Greyscale_File,bH.bPixelArrayOffset +  (RowCount-1 - i)*ByteWidth);
          BlockWrite(Greyscale_File,P^,ColumnCount);

          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;
      finally
        freemem(P);
      end;
    end;

    Close(RAW_File);
    Close(Greyscale_File);
    MessageShow('Greyscale bitmap created');
    ProgressBar_Status.Position := 0;
  end;
end;

{----------------------------------------------------------------------------}
Procedure TRN_To_Greyscale_Bitmap(TRN_FileName, Greyscale_FileName : string);
const
  ZeroByte : byte = 0;

var
  i, j :integer;
//  FileByte : byte;
  pColor : ColorConvert;
  TRN_File : File of byte;
  Greyscale_File : File of byte;
  TRN_Header : CondorTerrainHeader;
//  ByteCount : longint;
  Q : PWordArray;
  P : PByteArray;
  Value : single;
  WithPadding : integer;
  ByteWidth : integer;

begin
  if (NOT FileExists(TRN_FileName)) then begin
    MessageShow('Terrain file not found');
  end else begin
    AssignFile(TRN_File,TRN_FileName);
    Reset(TRN_File);
//    seek(TRN_File,0);

    if (UpperCase(ExtractFileExt(TRN_FileName)) = '.TR3') then begin
      ColumnCount := 193;
      RowCount := 193;
    end else begin
      if (UpperCase(ExtractFileExt(TRN_FileName)) = '.TRN') then begin
        BlockRead(TRN_File,TRN_Header,sizeof(TRN_Header));
        ColumnCount := TRN_Header.tWidth;
        RowCount := TRN_Header.tHeight;
      end else begin
        if (UpperCase(ExtractFileExt(TRN_FileName)) = '.RAW') then begin
//   MessageShow('Unable to convert'); Exit;
RAW_To_Greyscale_Bitmap(TRN_FileName, Greyscale_FileName);
exit;
        end else begin
          MessageShow('Unable to convert'); Exit;
        end;
      end;
    end;
    // Bitmap byte width must be divisible by 4
    ByteWidth := ((ColumnCount * (Color8Size div 8) +3) div 4) * 4;
    MessageShow('Converting terrain file to greyscale bitmap...');
    AssignFile(Greyscale_File,Greyscale_FileName);
    Rewrite(Greyscale_File);
    // create a BMP header
    with BitmapHeader_8bitColor do begin
      bDib.bPaletteColors := 256; // 8 bit greyscale
      bDib.bWidth := ColumnCount;
      bDib.bHeight := RowCount;
      bDib.bImageByteSize := ColumnCount*RowCount*Color8Size div 8;
//      bH.bFileByteSize := bDib.bImageByteSize+bH.bPixelArrayOffset;
      bH.bFileByteSize := ColumnCount*(Color8Size div 8) *ByteWidth + bH.bPixelArrayOffset;
      BlockWrite(Greyscale_File,BitmapHeader_8bitColor,
        sizeof(BitmapHeader_8bitColor));

      //write 256 color palette
      //seek(CG_File,sizeof(BMP_Header) + sizeof(BMP_DIB_Header));
      pColor.ByteValue[3]:=0;
      for i := 0 to 256-1 do begin //grey scale
//        Write(CG_File,byte(i),byte(i),byte(i),ZeroByte);
        pColor.ByteValue[0] := i;
        pColor.ByteValue[1] := i;
        pColor.ByteValue[2] := i;
        BlockWrite(Greyscale_File,pColor.ColorValue,sizeof(pColor));
      end;

      // write dummy last byte to force filesize
      seek(Greyscale_File,bH.bPixelArrayOffset+ByteWidth*RowCount-1);
      Write(Greyscale_File,pColor.ByteValue[3]);
      try
        P := AllocMem(RowCount*2); // one column at a time
        Q := PWordArray(P);
        ProgressBar_Status.Max := ColumnCount;
        for i := 0 to ColumnCount-1 do begin
          BlockRead(TRN_File,P^,RowCount*2);
          // convert integer to byte
          j := 0;
          While (j < RowCount*2) do begin
                Value := Q^[j div 2];
                if value <= 1 then value := 1;
                P^[j div 2] := trunc(60*log10(Value));
//                P^[j div 2] := trunc(Value/8);      // 6000' max
                INC(j,2);
          end;
	  //Now write as a column instead of a row
          for j := 0 to RowCount-1 do begin
            seek(Greyscale_File,bH.bPixelArrayOffset+((j)*ByteWidth)+(ColumnCount-1-i));
            Write(Greyscale_File,P^[j]);
          end;

          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;
      finally
        freemem(P);
      end;
    end;

    Close(TRN_File);
    Close(Greyscale_File);
    MessageShow('Greyscale bitmap created');
    ProgressBar_Status.Position := 0;
  end;
end;

{----------------------------------------------------------------------------}
Procedure ClampToZero(Q : PWordArray; Count : integer);
var
  i : integer;
begin
  for i := 0 to Count-1 do begin
//    if (Integer(Q^[i]) < 0) then begin  // typecast does not work ???
    if (Q^[i] > 32767) then begin
      Q^[i] := 0;
    end;
  end;
end;

// Terrain header must be prepared first !
{----------------------------------------------------------------------------}
Procedure RAW_To_TRN(RAW_FileName, TRN_FileName : string);
const
  ZeroByte : byte = 0;

var
  x, y :integer;
  RAW_File : File of Byte;
  TRN_File : File of Byte;
  Q : PWordArray;
//  Q : PIntArray;
  P : PByteArray;

begin
  if (NOT FileExists(RAW_FileName)) then begin
    MessageShow('RAW file not found');
  // for testing
    AssignFile(TRN_File,TRN_FileName);
    Rewrite(TRN_File);
    BlockWrite(TRN_File,TerrainHeader,sizeof(CondorTerrainHeader));
    Close(TRN_File);

  end else begin
    MessageShow('Converting RAW file to TRN...');
    AssignFile(RAW_File,RAW_FileName);
    Reset(RAW_File);
//    seek(RAW_File,0);

    AssignFile(TRN_File,TRN_FileName);
    Rewrite(TRN_File);
    BlockWrite(TRN_File,TerrainHeader,sizeof(CondorTerrainHeader));
    with TerrainHeader do begin
      // write dummy last value to force filesize
      seek(TRN_File,sizeof(CondorTerrainHeader)+tWidth*tHeight*TerrainSize-1);
      Write(TRN_File,ZeroByte);
      try
        P := AllocMem(tWidth*TerrainSize); // one row at a time
        Q := PWordArray(P);
        ProgressBar_Status.Max := tHeight;
        for y := 0 to tHeight-1 do begin
          BlockRead(RAW_File,P^,tWidth*TerrainSize);
          ClampToZero(Q,tWidth);
	  //Now write as a column instead of a row
          for x := 0 to tWidth-1 do begin
//            seek(TRN_File,sizeof(CondorTerrainHeader) + TerrainSize* ( (x*tHeight)+(tHeight-1-y)) );
            seek(TRN_File,sizeof(CondorTerrainHeader) + TerrainSize* ( ((tWidth-1-x)*tHeight)+(tHeight-1-y)) );
//            Write(TRN_File,Q^[x]); // only writes a byte
            Write(TRN_File,P^[x*2],P^[x*2+1]);
          end;

          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;
      finally
        freemem(P);
      end;
    end;

    Close(RAW_File);
    Close(TRN_File);
    MessageShow('TRN file created');
    ProgressBar_Status.Position := 0;
  end;
end;

{----------------------------------------------------------------------------}
Procedure Interpolate(Q3 : PWordArray; Size: integer);
var
  i : integer;
begin
  for i := 0 to Size-1 do begin
    if (i > 0) then begin
      Q3^[i*3+0] := round((Q3^[i*3-2]+Q3^[i*3+1]*2)/3);
    end else begin
      Q3^[i*3+0] := Q3^[i*3+1];
    end;
    if (i < Size-1) then begin
      Q3^[i*3+2] := round((Q3^[i*3+1]*2+Q3^[i*3+4])/3);
    end else begin
      Q3^[i*3+2] := Q3^[i*3+1];
    end;
  end;
end;

// must read terrain file first or set terrain header directly
{----------------------------------------------------------------------------}
Procedure RAW_To_RAW3(RAW_FileName, RAW3_FileName : string);
const
  ZeroByte : byte = 0;

var
  x, y :integer;
  RAW_File : File of Byte;
  RAW3_File : File of Byte;
//  Q : PWordArray;
////  Q : PIntArray;
//  P : PByteArray;
  P : PWordArray;
  P2 : PWordArray;
  Q3 : PWordArray;

begin
  if (NOT FileExists(RAW_FileName)) then begin
    MessageShow('RAW file not found');
  end else begin
    MessageShow('Converting RAW file to 3xRAW...');
    AssignFile(RAW_File,RAW_FileName);
    Reset(RAW_File);
//    seek(RAW_File,0);

    AssignFile(RAW3_File,RAW3_FileName);
    Rewrite(RAW3_File);
    with TerrainHeader do begin
      // write dummy last value to force filesize
      seek(RAW3_File,tWidth*3*tHeight*3*TerrainSize-1);
      Write(RAW3_File,ZeroByte);
      seek(RAW3_File,0);
      try
        P := AllocMem(tWidth*TerrainSize);    // one row at a time
        P2 := AllocMem(tWidth*TerrainSize);   // next row
        Q3 := AllocMem(tWidth*3*TerrainSize); // intermediate rows
        ProgressBar_Status.Max := tHeight;
        BlockRead(RAW_File,P^,tWidth*TerrainSize);
        for x := 0 to tWidth-1 do begin
          Q3^[x*3+1] := P^[x];
        end;
        Interpolate(@Q3[0],tWidth);                       // main row prepared
        BlockWrite(RAW3_File,Q3^,tWidth*3*TerrainSize);   // duplicate main for first row
        for y := 1 to tHeight-1 do begin
          BlockWrite(RAW3_File,Q3^,tWidth*3*TerrainSize); // main row
          BlockRead(RAW_File,P2^,tWidth*TerrainSize);
          for x := 0 to tWidth-1 do begin
            Q3^[x*3+1] := round((P^[x]*2+P2^[x])/3);
          end;
          Interpolate(@Q3[0],tWidth);
          BlockWrite(RAW3_File,Q3^,tWidth*3*TerrainSize); // intermediate 1
          for x := 0 to tWidth-1 do begin
            Q3^[x*3+1] := round((P^[x]+P2^[x]*2)/3);
          end;
          Interpolate(@Q3[0],tWidth);
          BlockWrite(RAW3_File,Q3^,tWidth*3*TerrainSize); // intermediate 2
          for x := 0 to tWidth-1 do begin
            Q3^[x*3+1] := P2^[x];
          end;
          Interpolate(@Q3[0],tWidth);                     // next main row prepared
          // keep as previous row
          for x := 0 to tWidth-1 do begin
            P^[x] := P2^[x];
          end;
          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;
        BlockWrite(RAW3_File,Q3^,tWidth*3*TerrainSize);   // main row
        BlockWrite(RAW3_File,Q3^,tWidth*3*TerrainSize);   // duplicate main as last row
      finally
        freemem(Q3);
        freemem(P2);
        freemem(P);
      end;
    end;

    Close(RAW_File);
    Close(RAW3_File);
    MessageShow('3xRAW file created');
    ProgressBar_Status.Position := 0;
{
 TBD, do in read terrain ?
    UTM_Zone    := 0;
    UTM_ZoneNS  := 0;
    UTM_Left    := 0;
    UTM_Bottom  := 0;
    UTM_Right   := 0;
    UTM_Top     := 0;
    RowCount    := 0;
    ColumnCount := 0;
    WriteSceneryHeader(RAW_FileName+'.hdr');  // for now
}
  end;
end;

{----------------------------------------------------------------------------}
Procedure TRN_To_RAW(TRN_FileName, RAW_FileName : string);
const
  ZeroByte : byte = 0;

var
  x, y :integer;
  RAW_File : File of Byte;
  TRN_File : File of Byte;
  Q : PWordArray;
//  Q : PIntArray;
  P : PByteArray;

begin
  if (NOT FileExists(TRN_FileName)) then begin
    MessageShow('TRN file not found');
  end else begin
    MessageShow('Converting TRN file to RAW...');
    AssignFile(TRN_File,TRN_FileName);
    Reset(TRN_File);
//    seek(TRN_File,0);
    BlockRead(TRN_File,TerrainHeader,sizeof(CondorTerrainHeader));

    AssignFile(RAW_File,RAW_FileName);
    Rewrite(RAW_File);
    with TerrainHeader do begin
      // write dummy last value to force filesize
      seek(RAW_File,tWidth*tHeight*TerrainSize-1);
      Write(RAW_File,ZeroByte);
      try
        P := AllocMem(tHeight*TerrainSize); // one row of terrain at a time (one column of RAW)
        Q := PWordArray(P);
        ProgressBar_Status.Max := tWidth;
        for x := 0 to tWidth-1 do begin
          BlockRead(TRN_File,P^,tHeight*TerrainSize);
	  //Now write as a column of RAW instead of a row of terrain
          for y := 0 to tHeight-1 do begin
//            seek(RAW_File,TerrainSize* ( (y*tWidth)+(tWidth-1-x)) );
            seek(RAW_File,TerrainSize* ( ((tHeight-1-y)*tWidth)+(tWidth-1-x)) );
//            Write(RAW_File,Q^[x]); // only writes a byte
            Write(RAW_File,P^[y*2],P^[y*2+1]);
          end;

          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;
      finally
        freemem(P);
      end;
    end;

    Close(TRN_File);
    Close(RAW_File);
    MessageShow('RAW file created');
    ProgressBar_Status.Position := 0;
{
 TBD, do in read terrain ?
    UTM_Zone    := 0;
    UTM_ZoneNS  := 0;
    UTM_Left    := 0;
    UTM_Bottom  := 0;
    UTM_Right   := 0;
    UTM_Top     := 0;
    RowCount    := 0;
    ColumnCount := 0;
    WriteSceneryHeader(RAW_FileName+'.hdr');  // for now
}
  end;
end;

// not really possible to extend to 193 x193 by reading rows from source
// give up on this version
// Terrain header must be prepared first !
// source file is multiple of 192, but output file is in multiple of 193
// RAW source arranged in rows, top row first, left first
// TR3 output arranged in columns, right column first, bottom first
// need to add one overlap on left and top
{----------------------------------------------------------------------------}
Procedure xRAW_To_TR3(RAW_FileName, TR3_FilePath : string);
const
  ZeroByte : byte = 0;
  st_Width =  192;
  st_Height = 192;
var
  st_X, st_Y :integer;
  st_X_Offset, st_Y_Offset :integer;
  st_ColumnCount, st_RowCount :integer;
  X, Y :integer;
  RAW_File : File of Byte;
  TR3_File : File of Byte;
  FileName : string;
  Q : PWordArray;
  P : PByteArray;

begin
  if (NOT FileExists(RAW_FileName)) then begin
    MessageShow('RAW file not found');
  end else begin
    MessageShow('Converting RAW file to TR3...');
    AssignFile(RAW_File,RAW_FileName);
    Reset(RAW_File);
//    seek(RAW_File,0);

    if (NOT DirectoryExists(TR3_FilePath)) then begin
      ForceDirectories(TR3_FilePath);
    end;

    st_ColumnCount := TerrainHeader.tWidth div 256 * 4;
    st_RowCount := TerrainHeader.tHeight div 256 * 4;

    ProgressBar_Status.Max := st_ColumnCount*st_RowCount;
    for st_X := 0 to st_ColumnCount-1 do begin
      for st_Y := 0 to st_RowCount-1 do begin
        FileName := TR3_FilePath+'\h'+format('%2.2d%2.2d.tr3',[st_X,st_Y]);
        st_X_Offset := (st_ColumnCount-1- st_X) * (256 * 3 div 4);
        st_Y_Offset := (st_RowCount-1- st_Y) * (256 * 3 div 4);

    AssignFile(TR3_File,FileName);
    Rewrite(TR3_File);
    begin
      // write dummy last value to force filesize
      seek(TR3_File,st_Width*st_Height*TerrainSize-1);
      Write(TR3_File,ZeroByte);
      try
        P := AllocMem(st_Width*TerrainSize); // one row at a time
        Q := PWordArray(P);
        for Y := 0 to st_Height-1 do begin
          seek(RAW_File, TerrainSize* ( st_X_Offset + (st_ColumnCount* (256 * 3 div 4))*(st_Y_Offset+Y)) );
          BlockRead(RAW_File,P^,st_Width*TerrainSize);
          ClampToZero(Q,st_Width);
	  //Now write as a column instead of a row
          for X := 0 to st_Width-1 do begin
            seek(TR3_File, TerrainSize* ( ((st_Width-1-X)*st_Height)+(st_Height-1-Y)) );
//            Write(TRN_File,Q^[x]); // only writes one byte
            Write(TR3_File,P^[X*2],P^[X*2+1]);
          end;

          Application.ProcessMessages;
        end;
      finally
        freemem(P);
      end;
    end;
    Close(TR3_File);
    ProgressBar_Status.StepIt;
    Application.ProcessMessages;

      end;
    end;

    Close(RAW_File);
    MessageShow('TR3 files created');
    ProgressBar_Status.Position := 0;
  end;
end;


// Terrain header must be prepared first !
// Read source in columns and output as rows
// source file is multiple of 192, but output file is in multiple of 193
// RAW source arranged in rows, top row first, left first
// TR3 output arranged in columns, right column first, bottom first
// need to add one overlap on left and top
// ??? compares OK with Condor generated TR3 except for last column which
// is offset by one too low. Last two terrain columns (last two file rows) should be
// be identical but are not - Condor BUG or intentional ?
// because too low, 0000 value appears in lowest, i.e. bottom left corner in Condor
{----------------------------------------------------------------------------}
Procedure RAW_To_TR3(RAW_FileName, TR3_FilePath : string);
const
  st_Size = 192;
  ZeroByte : byte = 0;
  Overlap = 1;  // overlap on left an top
  st_Width =  st_Size + Overlap;
  st_Height = st_Size + Overlap;
var
  st_X, st_Y :integer;
  st_X_Offset, st_Y_Offset :integer;
  st_ColumnCount, st_RowCount :integer;
  X, Y :integer;
  st_WidthMax :integer;
  st_HeightMax :integer;
  RAW_File : File of Byte;
  TR3_File : File of Byte;
  FileName : string;
  Q : PWordArray;
  P : PByteArray;

begin
  if (NOT FileExists(RAW_FileName)) then begin
    MessageShow('RAW file not found');
  end else begin
    MessageShow('Converting RAW file to TR3...');
    AssignFile(RAW_File,RAW_FileName);
    Reset(RAW_File);
//    seek(RAW_File,0);

    if (NOT DirectoryExists(TR3_FilePath)) then begin
      ForceDirectories(TR3_FilePath);
    end;

//    st_ColumnCount := TerrainHeader.tWidth div 256 * 4;
    st_ColumnCount := TerrainHeader.tWidth div pColumns;
//    st_RowCount := TerrainHeader.tHeight div 256 * 4;
    st_RowCount := TerrainHeader.tHeight div pRows;

    P := AllocMem(st_Height*TerrainSize); // allocate memory for one terrain column
    Q := PWordArray(P);

    ProgressBar_Status.Max := st_ColumnCount*st_RowCount;
    for st_X := 0 to st_ColumnCount-1 do begin
      for st_Y := 0 to st_RowCount-1 do begin
        FileName := TR3_FilePath+'\h'+format('%2.2d%2.2d.tr3',[st_X,st_Y]);
//        st_X_Offset := (st_ColumnCount-1- st_X) * (256 * 3 div 4);
        st_X_Offset := (st_ColumnCount-1- st_X) * (pColumns * 3);
//        st_Y_Offset := (st_RowCount-1- st_Y) * (256 * 3 div 4);
        st_Y_Offset := (st_RowCount-1- st_Y) * (pRows * 3);

        AssignFile(TR3_File,FileName);
        Rewrite(TR3_File);
        begin
          // write dummy last value to force filesize
          seek(TR3_File,st_Width*st_Height*TerrainSize-1);
          Write(TR3_File,ZeroByte);
          seek(TR3_File,0);
          try
            if (st_X_Offset = 0) then begin // special case for overlap - top edge
              st_WidthMax := st_Width-Overlap;
            end else begin
              st_WidthMax := st_Width;
            end;
            for X := 0 to st_WidthMax-1 do begin  // terrain column becomes file row
              if (st_Y_Offset = 0) then begin // special case for overlap - top edge
                st_HeightMax := st_Height-Overlap;
              end else begin
                st_HeightMax := st_Height;
              end;
              for Y := 0 to st_HeightMax-1 do begin
                seek(RAW_File, TerrainSize* ( st_X_Offset +(st_Size-1-X) +
                  (st_ColumnCount* st_Size)*(st_Y_Offset+(st_Size-1-Y))) );
                Read(RAW_File,P^[Y*2],P^[Y*2+1]);
              end;
              if (st_Y_Offset = 0) then begin // special case for overlap - top edge
                // duplicate last value
                P^[(st_Height-1)*2] :=   P^[(st_Height-2)*2];
                P^[(st_Height-1)*2+1] := P^[(st_Height-2)*2+1];
              end;

              //Now write as a row instead of a column
              ClampToZero(Q,st_Height);
              BlockWrite(TR3_File,P^,st_Height*TerrainSize);
              Application.ProcessMessages;
            end;
            if (st_X_Offset = 0) then begin // special case for overlap - left edge
              // duplicate last column
              ClampToZero(Q,st_Height);
              BlockWrite(TR3_File,P^,st_Height*TerrainSize);
            end;
          finally
          end;
        end;
        Close(TR3_File);
        ProgressBar_Status.StepIt;
        Application.ProcessMessages;

      end;
    end;

    freemem(P);
    Close(RAW_File);
    MessageShow('TR3 files created');
    ProgressBar_Status.Position := 0;
  end;
end;

{----------------------------------------------------------------------------}
Procedure UpdateTerrainUTMgrid_V2(TerrainFileName : string);
begin
  if (NOT FileExists(TerrainFileName)) then begin
    MessageShow('Terrain file not found');
  end else begin
    AssignFile(Terrain_File,TerrainFileName);
    reset(Terrain_File);
//    seek(Terrain_File,0);
    BlockRead(Terrain_File,TerrainHeader,sizeof(CondorTerrainHeader));
    with TerrainHeader do begin
      // UTM zone grid (A..Z or N/S) (only first char used (?))
      // if <= 'N' -> south, if >= 'O' -> north
      if (uppercase(tUTMgrid[0]) >= 'O') then begin
        tUTMgrid[0] := 'N';
      end else begin
        tUTMgrid[0] := 'S';
      end;
    end;
    seek(Terrain_File,0);
    BlockWrite(Terrain_File,TerrainHeader,sizeof(CondorTerrainHeader));

    Close(Terrain_File);
  end;
end;

{----------------------------------------------------------------------------}
begin { Initialization }
end.

{--- End of File ------------------------------------------------------------}


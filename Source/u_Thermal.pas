{
 * u_Thermal.pas
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
UNIT u_Thermal;

{----------------------------------------------------------------------------
The condor .TDM file is structured as follows:
- 8 byte header
  - 4 bytes for width (little-endian) = 256 * number of horizontal tiles
  - 4 bytes for height (little-endian) = 256 * number of vertical tiles
- n bytes -> n = 256*columns * 256*rows
  - each byte value
    - corresponds to a 90x90 metre square
    - contains a "thermal" value - 0 (no heat) to 255 (max heat)
  - byte order in file
    - by row
    - bottom row first
    - starting from the right
----------------------------------------------------------------------------}

{----------------------------------------------------------------------------
Implementation is based on reading tile size bitmaps and converting to
Condor thermal map format.
If a Thermal Map already exists, it is opened and data is added to it, else it
is first created in full with default thermal data.
Each tile is loaded and the the data written into the Thermal map.
If a tile doesn't exist that area of the thermal map is unchanged.
----------------------------------------------------------------------------}

//===========================================================================
INTERFACE

uses StdCtrls, comctrls,
  u_SceneryHDR;

const
  ThermalSize = 1; // size of value in file in bytes
//  tRows = 256;     //thermal tile height
//  tColumns = 256;  //thermal tile width
  ThermalHeader : record
    ThermalWidthOffset : longint;
    ThermalHeightOffset : longint;
    ThermalPixelOffset : longint;
  end = (ThermalWidthOffset:0;
         ThermalHeightOffset:4;
         ThermalPixelOffset:8;);

type
  TDM_Header_Type = record
    Width : longint;
    Height : longint;
  end;

var
  Memo_Message : TMemo;  // external TMemo for messages
  ProgressBar_Status : TProgressBar;
  SourceThermalFolder : string; // external path for file
  DestinationThermalFolder : string; // external path for file
  ThermalResolution : integer = 1;   // relative resolution with reference to tRows
  ThermalGrid : array[0..tRows-1,0..tColumns-1] of byte;
  Heating : Array[0..8] of byte;
  TDM_Header : TDM_Header_Type;

Procedure CreateThermalMap(ThermalFileName : string);
Procedure CreateThermalBitmap(ThermalFileName : string);
Procedure CreateThermalTileBitmap(ThermalFileName : string);
function ReadThermalBitmapTileIndexes(FileName:string) : boolean;
Procedure ClearThermalGrid;
Procedure TDM_To_Greyscale_Bitmap(TDM_FileName,Greyscale_FileName : string);
Procedure Greyscale_Bitmap_To_TDM(Greyscale_FileName,TDM_FileName : string);
Procedure WriteTDMHeader(TDM_FileName : string);
//Procedure ForceTDMsize(TDM_FileName : string);
Procedure ForceTDMsize(TDM_FileName : string; Default_Value : integer);
procedure Merge_TDM_File(Offset_X, Offset_Y, Min_X, Max_X, Min_Y, Max_Y : LongInt;
                          FilePath,Filename,
                          FilePath_a,Filename_a : string);

//===========================================================================
IMPLEMENTATION

uses forms, SysUtils, Graphics,
  {u_SceneryHDR,} u_TileList, u_BMP, u_MakeThermal, u_Forest, Unit_Graphics;

var
  Thermal_File : File of byte;

{----------------------------------------------------------------------------}
Procedure MessageShow(Info : string);
begin
  if (Memo_Message <> nil) then begin
    Memo_Message.lines.add(Info);
  end;
end;

{----------------------------------------------------------------------------}
Procedure ClearThermalGrid;
var
  i, j : integer;

begin
  for i := 0 to tRows-1 do begin
    for j := 0 to tColumns-1 do begin
      ThermalGrid[i,j] := 0;
    end;
  end;
end;

{----------------------------------------------------------------------------
bitmap format
- by row
- bottom row first
- starting from the left
----------------------------------------------------------------------------}
function ReadThermalBitmapTile(FileName:string) : boolean;
// create an array of bytes 256x256
// read each bitmap RGB and convert to thermal index
const
  tRGB : ColorConvert = ( ColorValue: $00000000 );

var
  BitmapFile : File of byte;
  i,j : integer;

begin
  Result := false; // assume for now
  //make sure bitmap is 24 bit color and 256x256
  Bitmap_GetWidthHeight(FileName);
  if NOT ((BitmapWidth = tColumns) AND (BitmapHeight = tRows)) then begin
    MessageShow('Bitmap file needs to be 256x256, 24 bit color');
    Beep;
  end else begin
    AssignFile(BitmapFile,u_BMP.BMPfolder+'\'+FileName);
    Reset(BitmapFile);
    seek(BitmapFile,BitmapHeader_24bitColor.Bitmap24PixelOffset);
    for i := tRows-1 downto 0 do begin // rows from bottom
      for j := 0 to tColumns-1 do begin // columns from left
        // read each pixel
        BlockRead(BitmapFile,tRGB.cRGB,sizeof(tRGB.cRGB));
        // match color to a thermal index
        if (tRGB.ColorValue = tWater.ColorValue) then begin
          ThermalGrid[i,j] := {1}Heating[3]; //0.5%
        end else begin
          if (tRGB.ColorValue = tSwamp.ColorValue) then begin
            ThermalGrid[i,j] := {217}Heating[2]; //85%
          end else begin
            if (tRGB.ColorValue = tSand.ColorValue) then begin
              ThermalGrid[i,j] := {230}Heating[7];  //90%
            end else begin
              if (tRGB.ColorValue = tYellowFields.ColorValue) then begin
                ThermalGrid[i,j] := {192}Heating[6]; //70%
              end else begin
                if (tRGB.ColorValue = tGreenFields.ColorValue) then begin
                  ThermalGrid[i,j] := {153}Heating[5]; //60%
                end else begin
                  if (tRGB.ColorValue = tDarkFields.ColorValue) then begin
                    ThermalGrid[i,j] := {230}Heating[4]; //90%
                  end else begin
                    if (tRGB.ColorValue = tDeciduous.ColorValue) then begin
                      if (ThermalGrid[i,j] = 0) then begin // not already done
                        ThermalGrid[i,j] := {1} Heating[1];
                      end;
                    end else begin
                      if (tRGB.ColorValue = tConiferous.ColorValue) then begin
                        if (ThermalGrid[i,j] = 0) then begin // not already done
                          ThermalGrid[i,j] := {2} Heating[2];
                        end;
                      end else begin
                        if (tRGB.ColorValue = tBoth.ColorValue) then begin
                          if (ThermalGrid[i,j] = 0) then begin // not already done
                            ThermalGrid[i,j] := {2} (Heating[1] + Heating[2]) div 2;
                          end;
                        end else begin
                          if (ThermalGrid[i,j] = 0) then begin // if no forest, or no forest tile
                            ThermalGrid[i,j] := {200}Heating[8]; //assume default (green fields)
                          end else begin
                            //leave as is
                          end;
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    Close(BitmapFile);
    Result := true;
  end;
end;

{----------------------------------------------------------------------------
bitmap format
- by row
- bottom row first
- starting from the left
----------------------------------------------------------------------------}
function ReadThermalBitmapTileIndexes(FileName:string) : boolean;
// create an array of bytes 256x256
// read each bitmap RGB and convert to thermal index
const
  tRGB : ColorConvert = ( ColorValue: $00000000 );

var
  BitmapFile : File of byte;
  i,j : integer;

begin
  Result := false; // assume for now
  //make sure bitmap is 24 bit color and 256x256
  Bitmap_GetWidthHeight(FileName);
  if NOT ((BitmapWidth = tColumns) AND (BitmapHeight = tRows)) then begin
    MessageShow('Bitmap file needs to be 256x256, 24 bit color');
    Beep;
  end else begin
    AssignFile(BitmapFile,u_BMP.BMPfolder+'\'+FileName);
    Reset(BitmapFile);
    seek(BitmapFile,BitmapHeader_24bitColor.Bitmap24PixelOffset);
    for i := tRows-1 downto 0 do begin // rows from bottom
      for j := 0 to tColumns-1 do begin // columns from left
        // read each pixel
        BlockRead(BitmapFile,tRGB.cRGB,sizeof(tRGB.cRGB));
        // match color to a thermal index
        if (tRGB.ColorValue = tWater.ColorValue) then begin
          ThermalGrid[i,j] := 3 {Heating[3]};
        end else begin
          if (tRGB.ColorValue = tSwamp.ColorValue) then begin
            ThermalGrid[i,j] := 2 {Heating[2]};
          end else begin
            if (tRGB.ColorValue = tSand.ColorValue) then begin
              ThermalGrid[i,j] := 7 {Heating[7]};
            end else begin
              if (tRGB.ColorValue = tYellowFields.ColorValue) then begin
                ThermalGrid[i,j] := 6 {Heating[6]};
              end else begin
                if (tRGB.ColorValue = tGreenFields.ColorValue) then begin
                  ThermalGrid[i,j] := 5 {Heating[5]};
                end else begin
                  if (tRGB.ColorValue = tDarkFields.ColorValue) then begin
                    ThermalGrid[i,j] := 4 {Heating[4]};
                  end else begin
                    if (tRGB.ColorValue = tDeciduous.ColorValue) then begin
                      if (ThermalGrid[i,j] = 0) then begin // not already done
                        ThermalGrid[i,j] := 1 {Heating[1]};
                      end;
                    end else begin
                      if (tRGB.ColorValue = tConiferous.ColorValue) then begin
                        if (ThermalGrid[i,j] = 0) then begin // not already done
                          ThermalGrid[i,j] := 2 {Heating[2]};
                        end;
                      end else begin
                        if (tRGB.ColorValue = tBoth.ColorValue) then begin
                          if (ThermalGrid[i,j] = 0) then begin // not already done
                            ThermalGrid[i,j] := 2 {Heating[2]};
                          end;  
                        end else begin
                          if (ThermalGrid[i,j] = 0) then begin // if no forest, or no forest tile
                            ThermalGrid[i,j] := 8  {Heating[8]}; //assume default
                           end;
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    Close(BitmapFile);
    Result := true;
  end;
end;

{----------------------------------------------------------------------------}
function ThermalTileUpdate(i,j : integer) : boolean;
var
  tFileName : string;
  fFileName : string;
  FilePath : string;
  TileName : string;

begin
  Result := false;
  TileName := format('%2.2d%2.2d',[i,j]);
  tFileName := TileName+'_t.bmp';
  fFileName := TileName+'_f.bmp';
  FilePath := SourceThermalFolder +'\SourceTiles\'+TileName;
  u_BMP.BMPfolder := FilePath;

  //get forest data first
  if (FileExists(FilePath+'\'+fFileName)) then begin
    Result := true;
    //read the bitmap file
    if ReadForestBitmapTile(fFileName) then begin
    end;
      ConvertForestMask(Heating[0],Heating[1],Heating[8]);
  end else begin
    ClearThermalGrid;
  end;

  if (FileExists(FilePath+'\'+tFileName)) then begin
    Result := true;
    //read the thermal bitmap file
    if ReadThermalBitmapTile(tFileName) then begin
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure ScaleThermalTile(TileColumn,TileRow: integer);
var
  SlopeBitmapFile : File of byte;
  SlopeFileName : string;
  FileIndex : longint;
  FileByte : byte;
//  ThermalValue : word;
  i, j : integer;

begin
  if (u_MakeThermal.Form_MakeThermal.RadioButton_SunnySlopes.Checked) then begin
    SlopeFileName := SourceThermalFolder+'\ThermalMap\SunnySlopes.bmp';
    if (NOT FileExists(SlopeFileName)) then begin
      MessageShow('SunnySlopes.bmp not found');
      Exit;
    end;
    AssignFile(SlopeBitmapFile,SlopeFileName);
    Reset(SlopeBitmapFile);

    FileIndex := 54+(256*4) +
      {Color8Size *} (TileRow*tRows*(TileColumnCount*tColumns) +
                      (((TileColumnCount-1)-TileColumn)*tColumns)
                     );
    for i := tRows-1 downto 0 do begin // rows from bottom
      seek(SlopeBitmapFile,FileIndex);
      for j :=  0 to tColumns-1 do begin // columns from left
        // write each thermal index
        read(SlopeBitmapFile,FileByte);
//        ThermalValue := FileByte * ThermalGrid[i,j];
//        ThermalValue := ThermalValue{(FileByte * ThermalGrid[i,j])} div 255;
//        ThermalGrid[i,j] := ThermalValue;
        ThermalGrid[i,j] := (FileByte * ThermalGrid[i,j]) div 255;
//        ThermalGrid[i,j] := FileByte; // for testing
//        ThermalGrid[i,j] := 0; // for testing
      end;
      INC(FileIndex,(TileColumnCount*tColumns){*Color8Size});
    end;
    close(SlopeBitmapFile);
  end;
end;

{----------------------------------------------------------------------------}
Procedure WriteThermalTile(TileColumn,TileRow: integer);
var
  i,j : integer;
  FileIndex : longint;

begin
  FileIndex := ThermalHeader.ThermalPixelOffset +
               ThermalSize * ((TileRow * tRows * (TileColumnCount*tColumns)) +
                              (TileColumn * tColumns) );
  for i := tRows-1 downto 0 do begin      // rows from bottom
    seek(Thermal_File,FileIndex);
    for j := tColumns-1 downto 0 do begin // columns from right
      // write each thermal index
      write(Thermal_File,ThermalGrid[i,j]);
    end;
    INC(FileIndex,(TileColumnCount*tColumns) * ThermalSize);
  end;
end;

{----------------------------------------------------------------------------
.TDM format
- by row
- bottom row first
- starting from the right
----------------------------------------------------------------------------}
Procedure CreateThermalMap(ThermalFileName : string);
//type
//  LongwordArray = array of Longword;
//  PLongwordArray = ^LongwordArray;

const
  ZeroByte : byte = 0;

var
  SlopeBitmapFile : File of byte;
  SlopeFileName : string;
  i, j :integer;
  tFileName : string;
  fFileName : string;
  FilePath : string;
  FileByte : byte;
  TileName : string;
  ByteCount : longint;
//  P : PLongwordArray;  //??? doesn't work ???
  P : PWordArray;
//  P : PByteArray;

begin
  FilePath := DestinationThermalFolder;
  if (NOT FileExists(FilePath+'\'+ThermalFileName)) then begin
    MessageShow('Creating Thermal map...');
    AssignFile(Thermal_File,FilePath+'\'+ThermalFileName);
    Rewrite(Thermal_File);
    // create a header
    FileByte := HI(ColumnCount);
    Write(Thermal_File,ZeroByte,FileByte,ZeroByte,ZeroByte);
    FileByte := HI(RowCount);
    Write(Thermal_File,ZeroByte,FileByte,ZeroByte,ZeroByte);

    // create a default file for now
    // SunnySlopes or uniform?
    if (u_MakeThermal.Form_MakeThermal.RadioButton_SunnySlopes.Checked) then begin
      SlopeFileName := SourceThermalFolder+'\ThermalMap\SunnySlopes.bmp';
      if (NOT FileExists(SlopeFileName)) then begin
        MessageShow('SunnySlopes.bmp not found');
        Exit;
      end;
      AssignFile(SlopeBitmapFile,SlopeFileName);
      Reset(SlopeBitmapFile);
      seek(SlopeBitmapFile,54+(256*4));

    try
      P := AllocMem(ColumnCount); // one row at a time
      ProgressBar_Status.Max := RowCount;
      for i := 0 to RowCount-1 do begin            // rows from top
{ doesn't work on 64 bit system ? - don't use seek to adjust filesize.
        for j := 0 to ColumnCount-1 do begin       // columns from left
          Read(SlopeBitmapFile,FileByte);
          seek(Thermal_File,ThermalHeader.ThermalPixelOffset+
            (i*ColumnCount)+(ColumnCount-1-j)); // columns from right
          write(Thermal_File,FileByte);
        end;
}
        BlockRead(SlopeBitmapFile,P^,ColumnCount);
        SwapBlockEndToEnd(PByteArray(P),ColumnCount);
        BlockWrite(Thermal_File,P^,ColumnCount);

        ProgressBar_Status.StepIt;
        Application.ProcessMessages;
      end;
    finally
      freemem(P);
    end;

      Close(SlopeBitmapFile);
    end else begin // uniform
      ByteCount := ColumnCount * RowCount;
      ProgressBar_Status.Max := ByteCount div 256;
      try
        P := AllocMem(256); // block of 256 bytes; bytes are set to 0
  //      for i := 0 to 256 div sizeof(longword)-1 do begin
  //        P^[i] := $FFFFFFFF; //all max thermal
//        for i := 0 to 256 div sizeof(word)-1 do begin
        for i := 0 to (256 div sizeof(p^[0]))-1 do begin
          P^[i] := $FFFF; //all max thermal
        end;
        While ByteCount > 256  do begin
          BlockWrite(Thermal_File,P^,256);
          DEC(ByteCount,256);
          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;
        BlockWrite(Thermal_File,P^,ByteCount);
      finally
        freemem(P);
      end;
    end;

    MessageShow('Thermal map created');
    ProgressBar_Status.Position := 0;
  end else begin
    AssignFile(Thermal_File,FilePath+'\'+ThermalFileName);
    Reset(Thermal_File);
    MessageShow('Thermal map opened');
  end;

  u_BMP.Memo_Message := Memo_Message;
  //for each tile look for the Thermal file and open if present or skip it if not
//  TileRowCount := RowCount div 256;
//  TileColumnCount := ColumnCount div 256;
  ProgressBar_Status.Max := (TileRowCount)*(TileColumnCount);
  for i := 0 to TileColumnCount-1 do begin
    for j := 0 to TileRowCount-1 do begin
      //see if forest and/or thermal files are present
      if (ThermalTileUpdate(i,j)) then begin
        //scale by sunny slope if wanted
        ScaleThermalTile(i,j);
        //write to Thermal file
        WriteThermalTile(i,j);
      end;
      ProgressBar_Status.StepIt;
      Application.ProcessMessages;
    end;
  end;

  Close(Thermal_File);
  MessageShow('Thermal map done.');
  ProgressBar_Status.Position := 0;

  TDM_To_Greyscale_Bitmap(FilePath+'\'+ThermalFileName,SourceThermalFolder+'\ThermalMap\Greyscale.bmp');

end;

{----------------------------------------------------------------------------}
Procedure WriteThermalBitmapTile(TileColumn,TileRow: integer);
var
  i,j : integer;
  FileIndex : longint;

begin
  FileIndex := BitmapHeader_24bitColor.Bitmap24PixelOffset +
               Color24Size * (TileRow*tRows*(TileColumnCount*tColumns) +
                              (((TileColumnCount-1)-TileColumn)*tColumns)
                             );
  for i := tRows-1 downto 0 do begin // rows from bottom
    seek(Thermal_File,FileIndex);
    for j := 0 to tColumns-1 do begin // columns from left
      // write each thermal index, greyscale
      write(Thermal_File,ThermalGrid[i,j],ThermalGrid[i,j],ThermalGrid[i,j]);
    end;
    INC(FileIndex,(TileColumnCount*tColumns) * Color24Size);
  end;
end;

{----------------------------------------------------------------------------}
Procedure CreateThermalBitmap(ThermalFileName : string);
const
  ZeroByte : byte = 0;

var
  SlopeBitmapFile : File of byte;
  SlopeFileName : string;
  i, j :integer;
//  tFileName : string;
//  fFileName : string;
  FilePath : string;
  FileByte : byte;
//  TileName : string;
  ByteCount : longint;
  P : PWordArray;
  TileUpdate : boolean;

begin
  FilePath := {SourceThermalFolder} DestinationThermalFolder;
  if (NOT FileExists(FilePath+'\'+ThermalFileName)) then begin
    MessageShow('Creating Thermal bitmap...');
    AssignFile(Thermal_File,FilePath+'\'+ThermalFileName);
    Rewrite(Thermal_File);
    // create a header
    with xBitmapHeader_24bitColor do begin
      bDib.bWidth := ColumnCount;
      bDib.bHeight := RowCount;
      bDib.bImageByteSize := ColumnCount*RowCount*xColor24Size div 8;
      bH.bFileByteSize := bDib.bImageByteSize+bH.bPixelArrayOffset;
    end;
    BlockWrite(Thermal_File,xBitmapHeader_24bitColor,
      sizeof(xBitmapHeader_24bitColor));

    // create a default file
    if (u_MakeThermal.Form_MakeThermal.RadioButton_SunnySlopes.Checked) then begin
      SlopeFileName := SourceThermalFolder+'\ThermalMap\SunnySlopes.bmp';
      if (NOT FileExists(SlopeFileName)) then begin
        MessageShow('SunnySlopes.bmp not found');
        Exit;
      end;
      AssignFile(SlopeBitmapFile,SlopeFileName);
      Reset(SlopeBitmapFile);
      seek(SlopeBitmapFile,54+(256*4));
      ProgressBar_Status.Max := RowCount;
      for i := 0 to RowCount-1 do begin      // rows from bottom
        for j := 0 to ColumnCount-1 do begin // Columns from left
          Read(SlopeBitmapFile,FileByte);
//          seek(Thermal_File,54+(i*3*ColumnCount)+3*(j)); // Columns from left
          write(Thermal_File,FileByte,FileByte,FileByte);
        end;
        ProgressBar_Status.StepIt;
        Application.ProcessMessages;
      end;
      Close(SlopeBitmapFile);
    end else begin
//      ByteCount := ColumnCount * RowCount;
      ByteCount := xBitmapHeader_24bitColor.bDib.bImageByteSize;
      ProgressBar_Status.Max := ByteCount div 256;
      try
        P := AllocMem(256); // block of 256 bytes; bytes are set to 0
        for i := 0 to 256 div sizeof(word)-1 do begin
          P^[i] := $FFFF; //all max thermal
        end;
        While ByteCount > 256  do begin
          BlockWrite(Thermal_File,P^,256);
          DEC(ByteCount,256);
          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;
        BlockWrite(Thermal_File,P^,ByteCount);
      finally
        freemem(P);
      end;
    end;

    ProgressBar_Status.Position := 0;
    MessageShow('Thermal bitmap created');
  end else begin
    AssignFile(Thermal_File,FilePath+'\'+ThermalFileName);
    Reset(Thermal_File);
    MessageShow('Thermal bitmap opened');
  end;

  u_BMP.Memo_Message := Memo_Message;
  //for each tile, look for the Thermal file and open if present or skip it if not
  ProgressBar_Status.Max := (TileRowCount)*(TileColumnCount);
  for i := 0 to TileColumnCount-1 do begin
    for j := 0 to TileRowCount-1 do begin
      //see if forest and/or thermal files are present
      if (ThermalTileUpdate(i,j)) then begin
        //scale by sunny slope if wanted
        ScaleThermalTile(i,j);
        //write to Thermal file
        WriteThermalBitmapTile(i,j);
      end;
      ProgressBar_Status.StepIt;
      Application.ProcessMessages;
    end;
  end;

  Close(Thermal_File);
  MessageShow('Thermal bitmap done.');
  ProgressBar_Status.Position := 0;
end;

{----------------------------------------------------------------------------}
Procedure CreateThermalTileBitmap(ThermalFileName : string);
var
  i, j : integer;
  BitmapFile : File of byte;
  P : PByteArray;

begin
    MessageShow('Creating thermal bitmap');
    AssignFile(BitmapFile,ThermalFileName);
    Rewrite(BitmapFile);
    // create a header
    with xBitmapHeader_24bitColor do begin
      bDib.bWidth := tColumns;
      bDib.bHeight := tRows;
      bDib.bImageByteSize := tColumns*tRows*xColor24Size div 8;
      bH.bFileByteSize := bDib.bImageByteSize+bH.bPixelArrayOffset;
    end;
    BlockWrite(BitmapFile,xBitmapHeader_24bitColor,
      sizeof(xBitmapHeader_24bitColor));
    try
      P := AllocMem(tColumns*Color24Size); // one row; bytes are set to 0
      for i := 0 to tRows-1 do begin       // tRows to do
        BlockWrite(BitmapFile,P^,tColumns*Color24Size);
      end;
    finally
      freemem(P);
    end;

    Close(BitmapFile);
    MessageShow('Thermal tile bitmap created');
end;

// Note: TDM has data reversed in horizontal direction 
{----------------------------------------------------------------------------}
Procedure TDM_To_Greyscale_Bitmap(TDM_FileName,Greyscale_FileName : string);
const
  ZeroByte : byte = 0;

var
  i, j :integer;
//  FileByte : byte;
  pColor : ColorConvert;
  TDM_File : File of byte;
  Greyscale_File : File of byte;
  TDM_Header : TDM_Header_Type;
//  ByteCount : longint;
  P : PWordArray;

begin
  if (NOT FileExists(TDM_FileName)) then begin
    MessageShow('TDM file not found');
    Exit;
  end;
  begin
    AssignFile(TDM_File,TDM_FileName);
    Reset(TDM_File);
    seek(TDM_File,0);
    BlockRead(TDM_File,TDM_Header,sizeof(TDM_Header));
    MessageShow('Converting TDM file to greyscale bitmap...');
    AssignFile(Greyscale_File,Greyscale_FileName);
    Rewrite(Greyscale_File);
    // create a BMP header
    with BitmapHeader_8bitColor do begin
      bDib.bPaletteColors := 256; // 8 bit greyscale
      bDib.bWidth := TDM_Header.Width;
      bDib.bHeight := TDM_Header.Height;
      bDib.bImageByteSize := TDM_Header.Width*TDM_Header.Height*Color8Size div 8;
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
{
      ByteCount := bDib.bImageByteSize;
      ProgressBar_Status.Max := ByteCount div 256;
      try
        P := AllocMem(256); // block of 256 bytes; bytes are set to 0
        While ByteCount > 256  do begin
          BlockRead(TDM_File,P^,256);
          BlockWrite(Greyscale_File,P^,256);
          DEC(ByteCount,256);
          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;
        BlockRead(TDM_File,P^,ByteCount);
        BlockWrite(Greyscale_File,P^,ByteCount);
      finally
        freemem(P);
      end;
}
      try
        P := AllocMem(TDM_Header.Width); // one row at a time
        ProgressBar_Status.Max := TDM_Header.Height;
        for i := 0 to TDM_Header.Height-1 do begin
          BlockRead(TDM_File,P^,TDM_Header.Width);
          SwapBlockEndToEnd(PByteArray(P),TDM_Header.Width);
          BlockWrite(Greyscale_File,P^,TDM_Header.Width);

          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;
      finally
        freemem(P);
      end;

    end;

    Close(TDM_File);
    Close(Greyscale_File);
    MessageShow('TDM to greyscale bitmap created');
    ProgressBar_Status.Position := 0;
  end;
end;

// Note: TDM has data reversed in horizontal direction
{----------------------------------------------------------------------------}
Procedure Greyscale_Bitmap_To_TDM(Greyscale_FileName,TDM_FileName : string);
const
  ZeroByte : byte = 0;

var
  i, j :integer;
//  FileByte : byte;
//  pColor : ColorConvert;
  TDM_File : File of byte;
  Greyscale_File : File of byte;
  TDM_Header : TDM_Header_Type;
//  ByteCount : longint;
  P : PWordArray;

begin
  if (NOT FileExists(Greyscale_FileName)) then begin
    MessageShow('Greyscale file not found');
    Exit;
  end;
  begin
    MessageShow('Converting Greyscale bitmap to TDM file...');
    AssignFile(Greyscale_File,Greyscale_FileName);
    Reset(Greyscale_File);
    seek(Greyscale_File,0);
    BlockRead(Greyscale_File,BitmapHeader_8bitColor,sizeof(BitmapHeader_8bitColor));
    AssignFile(TDM_File,TDM_FileName);
    Rewrite(TDM_File);
    // create a TDM header
    with BitmapHeader_8bitColor do begin
      // first confirm it is bmp and greyscale, i.e. 8 bit color
      if ((bH.bSignature <> $4D42) OR              // BitmapSignature
          (bDib.bPaletteColors <> 256)) then begin // 8 bit color
        MessageShow('Error: Not 8 bit color bitmap');
      end else begin
      TDM_Header.Width := bDib.bWidth;
      TDM_Header.Height := bDib.bHeight;
      BlockWrite(TDM_File,TDM_Header,
        sizeof(TDM_Header));

      //skip over palette; assume it is sequential 0..255
      seek(Greyscale_File,bH.bPixelArrayOffset);

      try
        P := AllocMem(TDM_Header.Width); // one row at a time
        ProgressBar_Status.Max := TDM_Header.Height;
        for i := 0 to TDM_Header.Height-1 do begin
          BlockRead(Greyscale_File,P^,TDM_Header.Width);
          SwapBlockEndToEnd(PByteArray(P),TDM_Header.Width);
          BlockWrite(TDM_File,P^,TDM_Header.Width);

          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;
      finally
        freemem(P);
      end;

      end;
    end;

    Close(Greyscale_File);
    Close(TDM_File);
    MessageShow('Greyscale bitmap to TDM file created');
    ProgressBar_Status.Position := 0;
  end;
end;

{----------------------------------------------------------------------------}
Procedure WriteTDMHeader(TDM_FileName : string);
const
  ZeroByte : byte = 0;
var
  TDM_File : File of byte;

begin
  AssignFile(TDM_File,TDM_FileName);
  if (FileExists(TDM_FileName)) then begin
    MessageShow('Updating Thermal header');
//    reset(TDM_File);
  end else begin
    MessageShow('Writing Thermal header');
//    rewrite(TDM_File);
  end;
    rewrite(TDM_File);  // ALWAYS start new file !
//  seek(TDM_File,0);
  BlockWrite(TDM_File,TDM_Header,sizeof(TDM_Header));
  Close(TDM_File);
end;

{----------------------------------------------------------------------------}
Procedure xxxForceTDMsize(TDM_FileName : string);
const
  ZeroByte : byte = 0;
var
  TDM_File : File of byte;
begin
  AssignFile(TDM_File,TDM_FileName);
  if (NOT FileExists(TDM_FileName)) then begin
    MessageShow('Thermal file not found');
    beep; Exit;
  end;
  reset(TDM_File);
  BlockRead(TDM_File,TDM_Header,sizeof(TDM_Header));
  // write dummy last value to force filesize
  with TDM_Header do begin
    seek(TDM_File,sizeof(TDM_Header)+Width*Height-1);
  end;
  Write(TDM_File,ZeroByte);
  Close(TDM_File);
end;

{----------------------------------------------------------------------------}
Procedure ForceTDMsize(TDM_FileName : string; Default_Value : integer);
var
  TDM_File : File of byte;
  P : PByteArray;
  ByteCount : longint;
begin
  AssignFile(TDM_File,TDM_FileName);
  if (NOT FileExists(TDM_FileName)) then begin
    MessageShow('Thermal file not found');
    beep; Exit;
  end;
  reset(TDM_File);
  BlockRead(TDM_File,TDM_Header,sizeof(TDM_Header));
 try
  with TDM_Header do begin
    P := AllocMem(512); // initial values
    for ByteCount := 0 to 512-1 do begin
      P^[ByteCount] := Default_Value;
    end;

    ByteCount := Width*Height;
    while (ByteCount > 0) do begin
      if (ByteCount > 512) then begin
        BlockWrite(TDM_File,P^,512);
      end else begin
        BlockWrite(TDM_File,P^,ByteCount);
      end;
      DEC(ByteCount,512);
    end;
  end;

 finally
   freemem(P);
 end;

  Close(TDM_File);
end;

// initial version without cropping
{----------------------------------------------------------------------------}
procedure xxx_Merge_TDM_File(Offset_X, Offset_Y : LongInt;
                          FilePath,Filename,
                          FilePath_a,Filename_a : string);
var
  TDM_File : File of Byte;
  TDM_File_a : File of Byte;
  P : PByteArray;
  i : integer;
  FileIndex : LongInt;
  cWidth, cHeight : integer;
  TDM_Header : TDM_Header_Type;

begin
  if (NOT FileExists(FilePath+'\'+Filename)) then begin
    MessageShow('Thermal file not found');
    Beep; Exit;
  end;
  AssignFile(TDM_File,FilePath+'\'+Filename);
  Reset(TDM_File);
  BlockRead(TDM_File,TDM_Header,sizeof(TDM_Header));
  // keep width and height
  cWidth := TDM_Header.Width;
  cHeight := TDM_Header.Height;

  AssignFile(TDM_File_a,FilePath_a+'\'+Filename_a);
  Reset(TDM_File_a);
  BlockRead(TDM_File_a,TDM_Header,sizeof(TDM_Header));

  with TDM_Header do begin
    // need a buffer
    P := AllocMem(Width);

    ProgressBar_Status.Max := Height;
    for i := 0 to Height-1 do begin
      BlockRead(TDM_File_a,P^,Width);
      FileIndex := sizeof(TDM_Header) +
        (i + Offset_Y) * cWidth +
        -Offset_X;
      seek(TDM_File,FileIndex);
      BlockWrite(TDM_File,P^,Width);
      ProgressBar_Status.StepIt;
      Application.ProcessMessages;
    end;
    Freemem(P);
  end;

  CloseFile(TDM_File_a);
  CloseFile(TDM_File);
  ProgressBar_Status.Position := 0;
end;

{----------------------------------------------------------------------------}
procedure Merge_TDM_File(Offset_X, Offset_Y, Min_X, Max_X, Min_Y, Max_Y : LongInt;
                          FilePath,Filename,
                          FilePath_a,Filename_a : string);
var
  TDM_File : File of Byte;
  TDM_File_a : File of Byte;
  P : PByteArray;
  i, i_Min, i_Max : integer;
  FileIndex : LongInt;
  cWidth, cHeight : integer;
  gWidth, gHeight : integer;
  TDM_Header : TDM_Header_Type;
  j_DeltaL, j_DeltaR, j_Index, j_Width : integer;

begin
  if (NOT FileExists(FilePath+'\'+Filename)) then begin
    MessageShow('Thermal file not found');
    Beep; Exit;
  end;
  AssignFile(TDM_File,FilePath+'\'+Filename);
  Reset(TDM_File);
  BlockRead(TDM_File,TDM_Header,sizeof(TDM_Header));
  // keep width and height
  cWidth  := TDM_Header.Width;
  cHeight := TDM_Header.Height;

  if (NOT FileExists(FilePath_a+'\'+Filename_a)) then begin
//    MessageShow('Warning: '+Filename_a+' file not found');
    Beep;
  end else begin
    AssignFile(TDM_File_a,FilePath_a+'\'+Filename_a);
    Reset(TDM_File_a);
    BlockRead(TDM_File_a,TDM_Header,sizeof(TDM_Header));

    with TDM_Header do begin
      gWidth  := TDM_Header.Width;
      gHeight := TDM_Header.Height;
    end;

    try
      // need a buffer
      P := AllocMem(gWidth);
// make limit calc common for BMP, TDM and TRN ???
      // calculate vertical crop limits i_Min and i_Max
      if (Min_Y > Offset_Y) then begin
        i_Min := Min_Y - Offset_Y;
      end else begin
        i_Min := 0;
      end;
      if (Max_Y > Offset_Y + gHeight) then begin
        i_Max := gHeight;
      end else begin
        i_Max := Max_Y - Offset_Y;
      end;
      // calculate horizontal crop limits j_Delta, j_Index and j_Width
      if ( ((-Offset_X)+gWidth)< Max_X) then begin // crop left ?
        j_Index := cWidth - ((-Offset_X)+gWidth - Min_X);
        j_DeltaL := 0;
      end else begin
        j_Index := 0;
        j_DeltaL := (-Offset_X)+gWidth - Max_X;
      end;
      j_DeltaR := 0;
      j_Width := gWidth - j_DeltaL;
      if (Min_X > (-Offset_X)) then begin // crop right ?
        j_DeltaR := Min_X + (-Offset_X);
        j_Width := j_Width - j_DeltaR;
      end;

      ProgressBar_Status.Max := gHeight;
      for i := i_Min to i_Max-1 do begin
        // get input data    // do seek only once and then just sequential ?
        FileIndex := sizeof(TDM_Header) +
          ((i) * gWidth);
        seek(TDM_File_a,FileIndex);
        BlockRead(TDM_File_a,P^,gWidth);
        // write output data
        FileIndex := sizeof(TDM_Header) +
          ((i + Offset_Y - Min_Y) * cWidth) +
//          j_Index;  // bug, need to reverse order for TDM
          (cWidth - j_Width - j_Index);
        seek(TDM_File,FileIndex);
        // use delta right offset because data in in reverse order for TDM
        BlockWrite(TDM_File,P^[j_DeltaR],j_Width);
        ProgressBar_Status.StepIt;
        Application.ProcessMessages;
      end;
    finally
      Freemem(P);
      CloseFile(TDM_File_a);
      ProgressBar_Status.Position := 0;
    end;
  end;
  CloseFile(TDM_File);
end;

{----------------------------------------------------------------------------}
begin { Initialization }
  Memo_Message := nil;
end.

{--- End of File ------------------------------------------------------------}

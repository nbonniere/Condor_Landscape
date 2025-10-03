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

uses
  StdCtrls, comctrls,
  u_SceneryHDR;

const
  ThermalSize = 1; // size of value in file in bytes
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
  ThermalGrid : array[0..tRows-1,0..tColumns-1] of byte; // [y,x]
  Heating : Array[0..8] of byte;
  TDM_Header : TDM_Header_Type;

Procedure CreateThermalMap(ThermalFileName : string);
Procedure CreateThermalBitmap(ThermalFileName : string);
Procedure CreateThermalTileBitmap(ThermalFileName : string);
function ReadThermalBitmapTileIndexes(FileName:string) : boolean;
Procedure ClearThermalGrid;
Procedure TDM_To_Greyscale_Bitmap(TDM_FileName,Greyscale_FileName : string);
Procedure Greyscale_Bitmap_To_TDM(Greyscale_FileName,TDM_FileName : string);
Procedure TM3_To_Color_Bitmap(TM3_FileName,Color_FileName : string);
Procedure Color_Bitmap_To_TM3(Color_FileName, TM3_FileName : string);
// for merging
Procedure WriteTDMHeader(TDM_FileName : string);
//Procedure ForceTDMsize(TDM_FileName : string);
Procedure MakeThermal_Blank(TDM_FileName : string; tWidth, tHeight : integer; Default_Value : integer);
Procedure ForceTDMsize(TDM_FileName : string; Default_Value : integer);
procedure Merge_TDM_File(Offset_X, Offset_Y, Min_X, Max_X, Min_Y, Max_Y : LongInt;
                          FilePath,Filename,
                          FilePath_a,Filename_a : string);

procedure MakeDummyBlank_TM3(Default_Value, Column, Row : integer; FilePath, Filename:string);
procedure MakeDummyTDM_TM3(LandscapePath,LandscapeName : string);
Procedure ForceTM3size(TM_FileName : string; Default_Value : integer);
procedure Merge_TM3_File(Offset_X, Offset_Y, Min_X, Max_X, Min_Y, Max_Y : LongInt;
                          FilePath,Filename,
                          FilePath_a,Filename_a : string);

//===========================================================================
IMPLEMENTATION

uses
  forms, Windows, SysUtils, Graphics, u_Terrain,
  u_TileList, u_BMP, u_MakeThermal, u_Forest, Unit_Graphics;

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
  x, y : integer;

begin
  for y := 0 to tRows-1 do begin
    for x := 0 to tColumns-1 do begin
      ThermalGrid[y,x] := 0;
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
  Bitmap_File : File of byte;
  Bitmap_Hdr : BMP_V1_Header;
  Palette_8 : BMP_8bit_ColorTable;
  Color_Index_8 : byte;
  x, y : integer;
  P8  : pByteArray;
  P24  : pRGBArray;

begin
  Result := false; // assume for now
  // make sure it exists
  if (NOT FileExists(u_BMP.BMPfolder+'\'+FileName)) then begin
    exit;
  end;
  //make sure bitmap is 8 or 24 bit color and 256x256
  AssignFile(Bitmap_File,u_BMP.BMPfolder+'\'+FileName);
  Reset(Bitmap_File);
  // read the file header
  BlockRead(Bitmap_File,Bitmap_Hdr,sizeof(Bitmap_Hdr));
  // check for bitmap signature
  if (Bitmap_Hdr.bH.bSignature <> $4D42) then begin
    MessageShow('Error: not bitmap');
    Close(Bitmap_File);
    exit;
  end;
  // check for correct size
  if ((Bitmap_Hdr.bDib.bWidth <> tColumns) OR
      (Bitmap_Hdr.bDib.bHeight <> tRows) ) then begin
    MessageShow('Error: bitmap size not 256');
    Close(Bitmap_File);
    exit;
  end;
  // now check the type 8 or 24 bit color
  case (Bitmap_Hdr.bDib.bColorBits) of
    8: begin
      // read the palette for the 8 bit main file to access colors
      BlockRead(Bitmap_File,Palette_8,sizeof(BMP_8bit_ColorTable));
    end;
    24: begin
    end;
    else begin
      MessageShow('Error: Not 8 or 24 bit color bitmap');
      Close(Bitmap_File);
      Exit;
    end;
  end;

  // now go through every pixel
  try
    P8  := AllocMem(tColumns * sizeof(Byte)); // one row at a time
    P24 := AllocMem(tColumns * sizeof(TRGBTriple)); // one row at a time
    for y := tRows-1 downto 0 do begin // rows from bottom
      case (Bitmap_Hdr.bDib.bColorBits) of
        8: begin
          BlockRead(Bitmap_File,P8^,tColumns * sizeof(Byte));
        end;
        24: begin
          BlockRead(Bitmap_File,P24^,tColumns * sizeof(tRGB.cRGB));
        end;
      end;
      // check each pixel
      for x := 0 to tColumns-1 do begin // columns from left
        case (Bitmap_Hdr.bDib.bColorBits) of
          8: begin
            tRGB.crgba := Palette_8[P8^[x]];
          end;
          24: begin
            tRGB.crgb := P24^[x];
          end;
        end;
        // match color to a thermal index
        if (tRGB.ColorValue = tWater.ColorValue) then begin
          ThermalGrid[y,x] := {1}Heating[3]; //0.5%
        end else begin
          if (tRGB.ColorValue = tSwamp.ColorValue) then begin
            ThermalGrid[y,x] := {217}Heating[2]; //85%
          end else begin
            if (tRGB.ColorValue = tSand.ColorValue) then begin
              ThermalGrid[y,x] := {230}Heating[7];  //90%
            end else begin
              if (tRGB.ColorValue = tYellowFields.ColorValue) then begin
                ThermalGrid[y,x] := {192}Heating[6]; //70%
              end else begin
                if (tRGB.ColorValue = tGreenFields.ColorValue) then begin
                  ThermalGrid[y,x] := {153}Heating[5]; //60%
                end else begin
                  if (tRGB.ColorValue = tDarkFields.ColorValue) then begin
                    ThermalGrid[y,x] := {230}Heating[4]; //90%
                  end else begin
                    if (tRGB.ColorValue = tDeciduous.ColorValue) then begin
                      if (ThermalGrid[y,x] = 0) then begin // not already done
                        ThermalGrid[y,x] := {1} Heating[1];
                      end;
                    end else begin
                      if (tRGB.ColorValue = tConiferous.ColorValue) then begin
                        if (ThermalGrid[y,x] = 0) then begin // not already done
                          ThermalGrid[y,x] := {2} Heating[2];
                        end;
                      end else begin
                        if (tRGB.ColorValue = tBoth.ColorValue) then begin
                          if (ThermalGrid[y,x] = 0) then begin // not already done
                            ThermalGrid[y,x] := {2} (Heating[1] + Heating[2]) div 2;
                          end;
                        end else begin
                          if (ThermalGrid[y,x] = 0) then begin // if no forest, or no forest tile
                            ThermalGrid[y,x] := {200}Heating[8]; //assume default (green fields)
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
    Result := true; // success
  finally
    freemem(P24);
    freemem(P8);
  end;
  Close(Bitmap_File);
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
  Bitmap_File : File of byte;
  Bitmap_Hdr : BMP_V1_Header;
  Palette_8 : BMP_8bit_ColorTable;
  Color_Index_8 : byte;
  x, y : integer;
  P8  : pByteArray;
  P24  : pRGBArray;

begin
  Result := false; // assume for now
  // make sure it exists
  if (NOT FileExists(u_BMP.BMPfolder+'\'+FileName)) then begin
    exit;
  end;
  //make sure bitmap is 8 or 24 bit color and 256x256
  AssignFile(Bitmap_File,u_BMP.BMPfolder+'\'+FileName);
  Reset(Bitmap_File);
  // read the file header
  BlockRead(Bitmap_File,Bitmap_Hdr,sizeof(Bitmap_Hdr));
  // check for bitmap signature
  if (Bitmap_Hdr.bH.bSignature <> $4D42) then begin
    MessageShow('Error: not bitmap');
    Close(Bitmap_File);
    exit;
  end;
  // check for correct size
  if ((Bitmap_Hdr.bDib.bWidth <> tColumns) OR
      (Bitmap_Hdr.bDib.bHeight <> tRows) ) then begin
    MessageShow('Error: bitmap size not 256');
    Close(Bitmap_File);
    exit;
  end;
  // now check the type 8 or 24 bit color
  case (Bitmap_Hdr.bDib.bColorBits) of
    8: begin
      // read the palette for the 8 bit main file to access colors
      BlockRead(Bitmap_File,Palette_8,sizeof(BMP_8bit_ColorTable));
    end;
    24: begin
    end;
    else begin
      MessageShow('Error: Not 8 or 24 bit color bitmap');
      Close(Bitmap_File);
      Exit;
    end;
  end;

  // now go through every pixel
  try
    P8  := AllocMem(tColumns * sizeof(Byte)); // one row at a time
    P24 := AllocMem(tColumns * sizeof(TRGBTriple)); // one row at a time
    for y := tRows-1 downto 0 do begin // rows from bottom

//      ProgressBar_Status.Max := tRows;
      case (Bitmap_Hdr.bDib.bColorBits) of
        8: begin
          BlockRead(Bitmap_File,P8^,tColumns * sizeof(Byte));
        end;
        24: begin
          BlockRead(Bitmap_File,P24^,tColumns * sizeof(tRGB.cRGB));
        end;
      end;
      // check each pixel
      for x := 0 to tColumns-1 do begin // columns from left
        case (Bitmap_Hdr.bDib.bColorBits) of
          8: begin
            tRGB.crgba := Palette_8[P8^[x]];
          end;
          24: begin
            tRGB.crgb := P24^[x];
          end;
        end;
        // match color to a thermal index
        if (tRGB.ColorValue = tWater.ColorValue) then begin
          ThermalGrid[y,x] := 3 {Heating[3]};
        end else begin
          if (tRGB.ColorValue = tSwamp.ColorValue) then begin
           ThermalGrid[y,x] := 2 {Heating[2]};
          end else begin
            if (tRGB.ColorValue = tSand.ColorValue) then begin
              ThermalGrid[y,x] := 7 {Heating[7]};
            end else begin
              if (tRGB.ColorValue = tYellowFields.ColorValue) then begin
                ThermalGrid[y,x] := 6 {Heating[6]};
              end else begin
                if (tRGB.ColorValue = tGreenFields.ColorValue) then begin
                  ThermalGrid[y,x] := 5 {Heating[5]};
                end else begin
                  if (tRGB.ColorValue = tDarkFields.ColorValue) then begin
                    ThermalGrid[y,x] := 4 {Heating[4]};
                  end else begin
                    if (tRGB.ColorValue = tDeciduous.ColorValue) then begin
                      if (ThermalGrid[y,x] = 0) then begin // not already done
                        ThermalGrid[y,x] := 1 {Heating[1]};
                      end;
                    end else begin
                      if (tRGB.ColorValue = tConiferous.ColorValue) then begin
                        if (ThermalGrid[y,x] = 0) then begin // not already done
                          ThermalGrid[y,x] := 2 {Heating[2]};
                        end;
                      end else begin
                        if (tRGB.ColorValue = tBoth.ColorValue) then begin
                          if (ThermalGrid[y,x] = 0) then begin // not already done
                            ThermalGrid[y,x] := 2 {Heating[2]};
                          end;
                        end else begin
                          if (ThermalGrid[y,x] = 0) then begin // if no forest, or no forest tile
                            ThermalGrid[y,x] := 8  {Heating[8]}; //assume default
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
//        ProgressBar_Status.StepIt;
//        Application.ProcessMessages;
      end;
    end;
    Result := true; // success
  finally
    freemem(P24);
    freemem(P8);
  end;
  Close(Bitmap_File);
//  ProgressBar_Status.Position := 0;
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
//  TileName := format('%2.2d%2.2d',[i,j]);
  TileName := MakeTileName(i,j, TileNameMode);
  tFileName := TileName+'_t.bmp';
  // need to pick correct one for V1 or V2 !  ???  and correct path too !
  fFileName := TileName+'_f.bmp';
  FilePath := SourceThermalFolder +'\SourceTiles\'+TileName;
  u_BMP.BMPfolder := FilePath;

  //get forest data first
  if ForestBitmap_To_ForestGrid(fFileName, False) then begin
    ConvertForestMask(Heating[0],Heating[1],Heating[8]);
    Result := true;
  end else begin
    ClearThermalGrid;
  end;

  //then read the thermal bitmap file
  if ReadThermalBitmapTile(tFileName) then begin
    Result := true;
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
  x, y : integer;
  cOffset, rOffset : integer;

begin
  if (u_MakeThermal.Form_MakeThermal.RadioButton_SunnySlopes.Checked) then begin
    SlopeFileName := SourceThermalFolder+'\ThermalMap\SunnySlopes.bmp';
    if (NOT FileExists(SlopeFileName)) then begin
      MessageShow('SunnySlopes.bmp not found');
      Exit;
    end;
    AssignFile(SlopeBitmapFile,SlopeFileName);
    Reset(SlopeBitmapFile);

    // for partial tiles, need to crop
    cOffset := 0;
    if (((TileColumn+1) * tColumns) > ColumnCount) then begin
      cOffset := ((TileColumn+1) * tColumns) - ColumnCount;
    end;
    rOffset := 0;
    if (((TileRow+1) * tRows) > RowCount) then begin
      rOffset := ((TileRow+1) * tRows) - RowCount;
    end;

    // skip header and palette, assume correct
    FileIndex := 54+(256*4) +
      {Color8Size *} (TileRow*tRows*(ColumnCount) +
                       (ColumnCount-tColumns+cOffset - TileColumn*tColumns)
                     );
    for y := tRows-1 downto rOffset do begin // rows from bottom
      seek(SlopeBitmapFile,FileIndex);
      for x :=  cOffset to tColumns-1 do begin // columns from left
        // write each thermal index
        read(SlopeBitmapFile,FileByte);
//        ThermalValue := FileByte * ThermalGrid[y,x];
//        ThermalValue := ThermalValue{(FileByte * ThermalGrid[y,x])} div 255;
//        ThermalGrid[y,x] := ThermalValue;
        ThermalGrid[y,x] := (FileByte * ThermalGrid[y,x]) div 255;
//        ThermalGrid[y,x] := FileByte; // for testing
//        ThermalGrid[y,x] := 0; // for testing
      end;
      INC(FileIndex,ColumnCount{*Color8Size});
    end;
    close(SlopeBitmapFile);
  end;
end;

{----------------------------------------------------------------------------}
Procedure WriteThermalTile(TileColumn,TileRow: integer);
var
  x, y, k : integer;
  FileIndex : longint;
  cOffset, rOffset : integer;
  P8  : pByteArray;

begin
  // for partial tiles, need to crop
  cOffset := 0;
  if (((TileColumn+1) * tColumns) > ColumnCount) then begin
    cOffset := ((TileColumn+1) * tColumns) - ColumnCount;
  end;
  rOffset := 0;
  if (((TileRow+1) * tRows) > RowCount) then begin
    rOffset := ((TileRow+1) * tRows) - RowCount;
  end;

  FileIndex := ThermalHeader.ThermalPixelOffset +
               ThermalSize * ((TileRow * tRows * ColumnCount) +
                              (TileColumn * tColumns) );
  try
    // write each thermal index
    P8  := AllocMem(tColumns * sizeof(Byte)); // one row at a time
    for y := tRows-1 downto rOffset do begin      // rows from bottom
      seek(Thermal_File,FileIndex);
      k := 0;
      for x := tColumns-1 downto cOffset do begin // columns from right
        p8^[k]:= ThermalGrid[y,x];
        INC(k);
      end;
      BlockWrite(Thermal_File,P8^,(tColumns-cOffset) * sizeof(Byte));
      INC(FileIndex,(ColumnCount) * ThermalSize);
    end;
  finally
    freemem(P8);
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
//  tFileName : string;
//  fFileName : string;
  FilePath : string;
  FileByte : byte;
  FileByte_1 : byte;
//  TileName : string;
  ByteCount : longint;
//  P : PLongwordArray;  //??? doesn't work ???
  P : PWordArray;
//  P : PByteArray;

begin
  FilePath := DestinationThermalFolder;
  if ( (NOT FileExists(FilePath+'\'+ThermalFileName) ) OR
       (u_MakeThermal.Form_MakeThermal.CheckBox_Erase.Checked)
     ) then begin
    MessageShow('Creating Thermal map...');
    AssignFile(Thermal_File,FilePath+'\'+ThermalFileName);
    Rewrite(Thermal_File);
    // create a header
    FileByte := HI(ColumnCount); FileByte_1 := LO(ColumnCount);
    Write(Thermal_File,FileByte_1,FileByte,ZeroByte,ZeroByte);
    FileByte := HI(RowCount); FileByte_1 := LO(RowCount);
    Write(Thermal_File,FileByte_1,FileByte,ZeroByte,ZeroByte);

    // create a default file to start
    // SunnySlopes or uniform?
    if (u_MakeThermal.Form_MakeThermal.RadioButton_SunnySlopes.Checked) then begin
      SlopeFileName := SourceThermalFolder+'\ThermalMap\SunnySlopes.bmp';
      if (NOT FileExists(SlopeFileName)) then begin
        MessageShow('SunnySlopes.bmp not found');
        Exit;
      end;
      AssignFile(SlopeBitmapFile,SlopeFileName);
      Reset(SlopeBitmapFile);
      // skip header and palette, assume correct
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
      ProgressBar_Status.Position := 0;
      Close(SlopeBitmapFile);
    end else begin // uniform
      ByteCount := ColumnCount * RowCount;
      ProgressBar_Status.Max := ByteCount div (256*sizeof(word));
      try
        P := AllocMem(256*sizeof(word)); // block of 256; set to $FF
  //      for i := 0 to 256 div sizeof(longword)-1 do begin
  //        P^[i] := $FFFFFFFF; //all max thermal
        for i := 0 to 256-1 do begin
          P^[i] := $FFFF; //all max thermal
        end;
        While ByteCount > 256*sizeof(word)  do begin
          BlockWrite(Thermal_File,P^,256*sizeof(word));
          DEC(ByteCount,256*sizeof(word));
          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;
        BlockWrite(Thermal_File,P^,ByteCount);
      finally
        freemem(P);
      end;
      ProgressBar_Status.Position := 0;
    end;

    MessageShow('Thermal map created');
  end else begin
    AssignFile(Thermal_File,FilePath+'\'+ThermalFileName);
    Reset(Thermal_File);
    MessageShow('Thermal map opened');
  end;

  // update map with each tile map
  u_BMP.Memo_Message := Memo_Message;
  //for each tile look for the Thermal file and open if present or skip it if not
  ProgressBar_Status.Max := TileColumnCount*TileRowCount;
  for i := 0 to TileColumnCount-1 do begin
    for j := 0 to TileRowCount-1 do begin
//      MessageShow(format('%2.2d%2.2d',[i,j]));
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
end;

{----------------------------------------------------------------------------}
Procedure WriteThermalBitmapTile(TileColumn,TileRow: integer);
var
  x, y, k : integer;
  FileIndex : longint;
  cOffset, rOffset : integer;
  P24 : pRGBArray;

begin
  // for partial tiles, need to crop
  cOffset := 0;
  if (((TileColumn+1) * tColumns) > ColumnCount) then begin
    cOffset := ((TileColumn+1) * tColumns) - ColumnCount;
  end;
  rOffset := 0;
  if (((TileRow+1) * tRows) > RowCount) then begin
    rOffset := ((TileRow+1) * tRows) - RowCount;
  end;

  FileIndex := BitmapHeader_24bitColor.Bitmap24PixelOffset +
               Color24Size * (TileRow*tRows*ColumnCount +
                               (ColumnCount-tColumns+cOffset - TileColumn*tColumns)
                             );
  try
    // write each thermal index
    P24 := AllocMem(tColumns * Color24Size); // one row at a time
    for y := tRows-1 downto rOffset do begin      // rows from bottom
      seek(Thermal_File,FileIndex);
      k := 0;
      for x := cOffset to tColumns-1 do begin // columns from left
        p24^[k].rgbtBlue :=  ThermalGrid[y,x];
        p24^[k].rgbtGreen := ThermalGrid[y,x];
        p24^[k].rgbtRed :=   ThermalGrid[y,x];
        Inc(k);
      end;
      BlockWrite(Thermal_File,P24^,(tColumns-cOffset) * Color24Size);
      INC(FileIndex,(ColumnCount) * Color24Size);
    end;
  finally
    freemem(P24);
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
//  TileUpdate : boolean;
  Palette_8 : BMP_8bit_ColorTable;
  pColor : ColorConvert;
  P8  : pAlphaArray;
  P24 : pRGBArray;

begin
  FilePath := {SourceThermalFolder} DestinationThermalFolder;
  if ( (NOT FileExists(FilePath+'\'+ThermalFileName) ) OR
       (u_MakeThermal.Form_MakeThermal.CheckBox_Erase.Checked)
     ) then begin
    MessageShow('Creating Thermal bitmap...');
    AssignFile(Thermal_File,FilePath+'\'+ThermalFileName);
    Rewrite(Thermal_File);
    // create a header
    with xBitmapHeader_24bitColor do begin
      bDib.bWidth := ColumnCount;
      bDib.bHeight := RowCount;
      //bDib.bImageByteSize := ColumnCount*RowCount*xColor24Size div 8;
      bDib.bImageByteSize := ColumnCount*RowCount*Color24Size;
      bH.bFileByteSize := bDib.bImageByteSize+bH.bPixelArrayOffset;
    end;
    BlockWrite(Thermal_File,xBitmapHeader_24bitColor,
      sizeof(xBitmapHeader_24bitColor));

    // create a default file to start
    if (u_MakeThermal.Form_MakeThermal.RadioButton_SunnySlopes.Checked) then begin
      SlopeFileName := SourceThermalFolder+'\ThermalMap\SunnySlopes.bmp';
      if (NOT FileExists(SlopeFileName)) then begin
        MessageShow('SunnySlopes.bmp not found');
        Exit;
      end;
      AssignFile(SlopeBitmapFile,SlopeFileName);
      Reset(SlopeBitmapFile);
      seek(SlopeBitmapFile,54); // skip header, assume correct
      // read the palette for the 8 bit file to access colors
      BlockRead(SlopeBitmapFile,Palette_8,sizeof(BMP_8bit_ColorTable));

      ProgressBar_Status.Max := RowCount;
      try
          P8  := AllocMem(ColumnCount * Color8Size);  // one row at a time
          P24 := AllocMem(ColumnCount * Color24Size); // one row at a time
          for i := 0 to RowCount-1 do begin
            BlockRead(SlopeBitmapFile,P8^,ColumnCount * sizeof(Byte));
            for j := 0 to ColumnCount-1 do begin
              pColor.cRGBA := Palette_8[P8^[j]]; // read 8 bit palette entry instead ?
              P24^[j] := pColor.cRGB;
            end;
            BlockWrite(Thermal_File,P24^,ColumnCount * sizeof(TRGBTriple));
            ProgressBar_Status.StepIt;
            Application.ProcessMessages;
          end;
      finally
        freemem(P24);
        freemem(P8);
      end;
      ProgressBar_Status.Position := 0;
      Close(SlopeBitmapFile);
    end else begin
//      ByteCount := ColumnCount * RowCount;
      ByteCount := xBitmapHeader_24bitColor.bDib.bImageByteSize;
      ProgressBar_Status.Max := ByteCount div (256*sizeof(word));
      try
        P := AllocMem(256*sizeof(word)); // block of 256; set to $FF
        for i := 0 to 256-1 do begin
          P^[i] := $FFFF; //all max thermal
        end;
        While (ByteCount > 256*sizeof(word)) do begin
          BlockWrite(Thermal_File,P^,256*sizeof(word));
          DEC(ByteCount,256*sizeof(word));
          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;
        BlockWrite(Thermal_File,P^,ByteCount);
      finally
        freemem(P);
      end;
      ProgressBar_Status.Position := 0;
    end;
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
//      MessageShow(format('%2.2d%2.2d',[i,j]));
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
      //bDib.bImageByteSize := tColumns*tRows*xColor24Size div 8;
      bDib.bImageByteSize := tColumns*tRows*Color24Size;
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
//      bDib.bImageByteSize := TDM_Header.Width*TDM_Header.Height*Color8Size div 8;
      bDib.bImageByteSize := TDM_Header.Width*TDM_Header.Height*Color8Size;
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
      ProgressBar_Status.Position := 0;
    end;

    Close(TDM_File);
    Close(Greyscale_File);
    MessageShow('TDM to greyscale bitmap created');
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
      ProgressBar_Status.Position := 0;
      end;
    end;

    Close(Greyscale_File);
    Close(TDM_File);
    MessageShow('Greyscale bitmap to TDM file created');
  end;
end;

// Note: TM3 has data reversed in horizontal direction
{----------------------------------------------------------------------------}
Procedure TM3_To_Color_Bitmap(TM3_FileName,Color_FileName : string);
var
  i, j :integer;
//  FileByte : byte;
//  pColor : ColorConvert;
  TRN_File : File of byte;
  TRN_FileName : string;
  TM3_File : File of byte;
  Color_File : File of byte;
  TRN_Header : CondorTerrainHeader;
//  ByteCount : longint;
  P : PByteArray;

begin
  if (NOT FileExists(TM3_FileName)) then begin
    MessageShow('TM3 file not found');
    Exit;
  end;
  begin
    // need to find dimensions from TRN file
    TRN_FileName := copy(TM3_FileName,1,length(TM3_FileName)-4)+'.trn';
    if (NOT FileExists(TRN_FileName)) then begin
      MessageShow('TRN file not found');
      Exit;
    end;
    AssignFile(TRN_File,TRN_FileName);
    Reset(TRN_File);
    seek(TRN_File,0);
    BlockRead(TRN_File,TRN_Header,sizeof(TRN_Header));
    close(TRN_File);

    AssignFile(TM3_File,TM3_FileName);
    Reset(TM3_File);
    seek(TM3_File,0);
    MessageShow('Converting TM3 file to color bitmap...');
    AssignFile(Color_File,Color_FileName);
    Rewrite(Color_File);
    // create a BMP header
    with xBitmapHeader_24bitColor do begin
      bDib.bWidth := TRN_Header.tWidth;
      bDib.bHeight := TRN_Header.tHeight;
      //bDib.bImageByteSize := bDib.bWidth*bDib.bHeight*xColor24Size div 8;
      bDib.bImageByteSize := bDib.bWidth*bDib.bHeight*Color24Size;
      bH.bFileByteSize := bDib.bImageByteSize+bH.bPixelArrayOffset;

      BlockWrite(Color_File,xBitmapHeader_24bitColor,
        sizeof(xBitmapHeader_24bitColor));

      try
        //P := AllocMem(TRN_Header.tWidth*xColor24Size div 8); // one row at a time
        P := AllocMem(TRN_Header.tWidth*Color24Size); // one row at a time
        ProgressBar_Status.Max := TRN_Header.tHeight;
        for i := 0 to TRN_Header.tHeight-1 do begin
          //BlockRead(TM3_File,P^,TRN_Header.tWidth*xColor24Size div 8);
          BlockRead(TM3_File,P^,TRN_Header.tWidth*Color24Size);
          SwapRGBBlockEndToEnd(PRGBArray(P),TRN_Header.tWidth);
          //BlockWrite(Color_File,P^,TRN_Header.tWidth*xColor24Size div 8);
          BlockWrite(Color_File,P^,TRN_Header.tWidth*Color24Size);

          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;
      finally
        freemem(P);
      end;
      ProgressBar_Status.Position := 0;
    end;

    Close(TM3_File);
    Close(Color_File);
    MessageShow('TM3 to color bitmap created');
  end;
end;

// Note: TM3 has data reversed in horizontal direction
{----------------------------------------------------------------------------}
Procedure Color_Bitmap_To_TM3(Color_FileName, TM3_FileName : string);
//const
//  ZeroByte : byte = 0;

var
  i, j :integer;
//  FileByte : byte;
//  pColor : ColorConvert;
  TM3_File : File of byte;
  Color_File : File of byte;
  Bitmap_Hdr : BMP_V1_Header;
//  TRN_Header : CondorTerrainHeader;
//  ByteCount : longint;
  P : PByteArray;

begin
  if (NOT FileExists(Color_FileName)) then begin
    MessageShow('Color BMP file not found');
    Exit;
  end;
  // need to find dimensions from BMP file
  AssignFile(Color_File,Color_FileName);
  Reset(Color_File);
//  seek(Color_File,0);
  BlockRead(Color_File,Bitmap_Hdr,sizeof(Bitmap_Hdr));
//    if (NOT ValidateNotCompressed(Bitmap_Hdr)) then begin
//      MessageShow('Error: Compression not allowed');
//    end;
  with Bitmap_Hdr do begin
    // first confirm it is bmp and 24 bit color
    if ((bH.bSignature <> $4D42) OR              // BitmapSignature
        (bDib.bColorBits <> 24)) then begin // 24 bit color
      MessageShow('Error: Not 24 bit color bitmap');
    end else begin
      MessageShow('Converting color bitmap to TM3 file...');
      AssignFile(TM3_File,TM3_FileName);
      Rewrite(TM3_File);
      try
        //P := AllocMem(bDib.bWidth*xColor24Size div 8); // one row at a time
        P := AllocMem(bDib.bWidth*Color24Size); // one row at a time
        ProgressBar_Status.Max := bDib.bHeight;
        for i := 0 to bDib.bHeight-1 do begin
          //BlockRead(Color_File,P^,bDib.bWidth*xColor24Size div 8);
          BlockRead(Color_File,P^,bDib.bWidth*Color24Size);
          SwapRGBBlockEndToEnd(PRGBArray(P),bDib.bWidth);
          //BlockWrite(TM3_File,P^,bDib.bWidth*xColor24Size div 8);
          BlockWrite(TM3_File,P^,bDib.bWidth*Color24Size);

          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;
      finally
        freemem(P);
      end;
      Close(TM3_File);
      MessageShow('Color bitmap to TM3 created');
      ProgressBar_Status.Position := 0;
    end;

    Close(Color_File);
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
Procedure MakeThermal_Blank(TDM_FileName : string; tWidth, tHeight : integer; Default_Value : integer);
var
  TDM_File : File of byte;
  P : PByteArray;
  ByteCount : longint;

begin
  AssignFile(TDM_File,TDM_FileName);
  rewrite(TDM_File);

 try
  with TDM_Header do begin
    Width := tWidth;
    Height := tHeight;
    // write the header
    BlockWrite(TDM_File,TDM_Header,sizeof(TDM_Header));

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

{----------------------------------------------------------------------------}
Procedure ForceTM3size(TM_FileName : string; Default_Value : integer);
var
  TM_File : File of byte;
  P : PByteArray;
  ByteCount : longint;
begin
  AssignFile(TM_File,TM_FileName);
  if (NOT FileExists(TM_FileName)) then begin
    MessageShow('Thermal tm3 file not found');
    beep; Exit;
  end;
  reset(TM_File);
  // read dummy header
  BlockRead(TM_File,TDM_Header,sizeof(TDM_Header));
  // got back to beginning
  seek(TM_File,0);
 try
  with TDM_Header do begin
    P := AllocMem(512); // initial values
    for ByteCount := 0 to 512-1 do begin
      P^[ByteCount] := Default_Value;
    end;

    ByteCount := Width*Height * 3;
    while (ByteCount > 0) do begin
      if (ByteCount > 512) then begin
        BlockWrite(TM_File,P^,512);
      end else begin
        BlockWrite(TM_File,P^,ByteCount);
      end;
      DEC(ByteCount,512);
    end;
  end;

 finally
   freemem(P);
 end;

  Close(TM_File);
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
    try
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
    finally
      Freemem(P);
    end;
    ProgressBar_Status.Position := 0;
  end;
  CloseFile(TDM_File_a);
  CloseFile(TDM_File);
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

// TM3 same as TDM but 3 bytes instead of one, and no header
// could be combined with Merge_TDM_File with a parameter - TDM/TM3
// could be combined with Merge_BMP24_File with a parameter - TDM/TM3/BMP24/BMP32
{----------------------------------------------------------------------------}
procedure Merge_TM3_File(Offset_X, Offset_Y, Min_X, Max_X, Min_Y, Max_Y : LongInt;
                          FilePath,Filename,
                          FilePath_a,Filename_a : string);
const
  value_Size = 3;
var
  TM_File : File of Byte;
  TM_File_a : File of Byte;
  P : PByteArray;
  i, i_Min, i_Max : integer;
  FileIndex : LongInt;
  cWidth, cHeight : integer;
  gWidth, gHeight : integer;
  TRN_Header : CondorTerrainHeader;
  j_DeltaL, j_DeltaR, j_Index, j_Width : integer;

begin
  if (NOT FileExists(FilePath+'\'+Filename)) then begin
    MessageShow('TM3 file not found');
    Beep; Exit;
  end;
  // need to read size from TRN file first
  AssignFile(TM_File,FilePath+'\'+copy(FileName,1,pos('.tm3',FileName)-1)+'.trn');
  Reset(TM_File);
  BlockRead(TM_File,TRN_Header,sizeof(TRN_Header));
  // keep width and height
  cWidth  := TRN_Header.tWidth;
  cHeight := TRN_Header.tHeight;
  CloseFile(TM_File);
  // now open the TM3 file
  AssignFile(TM_File,FilePath+'\'+Filename);
  Reset(TM_File);

  if (NOT FileExists(FilePath_a+'\'+Filename_a)) then begin
//    MessageShow('Warning: '+Filename_a+' file not found');
    Beep;
  end else begin
    // need to read size from TRN file first
    AssignFile(TM_File_a,FilePath_a+'\'+copy(FileName_a,1,pos('.tm3',FileName_a)-1)+'.trn');
    Reset(TM_File_a);
    BlockRead(TM_File_a,TRN_Header,sizeof(TRN_Header));
    // keep width and height
    gWidth  := TRN_Header.tWidth;
    gHeight := TRN_Header.tHeight;
    CloseFile(TM_File_a);
    // now open the TM3 file
    AssignFile(TM_File_a,FilePath_a+'\'+Filename_a);
    Reset(TM_File_a);

    try
      // need a buffer
      P := AllocMem(gWidth * value_Size);
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
        FileIndex := 0 +
          ((i) * gWidth) * value_Size;
        seek(TM_File_a,FileIndex);
        BlockRead(TM_File_a,P^,gWidth * value_Size);
        // write output data
        FileIndex := 0 +
          ((i + Offset_Y - Min_Y) * cWidth) * value_Size +
        // need to reverse order for TM3
//          (j_Index) * value_Size;
          (cWidth - j_Width - j_Index) * value_Size;
        seek(TM_File,FileIndex);
        // use delta right offset because data in in reverse order for TDM
        BlockWrite(TM_File,P^[j_DeltaR * value_Size],j_Width * value_Size);
        ProgressBar_Status.StepIt;
        Application.ProcessMessages;
      end;
    finally
      Freemem(P);
      CloseFile(TM_File_a);
      ProgressBar_Status.Position := 0;
    end;
  end;
  CloseFile(TM_File);
end;

{
TM3 is like an RGB bitmap. One of the colour layers is the Condor2 thermal map.
The other 2 channels are Alpiness (measure of how mountenous the terrain is)
and Convexity (measures of how convex/concave the terrain is.
}

//---------------------------------------------------------------------------
procedure MakeDummyTDM_TM3(LandscapePath, LandscapeName : string);
const
  Convexity : byte = 127; // signed(?) byte  127: flat, 0: very convex, 255: very concave
  Alpiness : byte = 0;  // unsigned byte, 0: flat, 255: very hilly

var
  i, j :integer;
//  FileByte : byte;
  pColor : ColorConvert;
  TDM_File : File of byte;
  TM3_File : File of byte;
  TDM_Header : TDM_Header_Type;
//  ByteCount : longint;
  P, P3 : PByteArray;
  TDM_Filename : string;
  TM3_Filename : string;

begin
  // use the TDM and
  // make the values an even colour for Convexity and Alpiness
  TDM_Filename := LandscapePath+'\'+LandscapeName+'.tdm';
  if (NOT FileExists(TDM_FileName)) then begin
    MessageShow('TDM file not found');
    Exit;
  end;
  begin
    AssignFile(TDM_File,TDM_FileName);
    Reset(TDM_File);
    seek(TDM_File,0);
    BlockRead(TDM_File,TDM_Header,sizeof(TDM_Header));
    MessageShow('Converting TDM file to partial TM3...');
    TM3_Filename := LandscapePath+'\'+LandscapeName+'.tm3';
    AssignFile(TM3_File,TM3_FileName);
    Rewrite(TM3_File);

      try
        P := AllocMem(TDM_Header.Width); // one row at a time
        P3 := AllocMem(TDM_Header.Width *3);
        ProgressBar_Status.Max := TDM_Header.Height;
        for i := 0 to TDM_Header.Height-1 do begin
          BlockRead(TDM_File,P^,TDM_Header.Width);
          for j := 0 to TDM_Header.Width do begin
            P3^[j*3]   := P^[j];      // BLUE
            P3^[j*3+1] := Alpiness;   // GREEN   could be reversed
            P3^[j*3+2] := Convexity;  // RED     could be reversed
          end;
          BlockWrite(TM3_File,P3^,TDM_Header.Width*3);

          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;
      finally
        freemem(P3);
        freemem(P);
      end;
      ProgressBar_Status.Position := 0;

    Close(TM3_File);
    Close(TDM_File);
  end;
end;

//---------------------------------------------------------------------------
procedure MakeDummyBlank_TM3(Default_Value, Column, Row : integer; FilePath, Filename:string);
var
  TM_File : File of byte;
  P : PByteArray;
  ByteCount : longint;

begin
  AssignFile(TM_File,FilePath+'\'+Filename);
  rewrite(TM_File);

 try
    P := AllocMem(512); // initial values
    for ByteCount := 0 to 512-1 do begin
      P^[ByteCount] := Default_Value;
    end;

    ByteCount := Column * Row * 3;
    while (ByteCount > 0) do begin
      if (ByteCount > 512) then begin
        BlockWrite(TM_File,P^,512);
      end else begin
        BlockWrite(TM_File,P^,ByteCount);
      end;
      DEC(ByteCount,512);
    end;

 finally
   freemem(P);
 end;

  Close(TM_File);
end;

{----------------------------------------------------------------------------}
begin { Initialization }
  Memo_Message := nil;
end.

{--- End of File ------------------------------------------------------------}

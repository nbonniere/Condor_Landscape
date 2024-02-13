{
 * u_Forest.pas
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

{ Note: default bitmap color table could possibly be reversed to avoid initializing
to $ff instead of $00, but Condor Landscape Editor might not like it.
Testing is needed before changing it! It depends if it uses the actual
color (green) or the color index (0,1).
}
//----------------------------------------------------------------------------
UNIT u_Forest;

{----------------------------------------------------------------------------
The condor .FOR file (V1) is structured as follows:
- n bytes -> n = 256*columns * 256*rows
  - each byte value
    - corresponds to four 45x45 metre squares
    - high-nibble contains 4 bits for coniferous forest
    - low-nibble contains 4 bits for deciduous forest
    - each nibble value
      - ???
  - byte order in file
    - by column
    - right column first
    - starting from the bottom
  - 4 quadrant interleave - messy !  
----------------------------------------------------------------------------}

{----------------------------------------------------------------------------
Implementation is based on reading tile size bitmaps and converting to
Condor Forest map format.
If a Forest Map already exists, it is opened and data is added to it, else it
is first created.
Each tile is loaded and the the data written into the forest map.
If a tile doesn't exist, the data stays blank but the file is still created.
----------------------------------------------------------------------------}

{============================================================================}
INTERFACE

uses StdCtrls, comctrls,
  u_SceneryHDR;

Type
  ForestType = (fDeciduous,fConiferous);

const
  ForestSize = 1;         // size of value in file in bytes
//  fRows = tRows*2;        //twice resolution
//  fColumns = tColumns*2;  //twice resolution

var
  Memo_Message : TMemo;  // external TMemo for messages
  ProgressBar_Status : TProgressBar;
  SourceForestFolder : string; // external path for file
  DestinationForestFolder : string; // external path for file
  ForestResolution : integer = 2;   // relative resolution with reference to tRows
//  ForestGrid : array[0..fRows-1,0..fColumns-1] of byte;
  ForestGrid : array of array of byte;

  OverallForestDeciduous : array of byte;
  OverallForestConiferous : array of byte;

Procedure CreateForestMap(Shrink : Boolean; ForestFileName : string);
Procedure CreateForestBitmap(Shrink : Boolean; Forest : ForestType; ForestFileName : string);
Procedure CreateForestTileBitmap(ForestFileName : string);
//function ReadForestBitmapTile(FileName:string) : boolean;
function ReadForestBitmapTile(FileName:string; Combine : Boolean) : Boolean;

Procedure FOR_To_OverallForest(ForestFileName : string);
Procedure Byte_To_Greyscale_Bitmap(var Data : array of byte; Greyscale_FileName : string);
Procedure OverallForest_To_FOR(ForestFileName : string);
Procedure Expand_x4_Save_V2(FOR_FilePath : string);

// first step for V2 forest FOR to BMP
Procedure V2_FOR_To_ForestGrid(ForestFileName : string);
Procedure V2_2Color_WriteBitmapForestTile(Forest : ForestType;ForestFIleName : string);
Procedure V2_16Color_Both_WriteBitmapForestTile(ForestFIleName : string);
Procedure V2_GreyScale_WriteBitmapForestTile(Forest : ForestType; ForestFileName : string);
Procedure V2_24bit_WriteBitmapForestTile(Forest : ForestType; ForestFileName : string);

Procedure V2_ForestGrid_To_FOR(ForestFileName : string);

// not/no-longer supported by bitmap
Procedure V2_4Color_Both_WriteBitmapForestTile(ForestFIleName : string);

{============================================================================}
IMPLEMENTATION

uses forms, SysUtils, Windows,
  {u_SceneryHDR,} u_TileList, u_BMP, u_Convolve;

var
  Forest_File : File of byte;

{----------------------------------------------------------------------------}
Procedure MessageShow(Info : string);
begin
  if (Memo_Message <> nil) then begin
    Memo_Message.lines.add(Info);
  end;
end;

{----------------------------------------------------------------------------
bitmap format
- by row
- bottom row first
- starting from the left
----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
function ReadForestBitmapTile(FileName:string; Combine : Boolean) : Boolean;
// create an array of bytes 512x512 (V1) or 2048x2048 (V2)
// read each bitmap RGB and convert to forest presence
const
  tRGB : ColorConvert = ( ColorValue: $00000000 );

var
  BitmapFile : File of byte;
  i, j : integer;
  P24  : pRGBArray;
  CurrentValue : byte;

begin
  //make sure bitmap is 24 bit color and 512x512 (V1) or 2048x2048 (V2)
  Bitmap_GetWidthHeight(FileName);
  if NOT ((BitmapWidth = tColumns*ForestResolution) AND (BitmapHeight = tRows*ForestResolution)) then begin
    MessageShow('Bitmap file - wrong size');
    ReadForestBitmapTile := false;
//    Beep;
  end else begin
    try
      P24 := AllocMem(tColumns*ForestResolution * 3); // one row at a time
      AssignFile(BitmapFile,u_BMP.BMPfolder+'\'+FileName);
      Reset(BitmapFile);
      seek(BitmapFile,BitmapHeader_24bitColor.Bitmap24PixelOffset);
      ProgressBar_Status.Max := tRows*ForestResolution;
      for i := tRows*ForestResolution-1 downto 0 do begin // rows from bottom
        BlockRead(BitmapFile,P24^,tColumns*ForestResolution * 3);
        for j := 0 to tColumns*ForestResolution-1 do begin // columns from left
           tRGB.cRGB := P24^[j]; // each pixel
           if (Combine) then begin
             CurrentValue := ForestGrid[i,j];
           end else begin
             CurrentValue := 0;
           end;
          // match color to a forest index
          if (tRGB.ColorValue = tDeciduous.ColorValue) then begin
            ForestGrid[i,j] := CurrentValue OR 1;
          end else begin
            if (tRGB.ColorValue = tConiferous.ColorValue) then begin
              ForestGrid[i,j] := CurrentValue OR 2;
            end else begin
              if (tRGB.ColorValue = tBoth.ColorValue) then begin
                ForestGrid[i,j] := CurrentValue OR 3;
              end else begin
                ForestGrid[i,j] := CurrentValue OR 0;
              end;
            end;
          end;
        end;
        ProgressBar_Status.StepIt;
        Application.ProcessMessages;
      end;
    finally
      freemem(P24);
    end;
    Close(BitmapFile);
    ReadForestBitmapTile := true;
    ProgressBar_Status.Position := 0;
  end;
end;

{----------------------------------------------------------------------------}
Procedure BitInterleave(var FileByte : byte;
                        InterleaveMask : byte;
                        Forest : byte);
var
  BitMask : byte;

begin
    BitMask := 2; // fConiferous
    if (Forest AND BitMask = BitMask) then begin
      FileByte := FileByte OR (InterleaveMask AND $F0); // high Nibble
    end else begin
      FileByte := FileByte AND NOT (InterleaveMask AND $F0); // high Nibble
    end;

    BitMask := 1; // fDeciduous
    if (Forest AND BitMask = BitMask) then begin
      FileByte := FileByte OR (InterleaveMask AND $0F); // Low Nibble
    end else begin
      FileByte := FileByte AND NOT (InterleaveMask AND $0F); // Low Nibble
    end;
end;

{----------------------------------------------------------------------------
.FOR format (V1)
- interleave four quadrants into 4 bits
- by column
- right column first
- starting from the bottom
----------------------------------------------------------------------------}

{----------------------------------------------------------------------------}
Procedure WriteForestTile(TileColumn,TileRow: integer);
var
  i,j : integer;
  FileIndex : longint;
  FileByte : byte;
  ColumnInterleave : integer;
  RowInterleave : integer;
  InterleaveMask : byte;
  TempMask : byte;

begin
  // 4 bit interleaving - each nibble contains 4 quadrant forest bits
  InterleaveMask := $88; // high nibble for coniferous, low nibble for deciduous
  // column interleaving
  ColumnInterleave := TileColumn * tColumns*ForestResolution;
//  // row interleaving
//  RowInterleave := TileRow * fRows;
  for i := tColumns*ForestResolution-1 downto 0 do begin // columns from right
    if (ColumnInterleave >= TileColumnCount*tColumns*ForestResolution div 2) then begin
      ColumnInterleave := ColumnInterleave - TileColumnCount*tColumns*ForestResolution div 2;
      InterleaveMask := InterleaveMask SHR 1;
    end;
    TempMask := InterleaveMask;
    // row interleaving
    RowInterleave := TileRow * tRows*ForestResolution;
    for j := tRows*ForestResolution-1 downto 0 do begin // rows from bottom
      if (RowInterleave >= TileRowCount*tRows*ForestResolution div 2) then begin
        RowInterleave := RowInterleave - TileRowCount*tRows*ForestResolution div 2;
        TempMask := TempMask SHR 2;
      end;
      FileIndex := ForestSize * (ColumnInterleave * (TileRowCount*tRows*ForestResolution div 2) +
                   RowInterleave);
      seek(Forest_File,FileIndex);
      read(Forest_File,FileByte); // needed because bits are interleaved
      seek(Forest_File,FileIndex); // needed to overwrite the byte
      BitInterleave(FileByte,TempMask,ForestGrid[j,i]);
      // write each forest presence, 2 bits; for deciduous, coniferous
      write(Forest_File,FileByte);
      INC(RowInterleave,1);
    end;
    INC(ColumnInterleave,1);
  end;
end;

{----------------------------------------------------------------------------}
Procedure CreateForestMap(Shrink : Boolean; ForestFileName : string);
//const
//  ZeroByte : byte = 0;

var
//  TileIndex : Integer;
  i, j :integer;
  FileName : string;
  FilePath : string;
//  FileByte : byte;
  TileName : string;
//  BitmapFilePath : string;
  ByteCount : longint;
  P : PByteArray;

begin
  FilePath := DestinationForestFolder;
  if (NOT FileExists(FilePath+'\'+ForestFileName)) then begin
    MessageShow('Creating Forest map...');
    AssignFile(Forest_File,FilePath+'\'+ForestFileName);
    Rewrite(Forest_File);
    // create a blank file for now
{    ProgressBar_Status.Max := (ColumnCount);
    for i := ColumnCount-1 downto 0 do begin
      for j := RowCount-1 downto 0 do begin
        Write(Forest_File,ZeroByte); // no forest
//        FileByte := $FF; Write(Forest_File,FileByte); // full forest
      end;
      ProgressBar_Status.StepIt;
      Application.ProcessMessages;
    end;
}
    ByteCount := ColumnCount * RowCount;
    ProgressBar_Status.Max := ByteCount div 256;
    try
      P := AllocMem(256); // block of 256 bytes; bytes are set to 0
      While ByteCount > 256  do begin
        BlockWrite(Forest_File,P^,256);
        DEC(ByteCount,256);
        ProgressBar_Status.StepIt;
        Application.ProcessMessages;
      end;
      BlockWrite(Forest_File,P^,ByteCount);
    finally
      freemem(P);
    end;

    MessageShow('Forest map created');
    ProgressBar_Status.Position := 0;
  end else begin
    AssignFile(Forest_File,FilePath+'\'+ForestFileName);
    Reset(Forest_File);
    MessageShow('Forest map opened');
  end;

  u_BMP.Memo_Message := Memo_Message;
  //for each tile look for the forest file and open if present or skip it if not
  ProgressBar_Status.Max := (TileRowCount)*(TileColumnCount);
  for i := 0 to TileColumnCount-1 do begin
    for j := 0 to TileRowCount-1 do begin
      TileName := format('%2.2d%2.2d',[i,j]);

      //first check if geoDatabase V1 format file is present
      FileName := 'f'+TileName+'.bmp';
//      FilePath := SourceForestFolder +'\Terragen\ForestMaps';
      FilePath := SourceForestFolder +'\SourceTiles\'+TileName;
      if (FileExists(FilePath+'\'+FileName)) then begin
        //read the bitmap file
        u_BMP.BMPfolder := FilePath;
        if (Shrink) then begin
          u_Convolve.ProgressBar_Status := ProgressBar_Status;
          ForestBitmapShrink_File(FilePath+'\'+FileName, FilePath+'\'+'sf'+TileName+'.bmp');
          FileName := 'sf'+TileName+'.bmp';
        end;
        if ReadForestBitmapTile(FileName, False) then begin
          //write to Forest file
          WriteForestTile(i,j);
        end;
      end else begin // look for original format file
        FileName := TileName+'_f.bmp';
        FilePath := SourceForestFolder +'\SourceTiles\'+TileName;
        if (FileExists(FilePath+'\'+FileName)) then begin
          //read the bitmap file
          u_BMP.BMPfolder := FilePath;
          if (Shrink) then begin
            u_Convolve.ProgressBar_Status := ProgressBar_Status;
            ForestBitmapShrink_File(FilePath+'\'+FileName, FilePath+'\'+TileName+'_f_s.bmp');
            FileName := TileName+'_f_s.bmp';
          end;
          if ReadForestBitmapTile(FileName, False) then begin
            //write to Forest file
            WriteForestTile(i,j);
          end;
        end;
      end;

      ProgressBar_Status.StepIt;
      Application.ProcessMessages;
    end;
  end;

  Close(Forest_File);
  MessageShow('Forest map done.');
  ProgressBar_Status.Position := 0;
end;

{----------------------------------------------------------------------------}
Function ByteToBit(Forest:ForestType;Grid:Array of byte; j:integer):byte;
var
  FileByte : byte;
  k : integer;
  BitMask : byte;

begin
  case Forest of
    fDeciduous: BitMask := 1;
    fConiferous: BitMask := 2;
    else Bitmask := 0;
  end;
  FileByte := 0;
  for k := 0 to 8-1 do begin
    if (Grid[j+k] AND BitMask = BitMask) then begin
      FileByte := FileByte SHL 1 +1;
    end else begin
      FileByte := FileByte SHL 1;
    end;
  end;
  ByteToBit := FileByte XOR $FF;  // why ?  -> color table index is reversed - change ?
end;

{----------------------------------------------------------------------------}
Function ByteTo2bits(Grid:Array of byte; j:integer):byte;
var
  FileByte : byte;
  k : integer;
  BitMask : byte;

begin
  FileByte := 0;
  BitMask := 3;
  for k := 0 to 4-1 do begin
    FileByte := FileByte SHL 2;
    FileByte := FileByte OR (Grid[j+k] AND BitMask);
  end;
  ByteTo2bits := FileByte;
end;

{----------------------------------------------------------------------------}
Function ByteTo4bits(Grid:Array of byte; j:integer):byte;
var
  FileByte : byte;
  k : integer;
  BitMask : byte;

begin
  FileByte := 0;
  BitMask := 3;
  for k := 0 to 2-1 do begin
    FileByte := FileByte SHL 4;
    FileByte := FileByte OR (Grid[j+k] AND BitMask);
  end;
  ByteTo4bits := FileByte;
end;

{----------------------------------------------------------------------------}
Function ByteToGreyScale(Forest:ForestType;Grid: byte):byte;
var
//  FileByte : byte;
//  k : integer;
  BitMask : byte;

begin
  case Forest of
    fDeciduous: BitMask := 1;
    fConiferous: BitMask := 2;
    else BitMask := 0;
  end;

  if (Grid AND BitMask = BitMask) then begin
    ByteToGreyScale := 255; // white
  end else begin
    ByteToGreyScale := 0; // black
  end;
end;

{----------------------------------------------------------------------------}
//Function ByteToColor(Forest:ForestType;Grid: byte) : ColorConvert;
Function ByteToColor(Forest:ForestType;Grid: byte) : TRGBTriple;
var
//  FileByte : byte;
//  k : integer;
  BitMask : byte;
  pColor : ColorConvert;

begin
  case Forest of
    fDeciduous: begin
      BitMask := 1;
//      pColor.ColorValue := $0000FF00;  // Alpha, R, G, B
      pColor.ColorValue := tDeciduous.ColorValue;
    end;
    fConiferous: begin
      BitMask := 2;
//      pColor.ColorValue := $00008000;  // Alpha, R, G, B
      pColor.ColorValue := tConiferous_LE.ColorValue;
    end;
    else begin
      BitMask := 0;
    end;
  end;

  if (Grid AND BitMask = BitMask) then begin
    ByteToColor := pColor.cRGB; // color
  end else begin
//    ByteToColor.ColorValue := $00000000; // black
    ByteToColor := tNone.cRGB; // black
  end;
end;

{----------------------------------------------------------------------------}
Procedure WriteBitmapForestTile(Forest:ForestType;TileColumn,TileRow: integer);
var
  i,j : integer;
  FileIndex : longint;
  FileByte : byte;

begin
//  FileIndex := BitmapHeader_1bitBW.BitmapBWPixelOffset +
  FileIndex := xBitmapHeader_1bitColor.bH.bPixelArrayOffset +
               xColor1Size * (TileRow*tRows*ForestResolution*((TileColumnCount*tColumns*ForestResolution) div 8) +
                              (((TileColumnCount-1)-TileColumn)*tColumns*ForestResolution div 8)
                             );
  for i := tRows*ForestResolution-1 downto 0 do begin // rows from bottom
    seek(Forest_File,FileIndex);
    for j := 0 to (tColumns*ForestResolution div 8)-1 do begin // columns from left
      //assemble 8 bits
      FileByte := ByteToBit(Forest,ForestGrid[i],j*8);
      // write each forest index, 2 color (BW)
      write(Forest_File,FileByte);
    end;
    INC(FileIndex,(xColor1Size * (TileColumnCount*tColumns*ForestResolution) div 8));
  end;
end;

{----------------------------------------------------------------------------}
Procedure CreateForestBitmap(Shrink : Boolean; Forest : ForestType; ForestFileName : string);
const
  ZeroByte : byte = 0;

var
//  TileIndex : Integer;
  i, j :integer;
  FileName : string;
  FilePath : string;
//  FileByte : byte;
  TileName : string;
//  BitmapFilePath : string;
  P : PWordArray;

begin
  FilePath := DestinationForestFolder;
  if (NOT FileExists(FilePath+'\'+ForestFileName)) then begin
    MessageShow('Creating Forest bitmap...');
    AssignFile(Forest_File,FilePath+'\'+ForestFileName);
    Rewrite(Forest_File);
    // create a header
    with xBitmapHeader_1bitColor do begin
      bDib.bWidth := ColumnCount*ForestResolution;
      bDib.bHeight := RowCount*ForestResolution;
      bDib.bImageByteSize := bDib.bWidth*bDib.bHeight*xColor1Size div 8;
      bH.bFileByteSize := bDib.bImageByteSize+bH.bPixelArrayOffset;
    end;
    BlockWrite(Forest_File,xBitmapHeader_1bitColor,
      sizeof(xBitmapHeader_1bitColor));

    //write 2 color palette
    BlockWrite(Forest_File,xBMP_1bit_ForestColorTable,
      sizeof(xBMP_1bit_ForestColorTable));

    // create a default file for now
    try
      P := AllocMem(ColumnCount*ForestResolution*xColor1Size div 8); // one row; bytes are set to 0
//      for i := 0 to (ColumnCount*2*xColor1Size div 8) div sizeof(word)-1 do begin
//        P^[i] := $FFFF; //all no forest
//      end;
      FillChar(P^[0], ColumnCount*ForestResolution*xColor1Size div 8, $FF);
      for i := 0 to RowCount*ForestResolution-1 do begin       // tRows to do
        BlockWrite(Forest_File,P^,ColumnCount*ForestResolution*xColor1Size div 8);
      end;
    finally
      freemem(P);
    end;

    MessageShow('Forest bitmap created');
  end else begin
    AssignFile(Forest_File,FilePath+'\'+ForestFileName);
    Reset(Forest_File);
    MessageShow('Forest bitmap opened');
  end;

  u_BMP.Memo_Message := Memo_Message;
  //for each tile look for the forest file and open if present or skip it if not
  ProgressBar_Status.Max := (TileRowCount)*(TileColumnCount);
  for i := 0 to TileColumnCount-1 do begin
    for j := 0 to TileRowCount-1 do begin
      TileName := format('%2.2d%2.2d',[i,j]);

      //first check if geoDatabase V1 format file is present
      FileName := 'f'+TileName+'.bmp';
//      FilePath := SourceForestFolder +'\Terragen\ForestMaps';
      FilePath := SourceForestFolder +'\SourceTiles\'+TileName;
      if (FileExists(FilePath+'\'+FileName)) then begin
        //read the bitmap file
        u_BMP.BMPfolder := FilePath;
        if (Shrink) then begin
          u_Convolve.ProgressBar_Status := ProgressBar_Status;
          ForestBitmapShrink_File(FilePath+'\'+FileName, FilePath+'\'+'sf'+TileName+'.bmp');
          FileName := 'sf'+TileName+'.bmp';
        end;
        if ReadForestBitmapTile(FileName, False) then begin
          //write to Forest file
          WriteBitmapForestTile(Forest,i,j);
        end;
      end else begin // look for original format file
        FileName := TileName+'_f.bmp';
        FilePath := SourceForestFolder +'\SourceTiles\'+TileName;
        if (FileExists(FilePath+'\'+FileName)) then begin
          //read the bitmap file
          u_BMP.BMPfolder := FilePath;
          if (Shrink) then begin
            u_Convolve.ProgressBar_Status := ProgressBar_Status;
            ForestBitmapShrink_File(FilePath+'\'+FileName, FilePath+'\'+TileName+'_f_s.bmp');
            FileName := TileName+'_f_s.bmp';
          end;
          if ReadForestBitmapTile(FileName, False) then begin
            //write to Forest file
            WriteBitmapForestTile(Forest,i,j);
          end;
        end;
      end;

      ProgressBar_Status.StepIt;
      Application.ProcessMessages;
    end;
  end;

  Close(Forest_File);
  MessageShow('Forest map done.');
  ProgressBar_Status.Position := 0;
end;

{----------------------------------------------------------------------------}
Procedure CreateForestTileBitmap(ForestFileName : string);
var
  i : integer;
  BitmapFile : File of byte;
  P : PByteArray;

begin
    MessageShow('Creating forest bitmap');
    AssignFile(BitmapFile,ForestFileName);
    Rewrite(BitmapFile);
    // create a header
    with xBitmapHeader_24bitColor do begin
      bDib.bWidth := tColumns*ForestResolution;
      bDib.bHeight := tRows*ForestResolution;
      bDib.bImageByteSize := bDib.bWidth*bDib.bHeight*xColor24Size div 8;
      bH.bFileByteSize := bDib.bImageByteSize+bH.bPixelArrayOffset;
    end;
    BlockWrite(BitmapFile,xBitmapHeader_24bitColor,
      sizeof(xBitmapHeader_24bitColor));
    try
      P := AllocMem(tColumns*ForestResolution*Color24Size); // one row; bytes are set to 0
      for i := 0 to tRows*ForestResolution-1 do begin       // fRows to do
        BlockWrite(BitmapFile,P^,tColumns*ForestResolution*Color24Size);
      end;
    finally
      freemem(P);
    end;

    Close(BitmapFile);
    MessageShow('Forest tile bitmap created');
end;

{----------------------------------------------------------------------------}
function bitToByte(flag : byte) : byte;
begin
  if (flag <> 0) then begin
    result := 255;
  end else begin
    result := 0;
  end;
end;

// Note: flip vertically for bitmap format
{----------------------------------------------------------------------------}
Procedure Byte_To_Greyscale_Bitmap(var Data : array of byte; Greyscale_FileName : string);
const
  ZeroByte : byte = 0;

var
  i :integer;
//  FileByte : byte;
  pColor : ColorConvert;
//  TDM_File : File of byte;
  Greyscale_File : File of byte;
//  TDM_Header : TDM_Header_Type;
//  ByteCount : longint;
  P : PByteArray;

begin
    AssignFile(Greyscale_File,Greyscale_FileName);
    Rewrite(Greyscale_File);
    with BitmapHeader_8bitColor do begin
      bDib.bPaletteColors := 256; // 8 bit greyscale
      bDib.bWidth := ColumnCount*2;
      bDib.bHeight := RowCount*2;
      bDib.bImageByteSize := bDib.bWidth*bDib.bHeight*Color8Size div 8;
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
//        P := AllocMem(bDib.bWidth); // one row at a time
    //    ProgressBar_Status.Max := bDib.bHeight;
        for i := 0 to bDib.bHeight-1 do begin
//          BlockRead(TDM_File,P^,TDM_Header.Width);
    //  P := @Data[i*bDib.bWidth];  // no flip
      P := @Data[bDib.bWidth*(bDib.bHeight-1) - i*bDib.bWidth];  // flip vertically
//          SwapBlockEndToEnd(PByteArray(P),TDM_Header.Width);
          BlockWrite(Greyscale_File,P^,bDib.bWidth);
    //      ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;
      finally
//        freemem(P);
      end;

    end;

    Close(Greyscale_File);
    MessageShow('Greyscale bitmap created');
  //  ProgressBar_Status.Position := 0;

end;

// OverallForest has one pixel of forest per byte
// would be 8x smaller with one per bit
// interleaved in 4 quadrants
{----------------------------------------------------------------------------}
Procedure FOR_To_OverallForest(ForestFileName : string);
var
  x, y :integer;
  P : PByteArray;
  Index0, Index1, Index2, Index3: integer;

begin
  if (NOT FileExists(ForestFileName)) then begin
    MessageShow('Forest file not found');
  end else begin
    AssignFile(Forest_File,ForestFileName);
    Reset(Forest_File);
      try
        P := AllocMem(RowCount*ForestSize); // one row of FOR at a time (one column of OverallForest)
        ProgressBar_Status.Max := ColumnCount;
        // forest is columnCount*2 x rowCount*2
        for x := 0 to (ColumnCount)-1 do begin
          BlockRead(Forest_File,P^,RowCount*ForestSize);
          for y := 0 to (RowCount)-1 do begin
            // x*Height + y, but reversed x and y
            // 4 quadrant pointers
            Index0 := ((RowCount*1)-1-y)*(ColumnCount*2) + (ColumnCount*1-1-x);
            Index1 := ((RowCount*1)-1-y)*(ColumnCount*2) + (ColumnCount*2-1-x);
            Index2 := ((RowCount*2)-1-y)*(ColumnCount*2) + (ColumnCount*1-1-x);
            Index3 := ((RowCount*2)-1-y)*(ColumnCount*2) + (ColumnCount*2-1-x);
            // assume 0 or 255 so any bit can be used as flag
            OverallForestDeciduous[Index0] := bitToByte(P^[y] AND 1);
            OverallForestDeciduous[Index1] := bitToByte(P^[y] AND 2);
            OverallForestDeciduous[Index2] := bitToByte(P^[y] AND 4);
            OverallForestDeciduous[Index3] := bitToByte(P^[y] AND 8);
            OverallForestConiferous[Index0] := bitToByte(P^[y] AND 16);
            OverallForestConiferous[Index1] := bitToByte(P^[y] AND 32);
            OverallForestConiferous[Index2] := bitToByte(P^[y] AND 64);
            OverallForestConiferous[Index3] := bitToByte(P^[y] AND 128);
          end;
          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;
      finally
        freemem(P);
      end;
    ProgressBar_Status.Position := 0;
    CloseFile(Forest_File);
  end;
end;

// OverallForest has one pixel of forest per byte
// interleaved in 4 quadrants
{----------------------------------------------------------------------------}
Procedure OverallForest_To_FOR(ForestFileName : string);
var
  x, y :integer;
  P : PByteArray;
  Index0, Index1, Index2, Index3: integer;

begin
  begin
    AssignFile(Forest_File,ForestFileName);
    Rewrite(Forest_File);
      try
        P := AllocMem(RowCount*ForestSize); // one row of FOR at a time (one column of OverallForest)
//        ProgressBar_Status.Max := ColumnCount;
        // forest is columnCount*2 x rowCount*2
        for x := 0 to (ColumnCount)-1 do begin
          for y := 0 to (RowCount)-1 do begin
            // x*Height + y, but reversed x and y
            // 4 quadrant pointers
            Index0 := ((RowCount*1)-1-y)*(ColumnCount*2) + (ColumnCount*1-1-x);
            Index1 := ((RowCount*1)-1-y)*(ColumnCount*2) + (ColumnCount*2-1-x);
            Index2 := ((RowCount*2)-1-y)*(ColumnCount*2) + (ColumnCount*1-1-x);
            Index3 := ((RowCount*2)-1-y)*(ColumnCount*2) + (ColumnCount*2-1-x);
            // assume 0 or 255 so any bit can be used as flag
            P^[y] := (OverallForestDeciduous[Index0] AND 1) OR
              (OverallForestDeciduous[Index1] AND 2) OR
              (OverallForestDeciduous[Index2] AND 4) OR
              (OverallForestDeciduous[Index3] AND 8) OR
              (OverallForestConiferous[Index0] AND 16) OR
              (OverallForestConiferous[Index1] AND 32) OR
              (OverallForestConiferous[Index2] AND 64) OR
              (OverallForestConiferous[Index3] AND 128);
          end;
          BlockWrite(Forest_File,P^,RowCount*ForestSize);
//          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;
      finally
        freemem(P);
      end;
//    ProgressBar_Status.Position := 0;
    CloseFile(Forest_File);
  end;
end;

{----------------------------------------------------------------------------}
Procedure Expand_x4_Save_V2(FOR_FilePath : string);
const
  st_Size = 512;
var
  st_X, st_Y :integer;
  st_X_Offset, st_Y_Offset :integer;
  st_ColumnCount, st_RowCount :integer;
  X, Y :integer;
  FOR_File : File of Byte;
  FileName : string;
  Index : integer;
  fValue : byte;
  P : PByteArray;

begin
  st_ColumnCount := ColumnCount div pColumns;
  st_RowCount := RowCount div pRows;

  P := AllocMem(st_Size); // allocate memory

  ProgressBar_Status.Max := st_ColumnCount*st_RowCount;
  for st_X := 0 to st_ColumnCount-1 do begin
    for st_Y := 0 to st_RowCount-1 do begin
      FileName := FOR_FilePath+'\'+format('%2.2d%2.2d.for',[st_X,st_Y]);
      AssignFile(FOR_File,FileName);
      Rewrite(FOR_File);
      st_X_Offset := (st_ColumnCount-1- st_X) * (pColumns * 2);
      st_Y_Offset := (st_RowCount-1- st_Y) * (pRows * 2);
      for X := 0 to (st_Size div 4)-1 do begin
        for Y := 0 to (st_Size div 4)-1 do begin
          Index := ForestSize* ( st_X_Offset +((st_Size div 4)-1-X) +
                  (st_ColumnCount* (st_Size div 4)*(st_Y_Offset+(st_Size div 4)-1-Y)));

          fValue := 0;
          if (OverallForestDeciduous[Index] <> 0) then begin
            fValue := fValue OR 2;
          end;
          if (OverallForestConiferous[Index] <> 0) then begin
            fValue := fValue OR 1;
          end;

          // expand horizontally x4
          P^[Y*4+0] := fvalue;
          P^[Y*4+1] := fvalue;
          P^[Y*4+2] := fvalue;
          P^[Y*4+3] := fvalue;

        end;
        // expand vertically x4
        BlockWrite(FOR_File,P^,st_Size);
        BlockWrite(FOR_File,P^,st_Size);
        BlockWrite(FOR_File,P^,st_Size);
        BlockWrite(FOR_File,P^,st_Size);
      end;
      Close(FOR_File);
      ProgressBar_Status.StepIt;
      Application.ProcessMessages;
    end;
  end;
  ProgressBar_Status.Position := 0;
  freemem(P);
end;

{----------------------------------------------------------------------------}
Procedure V2_FOR_To_ForestGrid(ForestFileName : string);
var
  x, y :integer;
  P : PByteArray;

begin
  if (NOT FileExists(ForestFileName)) then begin
    MessageShow('Forest file not found');
  end else begin
    ForestResolution := 8; // V2
    setLength(ForestGrid,pColumns*ForestResolution,pRows*ForestResolution);

    AssignFile(Forest_File,ForestFileName);
    Reset(Forest_File);
      try
        P := AllocMem(pColumns*ForestResolution); // one row of FOR at a time (one column of grid)
//        ProgressBar_Status.Max := pRows*ForestResolution;

        for y := 0 to (pRows*ForestResolution)-1 do begin
          BlockRead(Forest_File,P^,pColumns*ForestResolution);
          for x := 0 to (pColumns*ForestResolution)-1 do begin
            // lsb is coniferous in V2, i.e. reverse
            case P^[x] of
              1: begin
                P^[x] := 2;
              end;
              2: begin
                P^[x] := 1;
              end;
            end;
            ForestGrid[pColumns*ForestResolution-1-x,pColumns*ForestResolution-1-y] := P^[x];
          end;

//          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;

      finally
        freemem(P);
      end;
//    ProgressBar_Status.Position := 0;
    CloseFile(Forest_File);
  end;
end;

{----------------------------------------------------------------------------}
Procedure V2_ForestGrid_To_FOR(ForestFileName : string);
var
  x, y :integer;
  P : PByteArray;

begin
  begin
    ForestResolution := 8; // V2
    setLength(ForestGrid,pColumns*ForestResolution,pRows*ForestResolution);

    AssignFile(Forest_File,ForestFileName);
    Rewrite(Forest_File);
      try
        P := AllocMem(pColumns*ForestResolution); // one row of FOR at a time (one column of grid)
//        ProgressBar_Status.Max := pRows*ForestResolution;

        for y := 0 to (pRows*ForestResolution)-1 do begin
          for x := 0 to (pColumns*ForestResolution)-1 do begin
             P^[x] := ForestGrid[pColumns*ForestResolution-1-x,pColumns*ForestResolution-1-y];
            // lsb is coniferous in V2, i.e. reverse
            case P^[x] of
              1: begin
                P^[x] := 2;
              end;
              2: begin
                P^[x] := 1;
              end;
            end;
          end;
          BlockWrite(Forest_File,P^,pColumns*ForestResolution);

//          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;

      finally
        freemem(P);
      end;
//    ProgressBar_Status.Position := 0;
    CloseFile(Forest_File);
  end;
end;

{----------------------------------------------------------------------------}
Procedure V2_WriteBitmapForestTile(Forest : ForestType; ForestFileName : string);
var
  i,j : integer;
  FileIndex : longint;
  FileByte : byte;

  TileRow, TileColumn : integer;

begin
  AssignFile(Forest_File,ForestFileName);
  Rewrite(Forest_File);
  // create a header
  with xBitmapHeader_1bitColor do begin
    bDib.bWidth := pColumns*ForestResolution;
    bDib.bHeight := pRows*ForestResolution;
    bDib.bImageByteSize := bDib.bWidth*bDib.bHeight*xColor1Size div 8;
    bH.bFileByteSize := bDib.bImageByteSize+bH.bPixelArrayOffset;
  end;
  BlockWrite(Forest_File,xBitmapHeader_1bitColor,
    sizeof(xBitmapHeader_1bitColor));

  //write 2 color palette
  BlockWrite(Forest_File,xBMP_1bit_ForestColorTable,
    sizeof(xBMP_1bit_ForestColorTable));

  // do only one qTile for now
  TileRow := 0; TileColumn := 0;
  TileColumnCount := 1; TileRowCount := 1;

//  FileIndex := BitmapHeader_1bitBW.BitmapBWPixelOffset +
  FileIndex := xBitmapHeader_1bitColor.bH.bPixelArrayOffset +
               xColor1Size * (TileRow*pRows*ForestResolution*((TileColumnCount*pColumns*ForestResolution) div 8) +
                              (((TileColumnCount-1)-TileColumn)*pColumns*ForestResolution div 8)
                             );
  for i := pRows*ForestResolution-1 downto 0 do begin // rows from bottom
    seek(Forest_File,FileIndex);
    for j := 0 to (pColumns*ForestResolution div 8)-1 do begin // columns from left
      //assemble 8 bits
      FileByte := ByteToBit(Forest,ForestGrid[i],j*8);
      // write each forest index, 2 color (BW)
      write(Forest_File,FileByte);
    end;
    INC(FileIndex,(xColor1Size * (TileColumnCount*pColumns*ForestResolution) div 8));
  end;
end;

{----------------------------------------------------------------------------}
Procedure V2_2Color_WriteBitmapForestTile(Forest : ForestType; ForestFileName : string);
const
  pix = 8; // 8 pixels per byte
var
  i,j : integer;
  FileByte : byte;

begin
  AssignFile(Forest_File,ForestFileName);
  Rewrite(Forest_File);
  // create a header - 2 color
  with xBitmapHeader_1bitColor do begin
    bDib.bWidth := pColumns*ForestResolution;
    bDib.bHeight := pRows*ForestResolution;
    bDib.bImageByteSize := bDib.bWidth*bDib.bHeight*xColor1Size div 8;
    bH.bFileByteSize := bDib.bImageByteSize+bH.bPixelArrayOffset;
  end;
  BlockWrite(Forest_File,xBitmapHeader_1bitColor,
    sizeof(xBitmapHeader_1bitColor));

  //write 2 color palette
  BlockWrite(Forest_File,xBMP_1bit_ForestColorTable,
    sizeof(xBMP_1bit_ForestColorTable));

  for i := 0 to pRows*ForestResolution-1 do begin
    for j := 0 to (pColumns*ForestResolution div pix)-1 do begin // columns from left
      //assemble 8 bits
      FileByte := ByteToBit(Forest,ForestGrid[pRows*ForestResolution-1-i],j*pix); // from bottom
      // write each forest index, 2 color
      write(Forest_File,FileByte);
    end;
  end;
end;

// not/no-longer supported by bitmap
{----------------------------------------------------------------------------}
Procedure V2_4Color_Both_WriteBitmapForestTile(ForestFileName : string);
const
  pix = 4; // 4 pixels per byte
var
  i,j : integer;
  FileByte : byte;

begin
  AssignFile(Forest_File,ForestFileName);
  Rewrite(Forest_File);

  // create a header - 4 color
  with xBitmapHeader_2bitColor do begin
    bDib.bWidth := pColumns*ForestResolution;
    bDib.bHeight := pRows*ForestResolution;
    bDib.bImageByteSize := bDib.bWidth*bDib.bHeight*xColor2Size div 8;
    bH.bFileByteSize := bDib.bImageByteSize+bH.bPixelArrayOffset;
  end;
  BlockWrite(Forest_File,xBitmapHeader_2bitColor,
    sizeof(xBitmapHeader_2bitColor));

  //write 4 color palette
  BlockWrite(Forest_File,xBMP_2bit_ForestColorTable,
    sizeof(xBMP_2bit_ForestColorTable));

  for i := 0 to pRows*ForestResolution-1 do begin
    for j := 0 to (pColumns*ForestResolution div pix)-1 do begin // columns from left
      //assemble 4 * 2 bits
      FileByte := ByteTo2bits(ForestGrid[pRows*ForestResolution-1-i],j*pix); // from bottom
      // write each forest index, 4 color
      write(Forest_File,FileByte);
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure V2_16Color_Both_WriteBitmapForestTile(ForestFileName : string);
const
  pix = 2; // 2 pixels per byte
var
  i, j : integer;
//  FileByte : byte;
  P2pix  : pByteArray;

begin
  AssignFile(Forest_File,ForestFileName);
  Rewrite(Forest_File);

  // create a header - 16 color
  with xBitmapHeader_4bitColor do begin
    bDib.bWidth := pColumns*ForestResolution;
    bDib.bHeight := pRows*ForestResolution;
    bDib.bImageByteSize := bDib.bWidth*bDib.bHeight*xColor4Size div 8;
    bH.bFileByteSize := bDib.bImageByteSize+bH.bPixelArrayOffset;
  end;
  BlockWrite(Forest_File,xBitmapHeader_4bitColor,
    sizeof(xBitmapHeader_4bitColor));

  //write 16 color palette
  BlockWrite(Forest_File,xBMP_4bit_ForestColorTable,
    sizeof(xBMP_4bit_ForestColorTable));

  try
    P2pix := AllocMem(pColumns*ForestResolution div pix); // one row at a time
    for i := 0 to pRows*ForestResolution-1 do begin
      for j := 0 to (pColumns*ForestResolution div pix)-1 do begin // columns from left
        //assemble 2 * 4 bits
        P2pix^[j] := ByteTo4bits(ForestGrid[pRows*ForestResolution-1-i],j*pix); // from bottom
      end;
      // write each forest index, 4 color, 2 pix per byte
      BlockWrite(Forest_File,P2pix^,pColumns*ForestResolution div pix);
    end;
  finally
    freemem(P2pix);
  end;
end;

{----------------------------------------------------------------------------}
Procedure V2_GreyScale_WriteBitmapForestTile(Forest : ForestType; ForestFileName : string);
var
  i,j : integer;
//  FileByte : byte;
  Pindex  : pByteArray;
  pColor : ColorConvert;

begin
  AssignFile(Forest_File,ForestFileName);
  Rewrite(Forest_File);
  // create a header 8 bit color
  with BitmapHeader_8bitColor do begin
    bDib.bPaletteColors := 256; // 8 bit color
    bDib.bWidth := pColumns*ForestResolution;
    bDib.bHeight := pRows*ForestResolution;
    bDib.bImageByteSize := bDib.bWidth*bDib.bHeight*Color8Size div 8;
    bH.bFileByteSize := bDib.bImageByteSize+bH.bPixelArrayOffset;
    BlockWrite(Forest_File,BitmapHeader_8bitColor,
      sizeof(BitmapHeader_8bitColor));
  end;
  //write 256 color palette
  //seek(Forest_File,sizeof(BMP_Header) + sizeof(BMP_DIB_Header));
  pColor.ByteValue[3]:=0;
  for i := 0 to 256-1 do begin //grey scale
//      Write(Forest_File,byte(i),byte(i),byte(i),ZeroByte);
    pColor.ByteValue[0] := i;
    pColor.ByteValue[1] := i;
    pColor.ByteValue[2] := i;
    BlockWrite(Forest_File,pColor.ColorValue,sizeof(pColor));
  end;

  try
    Pindex := AllocMem(pColumns*ForestResolution); // one row at a time
    for i := 0 to pRows*ForestResolution-1 do begin
      for j := 0 to (pColumns*ForestResolution)-1 do begin // columns from left
        // convert to BW
        Pindex^[j] := ByteToGreyScale(Forest,ForestGrid[pRows*ForestResolution-1-i,j]); // from bottom
      end;
      // write each forest index, GreyScale, 1 pix per byte
      BlockWrite(Forest_File,Pindex^,pColumns*ForestResolution);
    end;
  finally
    freemem(Pindex);
  end;
end;

{----------------------------------------------------------------------------}
Procedure V2_24bit_WriteBitmapForestTile(Forest : ForestType; ForestFileName : string);
var
  i, j : integer;
  P24  : pRGBArray;

begin
  AssignFile(Forest_File,ForestFileName);
  Rewrite(Forest_File);
  // create a header
  with xBitmapHeader_24bitColor do begin
    bDib.bWidth := pColumns*ForestResolution;
    bDib.bHeight := pRows*ForestResolution;
    bDib.bImageByteSize := bDib.bWidth*bDib.bHeight*xColor24Size div 8;
    bH.bFileByteSize := bDib.bImageByteSize+bH.bPixelArrayOffset;
  end;
  BlockWrite(Forest_File,xBitmapHeader_24bitColor,
    sizeof(xBitmapHeader_24bitColor));
  try
    P24 := AllocMem(pColumns*ForestResolution * 3); // one row at a time
    for i := 0 to pRows*ForestResolution-1 do begin
      for j := 0 to (pColumns*ForestResolution)-1 do begin // columns from left
        // convert from index to color
        P24^[j] := ByteToColor(Forest,ForestGrid[pRows*ForestResolution-1-i,j]); // from bottom
      end;
      // write each forest index, color that matches Landscape Editor
      BlockWrite(Forest_File,P24^,pColumns*ForestResolution * 3);
    end;
  finally
    freemem(P24);
  end;
end;

{----------------------------------------------------------------------------}
begin { Initialization }
  Memo_Message := nil;
//testing
  ColumnCount := 8;
  RowCount := 5;
  SetLength(OverallForestDeciduous, ColumnCount*2*RowCount*2);
  SetLength(OverallForestConiferous,ColumnCount*2*RowCount*2);
//  OverallForest_To_FOR('Test.for');
//  FOR_To_OverallForest('Test.for');
//  Byte_To_Greyscale_Bitmap(OverallForestDeciduous, 'fDecidous.BMP');
//  Byte_To_Greyscale_Bitmap(OverallForestDeciduous, 'fConiferous.BMP');
end.

{--- End of File ------------------------------------------------------------}



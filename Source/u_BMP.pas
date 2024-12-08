{
 * u_BMP.pas
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

// Oct 2012 - known BUG !!! for each row, number of bytes must be divisible by 4 !!!
// if bitmap width is divisible by 4, it works OK

//---------------------------------------------------------------------------
UNIT u_BMP;

{----------------------------------------------------------------------------
functions to work on bitmap files *.bmp

NOTE: little-endian values

offset: $00, size 2 bytes, signature, must be 4D42 hex
offset: $02, size 4 bytes, file size in bytes
offset: $12, size 4 bytes, image width in pixels
offset: $16, size 4 bytes, image height in pixels
offset: $22, size 4 bytes, image size in bytes

----------------------------------------------------------------------------}

//===========================================================================
INTERFACE

uses
  Windows, StdCtrls, Graphics, comctrls, SysUtils;

type    // For scanline simplification
  TRGBArray = ARRAY[0..32767] OF TRGBTriple;
  pRGBArray = ^TRGBArray;

  TRGBAlphaArray = ARRAY[0..32767] OF TRGBQuad;
  pRGBAlphaArray = ^TRGBAlphaArray;

  TAlphaArray = ARRAY[0..32767] OF Byte;
  pAlphaArray = ^TAlphaArray;

type
  LongIntConvert = packed record
    case byte of
      0 : ( LongValue  : longword );
      1 : ( LongIntValue  : longint );
      2 : ( ByteValue  : array[0..3] of byte );
    end;

  WordConvert = packed record
    case byte of
      0 : ( WordValue : word );
      1 : ( IntegerValue : integer );
      2 : ( ByteValue : array[0..1] of byte );
    end;

  //TColor ->  (little-endian) Red, Green, Blue, 0
  //MC : ColorConvert;
  //mcolor : TColor;
  //MC.ColorValue := ByteSwapColor(mColor); //convert from TColor to ColorConvert
  //mColor := ByteSwapColor(MC.ColorValue); //convert from ColorConvert to TColor

  ColorConvert = packed record
    case byte of
      0 : ( ColorValue : Longword ); // byte reverse order and shifted from TColor
      1 : ( ByteValue : array[0..3] of byte );  // Alpha, B, G, R
      2 : ( cRGB : TRGBTriple; cAlpha : byte);
      3 : ( cRGBA : TRGBQuad);
    end;

//  BMP_8bit_ColorTable = array[0..256-1] of Longword;
  BMP_8bit_ColorTable = array[0..256-1] of TRGBQuad;
//  BMP_4bit_ColorTable = array[0..16-1] of Longword;
  BMP_4bit_ColorTable = array[0..16-1] of TRGBQuad;
//  BMP_2bit_ColorTable = array[0..4-1] of Longword;
  BMP_2bit_ColorTable = array[0..4-1] of TRGBQuad;
//  BMP_1bit_ColorTable = array[0..2-1] of Longword;
  BMP_1bit_ColorTable = array[0..2-1] of TRGBQuad;

  BMP_Header = packed record // packed for blockread/blockwrite alignment
    bSignature        : word;
    bFileByteSize     : longint;
    bReserved         : longint;
    bPixelArrayOffset : longint;
  end;

  BMP_DIB_Header = packed record
    bHeaderSize      : longint;
    bWidth           : longint;
    bHeight          : longint;
    bPlanes          : word;
    bColorBits       : word;
    bComp_biField    : longint;
    bImageByteSize   : longint;
    bHoriz_Ppm       : longint;
    bVert_Ppm        : longint;
    bPaletteColors   : longint;
    bImportantColors : longint;
  end;

  BMP_V1_Header = record
    bH   : BMP_Header;
    bDIB : BMP_DIB_Header;
  end;

const
  tDeciduous    : ColorConvert = (cRGB:(rgbtBlue:  0; rgbtGreen:255; rgbtRed:  0); cAlpha:0);
  tConiferous   : ColorConvert = (cRGB:(rgbtBlue:255; rgbtGreen:128; rgbtRed: 64); cAlpha:0);
  tConiferous_LE: ColorConvert = (cRGB:(rgbtBlue:  0; rgbtGreen:128; rgbtRed:  0); cAlpha:0);
  tBoth         : ColorConvert = (cRGB:(rgbtBlue: 64; rgbtGreen:128; rgbtRed:255); cAlpha:0);

  tNone         : ColorConvert = (cRGB:(rgbtBlue:  0; rgbtGreen:  0; rgbtRed:  0); cAlpha:0);
  tExclusion    : ColorConvert = (cRGB:(rgbtBlue:255; rgbtGreen:255; rgbtRed:255); cAlpha:0);

  tWater        : ColorConvert = (cRGB:(rgbtBlue:232; rgbtGreen:182; rgbtRed: 32); cAlpha:0);
  tSwamp        : ColorConvert = (cRGB:(rgbtBlue: 40; rgbtGreen:174; rgbtRed:255); cAlpha:0);
  tSand         : ColorConvert = (cRGB:(rgbtBlue: 88; rgbtGreen:169; rgbtRed:171); cAlpha:0);
  tGreenFields  : ColorConvert = (cRGB:(rgbtBlue: 45; rgbtGreen:198; rgbtRed:  0); cAlpha:0);
  tYellowFields : ColorConvert = (cRGB:(rgbtBlue:  0; rgbtGreen:238; rgbtRed:244); cAlpha:0);
  tDarkFields   : ColorConvert = (cRGB:(rgbtBlue:  0; rgbtGreen: 97; rgbtRed:149); cAlpha:0);

  tSelectRectColor : ColorConvert = (cRGB:(rgbtBlue:254; rgbtGreen:254; rgbtRed:254); cAlpha:0); //almost white

  BitmapSignature : word = $4D42; // 'BM'

  BitmapHeader : record
    BitmapSignatureOffset : longint;
    BitmapFileByteSizeOffset : longint;
    BitmapPixelArrayOffset : longint;
    BitmapHeaderSizeOffset : longint;
    BitmapWidthOffset : longint;
    BitmapHeightOffset : longint;
    BitmapPlanesOffset : longint;
    BitmapColorBitsOffset : longint;
    BitmapCompressionOffset : longint;
    BitmapImageByteSizeOffset : longint;
  end = (BitmapSignatureOffset:$00;     // 2 bytes
         BitmapFileByteSizeOffset:$02;  // 4 bytes - W*H*(C/8) + HS
         BitmapPixelArrayOffset:$0A;    // 4 bytes - HS - $00000036
         BitmapHeaderSizeOffset:$0E;    // 4 bytes - $00000028
         BitmapWidthOffset:$12;         // 4 bytes - W
         BitmapHeightOffset:$16;        // 4 bytes - H
         BitmapPlanesOffset:$1A;        // 2 bytes - $0001
         BitmapColorBitsOffset:$1C;     // 2 bytes - C - $0018 for 24 bit color
         BitmapCompressionOffset:$1E;   // 4 bytes - $00000000
         BitmapImageByteSizeOffset:$22; // 4 bytes - W*H*(C/8)
         );

{  // use 3 for BI_BITFIELD in compression field
  //Nick Notes - only recognized by some programs - don't use
  Color32Size = 4; //bytes
  BitmapHeader_32bitColor : record
    Bitmap32PixelOffset : longint;
    Bitmap32 : array[0..66-1] of byte;
  end = (Bitmap32PixelOffset:66;
         Bitmap32 : ($42,$4D,                //  bSignature        : word;
                     $11,$11,$11,$11,        //  bFileByteSize     : longint;
                     $00,$00,$00,$00,        //  bReserved         : longint;
                     $42,$00,$00,$00,        //  bPixelArrayOffset : longint;
                     $34,$00,$00,$00,        //  bHeaderSize      : longint;
                     $33,$33,$33,$33,        //  bWidth           : longint;
                     $44,$44,$44,$44,        //  bHeight          : longint;
                     $01,$00,                //  bPlanes          : word;
                     $20,$00,                //  bColorBits       : word;
                     $03,$00,$00,$00,        //  bComp_biField    : longint;
                     $22,$22,$22,$22,        //  bImageByteSize   : longint;
                     $00,$00,$00,$00,        //  bHoriz_Ppm       : longint;
                     $00,$00,$00,$00,        //  bVert_Ppm        : longint;
                     $00,$00,$00,$00,        //  bPaletteColors   : longint;
                     $00,$00,$00,$00,        //  bImportantColors : longint;
                     $FF,$00,$00,$00,
                     $00,$FF,$00,$00,
                     $00,$00,$FF,$00));
}
  // use 0 for BI_BITFIELD in compression field
  //Nick Notes - only recognized by some programs - don't use
  Color32Size = 4; //bytes
  BitmapHeader_32bitColor : record
    Bitmap32PixelOffset : longint;
    Bitmap32 : array[0..54-1] of byte;
  end = (Bitmap32PixelOffset:54;
         Bitmap32 : ($42,$4D,
                     $11,$11,$11,$11,
                     $00,$00,$00,$00,
                     $36,$00,$00,$00,
                     $28,$00,$00,$00,
                     $33,$33,$33,$33,
                     $44,$44,$44,$44,
                     $01,$00,
                     $20,$00,
                     $00,$00,$00,$00,
                     $22,$22,$22,$22,
                     $00,$00,$00,$00,
                     $00,$00,$00,$00,
                     $00,$00,$00,$00,
                     $00,$00,$00,$00));

  Color24Size = 3; //bytes
  BitmapHeader_24bitColor : record
    Bitmap24PixelOffset : longint;
    Bitmap24 : array[0..54-1] of byte;
  end = (Bitmap24PixelOffset:54;
         Bitmap24 : ($42,$4D,
                     $11,$11,$11,$11,
                     $00,$00,$00,$00,
                     $36,$00,$00,$00,
                     $28,$00,$00,$00,
                     $33,$33,$33,$33,
                     $44,$44,$44,$44,
                     $01,$00,
                     $18,$00,
                     $00,$00,$00,$00,
                     $22,$22,$22,$22,
                     $00,$00,$00,$00,
                     $00,$00,$00,$00,
                     $00,$00,$00,$00,
                     $00,$00,$00,$00));

  xColor24Size = 24; // bits
  xBitmapHeader_24bitColor : BMP_V1_Header = (
    bH:(
      bSignature: $4D42{BitmapSignature};
      bFileByteSize: 256*256*24 div 8 +
        sizeof(BMP_Header) + sizeof(BMP_DIB_Header);
      bReserved: 0;
      bPixelArrayOffset: sizeof(BMP_Header) + sizeof(BMP_DIB_Header) );
    bDIB:(
      bHeaderSize: sizeof(BMP_DIB_Header);
      bWidth:  256;
      bHeight: 256;
      bPlanes: 1;
      bColorBits: 24;
      bComp_biField: 0;
      bImageByteSize: 256*256*24 div 8;
      bHoriz_Ppm: 0;
      bVert_Ppm:  0;
      bPaletteColors: 0;
      bImportantColors: 0 );
  );

  xBMP_1bit_ForestColorTable : array[0..2-1] of Longword =
//    ($0000FF00, $00000000);  // why reversed ?
    ($00000000, $0000FF00);  // why reversed ?

  xBMP_2bit_ForestColorTable : array[0..4-1] of Longword =
    ($00000000, $0000FF00, $004080FF, $00FF8040);

  xBMP_4bit_ForestColorTable : array[0..16-1] of Longword =
    ($00000000, $0000FF00, $004080FF, $00FF8040,
     $00000000, $00000000, $00000000, $00000000,
     $00000000, $00000000, $00000000, $00000000,
     $00000000, $00000000, $00000000, $00000000);

  xColor1Size = 1; // bits
  xBitmapHeader_1bitColor : BMP_V1_Header = (
    bH:(
      bSignature: $4D42{BitmapSignature};
      bFileByteSize: 256*256*xColor1Size div 8 + sizeof(BMP_1bit_ColorTable) +
        sizeof(BMP_Header) + sizeof(BMP_DIB_Header);
      bReserved: 0;
      bPixelArrayOffset: sizeof(BMP_1bit_ColorTable) +
        sizeof(BMP_Header) + sizeof(BMP_DIB_Header) );
    bDIB:(
      bHeaderSize: sizeof(BMP_DIB_Header);
      bWidth:  256;
      bHeight: 256;
      bPlanes: 1;
      bColorBits: xColor1Size;
      bComp_biField: 0;
      bImageByteSize: 256*256*xColor1Size div 8;
      bHoriz_Ppm: 0;
      bVert_Ppm:  0;
      bPaletteColors: 2;  // can also be 0 -> default 2^1 entries
      bImportantColors: 0 );
  );

  // no-longer/not-ever supported ?
  xColor2Size = 2; // bits
  xBitmapHeader_2bitColor : BMP_V1_Header = (
    bH:(
      bSignature: $4D42{BitmapSignature};
      bFileByteSize: 256*256*xColor2Size div 8 + sizeof(BMP_2bit_ColorTable) +
        sizeof(BMP_Header) + sizeof(BMP_DIB_Header);
      bReserved: 0;
      bPixelArrayOffset: sizeof(BMP_2bit_ColorTable) +
        sizeof(BMP_Header) + sizeof(BMP_DIB_Header) );
    bDIB:(
      bHeaderSize: sizeof(BMP_DIB_Header);
      bWidth:  256;
      bHeight: 256;
      bPlanes: 1;
      bColorBits: xColor2Size;
      bComp_biField: 0;
      bImageByteSize: 256*256*xColor2Size div 8;
      bHoriz_Ppm: 0;
      bVert_Ppm:  0;
      bPaletteColors: 4; // can also be 0 -> default 2^8 entries
      bImportantColors: 0 );
  );

  xColor4Size = 4; // bits
  xBitmapHeader_4bitColor : BMP_V1_Header = (
    bH:(
      bSignature: $4D42{BitmapSignature};
      bFileByteSize: 256*256*xColor4Size div 8 + sizeof(BMP_4bit_ColorTable) +
        sizeof(BMP_Header) + sizeof(BMP_DIB_Header);
      bReserved: 0;
      bPixelArrayOffset: sizeof(BMP_4bit_ColorTable) +
        sizeof(BMP_Header) + sizeof(BMP_DIB_Header) );
    bDIB:(
      bHeaderSize: sizeof(BMP_DIB_Header);
      bWidth:  256;
      bHeight: 256;
      bPlanes: 1;
      bColorBits: xColor4Size;
      bComp_biField: 0;
      bImageByteSize: 256*256*xColor4Size div 8;
      bHoriz_Ppm: 0;
      bVert_Ppm:  0;
      bPaletteColors: 16; // can also be 0 -> default 2^8 entries
      bImportantColors: 0 );
  );

  Color8Size = 8; // bits
  BitmapHeader_8bitColor : BMP_V1_Header = (
    bH:(
      bSignature: $4D42{BitmapSignature};
      bFileByteSize: 256*256*Color8Size div 8 + sizeof(BMP_8bit_ColorTable) +
        sizeof(BMP_Header) + sizeof(BMP_DIB_Header);
      bReserved: 0;
      bPixelArrayOffset: sizeof(BMP_8bit_ColorTable) +
        sizeof(BMP_Header) + sizeof(BMP_DIB_Header) );
    bDIB:(
      bHeaderSize: sizeof(BMP_DIB_Header);
      bWidth:  256;
      bHeight: 256;
      bPlanes: 1;
      bColorBits: Color8Size;
      bComp_biField: 0;
      bImageByteSize: 256*256*Color8Size div 8;
      bHoriz_Ppm: 0;
      bVert_Ppm:  0;
      bPaletteColors: 256; // can also be 0 -> default 2^8 entries
      bImportantColors: 0 );
  );

{  ColorBWsize = 1; //bits
  // 8 extra bytes used to define two colors, 4 bytes for each color
  BitmapHeader_1bitBW : record
    BitmapBWPixelOffset : longint;
    BitmapBW : array[0..62-1] of byte;
  end = (BitmapBWPixelOffset:62;
         BitmapBW : ($42,$4D,$11,$11,$11,$11,$00,$00,
                     $00,$00,$3E,$00,$00,$00,$28,$00,
                     $00,$00,$33,$33,$33,$33,$44,$44,
                     $44,$44,$01,$00,$01,$00,$00,$00,
                     $00,$00,$22,$22,$22,$22,$00,$00,
                     $00,$00,$00,$00,$00,$00,$00,$00,
                     $00,$00,$00,$00,$00,$00,
                     $00,$BE,$00,$00,
                     $00,$00,$00,$00));
}
var
  BitmapSuccess : boolean;
  BitmapWidth : longint;
  BitmapHeight : longint;
  BitmapFileSize : longint;
  BitmapImageByteSize : longint;

  Memo_Message : TMemo;  // external TMemo for messages
  ProgressBar_Status : TProgressBar;
  BMPfolder : string;   // external path for file

//  NewColor : Tcolor;

type
  RGBArray = array[0..2] of Byte;
  HeightColorGradient = record
    fHeight : real;
    gColor : ColorConvert;
  end;

var
  Levels : integer;
  GradientArray : array of HeightColorGradient;
  GradientFile : file of HeightColorGradient;

function BMP_ColorSize(FileName : String) : Longint;
function BMP_ImageWidth(FileName : String) : Longint;
Procedure Bitmap_GetWidthHeight(FileName:string);
//function ColorMatch(tColor1,tColor2 : longword):boolean;
//function xColorMatch(tColor1,tColor2 : TColor):boolean;
procedure CalcGradientColor(Height : real; var Color : Tcolor);
procedure ReadGradientFile(FileName : string);
procedure WriteGradientFile(FileName : string);
procedure WriteBitMapToFile(mBitmap : TBitmap;FileName : string);
//function ByteReverseOrder32(L : Longword) : Longword;
Procedure SwapBlockEndToEnd(P : PByteArray; BlockSize : integer);
Procedure SwapRGBBlockEndToEnd(P : PRGBArray; BlockSize : integer);
function ByteSwapColor(Color: LongWord): LongWord; assembler;
procedure BMP_CopyMe(tobmp: TBitmap; frbmp : TBitmap);
//Procedure Bitmap_24_To_Bitmap_32(Bitmap_24_FileName,Bitmap_32_FileName : string);
Procedure Bitmap_24_To_Bitmap_32(Bitmap_24_FileName,
                                 Bitmap_32_FileName : string;
                                 AutoAlpha : boolean; Alpha : single);
procedure Rotate_180(Tile : Tbitmap);
Procedure Bitmap_24_To_Bitmap_32_Alpha(Bitmap_24_FileName,Bitmap_8_FileName,Bitmap_32_FileName : string);
Procedure Bitmap_24_To_Masked_24(Bitmap_24_FileName,Bitmap_8_FileName:string;maskColor:Tcolor);
Procedure NewBitmap_To_Masked(Bitmap_FileName,Bitmap_8_FileName:string; maskColor:Tcolor);
{procedure DDS_TO_BMP(aBmp : TBitmap);}

procedure WriteBMP24Header(FileName : String);
Procedure ForceBMP24size(FileName : string);
procedure Merge_BMP24_File(Offset_X, Offset_Y, Min_X, Max_X, Min_Y, Max_Y : LongInt;
                           FilePath,Filename,
                           FilePath_a,Filename_a : string);

//===========================================================================
IMPLEMENTATION

uses forms, FileCtrl{, Graphics};

var
  BitmapFile : File of byte;

{----------------------------------------------------------------------------}
Procedure MessageShow(Info : string);
begin
  if (Memo_Message <> nil) then begin
    Memo_Message.lines.add(Info);
  end;
end;

{----------------------------------------------------------------------------}
Procedure SwapBlockEndToEnd(P : PByteArray; BlockSize : integer);
var
  i : integer;
  Temp : byte;
begin
  for i := 0 to (BlockSize div 2) -1 do begin
    Temp := P^[i];
    P^[i] := P^[BlockSize-1-i];
    P^[BlockSize-1-i] := Temp;
  end;
end;

{----------------------------------------------------------------------------}
Procedure SwapRGBBlockEndToEnd(P : PRGBArray; BlockSize : integer);
var
  i : integer;
  Temp : TRGBTriple;
begin
  for i := 0 to (BlockSize div 2) -1 do begin
    Temp := P^[i];
    P^[i] := P^[BlockSize-1-i];
    P^[BlockSize-1-i] := Temp;
  end;
end;

{----------------------------------------------------------------------------}
function ByteSwapColor(Color: LongWord): LongWord; assembler;
asm
  BSWAP EAX
  SHR EAX,8
end;

{----------------------------------------------------------------------------}
function ByteReverseOrder32(L : Longword) : Longword;
asm
  BSWAP EAX
end;

{----------------------------------------------------------------------------}
Function Bitmap_CheckSignature : boolean;
var
  BitmapSig : word;

begin
  seek(BitmapFile,BitmapHeader.BitmapSignatureOffset);
  BlockRead(BitmapFile,BitmapSig,2);
  Bitmap_CheckSignature := (BitmapSig = BitmapSignature);
end;

// some 32 bitmaps indicate RLE 4bit compression, but are not
{----------------------------------------------------------------------------}
function ValidateNotCompressed(Header : BMP_V1_Header) : boolean;
begin
  with Header do begin
    if (bDib.bComp_biField = 0) then begin
    end else begin
      result := (bDib.bWidth * bDib.bHeight * (bDib.bColorbits div 8) +
        bH.bPixelArrayOffset = bh.bFileByteSize);
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure GetWidthHeight;
var
  FileByte : byte;
  LIC : LongIntConvert;

begin
  seek(BitmapFile,BitmapHeader.BitmapFileByteSizeOffset);
  BlockRead(BitmapFile,BitmapFileSize,4);

  seek(BitmapFile,BitmapHeader.BitmapImageByteSizeOffset);
  BlockRead(BitmapFile,BitmapImageByteSize,4);
{
  read(BitmapFile,FileByte);
  LIC.ByteValue[0] := FileByte;
  read(BitmapFile,FileByte);
  LIC.ByteValue[1] := FileByte;
  read(BitmapFile,FileByte);
  LIC.ByteValue[2] := FileByte;
  read(BitmapFile,FileByte);
  LIC.ByteValue[3] := FileByte;
  BitmapWidth := LIC.LongValue;
}
  seek(BitmapFile,BitmapHeader.BitmapWidthOffset);
  BlockRead(BitmapFile,BitmapWidth,4);
{
  read(BitmapFile,FileByte);
  LIC.ByteValue[0] := FileByte;
  read(BitmapFile,FileByte);
  LIC.ByteValue[1] := FileByte;
  read(BitmapFile,FileByte);
  LIC.ByteValue[2] := FileByte;
  read(BitmapFile,FileByte);
  LIC.ByteValue[3] := FileByte;
  BitmapHeight := LIC.LongValue;
}
  seek(BitmapFile,BitmapHeader.BitmapHeightOffset);
  BlockRead(BitmapFile,BitmapHeight,4);
end;

{----------------------------------------------------------------------------}
Procedure Bitmap_GetWidthHeight(FileName:string);
begin
  BitmapSuccess := false;
  if (NOT FileExists(BMPfolder+'\'+FileName)) then begin
//    MessageShow('Bitmap file Not Found');
////    MessageShow(BMPfolder+'\'+FileName);
//    Beep;
  end else begin
    AssignFile(BitmapFile,BMPfolder+'\'+FileName);
    Reset(BitmapFile);
    if (NOT Bitmap_CheckSignature) then begin
      MessageShow('Invalid bitmap file');
      Beep;
    end else begin
      GetWidthHeight;
      BitmapSuccess := true;
      if (BitmapSuccess) then begin
        MessageShow(format('Bitmap width: %d  height: %d',[BitmapWidth, BitmapHeight]));
      end;
    end;
    Close(BitmapFile);
  end;
end;

{----------------------------------------------------------------------------}
function BMP_ColorSize(FileName : String) : Longint;
var
  Bitmap_Hdr : BMP_V1_Header;
begin
  Result := -1; // assume at first
  if (NOT fileExists(FileName)) then begin
    exit;
  end;
  AssignFile(BitmapFile,FileName);
  Reset(BitmapFile);
  BlockRead(BitmapFile,Bitmap_Hdr,sizeof(Bitmap_Hdr));
  // check for bitmap signature
  if (Bitmap_Hdr.bH.bSignature <> $4D42) then begin
     Close(BitmapFile);
    exit;
  end;
  // get width
  result := Bitmap_Hdr.bDib.bColorBits;
  Close(BitmapFile);
end;

{----------------------------------------------------------------------------}
function BMP_ImageWidth(FileName : String) : Longint;
var
  Bitmap_Hdr : BMP_V1_Header;
begin
  Result := -1; // assume at first
  if (NOT fileExists(FileName)) then begin
    exit;
  end;
  AssignFile(BitmapFile,FileName);
  Reset(BitmapFile);
  BlockRead(BitmapFile,Bitmap_Hdr,sizeof(Bitmap_Hdr));
  // check for bitmap signature
  if (Bitmap_Hdr.bH.bSignature <> $4D42) then begin
     Close(BitmapFile);
    exit;
  end;
  // get width
  result := Bitmap_Hdr.bDib.bWidth;
  Close(BitmapFile);
end;

{----------------------------------------------------------------------------}
function xColorMatch(tColor1,tColor2 : TColor):boolean;
begin
  xColorMatch := (tColor1 = tColor2);
end;

{----------------------------------------------------------------------------}
function ColorMatch(tColor1,tColor2 : Longword):boolean;
begin
  ColorMatch := (tColor1 = tColor2);
end;

//type
//  RGBArray = array[0..2] of Byte;
//  HeightColorGradient = record
//    fHeight : real;
//    gColor : RGBArray;
//  end;
//
//var
//  Levels : integer;
//  GradientArray : array of HeightColorGradient;

{----------------------------------------------------------------------------}
Procedure SetGradientColors(Heights : array of real; Colors: array of TColor);
var
  i : integer;

begin
  Levels := High(Heights)+1;
  if Levels <> High(Colors)+1 then begin
    MessageShow('altitude color mismatch');
    exit;
  end;
  if Levels < 2 then begin // must be at least 2 colors
    MessageShow('Need at least two levels');
    exit;
  end else begin
    SetLength(GradientArray, Levels);
    for i := 0 to Levels-1 do begin
      Colors[i] := ColorToRGB(Colors[i]);
      GradientArray[i].fHeight := Heights[i];
//      GradientArray[i].gColor[0] := GetRValue(Colors[i]); // uses Windows
//      GradientArray[i].gColor[1] := GetGValue(Colors[i]);
//      GradientArray[i].gColor[2] := GetBValue(Colors[i]);
      GradientArray[i].gColor.ColorValue := Colors[i];
    end;
  end;
end;

{----------------------------------------------------------------------------}
procedure CalcGradientColor(Height : real; var Color : Tcolor);
var
  i,j : integer;
  Ratio : real;
  GradientColor : ColorConvert;

begin
{  search height in altitude array
  calc ratio
  calc color based on ratio
}
  if Height >= GradientArray[Levels-1].fHeight then begin
//    Color := RGB(GradientArray[Levels-1].gColor[0],
//                 GradientArray[Levels-1].gColor[1],
//                 GradientArray[Levels-1].gColor[2]);
    Color := GradientArray[Levels-1].gColor.ColorValue;
  end else begin
    for i := 0 to Levels-1 do begin
      if Height < GradientArray[i].fHeight then begin
        break;
      end;
    end;
    if (i = 0) then begin
//      Color := RGB(GradientArray[0].gColor[0],
//                   GradientArray[0].gColor[1],
//                   GradientArray[0].gColor[2]);
      Color := GradientArray[0].gColor.ColorValue;
    end else begin
      Ratio := (Height-GradientArray[i-1].fHeight) / (GradientArray[i].fHeight - GradientArray[i-1].fHeight);
      for j := 0 to 3 do begin
        GradientColor.ByteValue[j] := Trunc(GradientArray[i-1].gColor.ByteValue[j] +
                   ((GradientArray[i].gColor.ByteValue[j] -
                     GradientArray[i-1].gColor.ByteValue[j]) * Ratio));
      end;
//      Color := RGB(GradientColor.ByteValue[1], GradientColor.ByteValue[2], GradientColor.ByteValue[3]);
      Color := GradientColor.ColorValue;
    end;
  end;
end;

{----------------------------------------------------------------------------}
procedure ReadGradientFile(FileName : string);
var
  i : integer;

begin
  if (FileExists(BMPfolder+'\'+FileName)) then begin
    AssignFile(GradientFile,BMPfolder+'\'+FileName);
    Reset(GradientFile);
    i := 0;
    While (NOT EOF(GradientFile)) do begin
      read(GradientFile,GradientArray[i]);
      INC(i);
    end;
    Levels := i;
    Close(GradientFile);
  end;
end;

{----------------------------------------------------------------------------}
procedure WriteGradientFile(FileName : string);
var
  i : integer;
begin
  if (DirectoryExists(BMPfolder)) then begin
    AssignFile(GradientFile,BMPfolder+'\'+FileName);
    Rewrite(GradientFile);
    for i := 0 to Levels-1 do begin
      write(GradientFile,GradientArray[i]);
    end;
    Close(GradientFile);
  end;
end;

{24 and 32 bit bitmaps ------------------------------------------------------}
{----------------------------------------------------------------------------}
procedure WriteBitMapToFile(mBitmap : TBitmap;FileName : string);
var
  i, j : integer;
  BMP_File : File of byte;
  FileByte : byte;
  P : PByteArray;
  LIC : LongIntConvert;

const
  ZeroByte : byte = 0;

begin
    AssignFile(BMP_File,{FilePath+'\'+}FileName);
    Rewrite(BMP_File);
    // create a header
    case mBitmap.PixelFormat of
      pf24bit : begin
        for i := 0 to 54-1 do begin
          Write(BMP_File,BitmapHeader_24bitColor.Bitmap24[i]);
        end;
        seek(BMP_File,BitmapHeader.BitmapImageByteSizeOffset);
        LIC.LongValue := mBitmap.Width*mBitmap.Height*Color24Size; // 3 for 24 bit color
        Write(BMP_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
        seek(BMP_File,BitmapHeader.BitmapFileByteSizeOffset);
        LIC.LongValue := LIC.LongValue+54;
        Write(BMP_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
      end;
      pf32bit: begin // use 24bits and store 3 of 4 bytes instead
        for i := 0 to 54-1 do begin
          Write(BMP_File,BitmapHeader_32bitColor.Bitmap32[i]);
        end;
        seek(BMP_File,BitmapHeader.BitmapImageByteSizeOffset);
        LIC.LongValue := mBitmap.Width*mBitmap.Height*Color32Size; // 4 for 32 bit color
        Write(BMP_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
        seek(BMP_File,BitmapHeader.BitmapFileByteSizeOffset);
        LIC.LongValue := LIC.LongValue+54;
        Write(BMP_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
      end;
    end;
    seek(BMP_File,BitmapHeader.BitmapWidthOffset);
    LIC.LongValue := mBitmap.Width;
    Write(BMP_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
    seek(BMP_File,BitmapHeader.BitmapHeightOffset);
    LIC.LongValue := mBitmap.Height;
    Write(BMP_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
    //write pixel data
    seek(BMP_File,BitmapHeader_24bitColor.Bitmap24PixelOffset);
    //for i := 0 to integer(mBitmap.Height-1) do begin //reverse vertical
    for i := mBitmap.Height-1 downto 0 do begin
      P := mBitmap.Scanline[i];
        case mBitmap.PixelFormat of
          pf24bit: begin
            BlockWrite(BMP_File, P^, {sizeof(P^)}mBitmap.Width*3);
          end;
          pf32bit: begin
            BlockWrite(BMP_File, P^, {sizeof(P)}mBitmap.Width*4);
          end;
        end;
    end;
    Close(BMP_File);
end;

{24 and 32 bit bitmaps ------------------------------------------------------}
// modified for writing 24 instead of 32
{----------------------------------------------------------------------------}
procedure xWriteBitMapToFile(mBitmap : TBitmap;FileName : string);
var
  i, j : integer;
  BMP_File : File of byte;
  FileByte : byte;
  P : PByteArray;
  LIC : LongIntConvert;

const
  ZeroByte : byte = 0;

begin
    AssignFile(BMP_File,{FilePath+'\'+}FileName);
    Rewrite(BMP_File);
    // create a header
    case mBitmap.PixelFormat of
//      pf24bit, pf32bit : begin
      pf24bit : begin
        for i := 0 to 54-1 do begin
          Write(BMP_File,BitmapHeader_24bitColor.Bitmap24[i]);
        end;
        seek(BMP_File,BitmapHeader.BitmapImageByteSizeOffset);
        LIC.LongValue := mBitmap.Width*mBitmap.Height*Color24Size; // 3 for 24 bit color
        Write(BMP_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
        seek(BMP_File,BitmapHeader.BitmapFileByteSizeOffset);
        LIC.LongValue := LIC.LongValue+54;
        Write(BMP_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
      end;
      pf32bit: begin // use 24bits and store 3 of 4 bytes instead
        for i := 0 to 54-1 do begin
//          Write(BMP_File,BitmapHeader_32bitColor.Bitmap32[i]);
          Write(BMP_File,BitmapHeader_24bitColor.Bitmap24[i]); // force 24 bit and write 3 of 4
        end;
        seek(BMP_File,BitmapHeader.BitmapImageByteSizeOffset);
//        LIC.LongValue := mBitmap.Width*mBitmap.Height*Color32Size; // 4 for 32 bit color
        LIC.LongValue := mBitmap.Width*mBitmap.Height*Color24Size; // 3 for 24 bit color
        Write(BMP_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
        seek(BMP_File,BitmapHeader.BitmapFileByteSizeOffset);
        LIC.LongValue := LIC.LongValue+54;
        Write(BMP_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
      end;
    end;
    seek(BMP_File,BitmapHeader.BitmapWidthOffset);
    LIC.LongValue := mBitmap.Width;
    Write(BMP_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
    seek(BMP_File,BitmapHeader.BitmapHeightOffset);
    LIC.LongValue := mBitmap.Height;
    Write(BMP_File,LIC.ByteValue[0],LIC.ByteValue[1],LIC.ByteValue[2],LIC.ByteValue[3]);
    //write pixel data
    seek(BMP_File,BitmapHeader_24bitColor.Bitmap24PixelOffset);
    //for i := 0 to integer(mBitmap.Height-1) do begin //reverse vertical
    for i := mBitmap.Height-1 downto 0 do begin
      P := mBitmap.Scanline[i];
        case mBitmap.PixelFormat of
          pf24bit: begin
//            for j := 0 to mBitmap.Width-1 do begin
//              Write(BMP_File,P^[j*3],P^[j*3+1],P^[j*3+2]);
//            end;
            BlockWrite(BMP_File, P^, {sizeof(P^)}mBitmap.Width*3);
          end;
          pf32bit: begin
//            for j := 0 to mBitmap.Width-1 do begin
//              Write(BMP_File,P^[j*4],P^[j*4+1],P^[j*4+2]);
//            end;
  //          BlockWrite(BMP_File, P^, {sizeof(P)}mBitmap.Width*4);
     //          for j := 0 to mBitmap.Width-1 do begin
     //            BlockWrite(BMP_File, P^[j*4], 3);    // way too slow !
     //          end;
        for j := 0 to mBitmap.Width-1 do begin
          P^[j*3] := P^[j*4];
          P^[j*3+1] := P^[j*4+1];
          P^[j*3+2] := P^[j*4+2];
        end;
        BlockWrite(BMP_File, P^, {sizeof(P)}mBitmap.Width*3);
          end;
        end;
    end;
    Close(BMP_File);
end;

{A simple procedure to copy any TGraphic to a 24-bit TBitmap}
//----------------------------------------------------------------------------
procedure BMP_CopyMe(tobmp: TBitmap; frbmp : TBitmap);
begin
  tobmp.Width := frbmp.Width;
  tobmp.Height := frbmp.Height;
  tobmp.PixelFormat := pf24bit;
  tobmp.Canvas.Draw(0,0,frbmp);
end;

{----------------------------------------------------------------------------}
Procedure Bitmap_24_To_Bitmap_32(Bitmap_24_FileName,
                                 Bitmap_32_FileName : string;
                                 AutoAlpha : boolean; Alpha : single);
const
  ZeroByte : byte = 0;

var
  i, j :integer;
//  FileByte : byte;
  pColor : ColorConvert;
  Bitmap_24_File : File of byte;
  Bitmap_32_File : File of byte;
  Bitmap_Hdr : BMP_V1_Header;
//  ByteCount : longint;
  P24 : pRGBArray;
  P32 : pRGBAlphaArray;

begin
  if (NOT FileExists(Bitmap_24_FileName)) then begin
    MessageShow('Bitmap file not found');
    Exit;
  end;
  begin
    MessageShow('Converting 24 bit bitmap to 32 bitmap...');
    AssignFile(Bitmap_24_File,Bitmap_24_FileName);
    Reset(Bitmap_24_File);
//    seek(Bitmap_24_File,0);
    BlockRead(Bitmap_24_File,Bitmap_Hdr,sizeof(Bitmap_Hdr));
//    if (NOT ValidateNotCompressed(Bitmap_Hdr)) then begin
//      MessageShow('Error: Compression not allowed');
//    end;
    AssignFile(Bitmap_32_File,Bitmap_32_FileName);
    Rewrite(Bitmap_32_File);
    // create a TDM header
    with Bitmap_Hdr do begin
      // first confirm it is bmp and 24 bit color
      if ((bH.bSignature <> $4D42) OR              // BitmapSignature
          (bDib.bColorBits <> 24)) then begin // 24 bit color
        MessageShow('Error: Not 24 bit color bitmap');
      end else begin
        bDib.bColorBits := 32;
        bDib.bImageByteSize := bDib.bWidth * bDib.bHeight * Color32Size;
      BlockWrite(Bitmap_32_File,Bitmap_Hdr,
        sizeof(Bitmap_Hdr));
      seek(Bitmap_32_File,bH.bPixelArrayOffset); // always do in case header is larger than standard
      // now read 24 bit pixels and write 32 bit pixels
//      pColor.cAlpha := 0;
      try
        P24 := AllocMem(bDib.bWidth * Color24Size); // one row at a time
        P32 := AllocMem(bDib.bWidth * Color32Size); // one row at a time
        ProgressBar_Status.Max := bDib.bHeight;
        for i := 0 to bDib.bHeight-1 do begin
          BlockRead(Bitmap_24_File,P24^,bDib.bWidth * sizeof(TRGBTriple));
          for j := 0 to bDib.bWidth-1 do begin
            pColor.cRGB := P24^[j]; pColor.cAlpha := 0;
            if (AutoAlpha) then begin
              if (pColor.ColorValue <> 0) then begin
                pColor.cAlpha := 255; // create mask if color not black
              end else begin // dummy background color with alpha=0
//                pColor.ColorValue := $00385840;  // Alpha, B, G, R  (for darker drevesa.dds)
              end;
            end else begin
              pColor.cAlpha := round(Alpha*255);
            end;
            P32^[j] := pColor.cRGBA;
          end;
          BlockWrite(Bitmap_32_File,P32^,bDib.bWidth * sizeof(TRGBQuad));

          ProgressBar_Status.StepIt;
//          Application.ProcessMessages;
        end;
      finally
        freemem(P32);
        freemem(P24);
      end;

      end;
    end;

    Close(Bitmap_32_File);
    Close(Bitmap_24_File);
    MessageShow('32 bit color bitmap created.');
    ProgressBar_Status.Position := 0;
  end;
end;

{----------------------------------------------------------------------------}
Procedure Bitmap_24_To_Bitmap_32_Alpha(Bitmap_24_FileName,Bitmap_8_FileName,Bitmap_32_FileName : string);
const
  ZeroByte : byte = 0;

var
  i, j :integer;
//  FileByte : byte;
  pColor : ColorConvert;
  Bitmap_24_File : File of byte;
  Bitmap_32_File : File of byte;
  Bitmap_8_File : File of byte;
  Bitmap_Hdr_8 : BMP_V1_Header;
  Bitmap_Hdr : BMP_V1_Header;
//  ByteCount : longint;
  P8  : pAlphaArray;
  P24 : pRGBArray;
  P32 : pRGBAlphaArray;
  Palette_8 : BMP_8bit_ColorTable;

begin
  if (NOT FileExists(Bitmap_24_FileName)) then begin
    MessageShow('Bitmap file not found');
    Exit;
  end;
  if ((Bitmap_8_FileName <> '') AND (NOT FileExists(Bitmap_8_FileName))) then begin
    MessageShow('Alpha Bitmap file not found');
    Exit;
  end;
  begin
    AssignFile(Bitmap_8_File,Bitmap_8_FileName);
    Reset(Bitmap_8_File);
//    seek(Bitmap_8_File,0);
    BlockRead(Bitmap_8_File,Bitmap_Hdr_8,sizeof(Bitmap_Hdr));
    with Bitmap_Hdr_8 do begin
      // first confirm it is bmp and 8 bit color
      if ((bH.bSignature <> $4D42) OR              // BitmapSignature
          (bDib.bColorBits <> 8)) then begin // 8 bit color
        MessageShow('Error: Not 8 bit color bitmap');
        Close(Bitmap_8_File);
        Exit;
      end;
    end;

    MessageShow('Converting 24 bit bitmap to 32 bitmap...');
    AssignFile(Bitmap_24_File,Bitmap_24_FileName);
    Reset(Bitmap_24_File);
//    seek(Bitmap_24_File,0);
    BlockRead(Bitmap_24_File,Bitmap_Hdr,sizeof(Bitmap_Hdr));
    AssignFile(Bitmap_32_File,Bitmap_32_FileName);
    Rewrite(Bitmap_32_File);
    with Bitmap_Hdr do begin
      // first confirm it is bmp and 24 bit color
      if ((bH.bSignature <> $4D42) OR              // BitmapSignature
          (bDib.bColorBits <> 24)) then begin // 24 bit color
        MessageShow('Error: Not 24 bit color bitmap');
      end else begin
        if (Bitmap_Hdr_8.bDib.bWidth <> bDib.bWidth) then begin
          MessageShow('Error: bitmap size mismatch');
          exit;
        end;
        bDib.bColorBits := 32;
        bDib.bImageByteSize := bDib.bWidth * bDib.bHeight * Color32Size;
        BlockWrite(Bitmap_32_File,Bitmap_Hdr,
          sizeof(Bitmap_Hdr));

        // read the palette for the 8 bit file to access colors
        BlockRead(Bitmap_8_File,Palette_8,sizeof(BMP_8bit_ColorTable));
        // seek(Bitmap_8_File,Bitmap_Hdr_8.bH.bPixelArrayOffset); // need to skip over color table

        // now read 24 bit pixels, and alpha and write 32 bit pixels
        try
          P8  := AllocMem(bDib.bWidth * Color8Size);  // one row at a time
          P24 := AllocMem(bDib.bWidth * Color24Size); // one row at a time
          P32 := AllocMem(bDib.bWidth * Color32Size); // one row at a time
          ProgressBar_Status.Max := bDib.bHeight;
          for i := 0 to bDib.bHeight-1 do begin
            BlockRead(Bitmap_8_File,P8^,bDib.bWidth * sizeof(Byte));
            BlockRead(Bitmap_24_File,P24^,bDib.bWidth * sizeof(TRGBTriple));
            for j := 0 to bDib.bWidth-1 do begin
              pColor.cRGB := P24^[j];
              //pColor.cAlpha := P8^[j]; // read 8 bit palette entry instead ?
              pColor.cAlpha := Palette_8[P8^[j]].rgbRed; // read 8 bit palette entry instead ?
//              if (pColor.ColorValue <> 0) then begin
//                pColor.cAlpha := 255; // create mask if color not black
//              end else begin // dummy background color with alpha=0
////                pColor.ColorValue := $00385840;  // Alpha, B, G, R  (for darker drevesa.dds)
//              end;
              P32^[j] := pColor.cRGBA;
            end;
            BlockWrite(Bitmap_32_File,P32^,bDib.bWidth * sizeof(TRGBQuad));

            ProgressBar_Status.StepIt;
//            Application.ProcessMessages;
          end;
        finally
          freemem(P32);
          freemem(P24);
          freemem(P8);
        end;

      end;
    end;

    Close(Bitmap_32_File);
    Close(Bitmap_24_File);
    Close(Bitmap_8_File);
    MessageShow('Alpha added to bitmap.');
    ProgressBar_Status.Position := 0;
  end;
end;

// use an 8 bit mask file to force a given color when mask is not white 255,255,255
{----------------------------------------------------------------------------}
Procedure Bitmap_24_To_Masked_24(Bitmap_24_FileName,Bitmap_8_FileName:string;maskColor:Tcolor);
const
  ZeroByte : byte = 0;

var
  i, j :integer;
//  FileByte : byte;
  pColor : ColorConvert;
  Bitmap_24_File : File of byte;
  Bitmap_24_Masked_File : File of byte;
  Bitmap_8_File : File of byte;
  Bitmap_Hdr_8 : BMP_V1_Header;
  Bitmap_Hdr : BMP_V1_Header;
//  ByteCount : longint;
  P8  : pAlphaArray;
  P24 : pRGBArray;
  Palette_8 : BMP_8bit_ColorTable;
  Bitmap_Mask_FileName : string;

begin
  Bitmap_Mask_FileName := copy(Bitmap_24_FileName,1,pos('.bmp',Bitmap_24_FileName)-1)+'_orig.bmp';
  pColor.ColorValue := ByteSwapColor(maskColor); // need to convert format
  if (NOT FileExists(Bitmap_24_FileName)) then begin
    MessageShow('Bitmap file not found');
    Exit;
  end else begin
    // save original
//    if (NOT FileExists(Bitmap_Mask_FileName)) then begin
//      CopyFile(PChar(Bitmap_24_FileName),PChar(Bitmap_Mask_FileName),false); // no overwrite
//    end;
    CopyFile(PChar(Bitmap_24_FileName),PChar(Bitmap_Mask_FileName),true); // overwrite
  end;
  if ((Bitmap_8_FileName <> '') AND (NOT FileExists(Bitmap_8_FileName))) then begin
    MessageShow('Mask Bitmap file not found');
    Exit;
  end;
  begin
    AssignFile(Bitmap_8_File,Bitmap_8_FileName);
    Reset(Bitmap_8_File);
//    seek(Bitmap_8_File,0);
    BlockRead(Bitmap_8_File,Bitmap_Hdr_8,sizeof(Bitmap_Hdr));
    with Bitmap_Hdr_8 do begin
      // first confirm it is bmp and 8 bit color
      if ((bH.bSignature <> $4D42) OR              // BitmapSignature
          (bDib.bColorBits <> 8)) then begin // 8 bit color
        MessageShow('Error: Not 8 bit color bitmap');
        Close(Bitmap_8_File);
        Exit;
      end;
    end;

    MessageShow('Masking 24 bit bitmap with 8 bitmap...');
//    AssignFile(Bitmap_24_File,Bitmap_24_FileName);
    AssignFile(Bitmap_24_File,Bitmap_Mask_FileName);
    Reset(Bitmap_24_File);
//    seek(Bitmap_24_File,0);
    BlockRead(Bitmap_24_File,Bitmap_Hdr,sizeof(Bitmap_Hdr));
//    AssignFile(Bitmap_24_Masked_File,Bitmap_Mask_FileName);
    AssignFile(Bitmap_24_Masked_File,Bitmap_24_FileName);
    Rewrite(Bitmap_24_Masked_File);
    with Bitmap_Hdr do begin
      // first confirm it is bmp and 24 bit color
      if ((bH.bSignature <> $4D42) OR              // BitmapSignature
          (bDib.bColorBits <> 24)) then begin // 24 bit color
        MessageShow('Error: Not 24 bit color bitmap');
      end else begin
        if (Bitmap_Hdr_8.bDib.bWidth <> bDib.bWidth) then begin
          MessageShow('Error: bitmap size mismatch');
          exit;
        end;
        // copy the header as is
        BlockWrite(Bitmap_24_Masked_File,Bitmap_Hdr,
          sizeof(Bitmap_Hdr));

        // read the palette for the 8 bit file to access colors
        BlockRead(Bitmap_8_File,Palette_8,sizeof(BMP_8bit_ColorTable));
        // seek(Bitmap_8_File,Bitmap_Hdr_8.bH.bPixelArrayOffset); // need to skip over color table

        // now read 24 bit pixels, and overwrite when needed
        try
          P8  := AllocMem(bDib.bWidth * Color8Size);  // one row at a time
          P24 := AllocMem(bDib.bWidth * Color24Size); // one row at a time
          ProgressBar_Status.Max := bDib.bHeight;
          for i := 0 to bDib.bHeight-1 do begin
            BlockRead(Bitmap_8_File,P8^,bDib.bWidth * sizeof(Byte));
            BlockRead(Bitmap_24_File,P24^,bDib.bWidth * sizeof(TRGBTriple));
            for j := 0 to bDib.bWidth-1 do begin
              //pColor.cRGB := P24^[j];
              //pColor.cAlpha := P8^[j]; // read 8 bit palette entry instead ?
              //pColor.cAlpha := Palette_8[P8^[j]].rgbRed; // read 8 bit palette entry instead ?
//              if (pColor.ColorValue <> 0) then begin
//                pColor.cAlpha := 255; // create mask if color not black
//              end else begin // dummy background color with alpha=0
////                pColor.ColorValue := $00385840;  // Alpha, B, G, R  (for darker drevesa.dds)
//              end;
              if (Palette_8[P8^[j]].rgbRed <> 255) then begin
                P24^[j] := pColor.cRGB;
              end;
            end;
            BlockWrite(Bitmap_24_Masked_File,P24^,bDib.bWidth * sizeof(TRGBTriple));

            ProgressBar_Status.StepIt;
//            Application.ProcessMessages;
          end;
        finally
          freemem(P24);
          freemem(P8);
        end;

      end;
    end;

    Close(Bitmap_24_Masked_File);
    Close(Bitmap_24_File);
    Close(Bitmap_8_File);
    MessageShow('Bitmap masked.');
    ProgressBar_Status.Position := 0;
  end;
end;

// use an 8 bit mask file to force a given color when mask is not white 255,255,255
{----------------------------------------------------------------------------}
Procedure NewBitmap_To_Masked(Bitmap_FileName,Bitmap_8_FileName:string; maskColor:Tcolor);
const
  ZeroByte : byte = 0;

var
  i, j :integer;
//  FileByte : byte;
  pColor : ColorConvert;
  Bitmap_File : File of byte;
  Bitmap_Hdr : BMP_V1_Header;
  Bitmap_8_File : File of byte;
  Bitmap_Hdr_8 : BMP_V1_Header;
  Bitmap_Masked_File : File of byte;
//  ByteCount : longint;
  P8, P8mask  : pAlphaArray;
  P24 : pRGBArray;
  Palette_8mask : BMP_8bit_ColorTable;
  Palette_8 : BMP_8bit_ColorTable;
  Bitmap_Mask_FileName : string;
  PaletteIndex : integer;

begin
  // convert the desired color format
  pColor.ColorValue := ByteSwapColor(maskColor); // need to convert format
  // check presence of main input file
  if (NOT FileExists(Bitmap_FileName)) then begin
    MessageShow(format('File not found: %s',[ExtractFileName(Bitmap_FileName)]));
    Exit;
  end else begin
    // keep the original file by making a copy
    Bitmap_Mask_FileName := copy(Bitmap_FileName,1,pos('.bmp',Bitmap_FileName)-1)+'_orig.bmp';
    CopyFile(PChar(Bitmap_FileName),PChar(Bitmap_Mask_FileName),true); // overwrite
  end;
  // check presence of 8 bit mask input file
  if ((Bitmap_8_FileName <> '') AND (NOT FileExists(Bitmap_8_FileName))) then begin
    MessageShow(format('File not found: %s',[ExtractFileName(Bitmap_8_FileName)]));
    Exit;
  end;
  begin
    // read header and check 8 bit mask input file
    AssignFile(Bitmap_8_File,Bitmap_8_FileName);
    Reset(Bitmap_8_File);
//    seek(Bitmap_8_File,0);
    BlockRead(Bitmap_8_File,Bitmap_Hdr_8,sizeof(Bitmap_Hdr));
    with Bitmap_Hdr_8 do begin
      // first confirm it is bmp and 8 bit color
      if ((bH.bSignature <> $4D42) OR              // BitmapSignature
          (bDib.bColorBits <> 8)) then begin // 8 bit color
        MessageShow('Error: Not 8 bit color bitmap');
        Close(Bitmap_8_File);
        Exit;
      end;
    end;
    // read the palette for the 8 bit file to access colors
    BlockRead(Bitmap_8_File,Palette_8mask,sizeof(BMP_8bit_ColorTable));
    // seek(Bitmap_8_File,Bitmap_Hdr_8.bH.bPixelArrayOffset); // need to skip over color table

    MessageShow('Masking bit bitmap with 8 bitmap...');

    // read header and check main input file
    AssignFile(Bitmap_File,Bitmap_Mask_FileName);
    Reset(Bitmap_File);
//    seek(Bitmap_File,0);
    BlockRead(Bitmap_File,Bitmap_Hdr,sizeof(Bitmap_Hdr));
    with Bitmap_Hdr do begin
      // check for bitmap signature
      if (bH.bSignature <> $4D42) then begin
        MessageShow('Error: not bitmap');
        Close(Bitmap_File);
        exit;
      end;
      // check for matching size
      if (Bitmap_Hdr_8.bDib.bWidth <> bDib.bWidth) then begin
        MessageShow('Error: bitmap size mismatch');
        Close(Bitmap_File);
        exit;
      end else begin
        // prepare new file for output
        AssignFile(Bitmap_Masked_File,Bitmap_FileName);
        Rewrite(Bitmap_Masked_File);

      // check for 8 bit or 24 bit color
      case (bDib.bColorBits) of
        8: begin
          // copy the header as is
          BlockWrite(Bitmap_Masked_File,Bitmap_Hdr, sizeof(Bitmap_Hdr));
          // read the palette for the 8 bit main file to access colors
          BlockRead(Bitmap_File,Palette_8,sizeof(BMP_8bit_ColorTable));
          // seek(Bitmap_Masked_File,Bitmap_Hdr_8.bH.bPixelArrayOffset); // need to skip over color table
          // match the color and get index
          PaletteIndex := 0; // assume at first and will be default if no match
          for i := 0 to (bDib.bPaletteColors-1) do begin
//            if (ColorConvert(Palette_8[i])= pColor) then
            if (CompareMem(@Palette_8[i],@pColor.cRGB,3)) then begin
              PaletteIndex := i;
              break;
            end;
          end;
          // copy the palette as is
          BlockWrite(Bitmap_Masked_File,Palette_8,sizeof(BMP_8bit_ColorTable));
        end;
        24: begin
          // copy the header as is
          BlockWrite(Bitmap_Masked_File,Bitmap_Hdr, sizeof(Bitmap_Hdr));
        end;
        else begin
          MessageShow('Error: Not 8 or 24 bit color bitmap');
          Close(Bitmap_File);
          Close(Bitmap_Masked_File);
          Exit;
        end;
      end;

      // now read pixels, and overwrite when needed
      try
        ProgressBar_Status.Max := bDib.bHeight;
        P8mask := AllocMem(bDib.bWidth * Color8Size);  // one row at a time
        case (bDib.bColorBits) of
          8: begin
            P8 := AllocMem(bDib.bWidth * Color8Size);  // one row at a time
            for i := 0 to bDib.bHeight-1 do begin
              BlockRead(Bitmap_8_File,P8mask^,bDib.bWidth * sizeof(Byte));
              BlockRead(Bitmap_File,P8^,bDib.bWidth * sizeof(Byte));
              for j := 0 to bDib.bWidth-1 do begin
                if (Palette_8mask[P8mask^[j]].rgbRed <> 255) then begin
                  P8^[j] := PaletteIndex;
                end;
              end;
              BlockWrite(Bitmap_Masked_File,P8^,bDib.bWidth * sizeof(Byte));
            end;
            ProgressBar_Status.StepIt;
//          Application.ProcessMessages;
          end;
          24: begin
            P24 := AllocMem(bDib.bWidth * Color24Size);  // one row at a time
            for i := 0 to bDib.bHeight-1 do begin
              BlockRead(Bitmap_8_File,P8mask^,bDib.bWidth * sizeof(Byte));
              BlockRead(Bitmap_File,P24^,bDib.bWidth * sizeof(TRGBTriple));
              for j := 0 to bDib.bWidth-1 do begin
                if (Palette_8mask[P8mask^[j]].rgbRed <> 255) then begin
                  P24^[j] := pColor.cRGB;
                end;
              end;
              BlockWrite(Bitmap_Masked_File,P24^,bDib.bWidth * sizeof(TRGBTriple));
            end;
            ProgressBar_Status.StepIt;
//          Application.ProcessMessages;
          end;
          else begin
          end;
        end;
        finally
          case (bDib.bColorBits) of
            8: begin
          freemem(P8);
            end;
            24: begin
          freemem(P24);
            end;
            else begin
            end;
          end;
          freemem(P8mask);
        end;
      end;
    end;

    Close(Bitmap_Masked_File);
    Close(Bitmap_File);
    Close(Bitmap_8_File);
    MessageShow('Bitmap masked.');
    ProgressBar_Status.Position := 0;
  end;
end;

// rotate 180 deg by flipping vertically and flipping horizontally
//----------------------------------------------------------------------------
procedure Rotate_180(Tile : Tbitmap);
var
  i : integer;
  Pixel_ByteSize : integer;
  Row_Size : integer;
  P_Save : PByteArray;
  P_Row : PByteArray;

  //--------------------------------------------------------------------------
  procedure Reverse(Row : PByteArray; Group : integer);
  var
    j,k : integer;
    temp : byte;
  begin
    for j := 0 to (Tile.Width div 2)-1 do begin
      for k := 0 to Group-1 do begin
        Temp := Row[j*Group+k];
        Row[j*Group+k] := Row[(Tile.Width-1-j)*Group+k];
        Row[(Tile.Width-1-j)*Group+k] := Temp;
      end;
    end;
  end;

begin
 with Tile do begin
  MessageShow(format('Tile %d %d',[Tile.Width, Tile.Height]));
  case Tile.PixelFormat of
    pf8bit : begin
      Pixel_ByteSize := 1;
    end;
    pf16bit : begin
      Pixel_ByteSize := 2;
    end;
    pf24bit : begin
      Pixel_ByteSize := 3;
    end;
    pf32bit : begin
      Pixel_ByteSize := 4;
    end;
    else begin
     exit;
    end;
  end;
  MessageShow(format('pixel size %d',[i]));
  Row_Size := Tile.Width * Pixel_ByteSize;
  GetMem(P_Save, Row_Size); // one row at a time
  GetMem(P_Row, Row_Size); // one row at a time
  // flip vertically
  for i := 0 to (Tile.Height div 2)-1 do begin
    CopyMemory(P_Save,Tile.ScanLine[i],Row_Size);
    Reverse(P_Save,Pixel_ByteSize);
    CopyMemory(Tile.ScanLine[i],Tile.ScanLine[Tile.Height-1-i],Row_Size);
    Reverse(Tile.ScanLine[i],Pixel_ByteSize);
    CopyMemory(Tile.ScanLine[Tile.Height-1-i],P_Save,Row_Size);
  end;
  FreeMem(P_Row);
  FreeMem(P_Save);
 end;
end;

{----------------------------------------------------------------------------}
procedure WriteBMP24Header(FileName : String);
const
  ZeroByte : byte = 0;

begin
  AssignFile(BitmapFile,FileName);
  if (FileExists(FileName)) then begin
//    MessageShow('Updating BMP header');
//    reset(BitmapFile);
  end else begin
//    MessageShow('Writing BMP header');
//    rewrite(BitmapFile);
  end;
    rewrite(BitmapFile);  // ALWAYS start new file !
//  seek(BitmapFile,0);
  BlockWrite(BitmapFile,xBitmapHeader_24bitColor,sizeof(xBitmapHeader_24bitColor));
  Close(BitmapFile);
end;

{----------------------------------------------------------------------------}
Procedure ForceBMP24size(FileName : string);
const
  ZeroByte : byte = 0;
begin
  AssignFile(BitmapFile,FileName);
  if (NOT FileExists(FileName)) then begin
    MessageShow('BMP file not found');
    beep; Exit;
  end;
  reset(BitmapFile);
  BlockRead(BitmapFile,xBitmapHeader_24bitColor,sizeof(xBitmapHeader_24bitColor));
  // write dummy last value to force filesize
  with xBitmapHeader_24bitColor do begin
    seek(BitmapFile,sizeof(xBitmapHeader_24bitColor)+
      (bDib.bWidth*bDib.bHeight*xColor24Size div 8)-1);
  end;
  Write(BitmapFile,ZeroByte);
  Close(BitmapFile);
end;

// initialversion modified for vertical crop only as first step
{----------------------------------------------------------------------------}
procedure xxxMerge_BMP24_File(Offset_X, Offset_Y, Min_X, Max_X, Min_Y, Max_Y : LongInt;
                           FilePath,Filename,
                           FilePath_a,Filename_a : string);
var
  BMP_File : File of Byte;
  BMP_File_a : File of Byte;
  P : PByteArray;
  i, i_Min, i_Max : integer;
  FileIndex : LongInt;
  cWidth, cHeight : integer;
  gWidth, gHeight : integer;
//  cColorBits : integer;
  BMP_Header : BMP_V1_Header;

begin
  if (NOT FileExists(FilePath+'\'+Filename)) then begin
    MessageShow('BMP file not found');
    Beep; Exit;
  end;
  AssignFile(BMP_File,FilePath+'\'+Filename);
  Reset(BMP_File);
  BlockRead(BMP_File,BMP_Header,sizeof(BMP_Header));
  // keep width and height and color type
  cWidth := BMP_Header.bDib.bWidth;
  cHeight := BMP_Header.bDib.bHeight;
//  cColorBits := BMP_Header.bDib.bColorBits; // must be 24 !

  AssignFile(BMP_File_a,FilePath_a+'\'+Filename_a);
  Reset(BMP_File_a);
  BlockRead(BMP_File_a,BMP_Header,sizeof(BMP_Header));

  // only allow 24 bit format for now
  with BMP_Header do begin
     case BMP_Header.bDib.bColorBits of
       24: begin
         gWidth := bDib.bWidth;
         gHeight := bDib.bHeight;
       end;
       else begin
         MessageShow('BMP file not 24 bit format');
         Beep; Exit;
       end;
    end;

    // need a buffer
    P := AllocMem(bDib.bWidth * Color24Size);
    // could be 24 or 32, assume 32 as max size.
//    P_a := AllocMem(bDib.bWidth * Color32Size); // one row at a time

    // calculate limits
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

    ProgressBar_Status.Max := gHeight;
//    for i := 0 to gHeight-1 do begin
    for i := i_Min to i_Max-1 do begin
      FileIndex := sizeof(BMP_Header) +
        ((i) * gWidth) * Color24Size;
      seek(BMP_File_a,FileIndex);
      BlockRead(BMP_File_a,P^,gWidth * Color24Size);
      FileIndex := sizeof(BMP_Header) +
//        ((i + Offset_Y) * cWidth) * Color24Size +
        ((i + Offset_Y - Min_Y) * cWidth) * Color24Size +
        (cWidth-(-Offset_X)-gWidth) * Color24Size;
      seek(BMP_File,FileIndex);
      BlockWrite(BMP_File,P^,gWidth * Color24Size);
      ProgressBar_Status.StepIt;
      Application.ProcessMessages;
    end;
    Freemem(P);
  end;

  CloseFile(BMP_File_a);
  CloseFile(BMP_File);
  ProgressBar_Status.Position := 0;
end;

{----------------------------------------------------------------------------}
procedure Merge_BMP24_File(Offset_X, Offset_Y, Min_X, Max_X, Min_Y, Max_Y : LongInt;
                           FilePath,Filename,
                           FilePath_a,Filename_a : string);
var
  BMP_File : File of Byte;
  BMP_File_a : File of Byte;
  P : PByteArray;
  i, j, i_Min, i_Max : integer;
  FileIndex : LongInt;
  cWidth, cHeight : integer;
  gWidth, gHeight : integer;
  BMP_Header : BMP_V1_Header;
  j_DeltaL, j_DeltaR, j_Index, j_Width : integer;
  Flag_32 : boolean;
  Color_Size : integer;
  pColor : ColorConvert;

begin
  if (NOT FileExists(FilePath+'\'+Filename)) then begin
    MessageShow('BMP file not found');
    Beep; Exit;
  end;
  if (NOT FileExists(FilePath_a+'\'+Filename_a)) then begin
    MessageShow('Warning: '+Filename_a+' file not found');
    Beep; Exit;
  end;

  AssignFile(BMP_File,FilePath+'\'+Filename);
  Reset(BMP_File);
  BlockRead(BMP_File,BMP_Header,sizeof(BMP_Header));
  // keep width and height and color type
  cWidth := BMP_Header.bDib.bWidth;
  cHeight := BMP_Header.bDib.bHeight;

  AssignFile(BMP_File_a,FilePath_a+'\'+Filename_a);
  Reset(BMP_File_a);
  BlockRead(BMP_File_a,BMP_Header,sizeof(BMP_Header));

  // only allow 32 and 24 bit format for now
  with BMP_Header do begin
    gWidth := bDib.bWidth;
    gHeight := bDib.bHeight;
    case BMP_Header.bDib.bColorBits of
      32: begin
        Flag_32 := True;
        Color_Size := Color32Size;
      end;
      24: begin
        Flag_32 := False;
        Color_Size := Color24Size;
      end;
      else begin
        MessageShow('BMP file not 32 or 24 bit format');
        Beep; Exit;
      end;
    end;
  end;

//  if (NOT ValidateNotCompressed(BMP_Header)) then begin
//    MessageShow('Error: Compression not allowed');
//    Beep; Exit;
//  end;
  seek(BMP_File_a,BMP_Header.bH.bPixelArrayOffset); // always do in case header is larger than standard

  try
    // need a buffer
//    P := AllocMem(bDib.bWidth * Color24Size);
//    P := AllocMem(gWidth * Color24Size);
    // could be 24 or 32, assume 32 as max size.
    P := AllocMem(gWidth * Color_Size);
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
//      j_Width := j_Width - -j_DeltaR; // bug
      j_Width := j_Width - j_DeltaR;
    end;

    ProgressBar_Status.Max := gHeight;
//    for i := 0 to gHeight-1 do begin
    for i := i_Min to i_Max-1 do begin
      // get input data    // do seek only once and then just sequential ?
      FileIndex := sizeof(BMP_Header) +
        ((i) * gWidth) * Color_Size;
      seek(BMP_File_a,FileIndex);
      BlockRead(BMP_File_a,P^,gWidth * Color_Size);
      if (Flag_32) then begin // need to convert from 32 to 24 bit color
        for j := 0 to gWidth-1 do begin
          pColor.cRGBA := pRGBAlphaArray(P)^[j];
          pRGBArray(P)^[j] := pColor.cRGB;
        end;
      end;
      // write output data
      FileIndex := sizeof(BMP_Header) +
        ((i + Offset_Y - Min_Y) * cWidth) * Color24Size +
        j_Index * Color24Size;
      seek(BMP_File,FileIndex);
      BlockWrite(BMP_File,P^[j_DeltaL * Color24Size],j_Width * Color24Size);
      ProgressBar_Status.StepIt;
      Application.ProcessMessages;
    end;
  Finally
    Freemem(P);
    CloseFile(BMP_File_a);
    CloseFile(BMP_File);
    ProgressBar_Status.Position := 0;
  end;
end;

{
//---------------------------------------------------------------------------
procedure DDS_TO_BMP(aBmp : TBitmap);
var
  tBufr : TBitmap; // temp bitmap
begin
  tBufr := TBitmap.Create;
  BMP_CopyMe(tBufr,aBmp);

  WriteBitMapToFile(tBufr,'TEST.BMP');

  tBufr.Free;
end;
}
{
// open a bitmap file and split into 4x4 pieces
//---------------------------------------------------------------------------
procedure split_4x4(FilePath, FileName : string);
begin
  // bitmap row from bottom, column from left
  for y := 0 to Rows-1 do begin // do all rows
    if Row = 0 then begin
      open first 4 files
    end else
      if (Row mod quarterSize = 0) then begin
        close first 4 files
        open next 4 files
      end;
    end;
      BlockRead(Bitmap_File,P^, one row);
      BlockWrite(Bitmap_File_0,P^[0], quarter);
      BlockWrite(Bitmap_File_1,P^[0+n], quarter);
      BlockWrite(Bitmap_File_2,P^[0+2n], quarter);
      BlockWrite(Bitmap_File_3,P^[0+3n], quarter);
  end;
  close last 4 files
end;
}

{
// open 16 bitmap files and merge 4x4 pieces into one
// all 8 bit or all 24 bit bitmaps?
//---------------------------------------------------------------------------
function Bitmap_Merge_4x4(MergeFileName : string) : boolean;
  ErrorCode : integer;
  FilePath : string;
  FileName : string;
  y :integer;
  Column, Row : integer;
  whSize : integer; // size of width and height of merged file

  outFile : file of byte;
  inFile_0 : file of byte;
  inFile_1 : file of byte;
  inFile_2 : file of byte;
  inFile_3 : file of byte;

procedure OpenFiles(iRow : integer);
begin
  Assign(inFile_0,format('%s\%2.2d%2.2d.bmp'.[FilePath,Column*4+0,Row*4+iRow);
  Reset(inFile_0);
  read header and check proper type and size whSize/4
  Assign(inFile_0,format('%s\%2.2d%2.2d.bmp'.[FilePath,Column*4+1,Row*4+iRow);
  Reset(inFile_1);
  read header and check proper type and size whSize/4
  Assign(inFile_0,format('%s\%2.2d%2.2d.bmp'.[FilePath,Column*4+2,Row*4+iRow);
  Reset(inFile_2);
  read header and check proper type and size whSize/4
  Assign(inFile_0,format('%s\%2.2d%2.2d.bmp'.[FilePath,Column*4+3,Row*4+iRow);
  Reset(inFile_3);
  read header and check proper type and size whSize/4
end;

procedure CloseFiles;
begin
  Close(inFile_0);
  Close(inFile_1);
  Close(inFile_2);
  Close(inFile_3);
end;

begin
  result := false; // assume false to start
  // extract FilePath
  FilePath := ExtractFileDir(MergeFilename);
  FileName := ExtractFileName(MergeFilename);
  // extract Tile Column, Row
  Val(copy(FileName,1,2),Column,ErrorCode);
  // if errorcode or not in range -> error
  if (ErrorCode <> 0) then begin
    Exit;
  end;
  Val(copy(FileName,3,2),Row,ErrorCode);
  // if errorcode or not in range -> error
  if (ErrorCode <> 0) then begin
    Exit;
  end;

  // determine whSize
  whSize := ???;

  // determine 8 or 24 bit color
  Color8 or 24 := ???;

  //open merged file
  assign(outFile,format('%s\%2.2d%2.2d.bmp'.[FilePath,Column,Row);
  Rewrite(outFile);
  write a header with proper type and whSize

  try
    //create a transfer buffer, one full row
    alloc(P,whSize * sizeof(Color8 or 24))

    // bitmap row from bottom, column from left
    for y := 0 to Rows-1 do begin // do all rows
      if (y MOD (whSize div 4) = 0) then begin
        // close unless first time
        if (y <> 0) then
          //close 4 files
          CloseFiles;
        end;
        //open 4 files
        OpenFiles(y div (whSize div 4));
      end;
      BlockRead(Bitmap_File_0,P^[0*(whSize div 4)], (whSize div 4) * size of(Color8 or 24));
      BlockRead(Bitmap_File_1,P^[1*(whSize div 4)], (whSize div 4) * size of(Color8 or 24));
      BlockRead(Bitmap_File_2,P^[2*(whSize div 4)], (whSize div 4) * size of(Color8 or 24));
      BlockRead(Bitmap_File_3,P^[3*(whSize div 4)], (whSize div 4) * size of(Color8 or 24));
      BlockWrite(Bitmap_File,P^, whSize * size of(Color8 or 24));
    end;
  finally
    freemem(P);
  end;
  CloseFiles;
  Close(outFile);
  result := true;
end;
}
{----------------------------------------------------------------------------}
begin { Initialization }
  BitmapSuccess := false;
  Memo_Message := nil;
//  SetGradientColors([-300,0,100,700,1500],[clblack,clBlue,clGreen,clRed,clWhite]);
  SetGradientColors([-300,0,100,700,1500],[clblack,clBlue,clGreen,$0020C0E0,clWhite]);
end.

{--- End of File ------------------------------------------------------------}

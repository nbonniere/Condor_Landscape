{
 * u_TIFF.pas
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
UNIT u_TIFF;

{----------------------------------------------------------------------------
basic functions to write uncompressed TIFF files

NOTE: little-endian values is used

----------------------------------------------------------------------------}

{============================================================================}
INTERFACE

uses Windows, StdCtrls, Graphics, comctrls, SysUtils, extctrls;

type    // For scanline simplification
  TRGBArray = ARRAY[0..32767] OF TRGBTriple;
  pRGBArray = ^TRGBArray;

  TRGBAlphaArray = ARRAY[0..32767] OF TRGBQuad;
  pRGBAlphaArray = ^TRGBAlphaArray;

  TAlphaArray = ARRAY[0..32767] OF Byte;
  pAlphaArray = ^TAlphaArray;

type
  LongIntConvert = packed record  // size of structure is not correct and cannot be used as a group !
//  LongIntConvert = record
    case byte of
//    case cardinal of
      0 : ( LongValue    : longword );
      1 : ( LongIntValue : longint );
      2 : ( ByteValue    : array[0..3] of byte );
      3 : ( WordValue    : array[0..1] of Word );
      4 : ( IntegerValue : array[0..1] of Integer );
    end;

  WordConvert = packed record
    case byte of
      0 : ( WordValue : word );
      1 : ( IntegerValue : integer );
      2 : ( ByteValue : array[0..1] of byte );
    end;

  TIFF_Header_Type = packed record // packed for blockread/blockwrite alignment
    tSignature    : word;
    tRevision_0   : byte;
    tRevision_1   : byte;
    tIFD_Offset   : longword;
  end;

  TIFF_IFD_Entry_Type = packed record // packed for blockread/blockwrite alignment
    IFD_Tag           : word;
    IFD_Type          : word;
    IFD_NumValues     : longword;
//    IFD_ValueOrOffset : LongIntConvert;  // overhead creates a problem
    IFD_ValueOrOffset : longword;
  end;

  TIFF_IFD_Type = packed record // packed for blockread/blockwrite alignment
    iNumEntries     : word;
    iEntries        : array of TIFF_Ifd_Entry_Type;  // dynamic var overhead creates a problem
    iNextIFD_Offset : longword;
  end;

  TIFF_IFD_Values_Type = packed record // packed for blockread/blockwrite alignment
    iValues : array of byte;              // dynamic var overhead creates a problem
  end;

const
  Tiff_BigEndian_Signature {: word} = $4D4D; // 'MM'
  Tiff_LittleEndian_Signature {: word} = $4949; // 'II'
  Tiff_Revision_0 {: byte} = $2A;
  Tiff_Revision_1 {: byte} = 0;

  IFD_Type_Byte = 1;
  IFD_Type_Word = 3;
  IFD_Type_Long = 4;

  IFD_Tag_00FE = $00FE; // New subfile type (optional)
  IFD_Tag_0100 = $0100; // Image width
  IFD_Tag_0101 = $0101; // Image Height
  IFD_Tag_0102 = $0102; // bits per sample
  IFD_Tag_0103 = $0103; // compression method
  IFD_Tag_0106 = $0106; // photometric interpretation
  IFD_Tag_0111 = $0111; // strip offsets - pointers to data blocks (one pointer if one strip)
  IFD_Tag_0112 = $0112; // orientation (optional - default -> 1 - top-left)
  IFD_Tag_0115 = $0115; // components per pixel
  IFD_Tag_0116 = $0116; // rows per strip (image height if one strip)
  IFD_Tag_0117 = $0117; // strip byte counts (width*height*bytes per pixel)
  IFD_Tag_0118 = $0118; // minimum sample value (optional)
  IFD_Tag_0119 = $0119; // maximum sample value (optional)
  IFD_Tag_011C = $011C; // planar configuration (optional - default -> 1)
  IFD_Tag_0152 = $0152; // extra components (alpha)
  IFD_Tag_0153 = $0153; // sample format (optional - default 1 -> unsigned integers)

var
  Memo_Message : TMemo;  // external TMemo for messages
  ProgressBar_Status : TProgressBar;

Procedure BMP_24_To_TIF_32_WithAlpha(Bitmap_24_FileName,Bitmap_8_FileName,Tiff_32_FileName : string);
Procedure Save_24bit_Image_To_8bit_Tiff(Image: TImage; TIF_FileName : string);

{============================================================================}
IMPLEMENTATION

uses Forms, FileCtrl,
  u_BMP;

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
Procedure BMP_24_To_TIF_32_WithAlpha(Bitmap_24_FileName,Bitmap_8_FileName,Tiff_32_FileName : string);
const
  tif_Header : TIFF_Header_Type = (
    tSignature    :   Tiff_LittleEndian_Signature;
    tRevision_0   :   Tiff_Revision_0;
    tRevision_1   :   Tiff_Revision_1;
    tIFD_Offset   :   0;
  );
//  TestValue : array[0..4-1] of byte = ($AA,$BB,$CC,$ff);
//  ColorSize = 3; // RGB
  ColorSize = 4;  // RGBA
  Color8ByteSize = 1; // 1 byte

var
//  i, j, k : integer;
  i, j : integer;
  Temp : byte;

  tif_IFD : TIFF_IFD_Type;
  tif_IFD_Values : TIFF_IFD_Values_Type;
  NumEntries : word;
  NumValues : word;
  BitsPerSampleIndex : integer;

  Width : longword;
  Height : longword;
  SizeOf_DataBlock : longword;
  SizeOf_Tif_IFD : longword;

  Tiff_32_File   : File of byte;
  Bitmap_24_File : File of byte;
  Bitmap_8_File  : File of byte;
  Bitmap_Hdr_24  : BMP_V1_Header;
  Bitmap_Hdr_8   : BMP_V1_Header;

  pColor : ColorConvert;
  P8  : pAlphaArray;
  P24 : pRGBArray;
  P32 : pRGBAlphaArray;
  index_8  : longword;
  index_24 : longword;
  Palette_8 : BMP_8bit_ColorTable;

begin
  // error checking
  if (NOT FileExists(Bitmap_24_FileName)) then begin
    MessageShow('Bitmap file not found');
    Exit;
  end;
  if ((Bitmap_8_FileName <> '') AND (NOT FileExists(Bitmap_8_FileName))) then begin
    MessageShow('Alpha Bitmap file not found');
    Exit;
  end;

  AssignFile(Bitmap_8_File,Bitmap_8_FileName);
  Reset(Bitmap_8_File);
//  seek(Bitmap_8_File,0);
  BlockRead(Bitmap_8_File,Bitmap_Hdr_8,sizeof(Bitmap_Hdr_8));
  with Bitmap_Hdr_8 do begin
    // first confirm it is bmp and 8 bit color
    if ((bH.bSignature <> $4D42) OR              // BitmapSignature
        (bDib.bColorBits <> 8)) then begin // 8 bit color
      MessageShow('Error: Not 8 bit color bitmap');
      Close(Bitmap_8_File);
      Exit;
    end;
  end;

  AssignFile(Bitmap_24_File,Bitmap_24_FileName);
  Reset(Bitmap_24_File);
//  seek(Bitmap_24_File,0);
  BlockRead(Bitmap_24_File,Bitmap_Hdr_24,sizeof(Bitmap_Hdr_24));
  with Bitmap_Hdr_24 do begin
    // first confirm it is bmp and 24 bit color
    if ((bH.bSignature <> $4D42) OR              // BitmapSignature
        (bDib.bColorBits <> 24)) then begin // 24 bit color
      MessageShow('Error: Not 24 bit color bitmap');
      Close(Bitmap_24_File);
      Close(Bitmap_8_File);
      Exit;
    end else begin
      if (Bitmap_Hdr_8.bDib.bWidth <> bDib.bWidth) then begin
        MessageShow('Error: bitmap size mismatch');
        Close(Bitmap_24_File);
        Close(Bitmap_8_File);
        exit;
      end;
    end;
  end;

  // prepare TIFF file information
  Width := Bitmap_Hdr_24.bDib.bWidth;
  Height := Bitmap_Hdr_24.bDib.bHeight;
  // with no compression
  SizeOf_DataBlock := Width*Height*ColorSize;

  NumValues := 0;

  NumEntries := 1;
  tif_IFD.iNumEntries := NumEntries;
  setlength(tif_IFD.iEntries,NumEntries);
  with tif_IFD.iEntries[NumEntries-1] do begin
    IFD_Tag := IFD_Tag_0100;      // image width
    IFD_Type := IFD_Type_Long;    // or IFD_Type_Word
    IFD_NumValues := 1;
//    IFD_ValueOrOffset.LongValue := Width;  // overhead problem with blockWrite
    IFD_ValueOrOffset := Width;
  end;

  inc(NumEntries);
  tif_IFD.iNumEntries := NumEntries;
  setlength(tif_IFD.iEntries,NumEntries);
  with tif_IFD.iEntries[NumEntries-1] do begin
    IFD_Tag := IFD_Tag_0101;      // image height
    IFD_Type := IFD_Type_Long;    // or IFD_Type_Word
    IFD_NumValues := 1;
//    IFD_ValueOrOffset.LongValue := Height;
    IFD_ValueOrOffset := Height;
  end;

  inc(NumEntries);
  tif_IFD.iNumEntries := NumEntries;
  setlength(tif_IFD.iEntries,NumEntries);
  with tif_IFD.iEntries[NumEntries-1] do begin
    IFD_Tag := IFD_Tag_0102;      // bits per sample
    IFD_Type := IFD_Type_Word;
    IFD_NumValues := ColorSize;
//    IFD_ValueOrOffset.WordValue := Numvalues; // pointer
    IFD_ValueOrOffset := Numvalues; // current pointer offset
    //  words are 2 bytes
    setlength(tif_IFD_Values.ivalues,Numvalues+2*IFD_NumValues);
    for i := 0 to IFD_NumValues-1 do begin
      // need to write as Word, little-endian
      tif_IFD_Values.ivalues[Numvalues+2*i]   := 8;
      tif_IFD_Values.ivalues[Numvalues+2*i+1] := 0;
    end;
    INC(NumValues, 2*IFD_NumValues);
  end;
  BitsPerSampleIndex := NumEntries-1;

  inc(NumEntries);
  tif_IFD.iNumEntries := NumEntries;
  setlength(tif_IFD.iEntries,NumEntries);
  with tif_IFD.iEntries[NumEntries-1] do begin
    IFD_Tag := IFD_Tag_0103;      // compression type
    IFD_Type := IFD_Type_Word;
    IFD_NumValues := 1;
//    IFD_ValueOrOffset.WordValue := 1; // no compression
    IFD_ValueOrOffset := 1; // no compression
  end;

  inc(NumEntries);
  tif_IFD.iNumEntries := NumEntries;
  setlength(tif_IFD.iEntries,NumEntries);
  with tif_IFD.iEntries[NumEntries-1] do begin
    IFD_Tag := IFD_Tag_0106;      // photometric interpretation
    IFD_Type := IFD_Type_Word;
    IFD_NumValues := 1;
//    IFD_ValueOrOffset.WordValue := 2; // RGB
    IFD_ValueOrOffset := 2; // RGB
  end;

  inc(NumEntries);
  tif_IFD.iNumEntries := NumEntries;
  setlength(tif_IFD.iEntries,NumEntries);
  with tif_IFD.iEntries[NumEntries-1] do begin
    IFD_Tag := IFD_Tag_0111;      // strip offsets - with one strip, the
    IFD_Type := IFD_Type_Long;    // or IFD_Type_Word // offset to the data block
    IFD_NumValues := 1;
//    IFD_ValueOrOffset.LongValue := sizeof(TIFF_Header_Type); // skip over header
    IFD_ValueOrOffset := sizeof(TIFF_Header_Type); // skip over header
  end;
{  // not recognized by Win10 nor GDAL_translate
  inc(NumEntries);
  tif_IFD.iNumEntries := NumEntries;
  setlength(tif_IFD.iEntries,NumEntries);
  with tif_IFD.iEntries[NumEntries-1] do begin
    IFD_Tag := IFD_Tag_0112;      // orientation
    IFD_Type := IFD_Type_Word;
    IFD_NumValues := 1;
//    IFD_ValueOrOffset.LongValue := 1;
//    IFD_ValueOrOffset := 1;    // first pixel - top left
    IFD_ValueOrOffset := 4;    // first pixel - bottom left
  end;
}
  inc(NumEntries);
  tif_IFD.iNumEntries := NumEntries;
  setlength(tif_IFD.iEntries,NumEntries);
  with tif_IFD.iEntries[NumEntries-1] do begin
    IFD_Tag := IFD_Tag_0115;      // components per pixel
    IFD_Type := IFD_Type_Word;
    IFD_NumValues := 1;
//    IFD_ValueOrOffset.WordValue := 3;      // RGB
//    IFD_ValueOrOffset.WordValue := 4;      // RGBA
    IFD_ValueOrOffset := ColorSize;
  end;

  inc(NumEntries);
  tif_IFD.iNumEntries := NumEntries;
  setlength(tif_IFD.iEntries,NumEntries);
  with tif_IFD.iEntries[NumEntries-1] do begin
    IFD_Tag := IFD_Tag_0116;      // rows per strip (image height if only one strip)
    IFD_Type := IFD_Type_Long;    // or IFD_Type_Word
    IFD_NumValues := 1;
//    IFD_ValueOrOffset.LongValue := Height;
    IFD_ValueOrOffset := Height;
  end;

  inc(NumEntries);
  tif_IFD.iNumEntries := NumEntries;
  setlength(tif_IFD.iEntries,NumEntries);
  with tif_IFD.iEntries[NumEntries-1] do begin
    IFD_Tag := IFD_Tag_0117;      // strip byte counts (width*height*bytes per pixel)
    IFD_Type := IFD_Type_Long;    // or IFD_Type_Word
    IFD_NumValues := 1;
//    IFD_ValueOrOffset.LongValue := SizeOf_DataBlock;
    IFD_ValueOrOffset := SizeOf_DataBlock;
  end;

   if (ColorSize = 4) then begin
    inc(NumEntries);
    tif_IFD.iNumEntries := NumEntries;
    setlength(tif_IFD.iEntries,NumEntries);
    with tif_IFD.iEntries[NumEntries-1] do begin
      IFD_Tag := IFD_Tag_0152;      // extra component (alpha)
      IFD_Type := IFD_Type_Word;
      IFD_NumValues := 1;
//      IFD_ValueOrOffset.LongValue := 1;
//      IFD_ValueOrOffset.LongValue := 2;
      IFD_ValueOrOffset := 2;
    end;
  end;

  AssignFile(Tiff_32_File,Tiff_32_FileName);
  Rewrite(Tiff_32_File);

  // update pointer to IFD and write header
  tif_Header.tIFD_Offset :=
    sizeOf(TIFF_Header_Type) +  // skip over header;
    sizeOf_DataBlock;           // skip over datablock;
  BlockWrite(Tiff_32_File,tif_Header,
    sizeOf(TIFF_Header_Type));
{
  // write data block -> one 'strip' only
  // for testing
  for i := 0 to height-1 do begin
    for j := 0 to width-1 do begin
      for k := 0 to ColorSize-1 do begin
        write(Tiff_32_File,TestValue[k]);
      end;
    end;
  end;
}
        // read the palette for the 8 bit file to access colors
        BlockRead(Bitmap_8_File,Palette_8,sizeof(BMP_8bit_ColorTable));

        // now read 24 bit pixels, and alpha and write 32 bit pixels
        try
          P8  := AllocMem(Width * Color8ByteSize);  // one row at a time
          P24 := AllocMem(Width * Color24Size); // one row at a time
          P32 := AllocMem(Width * Color32Size); // one row at a time
          if (assigned(ProgressBar_Status)) then ProgressBar_Status.Max := Height;
          for i := 0 to Height-1 do begin
            // need to flip data vertically
            index_24 := Bitmap_Hdr_24.bH.bPixelArrayOffset + (Height-1-i)*(Width * Color24Size);
            index_8 := Bitmap_Hdr_8.bH.bPixelArrayOffset + (Height-1-i)*(Width * Color8ByteSize);
            seek(Bitmap_24_File,index_24);
            seek(Bitmap_8_File,index_8);
            BlockRead(Bitmap_24_File,P24^,Width * sizeof(TRGBTriple));
            BlockRead(Bitmap_8_File,P8^,Width * sizeof(Byte));
            for j := 0 to Width-1 do begin
              pColor.cRGB := P24^[j];
              // swap color bytes
              Temp := pColor.ByteValue[0];
              pColor.ByteValue[0] := pColor.ByteValue[2];
              pColor.ByteValue[2] := Temp;
              // pColor.cAlpha := P8^[j]; // read 8 bit palette entry instead ?
              pColor.cAlpha := Palette_8[P8^[j]].rgbRed; // read 8 bit palette entry instead
//     if pColor.cAlpha <> 255 then begin pColor.cAlpha := 127 end; // for testing alpha levels
         //     P32^[j] := TRGBQuad(ByteSwapColor(pColor.ColorValue));  // does not work ?
              P32^[j] := pColor.cRGBA;
            end;
            BlockWrite(Tiff_32_File,P32^,Width * sizeof(TRGBQuad));

            if (assigned(ProgressBar_Status)) then ProgressBar_Status.StepIt;
//            Application.ProcessMessages;
          end;
        finally
          freemem(P32);
          freemem(P24);
          freemem(P8);
        end;

  // update next IFD pointer
  with tif_IFD do begin
    iNextIFD_Offset := 0; // if last IFD, must be set to 0
    // update value pointers
    SizeOf_Tif_IFD := sizeOf(TIFF_IFD_Type) -
      {} 4 +  // dynamic var overhead
      sizeOf(TIFF_IFD_Entry_Type) * iNumEntries;
    iEntries[BitsPerSampleIndex].IFD_ValueOrOffset :=
      iEntries[2].IFD_ValueOrOffset +
      SizeOf(TIFF_Header_Type) +  // skip over header;
      SizeOf_DataBlock +          // skip over datablock;
      SizeOf_Tif_IFD;             // skip IFD
    // and write IFD
    // unfortunately cannot write as a block because of dynamic array of entries
    BlockWrite(Tiff_32_File,iNumEntries,
      {SizeOf(iNumEntries)} sizeOf(Word));
    for i := 0 to iNumEntries-1 do begin
      BlockWrite(Tiff_32_File,iEntries[i],
        sizeOf(TIFF_IFD_Entry_Type));
    end;
    BlockWrite(Tiff_32_File,iNextIFD_Offset,
      {SizeOf(iNextIFD_Offset)} sizeOf(LongWord));
  end;

  // write values
  BlockWrite(Tiff_32_File,tif_IFD_Values.iValues[0],
    NumValues);

  Close(Bitmap_8_File);
  Close(Bitmap_24_File);
  CloseFile(Tiff_32_File);
  MessageShow('TIFF file with alpha created.');
  if (assigned(ProgressBar_Status)) then ProgressBar_Status.Position := 0;
end;

// not 8 bit for now, but 24 bits - TBD
{----------------------------------------------------------------------------}
Procedure Save_24bit_Image_To_8bit_Tiff(Image: TImage; TIF_FileName : string);
const
  tif_Header : TIFF_Header_Type = (
    tSignature    :   Tiff_LittleEndian_Signature;
    tRevision_0   :   Tiff_Revision_0;
    tRevision_1   :   Tiff_Revision_1;
    tIFD_Offset   :   0;
  );
//  TestValue : array[0..4-1] of byte = ($AA,$BB,$CC,$ff);
  ColorSize = 3; // RGB
//  ColorSize = 4;  // RGBA
  Color8ByteSize = 1; // 1 byte

var
//  i, j, k : integer;
  i, j : integer;
  Temp : byte;

  tif_IFD : TIFF_IFD_Type;
  tif_IFD_Values : TIFF_IFD_Values_Type;
  NumEntries : word;
  NumValues : word;
  BitsPerSampleIndex : integer;

  Width : longword;
  Height : longword;
  SizeOf_DataBlock : longword;
  SizeOf_Tif_IFD : longword;

  Tiff_32_File   : File of byte;
  Bitmap_24_File : File of byte;
  Bitmap_8_File  : File of byte;
  Bitmap_Hdr_24  : BMP_V1_Header;
  Bitmap_Hdr_8   : BMP_V1_Header;

  pColor : ColorConvert;
  P8  : pAlphaArray;
  P24 : pRGBArray;
  P32 : pRGBAlphaArray;
  index_8  : longword;
  index_24 : longword;
  Palette_8 : BMP_8bit_ColorTable;

begin
  // error checking

  // prepare TIFF file information
  Width := Image.Picture.Width;
  Height := Image.Picture.Height;
  // with no compression
  SizeOf_DataBlock := Width*Height*ColorSize;

  NumValues := 0;

  NumEntries := 1;
  tif_IFD.iNumEntries := NumEntries;
  setlength(tif_IFD.iEntries,NumEntries);
  with tif_IFD.iEntries[NumEntries-1] do begin
    IFD_Tag := IFD_Tag_0100;      // image width
    IFD_Type := IFD_Type_Long;    // or IFD_Type_Word
    IFD_NumValues := 1;
//    IFD_ValueOrOffset.LongValue := Width;  // overhead problem with blockWrite
    IFD_ValueOrOffset := Width;
  end;

  inc(NumEntries);
  tif_IFD.iNumEntries := NumEntries;
  setlength(tif_IFD.iEntries,NumEntries);
  with tif_IFD.iEntries[NumEntries-1] do begin
    IFD_Tag := IFD_Tag_0101;      // image height
    IFD_Type := IFD_Type_Long;    // or IFD_Type_Word
    IFD_NumValues := 1;
//    IFD_ValueOrOffset.LongValue := Height;
    IFD_ValueOrOffset := Height;
  end;

  inc(NumEntries);
  tif_IFD.iNumEntries := NumEntries;
  setlength(tif_IFD.iEntries,NumEntries);
  with tif_IFD.iEntries[NumEntries-1] do begin
    IFD_Tag := IFD_Tag_0102;      // bits per sample
    IFD_Type := IFD_Type_Word;
    IFD_NumValues := ColorSize;
//    IFD_ValueOrOffset.WordValue := Numvalues; // pointer
    IFD_ValueOrOffset := Numvalues; // current pointer offset
    //  words are 2 bytes
    setlength(tif_IFD_Values.ivalues,Numvalues+2*IFD_NumValues);
    for i := 0 to IFD_NumValues-1 do begin
      // need to write as Word, little-endian
      tif_IFD_Values.ivalues[Numvalues+2*i]   := 8;
      tif_IFD_Values.ivalues[Numvalues+2*i+1] := 0;
    end;
    INC(NumValues, 2*IFD_NumValues);
  end;
  BitsPerSampleIndex := NumEntries-1;

  inc(NumEntries);
  tif_IFD.iNumEntries := NumEntries;
  setlength(tif_IFD.iEntries,NumEntries);
  with tif_IFD.iEntries[NumEntries-1] do begin
    IFD_Tag := IFD_Tag_0103;      // compression type
    IFD_Type := IFD_Type_Word;
    IFD_NumValues := 1;
//    IFD_ValueOrOffset.WordValue := 1; // no compression
    IFD_ValueOrOffset := 1; // no compression
  end;

  inc(NumEntries);
  tif_IFD.iNumEntries := NumEntries;
  setlength(tif_IFD.iEntries,NumEntries);
  with tif_IFD.iEntries[NumEntries-1] do begin
    IFD_Tag := IFD_Tag_0106;      // photometric interpretation
    IFD_Type := IFD_Type_Word;
    IFD_NumValues := 1;
//    IFD_ValueOrOffset.WordValue := 2; // RGB
    IFD_ValueOrOffset := 2; // RGB
  end;

  inc(NumEntries);
  tif_IFD.iNumEntries := NumEntries;
  setlength(tif_IFD.iEntries,NumEntries);
  with tif_IFD.iEntries[NumEntries-1] do begin
    IFD_Tag := IFD_Tag_0111;      // strip offsets - with one strip, the
    IFD_Type := IFD_Type_Long;    // or IFD_Type_Word // offset to the data block
    IFD_NumValues := 1;
//    IFD_ValueOrOffset.LongValue := sizeof(TIFF_Header_Type); // skip over header
    IFD_ValueOrOffset := sizeof(TIFF_Header_Type); // skip over header
  end;
{  // not recognized by Win10 nor GDAL_translate
  inc(NumEntries);
  tif_IFD.iNumEntries := NumEntries;
  setlength(tif_IFD.iEntries,NumEntries);
  with tif_IFD.iEntries[NumEntries-1] do begin
    IFD_Tag := IFD_Tag_0112;      // orientation
    IFD_Type := IFD_Type_Word;
    IFD_NumValues := 1;
//    IFD_ValueOrOffset.LongValue := 1;
//    IFD_ValueOrOffset := 1;    // first pixel - top left
    IFD_ValueOrOffset := 4;    // first pixel - bottom left
  end;
}
  inc(NumEntries);
  tif_IFD.iNumEntries := NumEntries;
  setlength(tif_IFD.iEntries,NumEntries);
  with tif_IFD.iEntries[NumEntries-1] do begin
    IFD_Tag := IFD_Tag_0115;      // components per pixel
    IFD_Type := IFD_Type_Word;
    IFD_NumValues := 1;
//    IFD_ValueOrOffset.WordValue := 3;      // RGB
//    IFD_ValueOrOffset.WordValue := 4;      // RGBA
    IFD_ValueOrOffset := ColorSize;
  end;

  inc(NumEntries);
  tif_IFD.iNumEntries := NumEntries;
  setlength(tif_IFD.iEntries,NumEntries);
  with tif_IFD.iEntries[NumEntries-1] do begin
    IFD_Tag := IFD_Tag_0116;      // rows per strip (image height if only one strip)
    IFD_Type := IFD_Type_Long;    // or IFD_Type_Word
    IFD_NumValues := 1;
//    IFD_ValueOrOffset.LongValue := Height;
    IFD_ValueOrOffset := Height;
  end;

  inc(NumEntries);
  tif_IFD.iNumEntries := NumEntries;
  setlength(tif_IFD.iEntries,NumEntries);
  with tif_IFD.iEntries[NumEntries-1] do begin
    IFD_Tag := IFD_Tag_0117;      // strip byte counts (width*height*bytes per pixel)
    IFD_Type := IFD_Type_Long;    // or IFD_Type_Word
    IFD_NumValues := 1;
//    IFD_ValueOrOffset.LongValue := SizeOf_DataBlock;
    IFD_ValueOrOffset := SizeOf_DataBlock;
  end;

   if (ColorSize = 4) then begin
    inc(NumEntries);
    tif_IFD.iNumEntries := NumEntries;
    setlength(tif_IFD.iEntries,NumEntries);
    with tif_IFD.iEntries[NumEntries-1] do begin
      IFD_Tag := IFD_Tag_0152;      // extra component (alpha)
      IFD_Type := IFD_Type_Word;
      IFD_NumValues := 1;
//      IFD_ValueOrOffset.LongValue := 1;
//      IFD_ValueOrOffset.LongValue := 2;
      IFD_ValueOrOffset := 2;
    end;
  end;

  AssignFile(Tiff_32_File,TIF_FileName);
  Rewrite(Tiff_32_File);

  // update pointer to IFD and write header
  tif_Header.tIFD_Offset :=
    sizeOf(TIFF_Header_Type) +  // skip over header;
    sizeOf_DataBlock;           // skip over datablock;
  BlockWrite(Tiff_32_File,tif_Header,
    sizeOf(TIFF_Header_Type));

{  // write data block -> one 'strip' only
  // for testing
  for i := 0 to Height-1 do begin
    for j := 0 to Width-1 do begin
      for k := 0 to ColorSize-1 do begin
        write(Tiff_32_File,TestValue[k]);
      end;
    end;
  end;
}

      if (assigned(ProgressBar_Status)) then ProgressBar_Status.Max := Height;
      for i := 0 to Height-1 do begin
        // just use blockwrite for now with 24 bits
        // note that color bytes are reversed and color will be incorrect
        // but not a big deal for DetectTree 'black' and 'not black'
        BlockWrite(Tiff_32_File,Image.Picture.Bitmap.ScanLine[i]^,Width * colorSize);

        if (assigned(ProgressBar_Status)) then ProgressBar_Status.StepIt;
        Application.ProcessMessages;
      end;

  // update next IFD pointer
  with tif_IFD do begin
    iNextIFD_Offset := 0; // if last IFD, must be set to 0
    // update value pointers
    SizeOf_Tif_IFD := sizeOf(TIFF_IFD_Type) -
      {} 4 +  // dynamic var overhead
      sizeOf(TIFF_IFD_Entry_Type) * iNumEntries;
    iEntries[BitsPerSampleIndex].IFD_ValueOrOffset :=
      iEntries[2].IFD_ValueOrOffset +
      SizeOf(TIFF_Header_Type) +  // skip over header;
      SizeOf_DataBlock +          // skip over datablock;
      SizeOf_Tif_IFD;             // skip IFD
    // and write IFD
    // unfortunately cannot write as a block because of dynamic array of entries
    BlockWrite(Tiff_32_File,iNumEntries,
      {SizeOf(iNumEntries)} sizeOf(Word));
    for i := 0 to iNumEntries-1 do begin
      BlockWrite(Tiff_32_File,iEntries[i],
        sizeOf(TIFF_IFD_Entry_Type));
    end;
    BlockWrite(Tiff_32_File,iNextIFD_Offset,
      {SizeOf(iNextIFD_Offset)} sizeOf(LongWord));
  end;

  // write values
  BlockWrite(Tiff_32_File,tif_IFD_Values.iValues[0],
    NumValues);

//  Close(Bitmap_8_File);
//  Close(Bitmap_24_File);
  CloseFile(Tiff_32_File);
  MessageShow('TIFF file created.');
  if (assigned(ProgressBar_Status)) then ProgressBar_Status.Position := 0;
end;

{----------------------------------------------------------------------------}
begin { Initialization }
  Memo_Message := nil;
//  Bitmap_24_To_Tiff_Alpha('Test_24.bmp','Test_8.bmp','Tif_32.tif');
end.

{--- End of File ------------------------------------------------------------}

{
 * u_DXT.pas
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
UNIT u_DXT;

{----------------------------------------------------------------------------
DDS DXT file

Header 128 bytes

data block  width*height /16 groups
mipmaps     successive data blocks, each 1/4 smaller

DXT1

if color0 > color1 then 4 colors
                   else 3 colors and alpha

each group of 16 pixel = total 8 bytes
  uint16 color0
  uint16 color1
  uint2  select00
  uint2  select01
  uint2  select02
  uint2  select03
  uint2  select10
  uint2  select11
  uint2  select12
  uint2  select13
  uint2  select20
  uint2  select21
  uint2  select22
  uint2  select23
  uint2  select30
  uint2  select31
  uint2  select32
  uint2  select33

DXT3

each group of 16 pixel = total 16 bytes
  unit4  alpha00
  unit4  alpha01
  unit4  alpha02
  unit4  alpha03
  unit4  alpha10
  unit4  alpha11
  unit4  alpha12
  unit4  alpha13
  unit4  alpha20
  unit4  alpha21
  unit4  alpha22
  unit4  alpha23
  unit4  alpha30
  unit4  alpha31
  unit4  alpha32
  unit4  alpha33
  uint16 color0
  uint16 color1
  uint2  select00
  uint2  select01
  uint2  select02
  uint2  select03
  uint2  select10
  uint2  select11
  uint2  select12
  uint2  select13
  uint2  select20
  uint2  select21
  uint2  select22
  uint2  select23
  uint2  select30
  uint2  select31
  uint2  select32
  uint2  select33

DXT5

each group of 16 pixel = total 16 bytes
  uint8  Alpha0
  uint8  Alpha1
  unit3  alpha00
  unit3  alpha01
  unit3  alpha02
  unit3  alpha03
  unit3  alpha10
  unit3  alpha11
  unit3  alpha12
  unit3  alpha13
  unit3  alpha20
  unit3  alpha21
  unit3  alpha22
  unit3  alpha23
  unit3  alpha30
  unit3  alpha31
  unit3  alpha32
  unit3  alpha33
  uint16 color0
  uint16 color1
  uint2  select00
  uint2  select01
  uint2  select02
  uint2  select03
  uint2  select10
  uint2  select11
  uint2  select12
  uint2  select13
  uint2  select20
  uint2  select21
  uint2  select22
  uint2  select23
  uint2  select30
  uint2  select31
  uint2  select32
  uint2  select33

----------------------------------------------------------------------------}

//===========================================================================
INTERFACE

uses
  stdctrls, comctrls;

const
  dds_Magic = $20534444;

  hdr_Flags_Index = 20;
    hdr_Flags_Alpha = 1;
  hdr_DXT_Index = 21;
    hdr_DXT1 = $31545844;
    hdr_DXT3 = $33545844;
    hdr_DXT5 = $35545844;
  hdr_Height = 3;
  hdr_Width = 4;
  hdr_LinearSize_Index = 5;
  hdr_NumMipMaps = 7;

type
//  t_Select = array[0..4-1] of byte;
  t_Select = LongWord;

  t_DDS_Header = array[0..32-1] of longWord;

  t_DDS_DXT1_Block =
    record
      color0 : word;
      color1 : word;
      select : t_Select;
    end;

  t_DDS_DXT3_Block =
    record
//      alpha  : array[0..8-1] of byte;
//      alpha  : uint64;
      alpha  : int64;
      color0 : word;
      color1 : word;
      select : t_Select;
    end;

var
  Memo_Message : TMemo;  // external TMemo for messages
  ProgressBar_Status : TProgressBar;

  dxt_Path     : String;
  dxt_FileName : String;

  Test_Select : t_Select;

procedure Convert_DXT3_5_to_DXT1;
procedure FixPuddles_DXT1;
function  DXT_ImageWidth(FileName : String) : Longint;
procedure DXT_Rotate_180(DDS_FileName, DDS_Rotated_FileName : string);
procedure DXT_Reduce;
procedure DXT_MakeEmpty(FileName : String);

//===========================================================================
IMPLEMENTATION

uses SysUtils;

var
  DDS_File_In : File of byte;
  DDS_File_Out : File of byte;

{----------------------------------------------------------------------------}
Procedure MessageShow(Info : string);
begin
  if (Memo_Message <> nil) then begin
    Memo_Message.lines.add(Info);
  end;
end;

{----------------------------------------------------------------------------}
procedure Convert_DXT3_5_to_DXT1; // no alpha
var
  DDS_Header   : t_DDS_Header;
  DDS_BlockIn  : t_DDS_DXT3_Block; // same for DXT5
  DDS_BlockOut : t_DDS_DXT1_Block;
  Count_1 : integer;
  Count_2 : integer;
  TEMP : integer;
  FileName, NewFileName : string;

begin
  Count_1 := 0;
  Count_2 := 0;
  FileName := dxt_Path+'\'+dxt_FileName;
  NewFileName := dxt_Path+'\'+dxt_FileName+'.dds';
  if fileExists(FileName) then begin
    AssignFile(DDS_File_In,FileName);
    Reset(DDS_File_In);
    BlockRead(DDS_File_In,DDS_Header,sizeof(DDS_Header));

    // check for DXT3/5 signature
    if ((DDS_Header[0] = dds_Magic) AND
//        (DDS_Header[hdr_DXT_Index] = hdr_DXT3))
        ((DDS_Header[hdr_DXT_Index] = hdr_DXT3) OR (DDS_Header[hdr_DXT_Index] = hdr_DXT5)) )
        then begin
      AssignFile(DDS_File_Out,FileName+'.dds');
      Rewrite(DDS_File_Out);

      // modify header
      DDS_Header[hdr_DXT_Index] := hdr_DXT1;
      DDS_Header[hdr_LinearSize_Index] := DDS_Header[hdr_LinearSize_Index] div 2;

      BlockWrite(DDS_File_Out,DDS_Header,sizeof(DDS_Header));

      While NOT EOF(DDS_File_In) do begin
        BlockRead(DDS_File_In,DDS_BlockIn,sizeof(DDS_BlockIn));

        DDS_BlockOut.color0 := DDS_BlockIn.color0;
        DDS_BlockOut.color1 := DDS_BlockIn.color1;
        DDS_BlockOut.select := DDS_BlockIn.select;
        // modify block if needed to make sure color0 > color1 for 4 colors and ignore transparency
        // OK to swap because 4 colors are definitely used in DXT3/5, not just 3.
        // NOTE: if you want transparency as well, then the 'color0 >= color1' is needed and colors need to be modified ! (TBD)
        // and need a threshold since DXT1 is just on/off versus 15 levels for DXT3  (TBD)
        // or alpha range in DXT5  (TBD)
	with DDS_BlockOut do begin
	  if (color0 < color1) then begin
	    // reverse colors
            TEMP := color0;
            color0 := color1;
            color1 := TEMP;
//            select := select XOR $FFFFFFFF; // bug 0->3, 1->2, 2->1, 3->0
            select := select XOR $55555555;   // 0->1, 1->0, 2->3, 3->2
            INC(Count_1);
	  end else begin
    	    if (color0 = color1) then begin
	      // force a difference
              if (color0 < $FFFF) then begin
                INC(color0);
              end else begin
                DEC(color1);
              end;
              INC(Count_2);
	    end;
	  end;
	end;

        BlockWrite(DDS_File_Out,DDS_BlockOut,sizeof(DDS_BlockOut));
      end;
      Close(DDS_File_Out);
      Close(DDS_File_in);
      MessageShow(format('File %s %d %d',[dxt_FileName, Count_1,Count_2]));

      // delete original
      DeleteFile(Filename);
      // rename newfile to replace original
      RenameFile(NewFileName,dxt_FileName)
    end else begin
      Close(DDS_File_in);
      MessageShow(format('Error: %s not DXT3 or DXT5 ',[dxt_FileName]));
    end;
  end else begin
    MessageShow(format('File %s not found',[dxt_FileName]));
  end;
end;

{----------------------------------------------------------------------------}
Function xScanAlpha(Alpha : array of byte) : boolean;
var
  i : Integer;
begin
  Result {ScanAlpha} := false;
  // DXT3
  for i := 0 to 8-1 do begin
    if (Alpha[i] <> $FF) then begin
       Result {ScanAlpha} := true;
      Exit;
    end;
  end;
  // DXT5 - TBD
  // messy with 3 bit indexes
  // use long and 2 times 24 ?
end;

{----------------------------------------------------------------------------}
Function ScanAlpha(Alpha : int64) : boolean;
var
  i : Integer;
begin
  ScanAlpha := false;
  // DXT3
  if (Alpha <> $FFFFFFFFFFFF) then begin
      ScanAlpha := true;
  end;
  // DXT5 - TBD
  // messy with 3 bit indexes
  // use long and 2 times 24 ?
end;

// convert DXT5 partial/full alpha to alpha or no alpha
{----------------------------------------------------------------------------}
Procedure Decode_DXT5_Alpha(var Alpha : array of byte);
begin
 // Alpha, Alpha1, 16 3 bit indexes -> 16 alphas
{
}
end;

{----------------------------------------------------------------------------}
Function xIsAlpha(Alpha : array of byte; aIndex : Integer) : boolean;
var
  i,j : Integer;
begin
  Result {IsAlpha} := false;
  // DXT3
  i := aIndex div 2;
  j := aIndex mod 2;
  if (((Alpha[i] SHR (j*4)) AND $0F) <> $0F) then begin
    Result {IsAlpha} := true;
  end;
  // DXT5 - TBD
  // messy with 3 bit indexes
  // use long and 2 times 24 ?
end;

// int64 SHR buggy ? - no seems to be OK
{----------------------------------------------------------------------------}
Function IsAlpha(Alpha : int64; aIndex : Integer) : boolean;
begin
  IsAlpha := false;
  // DXT3
  if (((Alpha SHR (aIndex * 4)) AND $000000000000000F) <> $0F) then begin
    IsAlpha := true;
  end;
  // DXT5 - TBD
  // messy with 3 bit indexes
  // use long and 2 times 24 ?
end;

{----------------------------------------------------------------------------}
procedure xConvert_DXT3_5_to_DXT1; // with alpha, DXT3 only, not DXT5
var
  DDS_Header   : t_DDS_Header;
  DDS_BlockIn  : t_DDS_DXT3_Block; // same for DXT5
  DDS_BlockOut : t_DDS_DXT1_Block;
  Count_1 : integer;
  Count_2 : integer;
  TEMP : integer;
  FileName, NewFileName : string;
  i : Integer;
  Selector : LongWord;

begin
  Count_1 := 0;
  Count_2 := 0;
  FileName := dxt_Path+'\'+dxt_FileName;
  NewFileName := dxt_Path+'\'+dxt_FileName+'.dds';
  if fileExists(FileName) then begin
    AssignFile(DDS_File_In,FileName);
    Reset(DDS_File_In);
    BlockRead(DDS_File_In,DDS_Header,sizeof(DDS_Header));

    // check for DXT3/5 signature
    if ((DDS_Header[0] = dds_Magic) AND
//        (DDS_Header[hdr_DXT_Index] = hdr_DXT3))
        ((DDS_Header[hdr_DXT_Index] = hdr_DXT3) OR (DDS_Header[hdr_DXT_Index] = hdr_DXT5)) )
        then begin
      AssignFile(DDS_File_Out,FileName+'.dds');
      Rewrite(DDS_File_Out);

      // modify header
      DDS_Header[hdr_DXT_Index] := hdr_DXT1;
      DDS_Header[hdr_LinearSize_Index] := DDS_Header[hdr_LinearSize_Index] div 2;

      BlockWrite(DDS_File_Out,DDS_Header,sizeof(DDS_Header));

      While NOT EOF(DDS_File_In) do begin
        BlockRead(DDS_File_In,DDS_BlockIn,sizeof(DDS_BlockIn));

        DDS_BlockOut.color0 := DDS_BlockIn.color0;
        DDS_BlockOut.color1 := DDS_BlockIn.color1;
        DDS_BlockOut.select := DDS_BlockIn.select;
   // if opaque
        // modify block if needed to make sure color0 > color1 for 4 colors and ignore transparency
        // OK to swap because 4 colors are definitely used in DXT3/5, not just 3.
        // NOTE: if you want transparency as well, then the 'color0 >= color1' is needed and colors need to be modified ! (TBD)
        // and need a threshold since DXT1 is just on/off versus 15 levels for DXT3  (TBD)
        // or alpha range in DXT5  (TBD)
   // if not opaque
        // modify block if needed to make sure color0 <= color1 for 3 colors and transparency if transparency used in block
	with DDS_BlockOut do begin
          if (NOT ScanAlpha(DDS_Blockin.Alpha)) then begin // if all alpha = 1
    	    if (color0 < color1) then begin // make sure opaque
	      // reverse colors
              TEMP := color0;
              color0 := color1;
              color1 := TEMP;
//              select := select XOR $FFFFFFFF; // bug 0->3, 1->2, 2->1, 3->0
              select := select XOR $55555555;   // 0->1, 1->0, 2->3, 3->2
              INC(Count_1);
	    end else begin
    	      if (color0 = color1) then begin
	        // force a difference
                if (color0 < $FFFF) then begin
                  INC(color0);
                end else begin
                  DEC(color1);
                end;
                INC(Count_2);
	      end;
	    end;
          end else begin // make sure not opaque
    	    if (color0 > color1) then begin // make sure opaque
	      // reverse colors
              TEMP := color0;
              color0 := color1;
              color1 := TEMP;
//              select := select XOR $FFFFFFFF; // bug 0->3, 1->2, 2->1, 3->0
              select := select XOR $55555555;   // 0->1, 1->0, 2->3, 3->2
              INC(Count_1);
	    end;
            // for any selector = 3, change to 2
            // for any alpha, change selector to 3
            // modify block if needed to make sure color0 <= color1 for 3 colors and transparency
            with DDS_BlockOut do begin
              // check for color3 use
              for i := 0 to 16-1 do begin
                Selector := Select AND $00000003;
                if (Selector = $00000003) then begin
                  Selector := 2; // do not use color3 !
                end;
                if (IsAlpha(DDS_Blockin.Alpha,i)) then begin
                  Selector := 3; // make transparent !
                end;
                Select := Select SHR 2 + Selector SHL 30;
              end;
            end;
          end;
	end;

        BlockWrite(DDS_File_Out,DDS_BlockOut,sizeof(DDS_BlockOut));
      end;
      Close(DDS_File_Out);
      Close(DDS_File_in);
      MessageShow(format('File %s %d %d',[dxt_FileName, Count_1,Count_2]));

      // delete original
      DeleteFile(Filename);
      // rename newfile to replace original
      RenameFile(NewFileName,dxt_FileName)
    end else begin
      Close(DDS_File_in);
      MessageShow(format('Error: %s not DXT3 or DXT5 ',[dxt_FileName]));
    end;
  end else begin
    MessageShow(format('File %s not found',[dxt_FileName]));
  end;
end;

{----------------------------------------------------------------------------}
procedure FixPuddles_DXT1;
var
  DDS_Header   : t_DDS_Header;
  DDS_BlockIn  : t_DDS_DXT1_Block;
  DDS_BlockOut : t_DDS_DXT1_Block;
  Count_1 : integer;
  Count_2 : integer;
  Count_3 : integer;
  Puddles : integer;
  i : integer;
  FileName, NewFileName : string;
  Flag_Transparent : LongWord;
  Selector : LongWord;

begin
  Count_1 := 0;
  Count_2 := 0;
  Count_3 := 0;
  Puddles := 0;
  FileName := dxt_Path+'\'+dxt_FileName;
  NewFileName := dxt_Path+'\'+dxt_FileName+'.dds';
  if fileExists(FileName) then begin
    AssignFile(DDS_File_In,FileName);
    Reset(DDS_File_In);
    BlockRead(DDS_File_In,DDS_Header,sizeof(DDS_Header));

    // check for DXT1 signature
    if ((DDS_Header[0] = dds_Magic) AND
        (DDS_Header[hdr_DXT_Index] = hdr_DXT1)) then begin
      AssignFile(DDS_File_Out,FileName+'.dds');
      Rewrite(DDS_File_Out);

      // check header
      Flag_Transparent := DDS_Header[hdr_Flags_Index] AND hdr_Flags_Alpha;

      BlockWrite(DDS_File_Out,DDS_Header,sizeof(DDS_Header));

      While NOT EOF(DDS_File_In) do begin
        BlockRead(DDS_File_In,DDS_BlockIn,sizeof(DDS_BlockIn));

        DDS_BlockOut.color0 := DDS_BlockIn.color0;
        DDS_BlockOut.color1 := DDS_BlockIn.color1;
        DDS_BlockOut.select := DDS_BlockIn.select;
        // modify block if needed to make sure color0 > color1 for 4 colors
        // NOTE: if you want transparency then the opposite is needed and colors need to be modified ! (TBD)
	with DDS_BlockOut do begin
          // check for color3 use
          for i := 0 to 16-1 do begin
            Selector := Select AND $00000003;
            if (Selector = $00000003) then begin
              INC(Count_3);
  	        if (color0 <= color1) then begin // transparency indicator ?
                  INC(Puddles);
                  Selector := 2; // do not use color3 !
                end;
            end;
            Select := Select SHR 2 + Selector SHL 30;
          end;
          // NOT OK to swap when 3 colors are used, unless colors are re-calculated...
          // pick nearest color, color2 above instead of color3 ?
          {
          if (color0 < color1) then begin
	    // reverse colors
            TEMP := color0;
            color0 := color1;
            color1 := TEMP;
//            select := select XOR $FFFFFFFF; // bug 0->3, 1->2, 2->1, 3->0
            select := select XOR $55555555;   // 0->1, 1->0, 2->3, 3->2
            INC(Count_1);
	  end else begin
    	    if (color0 = color1) then begin
	      // force a difference
              if (color0 < $FFFF) then begin
                INC(color0);
              end else begin
                DEC(color1);
              end;
              INC(Count_2);
	    end;
	  end;
          }
	end;

        BlockWrite(DDS_File_Out,DDS_BlockOut,sizeof(DDS_BlockOut));
      end;
      Close(DDS_File_Out);
      Close(DDS_File_in);
//      MessageShow(format('File %s %d %d %d %d %d',[dxt_FileName, Count_1,Count_2,Count_3,Puddles,Flag_Transparent]));
      MessageShow(format('File %s %d %d %d',[dxt_FileName,Flag_Transparent,Count_3,Puddles]));

     // delete original
     DeleteFile(Filename);
     // rename newfile to replace original
     RenameFile(NewFileName,dxt_FileName)
    end else begin
      MessageShow(format('Error: %s not DXT1',[dxt_FileName]));
      Close(DDS_File_in);
    end;
  end else begin
    MessageShow(format('File %s not found',[dxt_FileName]));
  end;
end;

{----------------------------------------------------------------------------}
function DXT_ImageWidth(FileName : String) : Longint;
var
  DDS_Header   : t_DDS_Header;
begin
  if (NOT fileExists(FileName)) then begin
    Result := -1;
  end else begin
    AssignFile(DDS_File_In,FileName);
    Reset(DDS_File_In);
    BlockRead(DDS_File_In,DDS_Header,sizeof(DDS_Header));

    // check for DXT signature
    if ((DDS_Header[0] = dds_Magic) AND (
        (DDS_Header[hdr_DXT_Index] = hdr_DXT1) OR
        (DDS_Header[hdr_DXT_Index] = hdr_DXT3)
      )) then begin
      Result := DDS_Header[hdr_Width];
    end;
    Close(DDS_File_in);
  end;
end;

{----------------------------------------------------------------------------}
procedure DXT_FlipSelects(var Select : t_Select);
var
  i : integer;
  sel : t_Select;
  Temp : t_Select;
begin
  Temp := 0;
  for i := 0 to 16-1 do begin
    Temp := Temp SHL 2;
    sel := Select AND $00000003;
    Temp := Temp + sel;
    Select := Select SHR 2;
  end;
  Select := Temp;
end;

// TBD  dxt3 support, lowest 2 mipmaps (2x2, 1x1)
{----------------------------------------------------------------------------}
procedure DXT_Rotate_180(DDS_FileName, DDS_Rotated_FileName : string);
var
  DDS_Header   : t_DDS_Header;
//  DDS_BlockIn  : t_DDS_DXT1_Block;
//  DDS_BlockOut : t_DDS_DXT1_Block;
//  DDS_Block : array [0..16-1] of byte; // for DXT1 ot DXT3
  DDS_Block : t_DDS_DXT1_Block; // for now
  DDS_Block_Size : integer;
  DDS_Width : Longint;
  DDS_Height : Longint;
  i : integer;
//  DDS_FileName, DDS_Rotated_FileName : string;

  NumMipMaps : Longint;
  xWidth : Longint;
  yHeight : Longint;
  m, x, y : integer;
  In_Index : Longint;
  Out_Index : Longint;
  Offset_Index : LongInt;

begin
//  DDS_FileName := dxt_Path+'\'+dxt_FileName;
//  DDS_Rotated_FileName := dxt_Path+'\'+dxt_FileName+'.dds';
  if fileExists(DDS_FileName) then begin
    AssignFile(DDS_File_In,DDS_FileName);
    Reset(DDS_File_In);
    BlockRead(DDS_File_In,DDS_Header,sizeof(DDS_Header));

    // check for DXT1 signature
    if ((DDS_Header[0] = dds_Magic) AND (
        (DDS_Header[hdr_DXT_Index] = hdr_DXT1) { OR
        (DDS_Header[hdr_DXT_Index] = hdr_DXT3) })) then begin
      if (DDS_Header[hdr_DXT_Index] = hdr_DXT1) then begin
        DDS_Block_Size := 8;
      end else begin  // else DXT3
        DDS_Block_Size := 16;
      end;
      AssignFile(DDS_File_Out,DDS_Rotated_FileName);
      Rewrite(DDS_File_Out);
      BlockWrite(DDS_File_Out,DDS_Header,sizeof(DDS_Header));

      // check header
//      DDS_Size := DDS_Header[hdr_DXT_Index] = hdr_DXT3)) then begin
      DDS_Width := DDS_Header[hdr_Width];
      DDS_Height := DDS_Header[hdr_Height];
      NumMipMaps := DDS_Header[hdr_NumMipMaps];

      Offset_Index := Sizeof(t_DDS_Header);
      xWidth := DDS_Width;
      yHeight := DDS_Height;

      for m := 0 to NumMipMaps-1 do begin
        for y := 0 to (yHeight div 4)-1 do begin
          for x := 0 to (xWidth div 4)-1 do begin

//            In_Index := Offset_Index + ( x + y *(xWidth div 4) )*DDS_Block_Size);
//            Seek(DDS_File_In,In_Index); // sequential, not needed
            BlockRead(DDS_File_In,DDS_Block,DDS_Block_Size);

//            DDS_BlockOut := DDS_BlockIn;
            //special cases for lowest mipmap levels
{            if (mipmap 1x1 or mipmap 2x2) then} if false then begin
            end else begin
              DXT_FlipSelects(DDS_Block.select);
            end;
            // rotate 180 deg
            Out_Index := Offset_Index + ( ((xWidth div 4)-1- x) + ((yHeight div 4)-1- y)*(xWidth div 4) )*DDS_Block_Size;
            Seek(DDS_File_Out,Out_Index);
            BlockWrite(DDS_File_Out,DDS_Block,DDS_Block_Size);

          end;
        end;
        Offset_Index := Offset_Index + ((xWidth div 4) * (yHeight div 4)) *DDS_Block_Size;
        if( xWidth > 4) then begin
          xWidth := xWidth div 2;
        end;
        if (yHeight > 4) then begin
          yHeight := yHeight div 2;
        end;
      end;
      Close(DDS_File_Out);
      Close(DDS_File_in);

     // delete original
//     DeleteFile(Filename);
     // rename newfile to replace original
//     RenameFile(NewFileName,dxt_FileName)
    end else begin
      MessageShow(format('Error: %s not DXT1 or DXT3',[dxt_FileName]));
      Close(DDS_File_in);
    end;
  end else begin
    MessageShow(format('File %s not found',[dxt_FileName]));
  end;
end;

{----------------------------------------------------------------------------}
procedure DXT_Reduce;
const
  DefaultBlockSize = 256;
var
  DDS_Header   : t_DDS_Header;
  DDS_BlockIn  : t_DDS_DXT3_Block; // assume biggest block
  DDS_BlockOut : t_DDS_DXT3_Block;
  i : integer;
  FileName, NewFileName : string;
  NumMipMaps : Longint;
  xWidth : Longint;
  yHeight : Longint;
  sSize : Longint;
  P : ^pByteArray;
  TotalSize : Longword;
  NumBytes : Longword;
  BlockSize : integer;

begin
  NewFileName := dxt_Path+'\'+dxt_FileName;
  FileName := dxt_Path+'\'+dxt_FileName+'.~dds'; // save original
  if (FileExists(FileName)) then begin
    DeleteFile(FileName);
  end;
  RenameFile(NewFileName,FileName);
  if (NOT fileExists(FileName)) then begin
    MessageShow(format('File %s not found',[dxt_FileName]));
    Exit;
  end else begin
    AssignFile(DDS_File_In,FileName);
    Reset(DDS_File_In);
    TotalSize := FileSize(DDS_File_In);
    BlockRead(DDS_File_In,DDS_Header,sizeof(DDS_Header));

    // check for signature and DXT1,3,5 signature
    if (DDS_Header[0] <> dds_Magic) then begin
      MessageShow(format('Error: %s not DXT',[dxt_FileName]));
      Close(DDS_File_in);
      Exit;
    end else begin
      case (DDS_Header[hdr_DXT_Index]) of
        hdr_DXT1: begin
          BlockSize := sizeof(t_DDS_DXT1_Block);
        end;
        hdr_DXT3, hdr_DXT5 : begin
          BlockSize := sizeof(t_DDS_DXT3_Block);
        end;
        else begin
          MessageShow(format('Error: %s not DXT1,3,5',[dxt_FileName]));
          Close(DDS_File_in);
          Exit;
        end;
      end;

//      AssignFile(DDS_File_Out,FileName+'.dds');
      AssignFile(DDS_File_Out,NewFileName);
      Rewrite(DDS_File_Out);

      // check header
      NumMipMaps := DDS_Header[hdr_NumMipMaps];
      if (NumMipMaps < 2) then begin
        MessageShow(format('File: %s MipMaps: %d',[dxt_FileName, NumMipMaps]));
        exit;
      end;
      yHeight := DDS_Header[hdr_Height];
      xWidth :=  DDS_Header[hdr_Width];
      sSize :=   DDS_Header[hdr_LinearSize_Index];

      DDS_Header[hdr_NumMipMaps] :=      NumMipMaps-1;
      DDS_Header[hdr_Height] :=          yHeight div 2;
      DDS_Header[hdr_Width] :=           xWidth div 2;
//      DDS_Header[hdr_LinearSize_Index]:= sSize - xWidth*yHeight*BlockSize;
//      DDS_Header[hdr_LinearSize_Index]:= DDS_Header[hdr_Height]div 4*DDS_Header[hdr_Width]div 4*BlockSize;
      DDS_Header[hdr_LinearSize_Index]:= DDS_Header[hdr_Height] div 4 * DDS_Header[hdr_Width] div 4 * BlockSize;

      BlockWrite(DDS_File_Out,DDS_Header,sizeof(DDS_Header));

      // skip over first MipMap
      seek (DDS_File_In, Sizeof(t_DDS_Header)+ (xWidth div 4)*(yHeight div 4)* BlockSize);
{
      // slow - improve - do more than one block at a time !!!
      While NOT EOF(DDS_File_In) do begin
        BlockRead(DDS_File_In,DDS_BlockIn,BlockSize);
        DDS_BlockOut := DDS_BlockIn;
        BlockWrite(DDS_File_Out,DDS_BlockOut,BlockSize);
      end;
}
      // faster
      try
        NumBytes := TotalSize-Sizeof(t_DDS_Header) -
          (xWidth div 4)*(yHeight div 4)*BlockSize;
        P := AllocMem(DefaultBlockSize);
        while(NumBytes >= DefaultBlockSize) do begin
          BlockRead(DDS_File_In,P^,DefaultBlockSize);
          BlockWrite(DDS_File_Out,P^,DefaultBlockSize);
          DEC(NumBytes,DefaultBlockSize);
        end;
        BlockRead(DDS_File_In,P^,NumBytes);
        BlockWrite(DDS_File_Out,P^,NumBytes);
      finally
        freemem(P);
      end;

      Close(DDS_File_Out);
      Close(DDS_File_in);
    end;
  end;
end;

{----------------------------------------------------------------------------}
procedure DXT_MakeEmpty(FileName : String);
var
  i : integer;
  DDS_Header   : t_DDS_Header;
  DDS_BlockOut : t_DDS_DXT3_Block;
  MaxCount : integer;

begin
  for i := 0 to 32-1 do begin
    DDS_Header[i] := 0;
  end;

  // DXT3, 128 byte header, 256x256,
  MaxCount := 5463;       // 256*256/16+ 128*128/16+... (for mipmaps)
  DDS_Header[0] := dds_Magic;
  DDS_Header[1] := 128-4; // header size, less magic
  DDS_Header[2] := $000A1007; // flags
  DDS_Header[hdr_Height] := 256;
  DDS_Header[hdr_Width] := 256;
  DDS_Header[hdr_LinearSize_Index] := 256 * 256;
  DDS_Header[hdr_NumMipMaps] := 9;
  DDS_Header[19] := $00000020;               // pixel format
  DDS_Header[hdr_Flags_Index] := $00000004;  // FOURCC
  DDS_Header[hdr_DXT_Index] := hdr_DXT3;
  DDS_Header[27] := $401008;                 //

  // semi-transparent (3/16), blueish
  with DDS_BlockOut do begin
//    for i := 0 to 8-1 do begin
//      alpha[i]  :=  $33;   // semi transparent, 3/16
//    end;
      alpha  :=  $3333333333333333;   // semi transparent, 3/16
      color0 := $1A2B;     // 00011 010001 01011  R G B  0001100 01000100 01011111  12 68 95
      color1 := $1A2B;
      select := $FFFFFFFF; // each of 16 pixels is color #3 -> 11
  end;

  AssignFile(DDS_File_out,FileName);
  Rewrite(DDS_File_out);
  BlockWrite(DDS_File_out,DDS_Header,sizeof(DDS_Header));
  for i := 0 to MaxCount-11 do begin
    BlockWrite(DDS_File_out,DDS_BlockOut,sizeof(DDS_BlockOut));
  end;
  Close(DDS_File_out);
end;

{----------------------------------------------------------------------------}
begin { Initialization }
end.

{--- End of File ------------------------------------------------------------}



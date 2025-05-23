
//http://www.koders.com/delphi/fid83F1E7967254FC6E4FC658631E1117C655F80D1A.aspx?s=delphi#L7

//
// This unit is part of the GLScene Project, http://glscene.org
//
{: DDS<p>

   Simple DDS (Microsoft DirectDraw Surface) format support
   for Delphi.<p>
   
   Note:<br>
   Only the main surface is loaded (mipmap levels, volume 
   textures and cubic environment maps are currently 
   ignored). Saves out to 24 or 32 bit uncompressed only.<p>

   Supported pixel formats:<ul>
      <li>16 bit (565, 1555, 4444)
      <li>24 bit (888)
      <li>32 bit (8888)
      <li>DXT1 (alpha and color)
      <li>DXT3
      <li>DXT5
   </ul>

   <b>History : </b><font size=-1><ul>
      <li>03/09/04 - SG - Delphi 5 compatibilty fixes (Ivan Lee Herring)
      <li>01/09/04 - SG - Added support for DXTC compression (DXT1, DXT3 and DXT5)
      <li>31/08/04 - SG - Added support for 16 bit textures,
                          it should support most uncompressed files now
      <li>31/08/04 - SG - Creation
   </ul></font>
}
unit DDS;

//===========================================================================
INTERFACE

{ $ i GLScene.inc}

uses Classes, SysUtils, {GLCrossPlatform, VectorGeometry,} DXTC, Graphics;

type

//   TDDSImage = class (TGLBitmap)
   TDDSImage = class (TBitmap)
      public
         { Public Declarations }
         procedure LoadFromStream(stream : TStream); override;
         procedure SaveToStream(stream : TStream); override;
	end;

   EDDSException = class(Exception)
   end;

//===========================================================================
IMPLEMENTATION

type
  PByte = ^Byte;
  PWord = ^Word;
  PCardinal = ^Cardinal;

const
   DDSD_CAPS        = $00000001;
   DDSD_HEIGHT      = $00000002;
   DDSD_WIDTH       = $00000004;
   DDSD_PITCH       = $00000008;
   DDSD_PIXELFORMAT = $00001000;
   DDSD_MIPMAPCOUNT = $00020000;
   DDSD_LINEARSIZE  = $00080000;
   DDSD_DEPTH       = $00800000;

   DDPF_ALPHAPIXELS = $00000001;
   DDPF_FOURCC      = $00000004;
   DDPF_RGB         = $00000040;

   DDSCAPS_COMPLEX  = $00000008;
   DDSCAPS_TEXTURE  = $00001000;
   DDSCAPS_MIPMAP   = $00400000;

   DDSCAPS2_CUBEMAP           = $00000200;
   DDSCAPS2_CUBEMAP_POSITIVEX = $00000400;
   DDSCAPS2_CUBEMAP_NEGATIVEX = $00000800;
   DDSCAPS2_CUBEMAP_POSITIVEY = $00001000;
   DDSCAPS2_CUBEMAP_NEGATIVEY = $00002000;
   DDSCAPS2_CUBEMAP_POSITIVEZ = $00004000;
   DDSCAPS2_CUBEMAP_NEGATIVEZ = $00008000;
   DDSCAPS2_VOLUME            = $00200000;


type
   TDDPIXELFORMAT = record
      dwSize,
      dwFlags,
      dwFourCC,
      dwRGBBitCount,
      dwRBitMask,
      dwGBitMask,
      dwBBitMask,
      dwRGBAlphaBitMask : Cardinal;
   end;

   TDDCAPS2 = record
      dwCaps1,
      dwCaps2 : Cardinal;
      Reserved : array[0..1] of Cardinal;
   end;

   TDDSURFACEDESC2 = record
      dwSize,
      dwFlags,
      dwHeight,
      dwWidth,
      dwPitchOrLinearSize,
      dwDepth,
      dwMipMapCount : Cardinal;
      dwReserved1 : array[0..10] of Cardinal;
      ddpfPixelFormat : TDDPIXELFORMAT;
      ddsCaps : TDDCAPS2;
      dwReserved2 : Cardinal;
   end;

   TDDSHeader = record
      Magic : Cardinal;
      SurfaceFormat : TDDSURFACEDESC2;
   end;

//   TFOURCC = array[0..3] of char;

const
   FOURCC_DXT1 = $31545844; // 'DXT1'
   FOURCC_DXT3 = $33545844; // 'DXT3'
   FOURCC_DXT5 = $35545844; // 'DXT5'


function GetBitsFromMask(Mask : Cardinal) : Byte;
var
   i, temp : Integer;
begin
   if Mask=0 then begin
     Result:=0;
     exit;
   end;
   temp:=Mask;
   for i:=0 to 31 do begin
      if (Temp and 1) = 1 then
         break;
      temp:=temp shr 1;
   end;
   Result:=i;
end;


// ------------------
// ------------------ TDDSImage ------------------
// ------------------

// LoadFromStream
//
procedure TDDSImage.LoadFromStream(stream : TStream);
var
   header : TDDSHeader;
   i, j, rowSize, ddsPixelSize, imgPixelSize : Integer;
   buf, decoded : PByteArray;
   col : PCardinal;
   RedShift, RedMult,
   GreenShift, GreenMult,
   BlueShift, BlueMult,
   AlphaShift, AlphaMult : Byte;
   trans : Boolean;
   SkipMip : boolean;

begin
   stream.Read(header, Sizeof(TDDSHeader));

//   if TFOURCC(header.Magic) <> 'DDS ' then
   if (header.Magic <> $20534444) then  // 'DDS '
      raise EDDSException.Create('Invalid DDS file');

   with header.SurfaceFormat do begin
      if (ddsCaps.dwCaps1 and DDSCAPS_TEXTURE)=0 then
         raise EDDSException.Create('Unsupported DDSCAPS settings');

      PixelFormat:= {gl}pf32bit;
      if (ddpfPixelFormat.dwFlags and DDPF_ALPHAPIXELS)>0 then
         Transparent:=True;

      Width:=dwWidth;
      Height:=dwHeight;

      // for 32 bit system, need a limit
      SkipMip := false;
{ works ok but for large DDS there is a memory issue anyway
      if ((Width > 8192) or (Height > 8192)) then begin
        if (dwMipMapCount > 1) then begin
          Width:=dwWidth div 2;
          Height:=dwHeight div 2;
          SkipMip := true;
//          Dec(dwMipMapCount);
        end else begin
          raise EDDSException.Create('DDS too large');
        end;
      end;
}
      // 16 pixel blocks, 4x4, to compressed 8 or 16 bytes
      // output is 32 bit color, 24 bit and alpha -> 4 bytes
      if (ddpfPixelFormat.dwFlags and DDPF_FOURCC)>0 then begin
//         GetMem(buf, dwPitchOrLinearSize);   // NICK - moved
       //  GetMem(decoded, Width*Height*4);
         GetMem(decoded, trunc(Width+3)*trunc(Height+3)*4); // need to round up for padding when not divsible by 4
//         Stream.Read(buf[0], dwPitchOrLinearSize);   // NICK - moved
         try
            case ddpfPixelFormat.dwFourCC of
               FOURCC_DXT1 : begin
                  // NICK - moved here for files with dwPitchOrLinearSize = 0
                  if (SkipMip) then begin
                    Stream.Seek((Width*Height div 2) * 4,soFromCurrent); // top mip 4 times bigger
                  end;
                  GetMem(buf, Width*Height div 2);
                  Stream.Read(buf[0], Width*Height div 2); // W/4*H/4 * 8
                  DecodeDXT1toBitmap32(buf, decoded, Width, Height, trans);
                  Transparent:=trans;
               end;
               FOURCC_DXT3 : begin
                  // NICK - moved here for files with dwPitchOrLinearSize = 0
                  if (SkipMip) then begin
                    Stream.Seek(Width*Height * 4,soFromCurrent); // top mip 4 times bigger
                  end;
                  GetMem(buf, Width*Height);
                  Stream.Read(buf[0], Width*Height); // W/4*H/4 * 16
                  DecodeDXT3toBitmap32(buf, decoded, Width, Height);
                  Transparent:=True;
               end;
               FOURCC_DXT5 : begin
                  // NICK - moved here for files with dwPitchOrLinearSize = 0
                  if (SkipMip) then begin
                    Stream.Seek(Width*Height * 4,soFromCurrent); // top mip 4 times bigger
                  end;
                  GetMem(buf, Width*Height);
                  Stream.Read(buf[0], Width*Height); // W/4*H/4 * 16
                  DecodeDXT5toBitmap32(buf, decoded, Width, Height);
                  Transparent:=True;
               end;
            else
//               raise EDDSException.CreateFmt('Unsupported compression type: %s',[TFOURCC(ddpfPixelFormat.dwFourCC)]);
               raise EDDSException.CreateFmt('Unsupported compression type: %8x',[ddpfPixelFormat.dwFourCC]);
            end;
            for j:=0 to Height-1 do
{$IFOPT R+}
  {$DEFINE RANGEON}
  {$R-}
{$ELSE}
  {$UNDEF RANGEON}
{$ENDIF}
               System.Move(decoded[4*j*Width], PCardinal(ScanLine[j])^, Width*4);
{$IFDEF RANGEON}
  {$R+}
  {$UNDEF RANGEON}
{$ENDIF}
         finally
            FreeMem(decoded);
            FreeMem(buf);
         end;
      end else begin
         imgPixelSize:=4;
         ddsPixelSize:=(ddpfPixelFormat.dwRGBBitCount div 8);
         rowSize:=ddsPixelSize*Integer(dwWidth);

         RedShift:=GetBitsFromMask(ddpfPixelFormat.dwRBitMask);
         GreenShift:=GetBitsFromMask(ddpfPixelFormat.dwGBitMask);
         BlueShift:=GetBitsFromMask(ddpfPixelFormat.dwBBitMask);
         if Transparent then
            AlphaShift:=GetBitsFromMask(ddpfPixelFormat.dwRGBAlphaBitMask)
         else
            AlphaShift:=0;

         RedMult:=1;
         GreenMult:=1;
         BlueMult:=1;
         AlphaMult:=1;

         if (ddpfPixelFormat.dwRBitMask shr RedShift)>0 then
            RedMult := 255 div (ddpfPixelFormat.dwRBitMask shr RedShift);
         if (ddpfPixelFormat.dwGBitMask shr GreenShift)>0 then
            GreenMult := 255 div (ddpfPixelFormat.dwGBitMask shr GreenShift);
         if (ddpfPixelFormat.dwBBitMask shr BlueShift)>0 then
            BlueMult := 255 div (ddpfPixelFormat.dwBBitMask shr BlueShift);
         if Transparent then
            if (ddpfPixelFormat.dwRGBAlphaBitMask shr AlphaShift)>0 then
               AlphaMult:=255 div (ddpfPixelFormat.dwRGBAlphaBitMask shr AlphaShift);

         GetMem(buf, rowSize);
         for j:=0 to Height-1 do begin
            Stream.Read(buf[0], rowSize);
            for i:=0 to Width-1 do begin
               col:=@buf[ddsPixelSize*i];
               PByteArray(ScanLine[j])^[imgPixelSize*i+0]:=
                  BlueMult*(col^ and ddpfPixelFormat.dwBBitMask) shr BlueShift;
               PByteArray(ScanLine[j])^[imgPixelSize*i+1]:=
                  GreenMult*(col^ and ddpfPixelFormat.dwGBitMask) shr GreenShift;
               PByteArray(ScanLine[j])^[imgPixelSize*i+2]:=
                  RedMult*(col^ and ddpfPixelFormat.dwRBitMask) shr RedShift;            if Transparent then begin
               if Transparent then
                  PByteArray(ScanLine[j])^[imgPixelSize*i+3]:=
                     AlphaMult*(col^ and ddpfPixelFormat.dwRGBAlphaBitMask) shr AlphaShift;            end;
            end;
         end;
         FreeMem(buf);
      end;
   end;
end;

// SaveToStream
//
procedure TDDSImage.SaveToStream(stream : TStream);
var
//   magic : TFOURCC;
   magic : cardinal;
   header : TDDSHeader;
   i, rowSize : Integer;
begin
   FillChar(header, SizeOf(TDDSHeader), 0);
//   magic:='DDS ';
   magic:=$20534444  ; //'DDS '
//   header.magic:=Cardinal(magic);
   header.magic:= magic;
   with header.SurfaceFormat do begin
      dwSize:=124;
      dwFlags:=DDSD_CAPS +
               DDSD_PIXELFORMAT +
               DDSD_WIDTH +
               DDSD_HEIGHT +
               DDSD_PITCH;
      dwWidth:=Width;
      dwHeight:=Height;
      case PixelFormat of
         {$IFDEF MSWINDOWS}
         {gl}pf24bit : begin
            ddpfPixelFormat.dwFlags:=DDPF_RGB;
            ddpfPixelFormat.dwRGBBitCount:=24;
            ddpfPixelFormat.dwRBitMask:=$00FF0000;
            ddpfPixelFormat.dwGBitMask:=$0000FF00;
            ddpfPixelFormat.dwBBitMask:=$000000FF;
         end;
         {$ENDIF}
         {gl}pf32bit : begin
            ddpfPixelFormat.dwFlags:=DDPF_RGB;
            ddpfPixelFormat.dwRGBBitCount:=32;
            ddpfPixelFormat.dwRBitMask:=$00FF0000;
            ddpfPixelFormat.dwGBitMask:=$0000FF00;
            ddpfPixelFormat.dwBBitMask:=$000000FF;
            if Transparent then begin
               ddpfPixelFormat.dwFlags:=ddpfPixelFormat.dwFlags + DDPF_ALPHAPIXELS;
               ddpfPixelFormat.dwRGBAlphaBitMask:=$FF000000;
            end;
         end;
      else
         raise EDDSException.Create('Unsupported pixel format format');
      end;
      rowSize:=(ddpfPixelFormat.dwRGBBitCount div 8)*dwWidth;
      dwPitchOrLinearSize:=dwHeight*Cardinal(rowSize);
      ddsCaps.dwCaps1:=DDSCAPS_TEXTURE;
      stream.Write(header, SizeOf(TDDSHeader));
      for i:=0 to Height-1 do
         stream.Write(ScanLine[i]^, rowSize);
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

 //  TGLPicture.RegisterFileFormat(
   TPicture.RegisterFileFormat(
     'dds', 'Microsoft DirectDraw Surface', TDDSImage);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
finalization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

 //  TGLPicture.UnregisterGraphicClass(TDDSImage);
   TPicture.UnregisterGraphicClass(TDDSImage);

end.

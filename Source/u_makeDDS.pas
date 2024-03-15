{
 * u_MakeDDS.pas
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
unit u_MakeDDS;

{----------------------------------------------------------------------------
Convert files to DDS format
----------------------------------------------------------------------------}
interface

uses StdCtrls, FileCtrl, SysUtils;

var
  Memo_Message : TMemo;       // external TMemo for messages
  DDSFolder : string;         // external path for file output
  GDALlibraryFolder : string; // external path for library
  CompressorFolder : string;  // external path for Texture Compressor Folder
//  ZoomLevel : string;
//  OutputTileSize : string;
//  File_Destination : string;
  DXT_Type : string;

Procedure MakeDDSbatchFile(TileIndex : integer);
Procedure MakeDDS_All_BatchFile;
Procedure MakeDDSquarterTile(CurrentRow, CurrentColumn, offset_Row, offset_Column : Integer);
Procedure MakeDDS_Generic(Name, FilePath, FileName : string);
Procedure MakeDDS_Object_Drop(FilePath, FileName : string);

//----------------------------------------------------------------------------
implementation

uses Math,
  u_TileList, u_GMIDlog, u_SceneryHDR, u_BMP;

type
  DXT_Generator = (g_nVDXT,g_CompressonatorCLI);

var
  DDSfile : TextFile;
  DXT_Gen : DXT_Generator;

{----------------------------------------------------------------------------}
Procedure MessageShow(Info : string);
begin
  if (Memo_Message <> nil) then begin
    Memo_Message.lines.add(Info);
  end;
end;

// 2^(n-1)
//-------------------------------------------------------------------------------------
function xConvertNumMips(BitmapWidth : integer) : string;
begin
  case BitmapWidth of
    256: begin
      result := '9';
    end;
    512: begin
      result := '10';
    end;
    1024: begin
      result := '11';
    end;
    2048: begin
      result := '12';
    end;
    4096: begin
      result := '13';
    end;
    8192: begin
      result := '14';
    end;
    else begin
      result := '1';
    end;
  end;
end;

//-------------------------------------------------------------------------------------
function ConvertNumMips(BitmapWidth : integer) : string;
begin
  result := IntToStr(round(log2(BitmapWidth)) + 1);
end;

//-------------------------------------------------------------------------------------
Procedure MakeDDSbatchFile(TileIndex : integer);

var
  TileName : string;
  ErrorCode : integer;
  TileRow, TileColumn : integer;
  i, j : integer;
//  NumMips : string;
  FileName : string;
  FilePath : string;
  SourcePath : string;
//  Algo : string;  // nmip image shrink algorithm

begin
  TileName := TileList[TileIndex].TileName;
  FilePath := DDSfolder +'\SourceTiles\'+ TileName;
  ForceDirectories(FilePath);

  // check for DXT generator
  if (FileExists(CompressorFolder+'\nvDXT.exe')) then begin
    DXT_Gen := g_nVDXT;
  end else begin
    if (FileExists(CompressorFolder+'\CompressonatorCLI.exe')) then begin
      DXT_Gen := g_CompressonatorCLI;
    end else begin
      MessageShow('Select nvDXT path or Compressonator path');
      Beep;
      Exit;
    end;
  end;

  //open the file
  FileName := 'DDS_'+TileName+'.bat';
  AssignFile(DDSfile, FilePath +'\'+ FileName);
  Rewrite(DDSfile);

  writeln(DDSfile,'@echo off');
  writeln(DDSfile,'setlocal');
  writeln(DDSfile,'set PATH=%PATH%;"'+GDALlibraryFolder+'"');
//  writeln(DDSfile,'set PATH=%PATH%;"'+CondorFolder+'\Tools"');
  writeln(DDSfile,'set PATH=%PATH%;"'+CompressorFolder+'"');
//set PATH=%PATH%;"C:\Compressonator_4.1.5083\bin\CLI"
  writeln(DDSfile,'rem goto directory where batch file is');
  writeln(DDSfile,'cd /d %~dp0');

  SourcePath := '..\..\Terragen\Textures\';
//  writeln(DDSfile,'set sourcebmp='+SourcePath+TileName+'.bmp');
  writeln(DDSfile,'set sourcebmp='+SourcePath+TileName);
  writeln(DDSfile,'if exist %sourcebmp%.tif (set fext=.tif) else (set fext=.bmp)');
  writeln(DDSfile,'set sourcebmp=%sourcebmp%%fext%');

  writeln(DDSfile,'if NOT exist %sourcebmp% (echo ERROR: %sourcebmp% NOT found & pause & exit /b 9)');
{
  // read bitmap size
  BMPfolder := FilePath;
  BitmapWidth := 1; // dummy to for manual insert later.
  Bitmap_GetWidthHeight(SourcePath+TileName+'.bmp');
  if (BitmapSuccess) then begin
    BitmapWidth := BitmapWidth div 4;
  end else begin
    BitmapWidth := strtoint(OutputTileSize) div 4;
  end;
}
  // find bitmap size
  writeln(DDSfile,'gdalinfo -nomd -norat -noct %sourcebmp% >info.txt');
  writeln(DDSfile,'set FileName=info.txt');
  writeln(DDSfile,'for /f "tokens=2  delims=," %%a in (''find "Size is " %FileName%'') do set bSize=%%a');
  writeln(DDSfile,'set /a BitmapWidth=bSize/4');
  writeln(DDSfile,'set /a b0=bSize/4*0');
  writeln(DDSfile,'set /a b1=bSize/4*1');
  writeln(DDSfile,'set /a b2=bSize/4*2');
  writeln(DDSfile,'set /a b3=bSize/4*3');

  // use -triangle for 16k bitmap as -cubic will fail
  writeln(DDSfile,'set algo=-cubic');
  writeln(DDSfile,'if %bSize% EQU 16384 set algo=-triangle');

{
  // convert size to NumMips
//  NumMips := 'nmips';
  NumMips := ConvertNumMips(BitmapWidth);
 // if not specified, nvdxt defaults to max, so no need
}
  Val(copy(TileName,1,2),TileColumn,ErrorCode);
  Val(copy(TileName,3,2),TileRow,ErrorCode);
// if errorcode or not in range -> error

  writeln(DDSfile,'mkdir TEMP');

  for i := 0 to 4-1 do begin
    for j := 0 to 4-1 do begin
      TileName := format('t%2.2d%2.2d',[TileColumn*4+i,TileRow*4+j]);
      writeln(DDSfile,'set destinationbmp='+'TEMP\'+TileName+'%fext%');
      writeln(DDSfile,'set destinationdds='+'TEMP\'+TileName+'.dds');
//      writeln(DDSfile,'gdal_translate -of BMP -srcwin '+
      writeln(DDSfile,'gdal_translate -srcwin '+
//       inttostr((3-i)*BitmapWidth) +' '+ inttostr((3-j)*BitmapWidth) +' '+
//       inttostr(BitmapWidth) +' '+ inttostr(BitmapWidth) +' '+
        format('%%b%d%% %%b%d%% %%BitmapWidth%% %%BitmapWidth%% ',[3-i,3-j]) +
        '%sourcebmp% %destinationbmp%');
      if (DXT_Gen = g_nVDXT) then begin
//        writeln(DDSfile,'nvDXT.exe -quality_highest %algo% -dxt1c -outdir "TEMP" -file %destinationbmp%');
//        writeln(DDSfile,'nvDXT.exe -quality_highest %algo% -dxt3 -outdir "TEMP" -file %destinationbmp%');
        if (DXT_Type = 'DXT1') then begin
          writeln(DDSfile,'nvDXT.exe -quality_highest %algo% -'+LowerCase(DXT_Type)+'c -outdir "TEMP" -file %destinationbmp%');
        end else begin
          writeln(DDSfile,'nvDXT.exe -quality_highest %algo% -'+LowerCase(DXT_Type)+' -outdir "TEMP" -file %destinationbmp%');
        end;
      end else begin // else must be compressonator
        writeln(DDSfile,'CompressonatorCLI.exe -fd '+DXT_Type+' -mipsize 1 -CompressionSpeed 0 %destinationbmp% %destinationdds%');
//        writeln(DDSfile,'CompressonatorCLI.exe -fd '+DXT_Type+' -DXT1UseAlpha 1 -AlphaThreshold 192 -mipsize 1 -CompressionSpeed 0 %destinationbmp% %destinationdds%');
      end;
      writeln(DDSfile,'del %destinationbmp%');
      writeln(DDSfile,'move %destinationdds% ' + '..\..\..\Textures');
    end;
  end;

  writeln(DDSfile,'endlocal');

  // close the file
  Close(DDSfile);

  MessageShow(FileName+' done ('+DXT_Type+').');
end;

//-------------------------------------------------------------------------------------
Procedure MakeDDSquarterTile(CurrentRow, CurrentColumn, offset_Row, offset_Column : Integer);

var
//  i : integer;
  FileName : string;
  FilePath : string;
//  NumMips : string;
  TileIndex : integer;
  TileName : string;
  TextureName : string;
//  Tile_B_Lat : double;
//  Tile_T_Lat : double;
//  Tile_L_Long : double;
//  Tile_R_Long : double;

begin
  // check for DXT generator
  if (FileExists(CompressorFolder+'\nvDXT.exe')) then begin
    DXT_Gen := g_nVDXT;
  end else begin
    if (FileExists(CompressorFolder+'\CompressonatorCLI.exe')) then begin
      DXT_Gen := g_CompressonatorCLI;
    end else begin
      MessageShow('Select nvDXT path or Compressonator path');
      Beep;
      Exit;
    end;
  end;

  TileIndex := CurrentRow * (TileColumnCount+1) + CurrentColumn;

  FilePath := DDSfolder +'\SourceTiles\'+ TileList[TileIndex].TileName +'\QuarterTiles';
  // create path
  ForceDirectories(FilePath);

  TileName := TileList[TileIndex].TileName+format('_%2.2d_%2.2d',[offset_Column,offset_Row]);
  TextureName := format('t%2.2d%2.2d',[CurrentColumn*4+offset_Column,CurrentRow*4+offset_Row]);
  //open the file
  FileName := 'DDS_'+TileName+'.bat';
  AssignFile(DDSfile, FilePath +'\'+ FileName);
  Rewrite(DDSfile);

  writeln(DDSfile,'@echo off');
  writeln(DDSfile,'setlocal');
//  writeln(DDSfile,'set PATH=%PATH%;"'+CondorFolder+'\Tools"');
  writeln(DDSfile,'set PATH=%PATH%;"'+CompressorFolder+'"');
  writeln(DDSfile,'rem goto directory where batch file is');
  writeln(DDSfile,'cd /d %~dp0');

  writeln(DDSfile,'rem converts .bmp file into .dds in "dds" folder');
//  writeln(DDSfile,'set sourcebmp='+TileName+'.bmp');
  writeln(DDSfile,'set sourcebmp='+TextureName+'.bmp');
//  writeln(DDSfile,'set destinationdds='+'dds\'+TileName+'.dds');
  writeln(DDSfile,'set destinationdds='+TextureName+'.dds');

  writeln(DDSfile,'if NOT exist %sourcebmp% (echo ERROR: %sourcebmp% NOT found & pause & exit /b 9)');
{
//  // 8 mipmaps, but probably should use 2^(n-1). For 1024 n = 11, for 2048 n = 12
//  NumMips := '8';
  // read bitmap size
  BMPfolder := FilePath;
  NumMips := 'nmips';
  Bitmap_GetWidthHeight(TileName+'.bmp');
  if (BitmapSuccess) then begin
    // convert size to NumMips
    NumMips := ConvertNumMips(BitmapWidth);
  end;
} // if not specified, nvdxt defaults to max, so no need
//  writeln(DDSfile,'nvDXT.exe -quality_highest -nmips '+NumMips+' -Cubic -dxt1c -outdir "dds" -file ',TextureName,'.bmp');
//  writeln(DDSfile,'nvDXT.exe -quality_highest -nmips '+NumMips+' -Cubic -dxt3 -outdir "dds" -file ',TextureName,'.bmp');
  if (DXT_Gen = g_nVDXT) then begin
    if (DXT_Type = 'DXT1') then begin
      writeln(DDSfile,'nvDXT.exe -quality_highest -Cubic -'+LowerCase(DXT_Type)+'c -outdir "dds" -file %sourcebmp%');
    end else begin
      writeln(DDSfile,'nvDXT.exe -quality_highest -Cubic -'+LowerCase(DXT_Type)+' -outdir "dds" -file %sourcebmp%');
    end;
  end else begin // must be Compressonator
    writeln(DDSfile,'CompressonatorCLI.exe -fd '+DXT_Type+' -mipsize 1 -CompressionSpeed 0 %sourcebmp% %destinationdds%');
  // if dxt1 with alpha
  //  writeln(DDSfile,'CompressonatorCLI.exe -fd '+DXT_Type+' -DXT1UseAlpha 1 -AlphaThreshold 192 -mipsize 1 -CompressionSpeed 0 %destinationbmp% %destinationdds%');
  end;

   writeln(DDSfile,'rem del %sourcebmp%');
   writeln(DDSfile,'rem move %destinationdds% ' + '..\..\..\..\Textures');

  writeln(DDSfile,'endlocal');

  // close the file
  Close(DDSfile);

  MessageShow(FileName+' done ('+DXT_Type+').');
end;

//-------------------------------------------------------------------------------------
Procedure MakeDDS_All_BatchFile;
var
  i,j : integer;
  FileName : string;
  FilePath : string;
  TileIndex : integer;
  Name : string;

begin
  // check for folder
  if (NOT DirectoryExists(DDSFolder)) then begin
    MessageShow('Destination Folder not found');
    exit;
  end;
  // create a folder if necessary
  FilePath := DDSFolder +'\SourceTiles';
  if (NOT DirectoryExists(FilePath)) then begin
    mkdir(FilePath);
  end;
  //open the file
  FileName := 'DDS_ALL.bat';
  AssignFile(DDSfile, FilePath +'\'+ FileName);
  Rewrite(DDSfile);

  writeln(DDSfile,'@echo off');
  writeln(DDSfile,'setlocal');
  writeln(DDSfile,'rem goto directory where batch file is');
  writeln(DDSfile,'cd /d %~dp0');
  for j := 0 to TileColumnCount-1 do begin
    for i := 0 to TileRowCount-1 do begin
      TileIndex := i*(TileColumnCount+1)+j;
      Name := TileList[TileIndex].TileName;
      // use || to execute next command if previous one failed
      writeln(DDSfile,'call '+Name+'\DDS_'+Name+'.bat || exit /b 9');
    end;
  end;
  writeln(DDSfile,'endlocal');

  // close the file
  Close(DDSfile);
  MessageShow(FileName+' done.');
end;

//-------------------------------------------------------------------------------------
Procedure MakeDDS_Generic(Name, FilePath, FileName : string);
begin
  // check for DXT generator
  if (FileExists(CompressorFolder+'\nvDXT.exe')) then begin
    DXT_Gen := g_nVDXT;
  end else begin
    if (FileExists(CompressorFolder+'\CompressonatorCLI.exe')) then begin
      DXT_Gen := g_CompressonatorCLI;
    end else begin
      MessageShow('Select nvDXT path or Compressonator path');
      Beep;
      Exit;
    end;
  end;

  //open the file
  AssignFile(DDSfile, FilePath +'\'+ FileName);
  Rewrite(DDSfile);

  writeln(DDSfile,'@echo off');
  writeln(DDSfile,'setlocal');
  writeln(DDSfile,'set PATH=%PATH%;"'+CompressorFolder+'"');
  writeln(DDSfile,'rem goto directory where batch file is');
  writeln(DDSfile,'cd /d %~dp0');

  writeln(DDSfile,'rem converts .bmp file into .dds');
  writeln(DDSfile,'set sourcebmp='+Name+'.bmp');
  writeln(DDSfile,'set destinationdds='+Name+'.dds');

  writeln(DDSfile,'if NOT exist %sourcebmp% (echo ERROR: %sourcebmp% NOT found & pause & exit /b 9)');
 // if not specified, nvdxt defaults to max, so no need
//  writeln(DDSfile,'nvDXT.exe -quality_highest -nmips '+NumMips+' -Cubic -dxt1c -outdir "dds" -file ',TextureName,'.bmp');
//  writeln(DDSfile,'nvDXT.exe -quality_highest -nmips '+NumMips+' -Cubic -dxt3 -outdir "dds" -file ',TextureName,'.bmp');
  if (DXT_Gen = g_nVDXT) then begin
    if (DXT_Type = 'DXT1') then begin
      writeln(DDSfile,'nvDXT.exe -quality_highest -Cubic -'+LowerCase(DXT_Type)+'c -file %sourcebmp%');
    end else begin
      writeln(DDSfile,'nvDXT.exe -quality_highest -Cubic -'+LowerCase(DXT_Type)+' -file %sourcebmp%');
    end;
  end else begin // must be Compressonator
    writeln(DDSfile,'CompressonatorCLI.exe -fd '+DXT_Type+' -mipsize 1 -CompressionSpeed 0 %sourcebmp% %destinationdds%');
  // if dxt1 with alpha
  //  writeln(DDSfile,'CompressonatorCLI.exe -fd '+DXT_Type+' -DXT1UseAlpha 1 -AlphaThreshold 192 -mipsize 1 -CompressionSpeed 0 %destinationbmp% %destinationdds%');
  end;

  writeln(DDSfile,'rem del %sourcebmp%');

  writeln(DDSfile,'endlocal');

  // close the file
  Close(DDSfile);

  MessageShow(FileName+' done ('+DXT_Type+').');
end;

//-------------------------------------------------------------------------------------
Procedure MakeDDS_Object_Drop(FilePath, FileName : string);
begin
  // check for DXT generator
  if (FileExists(CompressorFolder+'\nvDXT.exe')) then begin
    DXT_Gen := g_nVDXT;
  end else begin
    if (FileExists(CompressorFolder+'\CompressonatorCLI.exe')) then begin
      DXT_Gen := g_CompressonatorCLI;
    end else begin
      MessageShow('Select nvDXT path or Compressonator path');
      Beep;
      Exit;
    end;
  end;

  //open the file
  AssignFile(DDSfile, FilePath +'\'+ FileName);
  Rewrite(DDSfile);

  writeln(DDSfile,'@echo off');
  writeln(DDSfile,'setlocal');
  writeln(DDSfile,'set PATH=%PATH%;"'+CompressorFolder+'"');
  writeln(DDSfile,'rem goto directory where batch file is');
  writeln(DDSfile,'cd /d %~dp0');

  writeln(DDSfile,'rem converts graphic file into .dds');
  writeln(DDSfile,'IF [%1] NEQ [] (set sourcebmp=%1) else (echo ERROR: Drag-and-Drop file needed & pause & exit /b 9)');
  writeln(DDSfile,'set destinationdds=%~p1\%~n1.dds');

  writeln(DDSfile,'if NOT exist %sourcebmp% (echo ERROR: %sourcebmp% NOT found & pause & exit /b 9)');
  if (DXT_Gen = g_nVDXT) then begin
    writeln(DDSfile,'nvDXT.exe -quality_highest -Cubic -dxt1c -file %sourcebmp%');
  end else begin // must be Compressonator
    writeln(DDSfile,'CompressonatorCLI.exe -fd dxt1 -mipsize 1 -CompressionSpeed 0 %sourcebmp% %destinationdds%');
  end;
  writeln(DDSfile,'rem del %sourcebmp%');
  writeln(DDSfile,'endlocal');
  // close the file
  Close(DDSfile);
end;

{----------------------------------------------------------------------------}
begin
  Memo_Message := nil;
end.

//--- End of file ---------------------------------------------------------------------

{
 * u_Object.pas
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
UNIT u_Object;

{----------------------------------------------------------------------------
Condor .OBJ file

Each object takes 152 ($98) bytes

Gams3.X object

- 20 bytes
  - 6B 6C E8 47 - floating-point= 119000.83593 relative UTM long from right
  - AB 19 F5 47 - floating-point= 125491.33593 relative UTM lat from bottom
  - 00 00 8A 42 - floating-point= 69.0 meters elevation
  - 00 00 80 3F - floating-point= 1.0 scale/size 0.02-26 ?
  - 00 00 00 00 - floating-point= 0.0 rotation radians   +/- ?

- Object Name string - 1+131 bytes max ?
  - 07 - length of name
  - 47 61 6D 73 33 2E 58 -> Gams3.X
  - rest don't care
----------------------------------------------------------------------------}

//===========================================================================
INTERFACE

uses
     StdCtrls,
     u_Terrain;

type
{  FloatSingleConvert =
    record
      case byte of
        0 : ( RealValue : single );
        1 : ( ByteValue : array[0..3] of byte );
      end;
}
  CondorObject =
    record
      coEasting   : single;
      coNorthing  : single;
      coElevation : single;
      coScale     : single;
      coRotation  : single;
      coName      : string[131];
    end;

var
  Memo_Message : TMemo;  // external TMemo for messages

  lObjectFolderName : String;
  lObjectFileName   : String;
  Object_Count      : integer;
  Object_List       : array of CondorObject;

procedure ReadObjectFile;
procedure WriteObjectFile;
procedure ExportCSV_ObjectFile;    // relative UTM
procedure ExportCSV_LL_ObjectFile; // absolute Latitude/Longitude
procedure ImportCSV_ObjectFile;    // relative UTM
procedure ImportCSV_LL_ObjectFile; // absolute Latitude/Longitude
procedure Append_OBJ_File(Offset_X, Offset_Y, Min_X, Max_X, Min_Y, Max_Y : single;
                          FilePath,Filename,
                          FilePath_a,Filename_a : string);
procedure List_OBJ_File_Object_Details(FilePath,FileName : string);

//===========================================================================
IMPLEMENTATION

uses Windows, FileCtrl, SysUtils,
     u_X_CX, u_UTM{, u_Condor_NaviconDLL};

var
  Object_File : File of CondorObject;

{----------------------------------------------------------------------------}
Procedure MessageShow(Info : string);
begin
  if assigned(Memo_Message) then begin
    Memo_Message.lines.add(Info);
  end;
end;

{----------------------------------------------------------------------------}
procedure ReadObjectFile;
begin
  Object_Count := 0;
  AssignFile(Object_File,lObjectFolderName+lObjectFileName);
  Reset(Object_File);
  While NOT EOF(Object_File) do begin
    SetLength(Object_List,Object_Count+1);
    Read(Object_File,Object_list[Object_Count]);
    INC(Object_Count);
  end;

  Close(Object_File);
end;

{----------------------------------------------------------------------------}
procedure WriteObjectFile;
var
  i : integer;

begin
  AssignFile(Object_File,lObjectFolderName+lObjectFileName);
  Rewrite(Object_File);
//  for i := 0 to Object_Count-1 do begin
  for i := 0 to Length(Object_list)-1 do begin
    write(Object_File,Object_list[i]);
  end;

  Close(Object_File);
end;

{----------------------------------------------------------------------------}
procedure ExportCSV_ObjectFile; // relative UTM
var
  i : integer;
  CSV_File : TextFile;

begin
  if (NOT DirectoryExists(lObjectFolderName+'Working')) then begin
    ForceDirectories(lObjectFolderName+'Working');
  end;
  AssignFile(CSV_File,lObjectFolderName+'Working\'+lObjectFileName+'.csv');
  Rewrite(CSV_File);
  for i := 0 to Object_Count-1 do begin
    with Object_list[i] do begin
      writeln(CSV_File,format('%s,%1.6f,%1.6f,%1.6f,%1.6f,%1.6f',[
        coName, coEasting, coNorthing, coElevation, coScale, coRotation
      ]));
    end;
  end;

  Close(CSV_File);
end;

{----------------------------------------------------------------------------}
procedure ImportCSV_ObjectFile; // relative UTM
var
  CSV_File : TextFile;
  Input : string;
  CommaPos : integer;
  conv_Latitude : single;
  conv_Longitude : single;

begin
  AssignFile(CSV_File,lObjectFolderName+'\Working\'+lObjectFileName+'.csv');
  Reset(CSV_File);

  Object_Count := 0;
  SetLength(Object_List,Object_Count); // in case file is empty
  While NOT EOF(CSV_File) do begin
    SetLength(Object_List,Object_Count+1);
    with Object_list[Object_Count] do begin // coName, coEasting, coNorthing, coElevation, coScale, coRotation
      readln(CSV_File, Input);
      CommaPos := pos(',',Input);
      if (CommaPos <> 0) then begin
        coName := copy(Input,1,CommaPos-1);
        Input := copy(Input, CommaPos+1, length(Input));
      end;
      CommaPos := pos(',',Input);
      if (CommaPos <> 0) then begin
        coEasting := strToFloat(copy(Input,1,CommaPos-1));
        Input := copy(Input, CommaPos+1, length(Input));
      end;
      CommaPos := pos(',',Input);
      if (CommaPos <> 0) then begin
        coNorthing := strToFloat(copy(Input,1,CommaPos-1));
        Input := copy(Input, CommaPos+1, length(Input));
      end;
      CommaPos := pos(',',Input);
      if (CommaPos <> 0) then begin
        coElevation := strToFloat(copy(Input,1,CommaPos-1));
        Input := copy(Input, CommaPos+1, length(Input));
      end;
      CommaPos := pos(',',Input);
      if (CommaPos <> 0) then begin
        coScale := strToFloat(copy(Input,1,CommaPos-1));
        Input := copy(Input, CommaPos+1, length(Input));
      end;
      coRotation := strToFloat(Input);
    end;
    INC(Object_Count);
  end;
  WriteObjectFile;

  Close(CSV_File);
end;

{----------------------------------------------------------------------------}
{procedure xExportCSV_LL_ObjectFile; // absolute Latitude/Longitude
var
  i : integer;
  CSV_File : TextFile;
  conv_Latitude : single;
  conv_Longitude : single;

begin
  AssignFile(CSV_File,lObjectFolderName+'Working\'+lObjectFileName+'.LL.csv');
  Rewrite(CSV_File);
  for i := 0 to Object_Count-1 do begin
    with Object_list[i] do begin
      // convert to lat/long, using Condor navicon.dll
      conv_Longitude := Condor_Navicon_XYToLon(coEasting,coNorthing);
      conv_Latitude  := Condor_Navicon_XYToLat(coEasting,coNorthing);
      writeln(CSV_File,format('%s,%1.7f,%1.7f,%1.6f,%1.6f,%1.6f',[
        coName, conv_Longitude, conv_Latitude, coElevation, coScale, coRotation
      ]));
    end;
  end;

  Close(CSV_File);
end;
}
{----------------------------------------------------------------------------}
procedure ExportCSV_LL_ObjectFile; // absolute Latitude/Longitude
var
  i : integer;
  CSV_File : TextFile;

begin
  if (NOT DirectoryExists(lObjectFolderName+'Working')) then begin
    ForceDirectories(lObjectFolderName+'Working');
  end;
  AssignFile(CSV_File,lObjectFolderName+'Working\'+lObjectFileName+'.LL.csv');
  Rewrite(CSV_File);
  for i := 0 to Object_Count-1 do begin
    with Object_list[i] do begin
      with TerrainHeader do begin
//        UTMtoLatLong(tBottomMapNorthing+coNorthing, tRightMapEasting-coEasting, IntToStr(tUTMzone), tUTMgrid[0]);
        UTMtoLatLong(tBottomMapNorthing+coNorthing*tDeltaY/90.0, tRightMapEasting+coEasting*tDeltaX/90.0, IntToStr(tUTMzone), tUTMgrid[0]);
      end;
      writeln(CSV_File,format('%s,%1.7f,%1.7f,%1.6f,%1.6f,%1.6f',[
        coName, uLongitude, uLatitude, coElevation, coScale, coRotation
      ]));
    end;
  end;

  Close(CSV_File);
end;

{----------------------------------------------------------------------------}
procedure ImportCSV_LL_ObjectFile; // absolute Latitude/Longitude
var
  CSV_File : TextFile;
  Input : string;
  CommaPos : integer;

begin
  AssignFile(CSV_File,lObjectFolderName+'\Working\'+lObjectFileName+'.LL.csv');
  Reset(CSV_File);

  Object_Count := 0;
  SetLength(Object_List,Object_Count); // in case file is empty
  While NOT EOF(CSV_File) do begin
    SetLength(Object_List,Object_Count+1);
    with Object_list[Object_Count] do begin // coName, coEasting, coNorthing, coElevation, coScale, coRotation
      readln(CSV_File, Input);
      CommaPos := pos(',',Input);
      if (CommaPos <> 0) then begin
        coName := copy(Input,1,CommaPos-1);
        Input := copy(Input, CommaPos+1, length(Input));
      end;
      CommaPos := pos(',',Input);
      if (CommaPos <> 0) then begin
        uLongitude := strToFloat(copy(Input,1,CommaPos-1));
        Input := copy(Input, CommaPos+1, length(Input));
      end;
      CommaPos := pos(',',Input);
      if (CommaPos <> 0) then begin
        uLatitude := strToFloat(copy(Input,1,CommaPos-1));
        Input := copy(Input, CommaPos+1, length(Input));
      end;
      // convert lat/long to relative UTM
      with TerrainHeader do begin
        LatLongToUTM(uLatitude,uLongitude,IntToStr(tUTMzone), tUTMgrid[0]);
        coNorthing := (uNorthing - tBottomMapNorthing)*90.0/tDeltaY;
        coEasting := (uEasting - tRightMapEasting)*90.0/tDeltaX;
      end;
      CommaPos := pos(',',Input);
      if (CommaPos <> 0) then begin
        coElevation := strToFloat(copy(Input,1,CommaPos-1));
        Input := copy(Input, CommaPos+1, length(Input));
      end;
      CommaPos := pos(',',Input);
      if (CommaPos <> 0) then begin
        coScale := strToFloat(copy(Input,1,CommaPos-1));
        Input := copy(Input, CommaPos+1, length(Input));
      end;
      coRotation := strToFloat(Input);
    end;
    INC(Object_Count);
  end;
  WriteObjectFile;

  Close(CSV_File);
end;

{----------------------------------------------------------------------------}
procedure Append_OBJ_File(Offset_X, Offset_Y,Min_X, Max_X, Min_Y, Max_Y : single;
                          FilePath,Filename,
                          FilePath_a,Filename_a : string);
var
  OBJ_File : File of CondorObject;
  OBJ_File_a : File of CondorObject;
  ObjectFileName : string;
  ObjectFileName_a : string;
  HeapStat: THeapStatus;
begin
  SetLength(Object_List,1); // only need space for one at a time
  AssignFile(OBJ_File,FilePath+'\'+Filename+'.obj');
  if (NOT FileExists(FilePath+'\'+Filename+'.obj')) then begin
    Rewrite(OBJ_File);
  end else begin
    Reset(OBJ_File);
    // go to end of file
//    SeekEOF(OBJ_File);
    While not EOF(OBJ_File) do begin
      Read(OBJ_File,Object_list[0]);
    end;
  end;

  if (NOT FileExists(FilePath_a+'\'+Filename_a+'.obj')) then begin
//    MessageShow('Warning: '+Filename_a+'.obj file not found');
    Beep;
  end else begin
    AssignFile(OBJ_File_a,FilePath_a+'\'+Filename_a+'.obj');
    Reset(OBJ_File_a);

    with Object_list[0] do begin
      While not EOF(OBJ_File_a) do begin
        Read(OBJ_File_a,Object_list[0]);

        // adjust UTM coords
        coEasting  := coEasting + (-Offset_X) - Min_X;
        coNorthing := coNorthing + Offset_Y - Min_Y;

        if (coEasting > (Max_X-Min_X)) then begin
          continue;
        end;
        if (coEasting < 0) then begin
          continue;
        end;
        if (coNorthing > (Max_Y-Min_Y)) then begin
          continue;
        end;
        if (coNorthing < 0) then begin
          continue;
        end;

        // read Object file and copy textures files
        with Object_list[0] do begin
          ForceDirectories(FilePath+'\World\Objects');

          ObjectFileName_a := FilePath_a+'\World\Objects\'+coName;
          if (FileExists(ObjectFileName_a)) then begin

            ObjectFileName := FilePath+'\World\Objects\'+coName;
            // only copy if not done already
            if (NOT FileExists(ObjectFileName)) then begin
              CopyFile(pchar(ObjectFileName_a),
                pchar(ObjectFileName),false);

//  HeapStat := GetHeapStatus;
//  MessageShow(format('Heap: %s %d',[coName,HeapStat.TotalFree]));
  MessageShow(format('Object: %s',[coName]));

              ReadCondorC3Dfile(ObjectFileName);
              // Need to copy textures for this object
              CopyObjectTextures(FilePath,Filename,
                                 FilePath_a,Filename_a,
                                 'World\Objects');
              // update if changed
              WriteCondorC3Dfile(ObjectFileName);
            end;
          end;
        end;

        Write(OBJ_File,Object_list[0]);
      end;
    end;
    CloseFile(OBJ_File_a);
  end;
  CloseFile(OBJ_File);
end;

{----------------------------------------------------------------------------}
procedure List_OBJ_File_Object_Details(FilePath,FileName : string);
var
//  OBJ_File : File of CondorObject;
  objFileName : string;
//  ObjectFileName : string;
  SearchRec: TSearchRec;

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
Procedure Extract_Details;
var
  i : integer;
begin
  Append_C3D_Details(objFileName,FilePath+'\Working\'+'World_Object_Details.csv');
end;

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
begin
// based on the whole object list which has many duplicates
{
  SetLength(Object_List,1); // only need space for one at a time

  AssignFile(OBJ_File,FilePath+'\'+Filename+'.obj');
  if (NOT FileExists(FilePath+'\'+Filename+'.obj')) then begin
//    MessageShow('Warning: '+Filename+'.obj file not found');
    Beep;
    Exit;
  end else begin
    // new CSV file
    if (NOT DirectoryExists(FilePath+'Working')) then begin
      ForceDirectories(FilePath+'Working');
    end;
    DeleteFile(FilePath+'\Working\'+'World_Object_Details.csv');

    Reset(OBJ_File);
    While not EOF(OBJ_File) do begin
      Read(OBJ_File,Object_list[0]);

      // read all files and extract details
      with Object_list[0] do begin
        // look for O file
	objFileName := coName;
        ObjectFileName := FilePath+'\World\Objects\'+objFileName;
        if (FileExists(ObjectFileName)) then begin
          ReadCondorC3Dfile(ObjectFileName);
	  Extract_Details();
        end;
      end;
    end;
  end;
  CloseFile(OBJ_File);
}
// based on scanning the World folder for objects
    // new CSV file
    if (NOT DirectoryExists(FilePath+'\Working')) then begin
      ForceDirectories(FilePath+'\Working');
    end;
    DeleteFile(FilePath+'\Working\'+'World_Object_Details.csv');

  if (FindFirst(FilePath+'\World\Objects\'+'\*.c3d', faDirectory {faAnyFile}, SearchRec)) = 0 then begin
    if ((SearchRec.Name <> '.') AND (SearchRec.Name <> '..')) then begin
      objFileName := SearchRec.Name;
      ReadCondorC3Dfile(FilePath+'\World\Objects\'+objFileName);
      Extract_Details();
    end;

    while (FindNext(SearchRec) = 0) do begin
      if ((SearchRec.Name <> '.') AND (SearchRec.Name <> '..')) then begin
        objFileName := SearchRec.Name;
        ReadCondorC3Dfile(FilePath+'\World\Objects\'+objFileName);
	Extract_Details();
      end;
    end;
    FindClose(SearchRec);
  end;
end;

{----------------------------------------------------------------------------}
begin { Initialization }
end.

{--- End of File ------------------------------------------------------------}


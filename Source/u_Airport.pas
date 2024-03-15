{
 * u_Airport.pas
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
UNIT u_Airport;

{----------------------------------------------------------------------------
Each airport definition uses 72 ($48) bytes

Airport name string - 1+35 bytes max ?
  04 - airport name length
  52 56 53 53 -> RVSS
  rest 00

 87 69 34 42 - $346987 $42 byte 36 - Latitude - Floating point
 B5 41 97 C2 - $9741b5 $C2 byte 40 - Longitude - Floating point
 00 00 AE 42 - $AE0000 $42 byte 44 - altitude - Floating point

 F5 00 00 00 - $000000F5   byte 48 - direction
 97 04 00 00 - $00000497   byte 52 - length
 3E 00 00 00 - $0000003E   byte 56 - width
 00 00 00 00 - $00000000   byte 60 - no asphalt
               $00000001           - asphalt
 00 00 F7 42 - $F70000 $42 byte 64 - Frequency - Floating point
 00 00 00 00 - $00000000   byte 68 - no options
               $00000001           - primary dir reversed
               $00000100           - tow primary dir left side
               $00010000           - tow secondary dir left side

 V2 - direction - coded for two decimals 0..359.99
 - X = round(100000 + direction * 100)
 - Compatible with V1
   - if X >= 100000 then direction = (X-100000)/100
   - else direction = X (no decimals)

----------------------------------------------------------------------------}

//===========================================================================
INTERFACE

uses
  u_Terrain;

type
  FloatSingleConvert =
    record
      case byte of
        0 : ( RealValue : single );
        1 : ( ByteValue : array[0..3] of byte );
      end;

  CondorAirport =
    record
      apName : string[35];
      apLatitude : single;
      apLongitude : single;
      apAltitude : single;
      apDirection : longint;
      apLength : longint;
      apWidth : longint;
      apAsphaltFlag : longint;
      apFrequency : single;
      apOptions : longint;
    end;

var
  Airport_FolderName : String;
  Airport_FileName : String;
  Airport_Count : integer;
  Airport_File : File of CondorAirport;
  Airport_List : array of CondorAirport;

procedure ReadAirportFile;
procedure WriteAirportFile;
procedure ExportCSV_AirportFile;
procedure ImportCSV_AirportFile;
procedure Append_APT_File(UTM_Limits : Extents;
                          FilePath,Filename,
                          FilePath_a,Filename_a : string);
procedure List_APT_File_Object_Details(FilePath,Filename : string);

//===========================================================================
IMPLEMENTATION

uses Windows, FileCtrl, SysUtils,
  u_UTM, u_X_CX;

{----------------------------------------------------------------------------}
procedure ReadAirportFile;
begin
  Airport_Count := 0;
  AssignFile(Airport_File,Airport_FolderName+'\'+Airport_FileName);
  Reset(Airport_File);
  While NOT EOF(Airport_File) do begin
    SetLength(Airport_List,Airport_Count+1);
    Read(Airport_File,Airport_list[Airport_Count]);
    INC(Airport_Count);
  end;

  Close(Airport_File);
end;

{----------------------------------------------------------------------------}
procedure WriteAirportFile;
var
  i : integer;

begin
  AssignFile(Airport_File,Airport_FolderName+'\'+Airport_FileName);
  Rewrite(Airport_File);
  For i := 0 to Airport_Count-1 do begin
    Write(Airport_File,Airport_list[i]);
  end;

  Close(Airport_File);
end;

{----------------------------------------------------------------------------}
procedure ExportCSV_AirportFile;
var
  Temp : string;
  i : integer;
  CSV_File : TextFile;

begin
  if (NOT DirectoryExists(Airport_FolderName+'\Working')) then begin
    ForceDirectories(Airport_FolderName+'\Working');
  end;
  AssignFile(CSV_File,Airport_FolderName+'\Working\'+Airport_FileName+'.csv');
  Rewrite(CSV_File);
  for i := 0 to Airport_Count-1 do begin
    with Airport_list[i] do begin
      if (apDirection >= 100000) then begin // V2
        Temp := format('%1.2f',[(apDirection-100000)/100.0]);
      end else begin // V1 compatible
        Temp := format('%d',[apDirection]);
      end;
      writeln(CSV_File,format('%s,%1.6f,%1.6f,%1.6f,%s,%d,%d,%d,%1.6f,%d',[
      apName, apLatitude, apLongitude, apAltitude,
      Temp, apLength, apWidth, apAsphaltFlag,
      apFrequency, apOptions
      ]));
    end;
  end;

  Close(CSV_File);
end;

{----------------------------------------------------------------------------}
procedure ImportCSV_AirportFile;
var
  Temp : single;
  CSV_File : TextFile;
  Input : string;
  CommaPos : integer;

begin
  AssignFile(CSV_File,Airport_FolderName+'\Working\'+Airport_FileName+'.csv');
  Reset(CSV_File);

  Airport_Count := 0;
  SetLength(Airport_List,Airport_Count); // in case file is empty
  While NOT EOF(CSV_File) do begin
    SetLength(Airport_List,Airport_Count+1);
    with Airport_list[Airport_Count] do begin
      // apName, apLatitude, apLongitude, apAltitude, apDirection, apLength, apWidth, apAsphaltFlag, apFrequency, apOptions
      readln(CSV_File, Input);
      CommaPos := pos(',',Input);
      if (CommaPos <> 0) then begin
        apName := copy(Input,1,CommaPos-1);
        Input := copy(Input, CommaPos+1, length(Input));
      end;
      CommaPos := pos(',',Input);
      if (CommaPos <> 0) then begin
        apLatitude := strToFloat(copy(Input,1,CommaPos-1));
        Input := copy(Input, CommaPos+1, length(Input));
      end;
      CommaPos := pos(',',Input);
      if (CommaPos <> 0) then begin
        apLongitude := strToFloat(copy(Input,1,CommaPos-1));
        Input := copy(Input, CommaPos+1, length(Input));
      end;
      CommaPos := pos(',',Input);
      if (CommaPos <> 0) then begin
        apAltitude := strToFloat(copy(Input,1,CommaPos-1));
        Input := copy(Input, CommaPos+1, length(Input));
      end;
      CommaPos := pos(',',Input);
      if (CommaPos <> 0) then begin
        Temp := strToFloat(copy(Input,1,CommaPos-1));
        Input := copy(Input, CommaPos+1, length(Input));
        if (frac(Temp) > 0.009) then begin // V2
          apDirection := round(Temp*100+100000);
        end else begin // V1 compatible
         apDirection := round(Temp);
        end;
      end;
      CommaPos := pos(',',Input);
      if (CommaPos <> 0) then begin
        apLength := strToInt(copy(Input,1,CommaPos-1));
        Input := copy(Input, CommaPos+1, length(Input));
      end;
      CommaPos := pos(',',Input);
      if (CommaPos <> 0) then begin
        apWidth := strToInt(copy(Input,1,CommaPos-1));
        Input := copy(Input, CommaPos+1, length(Input));
      end;
      CommaPos := pos(',',Input);
      if (CommaPos <> 0) then begin
        apAsphaltFlag := strToInt(copy(Input,1,CommaPos-1));
        Input := copy(Input, CommaPos+1, length(Input));
      end;
      CommaPos := pos(',',Input);
      if (CommaPos <> 0) then begin
        apFrequency := strToFloat(copy(Input,1,CommaPos-1));
        Input := copy(Input, CommaPos+1, length(Input));
      end;
      apOptions := strToInt(Input);
    end;
    INC(Airport_Count);
  end;
  WriteAirportFile;

  Close(CSV_File);
end;

{----------------------------------------------------------------------------}
procedure Append_APT_File(UTM_Limits : Extents;
                          FilePath,Filename,
                          FilePath_a,Filename_a : string);
var
  APT_File : File of CondorAirport;
  APT_File_a : File of CondorAirport;
  ObjectFileName : string;
  ObjectFileName_a : string;

begin
  SetLength(Airport_List,1); // only need space for one at a time

  AssignFile(APT_File,FilePath+'\'+Filename+'.apt');
  if (NOT FileExists(FilePath+'\'+Filename+'.apt')) then begin
    Rewrite(APT_File);
  end else begin
    Reset(APT_File);
    // go to end of file
//    SeekEOF(APT_File);
    While not EOF(APT_File) do begin
      Read(APT_File,Airport_list[0]);
    end;
  end;

  if (NOT FileExists(FilePath_a+'\'+Filename_a+'.apt')) then begin
//    MessageShow('Warning: '+Filename_a+'.apt file not found');
    Beep;
  end else begin
    AssignFile(APT_File_a,FilePath_a+'\'+Filename_a+'.apt');
    Reset(APT_File_a);

    While not EOF(APT_File_a) do begin
      Read(APT_File_a,Airport_list[0]);

      // only keep airports within crop area
      with (Airport_list[0]) do begin
        LatLongToUTM(apLatitude, apLongitude, IntToStr(u_Terrain.TerrainHeader.tUTMzone), uGrid);
      end;
      if (uEasting > UTM_Limits.xMax) then begin
        continue;
      end;
      if (uEasting < UTM_Limits.xMin) then begin
        continue;
      end;
      if (uNorthing > UTM_Limits.yMax) then begin
        continue;
      end;
      if (uNorthing < UTM_Limits.yMin) then begin
        continue;
      end;

      Write(APT_File,Airport_list[0]);

      // read G and O files and copy textures files
      with Airport_list[0] do begin
        ForceDirectories(FilePath+'\Airports');

        // look for G file
        ObjectFileName_a := FilePath_a+'\Airports\'+apName+'G.c3d';
        if (FileExists(ObjectFileName_a)) then begin

          ObjectFileName := FilePath+'\Airports\'+apName+'G.c3d';
          CopyFile(pchar(ObjectFileName_a),
            pchar(ObjectFileName),false);

          ReadCondorC3Dfile(ObjectFileName, false);
          // Need to copy textures for this object
          CopyObjectTextures(FilePath,Filename,
                             FilePath_a,Filename_a,
                             'Airports');
          // update if changed
          WriteCondorC3Dfile(ObjectFileName);
        end;

        // look for O file
        ObjectFileName_a := FilePath_a+'\Airports\'+apName+'O.c3d';
        if (FileExists(ObjectFileName_a)) then begin

          ObjectFileName := FilePath+'\Airports\'+apName+'O.c3d';
          CopyFile(pchar(ObjectFileName_a),
            pchar(ObjectFileName),false);

          ReadCondorC3Dfile(ObjectFileName, false);
          // Need to copy textures for this object
          CopyObjectTextures(FilePath,Filename,
                             FilePath_a,FileName_a,
                             'Airports');
          // update if changed
          WriteCondorC3Dfile(ObjectFileName);
        end;
      end;
    end;
    CloseFile(APT_File_a);
  end;
  CloseFile(APT_File);
end;

{----------------------------------------------------------------------------}
procedure List_APT_File_Object_Details(FilePath,FileName : string);
var
  APT_File : File of CondorAirport;
  aptFileName : string;
  ObjectFileName : string;

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
Procedure Extract_Details;
var
  i : integer;
begin
  Append_C3D_Details(aptFileName,FilePath+'\Working\'+'Airport_Object_Details.csv');
end;

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
begin
  SetLength(Airport_List,1); // only need space for one at a time

  AssignFile(APT_File,FilePath+'\'+Filename+'.apt');
  if (NOT FileExists(FilePath+'\'+Filename+'.apt')) then begin
//    MessageShow('Warning: '+Filename+'.apt file not found');
    Beep;
    Exit;
  end else begin
    // new CSV file
    if (NOT DirectoryExists(FilePath+'\Working')) then begin
      ForceDirectories(FilePath+'\Working');
    end;
    DeleteFile(FilePath+'\Working\'+'Airport_Object_Details.csv');

    Reset(APT_File);
    While not EOF(APT_File) do begin
      Read(APT_File,Airport_list[0]);

      // read G and O files and extract details
      with Airport_list[0] do begin

        // look for G file
	aptFileName := apName+'G.c3d';
        ObjectFileName := FilePath+'\Airports\'+aptFileName;
        if (FileExists(ObjectFileName)) then begin
          ReadCondorC3Dfile(ObjectFileName, false);
          Extract_Details();
        end;

        // look for O file
	aptFileName := apName+'O.c3d';
        ObjectFileName := FilePath+'\Airports\'+aptFileName;
        if (FileExists(ObjectFileName)) then begin
          ReadCondorC3Dfile(ObjectFileName, false);
	  Extract_Details();
        end;
      end;
    end;
  end;
  CloseFile(APT_File);
end;

{----------------------------------------------------------------------------}
begin { Initialization }
end.

{--- End of File ------------------------------------------------------------}


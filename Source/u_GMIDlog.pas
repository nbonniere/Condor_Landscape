{
 * u_GMIDlog.pas
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
UNIT u_GMIDlog;

{----------------------------------------------------------------------------
The GMID log file is generated by the download process.
- extract the overall lat/long extent of the combined bitmap
----------------------------------------------------------------------------}

{============================================================================}
INTERFACE

uses
  StdCtrls;

var
  SourceLeftLongitude : double;
  SourceRightLongitude : double;
  SourceBottomLatitude : double;
  SourceTopLatitude : double;
  SourceTileOpen : boolean;

  Memo_Message : TMemo;  // external TMemo for messages
  GMIDFolder : string;   // external path to file

Procedure ReadSourceBitmapExtents(TileIndex : integer);
Procedure xReadSourceBitmapExtents(FileName : string);

{============================================================================}
IMPLEMENTATION

uses
  SysUtils,
  u_Util, u_TileList;

Type
  FieldFoundType = (fSourceLeft,fSourceBottom,fSourceRight,fSourceTop);

var
  FieldFoundFlag : set of FieldFoundType;
  Log_File : TextFile;
  FileError : boolean;

{----------------------------------------------------------------------------}
Procedure MessageShow(Info : string);
begin
  if (Memo_Message <> nil) then begin
    Memo_Message.lines.add(Info);
  end;
end;

{----------------------------------------------------------------------------}
Procedure ReadLogFileLines;

Var
//  InputString  : String[255];
  InputString  : String;
//  PartialString  : String[255];
  PartialString  : String;
  ErrorCode   : Integer;
  EqualPosition : byte;
//  i : byte;

{----------------------------------------------------------------------------}
{Procedure ReadLine;
var
  Ch: Char;

begin
  InputString := '';
  Read(Log_File, Ch);
  while ((Ch <> char($0A)) AND (NOT Eof(Log_File))) do begin // look for linefeed
    if (Ch <> char($0D)) then begin // ignore CR
      InputString := InputString + Ch;
    end;
    Read(Log_File, Ch);
  end;
end;
}
{----------------------------------------------------------------------------}
begin
  FieldFoundFlag := [];
  while (NOT Eof(Log_File)) AND (NOT FileError) AND
        (FieldFoundFlag <> [fSourceLeft,fSourceBottom,fSourceRight,fSourceTop]) do begin
//    ReadLine;
    ReadLine(@Log_File,InputString);
    EqualPosition := pos('=',InputString);
    if (EqualPosition <> 0) then begin
      PartialString := trim(copy(InputString,1,EqualPosition-1));
      InputString := copy(InputString,EqualPosition+1,length(InputString));
      if copy(PartialString,1,23) = 'Left_Longitude_download' then begin
        VAL(InputString,SourceLeftLongitude,ErrorCode);
        MessageShow(format('Left longitude: %1.8f',[SourceLeftLongitude]));
        FieldFoundFlag := FieldFoundFlag + [fSourceLeft];
      end else begin
        if copy(PartialString,1,24) = 'Right_Longitude_download' then begin
          VAL(InputString,SourceRightLongitude,ErrorCode);
          MessageShow(format('Right Longitude: %1.8f',[SourceRightLongitude]));
          FieldFoundFlag := FieldFoundFlag + [fSourceRight];
        end else begin
          if copy(PartialString,1,21) = 'Top_Latitude_download' then begin
            VAL(InputString,SourceTopLatitude,ErrorCode);
            MessageShow(format('Top Latitude: %1.8f',[SourceTopLatitude]));
            FieldFoundFlag := FieldFoundFlag + [fSourceTop];
          end else begin
            if copy(PartialString,1,24) = 'Bottom_Latitude_download' then begin
              VAL(InputString,SourceBottomLatitude,ErrorCode);
              MessageShow(format('Bottom Latitude: %1.8f',[SourceBottomLatitude]));
              FieldFoundFlag := FieldFoundFlag + [fSourceBottom];
            end;
          end;
        end;
      end;
    end;
  end;
  FileError := (FieldFoundFlag <> [fSourceLeft,fSourceBottom,fSourceRight,fSourceTop]);
end;

{-----------------------------------------------------------------------------}
Procedure ReadSourceBitmapExtents(TileIndex : integer);
var
  FileName : string;
  FilePath : string;

begin
  FileError := False;  // assume for now
  SourceTileOpen := false;  // do first

  FilePath := GMIDfolder +'\SourceTiles\'+ TileList[TileIndex].TileName;

  //open the file
  FileName := TileList[TileIndex].TileName+'.gmid';
  if (NOT FileExists(FilePath +'\'+FileName)) then begin
    MessageShow('File Not Found');
    FileError := true;
  end else begin
    MessageShow('Reading bitmap extent file...');
    AssignFile(Log_File,FilePath +'\'+FileName);
    Reset(Log_File);
    ReadLogFileLines;
    Close(Log_File);

    if (FileError) then begin
      MessageShow('Error in bitmap extents file');
    end else begin
      SourceTileOpen := true;
    end;
  end;
  if (FileError) then begin
    Beep;
  end;
end;

{-----------------------------------------------------------------------------}
Procedure xReadSourceBitmapExtents(FileName : string);
begin
  FileError := False;  // assume for now
  SourceTileOpen := false;  // do first

  //open the file
  if (NOT FileExists(FileName)) then begin
    MessageShow('File Not Found');
    FileError := true;
  end else begin
    MessageShow('Reading bitmap extent file...');
    AssignFile(Log_File,FileName);
    Reset(Log_File);
    ReadLogFileLines;
    Close(Log_File);

    if (FileError) then begin
      MessageShow('Error in bitmap extents file');
    end else begin
      SourceTileOpen := true;
    end;
  end;
  if (FileError) then begin
    Beep;
  end;
end;

{----------------------------------------------------------------------------}
begin { Initialization }
  SourceTileOpen := false;
  Memo_Message := nil;
end.

{--- End of File ------------------------------------------------------------}


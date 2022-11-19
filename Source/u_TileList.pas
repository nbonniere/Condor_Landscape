{
 * u_TileList.pas
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

// TBD - use same sequence (col:row) as Condor tiles for cornerList ?
// 0 -> 0:0, bottom right
// 1 -> 0:1, top right
// 2 -> 1:0, bottom left
// 3 -> 1:1, top left

//---------------------------------------------------------------------------
UNIT u_TileList;

{----------------------------------------------------------------------------
Reads/writes a list of tile corners
Each line contains the right-hand bottom corner coordinates of the tile
There is an extra row and column for a phantom row and column to provide
the left and top coordinates of the last row and columns of tiles.
----------------------------------------------------------------------------}

//===========================================================================
INTERFACE

uses StdCtrls, comctrls, u_Util;

type
  TileRecord = record
    TileName      : string;
    TileUTMRight  : double;
    TileUTMBottom : double;
    TileLongRight : real;
    TileLatBottom : real;
  end;

var
  TileRowCount : integer;    // external
  TileColumnCount : integer; // external
  TileCount : Integer;
  TileOpen : boolean;
  TileList : array of TileRecord;
  CornerList : array[0..4-1] of TileRecord;
  RangeList : array[0..4-1] of TileRecord;
  scCentre : TileRecord;

  Memo_Message : TMemo;  // external TMemo for messages
  ProgressBar_Status : TProgressBar;
  CondorFolder : string; // external path for Condor program folder

//procedure MakeTileList;
procedure MakeTileList(Easting, Northing : double);
Procedure ReadTileList(TileFileName : string);
Procedure WriteTileList(TileFileName : string);
Procedure WriteTileCorners(TileFileName : string);
Procedure WriteTileRanges(TileFileName : string);

//===========================================================================
IMPLEMENTATION

uses SysUtils, u_UTM, u_SceneryHDR;

var
  Tile_File : TextFile;
  FileError : boolean;

{----------------------------------------------------------------------------}
Procedure MessageShow(Info : string);
begin
  if (Memo_Message <> nil) then begin
    Memo_Message.lines.add(Info);
  end;
end;

{----------------------------------------------------------------------------}
Procedure ReadTileRecords;

Var
  InputString  : String[255];
  PartialString  : String[255];
  ErrorCode : Integer;
  CommaPosition : byte;
  i : byte;
  CurrentRow, CurrentColumn : integer;

{----------------------------------------------------------------------------}
Procedure ReadLine;
var
  Ch: Char;

begin
  InputString := '';
  Read(Tile_File, Ch);
  while ((Ch <> char($0A)) AND (NOT Eof(Tile_File))) do begin // look for linefeed
    if (Ch <> char($0D)) then begin // ignore CR
      InputString := InputString + Ch;
    end;
    Read(Tile_File, Ch);
  end;
end;

{----------------------------------------------------------------------------}
begin
  TileCount := 0;
  CurrentRow := 0;
  CurrentColumn := 0;

  while (NOT Eof(Tile_File)) AND (NOT FileError) do begin
    ReadLine;
    // ignore comments lines starting with a * and ignore blank lines
    if (Length(InputString) > 0) And (Copy(InputString,1,1) <> '*') then begin
      CommaPosition := pos(',',InputString);
      if CommaPosition = 0 then begin
        FileError := true;
      end else begin
        SetLength(TileList,TileCount+1);

        TileList[TileCount].TileName := format('%2.2d%2.2d',[CurrentColumn,CurrentRow]);

        PartialString := copy(InputString,1,CommaPosition-1);
        InputString := copy(InputString,CommaPosition+1,80);

        VAL(PartialString,TileList[TileCount].TileUTMRight,ErrorCode);

        CommaPosition := pos(',',InputString);
        if CommaPosition = 0 then begin
          FileError := true
        end else begin
          PartialString := copy(InputString,1,CommaPosition-1);
          InputString := copy(InputString,CommaPosition+1,80);

          VAL(PartialString,TileList[TileCount].TileUTMBottom,ErrorCode);

          CommaPosition := pos(',',InputString);
          if CommaPosition = 0 then begin
            FileError := true;
          end else begin
            PartialString := copy(InputString,1,CommaPosition-1);
            InputString := copy(InputString,CommaPosition+1,80);

            if Degrees(PartialString,Long_,TileList[TileCount].TileLatBottom) then begin
              FileError := true;
            end;

            CommaPosition := pos(',',InputString);
            if CommaPosition <> 0 then begin
              FileError := true
            end else begin
              PartialString := InputString;

              if Degrees(PartialString,Lat_,TileList[TileCount].TileLongRight) then begin
                FileError := true;
              end;
            end;
          end;
        end;

        INC(CurrentColumn);
        if (CurrentColumn > TileColumnCount) then begin
          CurrentColumn := 0;
          INC(CurrentRow);
        end;
        INC(TileCount);
      end;
    end;
  end;
end;

{-----------------------------------------------------------------------------}
Procedure ReadTileList(TileFileName : string);

begin
  FileError := False;  // assume for now
  TileOpen := false;  // do first

  if (NOT FileExists(TileFileName)) then begin
    MessageShow('Tile List File Not Found');
    FileError := true;
  end else begin
    MessageShow(format('Looking for %d tiles, %dx%d, %d tile corners',[TileRowCount*TileColumnCount,TileRowCount,TileColumnCount,(TileRowCount+1)*(TileColumnCount+1)]));
    AssignFile(Tile_File,TileFileName);
    Reset(Tile_File);
    ReadTileRecords;
    Close(Tile_File);

    if (FileError) then begin
      MessageShow('Error in tile file');
    end else begin
      if TileCount > 0 then begin  // must be at least one tile
        if TileCount <> (TileRowCount+1)*(TileColumnCount+1) then begin
          MessageShow(format('Tile count mismatch, %d tiles found',[TileCount]));
        end else begin
          TileOpen := true;
          MessageShow(format('%d tile corners found',[TileCount]));
        end;
      end;
    end;
  end;
  if (FileError) then begin
    Beep;
  end;
end;

{-----------------------------------------------------------------------------}
Procedure WriteTileRange(Column,Row : integer;ColOffset,RowOffset : longint);
begin
  with TileList[Column + Row*(TileColumnCount+1)] do begin
    UTMtoLatLong(UTM_Bottom+(TileUTMBottom+RowOffset),
                 UTM_Right-(TileUTMRight+ColOffset),
//                 UTM_Zone,uNorth);
                 UTM_Zone,uGrid);

    writeln(Tile_File,format('%1.0f,%1.0f,%1.8f,%1.8f',[
      TileUTMRight+ColOffset, TileUTMBottom+RowOffset,
      uLatitude, uLongitude]));
  end;
end;

{-----------------------------------------------------------------------------}
Procedure xWriteTileInfo(Column,Row : integer);
begin
  with TileList[Column + Row*(TileColumnCount+1)] do begin
    writeln(Tile_File,format('%1.0f,%1.0f,%1.8f,%1.8f',[
      TileUTMRight, TileUTMBottom,
      TileLatBottom, TileLongRight]));
  end;
end;

{-----------------------------------------------------------------------------}
Procedure WriteTileInfo(TileData : array of TileRecord; Index : integer);
begin
  with TileData[Index] do begin
    writeln(Tile_File,format('%1.0f,%1.0f,%1.8f,%1.8f',[
      TileUTMRight, TileUTMBottom,
      TileLatBottom, TileLongRight]));
  end;
end;

{-----------------------------------------------------------------------------}
Procedure WriteTileRanges(TileFileName : string);
begin
  MessageShow('Writing tile range');
  AssignFile(Tile_File,TileFileName);
  Rewrite(Tile_File);

  WriteTileInfo(CornerList,0);
  WriteTileInfo(CornerList,1);
  WriteTileInfo(CornerList,2);
  WriteTileInfo(CornerList,3);

  // last 1/4 tile not flyable
  WriteTileInfo(RangeList,0);
  WriteTileInfo(RangeList,1);
  WriteTileInfo(RangeList,2);
  WriteTileInfo(RangeList,3);

  Close(Tile_File);
  MessageShow('Tile range created');
end;

{-----------------------------------------------------------------------------}
Procedure WriteTileCorners(TileFileName : string);
var
  CurrentColumn, CurrentRow : integer;

begin
  MessageShow('Writing top Left and bottom right corners');
  AssignFile(Tile_File,TileFileName);
  Rewrite(Tile_File);

  WriteTileInfo(CornerList,0);
  WriteTileInfo(CornerList,3);

  Close(Tile_File);
  MessageShow('Tile corners created');
end;

{-----------------------------------------------------------------------------}
Procedure WriteTileList(TileFileName : string);
var
  i : integer;

begin
  MessageShow(format('creating %d tiles, %dx%d, %d tile corners',[TileRowCount*TileColumnCount,TileRowCount,TileColumnCount,(TileRowCount+1)*(TileColumnCount+1)]));
  AssignFile(Tile_File,TileFileName);
  Rewrite(Tile_File);

  for i := 0 to (TileRowCount*TileColumnCount-1) do begin
    WriteTileInfo(TileList,i);
  end;

  Close(Tile_File);
  MessageShow(format('%d tile corners created',[TileCount]));
end;

{----------------------------------------------------------------------------}
procedure MakeTileList(Easting, Northing : double);
var
  CurrentColumn, CurrentRow : integer;
//  Easting, Northing : double;

begin
  if (HeaderOpen) then begin
//    Easting := UTM_Right+Resolution/2;    // extra 1/2 of 90 metres all sides
//    Northing := UTM_Bottom-Resolution/2;  // i.e. from tile centre
    TileCount := 0;
//    uNorthSouth := ReadUTMzoneNS(UTM_ZoneNS);
//    uGrid := ReadUTMzoneNS(UTM_ZoneNS);
    ProgressBar_Status.Max := (TileRowCount)*(TileColumnCount);
    for CurrentRow := 0 to (TileRowCount) do begin
      for CurrentColumn := 0 to (TileColumnCount) do begin
        UTMtoLatLong(Northing+CurrentRow*Resolution*tRows,
//          Easting-CurrentColumn*Resolution*tRows,UTM_Zone,uNorthSouth);
          Easting-CurrentColumn*Resolution*tRows,UTM_Zone,UTM_ZoneNS);
        SetLength(TileList,TileCount+1);
        with TileList[TileCount] do begin
          TileName := format('%2.2d%2.2d',[CurrentColumn,CurrentRow]);
          TileUTMRight := CurrentColumn*Resolution*tRows;
          TileUTMBottom := CurrentRow*Resolution*tRows;
          TileLatBottom := uLatitude;
          TileLongRight := uLongitude;
          //ComboBox_Single.Items.append(TileName); //in row:col order
        end;
        INC(TileCount);
        ProgressBar_Status.StepIt;
      end;
    end;
    TileOpen := true;

    //Make a corner list for quick access
    CornerList[0] := TileList[0];  // bottom right
    CornerList[1] := TileList[TileColumnCount];  // bottom left
    CornerList[2] := TileList[(TileColumnCount+1)*TileRowCount];  // top right
    CornerList[3] := TileList[(TileColumnCount+1)*(TileRowCount+1)-1]; // top left

    //Make a flyable area list for quick access
    RangeList[0].TileName := CornerList[0].TileName;
    RangeList[0].TileUTMRight := CornerList[0].TileUTMRight+Resolution*tColumns div 4;
    RangeList[0].TileUTMBottom := CornerList[0].TileUTMBottom+Resolution*tRows div 4;
    UTMtoLatLong(Northing+RangeList[0].TileUTMBottom,
                 Easting-RangeList[0].TileUTMRight,UTM_Zone,UTM_ZoneNS);
    RangeList[0].TileLongRight := uLongitude;
    RangeList[0].TileLatBottom := uLatitude;

    RangeList[1].TileName := CornerList[1].TileName;
    RangeList[1].TileUTMRight := CornerList[1].TileUTMRight-Resolution*tColumns div 4;
    RangeList[1].TileUTMBottom := CornerList[1].TileUTMBottom+Resolution*tRows div 4;
    UTMtoLatLong(Northing+RangeList[1].TileUTMBottom,
                 Easting-RangeList[1].TileUTMRight,UTM_Zone,UTM_ZoneNS);
    RangeList[1].TileLongRight := uLongitude;
    RangeList[1].TileLatBottom := uLatitude;

    RangeList[2].TileName := CornerList[2].TileName;
    RangeList[2].TileUTMRight := CornerList[2].TileUTMRight+Resolution*tColumns div 4;
    RangeList[2].TileUTMBottom := CornerList[2].TileUTMBottom-Resolution*tRows div 4;
    UTMtoLatLong(Northing+RangeList[2].TileUTMBottom,
                 Easting-RangeList[2].TileUTMRight,UTM_Zone,UTM_ZoneNS);
    RangeList[2].TileLongRight := uLongitude;
    RangeList[2].TileLatBottom := uLatitude;

    RangeList[3].TileName := CornerList[3].TileName;
    RangeList[3].TileUTMRight := CornerList[3].TileUTMRight-Resolution*tColumns div 4;
    RangeList[3].TileUTMBottom := CornerList[3].TileUTMBottom-Resolution*tRows div 4;
    UTMtoLatLong(Northing+RangeList[3].TileUTMBottom,
//                 Easting-RangeList[3].TileUTMRight,UTM_Zone,uNorthSouth);
                 Easting-RangeList[3].TileUTMRight,UTM_Zone,UTM_ZoneNS);
    RangeList[3].TileLongRight := uLongitude;
    RangeList[3].TileLatBottom := uLatitude;

    //Make a scenery centre reference
    scCentre.TileName := 'Centre';
    scCentre.TileUTMRight := (CornerList[0].TileUTMRight + CornerList[3].TileUTMRight) /2;
    scCentre.TileUTMBottom := (CornerList[0].TileUTMBottom + CornerList[3].TileUTMBottom) /2;
    UTMtoLatLong(UTM_Bottom+scCentre.TileUTMBottom,
                 UTM_Right-scCentre.TileUTMRight,UTM_Zone,UTM_ZoneNS);
    scCentre.TileLongRight := uLongitude;
    scCentre.TileLatBottom := uLatitude;

    ProgressBar_Status.Position := 0;
  end;
end;

{----------------------------------------------------------------------------}
begin { Initialization }
  TileCount := 0;
  TileOpen := false;
  Memo_Message := nil;
end.

{--- End of File ------------------------------------------------------------}


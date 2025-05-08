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

//---------------------------------------------------------------------------
// for compile options
{$I Define.pas}

type
  TileRecord = record
    TileName      : string;
    TileUTMRight  : double;
    TileUTMBottom : double;
    TileLongRight : real;
    TileLatBottom : real;
  end;
  TileNameType = (xxyy, xxxyyy);

var
  TileNameMode : TileNameType;
  TileRowCount : integer;    // external
  TileColumnCount : integer; // external
  TileCount : Integer;
  TileOpen : boolean;
  TileList : array of TileRecord;
  CornerList : array[0..4-1] of TileRecord;
  RangeList : array[0..4-1] of TileRecord;
  scCentre : TileRecord;

  Memo_Message : TMemo;  // external TMemo for messages
  ProgressBar_Status : TProgressBar; // external progress bar
  CondorFolder : string; // external path for Condor program folder

procedure MakeTileList(Easting, Northing : double);
Procedure ReadTileList(TileFileName : string);
Procedure WriteTileList(TileFileName : string);
Procedure WriteTileCorners(TileFileName : string);
Procedure WriteTileRanges(TileFileName : string);

function MakeTileName(TileColumn, TileRow : integer; Mode : TileNameType) : string;
function GetTileIndex(TileName : String; var TileColumn, TileRow : integer) : boolean;

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

//---------------------------------------------------------------------------
function MakeTileName(TileColumn, TileRow : integer; Mode : TileNameType) : string;
begin
  if (Mode = xxyy) then begin
    result := format('%2.2d%2.2d',[TileColumn,TileRow]);
  end else begin // xxxyyy
    result := format('%3.3d%3.3d',[TileColumn,TileRow]);
  end;
end;

//---------------------------------------------------------------------------
function GetTileIndex(TileName : String; var TileColumn, TileRow : integer) : boolean;
var
  ErrorCode : integer;
begin
  // assume OK for now
  result := true;

  case length(TileName) of
    4: begin
      Val(copy(TileName,1,2),TileColumn,ErrorCode);
      Val(copy(TileName,3,2),TileRow,ErrorCode);
      // if errorcode or not in range -> error
    end;
    6: begin
      Val(copy(TileName,1,3),TileColumn,ErrorCode);
      Val(copy(TileName,4,3),TileRow,ErrorCode);
      // if errorcode or not in range -> error
    end;
    else begin
      result := false;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure ReadTileRecords;

Var
  InputString  : String;
  PartialString  : String;
  ErrorCode : Integer;
  CommaPosition : byte;
  CurrentRow, CurrentColumn : integer;

{----------------------------------------------------------------------------}
begin
  TileCount := 0;
  CurrentRow := 0;
  CurrentColumn := 0;

  while (NOT Eof(Tile_File)) AND (NOT FileError) do begin
    Readline(@Tile_File,InputString);
    // ignore comments lines starting with a * and ignore blank lines
    if (Length(InputString) > 0) And (Copy(InputString,1,1) <> '*') then begin
      CommaPosition := pos(',',InputString);
      if CommaPosition = 0 then begin
        FileError := true;
      end else begin
        SetLength(TileList,TileCount+1);
        TileList[TileCount].TileName := MakeTileName(CurrentColumn, CurrentRow, TileNameMode);
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

// NOTE: lat/long calculated based on 90m resolution, not calibration!
// NOTE: whole tiles only, no partial tiles i.e. patches
{----------------------------------------------------------------------------}
procedure MakeTileList(Easting, Northing : double);
var
  CurrentColumn, CurrentRow : integer;

begin
  if (HeaderOpen) then begin
//    Easting := UTM_Right+Resolution/2;    // extra 1/2 of 90 metres all sides
//    Northing := UTM_Bottom-Resolution/2;  // i.e. from tile centre
    TileCount := 0;
    ProgressBar_Status.Max := (TileRowCount)*(TileColumnCount);
    for CurrentRow := 0 to (TileRowCount) do begin
      for CurrentColumn := 0 to (TileColumnCount) do begin
        UTMtoLatLong(Northing+CurrentRow*Resolution*tRows,
          Easting-CurrentColumn*Resolution*tRows,UTM_Zone,UTM_ZoneNS);
        SetLength(TileList,TileCount+1);
        with TileList[TileCount] do begin
          TileName := MakeTileName(CurrentColumn, CurrentRow, TileNameMode);
          TileUTMRight := CurrentColumn*Resolution*tRows;
          TileUTMBottom := CurrentRow*Resolution*tRows;
          TileLatBottom := uLatitude;
          TileLongRight := uLongitude;
        end;
        INC(TileCount);
        ProgressBar_Status.StepIt;
      end;
    end;
    TileOpen := true;

    // account for partial tiles
    //Make a corner list for quick access
    // bottom right
    CornerList[0] := TileList[0];
    // bottom left
    UTMtoLatLong(Northing+0*Resolution, Easting-ColumnCount*Resolution,UTM_Zone,UTM_ZoneNS);
    CornerList[1].TileUTMRight :=  ColumnCount*Resolution;
    CornerList[1].TileUTMBottom := 0*Resolution;
    CornerList[1].TileLatBottom := uLatitude;
    CornerList[1].TileLongRight := uLongitude;
    // top right
    UTMtoLatLong(Northing+RowCount*Resolution, Easting-0*Resolution,UTM_Zone,UTM_ZoneNS);
    CornerList[2].TileUTMRight :=  0*Resolution;
    CornerList[2].TileUTMBottom := RowCount*Resolution;
    CornerList[2].TileLatBottom := uLatitude;
    CornerList[2].TileLongRight := uLongitude;
    // top left
    UTMtoLatLong(Northing+RowCount*Resolution, Easting-ColumnCount*Resolution,UTM_Zone,UTM_ZoneNS);
    CornerList[3].TileUTMRight :=  ColumnCount*Resolution;
    CornerList[3].TileUTMBottom := RowCount*Resolution;
    CornerList[3].TileLatBottom := uLatitude;
    CornerList[3].TileLongRight := uLongitude;

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


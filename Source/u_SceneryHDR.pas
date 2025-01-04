{
 * u_SceneryHDR.pas
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
UNIT u_SceneryHDR;

{----------------------------------------------------------------------------
The HDR file is generated from the elevation data.
- extract the UTM zone
- extract the UTM extents
- extract the number of rows and columns
- determine if V1 or V2
----------------------------------------------------------------------------}

{============================================================================}
INTERFACE

uses
  StdCtrls;

//---------------------------------------------------------------------------
// for compile options
{$I Define.pas}

const
  pColumns = 64;    //horizontal resolution of one patch (1/4 tile)
  pRows =    64;    //horizontal resolution of one patch (1/4 tile)
  tColumns = 256;   //horizontal resolution of one tile
  tRows =    256;   //vertical resolution of one tile
  Resolution = 90;  //90 metre

var
  UTM_Zone     : integer;
  UTM_ZoneNS   : char;
  UTM_Right    : double;
  UTM_Bottom   : double;
  UTM_Left     : double;
  UTM_Top      : double;
  RowCount     : integer;  // at 90 m
  ColumnCount  : integer;  // at 90 m

  // condor calibration - placed here for now, but comes from terrain header !
  cResolution : single;           // 90m
  cDeltaX : single;               // actual horizontal resolution in m (calibrated)
  cDeltaY : single;               // actual vertical resolution in m (calibrated)

  NumColumns   : integer;  // at 90m or 30m
  NumRows      : integer;  // at 90m or 30m
  DEM_Res      : integer;
  HeaderOpen   : boolean;
  Memo_Message : TMemo;  // external TMemo for messages

Procedure ReadSceneryHeader(HeaderFileName : string);
Procedure WriteSceneryHeader(HeaderFileName : string);

{============================================================================}
IMPLEMENTATION

uses
  SysUtils,
  u_Util;

Type
  FieldFoundType = (fUTMzone,fUTMright,fUTMbottom,fUTMleft,fUTMtop,fRows,fColumns);

var
  FieldFoundFlag : set of FieldFoundType;
  Header_File : TextFile;
  FileError : boolean;

{----------------------------------------------------------------------------}
Procedure MessageShow(Info : string);
begin
  if (Memo_Message <> nil) then begin
    Memo_Message.lines.add(Info);
  end;
end;

{----------------------------------------------------------------------------}
Procedure ReadHeaderFileLines;
var
//  InputString  : String[255];
  InputString  : String;
//  PartialString  : String[255];
  PartialString  : String;
  ErrorCode   : Integer;
  EqualPosition : integer;
  i : byte;

{----------------------------------------------------------------------------}
{Procedure ReadLine;
var
  Ch: Char;

begin
  InputString := '';
  Read(Header_File, Ch);
  while ((Ch <> char($0A)) AND (NOT Eof(Header_File))) do begin // look for linefeed
    if (Ch <> char($0D)) then begin // ignore CR
      InputString := InputString + Ch;
    end;
    Read(Header_File, Ch);
  end;
end;
}
{----------------------------------------------------------------------------}
Function ExtractZoneNumber(InString:string) : string;
begin
  //find first digit
  i := 0;
  while ((length(InString) > i) AND NOT (InString[i+1] in ['0'..'9'])) do begin
    INC(i);
  end;
  Instring := copy(InString,i+1,length(InString));
  //keep digits until not digits
  i := 0;
  while ((length(InString) > i) AND (InString[i+1] in ['0'..'9'])) do begin
    INC(i);
  end;
  ExtractZoneNumber := copy(InString,1,i);
  Instring := copy(InString,i+1,length(InString));
end;

{----------------------------------------------------------------------------}
Procedure ExtractUTMzone(InString:string);
begin
  // default north
  UTM_ZoneNS := 'N';
  //find first digit
  i := 0;
  while ((length(InString) > i) AND NOT (InString[i+1] in ['0'..'9'])) do begin
    INC(i);
  end;
  Instring := copy(InString,i+1,length(InString));
  //keep digits until not digits
  i := 0;
  while ((length(InString) > i) AND (InString[i+1] in ['0'..'9'])) do begin
    INC(i);
  end;
  UTM_Zone := strToInt(copy(InString,1,i));
  Instring := copy(InString,i+1,length(InString));
  i := 0;
  while ((length(InString) >= i) AND NOT (InString[i+1] in ['n','N','s','S'])) do begin
    INC(i);
  end;
  if (UpperCase(InString[i+1]) = 'N') then begin
    UTM_ZoneNS := 'N';
  end else begin
    UTM_ZoneNS := 'S';
  end;
  //skip rest
end;

//map info = {UTM, 1, 1, 288580, 5812682, 90, 90, 31, North,WGS-84}
{----------------------------------------------------------------------------}
Procedure ExtractUTMdetails(InString:string);
var
  CommaPosition : integer;
  pString : string;
begin
  CommaPosition := pos(',',InString);
  if (CommaPosition <> 0) then begin
    pString := trim(copy(InString,1,CommaPosition-1));
    InString := copy(InString,CommaPosition+1,length(InString));
    // confirm UTM
    if (pos('UTM',UpperCase(pString)) <> 0) then begin
      CommaPosition := pos(',',InString);
      if (CommaPosition <> 0) then begin
        // skip this item
        InString := copy(InString,CommaPosition+1,length(InString));
        CommaPosition := pos(',',InString);
        if (CommaPosition <> 0) then begin
          // skip this item
          InString := copy(InString,CommaPosition+1,length(InString));
          CommaPosition := pos(',',InString);
          if (CommaPosition <> 0) then begin
            pString := trim(copy(InString,1,CommaPosition-1));
            InString := copy(InString,CommaPosition+1,length(InString));
            UTM_Left := strToInt(pString);
            CommaPosition := pos(',',InString);
            if (CommaPosition <> 0) then begin
              pString := trim(copy(InString,1,CommaPosition-1));
              InString := copy(InString,CommaPosition+1,length(InString));
              UTM_Top := strToInt(pString);
              CommaPosition := pos(',',InString);
              if (CommaPosition <> 0) then begin
                pString := trim(copy(InString,1,CommaPosition-1));
                InString := copy(InString,CommaPosition+1,length(InString));
                DEM_Res := strToInt(pString);
                CommaPosition := pos(',',InString);
                if (CommaPosition <> 0) then begin
                  pString := trim(copy(InString,1,CommaPosition-1));
                  InString := copy(InString,CommaPosition+1,length(InString));
                  DEM_Res := strToInt(pString);
                  CommaPosition := pos(',',InString);
                  if (CommaPosition <> 0) then begin
                    pString := trim(copy(InString,1,CommaPosition-1));
                    InString := copy(InString,CommaPosition+1,length(InString));
                    UTM_Zone := strToint(pString);
                    CommaPosition := pos(',',InString);
                    if (CommaPosition <> 0) then begin
                      pString := uppercase(trim(copy(InString,1,CommaPosition-1)));
                      InString := copy(InString,CommaPosition+1,length(InString));
                      UTM_ZoneNS := ANSIChar(pString[1]);
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
procedure CheckForResolution; // 90m -> V1, 30m -> V2
begin
  DEM_Res := round ( (UTM_Right - UTM_Left) / (NumColumns));
  if (DEM_Res = 90) then begin
    MessageShow(format('Resolution: %d -> V1',[DEM_Res]));
  end else begin
    if (DEM_Res = 30) then begin
      MessageShow(format('Resolution: %d -> V2',[DEM_Res]));
    end else begin
      MessageShow(format('Invalid resolution: %d',[DEM_Res]));
    end;
  end;
end;

{----------------------------------------------------------------------------}
begin
  FieldFoundFlag := [];
  while (NOT Eof(Header_File)) AND (NOT FileError) AND
        (FieldFoundFlag <> [fUTMzone,fUTMright,fUTMbottom,fUTMleft,fUTMtop,fRows,fColumns]) do begin
//    ReadLine;
    ReadLine(@Header_File,InputString);
    EqualPosition := pos('=',InputString);
    if (EqualPosition <> 0) then begin
      PartialString := trim(copy(InputString,1,EqualPosition-1));
      InputString := copy(InputString,EqualPosition+1,length(InputString));
      if PartialString = 'map_projection' then begin
//        UTM_Zone := ExtractZoneNumber(InputString);
        ExtractUTMzone(InputString);
        MessageShow(format('UTM zone: %s %s',[UTM_Zone, UTM_ZoneNS]));
        FieldFoundFlag := FieldFoundFlag + [fUTMzone];
      end else begin
        if PartialString = 'right_map_x' then begin
          VAL(InputString,UTM_Right,ErrorCode);
          MessageShow(format('UTM Right: %1.0f',[UTM_Right]));
          FieldFoundFlag := FieldFoundFlag + [fUTMright];
        end else begin
          if PartialString = 'lower_map_y' then begin
            VAL(InputString,UTM_Bottom,ErrorCode);
            MessageShow(format('UTM Bottom: %1.0f',[UTM_Bottom]));
            FieldFoundFlag := FieldFoundFlag + [fUTMbottom];
          end else begin
            if PartialString = 'left_map_x' then begin
              VAL(InputString,UTM_Left,ErrorCode);
              MessageShow(format('UTM Left: %1.0f',[UTM_Left]));
              FieldFoundFlag := FieldFoundFlag + [fUTMleft];
            end else begin
              if PartialString = 'upper_map_y' then begin
                VAL(InputString,UTM_Top,ErrorCode);
                MessageShow(format('UTM Top: %1.0f',[UTM_Top]));
                FieldFoundFlag := FieldFoundFlag + [fUTMtop];
              end else begin
                if PartialString = 'number_of_rows' then begin
                  VAL(InputString,NumRows,ErrorCode);
                  MessageShow(format('Rows: %d',[NumRows]));
                  FieldFoundFlag := FieldFoundFlag + [fRows];
                end else begin
                  if PartialString = 'number_of_columns' then begin
                    VAL(InputString,NumColumns,ErrorCode);
                    MessageShow(format('Columns: %d',[NumColumns]));
                    FieldFoundFlag := FieldFoundFlag + [fColumns];
                  end else begin
                    // could be ENVI file instead
                    if PartialString = 'samples' then begin
                      VAL(InputString,NumColumns,ErrorCode);
                      MessageShow(format('Columns: %d',[NumColumns]));
                      FieldFoundFlag := FieldFoundFlag + [fColumns];
                    end else begin
                      if PartialString = 'lines' then begin
                        VAL(InputString,NumRows,ErrorCode);
                        MessageShow(format('Rows: %d',[NumColumns]));
                        FieldFoundFlag := FieldFoundFlag + [fRows];
                      end else begin
                        if PartialString = 'map info' then begin
                          ExtractUTMdetails(InputString);
                          FieldFoundFlag := FieldFoundFlag + [fUTMzone,fUTMleft,fUTMtop];
                          if ([fColumns,fRows] <= FieldFoundFlag) then begin
                            UTM_Right := UTM_Left + NumColumns * DEM_Res;
                            UTM_Bottom := UTM_Top - NumRows * DEM_Res;
                            FieldFoundFlag := FieldFoundFlag + [fUTMright,fUTMbottom];
                          end;
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  CheckForResolution; // 90m -> V1, 30m -> V2
  FileError := (FieldFoundFlag <> [fUTMzone,fUTMright,fUTMbottom,fUTMleft,fUTMtop,fRows,fColumns]);
end;

{-----------------------------------------------------------------------------}
Procedure ReadSceneryHeader(HeaderFileName : string);

begin
  FileError := False;  // assume for now
  HeaderOpen := false;  // do first

  if (NOT FileExists(HeaderFileName)) then begin
//    MessageShow('Header File Not Found');
//    FileError := true;
  end else begin
    MessageShow('Reading scenery header file...');
    AssignFile(Header_File,HeaderFileName);
    Reset(Header_File);
    ReadHeaderFileLines;
    Close(Header_File);

    if (FileError) then begin
      MessageShow('Error in header file');
    end else begin
      // counts at 90 m !
      RowCount := round( (UTM_Right - UTM_Left) / resolution);
      ColumnCount := round( (UTM_Top - UTM_Bottom) / resolution);
      if (frac(RowCount/tRows) <> 0.0) then begin  // must be divisible by 256
        MessageShow(format('Error - Row count %d must be divisible by 256',[RowCount]));
        FileError := true;
      end else begin
        if (frac(ColumnCount/tColumns) <> 0.0) then begin  // must be divisible by 256
          MessageShow(format('Error - Column count %d must be divisible by 256',[ColumnCount]));
          FileError := true;
        end else begin
          HeaderOpen := true;
        end;
      end;
    end;
  end;
  if (FileError) then begin
    Beep;
  end;
end;

//---------------------------------------------------------------------------
Procedure WriteSceneryHeader(HeaderFileName : string);
//procedure TForm_DEM.Create_DEM_HeaderFile;
//var
//  Hdr_File : TextFile;
begin
    AssignFile(Header_File, HeaderFileName);
    Rewrite(Header_File);
//file_title             = cropped
//data_format            = int16
    writeln(Header_File,format('map_projection         = UTM Zone %d%s',[UTM_Zone,UTM_ZoneNS]));
//ellipsoid              = WGS84
    writeln(Header_File,format('left_map_x             = %1.0f',[UTM_Left]) );
    writeln(Header_File,format('lower_map_y            = %1.0f',[UTM_Bottom]) );
    writeln(Header_File,format('right_map_x            = %1.0f',[UTM_Right]) );
    writeln(Header_File,format('upper_map_y            = %1.0f',[UTM_Top]) );
    writeln(Header_File,format('number_of_rows         = %d',[NumRows]) );
    writeln(Header_File,format('number_of_columns      = %d',[NumColumns]) );
//elev_m_unit            = meters
//elev_m_minimum         = 23
//elev_m_maximum         = 1609
//elev_m_missing_flag    = -32767
    writeln(Header_File,format('resolution             = %d',[DEM_Res]) );

    // close the file
    CloseFile(Header_file);
end;

{----------------------------------------------------------------------------}
begin { Initialization }
  HeaderOpen := false;
  Memo_Message := nil;
end.

{--- End of File ------------------------------------------------------------}


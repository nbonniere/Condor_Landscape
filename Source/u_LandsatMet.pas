{
 * u_LandsatMet.pas
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
UNIT u_LandsatMet;

{----------------------------------------------------------------------------
The Landsat .met file is provided with landsat 7 bands.
- extract the corner coordinates
----------------------------------------------------------------------------}

{============================================================================}
INTERFACE

uses StdCtrls;

type
  LandsatMetFileInfo = record
    Folder : string;
    MetName : string;
    SCENE_UL_CORNER_LAT : double;
    SCENE_UL_CORNER_LON : double;
    SCENE_UR_CORNER_LAT : double;
    SCENE_UR_CORNER_LON : double;
    SCENE_LL_CORNER_LAT : double;
    SCENE_LL_CORNER_LON : double;
    SCENE_LR_CORNER_LAT : double;
    SCENE_LR_CORNER_LON : double;
  end;

var
  LandsatMetFiles : array of LandsatMetFileInfo;
  FileCount : integer;

  Memo_Message : TMemo;  // external TMemo for messages
  MetOpen : boolean;

Procedure ReadLandsatTileCoords(MetFileName : string);

{============================================================================}
IMPLEMENTATION

uses SysUtils;

Type
  FieldFoundType = (UL_Lat,UL_Lon,UR_Lat,UR_Lon,LL_Lat,LL_Lon,LR_Lat,LR_Lon);

var
  FieldFoundFlag : set of FieldFoundType;
  MET_File : TextFile;
  FileError : boolean;

{----------------------------------------------------------------------------}
Procedure MessageShow(Info : string);
begin
  if (Memo_Message <> nil) then begin
    Memo_Message.lines.add(Info);
  end;
end;

{----------------------------------------------------------------------------}
Procedure ReadMetFileLines;
var
  InputString  : String[255];
  PartialString  : String[255];
  ErrorCode   : Integer;
  EqualPosition : byte;
  i : byte;

{----------------------------------------------------------------------------}
Procedure ReadLine;
var
  Ch: Char;

begin
  InputString := '';
  Read(Met_File, Ch);
  while ((Ch <> char($0A)) AND (NOT Eof(Met_File))) do begin // look for linefeed
    if (Ch <> char($0D)) then begin // ignore CR
      InputString := InputString + Ch;
    end;
    Read(Met_File, Ch);
  end;
end;

{----------------------------------------------------------------------------}
begin
  with LandsatMetFiles[FileCount] do begin
    FieldFoundFlag := [];
    while (NOT Eof(MET_File)) AND (NOT FileError) AND
          (FieldFoundFlag <> [UL_Lat,UL_Lon,UR_Lat,UR_Lon,LL_Lat,LL_Lon,LR_Lat,LR_Lon]) do begin
      ReadLine;
      EqualPosition := pos('=',InputString);
      if (EqualPosition <> 0) then begin
        PartialString := trim(copy(InputString,1,EqualPosition-1));
        InputString := trim(copy(InputString,EqualPosition+1,length(InputString)));
        if PartialString = 'SCENE_UL_CORNER_LAT' then begin
          VAL(InputString,SCENE_UL_CORNER_LAT,ErrorCode);
          MessageShow(format('SCENE_UL_CORNER_LAT: %1.3f',[SCENE_UL_CORNER_LAT]));
          FieldFoundFlag := FieldFoundFlag + [UL_Lat];
        end else begin
          if PartialString = 'SCENE_UL_CORNER_LON' then begin
            VAL(InputString,SCENE_UL_CORNER_LON,ErrorCode);
            MessageShow(format('SCENE_UL_CORNER_LON: %1.3f',[SCENE_UL_CORNER_LON]));
            FieldFoundFlag := FieldFoundFlag + [UL_Lon];
          end else begin
            if PartialString = 'SCENE_UR_CORNER_LAT' then begin
              VAL(InputString,SCENE_UR_CORNER_LAT,ErrorCode);
              MessageShow(format('SCENE_UR_CORNER_LAT: %1.3f',[SCENE_UR_CORNER_LAT]));
              FieldFoundFlag := FieldFoundFlag + [UR_Lat];
            end else begin
              if PartialString = 'SCENE_UR_CORNER_LON' then begin
                VAL(InputString,SCENE_UR_CORNER_LON,ErrorCode);
                MessageShow(format('SCENE_UR_CORNER_LON: %1.3f',[SCENE_UR_CORNER_LON]));
                FieldFoundFlag := FieldFoundFlag + [UR_Lon];
              end else begin
                if PartialString = 'SCENE_LL_CORNER_LAT' then begin
                  VAL(InputString,SCENE_LL_CORNER_LAT,ErrorCode);
                  MessageShow(format('SCENE_LL_CORNER_LAT: %1.3f',[SCENE_LL_CORNER_LAT]));
                  FieldFoundFlag := FieldFoundFlag + [LL_Lat];
                end else begin
                  if PartialString = 'SCENE_LL_CORNER_LON' then begin
                    VAL(InputString,SCENE_LL_CORNER_LON,ErrorCode);
                    MessageShow(format('SCENE_LL_CORNER_LON: %1.3f',[SCENE_LL_CORNER_LON]));
                    FieldFoundFlag := FieldFoundFlag + [LL_Lon];
                  end else begin
                    if PartialString = 'SCENE_LR_CORNER_LAT' then begin
                      VAL(InputString,SCENE_LR_CORNER_LAT,ErrorCode);
                      MessageShow(format('SCENE_LR_CORNER_LAT: %1.3f',[SCENE_LR_CORNER_LAT]));
                      FieldFoundFlag := FieldFoundFlag + [LR_Lat];
                    end else begin
                      if PartialString = 'SCENE_LR_CORNER_LON' then begin
                        VAL(InputString,SCENE_LR_CORNER_LON,ErrorCode);
                        MessageShow(format('SCENE_LR_CORNER_LON: %1.3f',[SCENE_LR_CORNER_LON]));
                        FieldFoundFlag := FieldFoundFlag + [LR_Lon];
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
    FileError := (FieldFoundFlag <> [UL_Lat,UL_Lon,UR_Lat,UR_Lon,LL_Lat,LL_Lon,LR_Lat,LR_Lon]);
  end;
end;

{-----------------------------------------------------------------------------}
Procedure ReadLandsatTileCoords(MetFileName : string);

begin
  FileError := False;  // assume for now
  MetOpen := false;  // do first

  if (NOT FileExists(MetFileName)) then begin
    MessageShow('File Not Found');
    FileError := true;
  end else begin
    MessageShow('Reading Landsat Met file...');
    AssignFile(Met_File,MetFileName);
    Reset(Met_File);
    ReadMetFileLines;
    Close(Met_File);

    if (FileError) then begin
      MessageShow('Error in Met file');
    end else begin
      MetOpen := true;
    end;
  end;
  if (FileError) then begin
    Beep;
  end;
end;

{----------------------------------------------------------------------------}
begin { Initialization }
  Metopen := false;
  Memo_Message := nil;
  FileCount := 0;
end.

{--- End of File ------------------------------------------------------------}


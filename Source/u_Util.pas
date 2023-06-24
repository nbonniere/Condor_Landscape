{
 * u_Util.pas
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

{============================================================================}
{                                                                            }
{ Module:     UTIL.PAS                                                       }
{ Created by: D. Bonniere                                                    }
{ Date:       18 Apr 99                                                      }
{ Abstract:   Provides Utility functions.                                    }
{                                                                            }
{============================================================================}

//---------------------------------------------------------------------------
UNIT u_Util;

{$S-,R-,I-,V-,F+,D+}

{============================================================================}
INTERFACE

uses stdctrls;

type
  LatLongType = (Lat_,Long_);

  File_Link = Text;
  F_Link = ^File_Link;

//  uFile_Link = File;
//  uF_Link = ^uFile_Link;

  File_Generic_Text = File of Char;
  File_GT_Link = File_Generic_Text;
  GT_Link = ^File_GT_Link;

var
  FileError     : boolean;

function ShortenFolderString(S: String; Size : integer): String;
function ExtractRoot(S: String): String;
Function Str_Seconds(secs:real):string;
Function Degrees(InString:string;
                 LatLong:LatLongType;
                 var DegreeValue:real):Boolean;
function CoordToString(Degrees : real;
                       LatLong:LatLongType;
                       Units : byte) : string;
procedure PercentRangeCheck(rEdit : TEdit);
function Set255(Clr : integer) : integer;
function ClampByte( value : integer ): byte;
function f_Minimum(A, B : double) : double;
function f_Maximum(A, B : double) : double;

Procedure ParseFloat(var Tempstr : string; var Result : double);
Procedure ParseInteger(var Tempstr : string; var Result : longint);
Procedure ParseText(var TempStr, Result : string);
Procedure ReadLine(X_File:F_Link;var InputString:string);

//Procedure xReadLine(u_File:uF_Link;var InputString:string);
//Procedure xReadLineReset;
Procedure GT_ReadLine(GT_File:GT_Link; var InputString:string);

function xxStrToFloat_DotDecimal(sString:String):real;
procedure Force_DecimalSeparator;

{============================================================================}
IMPLEMENTATION

uses SysUtils, Windows;

//---------------------------------------------------------------------------
function ShortenFolderString(S: String; Size : integer): String;
begin
  if (length(s) > Size) then begin
    if (copy(S,length(S),1) = '\') then begin
      S := copy(S,1,length(S)-1);
    end;
    ShortenFolderString := ExtractFileDrive(S) + '\...\'+ExtractFileName(S);
  end else begin
    ShortenFolderString := S;
  end;
end;

//---------------------------------------------------------------------------
function ExtractRoot(S: String): String;
var
  ExtPos : integer;

begin
  ExtPos := pos('.',S);
  if (ExtPos <> 0) then begin
    ExtractRoot := copy(S,1,ExtPos-1);
  end else begin
    ExtractRoot := S;
  end;
end;

{--------------------------------------------------------------------------}
Function Str_Seconds(secs:real):string;

var
  Hours   : real;
  Minutes : real;
  Seconds : real;
  TextString1 : string[2];
  TextString2 : string;

begin
  Seconds := trunc(secs);
  Minutes := trunc(Seconds / 60);
  Hours := trunc(Minutes / 60);
  Minutes := Minutes - Hours * 60;
  Seconds := Seconds - Minutes * 60 - Hours * 3600;

  if Hours < 10 then
  begin
    str (Hours:1:0,TextString1 );
    TextString2 := '0'+TextString1;
  end
  else
  begin
    str (Hours:2:0,TextString2 );
  end;

  if Minutes < 10 then
  begin
    str (Minutes:1:0,TextString1 );
    TextString2 := TextString2+':0'+Textstring1;
  end
  else
  begin
    str (Minutes:2:0,TextString1 );
    TextString2 := TextString2+':'+Textstring1;
  end;

  if Seconds < 10 then
  begin
    str (Seconds:1:0,TextString1 );
    TextString2 := TextString2+':0'+Textstring1;
  end
  else
  begin
    str (Seconds:2:0,TextString1 );
    TextString2 := TextString2+':'+Textstring1;
  end;

  Str_Seconds := TextString2;
end;

{----------------------------------------------------------------------------}
Function Degrees(InString:string;
                 LatLong:LatLongType;
                 var DegreeValue:real):Boolean;

const
  SPACE = char($20);
  TAB   = char($09);
  COLON = char($3A);

var
  Error     : Boolean;
  i         : integer;
  value     : real;
  errorcode : integer;
  temp      : real;
  DecimalFlag : boolean;
  ParsedString : string[255];
  sign : boolean;

begin
  Error := False;
  i := 0;
  while ((length(InString) > i) AND (InString[i+1] in [SPACE,TAB])) do
    INC(i);
  InString := copy(InString,i+1,length(InString));

  Sign := False;
  if (length(InString) > 0) then
  begin
    if ((InString[1] in ['N','n','S','s']) AND (LatLong = Lat_)) OR
       ((InString[1] in ['E','e','W','w']) AND (LatLong = Long_)) OR
        (InString[1] in ['+','-']) then
    begin
      if InString[1] in ['S','s','W','w','-'] then
      begin
        Sign := True;
      end;
      // left over string
      InString := copy(InString,2,length(InString));
    end;
  end;

  i := 0;
  while ((length(InString) > i) AND (InString[i+1] in [SPACE,TAB])) do
    INC(i);
  InString := copy(InString,i+1,length(InString));

  i := 0;
  DecimalFlag := false;
  while ((length(InString) > i) AND (InString[i+1] in ['.','0'..'9'])) do
  begin
    INC(i);
    if (InString[i+1] = '.') then
      DecimalFlag := true;
  end;

  ParsedString := copy(InString,1,i);
  InString := copy(InString,i+1,length(InString));

  VAL(ParsedString,value,ErrorCode);

  if ErrorCode <> 0 then
  begin
    Error := True
  end
  else
  begin
//    if value < 0 then
//    begin
//      Temp := ABS(value);
//      Sign := true;
//    end
//    else
//    begin
//      Temp := value;
//      Sign := false;
//    end;
    Temp := value;

    if (NOT DecimalFlag) then
    begin
      i := 0;
      while ((length(InString) > i) AND (InString[i+1] in [SPACE,TAB,COLON])) do
        INC(i);
      InString := copy(InString,i+1,length(InString));

      i := 0;
      DecimalFlag := false;
      while ((length(InString) > i) AND (InString[i+1] in ['.','0'..'9'])) do
      begin
        INC(i);
        if (InString[i+1] = '.') then
          DecimalFlag := true;
      end;

      ParsedString := copy(InString,1,i);
      InString := copy(InString,i+1,length(InString));

      VAL(ParsedString,value,ErrorCode);

      if ErrorCode <> 0 then
      begin
        Error := True
      end
      else
      begin
        if Value >= 60.0 then
        begin
          Error := true;
        end;

        Temp := Temp + value/60.0;
        if NOT DecimalFlag then
        begin
          i := 0;
          while ((length(InString) > i) AND (InString[i+1] in [SPACE,TAB,COLON])) do
            INC(i);
          InString := copy(InString,i+1,length(InString));

          i := 0;
          while ((length(InString) > i) AND (InString[i+1] in ['.','0'..'9'])) do
          begin
            INC(i);
          end;

          ParsedString := copy(InString,1,i);
          InString := copy(InString,i+1,length(InString));

          VAL(ParsedString,value,ErrorCode);

          if ErrorCode <> 0 then
          begin
            Error := True
          end
          else
          begin
            if Value >= 60.0 then
            begin
              Error := true;
            end;

            Temp := Temp + value/3600.0;
          end;
        end;
      end;
    end;

    if ((Temp >= 90.0) AND (LatLong = Lat_)) OR ((Temp >= 180.0) AND (LatLong = Long_)) then
    begin
      Error := true;
    end;

    i := 0;
    while ((length(InString) > i) AND (InString[i+1] in [SPACE,TAB])) do
      INC(i);
    InString := copy(InString,i+1,length(InString));

    if (NOT sign) AND (length(InString) > 0) then
    begin
      if ((InString[1] in ['N','n','S','s']) AND (LatLong = Lat_)) OR
         ((InString[1] in ['E','e','W','w']) AND (LatLong = Long_)) then
      begin
        if InString[1] in ['S','s','W','w'] then
        begin
          Temp := - Temp;
        end;
        // left over string
        InString := copy(InString,2,length(InString));
      end
      else
      begin
        Error := true;
      end;
    end;

    // check trailing string
    i := 0;
    while ((length(InString) > i) AND (InString[i+1] in [SPACE,TAB])) do
      INC(i);
    InString := copy(InString,i+1,length(InString));
    if Instring <> '' then
    begin
      Error := true;
    end;

  end;
  if sign then
    Temp := - Temp;
  DegreeValue := Temp;
  Degrees := Error;
end;

{---------------------------------------------------------------------------}
function CoordToString(Degrees : real;
                       LatLong:LatLongType;
                       Units : byte) : string;
var
  deg : real;
  min : real;
  s : string[3];
  sString : string;
  DegNeg : boolean;

begin
  if (Degrees < 0.0) then begin
    Degrees := -Degrees;
    DegNeg := True;
  end
  else begin
    DegNeg := False;
  end;

  case Units of
    0 : begin {degrees decimal}
      str(Degrees:9:5,sString);
    end;
    1 : begin {degrees:minutes decimal}
      deg := INT(Degrees);
      Degrees := FRAC(Degrees) * 60;
      str(Deg:3:0,s);
      str(Degrees:6:3,sString);
      sString := s + ':' + sString
    end;
    2 : begin {degrees:minutes:seconds decimal}
      deg := INT(Degrees);
      Degrees := FRAC(Degrees) * 60;
      min := INT(Degrees);
      Degrees := FRAC(Degrees) * 60;
      str(min:2:0,s);
      str(Degrees:4:1,sString);
      sString := s + ':' + sString;
      str(deg:3:0,s);
      sString := s + ':' + sString;
    end;
  end;
  if (LatLong = Lat_) then begin
    if DegNeg then begin
      sString := 'S' + sString;
    end
    else begin
      sString := 'N' + sString;
    end;
  end
  else begin
    if DegNeg then begin
      sString := 'W' + sString;
    end
    else begin
      sString := 'E' + sString;
    end;
  end;
  CoordToString := sString;
end;

//---------------------------------------------------------------------------
procedure PercentRangeCheck(rEdit : TEdit);
var
  Value : real;
begin
  Value := StrToFloat(rEdit.text);
  if (Value > 100) then begin rEdit.text := '100'; end;
  if (Value < 0) then begin rEdit.text := '0'; end;
end;

{This just forces a value to be 0 - 255 for rgb purposes.  I used asm in an
 attempt at speed, but I don't think it helps much.}
//----------------------------------------------------------------------------
function Set255(Clr : integer) : integer;
asm
  MOV  EAX,Clr  // store value in EAX register (32-bit register)
  CMP  EAX,254  // compare it to 254
  JG   @SETHI   // if greater than 254 then go set to 255 (max value)
  CMP  EAX,1    // if less than 255, compare to 1
  JL   @SETLO   // if less than 1 go set to 0 (min value)
  RET           // otherwise it doesn't change, just exit
@SETHI:         // Set value to 255
  MOV  EAX,255  // Move 255 into the EAX register
  RET           // Exit (result value is the EAX register value)
@SETLO:         // Set value to 0
  MOV  EAX,0    // Move 0 into EAX register
end;            // Result is in EAX

//----------------------------------------------------------------------------
function ClampByte( value : integer ): byte;
begin
  if value < 0 then begin
    result := 0;
  end else begin
    if value > 255 then begin
      result := 255;
    end else begin
      result := value;
    end;
  end;
end;

{----------------------------------------------------------------------------}
function f_Minimum(A, B : double) : double;
begin
  if (A < B) then begin
    result := A;
  end else begin
    result := B;
  end;
end;

{----------------------------------------------------------------------------}
function f_Maximum(A, B : double) : double;
begin
  if (A > B) then begin
    result := A;
  end else begin
    result := B;
  end;
end;

// ??? need to improve to skip over multple tabs
// - - - - - - - - - - - - - - - - - -
Procedure ParseText(var TempStr, Result : string);
var
  Index: integer;
begin
  // skip over space & ctrl chars
  TempStr := trim(TempStr);
  TempStr := StringReplace(TempStr,chr(9),' ',[rfReplaceAll]); // replace TAB by SPACE
  // keep all until space or tab or end
  Index := pos(' ',TempStr); // space
  if (Index = 0) then begin
    Result := TempStr;
    TempStr := '';
  end else begin
    Result := trim(copy(TempStr,1,Index-1));          // trim to remove extra tabs
    TempStr := copy(TempStr,Index+1,length(TempStr));
  end;
end;

// - - - - - - - - - - - - - - - - - -
Procedure ParseInteger(var Tempstr : string; var Result : longint);
var
  Temp : string;
begin
  ParseText(TempStr, Temp);
  Result := StrToInt(Temp);
end;

// - - - - - - - - - - - - - - - - - -
Procedure ParseFloat(var Tempstr : string; var Result : double);
var
  Temp : string;
begin
  ParseText(TempStr, Temp);
  Result := StrToFloat(Temp);
end;

// replacement readln for text file
// EOL could be CRLF, or just LF
{----------------------------------------------------------------------------}
Procedure ReadLine(X_File:F_Link;var InputString:string);
var
  Ch: Char;

begin
  InputString := '';
  Read(X_File^, Ch);
  while ((Ch <> char($0A)) AND (NOT Eof(X_File^))) do begin // look for linefeed
    if (Ch <> char($0D)) then begin // ignore CR
      InputString := InputString + Ch;
    end;
    Read(X_File^, Ch);
  end;
end;

// 40 times slower than regular readln -> not useable !
// replacement readln for text file
// EOL could be CRLF, just LF, or just CR
{----------------------------------------------------------------------------}
Procedure GT_ReadLine(GT_File:GT_Link; var InputString:string);
var
  Ch: Char;

begin
  InputString := '';
  While (true) do begin
    Read(GT_File^, Ch);
    case Ch of
      char($0A) : begin
        Exit;
      end;
      char($0D) : begin
        if ( NOT Eof(GT_File^) ) then begin
          Read(GT_File^, Ch);
          if (Ch <> char($0A)) then begin
            Seek(GT_File^, FilePos(GT_File^)-1);
          end;
        end;
        Exit;
      end;
      else begin
        InputString := InputString + Ch;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------}
{const
  MaxIndex = 512;
var
  Buffer  : array[0..MaxIndex-1] of Char;
  Index   : integer = MaxIndex;
  CRfound : Boolean = false;
  u_File : File;
  BytesRead : integer = 0;

// replacement readln for text file
// EOL could be CRLF, LF or just CR
//---------------------------------------------------------------------------
Procedure xReadLineReset;   // not working becasue of EOF issue
begin
  BytesRead := 0;
end;

//---------------------------------------------------------------------------
Procedure xReadLine(u_File:uF_Link;var InputString:string);
var
  InChar : char;
begin
  CRfound := false;
  InputString := '';
//  While (NOT EOF(u_File^)) do begin
  While (True) do begin
    if (Index >= BytesRead) then begin  // empty ?
      BlockRead(u_File^, buffer, MaxIndex, BytesRead);
      if (BytesRead = 0) then begin
        Exit; // EOF, all done
      end;
      Index := 0;
    end;
    InChar := Buffer[Index];

    if (CRfound) then begin
      if (Inchar = char($0A)) then begin
        INC(Index);
      end;
      Exit; //all done
    end else begin
      case InChar of
        char($0A): begin // LF
          INC(Index);
	  Exit; // all done
        end;
        char($0D): begin // CR
          CRfound := true; // possible end of line
        end;
//        char($09) begin // TAB
//        InChar := ' ';  // optional replace by space
//        InputString := InputString + InChar;
//      end;
        else begin
          InputString := InputString + InChar;
        end;
      end;
      INC(Index);
    end;
  end;
end;
}

// brute force for issue of ',' as decimal
//---------------------------------------------------------------------------
function xxStrToFloat_DotDecimal(sString:String):real;
begin
  try
    result := StrToFloat(sString);
  except
    // try comma for decimal instead
    sString := StringReplace(sString,'.', ',',[]);
//    sString := StringReplace(sString,',', '.',[]);
    result := StrToFloat(sString);
  end;
end;

//---------------------------------------------------------------------------
procedure Force_DecimalSeparator;
var
  changed : Boolean;
begin
  changed := (DecimalSeparator <> '.');
//  changed := (DecimalSeparator <> ',');

  if changed then begin
    DecimalSeparator := '.';
//    DecimalSeparator := ',';
  end;
end;

//---------------------------------------------------------------------------
begin
end.

{=== end of file ============================================================}


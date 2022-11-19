{
 * u_Polar.pas
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
UNIT u_Polar;

{----------------------------------------------------------------------------
Condor .pol file

There seems to be 3 polars with a variable mumber of data points

- Number of data points (Vn, Wn)
  - 05 00 00 00 - 32 bit value

- n times 3x2 floating point values
  - 00 00 80 3F - floating-point= 1.0
  - 00 00 80 3F - floating-point= 1.0
  - 00 00 80 3F - floating-point= 1.0
  - 00 00 80 3F - floating-point= 1.0
  - 00 00 80 3F - floating-point= 1.0
  - 00 00 80 3F - floating-point= 1.0
- etc...
----------------------------------------------------------------------------}

{============================================================================}
INTERFACE

type
  FloatSingleConvert =
    record
      case byte of
        0 : ( RealValue : single );
        1 : ( LongValue : Longword );
        2 : ( ByteValue : array[0..3] of byte );
      end;

  CondorPolar =
    record
      V_S : array[0..6-1] of FloatSingleConvert;
    end;

//var
//  Memo_Message : TMemo;  // external TMemo for messages
//  ProgressBar_Status : TProgressBar;
//  FileFolder : string; // external path for file

procedure ReadPolarFile(PolarFileName : String);
procedure WritePolarFile_Text(PolarFileName_Text : String);

{============================================================================}
IMPLEMENTATION

uses SysUtils;

var
  Polar_Count : FloatSingleConvert;
  Condor_Polars : array of CondorPolar;
  CondorPolar_File : File of FloatSingleConvert;
  CondorPolar_TextFile : TextFile;

{----------------------------------------------------------------------------}
procedure ReadPolarFile(PolarFileName : String);
var
  i,j : integer;

begin
  Polar_Count.LongValue := 0;
  AssignFile(CondorPolar_File,PolarFileName);
  Reset(CondorPolar_File);
  Read(CondorPolar_File,Polar_Count);
  SetLength(Condor_Polars,Polar_Count.LongValue);
  for i := 0 to Polar_Count.LongValue-1 do begin
    for j := 0 to 6-1 do begin
      Read(CondorPolar_File,Condor_Polars[i].V_S[j]);
    end;
  end;

  Close(CondorPolar_File);
end;

{----------------------------------------------------------------------------}
procedure WritePolarFile_Text(PolarFileName_Text : String);
var
  i,j : integer;

begin
  AssignFile(CondorPolar_TextFile,PolarFileName_Text);
  Rewrite(CondorPolar_TextFile);
  writeln(CondorPolar_TextFile,format('count: %d',[Polar_Count.LongValue]));
  SetLength(Condor_Polars,Polar_Count.LongValue);
  j := 0;
  while j<6 do begin
    for i := 0 to Polar_Count.LongValue-1 do begin
      writeln(CondorPolar_TextFile,format('- %f, %f',[
        Condor_Polars[i].V_S[j].RealValue,
        Condor_Polars[i].V_S[j+1].RealValue
        ]));
    end;
    INC(j,2);
    writeln(CondorPolar_TextFile);
  end;

  Close(CondorPolar_TextFile);
end;

{----------------------------------------------------------------------------}
begin { Initialization }
end.

{--- End of File ------------------------------------------------------------}


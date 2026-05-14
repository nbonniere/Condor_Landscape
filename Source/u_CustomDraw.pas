{
 * u_CustomDraw.pas
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
{ Module:     CustomDraw.pas                                                 }
{ Created by: D. Bonniere                                                    }
{ Date:       1 Apr 2026                                                     }
{ Abstract:   Provides custom drawing functions.                             }
{                                                                            }
{============================================================================}

//---------------------------------------------------------------------------
UNIT u_CustomDraw;

{$S-,R-,I-,V-,F+,D+}

{============================================================================}
INTERFACE

uses SysUtils;

//---------------------------------------------------------------------------
// for compile options
{$I Define.pas}

type
  TCallBackProc = procedure(X, Y, V : Integer);

procedure Bresenham(X1, Y1, X2, Y2, V: Integer; CallbackProc: TCallBackProc);
procedure PolyFill(P : PByteArray; xySize, xyExtra : Integer);

{============================================================================}
IMPLEMENTATION

//uses SysUtils, Windows, ShellAPI;

//---------------------------------------------------------------------------
procedure Bresenham(X1, Y1, X2, Y2, V: Integer; CallbackProc: TCallBackProc);
var
  Dx, Dy, Sx, Sy, Error, E2: Integer;
  Done: Boolean;
begin
  Dx := Abs(X2 - X1);
  if (X1 < X2) then begin
    Sx := 1
  end else begin
    Sx := -1;
  end;
  Dy := -Abs(Y2 - Y1);
  if (Y1 < Y2) then begin
    Sy := 1
  end else begin
    Sy := -1;
  end;
  Error := Dx + Dy;

  while (True) do begin
    if (Assigned(CallbackProc)) then begin
      CallbackProc(X1, Y1, V);
    end;
    if ((X1 = X2) and (Y1 = Y2)) then begin
      Exit;
    end;
    E2 := 2 * Error;
    if (E2 >= Dy) then begin
      if (X1 = X2) then begin
        Exit;
      end;
      Error := Error + Dy;
      X1 := X1 + Sx;
    end;
    if (E2 <= Dx) then begin
      if (Y1 = Y2) then begin
        Exit;
      end;
      Error := Error + Dx;
      Y1 := Y1 + Sy;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure PolyFill(P : PByteArray; xySize, xyExtra : Integer);
var
  i, j, k : Integer;
  Value, ValueSave : byte;
  IndexSave : integer;
  Status : (sIdle, sStart, sFinish);
begin
  Status := sIdle;
  // xySize is full width here
  for i := 0 to xyExtra+xySize-1 do begin    // Y direction
    for j := 0 to xyExtra+xySize-1 do begin  // X direction
      Value := P^[(j + i * (xySize+xyExtra))];
      // check mask edge start or finish
      case Status of
        sIdle: begin
          // check if mask set
          if (Value <> 0) then begin
            IndexSave := j;
            ValueSave := Value;
            Status := sStart;
          end;
        end;
        sStart: begin
          // check if mask set
          if (Value <> 0) then begin
            // check if same as previous
            if (Value <> ValueSave) then begin
              // found second edge, fill from IndexSave to here
              for k := IndexSave+1 to j-1 do begin
                P^[(k + i * (xySize+xyExtra))] := {255} 9;
              end;
              ValueSave := Value;
              Status := sFinish;
            end else begin // ignore, i.e. same edge
              IndexSave := j; // move index
            end;
          end;
        end;
        sFinish: begin
          if (Value = 0) then begin
            Status := sIdle;
          end else begin
            // check if same as previous, i.e. ignore
            if (Value <> ValueSave) then begin
              Status := sStart;
            end;
          end;
        end;
      end;
    end;
    // do next line
    Status := sIdle;
  end;
end;

//---------------------------------------------------------------------------
begin
end.

{=== end of file ============================================================}


{
 * Unit_Help.pas
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
unit Unit_Help;

//===========================================================================
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm_Help = class(TForm)
    Memo_Help: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowHelp(Help_FileName : string;
      Left_Pos, Top_Pos : longint);
  end;

var
  Form_Help: TForm_Help;

  ApplicationPath : string;

//===========================================================================
implementation

{$R *.DFM}

//---------------------------------------------------------------------------
procedure TForm_Help.ShowHelp(Help_FileName : string;
  Left_Pos, Top_Pos : longint);
var
  Help_File : TextFile;
  S : string;

begin
  if (NOT Form_Help.Showing) then begin
    Help_FileName := ApplicationPath+'\Help_Files\'+Help_FileName;
    if (FileExists(Help_FileName)) then begin
      {Form_Help.}Memo_Help.Lines.Clear;
      AssignFile(Help_File, Help_FileName);
      Reset(Help_File);
      While (NOT EOF(Help_File)) do begin
        Readln(Help_File, S);
        {Form_Help.}Memo_Help.Lines.Add(S);
      End;
      CloseFile(Help_File);
      {Form_Help.}Left := Left_Pos;
      {Form_Help.}Top  := Top_Pos;
      Form_Help.Show;
    end;
  end else begin
    Form_Help.Close;
  end;
end;

//---------------------------------------------------------------------------
end.

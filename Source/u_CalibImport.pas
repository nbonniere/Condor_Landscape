{
 * u_CalibImport.pas
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
unit u_CalibImport;

//===========================================================================
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm_CalibImport = class(TForm)
    GroupBox_SelectCalib: TGroupBox;
    Button_Proceed: TButton;
    Button_Cancel: TButton;
    GroupBox_CalibFileName: TGroupBox;
    Edit_TileListName: TEdit;
    Button_TileBrowse: TButton;
    OpenDialog_FileName: TOpenDialog;
    procedure Button_TileBrowseClick(Sender: TObject);
    procedure Button_ProceedClick(Sender: TObject);
    procedure Button_CancelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Edit_TileListNameEnter(Sender: TObject);
    procedure Edit_TileListNameExit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form_CalibImport: TForm_CalibImport;
  imActionRequest : boolean;
  imFileName : string;
  imInitialDir : string;
  imFileFilterString : string;

//===========================================================================
implementation

{$R *.DFM}

uses u_Util;

const
  ShortPathNameLength = 50;

//---------------------------------------------------------------------------
procedure TForm_CalibImport.Button_TileBrowseClick(Sender: TObject);
begin
  OpenDialog_FileName.Filter := imFileFilterString;
  OpenDialog_FileName.FileName := imFileName;
  OpenDialog_FileName.InitialDir := imInitialDir;
  if (OpenDialog_FileName.Execute) then begin
    imFileName := OpenDialog_FileName.FileName;

    Edit_TileListName.Text := ShortenFolderString(imFileName,ShortPathNameLength);
    Edit_TileListName.Hint := imFileName;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_CalibImport.Button_ProceedClick(Sender: TObject);
begin
  imActionRequest := true;
  Close;
end;

//---------------------------------------------------------------------------
procedure TForm_CalibImport.Button_CancelClick(Sender: TObject);
begin
  imActionRequest := false;
  Close;
end;

//---------------------------------------------------------------------------
procedure TForm_CalibImport.Edit_TileListNameEnter(Sender: TObject);
begin
  Edit_TileListName.Text := imFileName;
end;

//---------------------------------------------------------------------------
procedure TForm_CalibImport.Edit_TileListNameExit(Sender: TObject);
begin
  imFileName := Edit_TileListName.Text;
  Edit_TileListName.Text := ShortenFolderString(imFileName,ShortPathNameLength);
  Edit_TileListName.Hint := imFileName;
end;

//---------------------------------------------------------------------------
procedure TForm_CalibImport.FormActivate(Sender: TObject);
begin
  imActionRequest := false;
  Edit_TileListName.Text := ShortenFolderString(imFileName,ShortPathNameLength);
  Edit_TileListName.Hint := imFileName;
end;

begin
  imFileFilterString := 'File (*.CSV)|*.CSV|All files (*.*)|*.*';
end.

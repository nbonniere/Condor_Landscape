{
 * u_CalibExport.pas
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
unit u_CalibExport;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm_CalibExport = class(TForm)
    GroupBox_SelectCalib: TGroupBox;
    Button_Proceed: TButton;
    Button_Cancel: TButton;
    GroupBox_CalibFileName: TGroupBox;
    Edit_TileListName: TEdit;
    Button_TileBrowse: TButton;
    SaveDialog_Filename: TSaveDialog;
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
  Form_CalibExport: TForm_CalibExport;
  exActionRequest : boolean;
  exFileName : string;
  exInitialDir : string;
  exFileFilterString : string;

implementation

{$R *.DFM}

uses u_Util;

const
  ShortPathNameLength = 50;

//---------------------------------------------------------------------------
procedure TForm_CalibExport.Button_TileBrowseClick(Sender: TObject);
begin
  SaveDialog_FileName.Filter := exFileFilterString;
  SaveDialog_FileName.FileName := exFileName;
  SaveDialog_FileName.InitialDir := exInitialDir;
  if (SaveDialog_FileName.Execute) then begin
    exFileName := SaveDialog_FileName.FileName;

    Edit_TileListName.Text := ShortenFolderString(exFileName,ShortPathNameLength);
    Edit_TileListName.Hint := exFileName;
//    FIniFile.WriteString('Files','TileFile',TileName);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_CalibExport.Button_ProceedClick(Sender: TObject);
begin
  exActionRequest := true;
  Close;
end;

//---------------------------------------------------------------------------
procedure TForm_CalibExport.Button_CancelClick(Sender: TObject);
begin
  exActionRequest := false;
  Close;
end;

//---------------------------------------------------------------------------
procedure TForm_CalibExport.Edit_TileListNameEnter(Sender: TObject);
begin
  Edit_TileListName.Text := exFileName;
end;

//---------------------------------------------------------------------------
procedure TForm_CalibExport.Edit_TileListNameExit(Sender: TObject);
begin
  exFileName := Edit_TileListName.Text;
  Edit_TileListName.Text := ShortenFolderString(exFileName,ShortPathNameLength);
  Edit_TileListName.Hint := exFileName;
end;

//---------------------------------------------------------------------------
procedure TForm_CalibExport.FormActivate(Sender: TObject);
begin
  exActionRequest := false;
  Edit_TileListName.Text := ShortenFolderString(exFileName,ShortPathNameLength);
  Edit_TileListName.Hint := exFileName;
end;

begin
  exFileFilterString := 'File (*.CSV)|*.CSV|All files (*.*)|*.*';
end.

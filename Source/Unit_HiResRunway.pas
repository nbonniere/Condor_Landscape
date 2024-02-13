{
 * HiResRunway.pas
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
unit Unit_HiResRunway;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm_HiResRunway = class(TForm)
    GroupBox_RunwayDetails: TGroupBox;
    Button_Proceed: TButton;
    Button_Cancel: TButton;
    Label_Length: TLabel;
    Edit_Length: TEdit;
    Label_L_m: TLabel;
    Label_Width: TLabel;
    Edit_Width: TEdit;
    Label_W_m: TLabel;
    GroupBox_Offset: TGroupBox;
    Label_Easting: TLabel;
    Label_E_m: TLabel;
    Label_Northing: TLabel;
    Label_N_m: TLabel;
    Edit_Easting: TEdit;
    Edit_Northing: TEdit;
    procedure Button_CancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button_ProceedClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form_HiResRunway: TForm_HiResRunway;

  gActionRequest : boolean;

//----------------------------------------------------------------------------
implementation

{$R *.DFM}

//----------------------------------------------------------------------------
procedure TForm_HiResRunway.Button_CancelClick(Sender: TObject);
begin
  gActionRequest := false;
  Close;
end;

//----------------------------------------------------------------------------
procedure TForm_HiResRunway.FormCreate(Sender: TObject);
begin
  gActionRequest := false;
end;

//----------------------------------------------------------------------------
procedure TForm_HiResRunway.Button_ProceedClick(Sender: TObject);
begin
  gActionRequest := true;
  Close;
end;

//----------------------------------------------------------------------------
end.

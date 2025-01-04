{
 * Unit_Coords.pas
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
unit Unit_Coords;

//---------------------------------------------------------------------------
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm_Coords = class(TForm)
    GroupBox_LongLat: TGroupBox;
    GroupBox_RelativeUTM: TGroupBox;
    Edit_Longitude: TEdit;
    Edit_Latitude: TEdit;
    Edit_Northing: TEdit;
    Edit_Easting: TEdit;
    Label_Long: TLabel;
    Label_Lat: TLabel;
    Label_East: TLabel;
    Label1: TLabel;
    procedure Edit_LongitudeExit(Sender: TObject);
    procedure Edit_LatitudeExit(Sender: TObject);
    procedure Edit_EastingExit(Sender: TObject);
    procedure Edit_NorthingExit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form_Coords: TForm_Coords;

//---------------------------------------------------------------------------
implementation

{$R *.DFM}

uses
  u_SceneryHDR, u_TileList, u_UTM;

var
  coord_Longitude : double;
  coord_Latitude  : double;

  Relative_Easting  : double;
  Relative_Northing : double;

//---------------------------------------------------------------------------
procedure Calc_EastNorth;
var
  x,y : double;
begin
  LatLongToUTM(coord_Latitude,coord_Longitude,UTM_Zone,UTM_ZoneNS);
  x := -(uEasting - UTM_Right); // Condor X axis reversed
  y := uNorthing - UTM_Bottom;
  Form_Coords.Edit_Easting.Text  := format('%1.3f',[x]);
  Form_Coords.Edit_Northing.Text := format('%1.3f',[y]);
end;

//---------------------------------------------------------------------------
procedure Calc_LongLat;
var
  x,y : double;
begin
  x := -Relative_Easting + UTM_Right; // Condor X axis reversed
  y := Relative_Northing + UTM_Bottom;
  UTMtoLatLong(y,x,UTM_Zone,UTM_ZoneNS);
  Form_Coords.Edit_Longitude.Text := format('%1.6f',[uLongitude]);
  Form_Coords.Edit_Latitude.Text  := format('%1.6f',[uLatitude]);
end;

//---------------------------------------------------------------------------
procedure TForm_Coords.Edit_LongitudeExit(Sender: TObject);
begin
  coord_Longitude := strToFloat(Edit_Longitude.Text);
  Calc_EastNorth;
end;

//---------------------------------------------------------------------------
procedure TForm_Coords.Edit_LatitudeExit(Sender: TObject);
begin
  coord_Latitude := strToFloat(Edit_Latitude.Text);
  Calc_EastNorth;
end;

//---------------------------------------------------------------------------
procedure TForm_Coords.Edit_EastingExit(Sender: TObject);
begin
  Relative_Easting := strToFloat(Edit_Easting.Text);
  Calc_LongLat
end;

//---------------------------------------------------------------------------
procedure TForm_Coords.Edit_NorthingExit(Sender: TObject);
begin
  Relative_Northing := strToFloat(Edit_Northing.Text);
  Calc_LongLat
end;

//---------------------------------------------------------------------------
end.

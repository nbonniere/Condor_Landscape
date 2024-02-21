{
 * u_MakeThermal.pas
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
unit u_MakeThermal;

//===========================================================================
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, {Dialogs,}
  StdCtrls, Grids;

type
  TForm_MakeThermal = class(TForm)
    GroupBox_MakeThermal: TGroupBox;
    CheckBox_Thermal: TCheckBox;
    CheckBox_ThermalBitmap: TCheckBox;
    Button_Proceed: TButton;
    Button_Cancel: TButton;
    CheckBox_Erase: TCheckBox;
    GroupBox_BaseThermalMap: TGroupBox;
    RadioButton_UniformHeating: TRadioButton;
    RadioButton_SunnySlopes: TRadioButton;
    GroupBox_Options: TGroupBox;
    StringGrid_ThermalHeating: TStringGrid;
    Button_HeatingDefaults: TButton;
    procedure Button_CancelClick(Sender: TObject);
    procedure Button_ProceedClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Button_HeatingDefaultsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form_MakeThermal: TForm_MakeThermal;
  ActionRequest : boolean;

//===========================================================================
implementation

{$R *.DFM}

//---------------------------------------------------------------------------
procedure TForm_MakeThermal.Button_CancelClick(Sender: TObject);
begin
  ActionRequest := false;
  Close;
end;

//---------------------------------------------------------------------------
procedure TForm_MakeThermal.Button_ProceedClick(Sender: TObject);
begin
  ActionRequest := true;
  Close;
end;

//---------------------------------------------------------------------------
procedure TForm_MakeThermal.FormActivate(Sender: TObject);
begin
  ActionRequest := false;
end;

//---------------------------------------------------------------------------
procedure TForm_MakeThermal.Button_HeatingDefaultsClick(Sender: TObject);
begin
  with Form_MakeThermal.StringGrid_ThermalHeating do begin
    Cells[1,1] := '40';
    Cells[1,2] := '50';
    Cells[1,3] := '85';
    Cells[1,4] := '0.5';
    Cells[1,5] := '90';
    Cells[1,6] := '60';
    Cells[1,7] := '70';
    Cells[1,8] := '100';
    Cells[1,9] := {'89'}Cells[1,6];
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_MakeThermal.FormCreate(Sender: TObject);
begin
  with Form_MakeThermal.StringGrid_ThermalHeating do begin
    ColWidths[0] := 100;
    ColWidths[1] := 36;
    Width := ColWidths[0] + ColWidths[1] +2;
    Cells[0,0] := 'Terrain';
    Cells[1,0] := '  %';
    Cells[0,1] := 'Deciduous Forest';
    Cells[0,2] := 'Coniferuous Forest';
    Cells[0,3] := 'Swamp';
    Cells[0,4] := 'Water';
    Cells[0,5] := 'Dark Fields';
    Cells[0,6] := 'Green Fields';
    Cells[0,7] := 'Yellow Fields';
    Cells[0,8] := 'Sand/Rock';
    Cells[0,9] := 'Other';
    Button_HeatingDefaultsClick(Sender);
  end;
end;

end.


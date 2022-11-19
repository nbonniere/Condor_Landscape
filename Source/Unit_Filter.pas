{
 * Unit_Filter.pas
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
unit Unit_Filter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Grids, u_BMP;

type
  TForm_Filter = class(TForm)
    GroupBox_Filter: TGroupBox;
    RadioButton_RGB: TRadioButton;
    RadioButton_HSV: TRadioButton;
    GroupBox_RGB: TGroupBox;
    GroupBox_HSV: TGroupBox;
    Edit_MatchRange_Green: TEdit;
    Edit_MatchRange_Red: TEdit;
    Edit_MatchRange_Blue: TEdit;
    Label3: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    Edit_MatchRange_Saturation: TEdit;
    Edit_MatchRange_Hue: TEdit;
    Edit_MatchRange_Value: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    GroupBox_Highlight: TGroupBox;
    Shape1: TShape;
    ColorDialog1: TColorDialog;
    GroupBox_PixelizeColor: TGroupBox;
    Shape2: TShape;
    Button_MatchChange: TButton;
    Button_MatchDefault: TButton;
    Button_PixelizeChange: TButton;
    Button_PixelizeDefault: TButton;
    GroupBox_Convolve: TGroupBox;
    GroupBox_Canny: TGroupBox;
    GroupBox_PreFilter: TGroupBox;
    GroupBox_Thresholds: TGroupBox;
    CheckBox_CannyGaussian: TCheckBox;
    Label7: TLabel;
    Edit_Sigma: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Edit_HighThreshold: TEdit;
    Edit_LowThreshold: TEdit;
    Button_CannyDefault: TButton;
    GroupBox_Reduce: TGroupBox;
    Label1_MaxColors: TLabel;
    Edit_MaxColors: TEdit;
    StringGrid_Matrix: TStringGrid;
    GroupBox_Standard: TGroupBox;
    ComboBox_Matrix: TComboBox;
    GroupBox_Custom: TGroupBox;
    Button_ConvolveMatrixLoad: TButton;
    Button_ConvolveMatrixSave: TButton;
    Label_MatrixDivisor: TLabel;
    Edit_MatrixDivisor: TEdit;
    SaveDialog_Matrix: TSaveDialog;
    OpenDialog_Matrix: TOpenDialog;
    Label_MatrixName: TLabel;
    RadioButton_CIE: TRadioButton;
    GroupBox1: TGroupBox;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Edit_MatchRange_a: TEdit;
    Edit_MatchRange_L: TEdit;
    Edit_MatchRange_b: TEdit;
    procedure Button_MatchChangeClick(Sender: TObject);
    procedure Button_MatchDefaultClick(Sender: TObject);
    procedure Button_PixelizeChangeClick(Sender: TObject);
    procedure Button_PixelizeDefaultClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit_SigmaExit(Sender: TObject);
    procedure Button_CannyDefaultClick(Sender: TObject);
    procedure Edit_MaxColorsExit(Sender: TObject);
    procedure SetGrid(ray : array of integer);
    procedure ComboBox_MatrixChange(Sender: TObject);
    procedure Button_ConvolveMatrixLoadClick(Sender: TObject);
    procedure Button_ConvolveMatrixSaveClick(Sender: TObject);
    procedure Edit_MatchRange_RedExit(Sender: TObject);
    procedure Edit_MatchRange_GreenExit(Sender: TObject);
    procedure Edit_MatchRange_BlueExit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form_Filter: TForm_Filter;

var
//  MatchHighlightColor : Tcolor; //used to highlight colors that match
//  PixelizeHighlightColor : Tcolor; //used to highlight areas that are pixelized
  MatchHighlightColor : ColorConvert; //used to highlight colors that match
  PixelizeHighlightColor : ColorConvert; //used to highlight areas that are pixelized

//---------------------------------------------------------------------------
implementation

{$R *.DFM}

uses u_CannyEdge, u_Util;

var
//  DefaultMatchColor : Tcolor;
//  DefaultPixelizeColor : Tcolor;
  DefaultMatchColor : ColorConvert;
  DefaultPixelizeColor : ColorConvert;

//---------------------------------------------------------------------------
procedure TForm_Filter.Button_MatchChangeClick(Sender: TObject);
begin
  ColorDialog1.Color := Shape1.Brush.Color;
  if ColorDialog1.Execute then begin
    Shape1.Brush.Color := ColorDialog1.Color;
    MatchHighlightColor.ColorValue := ByteSwapColor(ColorDialog1.Color);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Filter.Button_MatchDefaultClick(Sender: TObject);
begin
  MatchHighlightColor := DefaultMatchColor;
  Shape1.Brush.Color := ByteSwapColor(MatchHighlightColor.ColorValue);
end;

//---------------------------------------------------------------------------
procedure TForm_Filter.Button_PixelizeChangeClick(Sender: TObject);
begin
  ColorDialog1.Color := Shape2.Brush.Color;
  if ColorDialog1.Execute then begin
    Shape2.Brush.Color := ColorDialog1.Color;
    PixelizeHighlightColor.ColorValue := ByteSwapColor(ColorDialog1.Color);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Filter.Button_PixelizeDefaultClick(Sender: TObject);
begin
  PixelizeHighlightColor := DefaultPixelizeColor;
  Shape2.Brush.Color := ByteSwapColor(PixelizeHighlightColor.ColorValue);
end;

//---------------------------------------------------------------------------
procedure TForm_Filter.FormCreate(Sender: TObject);
begin
  ComboBox_Matrix.ItemIndex := 0;
  ComboBox_MatrixChange(Sender);

  MatchHighlightColor := DefaultMatchColor;
  Shape1.Brush.Color := ByteSwapColor(MatchHighlightColor.ColorValue);
  PixelizeHighlightColor := DefaultPixelizeColor;
  Shape2.Brush.Color := ByteSwapColor(PixelizeHighlightColor.ColorValue);
end;

//---------------------------------------------------------------------------
procedure TForm_Filter.Edit_SigmaExit(Sender: TObject);
begin
  Label8.Caption := '('+IntToStr(CalcKsize(StrToFloat(Edit_Sigma.Text)))+' pixels)';
end;

//---------------------------------------------------------------------------
procedure TForm_Filter.Button_CannyDefaultClick(Sender: TObject);
begin
  Checkbox_CannyGaussian.Checked := true;
  Edit_Sigma.Text := '1.0';
  Edit_HighThreshold.Text := '90.0';
  Edit_LowThreshold.Text := '10.0';
end;

//---------------------------------------------------------------------------
procedure TForm_Filter.Edit_MaxColorsExit(Sender: TObject);
var
  MaxC : integer;
begin
  MaxC := StrToInt(Edit_MaxColors.text);
  if (MaxC > 100) then begin Edit_MaxColors.text := '256'; end;
  if (MaxC < 2) then begin Edit_MaxColors.text := '2'; end;
end;

{This populates the stringgrid with array values, for visual confirmation}
//----------------------------------------------------------------------------
procedure TForm_Filter.SetGrid(ray : array of integer);
begin
  StringGrid_Matrix.Cells[0,0] := IntToStr(ray[0]);
  StringGrid_Matrix.Cells[1,0] := IntToStr(ray[1]);
  StringGrid_Matrix.Cells[2,0] := IntToStr(ray[2]);
  StringGrid_Matrix.Cells[0,1] := IntToStr(ray[3]);
  StringGrid_Matrix.Cells[1,1] := IntToStr(ray[4]);
  StringGrid_Matrix.Cells[2,1] := IntToStr(ray[5]);
  StringGrid_Matrix.Cells[0,2] := IntToStr(ray[6]);
  StringGrid_Matrix.Cells[1,2] := IntToStr(ray[7]);
  StringGrid_Matrix.Cells[2,2] := IntToStr(ray[8]);
end;

{When a combobox selection is made, fill the array with the filter
 values, and set the divisor value.  Then update the stringgrid,
 and finally just do a button click to apply the filter.}
//----------------------------------------------------------------------------
procedure TForm_Filter.ComboBox_MatrixChange(Sender: TObject);
var
  z : integer;
  ray : array [0..8] of integer;
  
begin
  z := 1;  // just to avoid compiler warnings!
  case ComboBox_Matrix.ItemIndex of
    0 : begin // Laplace
      ray[0] := -1; ray[1] := -1; ray[2] := -1;
      ray[3] := -1; ray[4] :=  8; ray[5] := -1;
      ray[6] := -1; ray[7] := -1; ray[8] := -1;
      z := 1;
      end;
    1 : begin  // Hipass
      ray[0] := -1; ray[1] := -1; ray[2] := -1;
      ray[3] := -1; ray[4] :=  9; ray[5] := -1;
      ray[6] := -1; ray[7] := -1; ray[8] := -1;
      z := 1;
      end;
    2 : begin  // Find Edges (top down)
      ray[0] :=  1; ray[1] :=  1; ray[2] :=  1;
      ray[3] :=  1; ray[4] := -2; ray[5] :=  1;
      ray[6] := -1; ray[7] := -1; ray[8] := -1;
      z := 1;
      end;
    3 : begin  // Sharpen
      ray[0] := -1; ray[1] := -1; ray[2] := -1;
      ray[3] := -1; ray[4] := 16; ray[5] := -1;
      ray[6] := -1; ray[7] := -1; ray[8] := -1;
      z := 8;
      end;
    4 : begin  // Edge Enhance
      ray[0] :=  0; ray[1] := -1; ray[2] :=  0;
      ray[3] := -1; ray[4] :=  5; ray[5] := -1;
      ray[6] :=  0; ray[7] := -1; ray[8] :=  0;
      z := 1;
      end;
    5 : begin  // Color Emboss (Sorta)
      ray[0] :=  1; ray[1] :=  0; ray[2] :=  1;
      ray[3] :=  0; ray[4] :=  0; ray[5] :=  0;
      ray[6] :=  1; ray[7] :=  0; ray[8] := -2;
      z := 1;
      end;
    6 : begin  // Soften
      ray[0] :=  2; ray[1] :=  2; ray[2] :=  2;
      ray[3] :=  2; ray[4] :=  0; ray[5] :=  2;
      ray[6] :=  2; ray[7] :=  2; ray[8] :=  2;
      z := 16;
      end;
    7 : begin  // Blur
      ray[0] :=  3; ray[1] :=  3; ray[2] :=  3;
      ray[3] :=  3; ray[4] :=  8; ray[5] :=  3;
      ray[6] :=  3; ray[7] :=  3; ray[8] :=  3;
      z := 32;
      end;
    8 : begin  // Soften less
      ray[0] :=  0; ray[1] :=  1; ray[2] :=  0;
      ray[3] :=  1; ray[4] :=  2; ray[5] :=  1;
      ray[6] :=  0; ray[7] :=  1; ray[8] :=  0;
      z := 6
      end;
    9 : begin  // lowpass
      ray[0] :=  1; ray[1] :=  1; ray[2] :=  1;
      ray[3] :=  1; ray[4] :=  1; ray[5] :=  1;
      ray[6] :=  1; ray[7] :=  1; ray[8] :=  1;
      z := 9;
      end;
   10 : begin  // Gaussian
      ray[0] :=  1; ray[1] :=  2; ray[2] :=  1;
      ray[3] :=  2; ray[4] :=  4; ray[5] :=  2;
      ray[6] :=  1; ray[7] :=  2; ray[8] :=  1;
      z := 16;
      end;
  end;
  SetGrid(ray);
  Edit_MatrixDivisor.Text := IntToStr(z);
  Label_MatrixName.Caption := ComboBox_Matrix.Text;
end;

//---------------------------------------------------------------------------
procedure TForm_Filter.Button_ConvolveMatrixLoadClick(Sender: TObject);
var
  Filter_File : TextFile;
  i,j :integer;
  x : string;

begin
  OpenDialog_Matrix.Filter := 'Image filters (*.FIL)|*.FIL|All files (*.*)|*.*';
//  OpenDialog_Matrix.FileName := imFileName;
//  OpenDialog_Matrix.InitialDir := imInitialDir;
  if (OpenDialog_Matrix.Execute) then begin
    Label_MatrixName.Caption := ExtractFilename(OpenDialog_Matrix.FileName);
    AssignFile(Filter_File,OpenDialog_Matrix.FileName);
    Reset(Filter_File);
    for i := 0 to 2 do begin
      for j := 0 to 2 do begin
        readln(Filter_File,x);
        StringGrid_Matrix.Cells[j,i] := x;
      end;
    end;
    readln(Filter_File,x);
    Edit_MatrixDivisor.Text := x;
    CloseFile(Filter_File);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Filter.Button_ConvolveMatrixSaveClick(Sender: TObject);
var
  Filter_File : TextFile;
  i,j :integer;

begin
  SaveDialog_Matrix.Filter := 'Image filters (*.FIL)|*.FIL|All files (*.*)|*.*';
//  SaveDialog_Matrix.FileName := imFileName;
//  SaveDialog_Matrix.InitialDir := imInitialDir;
  if (SaveDialog_Matrix.Execute) then begin
    if (ExtractFileExt(SaveDialog_Matrix.FileName) ='') then begin
      SaveDialog_Matrix.FileName := SaveDialog_Matrix.FileName +'.fil';
    end;
    Label_MatrixName.Caption := ExtractFileName(SaveDialog_Matrix.FileName);
    AssignFile(Filter_File,SaveDialog_Matrix.FileName);
    Rewrite(Filter_File);
    for i := 0 to 2 do begin
      for j := 0 to 2 do begin
        writeln(Filter_File,StringGrid_Matrix.Cells[j,i]);
      end;
    end;
    writeln(Filter_File,Edit_MatrixDivisor.Text);
    CloseFile(Filter_File);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Filter.Edit_MatchRange_RedExit(Sender: TObject);
begin
  PercentRangeCheck(Edit_MatchRange_Red);
end;

//---------------------------------------------------------------------------
procedure TForm_Filter.Edit_MatchRange_GreenExit(Sender: TObject);
begin
  PercentRangeCheck(Edit_MatchRange_Green);
end;

//---------------------------------------------------------------------------
procedure TForm_Filter.Edit_MatchRange_BlueExit(Sender: TObject);
begin
  PercentRangeCheck(Edit_MatchRange_Blue);
end;

//---------------------------------------------------------------------------
begin
  DefaultMatchColor.ColorValue := $00FF0000 {RGB(255,0,0)};
//  DefaultPixelizeColor.ColorValue := $00000080 {RGB(0,0,128)};
  DefaultPixelizeColor.ColorValue := $00F50AB4 {RGB(245,10,180)};
end.

//--- End of File -----------------------------------------------------------


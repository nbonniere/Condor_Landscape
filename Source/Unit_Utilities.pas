{
 * Unit_Utilities.pas
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
unit Unit_Utilities;

//===========================================================================
INTERFACE

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, comctrls, ExtCtrls;

type
  TForm_Utilities = class(TForm)
    Button_BMP_Convert: TButton;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    Label2: TLabel;
    Button_BMP_TDM: TButton;
    Button_TDM_BMP: TButton;
    Button_TM3_BMP: TButton;
    Label4: TLabel;
    Button_ReadTerrain: TButton;
    Button_TRN_BMP: TButton;
    Label5: TLabel;
    Label6: TLabel;
    Button_ReadPolar: TButton;
    Label8: TLabel;
    Button_VFRmap: TButton;
    Label9: TLabel;
    Button_Contour: TButton;
    Label10: TLabel;
    Button_QuarterTile: TButton;
    Edit_QuarterTile: TEdit;
    Edit_VFR_Date: TEdit;
    Label11: TLabel;
    Button_DXT3_5_to_DXT1: TButton;
    Label12: TLabel;
    Label13: TLabel;
    Label15: TLabel;
    Button_OBJ_Import_CSV: TButton;
    Button_OBJ_Export_CSV: TButton;
    Label3: TLabel;
    Button_BMP_FOR: TButton;
    Button_FOR_BMP: TButton;
    Label16: TLabel;
    Button_APT_Import_CSV: TButton;
    Button_APT_CSV_Export: TButton;
    Label17: TLabel;
    Label18: TLabel;
    Button_AddAlpha: TButton;
    Label19: TLabel;
    Label20: TLabel;
    Button_FixPuddles: TButton;
    Label21: TLabel;
    Label22: TLabel;
    Button_Shrink: TButton;
    GroupBox1: TGroupBox;
    Button_WGET: TButton;
    ComboBox_GenericTileDownload: TComboBox;
    ComboBox_TileType: TComboBox;
    ComboBox_NamingType: TComboBox;
    Label7: TLabel;
    Button_Reduce: TButton;
    ComboBox_Reduce: TComboBox;
    Label14: TLabel;
    Button_Convert_V1_V2: TButton;
    Label23: TLabel;
    Button_XP_Convert: TButton;
    Label24: TLabel;
    Button_OBJ_LL_Import_CSV: TButton;
    Button_OBJ_LL_Export_CSV: TButton;
    Label25: TLabel;
    Label26: TLabel;
    Button_BMPmask: TButton;
    Shape_Pick: TShape;
    ColorDialog1: TColorDialog;
    Label27: TLabel;
    Label28: TLabel;
    Button_DDS_BMP: TButton;
    CheckBox_DDS_Color: TCheckBox;
    CheckBox_DDS_Transparent: TCheckBox;
    Label29: TLabel;
    Label30: TLabel;
    Button_Convert_V2_V3: TButton;
    Label31: TLabel;
    Button_Hash_Execute: TButton;
    Edit_BMP_Alpha: TEdit;
    CheckBox_AutoAlpha: TCheckBox;
    CheckBox_Water: TCheckBox;
    Label32: TLabel;
    Button_To_XXYY: TButton;
    Button_To_XXXYYY: TButton;
    procedure Button_BMP_ConvrtClick(Sender: TObject);
    procedure Button_BMP_TDMClick(Sender: TObject);
    procedure Button_TDM_BMPClick(Sender: TObject);
    procedure Button_TM3_BMPClick(Sender: TObject);
    procedure Button_ReadTerrainClick(Sender: TObject);
    procedure Button_TRN_BMPClick(Sender: TObject);
    procedure Button_ReadPolarClick(Sender: TObject);
    procedure Button_VFRmapClick(Sender: TObject);
    procedure Button_ContourClick(Sender: TObject);
    procedure Button_QuarterTileClick(Sender: TObject);
    procedure Button_DXT3_5_to_DXT1Click(Sender: TObject);
    procedure Button_OBJ_Import_CSVClick(Sender: TObject);
    procedure Button_OBJ_Export_CSVClick(Sender: TObject);
    procedure Button_APT_Import_CSVClick(Sender: TObject);
    procedure Button_APT_CSV_ExportClick(Sender: TObject);
    procedure Button_AddAlphaClick(Sender: TObject);
    procedure Button_FixPuddlesClick(Sender: TObject);
    procedure Button_WGETClick(Sender: TObject);
    procedure Button_ShrinkClick(Sender: TObject);
    procedure Button_ReduceClick(Sender: TObject);
    procedure Button_Convert_V1_V2Click(Sender: TObject);
    procedure Button_FOR_BMPClick(Sender: TObject);
    procedure Button_XP_ConvertClick(Sender: TObject);
    procedure Button_OBJ_LL_Import_CSVClick(Sender: TObject);
    procedure Button_OBJ_LL_Export_CSVClick(Sender: TObject);
    procedure Button_BMPmaskClick(Sender: TObject);
    procedure Shape_PickMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button_DDS_BMPClick(Sender: TObject);
    procedure V2_FOR_BMP(Sender: TObject);
    procedure Button_BMP_FORClick(Sender: TObject);
    procedure Button_Convert_V2_V3Click(Sender: TObject);
    procedure Button_Hash_ExecuteClick(Sender: TObject);
    procedure Button_To_XXXYYYClick(Sender: TObject);
    procedure Button_To_XXYYClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form_Utilities: TForm_Utilities;

var
  Memo_Message : TMemo;  // external TMemo for messages
  ProgressBar_Status : TProgressBar;
// it seems that if you use a file dialog, the current directory is changed to
// where the folder is for the selected file. So use the Application path
// instead of getcurrent dir, or a '.\' prefix
  ApplicationPath : string;
  Initial_Folder : string;    // external path for file
  Working_Folder : string;    // external path for file
  Condor_Folder : string;     // external path for file
  Compressor_Folder : string; // external path for file
  Library_Folder : string;    // external path for file
  WgetFolder : string;        // external path for Wget
  LEfolder : string;          // external path for Landscape Editor
  LandscapeName : string;     // external path for file
  File_Name : string;         // external name for file
  ZoomLevel : string;
  TileName : string;
  OutputTileSize : string;
  DXT_Type : string;
  ObjectPlacement_Count : Longint;
  opVersion : string;

  procedure MakeDummyFile(FilePath,Filename:string);
  procedure Make_Hashes(LandscapeName, BatchFileName : string);
  Function Execute_BatchFile(FilePath, FileName, Params : String) : DWORD;

//===========================================================================
IMPLEMENTATION

{$R *.DFM}

uses
  FileCtrl, IniFiles,
  Unit_objects, Unit_AirportPlacer, ShellAPI, u_Exec,
  u_BMP, u_Thermal, u_Forest, u_Terrain, u_Polar, u_Convolve, u_MakeGDAL,
  u_UTM, u_Util, u_SceneryHDR, u_TileList, u_Tile_XYZ, u_MakeGMID, u_MakeKML,
  u_GMIDlog, u_DXT, DXTC, u_Object, u_Airport, u_Tiff, u_MakeDDS, u_X_CX,
  u_QuarterTile, u_Condor_NaviconDLL, u_Airspace, u_AutoGen;

const
  faNormalFile = 0; // for use with FindFirst FindNext
  // instead of faAnyFile which includes directories and hidden, etc...

var
  DataChanged : boolean;

{----------------------------------------------------------------------------}
Procedure MessageShow(Info : string);
begin
  if assigned(Memo_Message) then begin
    Memo_Message.lines.add(Info);
  end;
end;

{----------------------------------------------------------------------------}
Procedure MessageClear;
begin
  if assigned(Memo_Message) then begin
    Memo_Message.Lines.Clear;
  end;
end;

//---------------------------------------------------------------------------
procedure MakeDummyFile(FilePath,Filename:string);
var
  Dummy_File : TextFile;
begin
  AssignFile(Dummy_File,FilePath+Filename);
  Rewrite(Dummy_File);
  // empty file
  CloseFile(Dummy_File);
end;

// 24 to 32 bit BMP with Alpha
//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_BMP_ConvrtClick(Sender: TObject);
var
  Filename : String;
  i : integer;
  Alpha : single;

begin
  // select Alpha
  Alpha := strToFloat(Edit_BMP_Alpha.Text);
  if ((Alpha > 1.0) or (Alpha <0.0)) then begin
    MessageShow('Error: must be >0.0 and <=1.0');
    beep; exit;
  end;
  // select files
  OpenDialog1.Options := [ofAllowMultiSelect, ofFileMustExist];
  OpenDialog1.InitialDir := Initial_Folder;
  OpenDialog1.Filter := 'BMP files (*.BMP)|*.BMP|All files (*.*)|*.*';
  OpenDialog1.FileName := '';
  if OpenDialog1.Execute then begin
    try
      Screen.Cursor := crHourGlass;  // Let user know we're busy...
      with OpenDialog1.Files do begin
        for i := 0 to Count - 1 do begin
          FileName := Strings[i];
//          File_Folder := ExtractFileDir(FileName);
          // convert
          u_BMP.Memo_Message := Memo_Message;
          u_BMP.ProgressBar_Status := ProgressBar_Status;
//          Bitmap_24_To_Bitmap_32(FileName,FileName+'-32.bmp');
          Bitmap_24_To_Bitmap_32(FileName,FileName+'-32.bmp',
            CheckBox_AutoAlpha.checked,Alpha);
        end;
      end;
    finally
      Screen.Cursor := crDefault;  // no longer busy
    end;
  end;
end;

// read a bitmap and save as thermal (.TDM) file
//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_BMP_TDMClick(Sender: TObject);
var
  Filename : String;

begin
  OpenDialog1.Options := [ofFileMustExist];
  OpenDialog1.InitialDir := Initial_Folder;
  OpenDialog1.Filter := 'BMP files (*.BMP)|*.BMP|All files (*.*)|*.*';
  OpenDialog1.FileName := '';
  if OpenDialog1.Execute then begin
    try
      Screen.Cursor := crHourGlass;  // Let user know we're busy...
      FileName := OpenDialog1.FileName;
//      File_Folder := ExtractFileDir(OpenDialog1.FileName);
      // convert
      u_Thermal.Memo_Message := Memo_Message;
      u_Thermal.ProgressBar_Status := ProgressBar_Status;
      Greyscale_Bitmap_To_TDM(FileName,FileName+'-tdm.tdm');
    finally
      Screen.Cursor := crDefault;  // no longer busy
    end;
  end;
end;

// read thermal (.TDM) file and save as bitmap
//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_TDM_BMPClick(Sender: TObject);
var
  Filename : String;

begin
  OpenDialog1.Options := [ofFileMustExist];
  OpenDialog1.InitialDir := Initial_Folder;
  OpenDialog1.Filter := 'TDM files (*.TDM)|*.TDM|All files (*.*)|*.*';
  OpenDialog1.FileName := '';
  if OpenDialog1.Execute then begin
    try
      Screen.Cursor := crHourGlass;  // Let user know we're busy...
      FileName := OpenDialog1.FileName;
//      File_Folder := ExtractFileDir(OpenDialog1.FileName);
      // convert
      u_Thermal.Memo_Message := Memo_Message;
      u_Thermal.ProgressBar_Status := ProgressBar_Status;
      TDM_To_Greyscale_Bitmap(FileName,FileName+'-bmp.bmp');
    finally
      Screen.Cursor := crDefault;  // no longer busy
    end;
  end;
end;

// read Hi-Res thermal map (.TM3) file and save as bitmap
//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_TM3_BMPClick(Sender: TObject);
var
  Filename : String;

begin
  OpenDialog1.Options := [ofFileMustExist];
  OpenDialog1.InitialDir := Initial_Folder;
  OpenDialog1.Filter := 'TM3 files (*.TM3)|*.TM3|All files (*.*)|*.*';
  OpenDialog1.FileName := '';
  if OpenDialog1.Execute then begin
    try
      Screen.Cursor := crHourGlass;  // Let user know we're busy...
      FileName := OpenDialog1.FileName;
//      File_Folder := ExtractFileDir(OpenDialog1.FileName);
      // convert
      u_Thermal.Memo_Message := Memo_Message;
      u_Thermal.ProgressBar_Status := ProgressBar_Status;
      TM3_To_Color_Bitmap(FileName,FileName+'-bmp.bmp');
    finally
      Screen.Cursor := crDefault;  // no longer busy
    end;
  end;
end;

// read bitmap file(s) and save as '.FOR' file(s)
//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_BMP_FORClick(Sender: TObject);
{
//  case V1
    ForestResolution := 2; // V1
    // process a patch (pColumns) or a tile (tColumns) ?
//    setLength(ForestGrid,pColumns*ForestResolution,pRows*ForestResolution);
    setLength(ForestGrid,tColumns*ForestResolution,tRows*ForestResolution);

// case V2
    ForestResolution := 8; // V2
    // process a patch (pColumns) or a tile (tColumns) ?
//    setLength(ForestGrid,pColumns*ForestResolution,pRows*ForestResolution);
    setLength(ForestGrid,tColumns*ForestResolution,tRows*ForestResolution);
// option to read a small forest map (forest res 512) and save as .FOR file
//    V2_ForestGrid_To_FOR(pColumns,path?+FileName+'.for');
// set the forest res to 2048 and setlength of ForestGrid
// with the file name, if prefix b or s, then read both, else just read the one combined one
// read file(s), combine into ForestGrid
   //ReadForestBitmapTile(FileName:string; Combine : Boolean) : Boolean;
// cut into 16 pieces and save into 16 .FOR files into ForestMaps or ForestMaps_im ?
// or into Working\ForestMaps or Working\ForestMaps_im ?
//   for each piece
//    V2_ForestGrid_To_FOR(Pcolumns,path?+FileName+'.for');
}
var
  Filename : String;
  File_Folder : String;
  File_Name : String;
  File_Name_NoExt : String;
  Dest_File_Path : String;
  i : integer;

begin
  if (opVersion = 'V1') then begin
    beep; exit;
  end;
  OpenDialog1.Options := [ofAllowMultiSelect, ofFileMustExist];
  OpenDialog1.InitialDir := Initial_Folder+'\Working\Terragen\ForestMaps';
  OpenDialog1.Filter := 'BMP files (*.BMP)|*.BMP|All files (*.*)|*.*';
  OpenDialog1.FileName := '';
  if OpenDialog1.Execute then begin
    ForestResolution := 8; // V2
//    setLength(ForestGrid,pColumns*ForestResolution,pRows*ForestResolution);
//    setLength(ForestGrid,tColumns*ForestResolution,tRows*ForestResolution);
    setLength(ForestGrid,tRows*ForestResolution,tColumns*ForestResolution);
    with OpenDialog1.Files do begin
      try
        ProgressBar_Status.Max := Count;
        Screen.Cursor := crHourGlass;  // Let user know we're busy...

        for i := 0 to Count - 1 do begin
          FileName := Strings[i];
          File_Folder := ExtractFileDir(FileName);
          File_Name := ExtractFileName(FileName);
          File_Name_NoExt := copy(File_Name,1,pos('.bmp',File_Name)-1);
//          Dest_File_Path := File_Folder+'\..\Working\ForestMaps';
//          Dest_File_Path := File_Folder+'\..\Working\ForestMaps_Import';
          Dest_File_Path := Initial_Folder+'\Working\ForestMaps_Import';
          ForceDirectories(Dest_File_Path);
          // read forest bitmap into forestGrid
          u_BMP.BMPfolder := File_Folder; // why u_BMP???
          if (ForestBitmap_To_ForestGrid(File_Name, false) ) then begin
//            // if 512, it is a patch
//            V2_ForestGrid_To_FOR(tcolumns,Dest_File_Path+'\'+File_Name+'.for');
            // if 2048, it is a tile, can be split into 16
            ForestGrid_To_V2_FOR_x16(Dest_File_Path+'\'+File_Name+'.for');
          end;
          ProgressBar_Status.StepIt; Application.ProcessMessages;
        end;
      finally
        Screen.Cursor := crDefault;  // no longer busy
        ProgressBar_Status.Position := 0;
        MessageShow('Conversion done.');
      end;
    end;
  end;
end;

// read '.FOR' file and save as bitmap
// option to recombine 16 patches into one terragen ?
//-------------------------------------------------------------------------------------
procedure TForm_Utilities.V2_FOR_BMP(Sender: TObject);
var
  Filename : String;
  File_Folder : String;
  File_Name : String;
  File_Name_NoExt : String;
  Dest_File_Path : String;
  i : integer;

begin
  OpenDialog1.Options := [ofAllowMultiSelect, ofFileMustExist];
  OpenDialog1.InitialDir := Initial_Folder+'\ForestMaps';
  OpenDialog1.Filter := 'FOR files (*.FOR)|*.FOR|All files (*.*)|*.*';
  OpenDialog1.FileName := '';
  if OpenDialog1.Execute then begin
    ForestResolution := 8; // V2
//    setLength(ForestGrid,pColumns*ForestResolution,pRows*ForestResolution);
    setLength(ForestGrid,tColumns*ForestResolution,tRows*ForestResolution);
    with OpenDialog1.Files do begin
      try
        ProgressBar_Status.Max := Count * 3;  // 3 files exported
        Screen.Cursor := crHourGlass;  // Let user know we're busy...

        for i := 0 to Count - 1 do begin
          FileName := Strings[i];
          File_Folder := ExtractFileDir(FileName);
          File_Name := ExtractFileName(FileName);
          File_Name_NoExt := copy(File_Name,1,pos('.for',File_Name)-1);
//          Dest_File_Path := File_Folder+'\..\Working\ForestMaps';
//          Dest_File_Path := File_Folder+'\..\Working\ForestMaps_Export';
          Dest_File_Path := Working_Folder+'\ForestMaps_Export';
          ForceDirectories(Dest_File_Path);
          // convert
          V2_qFOR_To_ForestGrid(FileName);
//          V2_ForestGrid_To_FOR(pcolumns,FileName+'.xxx'); // re-convert 'patch' for testing
//          ForestGrid_To_2Color_Bitmap(pColumns, fDeciduous, Dest_File_Path+'\bbb'+File_Name_NoExt+'.bmp');
//          ForestGrid_To_2Color_Bitmap(pColumns, fConiferous, Dest_File_Path+'\sss'+File_Name_NoExt+'.bmp');
//          ForestGrid_To_GreyScale_Bitmap(pColumns,fDeciduous,Dest_File_Path+'\bb'+File_Name_NoExt+'.bmp');
//          ForestGrid_To_GreyScale_Bitmap(pColumns,fConiferous,Dest_File_Path+'\ss'+File_Name_NoExt+'.bmp');
//          ForestGrid_To_256Color_Bitmap(pColumns,fDeciduous,Dest_File_Path+'\bb'+File_Name_NoExt+'.bmp');
//          ForestGrid_To_256Color_Bitmap(pColumns,fConiferous,Dest_File_Path+'\ss'+File_Name_NoExt+'.bmp');
          ProgressBar_Status.StepIt; Application.ProcessMessages;
          // b and s must be 24bit for Landscape Editor (?)
          ForestGrid_To_24bit_Bitmap(pColumns,fDeciduous,Dest_File_Path+'\b'+File_Name_NoExt+'.bmp');
          ProgressBar_Status.StepIt; Application.ProcessMessages;
          ForestGrid_To_24bit_Bitmap(pColumns,fConiferous,Dest_File_Path+'\s'+File_Name_NoExt+'.bmp');
          // also show a combined version
//          ForestGrid_To_4Color_Both_Bitmap(pColumns,Dest_File_Path+'\'+File_Name_NoExt+'.bmp');
          ProgressBar_Status.StepIt; Application.ProcessMessages;
          ForestGrid_To_16Color_Both_Bitmap(pColumns,Dest_File_Path+'\'+File_Name_NoExt+'.bmp');
        end;
      finally
        Screen.Cursor := crDefault;  // no longer busy
        ProgressBar_Status.Position := 0;
        MessageShow('Conversion done.');
      end;
    end;
  end;
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_FOR_BMPClick(Sender: TObject);
var
  FileName : string;

begin
  // need tile list
  if (NOT (HeaderOpen AND TileOpen) ) then begin
    MessageShow('Need Header file first');
    Beep;
  end else begin
    u_Forest.Memo_Message := Memo_Message;
    u_Forest.ProgressBar_Status := ProgressBar_Status;

    if (opVersion = 'V1') then begin
      try
        Screen.Cursor := crHourGlass;  // Let user know we're busy...
        SetLength(OverallForestDeciduous, ColumnCount*2*RowCount*2);
        SetLength(OverallForestConiferous,ColumnCount*2*RowCount*2);
        FileName := Initial_Folder+'\'+LandscapeName+'.for';
        FOR_To_OverallForest(FileName);
        Byte_To_Greyscale_Bitmap(OverallForestDeciduous, FileName+'.d.for.BMP');
        Byte_To_Greyscale_Bitmap(OverallForestConiferous,FileName+'.c.for.BMP');

// for testing re-convert
//        OverallForest_To_FOR(FileName+'.FOR');
//        FOR_To_OverallForest(FileName+'.FOR');
//        Byte_To_Greyscale_Bitmap(OverallForestDeciduous, FileName+'.FOR.d.for.BMP');
//        Byte_To_Greyscale_Bitmap(OverallForestConiferous,FileName+'.FOR.c.for.BMP');

      finally
        Screen.Cursor := crDefault;  // no longer busy
      end;
    end else begin // must be V2
      V2_FOR_BMP(Sender);
    end;
  end;
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_ReadTerrainClick(Sender: TObject);
var
  Filename : String;

begin
  OpenDialog1.Options := [ofFileMustExist];
  OpenDialog1.InitialDir := Initial_Folder;
  OpenDialog1.Filter := 'TRN files (*.TRN)|*.TRN|All files (*.*)|*.*';
  OpenDialog1.FileName := '';
  if OpenDialog1.Execute then begin
    try
      Screen.Cursor := crHourGlass;  // Let user know we're busy...
      FileName := OpenDialog1.FileName;
//      File_Folder := ExtractFileDir(OpenDialog1.FileName);
      // read terrain header
      u_Terrain.Memo_Message := Memo_Message;
      u_Terrain.ProgressBar_Status := ProgressBar_Status;
      ReadTerrainHeader(Filename);
    finally
      Screen.Cursor := crDefault;  // no longer busy
    end;
  end;
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_TRN_BMPClick(Sender: TObject);
var
  Filename : String;

begin
  OpenDialog1.Options := [ofFileMustExist];
  OpenDialog1.InitialDir := Initial_Folder;
  OpenDialog1.Filter := 'Terrain files (*.TRN;*.TR3;*.RAW)|*.TRN;*.TR3;*.RAW|All files (*.*)|*.*';
  OpenDialog1.FileName := '';
  if OpenDialog1.Execute then begin
    try
      FileName := OpenDialog1.FileName;
//      File_Folder := ExtractFileDir(OpenDialog1.FileName);
      // read terrain header
      u_Terrain.Memo_Message := Memo_Message;
      u_Terrain.ProgressBar_Status := ProgressBar_Status;
      TRN_To_Greyscale_Bitmap(FileName,FileName+'-bmp.bmp');
    finally
      Screen.Cursor := crDefault;  // no longer busy
    end;
  end;
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_ReadPolarClick(Sender: TObject);
var
  Filename : String;

begin
  OpenDialog1.Options := [ofFileMustExist];
  if (DirectoryExists(Condor_Folder + '\planes')) then begin
    OpenDialog1.InitialDir := Condor_Folder + '\planes';
  end else begin
    OpenDialog1.InitialDir := Condor_Folder;
  end;
  OpenDialog1.Filter := 'POL files (*.POL)|*.POL|All files (*.*)|*.*';
  OpenDialog1.FileName := '';
  if OpenDialog1.Execute then begin
    FileName := OpenDialog1.FileName;
//    File_Folder := ExtractFileDir(OpenDialog1.FileName);
    // read polar data
//    u_Polar.Memo_Message := Memo_Message;
//    u_Polar.ProgressBar_Status := ProgressBar_Status;
    ReadPolarFile(Filename);
    WritePolarFile_Text(Filename+'.txt');
    MessageShow('Polar '+Filename+'.txt generated');
  end;
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_ShrinkClick(Sender: TObject);
var
  Filename : String;
  i : integer;

begin
  OpenDialog1.Options := [ofAllowMultiSelect, ofFileMustExist];
  OpenDialog1.InitialDir := Initial_Folder;
  OpenDialog1.Filter := 'BMP files (*.BMP)|*.BMP|All files (*.*)|*.*';
  OpenDialog1.FileName := '';
  if OpenDialog1.Execute then begin
    with OpenDialog1.Files do begin
      for i := 0 to Count - 1 do begin
        FileName := Strings[i];
//        File_Folder := ExtractFileDir(FileName);
        u_Convolve.ProgressBar_Status := ProgressBar_Status;
//        u_Convolve.BitmapShrink_File(Filename, tDeciduous.cRGB);
        u_Convolve.ForestBitmapShrink_File(Filename, Filename+'_s.bmp');
      end;
    end;
  end;
end;

// change to use matching strings {z}{x}{y} but only works if file name is single
// parameter such as {y].jpg since file needs to be combined and need bare file name only
//-------------------------------------------------------------------------------------
procedure WGET_Generic(Row_B, Col_R, Row_T, Col_L, ZoomLevel : Integer;
  TMStype, SwapXY : Boolean; URL, Name, FilePath : string);
const
  ExtraDist = 0.1;  // extra 100 metres on each edge
var
  FileName : string;
  GDALfile, URLfile, RENfile : TextFile;
  i, j : integer;
  Xsize,Ysize : real;
  Generic_TN: array [0..2-1,0..2-1] of integer;
  f_URL : string;
  f_File : string;
begin
  // need landscape header and tile extent
  if (HeaderOpen) AND (TileOpen) then begin
    // zoom and tile type
    Zoom := ZoomLevel;
    TMS := TMStype;
    // extra distance around edges to make sure
    Ysize := arctan(ExtraDist/earthRadius)*180.0/Pi;
//    Xsize := Ysize*cos(CornerList[0].TileLatBottom*Pi/180);
    Xsize := Ysize*cos(TileList[0].TileLatBottom*Pi/180);

    // calc tile numbers from top left (corner[3]) and bottom right (corner[0])
    // top left
    lat_deg := f_Maximum(TileList[(Row_T)*(TileColumnCount+1)+Col_R].TileLatBottom,
      TileList[(Row_T)*(TileColumnCount+1)+Col_L].TileLatBottom) + Ysize;
    lon_deg := f_Minimum(TileList[(Row_T)*(TileColumnCount+1)+Col_L].TileLongRight,
      TileList[(Row_B)*(TileColumnCount+1)+Col_L].TileLongRight) - Xsize;
    LatLong_To_Tile;
    Generic_TN[0,0] := xtile;
    if (NOT TMS) then begin
      Generic_TN[0,1] := ytile;
    end else begin
      Generic_TN[1,1] := ytile;
    end;
    // bottom right
    lat_deg := f_Minimum(TileList[(Row_B)*(TileColumnCount+1)+Col_R].TileLatBottom,
      TileList[(Row_B)*(TileColumnCount+1)+Col_L].TileLatBottom) - Ysize;
    lon_deg := f_Maximum(TileList[(Row_B)*(TileColumnCount+1)+Col_R].TileLongRight,
      TileList[(Row_T)*(TileColumnCount+1)+Col_R].TileLongRight) + Xsize;
    LatLong_To_Tile;
    Generic_TN[1,0] := xtile;
    if (NOT TMS) then begin
      Generic_TN[1,1] := ytile;
    end else begin
      Generic_TN[0,1] := ytile;
    end;
    Memo_Message.Lines.Clear;
    MessageShow(format('Z: %d',[zoom]));
    MessageShow(format('Y: %d .. %d',[Generic_TN[0,1],Generic_TN[1,1]]));
    MessageShow(format('X: %d .. %d',[Generic_TN[0,0],Generic_TN[1,0]]));

    if (NOT DirectoryExists(FilePath+'\URLs')) then begin
      ForceDirectories(FilePath+'\URLs');
    end;

    if (SwapXY) then begin
      // convert from tile Z/X/Y to Z/Y/X format
      // mkdir row folders
      // move column files to rows and rename from row names to column names
      // open the file
      FileName := 'Swap_XY.bat';
      AssignFile(GDALfile, FilePath +'\'+ FileName);
      Rewrite(GDALfile);
      for i := Generic_TN[0,0] to Generic_TN[1,0] do begin
        writeln(GDALfile, 'mkdir '+Name+'\'+IntToStr(i));
        for j := Generic_TN[0,1] to Generic_TN[1,1] do begin
          writeln(GDALfile, 'move /Y '+Name+'\WG\'+IntToStr(j)+'\'+IntToStr(i)+'.jpg '+Name+'\'+IntToStr(i)+'\'+IntToStr(j)+'.jpg');
        end;
      end;
      // close the file
      CloseFile(GDALfile);
    end;

    FileName := 'Generic_WGET.bat';
    AssignFile(GDALfile, FilePath +'\'+ FileName);
    Rewrite(GDALfile);

    writeln(GDALfile,'echo off');
    writeln(GDALfile,'setlocal');
    writeln(GDALfile,'set PATH=%PATH%;'+WgetFolder);
    writeln(GDALfile,'cd /d %~dp0');
    if (NOT SwapXY) then begin
      for i := Generic_TN[0,0] to Generic_TN[1,0] do begin
//        writeln(GDALfile, 'wget -P '+Name+'\'+IntToStr(i)+' -i URLs\urls_'+IntToStr(i)+'.txt');
        writeln(GDALfile, 'wget -P '+Name+'\'+IntToStr(i)+' -i URLs\urls_'+IntToStr(i)+'.txt');
        writeln(GDALfile, 'call RENs\rens_'+IntToStr(i)+'.bat');
        AssignFile(RENfile, FilePath +'\RENs\rens_'+IntToStr(i)+'.bat');
        Rewrite(RENfile);
        writeln(RENfile,'setlocal');
        writeln(RENfile,'cd /d %~dp0');
        AssignFile(URLfile, FilePath +'\URLs\urls_'+IntToStr(i)+'.txt');
        Rewrite(URLfile);
        for j := Generic_TN[0,1] to Generic_TN[1,1] do begin
//          writeln(URLfile, URL+'/'+IntToStr(zoom)+'/'+IntToStr(i)+'/'+IntToStr(j)+'.jpg');
          f_URL := StringReplace(URL,'{z}',IntToStr(zoom), [rfReplaceAll]);
          f_URL := StringReplace(f_URL,'{x}',IntToStr(i), [rfReplaceAll]);
          f_URL := StringReplace(f_URL,'{y}',IntToStr(j), [rfReplaceAll]);
          writeln(URLfile, f_URL);
          f_File := StringReplace(f_URL,'/','\', [rfReplaceAll]);
          f_File := ExtractFileName(f_File);
          writeln(RENfile, 'rename ..\Overall\'+IntToStr(i)+'\'+f_File+' '+IntToStr(j)+'.jpg');
        end;
        writeln(RENfile,'endlocal');
        CloseFile(RENfile);
        CloseFile(URLfile);
      end;
    end else begin
      writeln(GDALfile, 'mkdir '+Name+'\WG');  // use Download folder in case of name conflict
      for i := Generic_TN[0,1] to Generic_TN[1,1] do begin
        writeln(GDALfile, 'wget -P '+Name+'\WG\'+IntToStr(i)+' -i URLs\urls_'+IntToStr(i)+'.txt');
        writeln(GDALfile, 'call RENs\rens_'+IntToStr(i)+'.bat');
        AssignFile(RENfile, FilePath +'\RENs\rens_'+IntToStr(i)+'.bat');
        Rewrite(RENfile);
        writeln(RENfile,'setlocal');
        writeln(RENfile,'cd /d %~dp0');
        AssignFile(URLfile, FilePath +'\URLs\urls_'+IntToStr(i)+'.txt');
        Rewrite(URLfile);
        for j := Generic_TN[0,0] to Generic_TN[1,0] do begin
//          writeln(URLfile, URL+'/'+IntToStr(zoom)+'/'+IntToStr(i)+'/'+IntToStr(j)+'.jpg');
          f_URL := StringReplace(URL,'{z}',IntToStr(zoom), [rfReplaceAll]);
          f_URL := StringReplace(f_URL,'{x}',IntToStr(i), [rfReplaceAll]);
          f_URL := StringReplace(f_URL,'{y}',IntToStr(j), [rfReplaceAll]);
          writeln(URLfile, f_URL);
          f_File := StringReplace(f_URL,'/','\', [rfReplaceAll]);
          f_File := ExtractFileName(f_File);
          writeln(RENfile, 'rename ..\Overall\'+IntToStr(i)+'\'+f_File+' '+IntToStr(j)+'.jpg');
        end;
        writeln(RENfile,'endlocal');
        CloseFile(RENfile);
        CloseFile(URLfile);
      end;
    end;

    writeln(GDALfile,'endlocal');
    // close the file
    CloseFile(GDALfile);

    if (NOT TMS) then begin
      xtile := Generic_TN[0,0]; ytile := Generic_TN[0,1];
      Tile_To_LatLong;
      SourceTopLatitude := lat_deg;
      SourceLeftLongitude := lon_deg;
      xtile := 1 + Generic_TN[1,0]; ytile := 1 + Generic_TN[1,1];
      Tile_To_LatLong;
      SourceBottomLatitude := lat_deg;
      SourceRightLongitude := lon_deg;
    end else begin
      xtile := Generic_TN[0,0]; ytile := Generic_TN[1,1];
      Tile_To_LatLong;
      SourceTopLatitude := lat_deg;
      SourceLeftLongitude := lon_deg;
      xtile := 1 + Generic_TN[1,0]; ytile := -1 + Generic_TN[0,1];
      Tile_To_LatLong;
      SourceBottomLatitude := lat_deg;
      SourceRightLongitude := lon_deg;
    end;
//    BitmapWidth  := 256 * (1 + Generic_TN[1,0] - Generic_TN[0,0]);
//    BitmapHeight := 256 * (1 + Generic_TN[1,1] - Generic_TN[0,1]);

//    GDALfolder := Working_Folder;
//    GDALlibraryfolder := Library_Folder;
//    MakeGDALoverallBatchFile(Name);  // fixed output size

    // also need a .gmid file for combiner to combine ???
    // TBD - use one from Overall map for now
    AssignFile(GDALfile, FilePath +'\'+Name+'.umd');
    Rewrite(GDALfile);
    writeln(GDALfile, '[MapsType]');
    if (NOT TMS) then begin
      writeln(GDALfile, 'MapsType=4');
    end else begin
      writeln(GDALfile, 'MapsType=6');
    end;

    // for completeness add area before and after and zoom ?
    writeln(GDALfile,'[AREA]');
    writeln(GDALfile,'LeftLongitude='+format('%1.8f',[SourceLeftLongitude]));
    writeln(GDALfile,'RightLongitude='+format('%1.8f',[SourceRightLongitude]));
    writeln(GDALfile,'TopLatitude='+format('%1.8f',[SourceTopLatitude]));
    writeln(GDALfile,'BottomLatitude='+format('%1.8f',[SourceBottomLatitude]));
    writeln(GDALfile);

    writeln(GDALfile,'Left_Longitude_download='+format('%1.8f',[SourceLeftLongitude]));
    writeln(GDALfile,'Right_Longitude_download='+format('%1.8f',[SourceRightLongitude]));
    writeln(GDALfile,'Top_Latitude_download='+format('%1.8f',[SourceTopLatitude]));
    writeln(GDALfile,'Bottom_Latitude_download='+format('%1.8f',[SourceBottomLatitude]));
    writeln(GDALfile);
    writeln(GDALfile,'[Zoom]');
    writeln(GDALfile,'Zoom='+IntToStr(Zoom));

    writeln(GDALfile, '[XY]');
    writeln(GDALfile, 'MinX='+IntToStr(Generic_TN[0,0]));
    writeln(GDALfile, 'MinY='+IntToStr(Generic_TN[0,1]));
    writeln(GDALfile, 'MaxX='+IntToStr(Generic_TN[1,0]));
    writeln(GDALfile, 'MaxY='+IntToStr(Generic_TN[1,1]));

    writeln(GDALfile, '[FolderStyle]');
    writeln(GDALfile, 'FolderStyleValue=2');
    writeln(GDALfile, '[TilenameStyle]');
    writeln(GDALfile, 'TilenameStyleValue=2');
    CloseFile(GDALfile);

  end else begin
    MessageShow('Need Header file first');
    Beep;
  end;
end;

{---------------------------------------------------------------------------
Identify and download tiles to get from VFRmap.com
- Map type ? - TMS, yyy folders, files xxx.jpg (VFRmap)
use same file structure as for Overall GMID so as to use the combiner
with overall.gmid and use Overal.gdal to convert the map
(assuming same zoom level)
NO, won't work -> UMD Yahoo maps uses Z/X/Y.jpg not Z/Y/X.jpg !
try using a different map type that uses  Z/Y/X ?
- Map type 0 - XYZ, xxx folders, files yyy.jpg (Google street maps)
- Map type 1 - XYZ, single folder, files gs_xxx_yyy_zz.jpg (Google)
- Map type 4 - XYZ, xxx folders, files yyy.jpg (Yahoo)
- Map type 6 - TMS, xxx folders, files yyy.jpg (Virtual Earth)
---------------------------------------------------------------------------}

// uses WGET, SwapXY, and UMD combiner.
//-------------------------------------------------------------------------------------
{
procedure TForm_Utilities.Button_VFRmapClick(Sender: TObject);
const
  ExtraDist = 0.1;  // extra 100 metres on each edge
var
  VFRdate : string;
  FileName : string;
  FilePath : string;
  GDALfile, URLfile : TextFile;
  i, j : integer;
  Xsize,Ysize : real;
  VFR_TN: array [0..2-1,0..2-1] of integer;
begin
//  WGET_Generic(0, 0, TileRowCount-1+1, TileColumnCount-1+1, 11, True, True,
//    'http://vfrmap.com/20200227/tiles/vfrc',
//    'TEST', Working_Folder+'\SourceTiles\TEST');
//  exit;

  // need landscape header and tile extent
  if (HeaderOpen) AND (TileOpen) then begin
    // date
    VFRdate := Edit_VFR_Date.Text;
    if (length(VFRdate) <> 8) then begin
      MessageShow('Date must be in yyyymmdd format');
      exit;
    end;

    // zoom and tile type
    zoom := 11;
    TMS := true;
    // extra distance around edges to make sure
    Ysize := arctan(ExtraDist/earthRadius)*180.0/Pi;
//    Xsize := Ysize*cos(CornerList[0].TileLatBottom*Pi/180);
    Xsize := Ysize*cos(TileList[0].TileLatBottom*Pi/180);
    // calc tile numbers from top left (corner[3]) and bottom right (corner[0])
    // top left
    lat_deg := f_Maximum(CornerList[2].TileLatBottom, CornerList[3].TileLatBottom) + Ysize;
    lon_deg := f_Minimum(CornerList[3].TileLongRight, CornerList[1].TileLongRight) - Xsize;
    LatLong_To_Tile;
    VFR_TN[0,0] := xtile;
    if (NOT TMS) then begin
      VFR_TN[0,1] := ytile;
    end else begin
      VFR_TN[1,1] := ytile;
    end;
    // bottom right
    lat_deg := f_Minimum(CornerList[0].TileLatBottom, CornerList[1].TileLatBottom) - Ysize;
    lon_deg := f_Maximum(CornerList[0].TileLongRight, CornerList[2].TileLongRight) + Xsize;
    LatLong_To_Tile;
    VFR_TN[1,0] := xtile;
    if (NOT TMS) then begin
      VFR_TN[1,1] := ytile;
    end else begin
      VFR_TN[0,1] := ytile;
    end;
    Memo_Message.Lines.Clear;
//    MessageShow('http://vfrmap.com/20181108/tiles/vfrc/Z/Y/X.jpg');
    MessageShow('http://vfrmap.com/'+VFRdate+'/tiles/vfrc/Z/Y/X.jpg');
    MessageShow(format('Z: %d',[zoom]));
    MessageShow(format('Y: %d .. %d',[VFR_TN[0,1],VFR_TN[1,1]]));
    MessageShow(format('X: %d .. %d',[VFR_TN[0,0],VFR_TN[1,0]]));

    FilePath := Working_Folder+'\SourceTiles\VFRmap';
    if (NOT DirectoryExists(FilePath+'\URLs')) then begin
      ForceDirectories(FilePath+'\URLs');
    end;
    if (NOT DirectoryExists(FilePath+'\VFRmap')) then begin
      ForceDirectories(FilePath+'\VFRmap');
    end;

    // convert from tile Z/X/Y to Z/Y/X format
    // mkdir row folders
    // move column files to rows and rename from row names to column names

    //open the file
    FileName := 'Swap_XY.bat';
    AssignFile(GDALfile, FilePath +'\'+ FileName);
    Rewrite(GDALfile);

    for i := VFR_TN[0,0] to VFR_TN[1,0] do begin
      writeln(GDALfile, 'mkdir VFRmap\T'+IntToStr(i));  // use Temporary folder in case of name conflict
      for j := VFR_TN[0,1] to VFR_TN[1,1] do begin
        writeln(GDALfile, 'move /Y VFRmap\'+IntToStr(j)+'\'+IntToStr(i)+'.jpg VFRmap\T'+IntToStr(i)+'\'+IntToStr(j)+'.jpg');
      end;
    end;
    // erase empty folders
    for j := VFR_TN[0,1] to VFR_TN[1,1] do begin
      writeln(GDALfile, 'rmdir VFRmap\'+IntToStr(j));
    end;
    // now rename temp folders
    for i := VFR_TN[0,0] to VFR_TN[1,0] do begin
      writeln(GDALfile, 'rename VFRmap\T'+IntToStr(i) + ' '+IntToStr(i));
    end;

    // close the file
    CloseFile(GDALfile);

    FileName := 'VFRmap_WGET.bat';
    AssignFile(GDALfile, FilePath +'\'+ FileName);
    Rewrite(GDALfile);

    writeln(GDALfile,'echo off');
    writeln(GDALfile,'setlocal');
    writeln(GDALfile,'set PATH=%PATH%;c:\programs\wget');
//    for i := VFR_TN[0,0] to VFR_TN[1,0] do begin
//      writeln(GDALfile, 'mkdir VFRmap\'+IntToStr(i));
//      for j := VFR_TN[0,1] to VFR_TN[1,1] do begin
////        writeln(GDALfile, 'wget -O .\VFRmap\'+IntToStr(i)+'\'+IntToStr(j)+'.jpg http://vfrmap.com/20181108/tiles/vfrc/'+IntToStr(zoom)+'/'+IntToStr(j)+'/'+IntToStr(i)+'.jpg');
//        writeln(GDALfile, 'wget -O .\VFRmap\'+IntToStr(i)+'\'+IntToStr(j)+'.jpg http://vfrmap.com/'+VFRdate+'/tiles/vfrc/'+IntToStr(zoom)+'/'+IntToStr(j)+'/'+IntToStr(i)+'.jpg');
//      end;
//    end;
    for i := VFR_TN[0,1] to VFR_TN[1,1] do begin
//      writeln(GDALfile, 'mkdir VFRmap\'+IntToStr(i));
      writeln(GDALfile, 'wget -P VFRmap\'+IntToStr(i)+' -i URLs\urls_'+IntToStr(i)+'.txt');
      AssignFile(URLfile, FilePath +'\URLs\urls_'+IntToStr(i)+'.txt');
      Rewrite(URLfile);
      for j := VFR_TN[0,0] to VFR_TN[1,0] do begin
//        writeln(URLfile, 'http://vfrmap.com/20181108/tiles/vfrc/'+IntToStr(zoom)+'/'+IntToStr(i)+'/'+IntToStr(j)+'.jpg');
        writeln(URLfile, 'http://vfrmap.com/'+VFRdate+'/tiles/vfrc/'+IntToStr(zoom)+'/'+IntToStr(i)+'/'+IntToStr(j)+'.jpg');
      end;
      CloseFile(URLfile);
    end;

    writeln(GDALfile,'endlocal');
    // close the file
    CloseFile(GDALfile);

    if (NOT TMS) then begin
      xtile := VFR_TN[0,0]; ytile := VFR_TN[0,1];
      Tile_To_LatLong;
      SourceTopLatitude := lat_deg;
      SourceLeftLongitude := lon_deg;
      xtile := 1 + VFR_TN[1,0]; ytile := 1 + VFR_TN[1,1];
      Tile_To_LatLong;
      SourceBottomLatitude := lat_deg;
      SourceRightLongitude := lon_deg;
    end else begin
      xtile := VFR_TN[0,0]; ytile := VFR_TN[1,1];
      Tile_To_LatLong;
      SourceTopLatitude := lat_deg;
      SourceLeftLongitude := lon_deg;
      xtile := 1 + VFR_TN[1,0]; ytile := -1 + VFR_TN[0,1];
      Tile_To_LatLong;
      SourceBottomLatitude := lat_deg;
      SourceRightLongitude := lon_deg;
    end;
//    BitmapWidth  := 256 * (1 + VFR_TN[1,0] - VFR_TN[0,0]);
//    BitmapHeight := 256 * (1 + VFR_TN[1,1] - VFR_TN[0,1]);

    u_MakeGDAL.GDALfolder := Working_Folder;
    u_MakeGDAL.GDALlibraryfolder := Library_Folder;
    MakeGDALoverallBatchFile('VFRmap');

    // also need a .gmid file for combiner to combine ???
    // TBD - use one from Overall map for now
    AssignFile(GDALfile, FilePath +'\VFRmap.umd');
    Rewrite(GDALfile);
    writeln(GDALfile, '[MapsType]');
    writeln(GDALfile, 'MapsType=6');
    writeln(GDALfile, '[XY]');
    writeln(GDALfile, 'MinX='+IntToStr(VFR_TN[0,0]));
    writeln(GDALfile, 'MinY='+IntToStr(VFR_TN[0,1]));
    writeln(GDALfile, 'MaxX='+IntToStr(VFR_TN[1,0]));
    writeln(GDALfile, 'MaxY='+IntToStr(VFR_TN[1,1]));

    writeln(GDALfile, '[FolderStyle]');
    writeln(GDALfile, 'FolderStyleValue=2');
    writeln(GDALfile, '[TilenameStyle]');
    writeln(GDALfile, 'TilenameStyleValue=2');
    CloseFile(GDALfile);

  end else begin
    MessageShow('Need Header file first');
    Beep;
  end;
end;
}
// uses UMD custom map definition
//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_VFRmapClick(Sender: TObject);
const
  ExtraDist = 0.1;  // extra 100 metres on each edge
  // with zoom of 10, a small expansion/warp is needed
  // with zoom of 11, a large shrinkage/warp is needed, but quality may be a little better
  Default_Zoom = '11';
  xyz_Order = '{z}/{y}/{x}';  // Y first
  espg = '3857';

var
  VFRdate : string;
  httpReference : string;
//  FileName : string;
  FilePath : string;
  GDALfile : TextFile;
//  i : integer;
  Xsize,Ysize : real;

  Tile_Top_Lat  : real;
  Tile_Left_Long : real;
  Tile_Bottom_Lat  : real;
  Tile_Right_Long : real;

begin
  // need landscape header and tile extent
  if (HeaderOpen) AND (TileOpen) then begin
    // date
    VFRdate := Edit_VFR_Date.Text;
    if (length(VFRdate) <> 8) then begin
      MessageShow('Date must be in yyyymmdd format');
      exit;
    end;

    // zoom and tile type
    zoom := 11;
    TMS := true;
    // extra distance around edges to make sure
    Ysize := arctan(ExtraDist/earthRadius)*180.0/Pi;
//    Xsize := Ysize*cos(CornerList[0].TileLatBottom*Pi/180);
    Xsize := Ysize*cos(TileList[0].TileLatBottom*Pi/180);
    // calc tile numbers from top left (corner[3]) and bottom right (corner[0])
    Tile_Bottom_Lat := f_Minimum(CornerList[0].TileLatBottom, CornerList[1].TileLatBottom);
    Tile_Top_Lat :=    f_Maximum(CornerList[2].TileLatBottom, CornerList[3].TileLatBottom);
    Tile_Left_Long :=  f_Minimum(CornerList[3].TileLongRight, CornerList[1].TileLongRight);
    Tile_Right_Long := f_Maximum(CornerList[0].TileLongRight, CornerList[2].TileLongRight);

    FilePath := Working_Folder+'\SourceTiles\VFRmap';
    if (NOT DirectoryExists(FilePath+'\VFRmap')) then begin
      ForceDirectories(FilePath+'\VFRmap');
    end;

    u_MakeGDAL.GDALfolder := Working_Folder;
    u_MakeGDAL.GDALlibraryfolder := Library_Folder;
//    MakeGDALoverallBatchFile('VFRmap');
    MakeAutoGDALoverallBatchFile('VFRmap');

    // make custom map (Universal)
    AssignFile(GDALfile, FilePath +'\Initial_VFRmap.umd');
    Rewrite(GDALfile);
    writeln(GDALfile, '[MapsType]');
    writeln(GDALfile, 'MapsType=263');
    writeln(GDALfile);
    writeln(GDALfile,'[AREA]');
    writeln(GDALfile,'LeftLongitude='+format('%1.8f',[Tile_Left_Long - Xsize]));
    writeln(GDALfile,'RightLongitude='+format('%1.8f',[Tile_Right_Long + Xsize]));
    writeln(GDALfile,'TopLatitude='+format('%1.8f',[Tile_Top_Lat + Ysize]));
    writeln(GDALfile,'BottomLatitude='+format('%1.8f',[Tile_Bottom_Lat - Ysize]));
    writeln(GDALfile);
    writeln(GDALfile,'[Zoom]');
    writeln(GDALfile,'Zoom='+Default_Zoom);
    writeln(GDALfile);
    writeln(GDALfile,'[CustomMap]');
    writeln(GDALfile,'PngOrJpg=2');   // jpeg
    if (TMS = true) then begin
      writeln(GDALfile,'Direction=2');
    end else begin
      writeln(GDALfile,'Direction=1');
    end;
    writeln(GDALfile,'Width=256');
    writeln(GDALfile,'Height=256');
    writeln(GDALfile,'SSL=0');
    httpReference := 'http://vfrmap.com/'+VFRdate+'/tiles/vfrc';
    writeln(GDALfile,'Referer='+httpReference);
    writeln(GDALfile,'UrlTemplate='+httpReference+'/'+xyz_Order+'.jpg');
    if (espg = '3857') then begin
      writeln(GDALfile,'MType=1');  // espg:3857
    end else begin
      writeln(GDALfile,'MType=9');  // espg:4326:
    end;

    CloseFile(GDALfile);
    MessageShow('Working\SourceTiles\VFRmap\VFRmap.umd generated');

  end else begin
    MessageShow('Need Header file first');
    Beep;
  end;
end;

{-------------------------------------------------------------------------------------
Reference: https://www.gdal.org/gdaldem.html
Steps to create a map with elevation contour lines
1) use a DEM elevation file to create a contour shape file
2) draw this shape file onto a map
Geo information can be extracted from DEM TIFF to apply to a bitmap, or
can be applied as UTM coordinates
-------------------------------------------------------------------------------------}
//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_ContourClick(Sender: TObject);
var
  FileName : string;
  FilePath : string;
  GDALfile : TextFile;
  Tile_B_Lat : double;
  Tile_T_Lat : double;
  Tile_L_Long : double;
  Tile_R_Long : double;

begin
  // need landscape header and tile extent
  if (HeaderOpen) AND (TileOpen) then begin
    // need a background map to add contour to
    // BaseMap.bmp

    FilePath := Working_Folder+'\SourceTiles\Contour';
    if (NOT DirectoryExists(FilePath)) then begin
      ForceDirectories(FilePath);
    end;

    // generate contour from DEM TIFF
    FileName := 'Generate_Contour.bat';
    AssignFile(GDALfile, FilePath +'\'+ FileName);
    Rewrite(GDALfile);

    writeln(GDALfile,'echo off');
    writeln(GDALfile,'setlocal');
    writeln(GDALfile,'set PATH=%PATH%;"'+Library_Folder+'"');

    writeln(GDALfile,'rem generate contour files - 100 metres');
    writeln(GDALfile,'set l_DEM=../../DEM/UTM_Cropped_90m.tif');
    writeln(GDALfile,'if NOT exist %l_DEM% (echo %l_DEM% missing & pause & exit)');
    writeln(GDALfile,'set l_SHP=Contour.shp');
    writeln(GDALfile,'gdal_contour -i 100.0 %l_DEM% %l_SHP%');

    writeln(GDALfile,'endlocal');
    // close the file
    CloseFile(GDALfile);

    // contour from DEM TIFF
    // geo info from UTM coords
    // UTM bitmap as background
    //open the file
    FileName := 'Make_Contour_FromBitmap.bat';
    AssignFile(GDALfile, FilePath +'\'+ FileName);
    Rewrite(GDALfile);

    writeln(GDALfile,'echo off');
    writeln(GDALfile,'setlocal');
    writeln(GDALfile,'set PATH=%PATH%;"'+Library_Folder+'"');
    writeln(GDALfile,'set GDAL_DATA='+Library_Folder+'\..\share\epsg_csv');
    // suppres generation of .xml file
    writeln(GDALfile,'set GDAL_PAM_ENABLED=NO');

    writeln(GDALfile,'set l_bitmap=UTM_Basemap.bmp');
    writeln(GDALfile,'if NOT exist %l_bitmap% (echo %l_bitmap% missing & pause & exit)');

//    writeln(GDALfile,'rem generate contour files - 100 metres');
//    writeln(GDALfile,'set l_DEM=../../DEM/UTM_Cropped_90m.tif');
//    writeln(GDALfile,'set l_SHP=Contour.shp');
//    writeln(GDALfile,'gdal_contour -i 100.0 %l_DEM% %l_SHP%');
    writeln(GDALfile,'call Generate_Contour.bat');

    Tile_B_Lat  := CornerList[0].TileUTMBottom + UTM_Bottom - Legacy_Offset;
//    Tile_T_Lat  := Tile_B_Lat + 23040 * TileRowCount;
    Tile_T_Lat  := Tile_B_Lat + Resolution * RowCount;
    Tile_L_Long  := UTM_Right + Legacy_Offset - CornerList[1].TileUTMRight;
//    Tile_R_Long  := Tile_L_Long + 23040 * TileColumnCount;
    Tile_R_Long  := Tile_L_Long + Resolution * ColumnCount;

    writeln(GDALfile,'rem convert bitmap to tiff and add geo data');
    writeln(GDALfile,format('set left=%1.1f',[Tile_L_Long]));
    writeln(GDALfile,format('set top=%1.1f',[Tile_T_Lat]));
    writeln(GDALfile,format('set right=%1.1f',[Tile_R_Long]));
    writeln(GDALfile,format('set bottom=%1.1f',[Tile_B_Lat]));
//    writeln(GDALfile,'set l_bitmap=UTM_Basemap.bmp');
    writeln(GDALfile,'set l_geotif=UTM_Basemap_geo.tif');
    writeln(GDALfile,'set l_SHP=Contour.shp');
    writeln(GDALfile,'gdal_translate -of GTIFF -a_ullr %left% %top% %right% %bottom% %l_bitmap% %l_geotif%');

    writeln(GDALfile,'rem add contour to geotiff file in black RGB (0,0,0)');
    writeln(GDALfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn 0 -burn 0 -burn 0 -l contour %l_SHP% %l_geotif%');

    writeln(GDALfile,'rem convert back to bitmap');
    writeln(GDALfile,'set l_bitmap=Contour.bmp');
    writeln(GDALfile,'gdal_translate -of BMP %l_geotif% %l_bitmap%');
    writeln(GDALfile,'del %l_geotif%');

    writeln(GDALfile,'endlocal');
    // close the file
    CloseFile(GDALfile);

{
    // contour from DEM TIFF
    // geo info from DEM TIFF - only OK if bitmap same resolution as DEM, i.e. both 90 m or both 30m
    // UTM bitmap as background
    //open the file
    FileName := 'From_TIFF.bat';
    AssignFile(GDALfile, FilePath +'\'+ FileName);
    Rewrite(GDALfile);

    writeln(GDALfile,'echo off');
    writeln(GDALfile,'setlocal');
    writeln(GDALfile,'set PATH=%PATH%;"'+Library_Folder+'"');

    writeln(GDALfile,'rem generate contour files - 100 metres');
    writeln(GDALfile,'set l_DEM=UTM_Cropped_DEM.tif');
    writeln(GDALfile,'set l_SHP=U_T_Folder');
    writeln(GDALfile,'gdal_contour -i 100.0 %l_DEM% %l_SHP%');

    writeln(GDALfile,'rem extract geo information');
    writeln(GDALfile,'set l_meta=U_T_meta.txt');
    writeln(GDALfile,'listgeo %l_DEM% > %l_meta%');

    writeln(GDALfile,'rem convert bitmap to tiff and add geo data');
    writeln(GDALfile,'set l_bitmap=UTM_Basemap.bmp');
    writeln(GDALfile,'set l_tif=UTM_Basemap.tif');
    writeln(GDALfile,'set l_geotif=UTM_Basemap_geo.tif');
    writeln(GDALfile,'gdal_translate -of GTIFF %l_bitmap% %l_tif%');
    writeln(GDALfile,'geotifcp -g %l_meta% %l_tif% %l_geotif%');
    writeln(GDALfile,'del %l_tif%');

    writeln(GDALfile,'rem add contour to geotiff file in black RGB (0,0,0)');
    writeln(GDALfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn 0 -burn 0 -burn 0 -l contour %l_SHP% %l_geotif%');

    writeln(GDALfile,'rem convert back to bitmap');
    writeln(GDALfile,'set l_bitmap=U_T_Contour.bmp');
    writeln(GDALfile,'gdal_translate -of BMP %l_geotif% %l_bitmap%');
    writeln(GDALfile,'del %l_geotif%');

    writeln(GDALfile,'endlocal');
    // close the file
    CloseFile(GDALfile);

    // contour from DEM - OK if you have .dem file
    // geo info from UTM coords
    // UTM bitmap as background
    //open the file
    FileName := 'From_DEM.bat';
    AssignFile(GDALfile, FilePath +'\'+ FileName);
    Rewrite(GDALfile);

    writeln(GDALfile,'echo off');
    writeln(GDALfile,'setlocal');
    writeln(GDALfile,'set PATH=%PATH%;"'+Library_Folder+'"');

    writeln(GDALfile,'rem generate contour files - 100 metres');
    writeln(GDALfile,'set l_DEM=UTM_Cropped_DEM.dem');
    writeln(GDALfile,'set l_SHP=U_D_Folder');
    writeln(GDALfile,'gdal_contour -i 100.0 %l_DEM% %l_SHP%');

    Tile_B_Lat  := CornerList[0].TileUTMBottom + UTM_Bottom - Legacy_Offset;
    Tile_T_Lat  := Tile_B_Lat + Resolution * RowCount;
    Tile_L_Long  := UTM_Right + Legacy_Offset - CornerList[1].TileUTMRight;
    Tile_R_Long  := Tile_L_Long + Resolution * ColumnCount;

    writeln(GDALfile,'rem convert bitmap to tiff and add geo data');
    writeln(GDALfile,format('set left=%1.1f',[Tile_L_Long]));
    writeln(GDALfile,format('set top=%1.1f',[Tile_T_Lat]));
    writeln(GDALfile,format('set right=%1.1f',[Tile_R_Long]));
    writeln(GDALfile,format('set bottom=%1.1f',[Tile_B_Lat]));
    writeln(GDALfile,'set l_bitmap=UTM_Basemap.bmp');
    writeln(GDALfile,'set l_geotif=UTM_Basemap_geo.tif');
    writeln(GDALfile,'gdal_translate -of GTIFF -a_ullr %left% %top% %right% %bottom% %l_bitmap% %l_geotif%');

    writeln(GDALfile,'rem add contour to geotiff file in black RGB (0,0,0)');
    writeln(GDALfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn 0 -burn 0 -burn 0 -l contour %l_SHP% %l_geotif%');

    writeln(GDALfile,'rem convert back to bitmap');
    writeln(GDALfile,'set l_bitmap=U_D_Contour.bmp');
    writeln(GDALfile,'gdal_translate -of BMP %l_geotif% %l_bitmap%');
    writeln(GDALfile,'del %l_geotif%');

    writeln(GDALfile,'endlocal');
    // close the file
    CloseFile(GDALfile);
}
    // batch file to generate a color-relief map to add contour to
    FileName := 'Make_ColourRelief.bat';
    AssignFile(GDALfile, FilePath +'\'+ FileName);
    Rewrite(GDALfile);

    writeln(GDALfile,'echo off');
    writeln(GDALfile,'setlocal');
    writeln(GDALfile,'set PATH=%PATH%;"'+Library_Folder+'"');
    writeln(GDALfile,'set GDAL_DATA='+Library_Folder+'\..\share\epsg_csv');
    // suppres generation of .xml file
    writeln(GDALfile,'set GDAL_PAM_ENABLED=NO');

    writeln(GDALfile,'rem generate color relief using color table versus elevation');
    writeln(GDALfile,'set l_DEM=../../DEM/UTM_Cropped_90m.tif');
    writeln(GDALfile,'if NOT exist %l_DEM% (echo %l_DEM% missing & pause & exit)');
    writeln(GDALfile,'set l_geotif=ColorRelief.tif');
    writeln(GDALfile,'gdaldem color-relief %l_DEM% Color_Table.txt %l_geotif%');

    writeln(GDALfile,'rem generate contour');
    writeln(GDALfile,'call Generate_Contour.bat');

    writeln(GDALfile,'set l_SHP=Contour.shp');
    writeln(GDALfile,'rem add contour to geotiff file in black RGB (0,0,0)');
    writeln(GDALfile,'gdal_rasterize -b 1 -b 2 -b 3 -burn 0 -burn 0 -burn 0 -l contour %l_SHP% %l_geotif%');

    writeln(GDALfile,'rem convert back to bitmap');
    writeln(GDALfile,'set l_bitmap=Contour_Relief.bmp');
    writeln(GDALfile,'gdal_translate -of BMP %l_geotif% %l_bitmap%');
    writeln(GDALfile,'del %l_geotif%');
    writeln(GDALfile,'rem move %l_bitmap% ../../../');
    writeln(GDALfile,'endlocal');
    // close the file
    CloseFile(GDALfile);

    MessageShow('Working\SourceTiles\Contour/Make_ColourRelief.bat generated');
    MessageShow('Adjust Color_Table.txt elevations and colors as desired');

    // if not already created,
    // make an elevation color table for use with color-relief generation
    FileName := 'Color_Table.txt';
    if (NOT FileExists(FilePath +'\'+ FileName)) then begin
      AssignFile(GDALfile, FilePath +'\'+ FileName);
      Rewrite(GDALfile);

      writeln(GDALfile,'rem elevation (m)  Red Green Blue (0..255 values)');
      writeln(GDALfile,'   0   0   0   0');  // black
      writeln(GDALfile,' 100  10  92  32');
      writeln(GDALfile,' 400 127 127   0');
      writeln(GDALfile,' 750 255 255 255');  // white
    // from 3DEM - option
//      writeln(GDALfile,'   0 139 146 112');
//      writeln(GDALfile,' 100 196 186 129');
//      writeln(GDALfile,' 200 202 180 121');
//      writeln(GDALfile,' 400 183 150 101');
//      writeln(GDALfile,' 750 167 157 151');

      // close the file
      CloseFile(GDALfile);
    end;

    // batch file to generate a hill-shade map to add contour to
    FileName := 'Make_HillShade.bat';
    AssignFile(GDALfile, FilePath +'\'+ FileName);
    Rewrite(GDALfile);

    writeln(GDALfile,'echo off');
    writeln(GDALfile,'setlocal');
    writeln(GDALfile,'set PATH=%PATH%;"'+Library_Folder+'"');
    writeln(GDALfile,'set GDAL_DATA='+Library_Folder+'\..\share\epsg_csv');
    // suppres generation of .xml file
    writeln(GDALfile,'set GDAL_PAM_ENABLED=NO');

    writeln(GDALfile,'rem generate hillshade from 225 degrees');
    writeln(GDALfile,'set l_DEM=../../DEM/UTM_Cropped_90m.tif');
    writeln(GDALfile,'if NOT exist %l_DEM% (echo %l_DEM% missing & pause & exit)');
    writeln(GDALfile,'set l_geotif=HillShade.tif');
    writeln(GDALfile,'gdaldem hillshade -az 225 %l_DEM% %l_geotif%');

    writeln(GDALfile,'rem generate contour');
    writeln(GDALfile,'call Generate_Contour.bat');

    writeln(GDALfile,'set l_SHP=Contour.shp');
    writeln(GDALfile,'rem add contour to geotiff file in black RGB (0,0,0)');
    writeln(GDALfile,'gdal_rasterize -burn 0 -l contour %l_SHP% %l_geotif%');

    writeln(GDALfile,'rem convert back to bitmap');
    writeln(GDALfile,'set l_bitmap=Contour_HillShade.bmp');
    writeln(GDALfile,'gdal_translate -of BMP %l_geotif% %l_bitmap%');
    writeln(GDALfile,'del %l_geotif%');
    writeln(GDALfile,'rem move %l_bitmap% ../../../');
    writeln(GDALfile,'endlocal');
    // close the file
    CloseFile(GDALfile);

  end else begin
    MessageShow('Need Header file first');
    Beep;
  end;
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_QuarterTileClick(Sender: TObject);
var
  ErrorCode : integer;
  TileName : string;
  qColumn, qRow : integer;
  column, row{, TileIndex} : integer;
  offset_Column, offset_Row : integer;

begin
  // need tile list
  if NOT ((HeaderOpen) AND (TileOpen)) then begin
    MessageShow('Need Header file first');
    Beep;
  end else begin
    // get desired tile
    TileName := Edit_QuarterTile.Text;
    if (NOT GetTileIndex(TileName,qColumn, qRow)) then begin
      Memo_Message.Lines.Add('Select a tile name first');
      Beep;
      Exit;
    end;

{    if (length(TileName) <> 4) then begin
       Memo_Message.Lines.Add('Select a tile name first');
       Beep;
       Exit;
    end else begin // tiles
      Val(copy(TileName,1,2),qColumn,ErrorCode);
      Val(copy(TileName,3,2),qRow,ErrorCode);
      // if errorcode or not in range -> error
    end;
}
    // find main tile and offset
    column        := qColumn div 4;
    offset_Column := qColumn mod 4;
    row           := qRow div 4;
    offset_Row    := qRow mod 4;

    // need to calculate the tile corners
    u_QuarterTile.CalcCorners(row, column, offset_Row, offset_Column);

    // prepare for KML
    //need a folder
    u_MakeKML.KMLfolder := Working_Folder;
    u_MakeKML.Memo_Message := Memo_Message;

    // now make KML - needs corners - global vars
    MakeKML_QuarterTile(row, column, offset_Row, offset_Column);

    MessageShow('KML file done.');

    // prepare for GMID & GDAL
    // need a folder
    u_MakeGMID.GMIDfolder := Working_Folder;
    u_MakeGMID.Memo_Message := Memo_Message;
    u_MakeGMID.ZoomLevel := ZoomLevel;                  // need extra box to select instead ???
    u_MakeGDAL.ZoomLevel := ZoomLevel;                  // need extra box to select instead ???
//    u_MakeGDAL.OutputTileSize := OutputTileSize;        // need extra box to select instead ???
    u_MakeGDAL.OutputTileSize := IntToStr(StrToInt(OutputTileSize) div 4);  // need extra box to select instead ???

    // now make GMID
    MakeGMIDquarterTile(false, row, column, offset_Row, offset_Column,
      Tile_LT_Lat_save, Tile_LT_Long_save, Tile_RB_Lat_save, Tile_RB_Long_save);
    MakeGMIDquarterTile(true, row, column, offset_Row, offset_Column,
      Tile_LT_Lat_save, Tile_LT_Long_save, Tile_RB_Lat_save, Tile_RB_Long_save);  // geid version as well

    // now prepare for GDAL - do after MakeKML ! - nneeds global vars
    // need lat, long, and size of source, and extent values, not true values
    SourceLeftLongitude  := Tile_LT_Long;
    SourceTopLatitude    := Tile_LT_Lat;
    SourceRightLongitude := Tile_RB_Long;
    SourceBottomLatitude := Tile_RB_Lat;
    // to be manually entered based on screen capture
    BitmapWidth  := 0;
    BitmapHeight := 0;

    //need a folder and file names
    u_MakeGDAL.Memo_Message := Memo_Message;
    u_MakeGDAL.GDALfolder := Working_Folder;
    u_MakeGDAL.GDALlibraryfolder := Library_Folder;
//    u_MakeGDAL.File_Destination := '..\..\Terragen\Textures';
    // to be manually entered based on screen capture
//    u_MakeGDAL.OutputTileSize := '0';

    // now make manual screen capture GDAL - needs source lat/long extents
    MakeGDALquarterTile_MC(row, column, offset_Row, offset_Column);
//    MessageShow('Need to manually set source image size in GDAL');
//    MessageShow('Need to manually set final image size in GDAL');

    // also make standard GDAL
    MakeAutoGDALquarterTile(4326, row, column, offset_Row, offset_Column); // for geid
    MakeAutoGDALquarterTile(3857, row, column, offset_Row, offset_Column); // for gmid

    // now make DDS
    u_MakeDDS.DDSfolder := Working_Folder;
    u_MakeDDS.Memo_Message := Memo_Message;
    u_MakeDDS.CompressorFolder := Compressor_Folder;
    // u_MakeGDAL.DXT_Type := DXT_Type;
    u_MakeDDS.DXT_Type := DXT_Type;
    MakeDDSquarterTile(row, column, offset_Row, offset_Column);
    // MessageShow('Need to manually set nmips=1+log2(ImageSize)');

    // now make a Batch ALL
    u_QuarterTile.QT_folder := Working_Folder;
    u_QuarterTile.Memo_Message := Memo_Message;
    Make_QT_All_BatchFile(3857, row, column, offset_Row, offset_Column);
    Make_QT_All_BatchFile(4326, row, column, offset_Row, offset_Column); // geid as well
  end;
end;

//-------------------------------------------------------------------------------------
{Procedure Test_int64;
var
  Alpha  : int64;
  Bravo  : int64;
  Charlie : int64;
  Delta   : Integer;
  aIndex : Integer;
  i : Integer;
  s : string;
begin
  Alpha := $500F000000F00000;
  aIndex := 0;
  for i := 0 to 16-1 do begin
    Bravo := Alpha SHR (aIndex * 4);
    Charlie := (Alpha SHR (aIndex * 4)) AND $000000000000000F;
    if ((Alpha SHR (aIndex * 4)) AND $000000000000000F) = $0F then begin
      Delta := 1;
    end else begin
      Delta := 0;
    end;
    INC(aIndex);
    s := format('%8x %8x %8x %d',[Alpha, Bravo, Charlie, Delta]);
    MessageShow(s);
  end;
end;
}
//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_DXT3_5_to_DXT1Click(Sender: TObject);
var
  Filename : String;
  i : integer;

begin
{  Test_int64;
}
  OpenDialog1.Options := [ofAllowMultiSelect, ofFileMustExist];
  OpenDialog1.InitialDir := Initial_Folder;
  OpenDialog1.Filter := 'DDS files (*.DDS)|*.DDS|All files (*.*)|*.*';
  OpenDialog1.FileName := '';
  if OpenDialog1.Execute then begin
    with OpenDialog1.Files do begin
      for i := 0 to Count - 1 do begin
        ProgressBar_Status.Max := Count;
        FileName := Strings[i];
        // convert
        u_DXT.Memo_Message := Memo_Message;
//        u_DXT.ProgressBar_Status := ProgressBar_Status;
        u_DXT.dxt_Path := ExtractFileDir(FileName);
        u_DXT.dxt_FileName := ExtractFileName(FileName);
        Convert_DXT3_5_to_DXT1;
        ProgressBar_Status.StepIt;
        Application.ProcessMessages;
      end;
    end;
    ProgressBar_Status.Position := 0;
  end;
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_FixPuddlesClick(Sender: TObject);
var
  Filename : String;
  i : integer;

begin
  OpenDialog1.Options := [ofAllowMultiSelect, ofFileMustExist];
  OpenDialog1.InitialDir := Initial_Folder;
  OpenDialog1.Filter := 'DDS files (*.DDS)|*.DDS|All files (*.*)|*.*';
  OpenDialog1.FileName := '';
  if OpenDialog1.Execute then begin
    Screen.Cursor := crHourGlass;  // Let user know we're busy...
    with OpenDialog1.Files do begin
      for i := 0 to Count - 1 do begin
        ProgressBar_Status.Max := Count;
        FileName := Strings[i];
        // convert
        u_DXT.Memo_Message := Memo_Message;
//        u_DXT.ProgressBar_Status := ProgressBar_Status;
        u_DXT.dxt_Path := ExtractFileDir(FileName);
        u_DXT.dxt_FileName := ExtractFileName(FileName);
        FixPuddles_DXT1; // for testing
        ProgressBar_Status.StepIt;
        Application.ProcessMessages;
      end;
    end;
    Screen.Cursor := crDefault;  // no longer busy
    ProgressBar_Status.Position := 0;
  end;
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_OBJ_Import_CSVClick(Sender: TObject);
begin
  // need landscape header and tile extent
  if (HeaderOpen) AND (TileOpen) then begin
    Object_FolderName := Initial_Folder;
    Object_FileName := LandscapeName+'.obj';
    if (FileExists(Object_FolderName+'\Working\'+Object_FileName+'.csv')) then begin
      ImportCSV_ObjectFile;
      MessageShow('File imported from Working folder');
    end else begin
      MessageShow('File '+'Working\'+Object_FileName+'.csv'+' not found');
      Beep;
    end;
  end else begin
    MessageShow('Need Header file first');
    Beep;
  end;
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_OBJ_Export_CSVClick(Sender: TObject);
begin
  // need landscape header and tile extent
  if (HeaderOpen) AND (TileOpen) then begin
    Object_FolderName := Initial_Folder;
    Object_FileName := LandscapeName+'.obj';
    if (FileExists(Object_FolderName+'\'+Object_FileName)) then begin
      ReadObjectFile;
      ExportCSV_ObjectFile;
      MessageShow('File exported to Working folder');
    end else begin
      MessageShow('File '+Object_FileName+' not found');
      Beep;
    end;
  end else begin
    MessageShow('Need Header file first');
    Beep;
  end;
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_OBJ_LL_Export_CSVClick(Sender: TObject);
begin
  // need landscape header and tile extent
  if (HeaderOpen) AND (TileOpen) then begin
    Object_FolderName := Initial_Folder;
    Object_FileName := LandscapeName+'.obj';
    if (FileExists(Object_FolderName+'\'+Object_FileName)) then begin
      // use Condor NAVICON DLL for conversion
//      u_Condor_NaviconDLL.CondorFolder := Condor_Folder;
//      if (NOT Condor_Navicon_Open (Condor_Folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.trn') ) then begin
//        MessageShow('Unable to open navicon.dll');
//        Beep; Exit;
//      end;
      ReadObjectFile;
      ExportCSV_LL_ObjectFile;
//      Condor_Navicon_Close;
      MessageShow('File exported to Working folder');

List_OBJ_File_Object_Details(Initial_Folder,LandscapeName);

    end else begin
      MessageShow('File '+Object_FileName+' not found');
      Beep;
    end;
  end else begin
    MessageShow('Need Header file first');
    Beep;
  end;
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_OBJ_LL_Import_CSVClick(Sender: TObject);
begin
  // need landscape header and tile extent
  if (HeaderOpen) AND (TileOpen) then begin
    Object_FolderName := Initial_Folder;
    Object_FileName := LandscapeName+'.obj';
    if (FileExists(Object_FolderName+'\Working\'+Object_FileName+'.LL.csv')) then begin
//      u_Condor_NaviconDLL.CondorFolder := Condor_Folder;
//      if (NOT Condor_Navicon_Open (LandscapeName) ) then begin
//        MessageShow('Unable to open navicon.dll');
//        Beep; Exit;
//      end;
      ImportCSV_LL_ObjectFile;
//      Condor_Navicon_Close;
      MessageShow('File imported from Working folder');
    end else begin
      MessageShow('File '+'Working\'+Object_FileName+'_LL.csv'+' not found');
      Beep;
    end;
  end else begin
    MessageShow('Need Header file first');
    Beep;
  end;
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_APT_Import_CSVClick(Sender: TObject);
begin
  // need landscape header and tile extent
  if (HeaderOpen) AND (TileOpen) then begin
    Airport_FolderName := Initial_Folder;
    Airport_FileName := LandscapeName+'.apt';
    if (FileExists(Airport_FolderName+'\Working\'+Airport_FileName+'.csv')) then begin
      ImportCSV_AirportFile;
      MessageShow('File imported from Working folder');
      Unit_AirportPlacer.CurrentLandscape := ''; // flag for reload
    end else begin
      MessageShow('File '+'Working\'+Airport_FileName+'.csv'+' not found');
      Beep;
    end;
  end else begin
    MessageShow('Need Header file first');
    Beep;
  end;
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_APT_CSV_ExportClick(Sender: TObject);
begin
  // need landscape header and tile extent
  if (HeaderOpen) AND (TileOpen) then begin
    Airport_FolderName := Initial_Folder;
    Airport_FileName := LandscapeName+'.apt';
    if (FileExists(Airport_FolderName+'\'+Airport_FileName)) then begin
      ReadAirportFile;
      ExportCSV_AirportFile;
      MessageShow('File exported to Working folder');

List_APT_File_Object_Details(Initial_Folder,LandscapeName);

    end else begin
      MessageShow('File '+Airport_FileName+' not found');
      Beep;
    end;
  end else begin
    MessageShow('Need Header file first');
    Beep;
  end;
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_AddAlphaClick(Sender: TObject);
var
  FullFilename : String;
  File_Folder : String;
  Alpha_File_Name : String;
  File_Name : String;
  File_Name_NoExt : String;
  i : integer;

begin
  OpenDialog1.Options := [ofAllowMultiSelect, ofFileMustExist];
  OpenDialog1.InitialDir := Initial_Folder;
  OpenDialog1.Filter := 'BMP files (*.BMP)|*.BMP|All files (*.*)|*.*';
  OpenDialog1.FileName := '';
  if OpenDialog1.Execute then begin
    with OpenDialog1.Files do begin
      for i := 0 to Count - 1 do begin
        ProgressBar_Status.Max := Count;
        FullFileName := Strings[i];
        File_Folder := ExtractFileDir(FullFileName);
        File_Name := ExtractFileName(FullFileName);
        File_Name_NoExt := copy(File_Name,1,pos('.bmp',File_Name)-1);

        if (CheckBox_Water.checked) then begin
          Alpha_File_Name := File_Folder+'\..\WaterMaps\'+File_Name;
        end else begin
          Alpha_File_Name := File_Folder+'\'+File_Name_NoExt+'_Alpha.bmp';
        end;

// compressonator uses pre-multiplied alpha with TIF - problem for water-tiles
// nvDXT uses pre-multiplied alpha with TIF - problem for water-tiles
        u_TIFF.Memo_Message := Memo_Message;
        u_TIFF.ProgressBar_Status := ProgressBar_Status;
        BMP_24_To_TIF_32_WithAlpha(FullFileName,
//                                File_Folder+'\..\WaterMaps\'+File_Name,
                                Alpha_File_Name,
                                File_Folder+'\'+File_Name_NoExt+'.tif'
                               );

// compressonator doesn't do alpha on bitmaps - problem for water-tiles
// GDAL_translate doesn't support 32 bit bitmaps !
        u_BMP.Memo_Message := Memo_Message;
        u_BMP.ProgressBar_Status := ProgressBar_Status;
        ForceDirectories(File_Folder+'\Saved');
        RenameFile(FullFileName,File_Folder+'\Saved\'+File_Name);
//        Bitmap_24_To_Bitmap_32_Alpha(FullFileName,
//                                     File_Folder+'\..\WaterMaps\'+File_Name,
//                                     File_Folder+'\'+File_Name_NoExt+'-32.bmp'
        Bitmap_24_To_Bitmap_32_Alpha(File_Folder+'\Saved\'+File_Name,
//                                     File_Folder+'\..\WaterMaps\'+File_Name,
                                     Alpha_File_Name,
                                     FullFileName
                                    );

        ProgressBar_Status.StepIt;
        Application.ProcessMessages;
      end;
    end;
    ProgressBar_Status.Position := 0;
  end;
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_BMPmaskClick(Sender: TObject);
var
  FullFilename : String;
  File_Folder : String;
  File_Name : String;
  File_Name_NoExt : String;
  i : integer;
  MaskFileName : string;

begin
  OpenDialog1.Options := [ofAllowMultiSelect, ofFileMustExist];
  OpenDialog1.InitialDir := Initial_Folder;
  OpenDialog1.Filter := 'BMP files (*.BMP)|*.BMP|All files (*.*)|*.*';
  OpenDialog1.FileName := '';
  if OpenDialog1.Execute then begin
    with OpenDialog1.Files do begin
      for i := 0 to Count - 1 do begin
        ProgressBar_Status.Max := Count;
        FullFileName := Strings[i];
        File_Folder := ExtractFileDir(FullFileName);
        File_Name := ExtractFileName(FullFileName);
        File_Name_NoExt := copy(File_Name,1,pos('.bmp',File_Name)-1);

        u_BMP.Memo_Message := Memo_Message;
        u_BMP.ProgressBar_Status := ProgressBar_Status;

        MaskFileName := File_Folder+'\..\WaterMaps\'+File_Name;
        if (NOT FileExists(MaskFileName)) then begin
          // try 4 character for tilename number instead
          MaskFileName := File_Folder+'\..\WaterMaps\'+
            copy(File_Name_NoExt,length(File_Name_NoExt)-(4-1),
              length(File_Name_NoExt))+'.bmp';
        end;

{        Bitmap_24_To_Masked_24(FullFileName,
                                     MaskFileName,
                                     Shape_pick.Brush.Color
                                    );
}        NewBitmap_To_Masked(FullFileName,
                                     MaskFileName,
                                     Shape_pick.Brush.Color
                                    );

        ProgressBar_Status.StepIt;
        Application.ProcessMessages;
      end;
    end;
    ProgressBar_Status.Position := 0;
  end;
end;

//-------------------------------------------------------------------------------------
Procedure MakeBatchDo_All(FilePath, TileName : string; DoDDS : Boolean);
var
  Batch_File : TextFile;
  FileName : string;

begin
  //open the file
  FileName := 'Do_All_'+TileName+'.bat';
  AssignFile(Batch_File, FilePath +'\'+ FileName);
  Rewrite(Batch_File);

  writeln(Batch_File,'@echo off');
  writeln(Batch_File,'setlocal');
  writeln(Batch_File,'rem goto directory where batch file is');
  writeln(Batch_File,'cd /d %~dp0');

  writeln(Batch_File,'call Generic_Wget.bat');
  writeln(Batch_File,'call Swap_XY.bat');
//  writeln(Batch_File,'call Batch_Combine_'+TileName+'.bat');
  writeln(Batch_File,'call Batch_Combine_'+TileName+'.bat bmp meters');
  writeln(Batch_File,'call GDAL_'+TileName+'.bat');
  if (DoDDS) then begin
    writeln(Batch_File,'call DDS_'+TileName+'.bat');
  end;

  writeln(Batch_File,'endlocal');

 // close the file
  Close(Batch_File);
  MessageShow(FileName+' done.');
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_WGETClick(Sender: TObject);
//const
//  URL = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile';
//  Tile_Type = 'XYZ'; // TMS or XYZ
//  xyz_Order = 'ZYX'; // ZXY, ZYX, xxx_yyy_zz
var
//  URL : string; // 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile'
  URL : string; // 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{x}/{y}.jpg'
  Tile_Type : string;  // TMS or XYZ
  xyz_Order : string;  // ZXY, ZYX, xxx_yyy_zz
  TilePath : String;
  TileIndex : integer;
  i, j : integer;
  TMS, SwapXY : boolean;
  ErrorCode : integer;

// - - - - - - - - - - - - - - - - - - - - - -
  Procedure DoOneTile;
  begin
    TileIndex := i*(TileColumnCount+1)+j;
    TileName := TileList[TileIndex].TileName;
    TilePath := Working_Folder+'\SourceTiles\'+TileName;
    ForceDirectories(TilePath+'\'+TileName);
    WGET_Generic(i, j, i+1, j+1, strtoint(ZoomLevel), TMS, SwapXY, URL,
      TileName, TilePath);
//    MakeBatchCombineFile(TilePath, TileName, '.umd');
    Make_Batch_DownloadCombine(td_C, TileName, '0', 'umd',
                               TilePath, 'Batch_Combine_'+TileName+'.bat',
                               '0',
                               0,0,0,0
                              );
    MakeBatchDo_All(TilePath, TileName, true);
  end;

// - - - - - - - - - - - - - - - - - - - - - -
begin
  if (NOT ((HeaderOpen) AND (TileOpen))) then begin
    MessageShow('Need Header file first');
    Beep;
    Exit;
  end;
  // alternately, look for URL file with this information
  // parse URL file - URL, type, xyz_Order
    // URL
    // Tile_Type: TMS, XYZ
    // xyz_Order: ZXY, ZYX, xxx_yyy_zz
    // epsg 4326 or 3857 ?
{  WGET_Generic(0, 0, TileRowCount-1+1, TileColumnCount-1+1, 11, False, True,
    'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile',
    'TEST', Working_Folder+'\SourceTiles\TEST');
  exit;
}
  URL := ComboBox_GenericTileDownload.Text;
  if (URL = '') then begin
    MessageShow('Need URL first');
    Beep;
    Exit;
  end;

  Tile_Type := ComboBox_TileType.Text;
  if (upperCase(Tile_Type) = 'TMS') then begin
    TMS := True;
  end else begin
    if (upperCase(Tile_Type) = 'XYZ') then begin
      TMS := False;
    end else begin
      MessageShow('Need Tile Type first');
      Beep;
      Exit;
    end;
  end;

  xyz_Order := ComboBox_NamingType.Text;
  if (upperCase(xyz_Order) = 'ZXY') then begin
    SwapXY := False;
  end else begin
    if (upperCase(xyz_Order) = 'ZYX') then begin
      SwapXY := True;
    end else begin
      MessageShow('Need Tile Naming Type first');
      Beep;
      Exit;
    end;
  end;

  // epsg 4326 or 3857 ?

  // confirm one or all ???

  if (TileName = '') then begin // default blank -> all
    // confirm make ALL tiles
    if MessageDlg('Proceed with ALL tiles ?', mtConfirmation,
      [mbYes, mbNo], 0) = mrNo then begin
      Exit;
    end;
    // confirm the zoom level
    if MessageDlg('Proceed with zoom level "' + ZoomLevel + '" ?', mtConfirmation,
      [mbYes, mbNo], 0) = mrNo then begin
      Exit;
    end;

    ProgressBar_Status.Max := TileColumnCount*TileRowCount;
    for j := 0 to TileColumnCount-1 do begin
      for i := 0 to TileRowCount-1 do begin
        DoOneTile;
        ProgressBar_Status.StepIt;
        Application.ProcessMessages;
      end;
    end;
    ProgressBar_Status.Position := 0;
    u_MakeGMID.GMIDfolder := Working_Folder;
    u_MakeGMID.Memo_Message := Memo_Message;
    // make batch file to run all batch files
    MakeWGET_All_BatchFile;
  end else begin
    if (TileName = 'Overall') then begin // only overall tile
      // confirm make ALL tiles
      if MessageDlg('Proceed with "Overall" tile ?', mtConfirmation,
        [mbYes, mbNo], 0) = mrNo then begin
        Exit;
      end;
      TilePath := Working_Folder+'\SourceTiles\'+TileName;
      ForceDirectories(TilePath+'\'+TileName);
      WGET_Generic(0, 0, TileRowCount, TileColumnCount, 10, TMS, SwapXY, URL,
        TileName, TilePath);
      Make_Batch_DownloadCombine(td_C, TileName, '0', 'umd',
                                 TilePath, 'Batch_Combine_'+TileName+'.bat',
                                 '0',
                                 0,0,0,0
                                );
      MakeBatchDo_All(TilePath, TileName, false);
    end else begin // individual tile only
      if (NOT GetTileIndex(TileName,j, i)) then begin
        MessageShow('Select a tile name first');
        Beep;
      end else begin
        // confirm make only one tile
        if MessageDlg('Proceed with tile "' + TileName + '" ?', mtConfirmation,
          [mbYes, mbNo], 0) = mrNo then begin
          Exit;
        end;
        // confirm the zoom level
        if MessageDlg('Proceed with zoom level "' + ZoomLevel + '" ?', mtConfirmation,
          [mbYes, mbNo], 0) = mrNo then begin
          Exit;
        end;
        DoOneTile;
      end;
    end;
  end;
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_ReduceClick(Sender: TObject);
var
  Filename : String;
  i, j : integer;

begin
  if (ComboBox_Reduce.Text = '') then begin  // if blank then allow multiple selection
    OpenDialog1.Options := [ofAllowMultiSelect, ofFileMustExist];
    OpenDialog1.InitialDir := Initial_Folder;
    OpenDialog1.Filter := 'DDS files (*.DDS)|*.DDS|All files (*.*)|*.*';
    OpenDialog1.FileName := '';
    if OpenDialog1.Execute then begin
      with OpenDialog1.Files do begin
        for i := 0 to Count - 1 do begin
          ProgressBar_Status.Max := Count;
          FileName := Strings[i];
          // convert
          u_DXT.Memo_Message := Memo_Message;
//          u_DXT.ProgressBar_Status := ProgressBar_Status;
          u_DXT.dxt_Path := ExtractFileDir(FileName);
          u_DXT.dxt_FileName := ExtractFileName(FileName);
          DXT_Reduce;
          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;
      end;
      ProgressBar_Status.Position := 0;
      MessageShow('Reduction done');
    end;
  end else begin
    if (ComboBox_Reduce.Text = 'All') then begin  // if 'All' then do whole folder
      if (NOT ((HeaderOpen) AND (TileOpen))) then begin
        MessageShow('Need Header file first');
        Beep;
        Exit;
      end;
  // need to confirm - cannot be undone
  // Are you sure you want to reduce ALL the textures in this landscape ?
      if (MessageDlg('Are you sure you want to reduce ALL the textures in this landscape ?', mtConfirmation,
          [mbYes, mbNo], 0) = mrYes) then begin
        ProgressBar_Status.Max := (RowCount div 64)*(ColumnCount div 64);
        for i := 0 to (RowCount div 64)-1 do begin
          for j := 0 to (ColumnCount div 64)-1 do begin
            u_DXT.dxt_Path := Working_Folder+'\..\Textures';
//            u_DXT.dxt_FileName := format('t%2.2d%2.2d.dds',[j,i]);
            u_DXT.dxt_FileName := format('t%s.dds',[MakeTileName(j,i, TileNameMode)]);
            DXT_Reduce;
            MessageShow(u_DXT.dxt_FileName);
            ProgressBar_Status.StepIt;
            Application.ProcessMessages;
          end;
        end;
        ProgressBar_Status.Position := 0;
        MessageShow('Reduction done');
      end;
    end else begin
      if (ComboBox_Reduce.Text = 'Edge') then begin  // if edge, then only do the outside quarter tile
        if (NOT ((HeaderOpen) AND (TileOpen))) then begin
          MessageShow('Need Header file first');
          Beep;
          Exit;
        end;
       // need to confirm - cannot be undone
       // Are you sure you want to reduce the outer 1/4 textures in this landscape ?
       if (MessageDlg('Are you sure you want to reduce the outer 1/4 textures in this landscape ?', mtConfirmation,
          [mbYes, mbNo], 0) = mrYes) then begin
         ProgressBar_Status.Max := (RowCount div 64)*(ColumnCount div 64);
          for i := 0 to (RowCount div 64)-1 do begin
            for j := 0 to (ColumnCount div 64)-1 do begin
              if ( (i = 0) OR
                   (i = (RowCount div 64)-1) OR
                   (j = 0) OR
                   (i = (ColumnCount div 64)-1) ) then begin
                u_DXT.dxt_Path := Working_Folder+'\..\Textures';
//                u_DXT.dxt_FileName := format('t%2.2d%2.2d.dds',[j,i]);
                u_DXT.dxt_FileName := format('t%s.dds',[MakeTileName(j,i, TileNameMode)]);
                DXT_Reduce;
                MessageShow(u_DXT.dxt_FileName);
              end;
              ProgressBar_Status.StepIt;
              Application.ProcessMessages;
            end;
          end;
          ProgressBar_Status.Position := 0;
          MessageShow('Reduction done');
        end;
      end else begin
        MessageShow('Unknown option');
        Beep;
        Exit;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure Dummy_V2_Forests(FOR_FilePath : string);
const
  st_Size = 512;
var
  st_X, st_Y :integer;
  st_ColumnCount, st_RowCount :integer;
  {X,} Y :integer;
//  st_WidthMax :integer;
//  st_HeightMax :integer;
  FOR_File : File of Byte;
  FileName : string;
  P : PByteArray;

begin
    // do by patches not tiles
    st_ColumnCount := TerrainHeader.tWidth div pColumns;
    st_RowCount := TerrainHeader.tHeight div pRows;

    P := AllocMem(st_Size); // allocate memory

    ProgressBar_Status.Max := st_ColumnCount*st_RowCount;
    for st_X := 0 to st_ColumnCount-1 do begin
      for st_Y := 0 to st_RowCount-1 do begin
//        FileName := FOR_FilePath+'\'+format('%2.2d%2.2d.for',[st_X,st_Y]);
        FileName := FOR_FilePath+'\'+format('%s.for',[MakeTileName(st_X,st_Y, TileNameMode)]);
        AssignFile(FOR_File,FileName);
        Rewrite(FOR_File);
        begin
          for Y := 0 to st_Size-1 do begin
            BlockWrite(FOR_File,P^,st_Size);
          end;
        end;
        Close(FOR_File);
        ProgressBar_Status.StepIt;
        Application.ProcessMessages;
      end;
    end;
    ProgressBar_Status.Position := 0;
    freemem(P);
//    MessageShow('Dummy Forests files created');
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_Convert_V1_V2Click(Sender: TObject);
var
  NewLandscapeName : string;
  NewLandscapePath : string;
  V1_LandscapePath : string;
  i{, j} :integer;
//  TileName : string;
  SearchRec: TSearchRec;
  Condor_Ini: TIniFile;
//  INI_File : TextFile;
  CurrentPath : string;
  X_ObjectFileName : string;
  c3d_ObjectFileName : string;

  Dummy_File : file;

//-------------------------------------------------------------------------------------
procedure CopyObjectTextures;
var
  NodeIndex : integer;
  Tn : string;
  Index : integer;
  Folder : string;
  Remainder : string;

//-------------------------------------------------------------------------------------
function GetFirstFolder(Tn : string) : integer;
begin
  Index := pos('\',Tn);
  if (Index <> 0) then begin
    Folder := copy(Tn,1,Index-1);
    Remainder := copy(Tn,Index+1,length(TN));
  end;
  result := Index;
end;

//-------------------------------------------------------------------------------------
begin
  // walk the object tree and fix file paths and copy textures
  NodeIndex := 0;
  repeat
    Tn := FindNextTexture(NodeIndex);
    if (Tn <> '') then begin
//      MessageShow(Tn);
      // in V1, paths are condor relative only (? it seems)
      // if path starts with 'World', in V1
      // need to copy textures from special folder World\V1
      Tn := StringReplace(Tn,'/','\',[rfReplaceAll]);   //could be / or \
      if (GetFirstFolder(Tn) = 0) then begin
        MessageShow('? '+Tn);
        continue; // error ?
      end else begin
        if (UpperCase(Folder) = 'WORLD') then begin
          Tn := 'World\Objects\V1\'+Remainder;
          ForceDirectories(NewLandscapePath+'\'+ExtractFilePath(Tn));
          // copy V1 original file from Condor_V1 folder
          CopyFile(pchar(ApplicationPath+'\Condor_V1\World\'+Remainder),
            pchar(NewLandscapePath+'\'+Tn),false);
          // condor V2 does not try condor relative world folder like Condor V1
          Tn := 'Landscapes\'+NewLandscapeName+'\'+tn;
        end else begin
          if (UpperCase(Folder) = 'LANDSCAPES') then begin
            if (GetFirstFolder(Remainder) = 0) then begin
              MessageShow('? '+Tn);
              continue; // error ?
            end else begin
              // Folder name should be landscape name, but could be wrong
              // don't check it, just skip it
// TBD             Tn := calculate relative path ;  // object relative path
              Tn := 'Landscapes\'+NewLandscapeName+'\'+Remainder; // Condor relative path
              ForceDirectories(NewLandscapePath+'\'+ExtractFilePath(Remainder));
              // copy file
              CopyFile(pchar(V1_LandscapePath+'\'+Remainder),
                pchar(NewLandscapePath+'\'+Remainder),false);
            end;
          end else begin
            // relative to current object
            ForceDirectories(NewLandscapePath+'\'+CurrentPath+'\'+ExtractFilePath(Tn));
            // copy file
            CopyFile(pchar(V1_LandscapePath+'\'+CurrentPath+'\'+Tn),
              pchar(NewLandscapePath+'\'+CurrentPath+'\'+Tn),false);
          end;
        end;
        // update the name in the file
        UpdateTextureFileName(NodeIndex, Tn);
      end;
      // look for the next one
      INC(NodeIndex);
    end;
  until (Tn = '');
end;

//-------------------------------------------------------------------------------------
procedure CopyAndConvert;
var
  i : integer;
//  FilePath : string;
//  FileName : string;
  FileExt : string;
  New_coName : string;
begin
  ProgressBar_Status.Max := Object_Count;
  for i := 0 to Object_Count-1 do begin
    with Object_List[i] do begin
//      FilePath := ExtractFilePath(coName);
//      FileName := ExtractFileName(coName);
      FileExt := ExtractFileExt(coName);
      New_coName := copy(coName,1,length(coName)-length(FileExt)) +'.c3d';
      // objects are in Condor\landscape\World\Objects
      //   search first
      //   could be in sub-folders if so named
      // or in Condor\World\Objects, so if not found search there
      //  if found need to copy object in world\V1\ and add V1 to object name
      // check Condor\landscape\World\Objects first
      X_ObjectFileName := V1_LandscapePath+'\World\Objects\'+coName;
      if (FileExists(X_ObjectFileName)) then begin
        ForceDirectories(NewLandscapePath+'\World\Objects\'+ExtractFilePath(New_coName));
        c3d_ObjectFileName := NewLandscapePath+'\World\Objects\'+New_coName;
        CurrentPath := 'World\Objects';
      end else begin
        // check Condor\World\Objects next
        X_ObjectFileName := ApplicationPath+'\Condor_V1\World\Objects\'+coName;
        if (FileExists(X_ObjectFileName)) then begin
          New_coName := 'V1\'+New_coName;
          ForceDirectories(NewLandscapePath+'\World\Objects\'+ExtractFilePath(New_coName));
          c3d_ObjectFileName := NewLandscapePath+'\World\Objects\'+New_coName;
          CurrentPath := 'World\Objects';
        end else begin
          // NOT found
          c3d_ObjectFileName := '';
        end;
      end;
      if (c3d_ObjectFileName <> '') then begin
         // open object file and save as c3d
        ReadCondorXfile(X_ObjectFileName, false);
        // copy the textures associated with this object
        CopyObjectTextures;
        // save as c3d
        WriteCondorC3Dfile(c3d_ObjectFileName);
      end;
      // save new name
      coName := New_CoName;
    end;
    ProgressBar_Status.StepIt;
    Application.ProcessMessages;
  end;
  ProgressBar_Status.Position := 0;
end;
//-------------------------------------------------------------------------------------
begin
  // need a new name
  // .trn - modify for N or S only.
  // tr3 - expand x3 from terrain
  // textures - rotate 180 degrees
  // forest - expand x4 from forest map
  // World - change all X, CX objects to c3d objects
  //  if drevesa (and tr1, tr2), avoid tree shadows by adding suffix '_ns' to mesh name
  // Airports
  // ignore S file and use G and O files
  // - change all X, CX objects to c3d objects
  // - add default windsock
  // - Create Default common World objects from Condor V1
  // use Landscape Creator to create 'hash' files for terrain and forest

  // need landscape header and tile extent
  if NOT ((HeaderOpen) AND (TileOpen)) then begin
    MessageShow('Need a V1 landscape selected and Read Header file first');
    Beep;
    Exit;
  end;

  V1_LandscapePath := ExpandFileName(Working_Folder+'\..');

//  if (DirectoryExists(Initial_Folder+'\HeightMaps') ) then begin
  if (DirectoryExists(V1_LandscapePath+'\HeightMaps') ) then begin
    MessageShow(LandscapeName + ' Not a V1 landscape');
    Beep;
    Exit;
  end;

  if MessageDlg('Are you sure you want convert "'+LandscapeName+'" scenery to V2 ?', mtConfirmation,
    [mbYes, mbNo], 0) = mrNo then begin
    Exit;
  end;

  if (NOT DirectoryExists(ApplicationPath+'\Condor_V1')) then begin
    MessageShow('Unable to access .\Condor_V1 folder');
    Beep;
    Exit;
  end;

  NewLandscapeName := LandscapeName+'-2';
  NewLandscapeName := InputBox('Enter landscape name', 'Prompt', NewLandscapeName);

  if MessageDlg('Confirm landscape name "'+NewLandscapeName+'" ?', mtConfirmation,
    [mbYes, mbNo], 0) = mrNo then begin
    Exit;
  end;

  if (DirectoryExists(Working_Folder+'\..\..\'+NewLandscapeName) ) then begin
    if MessageDlg(NewLandscapeName+' already exists - Overwrite ?', mtConfirmation,
      [mbYes, mbNo], 0) = mrNo then begin
      Exit;
    end;
  end;

  Screen.Cursor := crHourGlass;  // Let user know we're busy...

  // first create a landscape folder
  NewLandscapePath := ExpandFileName(Working_Folder+'\..\..\'+NewLandscapeName);
  ForceDirectories(NewLandscapePath);
  MessageShow('Creating '+NewLandscapePath);

  // now copy the terrain file
  MessageShow('Converting Terrain file');
  CopyFile(pchar(V1_LandscapePath+'\'+LandscapeName+'.trn'),
    pchar(NewLandscapePath+'\'+NewLandscapeName+'.trn'),false);
  // patch the terrain file for N/S used in V2 instead of A..Z
  UpdateTerrainUTMgrid_V2(NewLandscapePath+'\'+NewLandscapeName+'.trn');

  // convert the terrain and create the HeightMaps files
  ForceDirectories(NewLandscapePath+'\Working\DEM');
  u_Terrain.ProgressBar_Status := ProgressBar_Status;
  TRN_To_RAW(NewLandscapePath+'\'+NewLandscapeName+'.trn',
    NewLandscapePath+'\Working\DEM\'+NewLandscapeName+'.RAW');
  RAW_To_RAW3(NewLandscapePath+'\Working\DEM\'+NewLandscapeName+'.RAW',
    NewLandscapePath+'\Working\DEM\'+NewLandscapeName+'.3.RAW');

  //and create the HeightMaps files
  MessageShow('Creating Heightmaps');
  ForceDirectories(NewLandscapePath+'\HeightMaps');
  RAW_To_TR3(NewLandscapePath+'\Working\DEM\'+NewLandscapeName+'.3.RAW',
    NewLandscapePath+'\HeightMaps');

  // copy and rotate the texture files
  MessageShow('Converting Texture files');
  ForceDirectories(NewLandscapePath+'\Textures');
  ProgressBar_Status.Max := (ColumnCount div pColumns)*(RowCount div pRows);
  if (FindFirst(V1_LandscapePath+'\Textures\*.dds', faNormalFile, SearchRec)) = 0 then begin
    DXT_Rotate_180(V1_LandscapePath+'\Textures\'+SearchRec.Name,
      NewLandscapePath+'\Textures\'+SearchRec.Name);
    while (FindNext(SearchRec) = 0) do begin
      DXT_Rotate_180(V1_LandscapePath+'\Textures\'+SearchRec.Name,
        NewLandscapePath+'\Textures\'+SearchRec.Name);
      ProgressBar_Status.StepIt;
      Application.ProcessMessages;
    end;
    FindClose(SearchRec);
  end;
  ProgressBar_Status.Position := 0;

  // convert the forest to V2 format
  ForceDirectories(NewLandscapePath+'\ForestMaps');
  //// for now create a dummy blank forest
  //MessageShow('Create Dummy Forests files for now');
  //Dummy_V2_Forests(NewLandscapePath+'\ForestMaps');

  MessageShow('Converting Forest');
  // Read the Forest file
  u_Forest.ProgressBar_Status := ProgressBar_Status;
  SetLength(OverallForestDeciduous, ColumnCount*2*RowCount*2);
  SetLength(OverallForestConiferous,ColumnCount*2*RowCount*2);
  FOR_To_OverallForest(V1_LandscapePath+'\'+LandscapeName+'.for');
  // write Forest patches
  Expand_x4_Save_V2(NewLandscapePath+'\ForestMaps');

  // copy the thermal map file
  MessageShow('Copying Thermal map');
  CopyFile(pchar(V1_LandscapePath+'\'+LandscapeName+'.tdm'),
    pchar(NewLandscapePath+'\'+NewLandscapeName+'.tdm'),false);

  // sometimes, access to this folder doesn't work ???
  if (NOT DirectoryExists(ApplicationPath+'\Condor_V1')) then begin
    MessageShow('Unable to access .\Condor_V1 folder');
  end;

  // copy the airport file and create airport folder
  MessageShow('Converting Airports');
  CopyFile(pchar(V1_LandscapePath+'\'+LandscapeName+'.apt'),
    pchar(NewLandscapePath+'\'+NewLandscapeName+'.apt'),false);
  ForceDirectories(NewLandscapePath+'\Airports\V1\Textures');
  // for each airport,
    // ignore S file - probably used for friction with ground in V1
    // if find G
       // convert to c3d
    // else create blank G file
    // if find O file
      // add default windsock and convert to c3d
    // else create default O file with default windsock
    // get all texture files associated with airports
  Airport_FolderName := NewLandscapePath;
  Airport_FileName := NewLandscapeName+'.apt';
  ReadAirportFile;
  ProgressBar_Status.Max := Airport_Count;
  for i := 0 to Airport_Count-1 do begin
    with Airport_list[i] do begin
      // airport name cannot have a '/' or '\' because it affects texture path
      apName := StringReplace(apName,'/','-',[rfReplaceAll]);
      apName := StringReplace(apName,'\','-',[rfReplaceAll]);
      CurrentPath := 'Airports';
      // look for G file
      // Condor V2 only allows for grass, asphalt, grasspaint and asphaltpaint
      // and each must be a single item
      // cannot therefore convert Condor V1 runways properly...
    { disable for now and create a default runway
      X_ObjectFileName := V1_LandscapePath+'\'+CurrentPath+'\'+apName+'G.X';
      if (FileExists(X_ObjectFileName)) then begin // read it
        ReadCondorXfile(X_Object  FileName, false);
      end else begin
        X_ObjectFileName := V1_LandscapePath+'\'+CurrentPath+'\'+apName+'G.CX';
        if (FileExists(X_ObjectFileName)) then begin // read it
          ReadCondorXfile(X_ObjectFileName, false);
        end else begin
    }      // create a blank one that is needed for O object and windsock
          // for now use default 20m x 800m
          if (apAsphaltFlag <> 0) then begin
            ReadCondorXfile(ApplicationPath+'\Condor_V1\Rwy_Asphalt.px', false);
          end else begin
            ReadCondorXfile(ApplicationPath+'\Condor_V1\Rwy_Grass.px', false);
          end;
    {    end;
      end;
    }  // Need to copy textures for this object
      CopyObjectTextures;
      // save as c3d
      c3d_ObjectFileName := NewLandscapePath+'\Airports\'+apName+'G.c3d';
      WriteCondorC3Dfile(c3d_ObjectFileName);

      // look for O file
      X_ObjectFileName := V1_LandscapePath+'\'+CurrentPath+'\'+apName+'O.X';
      if (FileExists(X_ObjectFileName)) then begin // read it
          ReadCondorXfile(X_ObjectFileName, false);
      end else begin
        X_ObjectFileName := V1_LandscapePath+'\'+CurrentPath+'\'+apName+'O.CX';
        if (FileExists(X_ObjectFileName)) then begin // read it
          ReadCondorXfile(X_ObjectFileName, false);
        end else begin
          // create a blank one that is needed for O object and windsock
          ReadCondorXfile(ApplicationPath+'\Condor_V1\Empty.px', false);
        end;
      end;
      // append a Windsock
      // for now use default windsock at centre and off to the right 30 m
      ReadCondorXfile(ApplicationPath+'\Condor_V1\WindSock.px', true);
      // Need to copy textures for this object
      CopyObjectTextures;
      // save as c3d
      c3d_ObjectFileName := NewLandscapePath+'\Airports\'+apName+'O.c3d';
      WriteCondorC3Dfile(c3d_ObjectFileName);
    end;

    ProgressBar_Status.StepIt;
    Application.ProcessMessages;
  end;
  ProgressBar_Status.Position := 0;
  // in case data had to be changed
  WriteAirportFile;

  // now add textures for windsock mast and possibly others
  // special case since file(s) added
  if (FindFirst(ApplicationPath+'\Condor_V1\V1\Textures\*.*', faNormalFile, SearchRec)) = 0 then begin
    CopyFile(pchar(ApplicationPath+'\Condor_V1\V1\Textures\'+SearchRec.Name),
      pchar(NewLandscapePath+'\Airports\V1\Textures\'+SearchRec.Name),false);
    while (FindNext(SearchRec) = 0) do begin
      CopyFile(pchar(ApplicationPath+'\Condor_V1\V1\Textures\'+SearchRec.Name),
        pchar(NewLandscapePath+'\Airports\V1\Textures\'+SearchRec.Name),false);
    end;
    FindClose(SearchRec);
  end;

  // copy the object file if any
  MessageShow('Converting Objects');
  if (NOT FileExists(V1_LandscapePath+'\'+LandscapeName+'.obj')) then begin
    // create an empty file
    AssignFile(Dummy_File, NewLandscapePath+'\'+NewLandscapeName+'.obj');
    Rewrite(Dummy_File);
    CloseFile(Dummy_File);
  end else begin
    CopyFile(pchar(V1_LandscapePath+'\'+LandscapeName+'.obj'),
      pchar(NewLandscapePath+'\'+NewLandscapeName+'.obj'),false);
    // read the object list
    Object_FolderName := NewLandscapePath;
    Object_FileName := NewLandscapeName+'.obj';
    ReadObjectFile;
    // convert the links to c3d objects and copy and convert the objects
    CopyAndConvert;
    // save the converted object file
    WriteObjectFile;
  end;

  // copy the cup file
  MessageShow('Copying waypoints (CUP) file');
  CopyFile(pchar(V1_LandscapePath+'\'+LandscapeName+'.cup'),
    pchar(NewLandscapePath+'\'+NewLandscapeName+'.cup'),false);

  // copy the ini file; if no ini, create one
  // and add RealtimeShading line for V2
  MessageShow('Copying INI file');
  CopyFile(pchar(V1_LandscapePath+'\'+LandscapeName+'.ini'),
    pchar(NewLandscapePath+'\'+NewLandscapeName+'.ini'),false);
  Condor_Ini := TIniFile.Create(NewLandscapePath+'\'+NewLandscapeName+'.ini');
  with Condor_Ini do begin
    if (ReadString('General', 'Version', 'ERROR') = 'ERROR' ) then begin
      WriteString('General', 'Version', '1.0');
    end;
    WriteString('General', 'RealtimeShading', '1');
  end;
  Condor_Ini.Free;

  // copy all BMP files and rename the main BMP file
  MessageShow('Copying Flight-map and other maps');
  if (FindFirst(V1_LandscapePath+'\*.bmp', faNormalFile, SearchRec)) = 0 then begin
    CopyFile(pchar(V1_LandscapePath+'\'+SearchRec.Name),
      pchar(NewLandscapePath+'\'+SearchRec.Name),false);
    while (FindNext(SearchRec) = 0) do begin
      CopyFile(pchar(V1_LandscapePath+'\'+SearchRec.Name),
        pchar(NewLandscapePath+'\'+SearchRec.Name),false);
    end;
    FindClose(SearchRec);
  end;
  // if conversion done before, file already exists, so erase it first
  if (FileExists(NewLandscapePath+'\'+NewLandscapeName+'.bmp')) then begin
    DeleteFile(NewLandscapePath+'\'+NewLandscapeName+'.bmp');
  end;
  // now rename with new landscape name
  RenameFile(NewLandscapePath+'\'+LandscapeName+'.bmp',
    NewLandscapePath+'\'+NewLandscapeName+'.bmp');

  // copy the Images folder if any
  MessageShow('Copying Images');
  if (DirectoryExists(V1_LandscapePath+'\Images')) then begin
    ForceDirectories(NewLandscapePath+'\Images');
    if (FindFirst(V1_LandscapePath+'\Images\*.*', faNormalFile, SearchRec)) = 0 then begin
      CopyFile(pchar(V1_LandscapePath+'\Images\'+SearchRec.Name),
        pchar(NewLandscapePath+'\Images\'+SearchRec.Name),false);
      while (FindNext(SearchRec) = 0) do begin
        CopyFile(pchar(V1_LandscapePath+'\Images\'+SearchRec.Name),
          pchar(NewLandscapePath+'\Images\'+SearchRec.Name),false);
      end;
      FindClose(SearchRec);
    end;
  end;

  MessageShow('Conversion complete');
  MessageShow('Use Condor Landscape Editor to create Terrain and Forest hash files');
  Beep;

  Screen.Cursor := crDefault;  // no longer busy

end;

{
  When inserting triangle, keep in array in case same edge used again in
  adjacent triangle to not duplicate another extra vertex
    nvInserted : array of array[0..3-1] of longword;
      (IndexA, IndexB, NewVertex)
  Because vertex sequence on surfaces is counter-clockwise, other surfaces
  will use reverse sequence, i.e. IndexB, IndexA to search in table
  Once used, clear entry, and also clear table at end of surfaces before looping
}

{----------------------------------------------------------------------------}
// new variables for use with Grass3D generation based on Grass
const
  MaxEdgeSize = 100;
var
  NewFrameData   : pFrame;
  NewMeshData    : pMesh;
  NewNormalsData : pMeshNormals;
  NewTcoordsData : pMeshTcoord;
  NewMaterialListData : pMaterialList;
  NewMaterialData : pMaterial;
  NewFileNameData : pFileName;

  numInserted : longint;
  nvInserted : array of array[0..3-1] of longword;

{----------------------------------------------------------------------------}
procedure splitTriangle(indexA, indexB, Sfc, Edge : integer);
var
  i, j : integer;
  NewIndex : integer;
  matchingVertex : boolean;

begin
  // first check array for matching vertex
  matchingVertex := false;
  numInserted := length(nvInserted);
  if (numInserted > 0) then begin
    // scan array for matching edge
    for j := 0 to numInserted-1 do begin
      if ((indexA = nvInserted[j][1]) AND (indexB = nvInserted[j][0])) then begin
        matchingVertex := true;
        NewIndex := nvInserted[j][2];
        nvInserted[j][0] := 0;
        nvInserted[j][1] := 0;
        break;
      end;
    end;
  end;

  with NewMeshData^ do begin
    if (Not MatchingVertex) then begin
      // add a vertex
      Inc(tArray.aCount);
      // insert a vertex at mid-point
      NewIndex := tArray.aCount-1;
      setlength(tArray.aArray,tArray.aCount,3);
      // calculate mid point vertex
      for i := 0 to 3-1 do begin
        tArray.aArray[NewIndex][i] := (tArray.aArray[indexA][i] + tArray.aArray[indexB][i]) /2;
      end;

      // keep track of insertions for later matching
      numInserted := length(nvInserted);
      setlength(nvInserted,numInserted+1);
      nvInserted[numInserted][0] := indexA;
      nvInserted[numInserted][1] := indexB;
      nvInserted[numInserted][2] := NewIndex;
    end;

    // add a surface
    Inc(sArray.aCount);
    // insert a vertex at mid-point
    setlength(sArray.aArray,sArray.aCount);
    sArray.aArray[sArray.aCount-1].aCount := 3;
    setlength(sArray.aArray[sArray.aCount-1].aArray,3);
    // calculate new surface
    sArray.aArray[sArray.aCount-1].aArray[0] := NewIndex;
    sArray.aArray[sArray.aCount-1].aArray[1] := sArray.aArray[Sfc].aArray[(Edge+2) mod 3];
    sArray.aArray[sArray.aCount-1].aArray[2] := sArray.aArray[Sfc].aArray[Edge];
    // update current surface
    sArray.aArray[Sfc].aArray[Edge] := NewIndex;
  end;
  with NewNormalsData^ do begin
    if (Not MatchingVertex) then begin
      // add a vertex normal
      Inc(tArray.aCount);
      // insert a vertex at mid-point
      NewIndex := tArray.aCount-1;
      setlength(tArray.aArray,tArray.aCount,3);
      // calculate mid point vertex
      for i := 0 to 3-1 do begin
        tArray.aArray[NewIndex][i] := (tArray.aArray[indexA][i] + tArray.aArray[indexB][i]) /2;
      end;
    end;
    // add a surface
    Inc(sArray.aCount);
    // insert a vertex at mid-point
    setlength(sArray.aArray,sArray.aCount);
    sArray.aArray[sArray.aCount-1].aCount := 3;
    setlength(sArray.aArray[sArray.aCount-1].aArray,3);
    // calculate new surface
    sArray.aArray[sArray.aCount-1].aArray[0] := NewIndex;
    sArray.aArray[sArray.aCount-1].aArray[1] := sArray.aArray[Sfc].aArray[(Edge+2) mod 3];
    sArray.aArray[sArray.aCount-1].aArray[2] := sArray.aArray[Sfc].aArray[Edge];
    // update current surface
    sArray.aArray[Sfc].aArray[Edge] := NewIndex;
  end;
  with NewTcoordsData^ do begin
    if (Not MatchingVertex) then begin
      // add a coord
      Inc(tArray.aCount);
      // insert a coord at mid-point
      NewIndex := tArray.aCount-1;
      setlength(tArray.aArray,tArray.aCount,2);
      // calculate mid point coord
      for i := 0 to 2-1 do begin
        tArray.aArray[NewIndex][i] := (tArray.aArray[indexA][i] + tArray.aArray[indexB][i]) /2;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
function CalcLength(indexA, indexB : integer) : single;
begin
  with NewMeshData^ do begin
    result := (tArray.aArray[indexB][0] - tArray.aArray[indexA][0]) *
              (tArray.aArray[indexB][0] - tArray.aArray[indexA][0]) +
              (tArray.aArray[indexB][1] - tArray.aArray[indexA][1]) *
              (tArray.aArray[indexB][1] - tArray.aArray[indexA][1]);
  end;
end;

{----------------------------------------------------------------------------}
Procedure ScanTriangles;
var
  Done : boolean;
  Sfc, Edge, EdgeMax : integer;
  lengthSQ, LengthMax : single;
  IndexA, IndexB, IndexAmax, IndexBmax : integer;

begin
  Done := false;
  with NewMeshData^ do begin
    While (NOT Done) do begin
      Done := true; // assume for now
      // clear array
      setlength(nvInserted,0);
      // for each surface/triangle
      for Sfc := 0 to sArray.aCount-1 do begin
        // for each of 3 triangle edge
        LengthMax := 0;
        for Edge := 0 to 3-1 do begin
          indexA := sArray.aArray[Sfc].aArray[Edge];
          indexB := sArray.aArray[Sfc].aArray[(Edge+1) mod 3];
          lengthSQ := CalcLength(indexA,indexB);
          // find longest edge
          if (lengthSQ > LengthMax) then begin
            LengthMax := lengthSQ;
            IndexAmax := indexA;
            IndexBmax := indexB;
            EdgeMax := Edge;
          end;
        end; // edge
        // now check if longest edge too big
        if (lengthMax > MaxEdgeSize*MaxEdgeSize) then begin
          // too big, so split triangle
          splitTriangle(indexAmax, indexBmax, Sfc, EdgeMax);
          Done := false;
        end;
      end; // Sfc
    end; // while
  end;
end;

{----------------------------------------------------------------------------}
Procedure AddGrass3Dframe(oTreeView : TTreeView; Index : integer);
var
  Index2 : integer;
  pObjectData : pObjectItem;
  fTreeNode, fTreeNode2 : TTreeNode;
begin
  // insert Grass3D before Grass so that if process attempted, again
  // Grass3D found first will stop re-creating it again.

  // need the whole frame, i.e. all children
  // make a copy of the frame - contains name
  New(NewFrameData);
  ExtractFrame(NewFrameData^, oTreeView, Index-1);
  // make a copy of the mesh - contains vertices and surfaces
  New(NewMeshData);
  ExtractMesh(NewMeshData^, oTreeView, Index);
  // make a copy of the mesh normals - contains normals and surfaces
  New(NewNormalsData);
  ExtractNormals(NewNormalsData^, oTreeView, Index+1);
  // make a copy of the texture coordinates
  New(NewTCoordsData);
  ExtractTcoords(NewTCoordsData^, oTreeView, Index+2);
  // make a copy of the material list
  New(NewMaterialListData);
  ExtractMaterialList(NewMaterialListData^, oTreeView, Index+3);
  // make a copy of the material
  New(NewMaterialData);
  ExtractMaterial(NewMaterialData^, oTreeView, Index+4);
  // make a copy of the file name
  New(NewFileNameData);
  ExtractTFileName(NewFileNameData^, oTreeView, Index+5);

  // zero array on inserted vertices
  numInserted := 0;
  setlength(nvInserted,0);

  // examine each triangle and split if too large
  ScanTriangles; // in NewMeshData

  // now add new data into treeview
  //insert NewFrame
  New(pObjectData); //allocate space for an object data
  pObjectData^.oType := oFrame;
  pObjectData^.oPointer := NewFrameData;
  fTreeNode := oTreeView.Items.InsertObject(oTreeView.Items[Index], '',pObjectData);
  //insert NewMesh
  New(pObjectData); //allocate space for an object data
  pObjectData^.oType := oMesh;
  pObjectData^.oPointer := NewMeshData;
  NewMeshData^.tName := 'Grass3D';  // C3D mesh name!
  fTreeNode2 := oTreeView.Items.AddChildObject(fTreeNode, '',pObjectData);
  //insert NewNormals
  New(pObjectData); //allocate space for an object data
  pObjectData^.oType := oMeshNormals;
  pObjectData^.oPointer := NewNormalsData;
  fTreeNode2 := oTreeView.Items.AddChildObject(fTreeNode, '',pObjectData);
  //insert NewTCoord
  New(pObjectData); //allocate space for an object data
  pObjectData^.oType := oMeshTextureCoord;
  pObjectData^.oPointer := NewTCoordsData;
  fTreeNode2 := oTreeView.Items.AddChildObject(fTreeNode, '',pObjectData);
  //insert NewMaterialList
  New(pObjectData); //allocate space for an object data
  pObjectData^.oType := oMaterialList;
  pObjectData^.oPointer := NewMaterialListData;
  fTreeNode2 := oTreeView.Items.AddChildObject(fTreeNode, '',pObjectData);
  New(pObjectData); //allocate space for an object data
  pObjectData^.oType := oMaterial;
  pObjectData^.oPointer := NewMaterialData;
  NewMaterialData^.tRGBA[3] := 0.33; // 33% opaqueness to merge with background
  fTreeNode2 := oTreeView.Items.AddChildObject(fTreeNode2, '',pObjectData);
  New(pObjectData); //allocate space for an object data
  pObjectData^.oType := oFileName;
  NewFileNameData^.tqName := 'Textures/grass_rgba.dds';
  pObjectData^.oPointer := NewFileNameData;
  fTreeNode2 := oTreeView.Items.AddChildObject(fTreeNode2, '',pObjectData);

  DataChanged := true;
end;

{----------------------------------------------------------------------------}
Procedure AdjustMaterial(oTreeView : TTreeView; Index, Which : integer);
begin
  // check if indeed oFileName
  if ( (oTreeView.Items[Index].data <> nil) AND
       (pObjectItem(oTreeView.Items[Index].data)^.oType in
       [oMaterial]) ) then begin
    with pMaterial(pObjectItem(oTreeView.Items[Index].data)^.oPointer)^ do begin // pMaterialData
      case Which of
        0: begin
          //change material Alpha to 0.0
          tRGBA[3] := 0.0;
        end;
        1: begin
          //change material color to 0.55, 0.55, 0.55
          tRGBA[0] := 0.55;
          tRGBA[1] := 0.55;
          tRGBA[2] := 0.55;
        end;
      end;
    end;
    DataChanged := true;
  end;
end;

{----------------------------------------------------------------------------}
Procedure AdjustAsphaltMesh(oTreeView : TTreeView; Index : integer);
var
  TextureFileName : string;
begin
  // examine the texturefilename 5 nodes ahead
  if (oTreeView.Items.Count < Index+5) then begin
    // error
    Exit;
  end else begin
    // check if indeed oFileName
    if ( (oTreeView.Items[Index+5].data <> nil) AND
         (pObjectItem(oTreeView.Items[Index+5].data)^.oType in
         [oFileName]) ) then begin
      TextureFileName := pFileName(pObjectItem(oTreeView.Items[Index+5].data)^.oPointer)^.tqName;
      if (upperCase(TextureFileName) = 'TRANSPARENT.DDS') then begin
        //change material Alpha to 0.0
        AdjustMaterial(u_X_CX.oTreeView, Index+4, 0);
        MessageShow('- Adjust: Alpha ');
      end else begin
        if ((TextureFileName) = '') then begin
          //change material color to 0.55, 0.55, 0.55
          AdjustMaterial(u_X_CX.oTreeView, Index+4, 1);
          MessageShow('- Adjust: Color ');
        end;
      end;
    end;
  end;
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_Convert_V2_V3Click(Sender: TObject);
var
  i, Index : integer;
  V2_LandscapePath : string;
  FileName, ObjectFileName : string;
  nName : string;
  SearchRec: TSearchRec;
  GrassDone : boolean;
  Grass3DInserted : boolean;

  Dummy_File : file;
begin
  // convert in-place (and use a symbolic-link in V3 folder)
  // convert all BMP flightPlanner maps to 32 bits in if not already
  // add blank airspace file .air, .aha, .oha, dummy .tm3
  // use Landscape Creator to create extra 'hash' files and TM3 thermal file

  // need landscape header and tile extent
  if NOT ((HeaderOpen) AND (TileOpen)) then begin
    MessageShow('Need a V2 landscape selected and Read Header file first');
    Beep;
    Exit;
  end;

  V2_LandscapePath := ExpandFileName(Working_Folder+'\..');

  if (
      ( NOT DirectoryExists(V2_LandscapePath+'\HeightMaps') ) OR  // V1 ?
      ( FileExists(V2_LandscapePath+'\'+LandscapeName+'.air') )   // V3 ?
     ) then begin
    MessageShow(LandscapeName + ' Not a V2 landscape');
    Beep;
    Exit;
  end;

  if MessageDlg('Are you sure you want convert "'+LandscapeName+'" scenery to V3 ?', mtConfirmation,
    [mbYes, mbNo], 0) = mrNo then begin
    Exit;
  end;
{
  if MessageDlg('Add a link to this landscape "'+LandscapeName+'" ?', mtConfirmation,
    [mbYes, mbNo], 0) = mrYes then begin
    // add a symbolic-link in V3 folder (?)

  end;
}
  try
    Screen.Cursor := crHourGlass;  // Let user know we're busy...
    // need a treeView to store object data
    // make sure a treeView is selected; use one in Unit_Objects
    u_X_CX.oTreeView := Unit_Objects.Form_Objects.TreeView_Object;

    // need to assign progress bar
    u_BMP.ProgressBar_Status := ProgressBar_Status;
    u_Thermal.ProgressBar_Status := ProgressBar_Status;

    // convert 24 bit color BMPs to 32 bit color
    if (FindFirst(V2_LandscapePath+'\*.bmp', faNormalFile, SearchRec)) = 0 then begin
      FileName := V2_LandscapePath+'\'+SearchRec.Name;
      if (BMP_ColorSize(FileName) <> 32) then begin
        RenameFile(FileName, FileName+'.bmp');
        Bitmap_24_To_Bitmap_32(FileName+'.bmp', FileName, true, 1.0);
        DeleteFile(FileName+'.bmp');
      end;
      while (FindNext(SearchRec) = 0) do begin
        FileName := V2_LandscapePath+'\'+SearchRec.Name;
        if (BMP_ColorSize(FileName) <> 32) then begin
          RenameFile(FileName, FileName+'.bmp');
          Bitmap_24_To_Bitmap_32(FileName+'.bmp',FileName, false,1.0);
          DeleteFile(FileName+'.bmp');
        end;
        Application.ProcessMessages;
      end;
      FindClose(SearchRec);
    end;

    // add an empty Airspace file
    if (NOT FileExists(V2_LandscapePath+'\'+LandscapeName+'.air')) then begin
      MakeDummy_AIR(V2_LandscapePath,'\'+LandscapeName+'.air');
      // add an Airspace .aha HASH file for an empty Airspace file
      MakeDummy_AHA(V2_LandscapePath,'\'+LandscapeName+'.aha');
    end;

    // add an AutoGen HASH .oha file for an empty AutoGen folder
    if (NOT FileExists(V2_LandscapePath+'\'+LandscapeName+'.oha')) then begin
      MakeDummy_OHA(ColumnCount div 64, RowCount div 64, V2_LandscapePath,'\'+LandscapeName+'.oha');
    end;

    // add a dummy TM3 thermalMap file to start
    // unknown format (???) so use LandscapeEditor to make actual TM3
    if (NOT FileExists(V2_LandscapePath+'\'+LandscapeName+'.tm3')) then begin
//      MakeDummyBlank_TM3(128, ColumnCount, RowCount, V2_LandscapePath,'\'+LandscapeName+'.tm3');
      MakeDummyTDM_TM3(V2_LandscapePath,LandscapeName);
    end;

    // read airport file to get list of airports
    Airport_FolderName := V2_LandscapePath;
    Airport_FileName := LandscapeName+'.apt';
    if (NOT FileExists(V2_LandscapePath+'\'+Airport_FileName)) then begin
      MessageShow('ERROR: file '+Airport_FileName+' Not found');
    end else begin
      Grass3DInserted := false;
      ReadAirportFile;
      // for each airport
      for i := 0 to Airport_Count-1 do begin
        with Airport_list[i] do begin
          // look for the airportG file
          ObjectFileName := Airport_FolderName+'\Airports\'+apName+'G.c3d';
          if (NOT FileExists(ObjectFileName)) then begin
            MessageShow('ERROR: file '+apName+'G.c3d Not found');
          end else begin
            MessageShow('Check Airport: '+apName);
            DataChanged := false; // assume for now
            // open the airportG file
            ReadCondorC3Dfile(ObjectFileName, false);
            // look for Asphalt and Grass meshes
            Index := 0;
            GrassDone := false;
            While (Index <> -1) do begin
              // scan TreeView_G looking for mesh name = asphalt or grass
              Index := FindNodebyType(u_X_CX.oTreeView, Index, oMesh, nName);
              if (Index <> -1) then begin
                if (upperCase(nName) = 'ASPHALT') then begin
                  AdjustAsphaltMesh(u_X_CX.oTreeView, Index);
                end else begin
                  if (uppercase(nName) = 'GRASS3D') then begin
                    // already done; skip GRASS
                    GrassDone := true;
       	          end else begin
                    if ((uppercase(nName) = 'GRASS') AND (NOT GrassDone)) then begin
                  //    CopyAndAddFrame(u_X_CX.oTreeView, Index);
                  //    INC(Index,14-1); // 14 because added 7 above and processed 7 - ??? messy find another way ?
                      AddGrass3Dframe(u_X_CX.oTreeView, Index);
                      Grass3DInserted := true;
//                      INC(Index,7-1); // 7 because 7 processed, so just keep
                      INC(Index,14-1); // 14 because added 7 above and processed 7 - ??? messy find another way ?
                    end;
                  end;
                end;
                INC(Index,1);
              end;
            end;
            if (DataChanged) then begin
              // update the airportG file
              WriteCondorC3Dfile(ObjectFileName);
            end;
          end;
        end;
      end;
    end;

    // if grass3D inserted, also copy Textures/green_64.dds
    if (Grass3DInserted) then begin
      ForceDirectories(Airport_FolderName+'\Airports\Textures');
      if (NOT fileExists(Airport_FolderName+'\Airports\Textures\grass_rgba.dds')) then begin
        CopyFile(pchar(ApplicationPath+'\Condor_V2\grass_rgba.dds'),
          pchar(Airport_FolderName+'\Airports\Textures\grass_rgba.dds'),false);
      end;
    end;

    MessageShow('Conversion complete');
    MessageShow('Use Condor Landscape Editor to create additional files');
    Beep;

  finally
    Screen.Cursor := crDefault;  // no longer busy
  end;

end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_XP_ConvertClick(Sender: TObject);
type
  Object_Definition = record
    Path : string;
    Name : string;
  end;
  Object_Placement = record
    Index : longint;
    Longitude : double;
    Latitude : double;
    Orientation : double;
  end;
var
  i : integer;
  Temp_File : TextFile;
//  Temp_File : File_Generic_Text;
  TempSTR : string;
  pString : string;
  FilePath : string;
  LongMin, LongMax, LatMin, LatMax : double;
  ObjectList_Count : Longint;
//  ObjectPlacement_Count : Longint;
  ObjectList : array of Object_Definition;
  ObjectPlacement : array of Object_Placement;
  ObjName : string;
//  ObjRef : string;
  SourceFileName : string;
  OutputFileName : string;
  TextureFile    : string;
  TextureFolder  : string;
  SearchRec: TSearchRec;
  Reference_lib : string;
  Replacement_lib : string;
  TryIt : integer;

// - - - - - - - - - - - - - - - - - -
function CropToLandscape(OP : Object_Placement) : Boolean;
begin
  Result := false;
  if (OP.Longitude > LongMax) then begin
    Exit;
  end;
  if (OP.Longitude < LongMin) then begin
    Exit;
  end;
  if (OP.Latitude > LatMax) then begin
    Exit;
  end;
  if (OP.Latitude < LatMin) then begin
    Exit;
  end;
  Result := true;
end;

// - - - - - - - - - - - - - - - - - -
function Xplane_Search_Library(FileName : string) : boolean;
begin
  Result := false;
  AssignFile(Temp_file, FileName);
  Reset(Temp_file);
  while NOT EOF(Temp_file) do begin
//    GT_ReadLine(@Temp_file,TempSTR);
    ReadLine(@Temp_file,TempSTR);
//    ReadLn(Temp_file,TempSTR);
    ParseText(TempSTR,pString);
    if ((UpperCase(pString) = 'EXPORT') OR
        (UpperCase(pString) = 'EXPORT_EXTEND')) then begin
//EXPORT lib/g10/EU/suburban/gar1_6x8.obj EU/suburban/gar1_6x8Ga1_02.obj
      ParseText(Tempstr,Reference_lib);
      if (Reference_lib = ObjName) then begin
        ParseText(Tempstr,Replacement_lib);
        Result := true;
        break;
      end;
    end;
  end;
end;

// - - - - - - - - - - - - - - - - - -
function Xplane_Search_Default_lib : boolean;
var
  SearchPath : string;
begin
  Result := false;
  // look into each sub-folder in default_lib
  SearchPath := Working_Folder+'\XP\XP_Objects\default_lib';
  if (FindFirst(SearchPath+'\*.*', fadirectory, SearchRec)) = 0 then begin
    repeat
      if ((SearchRec.Name = '.') OR (SearchRec.Name = '..') )then begin
        continue;
      end;
      // look for library.txt file
      if (FileExists(SearchPath+'\'+SearchRec.Name+'\library.txt')) then begin
        if (Xplane_Search_Library(SearchPath+'\'+SearchRec.Name+'\library.txt')) then begin
          ObjName := 'default_lib\'+SearchRec.Name+'\'+Replacement_lib;
//          if (FileExists(SourceFileName)) then begin
            Result := true;
            break;
//          end;
        end;
      end;
    until ((FindNext(SearchRec) <> 0) OR Result);
    FindClose(SearchRec);
  end;
end;

// - - - - - - - - - - - - - - - - - -
procedure CheckLibReference;
begin
  // check first level folder - need to differentiate between /lib/ and lib/
  // if 'lib/' then check default_lib
  if (UpperCase(copy(ObjName,1,5)) = '/LIB/') then begin
    ObjName := copy(ObjName,2,length(ObjName));
  end else begin
    if (UpperCase(copy(ObjName,1,4)) = 'LIB/') then begin
      if (NOT Xplane_Search_Default_lib) then begin
        ObjName := 'None';
      end;
    end;
  end;
end;

// - - - - - - - - - - - - - - - - - -
procedure CheckForTextureFile;
var
  TextureFileName : string;
begin
                  // get texture file name
                  TextureFile := GetXplaneOBJ8texture;
                  TextureFile := StringReplace(TextureFile,'/','\',[rfReplaceAll]);   //could be / or \
                  if (TextureFile <> '') then begin
                    TextureFolder := ExtractFileDir(SourceFileName);
                    // copy the texture file
                    TryIt := 0;
                    repeat
                      case tryIt of
                        0: begin
                          INC(TryIt);
                        end;
                        1: begin
                          // try with dds instead.
                          TextureFile := copy(TextureFile,1,length(TextureFile)-
                                length(ExtractFileExt(TextureFile))) +'.dds';
                          INC(TryIt);
                        end;
                        else begin
                          MessageShow('Missing: '+TextureFolder +'\'+ TextureFile);
                          TryIt := 0; // all done
                        end;
                      end;
                      // check if file exists
                      if (FileExists(TextureFolder +'\'+ TextureFile)) then begin
                        if (TryIt > 1) then begin // has changed ?
                          AdjustXplaneOBJ8texture(TextureFile);
                        end;
                        // keep path name, so need to create folder if needed
                        TextureFileName := ExtractFileDir(OutputFileName) +'\'+ TextureFile;
                        ForceDirectories(ExtractFileDir(TextureFileName));
                        if (NOT FileExists(TextureFileName)) then begin
                          CopyFile(pchar(TextureFolder +'\'+ TextureFile),
                            pchar(TextureFileName),false);
                        end;
                        TryIt := 0; // all done
                      end;
                    until (Tryit = 0);  // all done ?
                  end;
end;

// - - - - - - - - - - - - - - - - - -
begin
  // first time around, create structure
  FilePath := Working_Folder+'\XP\XP_Objects';
  if (NOT DirectoryExists(FilePath)) then begin
    ForceDirectories(FilePath);
    MessageShow('XP folder created');
  end;
  ObjectList_Count := 0;
  ObjectPlacement_Count := 0;
  // need landscape header and tile extent
  if (NOT ((HeaderOpen) AND (TileOpen))) then begin
    MessageShow('Need Header file first');
    Beep;
  end else begin
    // calculate the extents to filter out objects
    UTMtoLatLong(UTM_Top, UTM_Left, UTM_Zone, UTM_ZoneNS);
    LatMax := uLatitude;
    LongMin := uLongitude;
    UTMtoLatLong(UTM_Bottom, UTM_Right, UTM_Zone, UTM_ZoneNS);
    LatMin :=uLatitude;
    LongMax := uLongitude;

    // look for DSF file and read the object details
    if (FileExists(Working_Folder+'\XP\DSF.txt')) then begin
      Screen.Cursor := crHourGlass;  // Let user know we're busy...
//      AssignFile(Temp_file, Working_Folder+'\XP\DSF.txt');
//      Reset(Temp_file);
//      while NOT EOF(Temp_file) do begin
//        GT_ReadLine(@Temp_file,TempSTR);
      AssignFile(Temp_file, Working_Folder+'\XP\DSF.txt');
      Reset(Temp_file);
      while NOT EOF(Temp_file) do begin
        ReadLn(Temp_file,TempSTR);
        ParseText(TempSTR,pString);
        if (UpperCase(pString) = 'OBJECT_DEF') then begin
          SetLength(ObjectList,ObjectList_Count+1);
          ParseText(Tempstr,ObjectList[ObjectList_Count].Name);
          INC(ObjectList_Count);
        end else begin
          if (UpperCase(pString) = 'OBJECT') then begin
            SetLength(ObjectPlacement,ObjectPlacement_Count+1);
            with ObjectPlacement[ObjectPlacement_Count] do begin
              ParseInteger(Tempstr,Index);
              ParseFloat(Tempstr,Longitude);
              ParseFloat(Tempstr,Latitude);
              ParseFloat(Tempstr,Orientation);
              // only keep .obj objects
              if ( (Index < ObjectList_Count) AND
//                   (ExtractFileExt(ObjectList[Index].name) = 'obj') ) then begin
                   (ExtractFileExt(StringReplace(ObjectList[Index].name,'/','\',[rfReplaceAll])) = '.obj') ) then begin
                // exclude files that have 'tree' in mame ?
                if ((Pos('TREE',UpperCase(ObjectList[Index].name)) <> 0) OR
                    (Pos('CONIFER',UpperCase(ObjectList[Index].name)) <> 0)) then begin
                  // skip
//                  MessageShow('Ignoring TREE');
                end else begin
                  // only keep objects in range
                  if (CropToLandscape(ObjectPlacement[ObjectPlacement_Count])) then begin
                    INC(ObjectPlacement_Count);
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
      CloseFile(Temp_file);

      // create Condor Object file
      SetLength(Object_List,ObjectPlacement_Count);
      for i := 0 to ObjectPlacement_Count-1 do begin
        with ObjectPlacement[i] do begin
          with Object_List[i] do begin
            // convert to relative UTM
            LatLongToUTM(Latitude,Longitude,UTM_Zone,UTM_ZoneNS);
            // make relative to scenery bottom-right
            coEasting   := (UTM_Right + Legacy_Offset) - uEasting;
            coNorthing  := uNorthing - (UTM_Bottom - Legacy_Offset);
            coElevation := 0.0;
            coScale     := 1.0;
            // adjust by 90 degrees
            coRotation  := (Orientation-90)*PI/180;
            // adjust path to World ???
            coName      := ObjectList[Index].Name;
          end;
        end;
      end;

      // Now write the Condor Object file
      Object_FolderName := Working_Folder+'\XP';
      Object_FileName := LandscapeName+'.obj';
      WriteObjectFile;

//      try
        // copy and convert object and textures
        // folder Working\XP\XP_Objects\objects     -> World\Objects\objects
        // folder Working\XP\XP_Objects\lib         -> World\Objects\lib
        // folder Working\XP\XP_Objects\default_lib -> World\Objects\default_lib
        ProgressBar_Status.Max := ObjectPlacement_Count;
        for i := 0 to ObjectPlacement_Count-1 do begin
          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
          with Object_List[i] do begin
            ObjName := coName;
            CheckLibReference;
            ObjName := StringReplace(ObjName,'/','\',[rfReplaceAll]);   //could be / or \
            SourceFileName := Working_Folder+'\XP\XP_Objects\'+ObjName;
            OutputFileName := ExtractFileExt(ObjName);
            coName := copy(ObjName,1,length(ObjName)-length(OutputFileName)) +'.c3d';
            OutputFileName := Working_Folder+'\..\World\Objects\'+ coName;
            if (NOT FileExists(OutputFileName)) then begin // already done ?
              if (NOT FileExists(SourceFileName)) then begin
                MessageShow('Missing: '+SourceFileName);
                continue; // skip rest of code below
              end;
              { else} begin
                // read the OBJ8 file
                if (NOT readXplaneOBJ8file(SourceFileName)) then begin
                  MessageShow('Error: '+SourceFileName);
                end else begin
                  // adjust the texture path?
                  CheckForTextureFile;
                  // write the Condor C3D file
                  ForceDirectories(ExtractFileDir(OutputFileName));
                  WriteCondorC3Dfile(OutputFileName);
                end;
              end;
            end;
          end;
        end;
//      except
//      end;

      // update object file with C3D file extension
      WriteObjectFile;

      // no longer busy
      ProgressBar_Status.Position := 0;
      Screen.Cursor := crDefault;
      MessageShow('Conversion done.');
    end;
  end;
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Shape_PickMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog1.Color := Shape_pick.Brush.Color;
  if (ColorDialog1.Execute) then begin
    Shape_pick.Brush.Color := ColorDialog1.Color;
  end;
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_DDS_BMPClick(Sender: TObject);
var
  FilePicture: TPicture; // to load DDS tiles
//  tBufr : TBitmap; // temporary bitmap
  Filename : String;
  File_Folder : String;
  File_Name : String;
  File_Name_NoExt : String;
  Dest_File_Path : String;
  i : integer;

begin
  OpenDialog1.Options := [ofAllowMultiSelect, ofFileMustExist];
  OpenDialog1.InitialDir := Initial_Folder+'\Textures';
  OpenDialog1.Filter := 'DDS files (*.DDS)|*.DDS|All files (*.*)|*.*';
  OpenDialog1.FileName := '';
  if OpenDialog1.Execute then begin
    with OpenDialog1.Files do begin
      try
        ProgressBar_Status.Max := Count;
        Screen.Cursor := crHourGlass;  // Let user know we're busy...
        FilePicture := TPicture.Create;
//        tBufr := TBitmap.Create;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clBtnFace;
    FilePicture.Bitmap.Canvas.fillrect(rect(0,0,FilePicture.Width,FilePicture.Height));

        for i := 0 to Count - 1 do begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clBtnFace;
    FilePicture.Bitmap.Canvas.fillrect(rect(0,0,FilePicture.Width,FilePicture.Height));
          FileName := Strings[i];
          File_Folder := ExtractFileDir(FileName);
          File_Name := ExtractFileName(FileName);
          File_Name_NoExt := copy(File_Name,1,pos('.dds',File_Name)-1);
//          Dest_File_Path := File_Folder+'\..\Working\Textures';
          Dest_File_Path := File_Folder+'\..\Working\Textures_Export';
          ForceDirectories(Dest_File_Path);
          // determine Options
          dxtOptions := 0;
          if (CheckBox_DDS_Color.Checked) then begin
            dxtOptions := dxtOptions OR dxt_Color;
          end;
          if (CheckBox_DDS_Transparent.Checked) then begin
            dxtOptions := dxtOptions OR dxt_Transparent;
          end;
          // convert DDS to Bitmap
          FilePicture.LoadFromFile(FileName);
          if ((dxtOptions AND dxt_Transparent) = dxt_Transparent) then begin
            // don't convert to pf24bit
          end else begin
// the following works to convert 32 bit to 24, but
// if there is transparency, the pixels will not be 'drawn' onto the default-white bitmap
// which could lead to errors if this bitmap is re-used without clearing!
// but very useful if that is what you want !
//            BMP_CopyMe(tBufr,FilePicture.bitmap);
//            WriteBitMapToFile(tBufr,Dest_File_Path+'\'+File_Name_NoExt+'.bmp');
      //   tried to convert 32 to 24 in modified WriteBitMapToFile while writing -> too slow
            FilePicture.Bitmap.PixelFormat := pf24bit;  // force 24 bit this way -> fastest
          end;
          WriteBitMapToFile(FilePicture.bitmap,Dest_File_Path+'\'+File_Name_NoExt+'.bmp');
           ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;

      finally
//        tBufr.Free;
        FilePicture.Free;
        Screen.Cursor := crDefault;  // no longer busy
        ProgressBar_Status.Position := 0;
      end;
      MessageShow('Conversion done.');
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure Make_Hashes(LandscapeName, BatchFileName : string);
var
  Batch_File : TextFile;

begin
  AssignFile(Batch_file, BatchFileName);
  Rewrite(Batch_file);
//  writeln(Hash_file,'C:\Condor2\CondorSceneryToolkit\LandscapeEditor.exe -hash '+ CurrentLandscape);
  writeln(Batch_file,LEfolder+'\LandscapeEditor.exe -hash '+ LandscapeName);
  // close the file
  CloseFile(Batch_file);
end;

//---------------------------------------------------------------------------
//Procedure Execute_BatchFile(FilePath, FileName, Params : String);
Function Execute_BatchFile(FilePath, FileName, Params : String) : DWORD;
//var
//  ExitCode : DWORD;
begin
  Result := Shell_Execute(FilePath, FileName, Params, true);
  case Result of
    DWORD(-1): begin
      MessageShow('ERROR - Cannot execute Batch file');
    end;
    0: begin
      MessageShow('Batch file done');
    end;
    else begin
      MessageShow(format('ERROR batch file exit code= %d',[Result]));
    end;
  end;
end;

//---------------------------------------------------------------------------
Procedure xExecute_BatchFile(FilePath, FileName, Params : String);
var
  SEInfo: TShellExecuteInfo;
  ExitCode: DWORD;
  ExecuteFile, ParamString, StartInString: string;
begin
  ExecuteFile:=FilePath+'\'+FileName;
  MessageShow('Executing batch file: '+FileName);

  FillChar(SEInfo, SizeOf(SEInfo), 0) ;
  SEInfo.cbSize := SizeOf(TShellExecuteInfo) ;
  with SEInfo do begin
    fMask := SEE_MASK_NOCLOSEPROCESS;
    Wnd := Application.Handle;
    lpFile := PChar(ExecuteFile) ;
{
  ParamString can contain the
  application parameters.
}
//  lpParameters := PChar(ParamString) ;
{
  StartInString specifies the
  name of the working directory.
  If ommited, the current directory is used.
}
//  lpDirectory := PChar(StartInString) ;
    nShow := SW_SHOWNORMAL;  // i.e. show the command window
  end;
  if ShellExecuteEx(@SEInfo) then begin
    repeat
      Application.ProcessMessages;
      GetExitCodeProcess(SEInfo.hProcess, ExitCode) ;
    until (ExitCode <> STILL_ACTIVE) or Application.Terminated;
    if (ExitCode = 0) then begin
      MessageShow('Batch file done');
    end else begin
      MessageShow(format('ERROR batch file exit code= %d',[ExitCode]));
    end;
  end else begin
    MessageShow('ERROR - Cannot execute Batch file');
  end;
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_Hash_ExecuteClick(Sender: TObject);
begin
  if (NOT ((HeaderOpen) AND (TileOpen)) ) then begin
    MessageShow('Need Header file first');
    Beep; Exit;
  end;
  // first make a batch file
  Make_Hashes(LandscapeName, Working_Folder+'\Make_Hash.bat');
  // then execute it
  Execute_BatchFile(Working_Folder, 'Make_Hash.bat', '');
end;

//-------------------------------------------------------------------------------------
procedure FormatConvert(nType : TileNameType);
var
  PatchColumnCount, PatchRowCount : integer;
  InBaseFileName, OutBaseFileName : string;
  FileName, FileFormat : string;
  FilePath, LandscapePath : string;

// - - - - - - - - - - - - - - - - - - - - - -
Procedure RenameFiles(FilePath, Prefix, Suffix : string);
var
  i, j : integer;

begin
  ProgressBar_Status.Max := (PatchRowCount)*(PatchColumnCount);
  for i := 0 to PatchRowCount-1 do begin
    for j := 0 to PatchColumnCount-1 do begin
      case ntype of
        xxyy: begin
          InBaseFileName :=  format('%s%3.3d%3.3d%s',[Prefix,j,i,Suffix]);
          OutBaseFileName := format('%s%2.2d%2.2d%s',[Prefix,j,i,Suffix]);
        end;
        xxxyyy: begin
          InBaseFileName :=  format('%s%2.2d%2.2d%s',[Prefix,j,i,Suffix]);
          OutBaseFileName := format('%s%3.3d%3.3d%s',[Prefix,j,i,Suffix]);
        end;
      end;
      RenameFile(FilePath+'\'+InBaseFileName, FilePath+'\'+OutBaseFileName);
      ProgressBar_Status.StepIt;
      Application.ProcessMessages;
    end;
  end;
  ProgressBar_Status.Position := 0;
end;

// - - - - - - - - - - - - - - - - - - - - - -
begin
  // need landscape header and tile extent
  if NOT ((HeaderOpen) AND (TileOpen)) then begin
    MessageShow('Need a V2 or V3 landscape selected and Read Header file first');
    Beep;
    Exit;
  end;

  LandscapePath := ExpandFileName(Working_Folder+'\..');
  PatchColumnCount := ColumnCount DIV pColumns;
  PatchRowCount    := RowCount DIV pRows;

  if (NOT DirectoryExists(LandscapePath+'\HeightMaps') ) then begin
    MessageShow(LandscapeName + ' not a V2 or V3 landscape');
    Beep;
    Exit;
  end;

  case ntype of
    xxyy: begin
      FileName := '0000';
      FileFormat := 'XXYY';
    end;
    xxxyyy: begin
      FileName := '000000';
      FileFormat := 'XXXYYY';
    end;
  end;

  if (FileExists(LandscapePath+'\HeightMaps\h'+FileName+'.tr3')) then begin
    MessageShow(LandscapeName + ' already in '+FileFormat+' format');
    Beep;
    Exit;
  end;

  if MessageDlg('Are you sure you want convert "'+LandscapeName+'" scenery to file format '+FileFormat+' ?', mtConfirmation,
    [mbYes, mbNo], 0) = mrNo then begin
    Exit;
  end;

  Screen.Cursor := crHourGlass;  // Let user know we're busy...
  try
    // rename files in heightmaps folder
    FilePath := LandscapePath+'\HeightMaps';
    RenameFiles(FilePath, 'h', '.tr3');

    // rename files in forests folder
    FilePath := LandscapePath+'\ForestMaps';
    RenameFiles(FilePath, '', '.for');

    // rename files in textures folder
    FilePath := LandscapePath+'\Textures';
    RenameFiles(FilePath, 't', '.dds');

    // rename files in autogen folder
    if (DirectoryExists(LandscapePath+'\AutoGen')) then begin
      FilePath := LandscapePath+'\AutoGen';
      RenameFiles(FilePath, 'o', '.c3d');
    end;

    // rename files in 22.5m folder
    if (DirectoryExists(LandscapePath+'\HeightMaps\22.5m')) then begin
      FilePath := LandscapePath+'\HeightMaps\22.5m';
      RenameFiles(FilePath, 'h', '.tr3f');
    end;

  finally
    Screen.Cursor := crDefault;  // no longer busy
  end;
  beep;
  MessageShow(LandscapeName + ' converted');

end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_To_XXXYYYClick(Sender: TObject);
begin
  FormatConvert(xxxyyy);
end;

//-------------------------------------------------------------------------------------
procedure TForm_Utilities.Button_To_XXYYClick(Sender: TObject);
begin
  FormatConvert(xxyy);
end;

//-------------------------------------------------------------------------------------
begin
end.


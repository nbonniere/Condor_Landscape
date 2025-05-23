{
 * Unit_Main.pas
 * Copyright (C) 2012- Nick Bonni�re
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
unit Unit_Main;

//===========================================================================
INTERFACE

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, Registry, Menus, ExtCtrls, ComCtrls, CheckLst, Dialogs;

//---------------------------------------------------------------------------
// for compile options
{$I Define.pas}

type
  TForm_Main = class(TForm)
    GroupBox_CondorPath: TGroupBox;
    Edit_CondorPath: TEdit;
    Button_CondorPath: TButton;
    Memo_Info: TMemo;
    Button_KML: TButton;
    MainMenu_main: TMainMenu;
    Menu_About: TMenuItem;
    Button_GMID: TButton;
    GroupBox_Grid: TGroupBox;
    Label_Rows: TLabel;
    Label_Columns: TLabel;
    GroupBox_TileSize: TGroupBox;
    ComboBox_TileSize: TComboBox;
    GroupBox_Tiles: TGroupBox;
    ComboBox_Single: TComboBox;
    GroupBox_Landscape: TGroupBox;
    ComboBox_Landscape: TComboBox;
    Button_GDAL: TButton;
    Button_Header: TButton;
    Button_Forest: TButton;
    Button_Thermal: TButton;
    Button_EditForest: TButton;
    Button_EditThermal: TButton;
    Button_ExportCalib: TButton;
    Button_OverrideCalib: TButton;
    GroupBox_ZoomLevel: TGroupBox;
    ComboBox_ZoomLevel: TComboBox;
    Button_Gradient: TButton;
    Button_Objects: TButton;
    ProgressBar_Status: TProgressBar;
    GroupBox_Imagery: TGroupBox;
    ComboBox_Imagery: TComboBox;
    Button_ObjectPlace: TButton;
    Button_GEO: TButton;
    Button_Utilities: TButton;
    Button_AirportPlace: TButton;
    GroupBox_Version: TGroupBox;
    ComboBox_Version: TComboBox;
    Button_DEM: TButton;
    GroupBox_GEO: TGroupBox;
    ComboBox_GEO: TComboBox;
    GroupBox_DXT: TGroupBox;
    ComboBox_DXT: TComboBox;
    Button_DDS: TButton;
    Button_Merge: TButton;
    GroupBox_MapType: TGroupBox;
    ComboBox_MapType: TComboBox;
    GroupBox_MapID: TGroupBox;
    ComboBox_MapID: TComboBox;
    Button_SimpleObjects: TButton;
    Label_CondorProg: TLabel;
    Button_GDALpath: TButton;
    Edit_GDALpath: TEdit;
    Label_GDAL_Lib: TLabel;
    Button_TextureCompressorPath: TButton;
    Edit_CompressorPath: TEdit;
    Label_Compressor: TLabel;
    Button_DownloaderPath: TButton;
    Edit_DownloaderPath: TEdit;
    Label_Downloader: TLabel;
    Edit_LEpath: TEdit;
    Button_LEpath: TButton;
    Label_Condor_LE: TLabel;
    Edit_WgetPath: TEdit;
    Button_WgetPath: TButton;
    Label_Wget: TLabel;
    Label_7zip: TLabel;
    Edit_7zipPath: TEdit;
    Button_7zipPath: TButton;
    GroupBox_FileNameFormat: TGroupBox;
    ComboBox_FileNameFormat: TComboBox;
    SaveDialog_File: TSaveDialog;
    OpenDialog_File: TOpenDialog;
    procedure Button_CondorPathClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button_KMLClick(Sender: TObject);
    procedure Button_GMIDClick(Sender: TObject);
    procedure Menu_AboutClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button_GDALClick(Sender: TObject);
    procedure Button_GEOClick(Sender: TObject);
    procedure Button_HeaderClick(Sender: TObject);
    procedure Button_GDALpathClick(Sender: TObject);
    procedure Button_ForestClick(Sender: TObject);
    procedure Button_ThermalClick(Sender: TObject);
    procedure Button_ExportCalibClick(Sender: TObject);
    procedure Button_OverrideCalibClick(Sender: TObject);
    procedure Button_EditForestClick(Sender: TObject);
    procedure Button_EditThermalClick(Sender: TObject);
    procedure Button_GradientClick(Sender: TObject);
    procedure CleanUp(Sender: TObject);
    procedure MakeFolderList(List:Tstrings; Path, Mask : string);
    procedure ComboBox_LandscapeChange(Sender: TObject);
    procedure Button_ObjectsClick(Sender: TObject);
    procedure Button_ObjectPlaceClick(Sender: TObject);
    procedure MakeDummyAirportList(FilePath,Filename:string);
    procedure xMakeDummyAirportList(FilePath,Filename:string);
    procedure Button_UtilitiesClick(Sender: TObject);
    procedure Button_AirportPlaceClick(Sender: TObject);
    function  GetTileFile(TileName:string):boolean;
    function  GetAlternateTileFile(TileName:string):boolean;
    function  IdentifyFiles(TileName:String;TileIndex:integer):boolean;
    procedure Button_DEMClick(Sender: TObject);
    procedure Button_TextureCompressorPathClick(Sender: TObject);
    procedure Button_DDSClick(Sender: TObject);
    procedure Button_MergeClick(Sender: TObject);
    procedure Button_DownloaderPathClick(Sender: TObject);
    procedure Button_SimpleObjectsClick(Sender: TObject);
    procedure Button_LEpathClick(Sender: TObject);
    procedure Button_7zipPathClick(Sender: TObject);
    procedure Button_WgetPathClick(Sender: TObject);
    procedure ComboBox_VersionChange(Sender: TObject);
    procedure ComboBox_FileNameFormatChange(Sender: TObject);
    procedure Edit_CondorPathEnter(Sender: TObject);
    procedure Edit_CondorPathExit(Sender: TObject);
    procedure Edit_CondorPathKeyPress(Sender: TObject; var Key: Char);
    procedure Edit_LEpathEnter(Sender: TObject);
    procedure Edit_LEpathExit(Sender: TObject);
    procedure Edit_LEpathKeyPress(Sender: TObject; var Key: Char);
    procedure Edit_GDALpathEnter(Sender: TObject);
    procedure Edit_GDALpathExit(Sender: TObject);
    procedure Edit_GDALpathKeyPress(Sender: TObject; var Key: Char);
    procedure Edit_7zipPathEnter(Sender: TObject);
    procedure Edit_7zipPathKeyPress(Sender: TObject; var Key: Char);
    procedure Edit_7zipPathExit(Sender: TObject);
    procedure Edit_WgetPathEnter(Sender: TObject);
    procedure Edit_WgetPathKeyPress(Sender: TObject; var Key: Char);
    procedure Edit_WgetPathExit(Sender: TObject);
    procedure Edit_DownloaderPathEnter(Sender: TObject);
    procedure Edit_DownloaderPathKeyPress(Sender: TObject; var Key: Char);
    procedure Edit_DownloaderPathExit(Sender: TObject);
    procedure Edit_CompressorPathEnter(Sender: TObject);
    procedure Edit_CompressorPathKeyPress(Sender: TObject; var Key: Char);
    procedure Edit_CompressorPathExit(Sender: TObject);
    procedure ComboBox_ZoomLevelExit(Sender: TObject);
    procedure ComboBox_TileSizeExit(Sender: TObject);
    procedure Button_WarpCropClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FIniFile: TRegIniFile;
  end;

var
  Form_Main: TForm_Main;

//===========================================================================
IMPLEMENTATION

{$R *.DFM}

uses
  FileCtrl, Math,
  Unit_About, Unit_ObjectPlacer, Unit_AirportPlacer, Unit_Utilities,
  Unit_DEM, Unit_Merge, Unit_Graphics, Unit_Objects, Unit_SimpleObjects,
  Unit_Help, u_MakeGradient, Unit_WarpCrop,
  u_MakeDDS, u_MakeKML, u_MakeGMID, u_makeGDAL, u_makeGEO,
  u_MakeForest, u_MakeThermal,
  u_TileList, u_Util, u_SceneryHdr, u_GMIDlog, u_BMP,
  u_Terrain, u_Forest, u_Thermal, u_UTM,
  u_X_CX, {u_CalibImport,} u_LandsatMet,
  {u_CalibExport,} u_Object, u_CUP, u_Airspace, u_INI, u_DXT,
  u_Airport, u_ReduceColors, u_Tile_XYZ, u_TIFF,
  u_BrowseFolder{, u_FTR};

const
  ShortPathNameLength = 50;

var
  CondorPathName : string;
  GDALpathName : string;
  CompressorPathName : string;
  DownloaderPathName : string;
  CondorLEpathName : string;
  WgetPathName : string;
  sZipPathName : string;

  CondorLandscapeName : string;
  LandscapePathName : string;
  ApplicationPathName : string;
  WorkingPathName : string;
  TileName : string;

//---------------------------------------------------------------------------
function LandscapeSelected : boolean;
begin
  if (CondorLandscapeName <> '') then begin
    LandscapeSelected := true;
  end else begin
    LandscapeSelected := false;
    Form_Main.Memo_Info.Lines.Add('Need to select a landscape first');
    Beep;
  end;
end;

//---------------------------------------------------------------------------
function LandscapeOpened : boolean;
begin
    if ((HeaderOpen) AND (TileOpen)) then begin
    LandscapeOpened := true;
  end else begin
    LandscapeOpened := false;
    Form_Main.Memo_Info.Lines.Add('Need to read Header file first');
    Beep;
  end;
end;

//-----------------------------------------------------------------------------
procedure SearchForLandsatFiles;
var
  SearchRec: TSearchRec;
  SearchRec2: TSearchRec;
  Path, Mask : string;

begin
  FileCount := 0;
  //search for folders that start with 'Path' in working\Landsat folder
  //if found look for .met file and extract Lat/Long extents
  //to plot in overall KML file
  Path := WorkingPathName+'\Landsat';
  Mask := '\Path*.*';
  if (FindFirst(Path+Mask, faDirectory {faAnyFile}, SearchRec)) = 0 then begin
    if ((SearchRec.Name <> '.') AND (SearchRec.Name <> '..')) then begin
      Form_Main.Memo_Info.Lines.Add(SearchRec.Name);
      SetLength(LandsatMetFiles,FileCount+1);
      LandsatMetFiles[FileCount].Folder := SearchRec.Name;
      if (FindFirst(Path+'\'+SearchRec.Name+'\*.met', faAnyFile, SearchRec2)) = 0 then begin
        LandsatMetFiles[FileCount].MetName := SearchRec2.Name;
        ReadLandsatTileCoords(Path+'\'+SearchRec.Name+'\'+SearchRec2.Name);
        if (MetOpen) then begin
          INC(FileCount);
        end;
      end;
    end;

    while (FindNext(SearchRec) = 0) do begin
      if ((SearchRec.Name <> '.') AND (SearchRec.Name <> '..')) then begin
        Form_Main.Memo_Info.Lines.Add(SearchRec.Name);
        SetLength(LandsatMetFiles,FileCount+1);
        LandsatMetFiles[FileCount].Folder := SearchRec.Name;
        if (FindFirst(Path+'\'+SearchRec.Name+'\*.met', faAnyFile, SearchRec2)) = 0 then begin
          LandsatMetFiles[FileCount].MetName := SearchRec2.Name;
          ReadLandsatTileCoords(Path+'\'+SearchRec.Name+'\'+SearchRec2.Name);
          if (MetOpen) then begin
            INC(FileCount);
          end;
        end;
      end;
    end;
    FindClose(SearchRec);
  end;
end;

//-----------------------------------------------------------------------------
procedure TForm_Main.MakeFolderList(List:Tstrings; Path, Mask : string);
var
  SearchRec: TSearchRec;

begin
  List.Clear;  //clear the list
  if (FindFirst(Path+Mask, faDirectory {faAnyFile}, SearchRec)) = 0 then begin
    if ((SearchRec.Name <> '.') AND (SearchRec.Name <> '..')) then begin
      List.Append(SearchRec.Name);
    end;

    while (FindNext(SearchRec) = 0) do begin
      if ((SearchRec.Name <> '.') AND (SearchRec.Name <> '..')) then begin
        List.Append(SearchRec.Name);
      end;
    end;
    FindClose(SearchRec);
  end;
end;

//---------------------------------------------------------------------------
procedure MakeWorkingPath;
begin
  if ((CondorPathName <> '') AND (CondorLandscapeName <> '')) then begin
    LandscapePathName := CondorPathName + '\Landscapes\' + CondorLandscapeName;
    WorkingPathName := LandscapePathName + '\Working';
  end else begin
    WorkingPathName := '';
  end;  
end;

//---------------------------------------------------------------------------
procedure TForm_Main.ComboBox_LandscapeChange(Sender: TObject);
begin
  if (CondorLandscapeName <> ComboBox_Landscape.Text) then begin
    CondorLandscapeName := ComboBox_Landscape.Text;
    FIniFile.WriteString('Paths','CondorLandscape',CondorLandscapeName);
    MakeWorkingPath;
    ComboBox_Single.Clear;
    HeaderOpen := false;
    TerrainOpen := false;
    TileOpen := false;
    Memo_Info.Lines.Clear;
    Unit_SimpleObjects.objFolderOpen := false;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Edit_CondorPathEnter(Sender: TObject);
begin
  if (Edit_CondorPath.Text <> CondorPathName) then begin
    Edit_CondorPath.Text := CondorPathName;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Edit_CondorPathKeyPress(Sender: TObject;
  var Key: Char);
begin
  if ord(Key) = VK_RETURN then
  begin
    Key := #0; // prevent beeping
    Button_CondorPath.SetFocus; // create an exit, by changing focus
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Edit_CondorPathExit(Sender: TObject);
begin
  if (Edit_CondorPath.Text <> CondorPathName) then begin
    CondorPathName := Edit_CondorPath.Text;
    Edit_CondorPath.Hint := CondorPathName;
    FIniFile.WriteString('Paths','CondorPath',CondorPathName);

    CondorLandscapeName := ''; {default}
    ComboBox_Landscape.ItemIndex := -1;
    FIniFile.WriteString('Paths','CondorLandscape',CondorLandscapeName);
    MakeFolderList(ComboBox_Landscape.Items, CondorPathName+'\Landscapes', '\*.*');
    MakeWorkingPath;
    ComboBox_Single.Clear;
    HeaderOpen := false;
    TerrainOpen := false;
    TileOpen := false;
  end;
  Edit_CondorPath.Text := ShortenFolderString(CondorPathName,ShortPathNameLength);
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_CondorPathClick(Sender: TObject);

var S : String;

begin
  Edit_CondorPathEnter(Sender);
  S := BrowseForFolder('Condor program folder',CondorPathName);
  if ((S <> '') AND (S <> CondorPathName)) then begin
    Edit_CondorPath.Text := S;
  end;
  Edit_CondorPathExit(Sender);
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Edit_LEpathEnter(Sender: TObject);
begin
  if (Edit_LEpath.Text <> CondorLEpathName) then begin
    Edit_LEpath.Text := CondorLEpathName;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Edit_LEpathKeyPress(Sender: TObject; var Key: Char);
begin
  if ord(Key) = VK_RETURN then
  begin
    Key := #0; // prevent beeping
    Button_LEpath.SetFocus; // create an exit, by changing focus
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Edit_LEpathExit(Sender: TObject);
begin
  if (Edit_LEpath.Text <> CondorLEpathName) then begin
    CondorLEpathName := Edit_LEpath.Text;
    Edit_LEpath.Hint := CondorLEpathName;
    FIniFile.WriteString('Paths','CondorLEpath',CondorLEpathName);
  end;
  Edit_LEpath.Text := ShortenFolderString(CondorLEpathName,ShortPathNameLength);
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_LEpathClick(Sender: TObject);
var
  S : String;

begin
  Edit_LEpathEnter(Sender);
  S := BrowseForFolder('Condor Landscape Editor folder',CondorLEpathName);
  if ((S <> '') AND (S <>CondorLEpathName)) then begin
    Edit_LEpath.Text := S;
  end;
  Edit_LEpathExit(Sender);
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Edit_GDALpathEnter(Sender: TObject);
begin
  if (Edit_GDALpath.Text <> GDALpathName) then begin
    Edit_GDALpath.Text := GDALpathName;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Edit_GDALpathKeyPress(Sender: TObject; var Key: Char);
begin
  if ord(Key) = VK_RETURN then
  begin
    Key := #0; // prevent beeping
    Button_GDALpath.SetFocus; // create an exit, by changing focus
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Edit_GDALpathExit(Sender: TObject);
begin
  if (Edit_GDALpath.Text <> GDALpathName) then begin
    GDALpathName := Edit_GDALpath.Text;
    Edit_GDALpath.Hint := GDALpathName;
    FIniFile.WriteString('Paths','GDALpath',GDALpathName);
  end;
  Edit_GDALpath.Text := ShortenFolderString(GDALpathName,ShortPathNameLength);
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_GDALpathClick(Sender: TObject);
var
  S : String;

begin
  Edit_GDALpathEnter(Sender);
  S := BrowseForFolder('GDAL library folder',GDALpathName);
  if ((S <> '') AND (S <> GDALpathName)) then begin
    Edit_GDALpath.Text := S;
  end;
  Edit_GDALpathExit(Sender);
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Edit_CompressorPathEnter(Sender: TObject);
begin
  if (Edit_CompressorPath.Text <> CompressorPathName) then begin
    Edit_CompressorPath.Text := CompressorPathName;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Edit_CompressorPathKeyPress(Sender: TObject;
  var Key: Char);
begin
  if ord(Key) = VK_RETURN then
  begin
    Key := #0; // prevent beeping
    Button_TextureCompressorPath.SetFocus; // create an exit, by changing focus
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Edit_CompressorPathExit(Sender: TObject);
begin
  if (Edit_CompressorPath.Text <> CompressorPathName) then begin
    CompressorPathName := Edit_CompressorPath.Text;
    Edit_CompressorPath.Hint := CompressorPathName;
    FIniFile.WriteString('Paths','CompressorPath',CompressorPathName);
  end;
  Edit_CompressorPath.Text := ShortenFolderString(CompressorPathName,ShortPathNameLength);
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_TextureCompressorPathClick(Sender: TObject);
var
  S : String;

begin
  Edit_CompressorPathEnter(Sender);
  S := BrowseForFolder('Texture compressor folder',CompressorPathName);
  if ((S <> '') AND (S <> CompressorPathName)) then begin
    Edit_CompressorPath.Text := S;
  end;
  Edit_CompressorPathExit(Sender);
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Edit_7zipPathEnter(Sender: TObject);
begin
  if (Edit_7zipPath.Text <> sZIPpathName) then begin
    Edit_7zipPath.Text := sZIPpathName;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Edit_7zipPathKeyPress(Sender: TObject; var Key: Char);
begin
  if ord(Key) = VK_RETURN then
  begin
    Key := #0; // prevent beeping
    Button_7zipPath.SetFocus; // create an exit, by changing focus
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Edit_7zipPathExit(Sender: TObject);
begin
  if (Edit_7zipPath.Text <> sZIPpathName) then begin
    sZIPpathName := Edit_7zipPath.Text;
    Edit_7zipPath.Hint := sZIPpathName;
    FIniFile.WriteString('Paths','7zipPath',sZIPpathName);
  end;
  Edit_7zipPath.Text := ShortenFolderString(sZIPpathName,ShortPathNameLength);
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_7zipPathClick(Sender: TObject);
var
  S : String;

begin
  Edit_7zipPathEnter(Sender);
  S := BrowseForFolder('7-zip Archiver folder',sZIPpathName);
  if ((S <> '') AND (S <> sZIPpathName)) then begin
    Edit_7zipPath.Text := S;
  end;
  Edit_7zipPathExit(Sender);
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Edit_WgetPathEnter(Sender: TObject);
begin
  if (Edit_WgetPath.Text <> WgetPathName) then begin
    Edit_WgetPath.Text := WgetPathName;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Edit_WgetPathKeyPress(Sender: TObject; var Key: Char);
begin
  if ord(Key) = VK_RETURN then
  begin
    Key := #0; // prevent beeping
    Button_WGETpath.SetFocus; // create an exit, by changing focus
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Edit_WgetPathExit(Sender: TObject);
begin
  if (Edit_GDALpath.Text <> WgetPathName) then begin
    WgetPathName := Edit_WgetPath.Text;
    Edit_WgetPath.Hint := WgetPathName;
    FIniFile.WriteString('Paths','WgetPath',WgetPathName);
  end;
  Edit_WgetPath.Text := ShortenFolderString(WgetPathName,ShortPathNameLength);
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_WgetPathClick(Sender: TObject);
var
  S : String;

begin
  Edit_WGETpathEnter(Sender);
  S := BrowseForFolder('Wget downloader folder',WgetPathName);
  if ((S <> '') AND (S <> WgetPathName)) then begin
    Edit_WGETpath.Text := S;
  end;
  Edit_WGETpathExit(Sender);
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Edit_DownloaderPathEnter(Sender: TObject);
begin
  if (Edit_DownloaderPath.Text <> DownloaderPathName) then begin
    Edit_DownloaderPath.Text := DownloaderPathName;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Edit_DownloaderPathKeyPress(Sender: TObject;
  var Key: Char);
begin
  if ord(Key) = VK_RETURN then
  begin
    Key := #0; // prevent beeping
    Button_DownloaderPath.SetFocus; // create an exit, by changing focus
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Edit_DownloaderPathExit(Sender: TObject);
var
  Ext_File : TextFile;
  fExt : string;

begin
  if (Edit_DownloaderPath.Text <> DownloaderPathName) then begin
    DownloaderPathName := Edit_DownloaderPath.Text;
    Edit_DownloaderPath.Hint := DownloaderPathName;

    if (FileExists(Edit_DownloaderPath.Text+'\INI\t.txt')) then begin
      AssignFile(Ext_File,Edit_DownloaderPath.Text+'\INI\t.txt');
      Reset(Ext_File);
      readln(Ext_File,fExt);
      ComboBox_MapType.Text := fExt;
      CloseFile(Ext_File);
    end;

    FIniFile.WriteString('Paths','DownloaderPath',DownloaderPathName);
  end;
  Edit_DownloaderPath.Text := ShortenFolderString(DownloaderPathName,ShortPathNameLength);
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_DownloaderPathClick(Sender: TObject);
var
  S : String;
begin
  Edit_DownloaderPathEnter(Sender);
  S := BrowseForFolder('Downloader/Combiner folder',DownloaderPathName);
  if ((S <> '') AND (S <> DownloaderPathName)) then begin
    Edit_DownloaderPath.Text := S;
  end;
  Edit_DownloaderPathExit(Sender);
end;

//---------------------------------------------------------------------------
procedure TForm_Main.xMakeDummyAirportList(FilePath,Filename:string);
var
  i,j : integer;

//---------------------------------------------------------------------------
procedure MakeDummyAirport(Index, Index2 : integer);
begin
  SetLength(Airport_List,Airport_Count+1);
  with Airport_List[Airport_Count] do begin
    apName := TileList[Index].TileName;
    apLatitude := (TileList[Index].TileLatBottom + TileList[Index2].TileLatBottom)/2;
    apLongitude := (TileList[Index].TileLongRight + TileList[Index2].TileLongRight)/2;
    apAltitude := 0.0;
    apDirection := 0;
    apLength := 1000;
    apWidth := 25;
    apAsphaltFlag := 0;
    apFrequency := 123.5;
    apOptions := 0;
  end;
  INC(Airport_Count);
end;

//---------------------------------------------------------------------------
begin
  if (TileOpen) then begin
    Airport_Count := 0;
    ProgressBar_Status.Max := (TileRowCount)*(TileColumnCount);
    for i := 0 to TileRowCount-1 do begin
      for j := 0 to TileColumnCount-1 do begin
        MakeDummyAirport(i*(TileColumnCount+1)+j,(i+1)*(TileColumnCount+1)+(j+1));
        ProgressBar_Status.StepIt;
        Application.ProcessMessages;
      end;
    end;
    Memo_Info.Lines.Add(format('%d dummy centre tile airports %s created',[TileRowCount*TileColumnCount,FileName]));
    ProgressBar_Status.Position := 0;
  end;
  Airport_FolderName := FilePath;
  Airport_FileName := FileName;
  WriteAirportFile;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.MakeDummyAirportList(FilePath,Filename:string);
begin
  if (TileOpen) then begin
    Airport_Count := 0;
    SetLength(Airport_List,Airport_Count+1);
    with Airport_List[Airport_Count] do begin
      apName := 'Center';
      apLatitude := scCentre.TileLatBottom;
      apLongitude := scCentre.TileLongRight;
      apAltitude := 0.0;
      apDirection := 0;
      apLength := 1000;
      apWidth := 25;
      apAsphaltFlag := 0;
      apFrequency := 123.5;
      apOptions := 0;
    end;
    INC(Airport_Count);
    Memo_Info.Lines.Add(format('dummy centre airport %s created',[FileName]));
  end;
  Airport_FolderName := FilePath;
  Airport_FileName := FileName;
  WriteAirportFile;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_HeaderClick(Sender: TObject);
var
  CurrentRow, CurrentColumn : integer;
//  SS: string;
//  SSf : double;
//  ErrorCode :integer;
  ItemCount : integer;
  FileName : string;

begin
  Memo_Info.Lines.Clear;

  if (LandscapeSelected) then begin
    u_SceneryHDR.Memo_Message := Memo_Info;
    ReadSceneryHeader(WorkingPathName+'\'+CondorLandscapeName+'.hdr'); // tile centre or corner ?

    // added here for now - default to 90 m
    cResolution := 90;     // typically 90m
    cDeltaX := -90;        // actual horizontal resolution in m (calibrated)
    cDeltaY := 90;         // actual vertical resolution in m (calibrated)

 //  comment out for now - resolve substracting resolution/2 or not first !!!
 //  if (NOT HeaderOpen) then begin
 //    ReadSceneryHeader(WorkingPathName+'\DEM\scenery.hdr'); // tile centre or corner ?

// Note - use terrain header which contains the calibration scale applied for Condor !
// the scenery header does not have this information.
// this is needed when calibration has been applied and needs to be used for airport placement !

    if (NOT HeaderOpen) then begin
      // try terrain header instead
      u_Terrain.Memo_Message := Memo_Info;
      ReadTerrainHeader(WorkingPathName+'\..\'+CondorLandscapeName+'.trn'); // tile centre or corner ?
      if (TerrainOpen) then begin
        // use terrain information
        with TerrainHeader do begin
//          UTM_Zone := format('%d',[tUTMzone]);
          UTM_Zone := tUTMzone;
          // convert UTM grid to N/S if needed - ambiguous - cannot be determined 100%
          // if not in ('N', 'S') then must be V1, else could be V1 or V2
          if ( NOT (tUTMgrid[0] IN ['N', 'S']) ) then begin
            UTM_ZoneNS := UTMgridConvert(tUTMgrid[0]);
            ComboBox_Version.text := 'V1';
          end else begin
            if (DirectoryExists(WorkingPathName+'\..\HeightMaps')) then begin
              ComboBox_Version.text := 'V2';
            end;
            if (FileExists(WorkingPathName+'\..\HeightMaps\h000000.tr3')) then begin
              ComboBox_Version.text := 'V3';
            end else begin
              if (FileExists(WorkingPathName+'\..\'+CondorLandscapeName+'.air')) then begin
                ComboBox_Version.text := 'V3';
              end;
            end;
          end;
          if (ComboBox_Version.text = 'V1') then begin
            UTM_ZoneNS := UTMgridConvert(tUTMgrid[0]);
          end else begin
            UTM_ZoneNS := tUTMgrid[0];
          end;
          // assume corner and calc centre - possible problem here
//          UTM_Right := tRightMapEasting - round(tResolution/2);     //90m tile centre
          UTM_Right := tRightMapEasting - Legacy_Offset;     //90m tile centre
//          UTM_Bottom := tBottomMapNorthing + round(tResolution/2);  //90m tile centre
          UTM_Bottom := tBottomMapNorthing + Legacy_Offset;  //90m tile centre
          UTM_Left := UTM_Right - tWidth * tResolution;
          UTM_Top := UTM_Bottom + tHeight * tResolution;
          RowCount := tHeight;
          ColumnCount := tWidth;
          // added here for now
          cResolution := tResolution;     // typically 90m
          cDeltaX := tDeltaX;             // actual horizontal resolution in m (calibrated)
          cDeltaY := tDeltaY;             // actual vertical resolution in m (calibrated)
          // adjust FileNameFormat as needed
          ComboBox_VersionChange(Sender);
        end;
        // information is now valid
        HeaderOpen := true;
        // cannot override terrain file since it is used
        Button_OverrideCalib.enabled := false;
      end;
    end else begin // scenery header, not terrain header
      Button_OverrideCalib.enabled := true;
      if (DEM_Res = 30) then begin
        ComboBox_Version.text := 'V2';
      end else begin
        ComboBox_Version.text := 'V1';
      end;
    end;

//  end;

    if (HeaderOpen) then begin
//      {u_TileList.}TileRowCount := RowCount div 256;
//      {u_TileList.}TileColumnCount := ColumnCount div 256;
      {u_TileList.}TileRowCount := Ceil(RowCount / 256);        // round up for partial tiles
      {u_TileList.}TileColumnCount := Ceil(ColumnCount / 256);  // round up for partial tiles
      Label_Rows.Caption := format('Rows:     %3.2f',[RowCount/256]);
      Label_Columns.Caption := format('Columns: %3.2f',[ColumnCount/256]);

      TileOpen := false;
      u_TileList.ProgressBar_Status := ProgressBar_Status;
      // create a table of lat/long and UTM coord for each tile bottom-right
      MakeTileList(UTM_Right+Legacy_Offset, UTM_Bottom-Legacy_Offset);

      // add blank for 'all'
      ComboBox_Single.Items.append('');

      // add overall map
      ComboBox_Single.Items.append('Overall');

      //add to combo list in column:row order
      for CurrentColumn := 0 to TileColumnCount-1 do begin
        for CurrentRow := 0 to TileRowCount-1 do begin
          ComboBox_Single.Items.append(
            TileList[CurrentRow*(TileColumnCount+1)+CurrentColumn].TileName);
        end;
      end;
      // adjust size
      ItemCount := ComboBox_Single.Items.Count;
      if (ItemCount > 16) then begin
        ItemCount:= 16;
      end;
      ComboBox_Single.DropDownCount := ItemCount;

      // check if landscape being constructed
      if (DirectoryExists(WorkingPathName)) then begin
        if (NOT FileExists(WorkingPathName+'\d'+CondorLandscapeName+'.apt')) then begin
          MakeDummyAirportList(WorkingPathName,'\d'+CondorLandscapeName+'.apt');
        end;
        if (NOT FileExists(WorkingPathName+'\..\'+CondorLandscapeName+'.apt')) then begin
          MakeDummyAirportList(WorkingPathName,'\..\'+CondorLandscapeName+'.apt');
        end;
        if (NOT FileExists(WorkingPathName+'\..\'+CondorLandscapeName+'.ini')) then begin
          MakeDummyINI(WorkingPathName,'..\'+CondorLandscapeName+'.ini');
        end;
        if (NOT FileExists(WorkingPathName+'\..\'+CondorLandscapeName+'.cup')) then begin
          MakeDummyCUP(WorkingPathName,'..\'+CondorLandscapeName+'.cup');
        end;
        if (NOT FileExists(WorkingPathName+'\..\'+CondorLandscapeName+'.obj')) then begin
          MakeDummyFile(WorkingPathName,'\..\'+CondorLandscapeName+'.obj');
        end;
        if (NOT FileExists(WorkingPathName+'\..\'+CondorLandscapeName+'.tdm')) then begin
          MakeThermal_Blank(WorkingPathName+'\..\'+CondorLandscapeName+'.tdm', ColumnCount, RowCount, 192);
        end;
        FileName := WorkingPathName+'\..\'+CondorLandscapeName+'.bmp';
        if (NOT FileExists(FileName)) then begin
// too          TRN_To_Color_Bitmap(WorkingPathName+'\..\'+CondorLandscapeName+'.trn',
// slow           WorkingPathName+'\..\'+CondorLandscapeName+'.bmp');
          MakeGEO_Blank_24bit(FileName, ColumnCount, RowCount, 192);
          // bmp for V3 needs to be 32 bit color RGBA
          if (ComboBox_Version.text = 'V3') then begin //TBD
            RenameFile(FileName, FileName+'.bmp');
            Bitmap_24_To_Bitmap_32(FileName+'.bmp', FileName, true, 1.0);
            DeleteFile(FileName+'.bmp');
          end;
        end;
        // dummy \Forestmaps\*.for  not needed, .tha needed, plane not controllable
//        if (NOT FileExists(WorkingPathName+'\..\Textures\t0000.dds')) then begin
        if (NOT FileExists(WorkingPathName+'\..\Textures\t'+MakeTileName(0,0, TileNameMode)+'.dds')) then begin
          ForceDirectories(WorkingPathName+'\..\Textures');
          DXT_MakeEmpty(WorkingPathName+'\..\Textures\t'+MakeTileName(0,0, TileNameMode)+'.dds');
        end;
        if (ComboBox_Version.text = 'V3') then begin
          if (NOT FileExists(WorkingPathName+'\..\'+CondorLandscapeName+'.air')) then begin
            MakeDummy_AIR(WorkingPathName,'..\'+CondorLandscapeName+'.air');
          end;
        end;
      end;

      // to clear data if already opened
      Unit_AirportPlacer.CurrentLandscape := '';
      Unit_ObjectPlacer.CurrentLandscape := '';

      // to clean up old files
      //CleanUp(Sender);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure xMakeOverallKML;
begin
  if (Form_Main.ComboBox_Imagery.Text = 'Landsat') then begin
    //add landsat tiles to overall KML
    u_LandsatMet.Memo_Message := Form_Main.Memo_Info;
    SearchForLandsatFiles;
  end;  
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_KMLClick(Sender: TObject);

var
  i, j : integer;

begin
  Memo_Info.Clear;

  if (LandscapeOpened) then begin
    // create KML files
    u_MakeKML.Memo_Message := Memo_Info;
    u_MakeKML.KMLfolder := WorkingPathName;

    // Clean up !
    if (Form_Main.ComboBox_Imagery.Text = 'Landsat') then begin
      //add landsat tiles to overall KML
      u_LandsatMet.Memo_Message := Form_Main.Memo_Info;
      SearchForLandsatFiles;
    end;
    ProgressBar_Status.Max := (TileRowCount)*(TileColumnCount);
    for i := 0 to TileRowCount-1 do begin
      for j := 0 to TileColumnCount-1 do begin
        MakeKML(i*(TileColumnCount+1)+j);
        ProgressBar_Status.StepIt;
        Application.ProcessMessages;
      end;
    end;
    Memo_Info.Lines.Add(format('All done, %d tiles created',[TileRowCount*TileColumnCount]));
    ProgressBar_Status.Position := 0;

    OverallFolder := 'SourceTiles\Overall';
    // Clean up !
    // ??? use same calcs as WriteTileRange()
    MakeOverallKML(0,
      0*(TileColumnCount+1)+(TileColumnCount-1),
      (TileRowCount-1)*(TileColumnCount+1)+0,
      (TileRowCount-1)*(TileColumnCount+1)+(TileColumnCount-1));

  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.CleanUp(Sender: TObject);

var
  i, j : integer;
  TileIndex : integer;

begin
  if (HeaderOpen) then begin
    for i := 0 to TileRowCount-1 do begin
      for j := 0 to TileColumnCount-1 do begin
        TileIndex := i*(TileColumnCount+1)+j;
        DeleteFile(WorkingPathName+'\SourceTiles\'
          + TileList[TileIndex].TileName +'\'
          + TileList[TileIndex].TileName
          + '\bigmap.bmp');
        DeleteFile(WorkingPathName+'\SourceTiles\'
          + TileList[TileIndex].TileName +'\'
          + TileList[TileIndex].TileName
          + '\bigmap.tif');
        DeleteFile(WorkingPathName+'\SourceTiles\'
          + TileList[TileIndex].TileName +'\'
          + TileList[TileIndex].TileName
          + '\UTMmap.tif');
        DeleteFile(WorkingPathName+'\SourceTiles\'
          + TileList[TileIndex].TileName +'\'
          + TileList[TileIndex].TileName
          + '.bmp');
      end;
    end;
  end else begin
    Memo_Info.Lines.Add('Need Header file first');
    Beep;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_GMIDClick(Sender: TObject);

var
  ErrorCode : integer;
  i, j : integer;
  TileIndex : integer;
  TileRow, TileColumn : integer;

begin
  Memo_Info.Clear;

  if (LandscapeOpened) then begin

    if (NOT u_MakeGMID.OpenDLL) then begin
      Memo_Info.Lines.Add('Unable to Open DLL to create symbolic links');
//      Beep; Exit;
      Memo_Info.Lines.Add('Defaulting to folders');
    end;

    u_MakeGMID.Memo_Message := Memo_Info;
    u_MakeGMID.GMIDfolder := WorkingPathName;
    u_MakeGMID.GMIDProgramsfolder := DownloaderPathName;
    u_MakeGMID.GMIDMapID := ComboBox_MapID.Text;
    u_MakeGMID.GMIDMapType := ComboBox_MapType.Text;
    TileName := ComboBox_Single.text;

    if (TileName = 'Overall') then begin // only overall tile
      MakeGMIDoverallProjectFile;
    end else begin
      // first confirm the zoom level
      u_MakeGMID.ZoomLevel := ComboBox_ZoomLevel.text;
      if MessageDlg('Proceed with zoom level "' + u_MakeGMID.ZoomLevel + '" ?', mtConfirmation,
        [mbYes, mbNo], 0) = mrNo then begin
        Exit;
      end;

      if (TileName = '') then begin // default blank -> all
        // create all individual GMID files
        ProgressBar_Status.Max := (TileRowCount)*(TileColumnCount);
        for i := 0 to TileRowCount-1 do begin
          for j := 0 to TileColumnCount-1 do begin
            MakeGMIDprojectFile(false, i*(TileColumnCount+1)+j);
            MakeGMIDprojectFile(true, i*(TileColumnCount+1)+j); // make geid version
            ProgressBar_Status.StepIt;
            Application.ProcessMessages;
          end;
        end;
        Memo_Info.Lines.Add(format('All done, %d tiles created',[TileRowCount*TileColumnCount]));
        ProgressBar_Status.Position := 0;

        // also make an overall map
        MakeGMIDoverallProjectFile;

        // make a batchfile to call all batch files
        MakeGMID_All_BatchFile;
        MakeGMID_All_Combine_BatchFile;

      end else begin // individual tile only
        if (NOT GetTileIndex(TileName,TileColumn, TileRow)) then begin
          Memo_Info.Lines.Add('Select a tile name first');
          Beep;
        end else begin
          TileIndex := TileRow*(TileColumnCount+1)+TileColumn;
          MakeGMIDprojectFile(false, TileIndex);
          MakeGMIDprojectFile(true, TileIndex); // make geid version
        end;
{
        if (length(TileName) <> 4) then begin
           Memo_Info.Lines.Add('Select a tile name first');
           Beep;
        end else begin
          Val(copy(TileName,1,2),TileColumn,ErrorCode);
          Val(copy(TileName,3,2),TileRow,ErrorCode);
  //        if errorcode or not in range -> error
          TileIndex := TileRow*(TileColumnCount+1)+TileColumn;
          MakeGMIDprojectFile(false, TileIndex);
          MakeGMIDprojectFile(true, TileIndex); // make geid version
        end;
}     end;
    end;

    u_MakeGMID.CloseDLL;

  end;
end;

//---------------------------------------------------------------------------
Procedure MakeGeoBatch(TileIndex : integer);
begin
  ForceDirectories(GEOfolder +'\SourceTiles\'+ TileList[TileIndex].TileName);
  Form_Main.Memo_Info.Lines.Add(GEOfolder +'\SourceTiles\'+ TileList[TileIndex].TileName);

  case u_MakeGEO.GeoDatabaseType of
    OSM, CanVec: begin
      MakeGEObatchFile(TileIndex);  // do first to create paths if needed
// still make for now       if (ComboBox_Version.text = 'V1') then begin
        MakeGEO_V1_Forest_batchFile(TileIndex);
//      end else begin
        MakeGEO_V2_Forest_Deciduous_batchFile(TileIndex);
        MakeGEO_V2_Forest_Coniferous_batchFile(TileIndex);
        MakeGEO_V2_Water_batchFile(TileIndex);
//      end;
      MakeGEO_Thermal_batchFile(TileIndex);
    end;
    GLC: begin
      MakeGEO_GLC_batchFile(TileIndex, gGeneric, ggSize); // generic
      MakeGEO_GLC_batchFile(TileIndex, gThermal, gtSize); // thermal
      MakeGEO_GLC_batchFile(TileIndex, gV2decideous, gfV2Size); // forest - deciduous
//      MakeGEO_GLC_batchFile(TileIndex, gV2coniferous, gfV2Size); // forest - conifeous
      // make a blank coniferous file instead
      MakeGEO_GLC_Blank(TileIndex, gV2coniferous, gfV2Size); // forest - conifeous - blank
      MakeGEO_GLC_batchFile(TileIndex, gWater, StrToInt(u_MakeGEO.OutputTileSize)); // water (must be same size as texture)
    end;
    else begin
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_GEOClick(Sender: TObject);

var
  ErrorCode : integer;
  i, j : integer;
  TileIndex : integer;
  TileRow, TileColumn : integer;

begin
  Memo_Info.Clear;

  if (LandscapeOpened) then begin

    if MessageDlg('Proceed with Vector Data "' + ComboBox_GEO.Text + '" ?', mtConfirmation,
      [mbYes, mbNo], 0) = mrNo then begin
      Exit;
    end;

    // create GEO files
    u_MakeGEO.Memo_Message := Memo_Info;
    u_MakeGEO.GEOfolder := WorkingPathName;
    u_MakeGEO.GDALlibraryfolder := GDALpathName;
    u_MakeGEO.WGETfolder := WgetPathName; // for Wget
    u_MakeGEO.ApplicationPath := ApplicationPathName;
    ForceDirectories(WorkingPathName+'\GeoDatabase');
//    u_MakeGEO.OutputTileSize := '1024'; // default for now
    u_MakeGEO.OutputTileSize := ComboBox_TileSize.text;
    if MessageDlg('Proceed with tile size "' + u_MakeGEO.OutputTileSize + '" ?', mtConfirmation,
      [mbYes, mbNo], 0) = mrNo then begin
      Exit;
    end;
    if (ComboBox_GEO.Text = 'GLC') then begin
      u_MakeGEO.GeoDatabaseType := GLC;
    end else begin
      if (ComboBox_GEO.Text = 'CanVec') then begin
        u_MakeGEO.GeoDatabaseType := CanVec;
      end else begin
        u_MakeGEO.GeoDatabaseType := OSM;
      end;
    end;
    u_MakeGEO.Init;

    u_MakeGEO.File_Destination := '..\..\Terragen';
    // create if needed
    ForceDirectories(WorkingPathName+'\Terragen\ForestMaps');
    ForceDirectories(WorkingPathName+'\Terragen\WaterMaps');
    // create a blank bitmap in GEO folder
    MakeGEO_Blank_24bit(GEOFolder+'\GeoDatabase\Blank_256_blk.bmp', 256, 256, 0);
    MakeGEO_Blank_Greyscale(GEOFolder+'\GeoDatabase\Blank_256_wht.bmp', 256, 255);

    TileName := ComboBox_Single.text;
    if (TileName = '') then begin // default blank -> all
      // make all individual batch files
      ProgressBar_Status.Max := (TileRowCount)*(TileColumnCount);
      for i := 0 to TileRowCount-1 do begin
        for j := 0 to TileColumnCount-1 do begin
          TileIndex := i*(TileColumnCount+1)+j;
          MakeGeoBatch(TileIndex);
          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;
      end;
      Memo_Info.Lines.Add(format('All done, %d tiles created',[TileRowCount*TileColumnCount]));
      ProgressBar_Status.Position := 0;

      // make a batchfile to call all batch files
      u_MakeGEO.MakeGEO_GO_batchFile;

    end else begin // only individual tile
      if (NOT GetTileIndex(TileName,TileColumn, TileRow)) then begin
        Memo_Info.Lines.Add('Select a tile name first');
        Beep;
      end else begin
        TileIndex := TileRow*(TileColumnCount+1)+TileColumn;
        MakeGeoBatch(TileIndex);
      end;
{      if (length(TileName) <> 4) then begin
         Memo_Info.Lines.Add('Select a tile name first');
         Beep;
      end else begin
        Val(copy(TileName,1,2),TileColumn,ErrorCode);
        Val(copy(TileName,3,2),TileRow,ErrorCode);
  //     if errorcode or not in range -> error
        TileIndex := TileRow*(TileColumnCount+1)+TileColumn;
        MakeGeoBatch(TileIndex);
      end;
}    end;
    if (ComboBox_GEO.Text = 'GLC') then begin
      // show files needed
      MakeGEO_GLC_Wget;
      MakeGEO_Blank_GreyScale(WorkingPathName+'\Terragen\ForestMaps\sBlank.bmp', gfV2Size, 0);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_GDALClick(Sender: TObject);
var
  ErrorCode : integer;
  TileName : string;
  TileIndex : integer;
  TileRow, TileColumn : integer;
  i, j : integer;
//  FileName : string;

begin
  Memo_Info.Lines.Clear;

//  if (HeaderOpen) AND (TileOpen) then begin
  if (LandscapeOpened) then begin

    u_MakeGDAL.Memo_Message := Memo_Info;
    u_MakeGDAL.GDALfolder := WorkingPathName;
    u_MakeGDAL.GDALlibraryfolder := GdalpathName;
//    u_MakeGDAL.CondorFolder := CondorPathName;
 //   u_MakeGDAL.CompressorFolder := CompressorPathName;
 //   u_MakeGDAL.DXT_Type := ComboBox_DXT.Text;

    if (ComboBox_Version.text <> 'V1') then begin
      u_MakeGDAL.File_Destination := '\Terragen\Textures';
    end else begin
      u_MakeGDAL.File_Destination := '\Terragen';
    end;
    // create if needed
    ForceDirectories(WorkingPathName+u_MakeGDAL.File_Destination);
    ForceDirectories(WorkingPathName+u_MakeGDAL.File_Destination+'_DetectTree');
    u_MakeGDAL.File_Destination := '..\..'+u_MakeGDAL.File_Destination;

    if (ComboBox_Imagery.Text = 'Landsat') then begin
      ProgressBar_Status.Max := (TileRowCount)*(TileColumnCount);
      for i := 0 to TileRowCount-1 do begin
        for j := 0 to TileColumnCount-1 do begin
          MakeLandsatGDALbatchFile(i*(TileColumnCount+1)+j);
          ProgressBar_Status.StepIt;
          Application.ProcessMessages;
        end;
      end;
      Memo_Info.Lines.Add(format('All done, %d tiles created',[TileRowCount*TileColumnCount]));
      ProgressBar_Status.Position := 0;

    end else begin // Tiles
      TileName := ComboBox_Single.text;
      if (TileName = '') then begin // default blank -> all
        // first confirm the zoom level (needed for geid)
        u_MakeGDAL.ZoomLevel := ComboBox_ZoomLevel.text;
        if MessageDlg('Proceed with zoom level "' + u_MakeGDAL.ZoomLevel + '" ?', mtConfirmation,
          [mbYes, mbNo], 0) = mrNo then begin
          Exit;
        end;
        // second confirm the size
        u_MakeGDAL.OutputTileSize := ComboBox_TileSize.text;
        if MessageDlg('Proceed with tile size "' + u_MakeGDAL.OutputTileSize + '" ?', mtConfirmation,
          [mbYes, mbNo], 0) = mrNo then begin
          Exit;
        end;

        // make individual tiles
        ProgressBar_Status.Max := (TileRowCount)*(TileColumnCount);
        for i := 0 to TileRowCount-1 do begin
          for j := 0 to TileColumnCount-1 do begin
            TileIndex := i*(TileColumnCount+1)+j;
            //MakeGDALbatchTiffFile(TileIndex); // needs 3857 not 4326, but for tiles distortion is minimal
//            MakeDDSbatchFile(TileIndex);
            // New batch file with built-in calculations
            //xMakeAutoGDALbatchFile(TileIndex); // 4326
            MakeAutoGDALbatchFile(False, 3857, TileIndex);  // 3857
            MakeAutoGDALbatchFile(False, 4326, TileIndex);  // 4326
            // for DetectTree
            MakeAutoGDALbatchFile(True, 3857, TileIndex);  // 3857
            MakeAutoGDALbatchFile(True, 4326, TileIndex);  // 4326
            ProgressBar_Status.StepIt;
            Application.ProcessMessages;
          end;
        end;
        ProgressBar_Status.Position := 0;

        // make a batchfile to call all batch files
        MakeGDAL_All_BatchFile(False,False,3857);
        MakeGDAL_All_BatchFile(False,False,4326);
        // for DetectTree
        MakeGDAL_All_BatchFile(True,False,3857);
        Make_DetectTree_to_ForestMaps_BatchFile;

//        MakeDDS_All_BatchFile;
        // when GeoTiff available, no need for actual coordinates
        // make a batchfile to call all TIFF batch files
        //MakeGDAL_All_BatchFile(True); // needs 3857 not 4326, but for tiles distortion is minimal

        // when GeoTiff available, no need for actual coordinates
        // make overall batch file - enable only when Allmapsoft has fixed combine TIF generation
        //MakeGDALoverallTiffBatchFile('Overall'); // needs 3857 not 4326
        // new
        MakeAutoGDALoverallBatchFile('Overall'); // converts 4326 to 3857

      // otherwise, need actual coordinates from GMID download
      end else begin
        if (TileName = 'Overall') then begin
          u_BMP.Memo_Message := Memo_Info;
          u_BMP.BMPfolder := WorkingPathName+'\SourceTiles\'+
             'Overall\';
          if (DirectoryExists(u_BMP.BMPfolder +'Overall'+ '_combined')) then begin
            u_BMP.BMPfolder := u_BMP.BMPfolder +'Overall'+ '_combined';
          end;
          Bitmap_GetWidthHeight('Overall.bmp');
          if (BitmapSuccess) then begin
            u_GMIDlog.Memo_Message := Memo_Info;
            u_GMIDlog.GMIDfolder := WorkingPathName+'\SourceTiles\'+
             'Overall';
//            ReadSourceBitmapExtents(TileIndex);
            if (FileExists(u_GMIDlog.GMIDfolder+'\Overall.umd')) then begin
              xReadSourceBitmapExtents(u_GMIDlog.GMIDfolder+'\Overall.umd');
            end else begin
              xReadSourceBitmapExtents(u_GMIDlog.GMIDfolder+'\Overall.gmid');
            end;
            if (SourceTileOpen) then begin
              MakeGDALoverallBatchFile('Overall');
             // xxMakeGDALoverallBatchFile('Overall');   // needs 3857 not 4326
             // MakeGDALoverallTiffBatchFile('Overall'); // needs 3857 not 4326
            end;
          end else begin // make Auto file anyway
            MakeAutoGDALoverallBatchFile('Overall'); // converts 4326 to 3857
          end;
        end else begin // not overall, individual tile
          if (length(TileName) <> 4) then begin
            Memo_Info.Lines.Add('Select a tile name first');
            Beep;
          end else begin // tiles
            // first confirm the zoom level (needed for geid)  -  how to avoid ???
            u_MakeGDAL.ZoomLevel := ComboBox_ZoomLevel.text;
            if MessageDlg('Proceed with zoom level "' + u_MakeGDAL.ZoomLevel + '" ?', mtConfirmation,
              [mbYes, mbNo], 0) = mrNo then begin
              Exit;
            end;
            // first confirm the size
            u_MakeGDAL.OutputTileSize := ComboBox_TileSize.text;
            if MessageDlg('Proceed with tile size "' + u_MakeGDAL.OutputTileSize + '" ?', mtConfirmation,
              [mbYes, mbNo], 0) = mrNo then begin
              Exit;
            end;

            GetTileIndex(TileName,TileColumn, TileRow);
//            Val(copy(TileName,1,2),TileColumn,ErrorCode);
//            Val(copy(TileName,3,2),TileRow,ErrorCode);
  //   if errorcode or not in range -> error
            TileIndex := TileRow*(TileColumnCount+1)+TileColumn;
// new batch file with built-in calcs replaces check for coords and BMP size,
// but leave for now
            u_BMP.Memo_Message := Memo_Info;
            u_BMP.BMPfolder := WorkingPathName+'\SourceTiles\'+
               TileList[TileIndex].TileName;
            if (DirectoryExists(u_BMP.BMPfolder +'\'+TileList[TileIndex].TileName + '_combined')) then begin
              u_BMP.BMPfolder := u_BMP.BMPfolder +'\'+TileList[TileIndex].TileName + '_combined';
            end;
            Bitmap_GetWidthHeight(TileList[TileIndex].TileName+'.bmp');
            if (BitmapSuccess) then begin
              u_GMIDlog.Memo_Message := Memo_Info;
              u_GMIDlog.GMIDfolder := WorkingPathName+'\SourceTiles\'+TileName;
//              ReadSourceBitmapExtents(TileIndex);
              if (FileExists(u_GMIDlog.GMIDfolder+'\'+TileName+'.geid')) then begin
                xReadSourceBitmapExtents(u_GMIDlog.GMIDfolder+'\'+TileName+'.geid'); // geid is 4326 !
              end else begin
                xReadSourceBitmapExtents(u_GMIDlog.GMIDfolder+'\'+TileName+'.gmid'); // gmid is 3857
              end;
              if (SourceTileOpen) then begin
                //xMakeGDALbatchFile(TileIndex); // 4326
                MakeAutoGDALbatchFile(False, 3857, TileIndex); // 3857
                MakeAutoGDALbatchFile(False, 4326, TileIndex); // 4326
//                MakeDDSbatchFile(TileIndex);
                //MakeGDALbatchTiffFile(TileIndex); // needs 3857 not 4326
                // for DetectTree
                MakeAutoGDALbatchFile(True, 3857, TileIndex);  // 3857
                MakeAutoGDALbatchFile(True, 4326, TileIndex);  // 4326
              end else begin // do it anyway
                MakeAutoGDALbatchFile(False, 3857, TileIndex); // 3857
                MakeAutoGDALbatchFile(False, 4326, TileIndex); // 4326
                // for DetectTree
                MakeAutoGDALbatchFile(True, 3857, TileIndex);  // 3857
                MakeAutoGDALbatchFile(True, 4326, TileIndex);  // 4326
              end;
            end else begin // make Auto file anyway
              MakeAutoGDALbatchFile(False, 3857, TileIndex); // 3857
              MakeAutoGDALbatchFile(False, 4326, TileIndex); // 4326
//              MakeDDSbatchFile(TileIndex);
              // for DetectTree
              MakeAutoGDALbatchFile(True, 3857, TileIndex);  // 3857
              MakeAutoGDALbatchFile(True, 4326, TileIndex);  // 4326
            end;
          end;
        end;
      end;
    end;
//  end else begin
//    Memo_Info.Lines.Add('Need Header file first');
//    Beep;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_DDSClick(Sender: TObject);
var
  ErrorCode : integer;
  TileName : string;
  TileIndex : integer;
  TileRow, TileColumn : integer;
  i, j : integer;
//  FileName : string;

begin
  Memo_Info.Lines.Clear;

  if (LandscapeOpened) then begin

    u_MakeDDS.Memo_Message := Memo_Info;
    u_MakeDDS.DDSfolder := WorkingPathName;
    u_MakeDDS.GDALlibraryfolder := GdalpathName;
//    u_MakeDDS.CondorFolder := CondorPathName;
    u_MakeDDS.CompressorFolder := CompressorPathName;
    u_MakeDDS.DXT_Type := ComboBox_DXT.Text;
    ForceDirectories(WorkingPathName+'\..\Textures');

    begin // Tiles
      TileName := ComboBox_Single.text;
      if (TileName = '') then begin // default blank -> all
        // make individual tiles
        ProgressBar_Status.Max := (TileRowCount)*(TileColumnCount);
        for i := 0 to TileRowCount-1 do begin
          for j := 0 to TileColumnCount-1 do begin
            TileIndex := i*(TileColumnCount+1)+j;
            MakeDDSbatchFile(TileIndex);
            ProgressBar_Status.StepIt;
            Application.ProcessMessages;
          end;
        end;
        ProgressBar_Status.Position := 0;

        // make a batchfile to call all batch files
        MakeDDS_All_BatchFile;
      // otherwise, need actual coordinates from GMID download
      end else begin
        if (TileName = 'Overall') then begin // not applicable
          Memo_Info.Lines.Add('Select a tile name first');
          Beep;
        end else begin // not overall, individual tile
          if (NOT GetTileIndex(TileName,TileColumn, TileRow)) then begin
            Memo_Info.Lines.Add('Select a tile name first');
            Beep;
          end else begin
            TileIndex := TileRow*(TileColumnCount+1)+TileColumn;
            MakeDDSbatchFile(TileIndex);
          end;
{          if (length(TileName) <> 4) then begin
            Memo_Info.Lines.Add('Select a tile name first');
            Beep;
          end else begin // tiles
            Val(copy(TileName,1,2),TileColumn,ErrorCode);
            Val(copy(TileName,3,2),TileRow,ErrorCode);
  //   if errorcode or not in range -> error
            TileIndex := TileRow*(TileColumnCount+1)+TileColumn;
            MakeDDSbatchFile(TileIndex);
          end;
}        end;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_ForestClick(Sender: TObject);
begin
  if (ComboBox_Version.text = 'V1') then begin
    ForestResolution := 2;
  end else begin
    ForestResolution := 8;
  end;
  begin
    setLength(ForestGrid,tColumns*ForestResolution,tRows*ForestResolution);
    if (HeaderOpen) then begin
//      Form_MakeForest.Position := poDefault;
      // offset to be able to see status and progressbar
      Form_MakeForest.Left := Self.Left + ProgressBar_Status.left + ProgressBar_Status.width + 10;
      Form_MakeForest.Top  := Self.Top + 0;
      Form_MakeForest.ShowModal;
      Application.ProcessMessages;
      if (u_MakeForest.ActionRequest) then begin
        // create a Forest file
        u_Forest.Memo_Message := Memo_Info;
        u_Forest.ProgressBar_Status := ProgressBar_Status;
        u_Forest.SourceForestFolder := WorkingPathName;
        u_Forest.DestinationForestFolder := WorkingPathName+'\..\ForestMaps';
        ForceDirectories(u_Forest.DestinationForestFolder);
        if (form_MakeForest.CheckBox_Forest.checked) then begin
          CreateForestMap(form_MakeForest.CheckBox_Shrink.checked, CondorLandscapeName+'.for');
        end;
        u_Forest.DestinationForestFolder := WorkingPathName+'\ForestMap';
        ForceDirectories(u_Forest.DestinationForestFolder);
        if (form_MakeForest.CheckBox_IntermediateConiferous.checked) then begin
          CreateForestBitmap(form_MakeForest.CheckBox_Shrink.checked, fConiferous,'ConiferousMap.bmp');
        end;
        if (form_MakeForest.CheckBox_IntermediateDeciduous.checked) then begin
          CreateForestBitmap(form_MakeForest.CheckBox_Shrink.checked, fDeciduous,'DeciduousMap.bmp');
        end;
        if (form_MakeForest.CheckBox_Export_LE.checked) then begin
          u_Forest.SourceForestFolder := WorkingPathName+'\..\ForestMaps';
          u_Forest.DestinationForestFolder := WorkingPathName+'\ForestMaps';
          ForceDirectories(u_Forest.DestinationForestFolder);
          Export_Forest_To_LE;
        end;
//        Beep; //all done
      end;
    end else begin
      Memo_Info.Lines.Add('Need Header file first');
      Beep;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_ThermalClick(Sender: TObject);
var
  i : integer;

begin
  if (HeaderOpen) then begin
    if (ComboBox_Version.text = 'V1') then begin
      form_MakeThermal.RadioButton_SunnySlopes.enabled := true;
      ForestResolution := 2;
    end else begin // V2
      form_MakeThermal.RadioButton_SunnySlopes.enabled := false;
      form_MakeThermal.RadioButton_UniformHeating.checked := true;
      ForestResolution := 8;
    end;
//    Form_MakeThermal.Position := poDefault;
    // offset to be able to see status and progressbar
    Form_MakeThermal.Left := Self.Left + ProgressBar_Status.left + ProgressBar_Status.width + 10;
    Form_MakeThermal.Top  := Self.Top + 0;
    form_MakeThermal.ShowModal;
    Application.ProcessMessages;
    if (u_MakeThermal.ActionRequest) then begin

      //get thermal heating values
      with u_MakeThermal.Form_MakeThermal.StringGrid_ThermalHeating do begin
        for i := 0 to high(Heating) do begin
          Heating[i] := ClampByte(Round(StrToFloat(Cells[1,i+1])*255/100));
        end;
      end;

      // create a Thermal file
      u_Thermal.Memo_Message := Memo_Info;
      u_Thermal.ProgressBar_Status := ProgressBar_Status;
      u_Thermal.SourceThermalFolder := WorkingPathName;
      u_Thermal.DestinationThermalFolder := WorkingPathName+'\..';
//      if (NOT DirectoryExists(WorkingPathName+'\ThermalMap')) then begin
//        mkdir(WorkingPathName+'\ThermalMap');
//      end;
      ForceDirectories(WorkingPathName+'\ThermalMap');
      //skip if .trn file not changed and
      //if SunnySlopes.bmp not changed
      if (form_MakeThermal.RadioButton_SunnySlopes.checked) then begin
        if (NOT fileExists(WorkingPathName+'\ThermalMap\SunnySlopes.bmp') ) then begin
          u_Terrain.Memo_Message := Memo_Info;
          u_Terrain.ProgressBar_Status := ProgressBar_Status;
          CreateSlopeGradientBitmap(WorkingPathName+'\..\'+CondorLandscapeName+'.trn',
            WorkingPathName+'\ThermalMap\SunnySlopes.bmp');
        end else begin
          Memo_Info.Lines.Add('SunnySlopes.bmp already exists');
        end;
      end;
      if (form_MakeThermal.CheckBox_Thermal.checked) then begin
        CreateThermalMap(CondorLandscapeName+'.tdm');
      end;
      u_Thermal.DestinationThermalFolder := WorkingPathName+'\ThermalMap';
      ForceDirectories(u_Thermal.DestinationThermalFolder);
      if (form_MakeThermal.CheckBox_IntermediateThermalBitmap.checked) then begin
        CreateThermalBitmap('GreyScale.bmp');
      end;
      if (form_MakeThermal.CheckBox_Export_LE.checked) then begin
        // put it where Landscape Editor expects it
        TDM_To_Greyscale_Bitmap(WorkingPathName+'\..\'+CondorLandscapeName+'.tdm',
          WorkingPathName+'\ThermalMap.bmp');
      end;
//      Beep; //all done
    end;
  end else begin
    Memo_Info.Lines.Add('Need Header file first');
    Beep;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_ExportCalibClick(Sender: TObject);
begin
  if (HeaderOpen) then begin
    exFileFilterString := 'Tile List (*.CSV)|*.CSV|All files (*.*)|*.*';
    exFileName := 'CalibrationPoints.csv';
    exInitialDir := WorkingPathName;
//    form_CalibExport.ShowModal;
//    if (u_CalibExport.exActionRequest) then begin
    if SaveDialog(SaveDialog_File, exFileName, exInitialDir, exFileFilterString) then begin
      u_TileList.CondorFolder := CondorPathName;
      u_TileList.Memo_Message := Memo_Info;
      //write 2 opposite corners, TopLeft and BottomRight
      WriteTileCorners(exFileName);
      //Also write corners for each tile
      WriteTileList(extractFilePath(exFileName)+'TileExtents.csv');
      //Also write overall corners and flying range corners
      WriteTileRanges(extractFilePath(exFileName)+'TileRanges.csv');
    end;
  end else begin
    Memo_Info.Lines.Add('Need Header file first');
    Beep;
  end;
end;

//---------------------------------------------------------------------------
{ no longer enabled. Not needed. Override calib procedure provided instead.
procedure TForm_Main.Button_ImportCalibClick(Sender: TObject);
begin
  if (HeaderOpen) then begin
    imFileFilterString := 'Tile List (*.CSV)|*.CSV|All files (*.*)|*.*';
    imFileName := 'cal.csv';
    imInitialDir := WorkingPathName;
//    form_CalibImport.ShowModal;
//    if (u_CalibImport.imActionRequest) then begin
    if OpenDialog(SaveDialog_File, imFileName, imInitialDir, imFileFilterString) then begin
      u_TileList.CondorFolder := CondorPathName;
      u_TileList.Memo_Message := Memo_Info;
//      u_TileList.TileRowCount := RowCount div 256;
//      u_TileList.TileColumnCount := ColumnCount div 256;
      ReadTileList(imFileName);
    end;
  end else begin
    Memo_Info.Lines.Add('Need Header file first');
    Beep;
  end;
end;
}
//---------------------------------------------------------------------------
procedure TForm_Main.Button_OverrideCalibClick(Sender: TObject);
begin
  if (HeaderOpen) then begin
    u_Terrain.Memo_Message := Memo_Info;
    OverrideTerrainCalibration(WorkingPathName+'\..\'+
      CondorLandscapeName+'.trn');
  end else begin
    Memo_Info.Lines.Add('Need Header file first');
    Beep;
  end;
end;

//---------------------------------------------------------------------------
function TForm_Main.GetTileFile(TileName:String):boolean;
begin
  tFileName := WorkingPathName+'\lsTerragen\'+TileName+'.bmp';
  if (FileExists(tFileName) AND
    (ComboBox_Imagery.Text = 'Landsat') ) then begin
  end else begin
    tFileName := WorkingPathName+'\gTerragen\'+TileName+'.bmp';
    if (FileExists(tFileName) AND
      (ComboBox_Imagery.Text <> 'Landsat') ) then begin
    end else begin // alternate
      tFileName := WorkingPathName+'\Terragen\Alternates\'+TileName+'.bmp';
      if (FileExists(tFileName)) then begin
      end else begin // Condor V1
        tFileName := WorkingPathName+'\Terragen\'+TileName+'.bmp';
        if (FileExists(tFileName)) then begin
        end else begin // Condor V2
          tFileName := WorkingPathName+'\Terragen\Textures\'+TileName+'.bmp';
          if (FileExists(tFileName)) then begin
          end else begin
//            Memo_Info.Lines.Add('Bitmap File Not Found');
//            Beep;
            tFileName := '';
            Result := false;
            Exit;
          end;
        end;
      end;
    end;
  end;
  Result := true;
end;

//---------------------------------------------------------------------------
function TForm_Main.GetAlternateTileFile(TileName:String):boolean;
var
  Path : string;
begin
        Path := WorkingPathName+'\GeoDatabase';
        bFileName := Path+'\'+TileName+'.bmp';
        if (FileExists(bFileName)) then begin
          Form_Graphic.Image_Alternate.Stretch := true; // stretch it to same size as tile
          Form_Graphic.Image_Alternate.{Picture.Bitmap.}Width := Form_Graphic.Image_Tile.Picture.Bitmap.Width;
          Form_Graphic.Image_Alternate.{Picture.Bitmap.}Height := Form_Graphic.Image_Tile.Picture.Bitmap.Height;
//          Form_Graphic.Image_Alternate.Picture.Bitmap.TransparentMode := tmFixed;
          Form_Graphic.Image_Alternate.Picture.LoadFromFile(bFileName);
          Form_Graphic.Image_Alternate.Picture.Bitmap.TransparentColor := tNone.ColorValue{clBlack};
          Form_Graphic.Image_Alternate.enabled := true;
          Form_Graphic.CheckBox_Alternate.enabled := true;
        end else begin
          // not found so make disable the image
          Form_Graphic.Image_Alternate.enabled := false;
          Form_Graphic.CheckBox_Alternate.enabled := false;
        end;
        Form_Graphic.CheckBox_Alternate.checked := false;
  result := true;
end;

//---------------------------------------------------------------------------
function TForm_Main.IdentifyFiles(TileName:String;TileIndex:integer):boolean;
var
  Path : string;
begin
  Path := WorkingPathName+'\SourceTiles\'+
          TileList[TileIndex].TileName;
  atFileName := Path+'\'+TileName+'_a.bmp'; //saved background tile

  // thermal tile
  thFileName := Path+'\'+TileName+'_t.bmp'; //thermal tile
  if (NOT FileExists(thFileName)) then begin
    ForceDirectories(Path);
    CreateThermalTileBitmap(thFileName); // V1 and V2 style 256x256
  end;

  // Forest tile
{  // first check if V2 deciduous file is present
  // really need to get coniferous too ! TBD
  eFileName := WorkingPathName+'\Terragen\ForestMaps\b'+TileName+'.bmp';
  if (FileExists(eFileName)) then begin  // if so use it
    mFileName := eFileName;  // V2 style 2048x2048
  end else begin  // else use V1 bitmap
    mFileName := Path+'\'+TileName+'_f.bmp';
    if (NOT FileExists(mFileName)) then begin
      ForceDirectories(Path);
      CreateForestTileBitmap(mFileName); // V1 style 512x512
    end;
  end;
}
 if (ComboBox_Version.text = 'V1') then begin
    mFileName := Path+'\f'+TileName+'.bmp'; // check newer version first
    if (NOT FileExists(mFileName)) then begin
      mFileName := Path+'\'+TileName+'_f.bmp'; // try old version
      if (NOT FileExists(mFileName)) then begin
        ForceDirectories(Path);
        CreateForestTileBitmap(mFileName); // V1 style 512x512
      end;
    end;
  end else begin // V2
    path := WorkingPathName+'\Terragen\ForestMaps';
//    // check if V2 deciduous file is present
//    mFileName := path +'\b'+TileName+'.bmp';
    // check if V2 combined file is present
    mFileName := path + '\'+TileName+'.bmp';
    if (NOT FileExists(mFileName)) then begin  // if so use it
      ForceDirectories(Path);
      CreateForestTileBitmap(mFileName); // V2 style 2048x2048
    end;
  end;

  result := true;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_EditForestClick(Sender: TObject);
var
  ErrorCode : integer;
  TileIndex : integer;
//  Path : string;
  TileColumn, TileRow : integer;

begin
  if (ComboBox_Version.text = 'V1') then begin
    ForestResolution := 2;
    setLength(ForestGrid,tColumns*ForestResolution,tRows*ForestResolution);
  end else begin
    ForestResolution := 8;
    setLength(ForestGrid,tColumns*ForestResolution,tRows*ForestResolution);
  end;
  u_Forest.Memo_Message := Memo_Info;
  u_Forest.ProgressBar_Status := ProgressBar_Status;
  u_Thermal.Memo_Message := Memo_Info;
  u_Thermal.ProgressBar_Status := Form_Graphic.ProgressBar_Status;
  u_TIFF.Memo_Message := Memo_Info;
//  if (HeaderOpen) AND (TileOpen) then begin
  if (LandscapeOpened) then begin
    TileName := ComboBox_Single.text;
    if (NOT GetTileIndex(TileName,TileColumn, TileRow)) then begin
      Memo_Info.Lines.Add('Select a tile name first');
      Beep;
    end else begin
{    if (length(TileName) <> 4) then begin
      Memo_Info.Lines.Add('Select a tile name first');
      Beep;
    end else begin
}      Form_Graphic.Caption := 'Forest Map '+TileName;
//      Val(copy(TileName,1,2),TileColumn,ErrorCode);
//      Val(copy(TileName,3,2),TileRow,ErrorCode);
//// if errorcode or not in range -> error
      TileIndex := TileRow*(TileColumnCount+1)+TileColumn;
      if (NOT GetTileFile(TileName)) then begin
        // TBD - option to import from Textures (combine 16 tiles)
        Memo_Info.Lines.Add('Terragen Bitmap File Not Found');
        Beep;
      end else begin
        // check if too large to load - avoid crash
        if (BMP_ImageWidth(tFileName) > 32768) then begin
          Memo_Info.Lines.Add('Terragen bitmap too large');
          Beep; Exit;
        end;
        // set image to auto take its size from picture 1:1 and fit in window
        Form_Graphic.Image_Tile.Align := alClient;
        Form_Graphic.Image_Tile.AutoSize := true;
        Form_Graphic.Image_Tile.Stretch := false; // no stretch - 1:1 resolution
        // now load the picture bitmap
        Form_Graphic.Image_Tile.Picture.LoadFromFile(tFileName);
        if (Form_Graphic.Image_Tile.Picture.Bitmap.PixelFormat <> pf24bit) then begin
          //Form_Graphic.Image_Tile.Picture.Bitmap.PixelFormat := pf24bit; // force 24 bit color
          Memo_Info.Lines.Add('Terragen Bitmap must be 24 bit color');
          Beep; Exit;
        end;
        u_ReduceColors.ColorsReduced := false;
        Form_Graphic.Image_PaletteClick(Sender);
        Form_Graphic.ScrollBox_Image.HorzScrollBar.Range := Form_Graphic.Image_Tile.Picture.Width;
        Form_Graphic.ScrollBox_Image.VertScrollBar.Range := Form_Graphic.Image_Tile.Picture.Height;

        // load alternate background file if any
        GetAlternateTileFile(TileName);

        IdentifyFiles(TileName, TileIndex);

        // get forest tile bitmap
        Form_Graphic.Image_Mask.Stretch := true; // stretch it to same size as tile
        Form_Graphic.Image_Mask.{Picture.Bitmap.}Width := Form_Graphic.Image_Tile.Picture.Bitmap.Width;
        Form_Graphic.Image_Mask.{Picture.Bitmap.}Height := Form_Graphic.Image_Tile.Picture.Bitmap.Height;
        Form_Graphic.Image_Mask.Picture.LoadFromFile(mFileName);
// Doesn't work for 2 color bitmaps - transparent color is bottom right pixel color, and other color is set to black - can't seem to overide
// force 24 bit color
        Form_Graphic.Image_Mask.Picture.Bitmap.Pixelformat := pf24bit;
//        Form_Graphic.Image_Mask.Picture.Bitmap.Pixelformat := pf4bit; // works OK when applied to pf1bit bitmaps
        Form_Graphic.Image_Mask.Picture.Bitmap.TransparentMode := tmFixed;
        Form_Graphic.Image_Mask.Picture.Bitmap.TransparentColor := tNone.ColorValue{clBlack};

        if ( (Form_Graphic.Image_Tile.Picture.Width mod tColumns*ForestResolution <> 0) OR
             (Form_Graphic.Image_Tile.Picture.Height mod tRows*ForestResolution <> 0) )then begin
          Memo_Info.Lines.Add('Error - Bitmap ratio not supported.');
          Beep;
          Exit;
        end;

        Unit_Graphics.zoomScale := 1.0;
        xScale := Form_Graphic.Image_Tile.Picture.Width div Form_Graphic.Image_Mask.Picture.Bitmap.Width;
        yScale := Form_Graphic.Image_Tile.Picture.Height div Form_Graphic.Image_Mask.Picture.Bitmap.Height;
        SelectionRectangle := rect(0,0,
          Form_Graphic.Image_Mask.Picture.Bitmap.Width,
          Form_Graphic.Image_Mask.Picture.Bitmap.Height);
        Form_Graphic.Button_NoneClick(Sender);

        Unit_Graphics.xCoord := TileList[TileIndex].TileUTMRight;
        Unit_Graphics.yCoord := TileList[TileIndex].TileUTMBottom;

        Unit_Graphics.Graphic_Mode := gmForest;
        Form_Graphic.Button_Color0.Caption := 'Coniferous';
        Form_Graphic.Button_Color1.Caption := 'Deciduous';
        Form_Graphic.Button_Color2.Caption := 'Both';
        Form_Graphic.Button_Tool_0.Caption := 'b/s Import';
        Form_Graphic.Button_Sand.Visible := false;
        Form_Graphic.Button_Swamp.Visible := false;
        Form_Graphic.Button_Import.Caption := 'T Import';
        if (ComboBox_Version.text = 'V1') then begin
          ForestResolution := 2;
          Form_Graphic.Button_Tool_0.Visible := false;
        end else begin
          ForestResolution := 8;
          Form_Graphic.Button_Tool_0.Visible := true;
        end;
        Form_Graphic.Button_Import.Enabled := true;
        Form_Graphic.Button_Save_TIF.Visible := true;

        Unit_Graphics.wWorkingPath := WorkingPathName;
        Form_Graphic.ShowModal;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_EditThermalClick(Sender: TObject);
var
  ErrorCode : integer;
  TileIndex : integer;
//  Path : string;
  TileColumn, TileRow : integer;

begin
  if (ComboBox_Version.text = 'V1') then begin
    ForestResolution := 2;
    setLength(ForestGrid,tColumns*ForestResolution,tRows*ForestResolution);
  end else begin
    ForestResolution := 8;
    setLength(ForestGrid,tColumns*ForestResolution,tRows*ForestResolution);
  end;
  u_Thermal.Memo_Message := Memo_Info;
  u_Thermal.ProgressBar_Status := Form_Graphic.ProgressBar_Status;
  u_Forest.Memo_Message := Memo_Info;
  u_Forest.ProgressBar_Status := Form_Graphic.ProgressBar_Status;
  if (LandscapeOpened) then begin
    TileName := ComboBox_Single.text;
    if (NOT GetTileIndex(TileName,TileColumn, TileRow)) then begin
      Memo_Info.Lines.Add('Select a tile name first');
      Beep;
    end else begin
{    if (length(TileName) <> 4) then begin
      Memo_Info.Lines.Add('Select a tile name first');
      Beep;
    end else begin
}      Form_Graphic.Caption := 'Thermal Map '+TileName;
//      Val(copy(TileName,1,2),TileColumn,ErrorCode);
//      Val(copy(TileName,3,2),TileRow,ErrorCode);
//// if errorcode or not in range -> error
      TileIndex := TileRow*(TileColumnCount+1)+TileColumn;
      if (NOT GetTileFile(TileName)) then begin
        // TBD - option to import from Textures (combine 16 tiles)
        Memo_Info.Lines.Add('Terragen Bitmap File Not Found');
        Beep;
      end else begin
        // check if too large to load - avoid crash
        if (BMP_ImageWidth(tFileName) > 32768) then begin
          Memo_Info.Lines.Add('Terragen bitmap too large');
          Beep; Exit;
        end;
        // set image to auto take its size from picture 1:1 and fit in window
        Form_Graphic.Image_Tile.Align := alClient;
        Form_Graphic.Image_Tile.AutoSize := true;
        Form_Graphic.Image_Tile.Stretch := false; // no stretch - 1:1 resolution
        // now load the picture bitmap
        Form_Graphic.Image_Tile.Picture.LoadFromFile(tFileName);
        if (Form_Graphic.Image_Tile.Picture.Bitmap.PixelFormat <> pf24bit) then begin
          //Form_Graphic.Image_Tile.Picture.Bitmap.PixelFormat := pf24bit; // force 24 bit color
          Memo_Info.Lines.Add('Terragen Bitmap must be 24 bit color');
          Beep; Exit;
        end;
        u_ReduceColors.ColorsReduced := false;
        Form_Graphic.Image_PaletteClick(Sender);
        Form_Graphic.ScrollBox_Image.HorzScrollBar.Range := Form_Graphic.Image_Tile.Picture.Width;
        Form_Graphic.ScrollBox_Image.VertScrollBar.Range := Form_Graphic.Image_Tile.Picture.Height;

        // load alternate background file if any
        GetAlternateTileFile(TileName);

        IdentifyFiles(TileName,TileIndex);

        // get thermal tile bitmap
        Form_Graphic.Image_Mask.Stretch := true; // stretch it to same size as tile
        Form_Graphic.Image_Mask.{Picture.Bitmap.}Width := Form_Graphic.Image_Tile.Picture.Bitmap.Width;
        Form_Graphic.Image_Mask.{Picture.Bitmap.}Height := Form_Graphic.Image_Tile.Picture.Bitmap.Height;
        Form_Graphic.Image_Mask.Picture.LoadFromFile(thFileName);
        Form_Graphic.Image_Mask.Picture.Bitmap.PixelFormat := pf24bit; // force 24 bit color

        Form_Graphic.Image_Mask.Picture.Bitmap.TransparentMode := tmFixed;
        Form_Graphic.Image_Mask.Picture.Bitmap.TransparentColor := tNone.ColorValue{clBlack};

        if ( (Form_Graphic.Image_Tile.Picture.Width mod tColumns <> 0) OR
             (Form_Graphic.Image_Tile.Picture.Height mod tRows <> 0) )then begin
          Memo_Info.Lines.Add('Error - Bitmap ratio not supported.');
          Beep;
          Exit;
        end;

        Unit_Graphics.zoomScale := 1.0;
//        xScale := Form_Graphic.Image_Tile.Picture.Width div tColumns;
//        yScale := Form_Graphic.Image_Tile.Picture.Height div tRows;
        xScale := Form_Graphic.Image_Tile.Picture.Width div Form_Graphic.Image_Mask.Picture.Bitmap.Width;
        yScale := Form_Graphic.Image_Tile.Picture.Height div Form_Graphic.Image_Mask.Picture.Bitmap.Height;
        Form_Graphic.Button_NoneClick(Sender);

        Unit_Graphics.xCoord := TileList[TileIndex].TileUTMRight;
        Unit_Graphics.yCoord := TileList[TileIndex].TileUTMBottom;

        Unit_Graphics.Graphic_Mode := gmThermal;
        Form_Graphic.Button_Color0.Caption := 'Green Fields';
        Form_Graphic.Button_Color1.Caption := 'Yellow Fields';
        Form_Graphic.Button_Color2.Caption := 'Dark Fields';
        Form_Graphic.Button_Tool_0.Caption := 'Water';
        Form_Graphic.Button_Tool_0.Visible := true;
        Form_Graphic.Button_Sand.Visible := true;
        Form_Graphic.Button_Swamp.Visible := true;
        Form_Graphic.Button_Import.Caption := 'F Import';
        if (ComboBox_Version.text = 'V1') then begin
          ForestResolution := 2;
          Form_Graphic.Button_Import.Enabled := true;
        end else begin
          ForestResolution := 8;
//          Form_Graphic.Button_Import.Enabled := false;
          Form_Graphic.Button_Import.Enabled := true;
        end;
        Form_Graphic.Button_Save_TIF.Visible := false;

        Form_Graphic.ShowModal;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_GradientClick(Sender: TObject);
begin
  if (HeaderOpen) then begin
    gFileName := CondorPathName+'\Landscapes\'+
      CondorLandscapeName+'\HeightGradient.bmp';
    GradientFolder := WorkingPathName;

//    Form_Gradient.Position := poDefault;
    // offset to be able to see status and progressbar
    Form_Gradient.Left := Self.Left + ProgressBar_Status.left + ProgressBar_Status.width + 10;
    Form_Gradient.Top  := Self.Top + 0;

    form_Gradient.ShowModal;
    Application.ProcessMessages;
    if (u_MakeGradient.gActionRequest) then begin
      u_Terrain.FileFolder := WorkingPathName;
      u_Terrain.Memo_Message := Memo_Info;
      u_Terrain.ProgressBar_Status := ProgressBar_Status;
      CreateColorGradientBitmap(WorkingPathName+'\..\'+CondorLandscapeName+'.trn',
        gFileName);
    end;
  end else begin
    Memo_Info.Lines.Add('Need Header file first');
    Beep;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_ObjectsClick(Sender: TObject);
begin
  Unit_Objects.ApplicationPath := ApplicationPathName;
//  oFolder := WorkingPathName;
  if (HeaderOpen) AND (TileOpen) then begin
    oFolder := CondorPathName+'\Landscapes\' + CondorLandscapeName;
  end else begin
    oFolder := CondorPathName+'\Landscapes';
  end;
  Unit_Objects.CondorFolder := CondorPathName;
  Unit_Objects.WorkingFolder := WorkingPathName;
  Unit_Objects.Memo_Message := Memo_Info;
  u_X_CX.OBJ_Type := Form_Objects.ComboBox_OBJ_Type.ItemIndex;

//  if (NOT DirectoryExists(WorkingPathName+'\Temp')) then begin
//    mkdir(WorkingPathName+'\Temp');
//  end;

//  Form_Objects.Position := poDefault;
  // offset to be able to see status and progressbar
  Form_Objects.Left := Self.Left + ProgressBar_Status.left + ProgressBar_Status.width + 10;
  Form_Objects.Top  := Self.Top + 0;
  Form_Objects.ShowModal;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_ObjectPlaceClick(Sender: TObject);
begin
//  if (HeaderOpen) AND (TileOpen) then begin
  if (LandscapeOpened) then begin
    if (Unit_ObjectPlacer.CurrentLandscape <> CondorLandscapeName) then begin
      Form_ObjectPlacer.GroupBox_ObjectPlace.Caption := CondorLandscapeName;
//      Object_FolderName := WorkingPathName+'\..';
      Object_FolderName := LandscapePathName;
      Object_FileName := CondorLandscapeName+'.obj';
      if (NOT FileExists(LandscapePathName+'\'+Object_FileName)) then begin
        Memo_Info.Lines.Add('Object file not found: '+Object_FileName);
        Beep;
        exit;
      end;
//      Screen.Cursor := crHourGlass;  // Let user know we're busy...
//      ReadObjectFile;
//      Form_ObjectPlacer.Initialize(Sender);
//      Screen.Cursor := crDefault;  // no longer busy
      Unit_ObjectPlacer.CurrentLandscape := CondorLandscapeName;
      Unit_ObjectPlacer.opVersion:= ComboBox_Version.text;
      Unit_ObjectPlacer.Memo_Message := Memo_Info;
    end else begin
    end;
    Screen.Cursor := crHourGlass;  // Let user know we're busy...
    ReadObjectFile;
    Form_ObjectPlacer.Initialize(Sender);
    Screen.Cursor := crDefault;  // no longer busy

//    Form_ObjectPlacer.Position := poDefault;
    // offset to be able to see status and progressbar
    Form_ObjectPlacer.Left := Self.Left + ProgressBar_Status.left + ProgressBar_Status.width + 10;
    Form_ObjectPlacer.Top  := Self.Top + 0;
    Form_ObjectPlacer.ShowModal;
//  end else begin
//    Memo_Info.Lines.Add('Need Header file first');
//    Beep;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_AirportPlaceClick(Sender: TObject);
begin
  if (LandscapeOpened) then begin
    if (Unit_AirportPlacer.CurrentLandscape <> CondorLandscapeName) then begin
      Form_AirportPlacer.GroupBox_AirportPlace.Caption := CondorLandscapeName;
      Airport_FolderName := WorkingPathName+'\..';
      Airport_FileName := CondorLandscapeName+'.apt';
      if (NOT FileExists(Airport_FolderName+'\'+Airport_FileName)) then begin
        Memo_Info.Lines.Add('Airport file not found: '+Airport_FileName);
        Beep;
        exit;
      end;
//      ReadAirportFile;
//      Form_AirportPlacer.Initialize(Sender);
      Unit_AirportPlacer.CurrentLandscape := CondorLandscapeName;
      Unit_AirportPlacer.apVersion:= ComboBox_Version.text;
      Unit_AirportPlacer.Memo_Message := Memo_Info;
    end else begin
    end;
    Screen.Cursor := crHourGlass;  // Let user know we're busy...
    ReadAirportFile;
    Form_AirportPlacer.Initialize(Sender);
    Screen.Cursor := crDefault;  // no longer busy

    // for HiResRunway
    u_MakeDDS.CompressorFolder := CompressorPathName;
    u_MakeDDS.DXT_Type := ComboBox_DXT.Text;
    // for HiResRunway
    u_MakeGDAL.GDALfolder := WorkingPathName;
    u_MakeGDAL.GDALlibraryfolder := GdalpathName;
    // for HiResRunway
    u_MakeGMID.GMIDProgramsfolder := DownloaderPathName;
    u_MakeGMID.GMIDfolder := WorkingPathName;
    u_MakeGMID.GMIDMapID := ComboBox_MapID.Text;
    u_MakeGMID.GMIDMapType := ComboBox_MapType.Text;
//    Form_AirportPlacer.Position := poDefault;
    // offset to be able to see status and progressbar
    Form_AirportPlacer.Left := Self.Left + ProgressBar_Status.left + ProgressBar_Status.width + 10;
    Form_AirportPlacer.Top  := Self.Top + 0;
    Form_AirportPlacer.ShowModal;
  end;
end;

//---------------------------------------------------------------------------
Procedure ComboBoxMatchString(CB : TComboBox; SearchText : string);
var
  i: integer;

begin
  with CB do begin
    ItemIndex := -1;
    for i := 0 to Items.Count-1 do begin
      if (Items.Strings[i] = SearchText) then begin
        ItemIndex := i;
        Exit; //all done
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
function CalcZoomResolution(zoomLevel : string; Latitude : single) : single;
const
  pixelsPerTile = 256; // base resolution of typical downloaded tile
var
  zL : single;
begin
  zL := strtofloat(zoomLevel);
  //size in meters per pixel
  result := (earthRadius *1000 * 2 * PI) * cos(Latitude /180 * PI) /pixelsPerTile / (power(2,zL));
end;

//---------------------------------------------------------------------------
function CalcCondorResolution(tileSize : string) : single;
const
  pixelsPerTile = 256; // base resolution of typical downloaded tile
var
  tS : single;
begin
  tS := strtofloat(tileSize);
  //condor tile size in meters
  result := 256 * 90 / tS;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.FormCreate(Sender: TObject);
var
//  Default : string;
  Desktop:   TRect;

begin
  // problem with decimal separator ',' in Europe
  // try to force it to '.' or this program
  Force_DecimalSeparator;
//    Memo_Info.Lines.Add('Decimal separator: ' + DecimalSeparator);
    Memo_Info.Lines.Add('Decimal separator: ' + DecimalChar);

//  ProgramPathName := getCurrentDir;
// it seems that if you use a file dialog, the current directory is changed to
// where the folder is for the selected file
  ApplicationPathName := ExtractFileDir(Application.ExeName);
  ForceDirectories(ApplicationPathName+'\Temp');

  FIniFile := TRegIniFile.Create('Varicalc_CondorScenery');

  // retrieve saved values from registry
//  TileName := Edit_TileListName.text; {default}
//  TileName := FIniFile.ReadString('Files','TileFile',TileName);
//  Edit_TileListName.Text := ShortenFolderString(TileName,ShortPathNameLength);
//  Edit_TileListName.Hint := TileName;

  CondorPathName := Edit_CondorPath.text; {default}
  CondorPathName := FIniFile.ReadString('Paths','CondorPath',CondorPathName);
  Edit_CondorPath.Text := ShortenFolderString(CondorPathName,ShortPathNameLength);
  Edit_CondorPath.Hint := CondorPathName;
  MakeFolderList(ComboBox_Landscape.Items, CondorPathName+'\Landscapes', '\*.*');

  CondorLEpathName := Edit_LEpath.text; {default}
  CondorLEpathName := FIniFile.ReadString('Paths','CondorLEpath',CondorLEpathName);
  Edit_LEpath.Text := ShortenFolderString(CondorLEpathName,ShortPathNameLength);
  Edit_LEpath.Hint := CondorLEpathName;

//  WorkingPathName := Edit_DestinationPath.text; {default}
//  WorkingPathName := FIniFile.ReadString('Paths','DestinationPath',WorkingPathName);
//  Edit_DestinationPath.Text := ShortenFolderString(WorkingPathName,ShortPathNameLength);
//  Edit_DestinationPath.Hint := WorkingPathName;

  CondorLandscapeName := ''; {default}
  CondorLandscapeName := FIniFile.ReadString('Paths','CondorLandscape',CondorLandscapeName);
  ComboBoxMatchString(ComboBox_Landscape,CondorLandscapeName);
  if (ComboBox_Landscape.ItemIndex = -1) then begin
    CondorLandscapeName := '';
  end;
//  ComboBox_Landscape.Text := CondorLandscapeName;
  MakeWorkingPath;

  GDALpathName := Edit_GDALpath.text; {default}
  GDALpathName := FIniFile.ReadString('Paths','GDALpath',GDALpathName);
  Edit_GDALpath.Text := ShortenFolderString(GDALpathName,ShortPathNameLength);
  Edit_GDALpath.Hint := GDALpathName;

  CompressorPathName := Edit_CompressorPath.text; {default}
  CompressorPathName := FIniFile.ReadString('Paths','CompressorPath',CompressorPathName);
  Edit_CompressorPath.Text := ShortenFolderString(CompressorPathName,ShortPathNameLength);
  Edit_CompressorPath.Hint := CompressorPathName;

  DownloaderPathName := Edit_DownloaderPath.text; {default}
  DownloaderPathName := FIniFile.ReadString('Paths','DownloaderPath',DownloaderPathName);
  Edit_DownloaderPath.Text := ShortenFolderString(DownloaderPathName,ShortPathNameLength);
  Edit_DownloaderPath.Hint := DownloaderPathName;

  WgetPathName := Edit_WgetPath.text; {default}
  WgetPathName := FIniFile.ReadString('Paths','WgetPath',WgetPathName);
  Edit_WgetPath.Text := ShortenFolderString(WgetPathName,ShortPathNameLength);
  Edit_WgetPath.Hint := WgetPathName;

  sZIPpathName := Edit_7zipPath.text; {default}
  sZIPpathName := FIniFile.ReadString('Paths','7zipPath',sZIPpathName);
  Edit_7zipPath.Text := ShortenFolderString(sZIPpathName,ShortPathNameLength);
  Edit_7zipPath.Hint := sZIPpathName;

  ComboBox_DXT.text:= FIniFile.ReadString('Tiles','DXT_Type',ComboBox_DXT.Items[0]);
  ComboBoxMatchString(ComboBox_DXT,'');
  ComboBox_TileSize.text:= FIniFile.ReadString('Tiles','OutputSize',ComboBox_TileSize.Items[0]);
  ComboBoxMatchString(ComboBox_TileSize,'');
  ComboBox_TileSizeExit(Sender);
  ComboBox_ZoomLevel.text:= FIniFile.ReadString('MapData','ZoomLevel',ComboBox_ZoomLevel.Items[0]);
  ComboBoxMatchString(ComboBox_ZoomLevel,'');
  ComboBox_ZoomLevelExit(Sender);
  ComboBox_Imagery.text:= FIniFile.ReadString('MapData','Imagery',ComboBox_Imagery.Items[0]);
  ComboBoxMatchString(ComboBox_Imagery,'');
  ComboBox_GEO.text:= FIniFile.ReadString('MapData','GeoData',ComboBox_GEO.Items[0]);
  ComboBoxMatchString(ComboBox_GEO,'');
  ComboBox_MapID.text:= FIniFile.ReadString('MapData','MapID',ComboBox_MapID.Items[0]);
  ComboBoxMatchString(ComboBox_MapID,'');
  ComboBox_MapType.text:= FIniFile.ReadString('MapData','MapType',ComboBox_MapType.Items[0]);
  ComboBoxMatchString(ComboBox_MapType,'');
  ComboBox_Version.text:= FIniFile.ReadString('Condor','Version',ComboBox_Version.Items[0]);
  ComboBoxMatchString(ComboBox_Version,'');
  ComboBox_FileNameFormat.text:= FIniFile.ReadString('Condor','FileNameFormat',ComboBox_FileNameFormat.Items[0]);
  ComboBoxMatchString(ComboBox_FileNameFormat,'');

  // centre vertically and offset horizontally for other windows to show memo  and progress bar
  if SystemParametersInfo(SPI_GETWORKAREA,0,@Desktop,0) then begin
    Form_Main.Left := (Desktop.Right - Desktop.Left - Form_Main.Width) div 2 - 150;
    if (Form_Main.Left < 0) then Form_Main.Left :=0;
    Form_Main.Top  := (Desktop.Bottom - Desktop.Top - Form_Main.Height) div 2;
  end;

  Unit_Help.ApplicationPath := ApplicationPathName;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FIniFile.WriteString('Tiles','DXT_type',ComboBox_DXT.text);
  FIniFile.WriteString('Tiles','OutputSize',ComboBox_TileSize.text);
  FIniFile.WriteString('MapData','ZoomLevel',ComboBox_ZoomLevel.text);
  FIniFile.WriteString('MapData','Imagery',ComboBox_Imagery.text);
  FIniFile.WriteString('MapData','GeoData',ComboBox_GEO.text);
  FIniFile.WriteString('MapData','MapID',ComboBox_MapID.text);
  FIniFile.WriteString('MapData','MapType',ComboBox_MapType.text);
  FIniFile.WriteString('Condor','Version',ComboBox_Version.text);
  FIniFile.WriteString('Condor','FileNameFormat',ComboBox_FileNameFormat.text);
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Menu_AboutClick(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_UtilitiesClick(Sender: TObject);
begin
  Unit_Utilities.Memo_Message := Memo_Info;
  Unit_Utilities.ProgressBar_Status := ProgressBar_Status;
  Unit_Utilities.ApplicationPath := ApplicationPathName;
  Unit_Utilities.Condor_Folder := CondorPathName;
  Unit_Utilities.Compressor_Folder := CompressorPathName;
  Unit_Utilities.Working_Folder := WorkingPathName;
  Unit_Utilities.LandscapeName := '';
  if (HeaderOpen) AND (TileOpen) then begin
    Unit_Utilities.LandscapeName := CondorLandscapeName;
    Unit_Utilities.Initial_Folder := CondorPathName+'\Landscapes\' + CondorLandscapeName;
  end else begin
    if (CondorPathName <> '') then begin
      Unit_Utilities.Initial_Folder := CondorPathName+'\Landscapes';
    end else begin
      Unit_Utilities.Initial_Folder := '';
    end;
  end;
  Unit_Utilities.WGETfolder := WgetPathName; // for Wget
  Unit_Utilities.library_Folder := GDALpathName;
  Unit_Utilities.ZoomLevel := ComboBox_ZoomLevel.text;
  Unit_Utilities.TileName := ComboBox_Single.text;
  Unit_Utilities.OutputTileSize := ComboBox_TileSize.text;
  Unit_Utilities.DXT_Type := ComboBox_DXT.Text;
  u_X_CX.oTreeView := Unit_Objects.Form_Objects.TreeView_Object;
  u_MakeGMID.GMIDProgramsfolder := DownloaderPathName;
  u_MakeGMID.GMIDMapID := ComboBox_MapID.Text;
  u_MakeGMID.GMIDMapType := ComboBox_MapType.Text;
  Unit_Utilities.opVersion:= ComboBox_Version.text;

//  Form_Utilities.Position := poDesigned;
//  Form_Utilities.Position := poDefault;
  // offset to be able to see status and progressbar
  Form_Utilities.Left := Self.Left + ProgressBar_Status.left + ProgressBar_Status.width + 10;
  Form_Utilities.Top  := Self.Top + 0;
  Form_Utilities.Height  := 630;

  Form_Utilities.ShowModal;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_DEMClick(Sender: TObject);
begin
  if (LandscapeSelected) then begin

    if (NOT ((HeaderOpen) AND (TileOpen))) then begin
      if MessageDlg('Proceed with "' + CondorLandscapeName + '" landscape ? (else create a new landscape folder first)', mtConfirmation,
        [mbYes, mbNo], 0) = mrNo then begin
        Exit;
      end;
    end;

    form_DEM.Caption := 'DEM Helper - '+ CondorLandscapeName;

    // make label with Condor version
    Unit_DEM.CondorVersion := ComboBox_Version.text;
    form_DEM.GroupBox_Condor.Caption := format('Condor Scenery ( Version %s )',[Unit_DEM.CondorVersion]);

    // provide access to the memo box for messages
    Unit_DEM.Memo_Message := Memo_Info;
    Unit_DEM.ProgressBar_Status := ProgressBar_Status;
    Unit_DEM.library_Folder := GDALpathName;
    Unit_DEM.File_Folder := WorkingPathName+'\DEM';
    Unit_DEM.CurrentLandscape := CondorLandscapeName;
    Unit_DEM.sZipFolder := sZipPathName; // for 7-zip
    Unit_DEM.WGETfolder := WgetPathName; // for Wget
    Unit_DEM.ApplicationPath := ApplicationPathName;
    Unit_Utilities.LEfolder := CondorLEpathName; // for LandscapeEditor hashes

//    Form_DEM.Position := poDefault;
    // offset to be able to see status and progressbar
    Form_DEM.Left := Self.Left + ProgressBar_Status.left + ProgressBar_Status.width + 10;
    Form_DEM.Top  := Self.Top + 0;

    form_DEM.ShowModal;

  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_MergeClick(Sender: TObject);
begin
  Unit_Merge.Memo_Message := Memo_Info;
  Unit_Merge.ProgressBar_Status := ProgressBar_Status;
//  Unit_Merge.ApplicationPath := ApplicationPathName;
  Unit_Merge.Condor_Folder := CondorPathName;
//  Unit_Merge.Compressor_Folder := CompressorPathName;
//  Unit_Merge.Working_Folder := WorkingPathName;
    Unit_Merge.mgVersion:= ComboBox_Version.text;
//  if (HeaderOpen) AND (TileOpen) then begin
  if (((HeaderOpen) AND (TileOpen)) OR
      (fileExists(CondorPathName+'\Landscapes\'+CondorLandscapeName+'\MERGE.txt'))) then begin
    Unit_Merge.LandscapeName := CondorLandscapeName;
//    Unit_Merge.Initial_Folder := CondorPathName+'\Landscapes\' + CondorLandscapeName;
  end else begin
    Unit_Merge.LandscapeName := '';
//    if (CondorPathName <> '') then begin
//      Unit_Merge.Initial_Folder := CondorPathName+'\Landscapes';
//    end else begin
//      Unit_Merge.Initial_Folder := '';
//    end;
  end;
//  Unit_Merge.library_Folder := GDALpathName;
//  Unit_Merge.ZoomLevel := ComboBox_ZoomLevel.text;
//  Unit_Merge.TileName := ComboBox_Single.text;
//  Unit_Merge.OutputTileSize := ComboBox_TileSize.text;
//  Unit_Merge.DXT_Type := ComboBox_DXT.Text;
  u_X_CX.oTreeView := Unit_Objects.Form_Objects.TreeView_Object;
  Unit_Merge.LandscapeList := ComboBox_Landscape.Items;

//  Form_Merge.Position := poDefault;
  // offset to be able to see status and progressbar
  Form_Merge.Left := Self.Left + ProgressBar_Status.left + ProgressBar_Status.width + 10;
  Form_Merge.Top  := Self.Top + 0;
  Form_Merge.ShowModal;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_SimpleObjectsClick(Sender: TObject);
begin
  if ( NOT DirectoryExists(ApplicationPathName+'\SimpleObjects')) then begin
    Form_Main.Memo_Info.Lines.Add('SimpleObjects folder not found');
    Beep; Exit;
  end;
  Unit_SimpleObjects.ApplicationPath := ApplicationPathName;
  Unit_SimpleObjects.ObjectFolder := ApplicationPathName+'\SimpleObjects';
  if (HeaderOpen) AND (TileOpen) then begin
    if (Unit_SimpleObjects.objFolderOpen <> true) then begin
      Unit_SimpleObjects.objFolderOpen := true;
      Unit_SimpleObjects.objFolder := CondorPathName+'\Landscapes\'+CondorLandscapeName+'\Working\Objects';
      Form_SimpleObjects.InitDetailGrid;
      ForceDirectories(Unit_SimpleObjects.objFolder);
      u_MakeDDS.CompressorFolder := CompressorPathName;
      MakeDDS_Object_Drop(Unit_SimpleObjects.objFolder, 'Make_DDS.bat', 'DXT1');
    end;
  end else begin
    Unit_SimpleObjects.objFolder := CondorPathName+'\Landscapes';
  end;
  Unit_SimpleObjects.CondorFolder := CondorPathName;
  Unit_SimpleObjects.WorkingFolder := WorkingPathName;
  Unit_SimpleObjects.Memo_Message := Memo_Info;
//  Form_SimpleObjects.Position := poDefault;
  // offset to be able to see status and progressbar
  Form_SimpleObjects.Left := Self.Left + ProgressBar_Status.left + ProgressBar_Status.width + 10;
  Form_SimpleObjects.Top  := Self.Top + 0;
  Form_SimpleObjects.ShowModal;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.ComboBox_VersionChange(Sender: TObject);
begin
  // assume default
  ComboBox_FileNameFormat.text := 'XXYY';
  u_TileList.TileNameMode := xxyy;
  GroupBox_FileNameFormat.Enabled := false;
  // check for format allowance
  if (ComboBox_Version.text = 'V3') then begin
    if ((RowCount>100*64) or (ColumnCount>100*64)) then begin
      ComboBox_FileNameFormat.text := 'XXXYYY';
      u_TileList.TileNameMode := xxxyyy;
    end else begin
      if (FileExists(WorkingPathName+'\..\HeightMaps\h000000.tr3')) then begin
        ComboBox_FileNameFormat.text := 'XXXYYY';
        u_TileList.TileNameMode := xxxyyy;
      end else begin
        if (FileExists(WorkingPathName+'\..\HeightMaps\h0000.tr3')) then begin
        end else begin // format not yet established
          GroupBox_FileNameFormat.Enabled := true;
        end;
      end;
    end;
  end else begin
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.ComboBox_FileNameFormatChange(Sender: TObject);
begin
  if (ComboBox_FileNameFormat.Text = 'XXYY') then begin
    u_TileList.TileNameMode := xxyy;
  end else begin
    u_TileList.TileNameMode := xxxyyy;
  end;
end;

//---------------------------------------------------------------------------
function FindNearestBinary(tileSize :string) : string;
var
  Temp : single;
begin
  try // avoid potential runtime error
    Temp := strtofloat(tileSize);
  except
    Temp := 2048;
  end;
  Temp := logN(2,Temp);
  Temp := power(2,trunc(Temp+0.5));
  if (Temp < 256) then begin
    result := '256';
  end else begin
    if (Temp >  131072) then begin
      result := '131072';
    end else begin
      result := Format('%1.0f',[Temp]);
    end;
  end;
end;

//---------------------------------------------------------------------------
function FindNearestBinaryExponent(zoomLevel :string) : string;
var
  Temp : single;
begin
  try // avoid potential runtime error
    Temp := strtofloat(zoomLevel);
  except
    Temp := 12;
  end;
  if (Temp < 10) then begin
    result := '10';
  end else begin
    if (Temp >  20) then begin
      result := '20';
    end else begin
      result := Format('%1.0f',[Temp]);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Main.ComboBox_TileSizeExit(Sender: TObject);
var
  Nearest : string;
begin
  Nearest := FindNearestBinary(ComboBox_TileSize.Text);
//  if (Nearest <> ComboBox_TileSize.Text) then begin
    ComboBox_TileSize.Text := Nearest;
//  end;
  ComboBox_TileSize.hint := format('%1.3f m/pixel',[CalcCondorResolution(ComboBox_TileSize.Text)]);
end;

// also need to call this function when reading landscape header to re-calc with latitude ??? TBD
//---------------------------------------------------------------------------
procedure TForm_Main.ComboBox_ZoomLevelExit(Sender: TObject);
var
  Nearest : string;
begin
  Nearest := FindNearestBinaryExponent(ComboBox_Zoomlevel.Text);
//  if (Nearest <> ComboBox_Zoomlevel.Text) then begin
    ComboBox_Zoomlevel.Text := Nearest;
//  end;
  ComboBox_ZoomLevel.hint := format('%1.3f m/pixel',[CalcZoomResolution(ComboBox_ZoomLevel.Text,0.0)]);
end;

//---------------------------------------------------------------------------
procedure TForm_Main.Button_WarpCropClick(Sender: TObject);
begin
end;

//---------------------------------------------------------------------------
end.

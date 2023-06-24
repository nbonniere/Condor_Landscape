{
 * Unit_Merge.pas
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
unit Unit_Merge;

//===========================================================================
INTERFACE

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, comctrls, ExtCtrls, CheckLst;

type
  TForm_Merge = class(TForm)
    GroupBox_Landscapes: TGroupBox;
    Image_Grid: TImage;
    Button_Check: TButton;
    GroupBox_New: TGroupBox;
    Edit_Name: TEdit;
    Button_Create: TButton;
    Button_Merge: TButton;
    CheckListBox_LandscapeList: TCheckListBox;
    GroupBox_Reference: TGroupBox;
    Label_Latitude: TLabel;
    Label_Longitude: TLabel;
    Edit_Latitude: TEdit;
    Edit_Longitude: TEdit;
    GroupBox_CropExpand: TGroupBox;
    Edit_Top: TEdit;
    Edit_Bottom: TEdit;
    Edit_Left: TEdit;
    Edit_Right: TEdit;
    GroupBox_Defaults: TGroupBox;
    Label_Elevation: TLabel;
    Edit_Elevation: TEdit;
    procedure Button_CreateClick(Sender: TObject);
    procedure Button_CheckClick(Sender: TObject);
    procedure Button_MergeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Data_Change(Sender: TObject);
    procedure Edit_NameChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form_Merge: TForm_Merge;

var
  Memo_Message : TMemo;  // external TMemo for messages
  ProgressBar_Status : TProgressBar;
// it seems that if you use a file dialog, the current directory is changed to
// where the folder is for the selected file. So use the Application path
// instead of getcurrent dir, or a '.\' prefix
//  ApplicationPath : string;
//  Initial_Folder : string;   // external path for file
//  Working_Folder : string;   // external path for file
  Condor_Folder : string;    // external path for file
//  Compressor_Folder : string;    // external path for file
//  Library_Folder : string;   // external path for file
  LandscapeName : string;    // external path for file
//  ZoomLevel : string;
//  TileName : string;
//  OutputTileSize : string;
//  File_Name : string;   // external name for file
//  DXT_Type : string;
  LandscapeList : Tstrings;

//===========================================================================
IMPLEMENTATION

{$R *.DFM}

uses
  FileCtrl,
  Unit_DEM,
  u_UTM, u_Terrain, u_Thermal, u_BMP, u_DXT,
  u_Object, u_Airport, u_INI, u_CUP;

type
  Merge_Type = record
    Name   : string;
    Header : CondorTerrainHeader;
    qtX : single;
    qtY : single;
  end;

const
  faNormalFile = 0; // for use with FindFirst FindNext
  // instead of faAnyFile which includes directories and hidden, etc...

var
  mBitmap : TBitmap;
  Rows, Columns : single;      // tiles.  Change to quarter-tiles ?
  Merge_Count : Integer;
  Merge_Array : array of Merge_Type;
  ce_East, ce_West, ce_North, ce_South : integer;

var
  oe : Extents; // overall extents
  ce : Extents; // crop/expand extents

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
Procedure Save_Merge_List;
var
  i : integer;
  Temp_File : TextFile;
begin
  if (NOT DirectoryExists(Condor_Folder+'\Landscapes\'+LandscapeName)) then begin
    ForceDirectories(Condor_Folder+'\Landscapes\'+LandscapeName);
  end;

  AssignFile(Temp_file, Condor_folder+'\Landscapes\'+LandscapeName+'\MERGE.txt');
  Rewrite(Temp_file);
  writeln(Temp_file, Form_Merge.Edit_Right.text);
  writeln(Temp_file, Form_Merge.Edit_Left.text);
  writeln(Temp_file, Form_Merge.Edit_Top.text);
  writeln(Temp_file, Form_Merge.Edit_Bottom.text);
  writeln(Temp_file, Form_Merge.Edit_Elevation.text);
  writeln(Temp_file, Form_Merge.Edit_Latitude.text);
  writeln(Temp_file, Form_Merge.Edit_Longitude.text);
  with Form_Merge.CheckListBox_LandscapeList do begin
    for i := 0 to Items.Count-1 do begin
      if (Checked[i]) then begin
        writeln(Temp_file, Items[i]);
      end;
    end;
  end;
  CloseFile(Temp_file);
end;

{----------------------------------------------------------------------------}
Procedure Read_Merge_List;
var
  i : integer;
  Temp_File : TextFile;
  TempSTR : string;
begin
  if (FileExists(Condor_Folder+'\Landscapes\'+LandscapeName+'\MERGE.txt')) then begin
    Form_Merge.Edit_Name.text := LandscapeName;
    AssignFile(Temp_file, Condor_Folder+'\Landscapes\'+LandscapeName+'\MERGE.txt');
    Reset(Temp_file);
    readln(Temp_file, TempSTR);
    Form_Merge.Edit_Right.text := TempSTR;
    readln(Temp_file, TempSTR);
    Form_Merge.Edit_Left.text := TempSTR;
    readln(Temp_file, TempSTR);
    Form_Merge.Edit_Top.text := TempSTR;
    readln(Temp_file, TempSTR);
    Form_Merge.Edit_Bottom.text := TempSTR;
    readln(Temp_file, TempSTR);
    Form_Merge.Edit_Elevation.text := TempSTR;
    readln(Temp_file, TempSTR);
    Form_Merge.Edit_Latitude.text := TempSTR;
    readln(Temp_file, TempSTR);
    Form_Merge.Edit_Longitude.text := TempSTR;
    with Form_Merge.CheckListBox_LandscapeList do begin
      While (NOT EOF(Temp_File)) do begin
        Readln(Temp_file, TempSTR);
        // search list and tick box if match
        for i := 0 to Items.Count-1 do begin
          if (Items[i] = TempSTR) then begin
            Checked[i] := true;
            Break;
          end;
        end;
      end;
    end;
    CloseFile(Temp_file);
  end else begin
    Form_Merge.Edit_Name.text := '';
    LandscapeName := Form_Merge.Edit_Name.text;
  end;
end;

{----------------------------------------------------------------------------}
procedure TForm_Merge.Button_CreateClick(Sender: TObject);
begin
  if (LandscapeName <> '') then begin
//    if (DirectoryExists(Condor_folder+'\Landscapes\'+LandscapeName)) then begin
//      MessageShow('Landscape already exists');
//      beep; Exit;
//    end;
    // create landscape folder
    ForceDirectories(Condor_folder+'\Landscapes\'+LandscapeName);
    // create an initial dummy .trn file.
    WriteTerrainHeader(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.trn');
    // create an initial dummy .tdm file.
MessageShow(Format('W= %d, H= %d',[u_Terrain.TerrainHeader.twidth,u_Terrain.TerrainHeader.tHeight]));
    WriteTDMHeader(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.tdm');
    // create an initial dummy .bmp file.
    WriteBMP24Header(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.bmp');
    // save landscape list as a 'merge flag'
    Save_Merge_List;
    Button_Merge.Enabled := true;
  end else begin
    MessageShow('No Landscape name specified');
    beep;
  end;
end;

{----------------------------------------------------------------------------}
procedure TForm_Merge.Button_CheckClick(Sender: TObject);
var
  i : integer;
  xDelta : single;
  yDelta : single;
  AbleToMerge : boolean;
  LatRef, LongRef : double;

{ - - - - - - - - - - - - - - - - - - - - - - }
Procedure Get_Reference;
begin
  try
    LatRef := StrToFloat(Edit_Latitude.text);
    LongRef := StrToFloat(Edit_Longitude.text);

    // validate
    if ( (LatRef > 84) OR (LatRef < -80) ) then begin
//      MessageShow('DEM: Latitude entry error');
//      Beep;
      exit;
    end;
    if ( (LongRef > 180) OR (LongRef < -180) ) then begin
//      MessageShow('DEM: Longitude entry error');
//      Beep;
      exit;
    end;

    // check UTM zone
    CalcUTMzone(LatRef, LongRef, 0);
    // allow range +1 or -1
     // need to check wrap-around ???
    if (uGrid <> u_Terrain.TerrainHeader.tUTMgrid[0])
     OR (DifferenceUTMzone(StrToInt(uZone), u_Terrain.TerrainHeader.tUTMzone) >  1)
     OR (DifferenceUTMzone(StrToInt(uZone), u_Terrain.TerrainHeader.tUTMzone) < -1)
    then begin
//      MessageShow('Zone mismatch');
//      Beep;
       Exit;
    end;

    // convert to UTM
    LatLongToUTM(LatRef, LongRef, IntToStr(u_Terrain.TerrainHeader.tUTMzone), uGrid);
    ReferencePoint.X := round(uEasting); ReferencePoint.Y := round(uNorthing); // reference point

    ReferencePointDefined:= true;
  except
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - }
function Get_CropExpand : Boolean;
begin
  Result := false;
  try
    ce_East := StrToInt(Edit_Right.text);
    ce_West := StrToInt(Edit_Left.text);
    ce_North := StrToInt(Edit_Top.text);
    ce_South := StrToInt(Edit_Bottom.text);

    // validate
    if ((ce_East <> 0) OR (ce_West <> 0) OR
        (ce_North <> 0) OR (ce_South <> 0)) then begin
      Result := true;
    end;
  except
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - }
begin
  MessageClear;
  AbleToMerge := false;
  Merge_Count := 0;
  // create a merge array
  with Form_Merge.CheckListBox_LandscapeList do begin
    for i := 0 to Items.Count-1 do begin
      if (Checked[i]) then begin
        INC(Merge_Count);
        SetLength(Merge_Array,Merge_Count);
        Merge_Array[Merge_Count-1].Name := Items[i];
      end;
    end;
  end;

  if (Merge_Count <= 0) then begin
    MessageShow('No landscapes selected');
    Beep; Exit;
  end;

  // read headers
  for i := 0 to Merge_Count-1 do begin
    with Merge_Array[i] do begin
    ReadTerrainHeader(Condor_folder+'\Landscapes\'+Name+'\'+Name+'.trn');
      with u_Terrain.TerrainHeader do begin
        Header := u_Terrain.TerrainHeader;
      end;
    end;
  end;

  // prepare new terrain header
  u_Terrain.TerrainHeader := Merge_Array[0].Header;

  if (Merge_Count > 1) then begin
    // check zones
    for i := 1 to Merge_Count-1 do begin
      with Merge_Array[i].Header do begin
        if (tUTMzone <> u_Terrain.TerrainHeader.tUTMzone)
         OR (tUTMgrid[0] <> u_Terrain.TerrainHeader.tUTMgrid[0]) then begin
          MessageShow('Zone mismatch');
          Beep; Exit;
        end;
      end;
    end;
    MessageShow('Zone match');

    // check calibration
    for i := 1 to Merge_Count-1 do begin
      with Merge_Array[i].Header do begin
        if (tResolution <> u_Terrain.TerrainHeader.tResolution)
         OR (tDeltaX <> u_Terrain.TerrainHeader.tDeltaX)
         OR (tDeltaY <> u_Terrain.TerrainHeader.tDeltaY) then begin
          MessageShow('Calibration mismatch');
          Beep; Exit;
        end;
      end;
    end;
    MessageShow('Calibration match');

    // check UTM grid for multiple of quarter tile
    for i := 1 to Merge_Count-1 do begin
      with Merge_Array[i].Header do begin
        xDelta := abs(tRightMapEasting) - abs(u_Terrain.TerrainHeader.tRightMapEasting);
        yDelta := abs(tBottomMapNorthing) - abs(u_Terrain.TerrainHeader.tBottomMapNorthing);
        if (trunc(xDelta) MOD 256*90 <> 0)
         OR (trunc(yDelta) MOD 256*90 <> 0) then begin
          MessageShow('UTM grid mismatch');
          Beep; Exit;
        end;
      end;
    end;
    MessageShow('UTM grid match');
  end;
  // check maximum quarter-tile qt max indexes X=0..99 and Y=0..99
  with Merge_Array[0].Header do begin
    oe.xMax := tRightMapEasting;
    oe.xMin := oe.xMax + tWidth * tDeltaX;
    oe.yMin := tBottomMapNorthing;
    oe.yMax := oe.yMin + tHeight * tDeltaY;
  end;
  for i := 1 to Merge_Count-1 do begin
    with Merge_Array[i].Header do begin
      if (oe.xMax < tRightMapEasting) then begin
        oe.xMax := tRightMapEasting;
      end;
      if (oe.xMin > tRightMapEasting + tWidth * tDeltaX) then begin
        oe.xMin := tRightMapEasting + tWidth * tDeltaX;
      end;
      if (oe.yMin > tBottomMapNorthing) then begin
        oe.yMin := tBottomMapNorthing;
      end;
      if (oe.yMax < tBottomMapNorthing + tHeight * tDeltaY) then begin
        oe.yMax := tBottomMapNorthing + tHeight * tDeltaY;
      end;
    end;
  end;

  if (Get_CropExpand) then begin
    CropAreaPointsDefined:= true;
    ce.xMax := oe.xMax + ce_East * -u_Terrain.TerrainHeader.tDeltaX * (256 DIV 4);
    ce.xMin := oe.xMin - ce_West * -u_Terrain.TerrainHeader.tDeltaX * (256 DIV 4);
    ce.yMax := oe.yMax + ce_North * u_Terrain.TerrainHeader.tDeltaY * (256 DIV 4);
    ce.yMin := oe.yMin - ce_South * u_Terrain.TerrainHeader.tDeltaY * (256 DIV 4);
    if (ce_East > 0) then begin
      oe.xMax := ce.xMax;
    end;
    if (ce_West > 0) then begin
      oe.xMin := ce.xMin;
    end;
    if (ce_North > 0) then begin
      oe.yMax := ce.yMax;
    end;
    if (ce_South > 0) then begin
      oe.yMin := ce.yMin;
    end;
  end else begin
    CropAreaPointsDefined := false;
    ce := oe;
  end;

  Columns := trunc((ce.xMax - ce.xMin) / -u_Terrain.TerrainHeader.tDeltaX) DIV (256 DIV 4);
  Rows    := trunc((ce.yMax - ce.yMin) /  u_Terrain.TerrainHeader.tDeltaY) DIV (256 DIV 4);
  if ( Columns > (99+1))
   OR ( Rows > (99+1)) then begin
    MessageShow('WARNING: X, Y index > 25');
//    Beep; Exit;     // allow but warn !
  end;
  Columns := Columns / 4;  // need tiles not qt
  Rows := Rows / 4;        // need tiles not qt
  MessageShow(Format('X= %0.2f, Y= %0.2f',[Columns,Rows]));

  AbleToMerge := true;

  // create quarter-tile offsets, relative to overall
  for i := 0 to Merge_Count-1 do begin
    with Merge_Array[i] do begin
      Merge_Array[i].qtX := (Header.tRightMapEasting - oe.xMax) / -Header.tDeltaX / (256/4);
      Merge_Array[i].qtY := (Header.tBottomMapNorthing - oe.yMin) / Header.tDeltaY / (256/4);
    end;
  end;

  // update new terrain header
  with u_Terrain.TerrainHeader do begin
    tRightMapEasting := ce.xMax;                    // UTM absolute Easting, bottom right
    tBottomMapNorthing := ce.yMin;                  // UTM absolute Northing, bottom right
    tWidth :=  trunc((ce.xMax - ce.xMin) / -tDeltaX);  // number of columns
    tHeight := trunc((ce.yMax - ce.yMin) /  tDeltaY);  // number of rows
    // prepare TDM header
    with u_Thermal.TDM_Header do begin
      Width := tWidth;
      Height := tHeight;
    end;
    // prepare BMP header
    with xBitmapHeader_24bitColor do begin
      bDib.bWidth := tWidth;
      bDib.bHeight := tHeight;
      bDib.bImageByteSize := tWidth*tHeight*xColor24Size div 8;
      bH.bFileByteSize := bDib.bImageByteSize+bH.bPixelArrayOffset;
    end;
  end;

  if (AbleToMerge) then begin  // Draw grid and areas
    // Draw grid
    SetLength(Areas,Merge_Count+2); // extra 2 for overall and flying extents
    // area 0 is overall extents. Top-left and Bottom-Right
    Areas[0][0].X := round(oe.xMin); Areas[0][0].Y := round(oe.yMax);
    Areas[0][2].X := round(oe.xMax); Areas[0][2].Y := round(oe.yMin);
//   Areas[0][1].X := Areas[0][2].X; Areas[0][1].Y := Areas[0][0].Y;
//   Areas[0][3].X := Areas[0][0].X; Areas[0][3].Y := Areas[0][2].Y;

    // area 1 is flyable extents
    Areas[1][0].X := round(ce.xMin + -u_Terrain.TerrainHeader.tDeltaX * (256 DIV 4));
    Areas[1][0].Y := round(ce.yMax -  u_Terrain.TerrainHeader.tDeltaY * (256 DIV 4));
    Areas[1][2].X := round(ce.xMax - -u_Terrain.TerrainHeader.tDeltaX * (256 DIV 4));
    Areas[1][2].Y := round(ce.yMin +  u_Terrain.TerrainHeader.tDeltaY * (256 DIV 4));
    Areas[1][1].X := Areas[1][2].X; Areas[1][1].Y := Areas[1][0].Y;
    Areas[1][3].X := Areas[1][0].X; Areas[1][3].Y := Areas[1][2].Y;

    // draw areas
    AreaPointsDefined := false;
    for i := 0 to Merge_Count-1 do begin
      with Merge_Array[i].Header do begin
        AreaPointsDefined := true;
        Areas[i+2][0].X := round(tRightMapEasting + tWidth * tDeltaX);
        Areas[i+2][0].Y := round(tBottomMapNorthing + tHeight * tDeltaY);
        Areas[i+2][2].X := round(tRightMapEasting);
        Areas[i+2][2].Y := round(tBottomMapNorthing);
        Areas[i+2][1].X := Areas[i+2][2].X; Areas[i+2][1].Y := Areas[i+2][0].Y;
        Areas[i+2][3].X := Areas[i+2][0].X; Areas[i+2][3].Y := Areas[i+2][2].Y;
      end;
    end;

    // add reference if available
    ReferencePointDefined:= false;
    Get_Reference;

    // add crop area if available
//    if (Get_CropExpand) then begin
    if (CropAreaPointsDefined) then begin
      i := length(Areas) + 1;
      SetLength(Areas,i);
      with u_Terrain.TerrainHeader do begin
//        Areas[i-1][0].X := Areas[0][0].X +trunc(ce_West *64*tDeltaX);  // min X
//        Areas[i-1][0].Y := Areas[0][0].Y +trunc(ce_North*64*tDeltaY);  // max Y
//        Areas[i-1][2].X := Areas[0][2].X -trunc(ce_East *64*tDeltaX);  // max X
//        Areas[i-1][2].Y := Areas[0][2].Y -trunc(ce_South*64*tDeltaY);  // min Y
    Areas[i-1][0].X := round(ce.xMin); Areas[i-1][0].Y := round(ce.yMax);
    Areas[i-1][2].X := round(ce.xMax); Areas[i-1][2].Y := round(ce.yMin);
        Areas[i-1][1].X := Areas[i-1][2].X; Areas[i-1][1].Y := Areas[i-1][0].Y;
        Areas[i-1][3].X := Areas[i-1][0].X; Areas[i-1][3].Y := Areas[i-1][2].Y;
      end;
//      CropAreaPointsDefined:= true;
    end;

    // need to show overall columns, rows
    Columns := trunc((oe.xMax - oe.xMin) / -u_Terrain.TerrainHeader.tDeltaX) DIV (256 DIV 4) /4;
    Rows    := trunc((oe.yMax - oe.yMin) /  u_Terrain.TerrainHeader.tDeltaY) DIV (256 DIV 4) /4;
    Form_DEM.ShowGrid(mBitmap, Columns, Rows);

    Button_Create.Enabled := true;
  end;
end;

{----------------------------------------------------------------------------}
procedure TForm_Merge.Edit_NameChange(Sender: TObject);
begin
  LandscapeName := Edit_Name.Text;
end;

type
  IndexFile_Type = (Type_TR3, Type_FOR, Type_DDS);

{----------------------------------------------------------------------------}
procedure Create_Dummy_Files(IF_Type : IndexFile_Type;
  FilePath, Name : string);
var
  Dummy_File : File of Byte;
  TRN_File : File of Byte;
  i,j : integer;
  File_Prefix, File_Ext, File_Folder : string;
  File_Name : string;
  FileBytes : longint;
  ByteCount : longint;
  P : PByteArray;
begin
  case IF_Type of
    Type_TR3: begin
      File_Prefix := 'h';
      File_Ext := '.tr3';
      File_Folder := 'HeightMaps';
      FileBytes := 193*193*2;
    end;
    Type_FOR: begin
      File_Prefix := '';
      File_Ext := '.for';
      File_Folder := 'ForestMaps';
      FileBytes := 512*512;
    end;
    Type_DDS: begin
//      File_Prefix := 't';
      File_Ext := '.dds';
      File_Folder := 'Textures';
      // create 'empty.dds'
      DXT_MakeEmpty(FilePath+'\'+File_Folder+'\empty'+File_Ext);
      Exit;
    end;
    else begin
      Beep; Exit;
    end;
  end;
  AssignFile(TRN_File,FilePath+'\'+Name+'.trn');
  Reset(TRN_File);
  BlockRead(TRN_File,TerrainHeader,sizeof(CondorTerrainHeader));

 try
  P := AllocMem(512); // initial 0's

  with TerrainHeader do begin
    ProgressBar_Status.Max := tWidth div 64;
    for i := 0 to (tWidth div 64)-1 do begin
      for j := 0 to (tHeight div 64)-1 do begin
        File_Name := FilePath+'\'+File_Folder+'\'+
          format('%s%2.2d%2.2d%s',[File_Prefix,i,j,File_Ext]);
        if (NOT FileExists(File_Name)) then begin
          AssignFile(Dummy_File,File_Name);
          Rewrite(Dummy_File);
          ByteCount := FileBytes;
          while (ByteCount > 0) do begin
            if (ByteCount > 512) then begin
              BlockWrite(Dummy_File,P^,512);
            end else begin
              BlockWrite(Dummy_File,P^,ByteCount);
            end;
            DEC(ByteCount,512);
          end;
          CloseFile(Dummy_File);
        end;
      end;
      ProgressBar_Status.StepIt;
      Application.ProcessMessages;
    end;
  end;

 finally
   freemem(P);
 end;

  CloseFile(TRN_File);
  ProgressBar_Status.Position := 0;
end;

{----------------------------------------------------------------------------}
procedure Copy_ReIndex_Files(IF_Type : IndexFile_Type;
  Offset_X, Offset_Y, Min_X, Max_X, Min_Y, Max_Y : LongInt;
  FilePath,FilePath_a,Name_a : string);
var
  TRN_File_a : File of Byte;      // use Word instead of Byte and terrain-size ???
  i,j : integer;
  File_Prefix, File_Ext, File_Folder : string;
  File_Name, File_Name_a : string;
begin
  case IF_Type of
    Type_TR3: begin
      File_Prefix := 'h';
      File_Ext := '.tr3';
      File_Folder := 'HeightMaps';
    end;
    Type_FOR: begin
      File_Prefix := '';
      File_Ext := '.for';
      File_Folder := 'ForestMaps';
    end;
    Type_DDS: begin
      File_Prefix := 't';
      File_Ext := '.dds';
      File_Folder := 'Textures';
    end;
    else begin
      Beep; Exit;
    end;
  end;
  AssignFile(TRN_File_a,FilePath_a+'\'+Name_a+'.trn');
  Reset(TRN_File_a);
  BlockRead(TRN_File_a,TerrainHeader,sizeof(CondorTerrainHeader));

  ForceDirectories(FilePath+'\'+File_Folder);
  with TerrainHeader do begin
    ProgressBar_Status.Max := tWidth div 64;
    for i := 0 to (tWidth div 64)-1 do begin
      for j := 0 to (tHeight div 64)-1 do begin
        // check crop limits - TBD, must be a better way ???
        if ( (i+(-Offset_X) >= Min_X) AND (i+(-Offset_X) < Max_X) AND
             (j+  Offset_Y  >= Min_Y) AND (j+  Offset_Y  < Max_Y) ) then begin
//        if (true) then begin
//          File_Name := format('%s%2.2d%2.2d%s',[File_Prefix,i+(-Offset_X),j+Offset_Y,File_Ext]);
          File_Name := format('%s%2.2d%2.2d%s',[File_Prefix,i+(-Offset_X)-Min_X,j+Offset_Y-Min_Y,File_Ext]);
          File_Name_a := format('%s%2.2d%2.2d%s',[File_Prefix,i,j,File_Ext]);
          CopyFile(pchar(FilePath_a+'\'+File_Folder+'\'+File_Name_a),
            pchar(FilePath+'\'+File_Folder+'\'+File_Name),false);
        end;
      end;
      ProgressBar_Status.StepIt;
      Application.ProcessMessages;
    end;
  end;

  CloseFile(TRN_File_a);
  ProgressBar_Status.Position := 0;
end;

{----------------------------------------------------------------------------}
procedure TForm_Merge.Button_MergeClick(Sender: TObject);
var
  i, j : integer;
  FolderName : string;
  NewFolderName : string;
  SearchRec: TSearchRec;
  Crop_Min_X, Crop_Min_Y, Crop_Max_X, Crop_Max_Y : longint;
  df_Elevation : integer;
  FileList : array of string;
  FileCount : integer;

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  Procedure FilterFile(BaseName, FoundName : string);
  var
    i : integer;
  begin
    if (Basename+'.bmp' <> FoundName) then begin
      for i := 0 to FileCount-1 do begin
        if (FileList[i] = FoundName) then begin
          exit; // duplicate, ignore
        end;
      end;
      // FileCount = 0 or not already in list
      INC(FileCount);
      SetLength(FileList,FileCount);
      FileList[FileCount-1] := FoundName;
    end;
  end;

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
begin
  // if .trn terrain file already created externally, i.e. check size
  // then nothing to do (.trn is not flattened, only .tr3 are flattened)
  // else fill with default height and copy data over from other Landscapes
  MessageClear;
  MessageShow('Merging terrain');
  u_Terrain.Memo_Message := Memo_Message;
  u_Terrain.ProgressBar_Status := ProgressBar_Status;
  // first force size
  ForceTerrainSize(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.trn');
  // set up crop range, relative to overall ??? must be a better way ???
  Crop_Min_X := 0;
  if (ce_East < 0) then begin
    Crop_Min_X := Crop_Min_X -ce_East;
  end;
  Crop_Max_X := Crop_Min_X + TerrainHeader.tWidth div 64;
  Crop_Min_Y := 0;
  if (ce_South < 0) then begin
    Crop_Min_Y := Crop_Min_Y -ce_South;
  end;
  Crop_Max_Y := Crop_Min_Y + TerrainHeader.tHeight div 64;

  // now merge
  for i := 0 to Merge_Count-1 do begin
    with Merge_Array[i] do begin
      Merge_TRN_File(trunc(qtX) * (256 div 4),
                     trunc(qtY) * (256 div 4),
                     Crop_Min_X * (256 div 4),
                     Crop_Max_X * (256 div 4),
                     Crop_Min_Y * (256 div 4),
                     Crop_Max_Y * (256 div 4),
        Condor_folder+'\Landscapes\'+LandscapeName,LandscapeName+'.trn',
        Condor_folder+'\Landscapes\'+Name,Name+'.trn');
    end;
  end;

  // 'HeightMaps' folder
  // need to offset XY tile indexes/names
  // if new .tr3 files already created separately, then only copy .tr3 files under airports
  // that obviously have been flatened
  // else copy all .tr3 files from other landscapes and fill in the rest with default
  // could use symbolic links instead of copies ???
  MessageShow('Merging HeightMaps');
  for i := 0 to Merge_Count-1 do begin
    with Merge_Array[i] do begin
      Copy_ReIndex_Files(Type_TR3,
        trunc(qtX), trunc(qtY), Crop_Min_X, Crop_Max_X, Crop_Min_Y, Crop_Max_Y,
        Condor_folder+'\Landscapes\'+LandscapeName,
        Condor_folder+'\Landscapes\'+Name, Name);
    end;
  end;

  // then add dummy missing files for empty areas
  // fix seams after to also make a good transition edge
  MessageShow('Adding missing tiles');
  // use a user defined default elevation
  df_Elevation := StrToInt(Edit_Elevation.text);
  // duplicate TR3 edge and fade to 0 (or other) for smoother transition  ???
  // for now just add tiles of 0s
//  Create_Dummy_Files(Type_TR3,
//    Condor_folder+'\Landscapes\'+LandscapeName, LandscapeName);
  // use a user defined default elevation
  df_Elevation := StrToInt(Edit_Elevation.text);
//  u_Terrain.Memo_Message := Memo_Message;
//  u_Terrain.ProgressBar_Status := ProgressBar_Status;
  Create_Dummy_TR3_Files(df_Elevation,
    Condor_folder+'\Landscapes\'+LandscapeName, LandscapeName);

// !!! need to fix 'duplicate' tr3 before fixing seams ???
// do fix at end after executing a batch file for duplicate tr3, for, dds ???

  // if new .tr3 files already created separately, then there should be no seams to fix
  // else fix seams, i.e. fix the overlap
  // for top edge tiles, if there is a tile already there above, then copy the overlap
  // for left edge tiles, if there is a tile already there to the left, then copy the overlap
  // for bottom edge tiles, if there is a tile already there below, then copy the overlap
  // for right edge tiles, if there is a tile already there to the right, then copy the overlap
  MessageShow('Fixing seams');
//  u_Terrain.Memo_Message := Memo_Message;
//  u_Terrain.ProgressBar_Status := ProgressBar_Status;
//  for i := 1 to Merge_Count-1 do begin  // no need to do first one, start at 1
  for i := 0 to Merge_Count-1 do begin  // no need to do first one, start at 1
    with Merge_Array[i] do begin
      Fix_TR3_Seams(trunc(qtX) * (256 div 4),
                    trunc(qtY) * (256 div 4),
        Condor_folder+'\Landscapes\'+LandscapeName,LandscapeName+'.trn',
        Condor_folder+'\Landscapes\'+Name,Name+'.trn');
    end;
  end;

  // 'ForestMaps' folder
  // need to offset XY tile indexes/names
  // copy all .for files from landscapes and fill in rest with default blank
  // could use symbolic links instead of copies ???
  MessageShow('Merging ForestMaps');
  for i := 0 to Merge_Count-1 do begin
    with Merge_Array[i] do begin
      Copy_ReIndex_Files(Type_FOR,
        trunc(qtX), trunc(qtY), Crop_Min_X, Crop_Max_X, Crop_Min_Y, Crop_Max_Y,
        Condor_folder+'\Landscapes\'+LandscapeName,
        Condor_folder+'\Landscapes\'+Name, Name);
    end;
  end;
  // then add dummy missing files
  // just add tiles of 0s -> no forest
  Create_Dummy_Files(Type_FOR,
    Condor_folder+'\Landscapes\'+LandscapeName, LandscapeName);

  // merge the 'Textures' folder
  // need to offset XY tile indexes/names
  // could use symbolic links instead of copies ???
  MessageShow('Merging Textures');
  for i := 0 to Merge_Count-1 do begin
    with Merge_Array[i] do begin
      Copy_ReIndex_Files(Type_DDS,
        trunc(qtX), trunc(qtY),  Crop_Min_X, Crop_Max_X, Crop_Min_Y, Crop_Max_Y,
        Condor_folder+'\Landscapes\'+LandscapeName,
        Condor_folder+'\Landscapes\'+Name, Name);
    end;
  end;
  // then add an empty.dds file, to account for any missing tiles
  Create_Dummy_Files(Type_DDS,
    Condor_folder+'\Landscapes\'+LandscapeName, LandscapeName);

  // merge flight planner map bitmaps
  // ??? what about 24 versus 32 bit color ???
  MessageShow('Merging FlightPlanner');
  u_BMP.Memo_Message := Memo_Message;
  u_BMP.ProgressBar_Status := ProgressBar_Status;
  // first force size
  ForceBMP24size(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.bmp');
  // now merge
  for i := 0 to Merge_Count-1 do begin
    with Merge_Array[i] do begin
      Merge_BMP24_File(trunc(qtX) * (256 div 4),
                       trunc(qtY) * (256 div 4),
                       Crop_Min_X * (256 div 4),
                       Crop_Max_X * (256 div 4),
                       Crop_Min_Y * (256 div 4),
                       Crop_Max_Y * (256 div 4),
        Condor_folder+'\Landscapes\'+LandscapeName,LandscapeName+'.bmp',
        Condor_folder+'\Landscapes\'+Name,Name+'.bmp');
    end;
  end;

  // for other map bitmaps, make list of all bitmaps and then merge by matching name
  MessageShow('Merging alternate FlightPlanner(s)');
  // first make list, except for flightplanner, and avoid duplicates
  FileCount := 0;
  for i := 0 to Merge_Count-1 do begin
    FolderName := Condor_folder+'\Landscapes\'+
      Merge_Array[i].Name;
    if (FindFirst(FolderName+'\*.bmp', faNormalFile, SearchRec)) = 0 then begin
      FilterFile(Merge_Array[i].Name, SearchRec.Name);
      while (FindNext(SearchRec) = 0) do begin
        FilterFile(Merge_Array[i].Name, SearchRec.Name);
      end;
      FindClose(SearchRec);
    end;
  end;

  u_BMP.Memo_Message := Memo_Message;
  u_BMP.ProgressBar_Status := ProgressBar_Status;
  // then for each unique filename, merge files with same name
  for j := 0 to FileCount-1 do begin
    // create an initial dummy .bmp file.
    WriteBMP24Header(Condor_folder+'\Landscapes\'+LandscapeName+'\'+FileList[j]);
    // now force its size
    ForceBMP24size(Condor_folder+'\Landscapes\'+LandscapeName+'\'+FileList[j]);
    for i := 0 to Merge_Count-1 do begin
      with Merge_Array[i] do begin
        Merge_BMP24_File(trunc(qtX) * (256 div 4),
                         trunc(qtY) * (256 div 4),
                         Crop_Min_X * (256 div 4),
                         Crop_Max_X * (256 div 4),
                         Crop_Min_Y * (256 div 4),
                         Crop_Max_Y * (256 div 4),
          Condor_folder+'\Landscapes\'+LandscapeName,FileList[j],
          Condor_folder+'\Landscapes\'+Name,FileList[j]);
      end;
    end;
  end;
  MessageShow('Delete incomplete alternate flightplanner(s) as desired');

  // merge .obj files
  MessageShow('Merging Objects');
  u_Object.Memo_Message := Memo_Message;
  // make sure there is no old file
  DeleteFile(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.obj');
  // - ??? how to deal with duplicate names ???
  // remove objects when cropping enabled
  // UTM relative, so merging needs UTM adjustment
  // also merge object folders
  // - ??? how to deal with duplicate texture file names ???
  // for each object in cropped object list
  // - open the object file and search for textures and copy
  // can open file and copy blocks of 152 bytes for each object
  for i := 0 to Merge_Count-1 do begin
    with Merge_Array[i] do begin
//      Append_OBJ_File(qtX * Header.tDeltaX * (256/4),
//                      qtY * Header.tDeltaY * (256/4),
//                      Crop_Min_X * -Header.tDeltaX * (256/4),
//                      Crop_Max_X * -Header.tDeltaX * (256/4),
//                      Crop_Min_Y *  Header.tDeltaY * (256/4),
//                      Crop_Max_Y *  Header.tDeltaY * (256/4),
      Append_OBJ_File(qtX * 90 * (256/4),  // needs to be UTM grid relative ???
                      qtY * 90 * (256/4),
                      Crop_Min_X * 90 * (256/4),
                      Crop_Max_X * 90 * (256/4),
                      Crop_Min_Y * 90 * (256/4),
                      Crop_Max_Y * 90 * (256/4),
        Condor_folder+'\Landscapes\'+LandscapeName,LandscapeName,
        Condor_folder+'\Landscapes\'+Name,Name);
    end;
  end;

  // merge .apt files
  MessageShow('Merging Airports');
  // make sure there is no old file
  DeleteFile(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.apt');
  // lat and long only, no UTM, so copy is straight forward
  // - ??? how to deal with duplicate airport names ???
  // remove airports when cropping enabled
  // also merge 'Airports' folders
  // - ??? how to deal with duplicate texture file names ???
  // for each airport in cropped airport list
  // - open the G file and search for textures and copy
  // - open the O file and search for textures and copy
  // can open file and copy blocks of 72 bytes for each airport
  for i := 0 to Merge_Count-1 do begin
    Append_APT_File(ce,
      Condor_folder+'\Landscapes\'+LandscapeName,LandscapeName,
      Condor_folder+'\Landscapes\'+Merge_Array[i].Name,Merge_Array[i].Name);
  end;

  // merge .tdm thermal files
  MessageShow('Merging thermal map');
  u_Thermal.Memo_Message := Memo_Message;
  u_Thermal.ProgressBar_Status := ProgressBar_Status;
  // first force size
//  ForceTDMsize(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.tdm');
  ForceTDMsize(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.tdm',128);
  // now merge
  for i := 0 to Merge_Count-1 do begin
    with Merge_Array[i] do begin
      Merge_TDM_File(trunc(qtX) * (256 div 4),
                     trunc(qtY) * (256 div 4),
                     Crop_Min_X * (256 div 4),
                     Crop_Max_X * (256 div 4),
                     Crop_Min_Y * (256 div 4),
                     Crop_Max_Y * (256 div 4),
        Condor_folder+'\Landscapes\'+LandscapeName,LandscapeName+'.tdm',
        Condor_folder+'\Landscapes\'+Name,Name+'.tdm');
    end;
  end;

  // merge .cup files
  MessageShow('Merging Turnpoints');
  // make sure there is no old file
  DeleteFile(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.cup');
  // lat and long only, no UTM, so copy is straight forward
  // - ??? how to deal with duplicate turnpoint names ???
  // remove turnpoints when cropping enabled
  // can open file and each line for each turnpoint
  for i := 0 to Merge_Count-1 do begin
    Append_CUP_File(ce,
      Condor_folder+'\Landscapes\'+LandscapeName,LandscapeName,
      Condor_folder+'\Landscapes\'+Merge_Array[i].Name,Merge_Array[i].Name);
  end;

  // merge the 'Images' folders
  // ??? how to deal with duplicate names ???
  MessageShow('Copying Images');
  for i := 0 to Merge_Count-1 do begin
    // copy the Images folder if any
    FolderName := Condor_folder+'\Landscapes\'+
      Merge_Array[i].Name+'\Images';
    if (DirectoryExists(FolderName)) then begin
      NewFolderName := Condor_folder+'\Landscapes\'+LandscapeName+'\Images';
      ForceDirectories(NewFolderName);
      if (FindFirst(FolderName+'\*.*', faNormalFile, SearchRec)) = 0 then begin
        CopyFile(pchar(FolderName+'\'+SearchRec.Name),
          pchar(NewFolderName+'\'+SearchRec.Name),false);
        while (FindNext(SearchRec) = 0) do begin
          CopyFile(pchar(FolderName+'\'+SearchRec.Name),
            pchar(NewFolderName+'\'+SearchRec.Name),false);
        end;
        FindClose(SearchRec);
      end;
    end;
  end;

  // create new .ini file
  MessageShow('Creating INI version file');
  MakeDummyINI(Condor_folder+'\Landscapes\'+LandscapeName,LandscapeName+'.ini');

  // Can't do with Condor_Tiles
  // create new .tha and .fha hash files with Landscape Editor.
  MessageShow('You now need to create the .FHA and .THA files');

  // use the Condor Landscape Editor to calculate the hashes
  //MessageShow('creating the .FHA and .THA files');
//  Execute_BatchFile(File_Folder, '???.bat', ''); // use batch file ?  or execute directly ?
//  C:\CST2\LandscapeEditor.exe -hash YOURVERYVERYBIGSCENERY

end;

{----------------------------------------------------------------------------}
procedure TForm_Merge.FormCreate(Sender: TObject);
begin
  Image_Grid.Picture.Bitmap.Create; // must be square for current code !
  Image_Grid.Picture.Bitmap.Width := Image_Grid.Width;
  Image_Grid.Picture.Bitmap.Height := Image_Grid.HeigHt;
  mBitmap := Image_Grid.Picture.Bitmap;
end;

{----------------------------------------------------------------------------}
procedure TForm_Merge.FormActivate(Sender: TObject);
begin
  CheckListBox_LandscapeList.Clear;  //clear the list
  CheckListBox_LandscapeList.Items.AddStrings(LandscapeList);
  if (LandscapeName <> '') then begin   // if 'Read Header' was done
    //look for merge file and read if found
    Read_Merge_List;
  end;
end;

{----------------------------------------------------------------------------}
procedure TForm_Merge.Data_Change(Sender: TObject);
begin
  Button_Create.Enabled := false;
  Button_Merge.Enabled := false;
end;

{----------------------------------------------------------------------------}
end.


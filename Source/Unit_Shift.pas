{
 * Unit_Shift.pas
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
unit Unit_Shift;

//===========================================================================
INTERFACE

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, comctrls, ExtCtrls, CheckLst;

type
  TForm_Shift = class(TForm)
    GroupBox_Landscapes: TGroupBox;
    Image_Grid: TImage;
    Button_Check: TButton;
    GroupBox_New: TGroupBox;
    Edit_Name: TEdit;
    Button_Create: TButton;
    Button_Shift: TButton;
    CheckListBox_LandscapeList: TCheckListBox;
    GroupBox_Reference: TGroupBox;
    Label_Latitude: TLabel;
    Label_Longitude: TLabel;
    Edit_Latitude: TEdit;
    Edit_Longitude: TEdit;
    GroupBox_Crop: TGroupBox;
    Edit_Top: TEdit;
    Edit_Bottom: TEdit;
    Edit_Left: TEdit;
    Edit_Right: TEdit;
    GroupBox_Defaults: TGroupBox;
    Label_Shift_E_W: TLabel;
    Edit_E_W: TEdit;
    Label_Shift_N_S: TLabel;
    Edit_N_S: TEdit;
    CheckBox_SG: TCheckBox;
    Label_SG: TLabel;
    Label_AP_tweak: TLabel;
    CheckBox_ApTweak: TCheckBox;
    procedure Button_CreateClick(Sender: TObject);
    procedure Button_CheckClick(Sender: TObject);
    procedure Button_ShiftClick(Sender: TObject);
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
  Form_Shift: TForm_Shift;

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
  mgVersion : string;

//===========================================================================
IMPLEMENTATION

{$R *.DFM}

uses
  FileCtrl, Math,
  Unit_DEM,
  u_UTM, u_Terrain, u_Thermal, u_BMP, u_DXT, u_X_CX,
  u_Object, u_Airport, u_INI, u_CUP, u_Airspace, u_TileList;

type
  Shift_Type = record
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
  Shift_Count : Integer;
  Shift_Array : array of Shift_Type;
  ce_East, ce_West, ce_North, ce_South : integer;
  sh_E_W, sh_N_S : integer;
  t3f_Error_X, t3f_Error_y : single;
  t3f_Found : boolean;

var
  oe : Extents; // overall extents
  ce : Extents; // crop extents

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
Procedure Save_Shift_List;
var
  i : integer;
  Temp_File : TextFile;
begin
  if (NOT DirectoryExists(Condor_Folder+'\Landscapes\'+LandscapeName)) then begin
    ForceDirectories(Condor_Folder+'\Landscapes\'+LandscapeName);
  end;

  AssignFile(Temp_file, Condor_folder+'\Landscapes\'+LandscapeName+'\SHIFT.txt');
  Rewrite(Temp_file);
  writeln(Temp_file, Form_Shift.Edit_Right.text);
  writeln(Temp_file, Form_Shift.Edit_Left.text);
  writeln(Temp_file, Form_Shift.Edit_Top.text);
  writeln(Temp_file, Form_Shift.Edit_Bottom.text);
  writeln(Temp_file, Form_Shift.Edit_E_W.text);
  writeln(Temp_file, Form_Shift.Edit_N_S.text);
  writeln(Temp_file, Form_Shift.Edit_Latitude.text);
  writeln(Temp_file, Form_Shift.Edit_Longitude.text);
  if (Form_Shift.CheckBox_SG.Checked) then begin
    writeln(Temp_file, 'Standard-Grid');
  end else begin
    writeln(Temp_file, 'No-Grid');
  end;
  if (Form_Shift.CheckBox_ApTweak.Checked) then begin
    writeln(Temp_file, 'Airport-Tweak');
  end else begin
    writeln(Temp_file, 'No-Airport-Tweak');
  end;
  with Form_Shift.CheckListBox_LandscapeList do begin
    for i := 0 to Items.Count-1 do begin
      if (Checked[i]) then begin
        writeln(Temp_file, Items[i]);
      end;
    end;
  end;
  CloseFile(Temp_file);
end;

{----------------------------------------------------------------------------}
Procedure Read_Shift_List;
var
  i : integer;
  Temp_File : TextFile;
  TempSTR : string;
begin
  if (FileExists(Condor_Folder+'\Landscapes\'+LandscapeName+'\SHIFT.txt')) then begin
    Form_Shift.Edit_Name.text := LandscapeName;
    AssignFile(Temp_file, Condor_Folder+'\Landscapes\'+LandscapeName+'\SHIFT.txt');
    Reset(Temp_file);
    readln(Temp_file, TempSTR);
    Form_Shift.Edit_Right.text := TempSTR;
    readln(Temp_file, TempSTR);
    Form_Shift.Edit_Left.text := TempSTR;
    readln(Temp_file, TempSTR);
    Form_Shift.Edit_Top.text := TempSTR;
    readln(Temp_file, TempSTR);
    Form_Shift.Edit_Bottom.text := TempSTR;
    readln(Temp_file, TempSTR);
    Form_Shift.Edit_E_W.text := TempSTR;
    readln(Temp_file, TempSTR);
    Form_Shift.Edit_N_S.text := TempSTR;
    readln(Temp_file, TempSTR);
    Form_Shift.Edit_Latitude.text := TempSTR;
    readln(Temp_file, TempSTR);
    Form_Shift.Edit_Longitude.text := TempSTR;
    readln(Temp_file, TempSTR);
    if (TempSTR = 'Standard-Grid') then begin
      Form_Shift.CheckBox_SG.Checked := true;
    end else begin
      Form_Shift.CheckBox_SG.Checked := false;
    end;
    readln(Temp_file, TempSTR);
    if (TempSTR = 'Airport-Tweak') then begin
      Form_Shift.CheckBox_ApTweak.Checked := true;
    end else begin
      Form_Shift.CheckBox_ApTweak.Checked := false;
    end;
    with Form_Shift.CheckListBox_LandscapeList do begin
      While (NOT EOF(Temp_File)) do begin
        Readln(Temp_file, TempSTR);
        // search list and tick box if match
        for i := 0 to Items.Count-1 do begin
          if (Items[i] = TempSTR) then begin
            Checked[i] := true;
            ItemIndex := i; // scroll list to make it visible
            Break;
          end;
        end;
      end;
    end;
    CloseFile(Temp_file);
  end else begin
    Form_Shift.Edit_Name.text := '';
    LandscapeName := Form_Shift.Edit_Name.text;
  end;
end;

{----------------------------------------------------------------------------}
procedure TForm_Shift.Button_CreateClick(Sender: TObject);
//var
//  b_Error_X, b_Error_y : single;
//  d_Error_X, d_Error_y : single;
//  f_Error_X, f_Error_y : single;
//  t3_Error_X, t3_Error_y : single;
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
//MessageShow(Format('W= %d, H= %d',[u_Terrain.TerrainHeader.twidth,u_Terrain.TerrainHeader.tHeight]));
    WriteTDMHeader(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.tdm');
    // create an initial dummy .bmp file.
    WriteBMP24Header(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.bmp');
    // create an initial dummy TM3 file
    if (mgVersion = 'V3') then begin
      WriteTDMHeader(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.tm3');
    end;
    // save landscape list as a 'Shift flag'
    Save_Shift_List;
    Button_Shift.Enabled := true;
    // calculate resolution errors for info
    // TRN,BMP,TDM,TM3 - 90m resolution
//    b_Error_X := ((sh_E_W / 90) - round(sh_E_W / 90)) * 90;
//    b_Error_Y := ((sh_N_S / 90) - round(sh_N_S / 90)) * 90;
    // TR3 - 30m resolution
//    t3_Error_X := ((sh_E_W / 30) - round(sh_E_W / 30)) * 30;
//    t3_Error_Y := ((sh_N_S / 30) - round(sh_N_S / 30)) * 30;
    // TR3F - 22.5m resolution
    t3f_Error_X := ((sh_E_W / 22.5) - round(sh_E_W / 22.5)) * 22.5;
    t3f_Error_Y := ((sh_N_S / 22.5) - round(sh_N_S / 22.5)) * 22.5;
    MessageShow(format('TR3F Offset: %2.3f, %2.3f',[t3f_Error_X,t3f_Error_Y]));
    // FOR - 11.25m resolution
//    f_Error_X := ((sh_E_W / 11.25) - round(sh_E_W / 11.25)) * 11.25;
//    f_Error_Y := ((sh_N_S / 11.25) - round(sh_N_S / 11.25)) * 11.25;
    // DDS - 22.5m resolution @ 1024x1024 with 4x4 pixel groups   64*90/(1024/4)=22.5
//    d_Error_X := ((sh_E_W / 22.5) - round(sh_E_W / 22.5)) * 22.5;
//    d_Error_Y := ((sh_N_S / 22.5) - round(sh_N_S / 22.5)) * 22.5;
//    MessageShow(format('DDS_1024 Offset: %2.3f, %2.3f',[d_Error_X,d_Error_Y]));

  end else begin
    MessageShow('No Landscape name specified');
    beep;
  end;
end;

{----------------------------------------------------------------------------}
procedure TForm_Shift.Button_CheckClick(Sender: TObject);
var
  i : integer;
  xDelta : single;
  yDelta : single;
  Temp : longint;
  AbleToShift : boolean;
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
//     OR (DifferenceUTMzone(StrToInt(uZone), u_Terrain.TerrainHeader.tUTMzone) >  1)
     OR (DifferenceUTMzone(uZone, u_Terrain.TerrainHeader.tUTMzone) >  1)
//     OR (DifferenceUTMzone(StrToInt(uZone), u_Terrain.TerrainHeader.tUTMzone) < -1)
     OR (DifferenceUTMzone(uZone, u_Terrain.TerrainHeader.tUTMzone) < -1)
    then begin
//      MessageShow('Zone mismatch');
//      Beep;
       Exit;
    end;

    // convert to UTM
    LatLongToUTM(LatRef, LongRef,
//      IntToStr(u_Terrain.TerrainHeader.tUTMzone), uGrid);
      u_Terrain.TerrainHeader.tUTMzone, uGrid);
    ReferencePoint.X := round(uEasting); ReferencePoint.Y := round(uNorthing); // reference point

    ReferencePointDefined:= true;
  except
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - }
function Get_Shift_Crop : Boolean;
begin
  Result := false;
  try
    sh_E_W := StrToInt(Edit_E_W.text);
    sh_N_S := StrToInt(Edit_N_S.text);

    // validate
    // can't both be 0
    if ((sh_E_W = 0) AND (sh_N_S = 0)) then begin
      Result := false;
      exit;
    end else begin
      if (sh_E_W <> 0) then begin
        if ((sh_E_W <= -2880) OR (sh_E_W >= 2880)) then begin
          Result := false;
          exit;
        end else begin
          if (sh_E_W > 0) then begin
            ce_East := -1;
            ce_West := 0;
          end else begin
            ce_East := 0;
            ce_West := -1;
          end;
          Result := true;
        end;
      end else begin
        ce_East := 0;
        ce_West := 0;
      end;

      if (sh_N_S <> 0) then begin
        if ((sh_N_S <= -2880) OR (sh_N_S >= 2880)) then begin
          Result := false;
          exit;
        end else begin
          if (sh_N_S > 0) then begin
            ce_North := -1;
            ce_South := 0;
          end else begin
            ce_North := 0;
            ce_South := -1;
          end;
          Result := True;
        end;
      end else begin
        ce_North := 0;
        ce_South := 0;
      end;
    end;

    Edit_Right.text := IntToStr(ce_East);
    Edit_Left.text := IntToStr(ce_West);
    Edit_Top.text := IntToStr(ce_North);
    Edit_Bottom.text := IntToStr(ce_South);

  except
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - }
begin
  MessageClear;
  AbleToShift := false;
  Shift_Count := 0;
  // create a Shift array
  with Form_Shift.CheckListBox_LandscapeList do begin
    for i := 0 to Items.Count-1 do begin
      if (Checked[i]) then begin
        INC(Shift_Count);
        SetLength(Shift_Array,Shift_Count);
        Shift_Array[Shift_Count-1].Name := Items[i];
      end;
    end;
  end;

  if (Shift_Count <= 0) then begin
    MessageShow('No landscapes selected');
    Beep; Exit;
  end;

  // read headers
  for i := 0 to Shift_Count-1 do begin
    with Shift_Array[i] do begin
    ReadTerrainHeader(Condor_folder+'\Landscapes\'+Name+'\'+Name+'.trn');
      with u_Terrain.TerrainHeader do begin
        Header := u_Terrain.TerrainHeader;
      end;
    end;
  end;

  // prepare new terrain header
  u_Terrain.TerrainHeader := Shift_Array[0].Header;

  if (Shift_Count > 1) then begin
    // check zones
    for i := 1 to Shift_Count-1 do begin
      with Shift_Array[i].Header do begin
        if (tUTMzone <> u_Terrain.TerrainHeader.tUTMzone)
         OR (tUTMgrid[0] <> u_Terrain.TerrainHeader.tUTMgrid[0]) then begin
          MessageShow('Zone mismatch');
          Beep; Exit;
        end;
      end;
    end;
    MessageShow('Zone match');

    // check calibration
    for i := 1 to Shift_Count-1 do begin
      with Shift_Array[i].Header do begin
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
    for i := 1 to Shift_Count-1 do begin
      with Shift_Array[i].Header do begin
        xDelta := abs(tRightMapEasting) - abs(u_Terrain.TerrainHeader.tRightMapEasting);
        yDelta := abs(tBottomMapNorthing) - abs(u_Terrain.TerrainHeader.tBottomMapNorthing);
//        if (trunc(xDelta) MOD 256*90 <> 0)
//         OR (trunc(yDelta) MOD 256*90 <> 0) then begin
        if (trunc(xDelta) MOD 64*90 <> 0)
         OR (trunc(yDelta) MOD 64*90 <> 0) then begin
          MessageShow('UTM grid mismatch');
          Beep; Exit;
        end;
      end;
    end;
    MessageShow('UTM grid match');
  end;
  // check maximum quarter-tile qt max indexes X=0..99 and Y=0..99
  with Shift_Array[0].Header do begin
    oe.xMax := tRightMapEasting;
    oe.xMin := oe.xMax + tWidth * tDeltaX;
    oe.yMin := tBottomMapNorthing;
    oe.yMax := oe.yMin + tHeight * tDeltaY;
  end;
  for i := 1 to Shift_Count-1 do begin
    with Shift_Array[i].Header do begin
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

  // check to see if a standard grid is to be used
  if (Form_Shift.CheckBox_SG.Checked) then begin
    Temp := round(oe.xMax) MOD (64 * 90);
    if (Temp > ((64 * 90) div 2)) then begin
      Temp := Temp - (64 * 90);
    end;
    Form_Shift.Edit_E_W.text := IntToStr( - Temp);

    Temp := round(oe.yMin) MOD (64 * 90);
    if (Temp > ((64 * 90) div 2)) then begin
      Temp := Temp - (64 * 90);
    end;
    Form_Shift.Edit_N_S.text := IntToStr( - Temp);
  end;

  // now create the cropped area
  if (Get_Shift_Crop) then begin
    CropAreaPointsDefined:= true;
    ce.xMax := oe.xMax + ce_East * -u_Terrain.TerrainHeader.tDeltaX * (256 DIV 4) + sh_E_W;
    ce.xMin := oe.xMin - ce_West * -u_Terrain.TerrainHeader.tDeltaX * (256 DIV 4) + sh_E_W;
    ce.yMax := oe.yMax + ce_North * u_Terrain.TerrainHeader.tDeltaY * (256 DIV 4) + sh_N_S;
    ce.yMin := oe.yMin - ce_South * u_Terrain.TerrainHeader.tDeltaY * (256 DIV 4) + sh_N_S;
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
    MessageShow('Shift must be between -2880 and +2880');
    Beep; Exit;
  end;

  Columns := round((ce.xMax - ce.xMin) / -u_Terrain.TerrainHeader.tDeltaX) DIV (256 DIV 4);
  Rows    := round((ce.yMax - ce.yMin) /  u_Terrain.TerrainHeader.tDeltaY) DIV (256 DIV 4);
  if ( Columns > (99+1))
   OR ( Rows > (99+1)) then begin
    MessageShow('WARNING: X, Y index > 25');
//    Beep; Exit;     // allow but warn !
  end;
  Columns := Columns / 4;  // need tiles not qt
  Rows := Rows / 4;        // need tiles not qt
  MessageShow(Format('X= %0.2f, Y= %0.2f',[Columns,Rows]));

  AbleToShift := true;

  // create offsets, relative to overall
  for i := 0 to Shift_Count-1 do begin
    with Shift_Array[i] do begin
//      Shift_Array[i].qtX := (Header.tRightMapEasting - oe.xMax) / -Header.tDeltaX / (256/4);
//      Shift_Array[i].qtY := (Header.tBottomMapNorthing - oe.yMin) / Header.tDeltaY / (256/4);
      Shift_Array[i].qtX := (Header.tRightMapEasting - oe.xMax) / -Header.tDeltaX;
      Shift_Array[i].qtY := (Header.tBottomMapNorthing - oe.yMin) / Header.tDeltaY;
    end;
  end;

  // update new terrain header
  with u_Terrain.TerrainHeader do begin
    tRightMapEasting := ce.xMax;                    // UTM absolute Easting, bottom right
    tBottomMapNorthing := ce.yMin;                  // UTM absolute Northing, bottom right
    tWidth :=  round((ce.xMax - ce.xMin) / -tDeltaX);  // number of columns
    tHeight := round((ce.yMax - ce.yMin) /  tDeltaY);  // number of rows
    // prepare TDM header
    with u_Thermal.TDM_Header do begin
      Width := tWidth;
      Height := tHeight;
    end;
    // prepare BMP header
    with xBitmapHeader_24bitColor do begin
      bDib.bWidth := tWidth;
      bDib.bHeight := tHeight;
      //bDib.bImageByteSize := tWidth*tHeight*xColor24Size div 8;
      // multiplying by 24 can exceed integer size and cause overflow
      bDib.bImageByteSize := tWidth*tHeight*Color24Size;
      bH.bFileByteSize := bDib.bImageByteSize+bH.bPixelArrayOffset;
    end;
  end;

  if (AbleToShift) then begin  // Draw grid and areas
    // Draw grid
    SetLength(Areas,Shift_Count+2); // extra 2 for overall and flying extents
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
    for i := 0 to Shift_Count-1 do begin
      with Shift_Array[i].Header do begin
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
    Columns := round((oe.xMax - oe.xMin) / -u_Terrain.TerrainHeader.tDeltaX) DIV (256 DIV 4) /4;
    Rows    := round((oe.yMax - oe.yMin) /  u_Terrain.TerrainHeader.tDeltaY) DIV (256 DIV 4) /4;
    Form_DEM.ShowGrid(mBitmap, Columns, Rows);

    Button_Create.Enabled := true;
  end;
end;

{----------------------------------------------------------------------------}
procedure TForm_Shift.Edit_NameChange(Sender: TObject);
begin
  LandscapeName := Edit_Name.Text;
end;

type
  IndexFile_Type = (Type_TR3, Type_FOR, Type_DDS, Type_TR3F, Type_C3D);

{----------------------------------------------------------------------------}
{procedure Create_Dummy_Files(IF_Type : IndexFile_Type;
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
//    Type_TR3f: begin   // not needed
//      File_Prefix := 'h';
//      File_Ext := '.tr3f';
//      File_Folder := 'HeightMaps\22.5';
//      FileBytes := 257*257*4;
//    end;
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
          format('%s%s%s',[File_Prefix,MakeTileName(i,j, TileNameMode),File_Ext]);
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
}
{----------------------------------------------------------------------------}
procedure Copy_ReIndex_Files(IF_Type : IndexFile_Type;
  Offset_X, Offset_Y : single;
  FilePath, FilePath_a, Name_a : string);
var
  TRN_File : File of Byte;      // use Word instead of Byte and terrain-size ???
  i,j : integer;
  File_Prefix, File_Ext, File_Folder : string;
  File_Name, File_Name_a : string;
  resolution : single;
  xySize, xyExtra : Integer;
  Patch_File : File of Byte;
  P : pByteArray;
  dSize : integer;
  Patch_Files : array [0..4-1] of File of Byte;
  DDS_Headers : array [0..4-1] of array[0..32-1] of Cardinal; // TDDSHeader in u_DDS.pas
  DDS_MipMap :  array [0..4-1] of integer;
  DDS_DXT_Type :  array [0..4-1] of integer;
  DDS_dSize : array [0..4-1] of integer;
  ref_Index : integer;
  ref_Mipmap : integer;
  FileCount : integer;  // need at least one file of 2 or 4 to generate TR3F file

  ExtractedIndexes : array[0..16-1] of Cardinal;
  ExtractedAlphas : array[0..16-1] of Cardinal;
const
  MaskTable : array[0..4-1] of array[0..8-1] of byte = (
    (5,4,5,4,1,0,1,0), (7,6,7,6,3,2,3,2),
    (13,12,13,12,9,8,9,8), (15,14,15,14,11,10,11,10)
    );

// determine if 2 or 4 files are needed for the shift
// for a purely horizontal ot vertical shift, only 2 files needed, not 4
{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
function TwoOrFourFiles(X, Y : single) : byte;
begin
  // create a bit mask for which files are needed
  result := $01; // file 0,0 is always needed
  if (X = 0) then begin // or within +/- 15 ?
    result := result OR $04; // file 0,1
  end else begin
    result := result OR $02; // file 1,0
    if (Y = 0) then begin
    end else begin
      result := result OR $04; // file 0,1
      result := result OR $08; // file 1,1
    end;
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
Procedure Expand_TR3_TR3f(i, j, X, Y : integer);
var
  in_x, in_y : integer;
  out_x, out_y : integer;
  A, B, C, D : single; // for interpolation
  Q : pFloatArray;

begin
  Q := @P;
  // first expand horizontally, then vertically
  // start at bottom right
  in_y := 193-1; out_y := 257-1;
  repeat
    in_x := 193-1; out_x := 257-1;
    repeat
      D := 1.0 * pWordArray(P)^[in_x-0 + (X + (Y + in_y)*(256+256+1))*2]; // *2 because data is 4 bytes and word is 2 bytes -> *2 Kludge
      Q^[(X + out_x-0 + (Y + out_y)*(256+256+1))] := D;                   // may be better to have input data in its own grid
      C := 1.0 * pWordArray(P)^[in_x-1 + (X + (Y + in_y)*(256+256+1))*2]; // instead of sharing the 257 grid byt the 193 grid ?
      Q^[(X + out_x-1 + (Y + out_y)*(256+256+1))] := (C*3+D)/4;
      B := 1.0 * pWordArray(P)^[in_x-2 + (X + (Y + in_y)*(256+256+1))*2];
      Q^[(X + out_x-2 + (Y + out_y)*(256+256+1))] := (B+C)/2;
      A := 1.0 * pWordArray(P)^[in_x-3 + (X + (Y + in_y)*(256+256+1))*2];
      Q^[(X + out_x-3 + (Y + out_y)*(256+256+1))] := (A+B*3)/4;
      // now move left
      dec(in_x,3); dec(out_x,4);
    until (in_x = 0);
    Q^[(X + out_x + (Y + out_y)*(256+256+1))] := 1.0 * pWordArray(P)^[in_x + (X + (Y + in_y)*(256+256+1))*2];
    dec(in_y,1); dec(out_y,1);
  until (in_y < 0);
  // now interpolate vertically
  // start at top right
  in_y := 257-193; out_y := 0;
  repeat
    in_x := 257-1; out_x := 257-1;
    repeat
      D := 1.0 * Q^[(X + in_x + (Y + in_y+0)*(256+256+1))];
      Q^[(X + out_x + (Y + out_y+0)*(256+256+1))] := D;
      C := 1.0 * Q^[(X + in_x + (Y + in_y+1)*(256+256+1))];
      Q^[(X + out_x + (Y + out_y+1)*(256+256+1))] := (C*3+D)/4;
      B := 1.0 * Q^[(X + in_x + (Y + in_y+2)*(256+256+1))];
      Q^[(X + out_x + (Y + out_y+2)*(256+256+1))] := (B+C)/2;
      A := 1.0 * Q^[(X + in_x + (Y + in_y+3)*(256+256+1))];
      Q^[(X + out_x + (Y + out_y+3)*(256+256+1))] := (A+B*3)/4;
      // now move left
      dec(in_x,1); dec(out_x,1);
    until (in_x < 0);
    inc(in_y,3); inc (out_y,4)
  until (out_y = 257-1);
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
Procedure ReadTheFile(i, j, X, Y : integer);
var
  k : integer;
begin
  File_Name_a := format('%s%s%s',[File_Prefix,MakeTileName(i, j, TileNameMode),File_Ext]);
  if (FileExists(FilePath_a+'\'+File_Folder+'\'+File_Name_a)) then begin
    AssignFile(Patch_File,FilePath_a+'\'+File_Folder+'\'+File_Name_a);
    Reset(Patch_File);
    for k := 0 to (xySize+xyExtra)-1 do begin
      BlockRead(Patch_File, P^[(X + (Y + k) * (xySize+xySize+xyExtra)) *dSize],
        (xySize+xyExtra)*dSize);
    end;
    CloseFile(Patch_File);
    INC(FileCount); // needed to check for at least one file for TR3F
  end else begin // file not found
    // special case for tr3f - use tr3 and expand instead
    if (copy(File_Ext,1,4) = '.tr3') then begin
      File_Name_a := format('%s%s%s',[File_Prefix,MakeTileName(i, j, TileNameMode),'.tr3']);
      if (FileExists(FilePath_a+'\'+File_Folder+'\..\'+File_Name_a)) then begin
        AssignFile(Patch_File,FilePath_a+'\'+File_Folder+'\..\'+File_Name_a);
        Reset(Patch_File);
        // read 193x193 in top of 257x257 grid to start
        for k := 0 to (192+1)-1 do begin
          BlockRead(Patch_File, P^[(X + (Y + k) * (xySize+xySize+xyExtra)) *dSize],
            (192+1)*2);
        end;
        CloseFile(Patch_File);
        // now expand it from integer to floating-point
        // and interpolate from 3 to 4 steps
        Expand_TR3_TR3f(i, j, X, Y);
      end;
    end;
  end;
end;

// X, Y swaped for diagonal mirror
{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
Procedure WriteTheFile(i, j : integer; X, Y : single);
var
  k : integer;
  oX, oY : integer;
begin
  File_Name := format('%s%s%s',[File_Prefix,MakeTileName(i, j, TileNameMode),File_Ext]);

  if (X = 0) then begin
    oY := 0;
  end else begin
    if (X > 0) then begin
      oY := xySize - round(X / resolution);
    end else begin
      oY := - round(X / resolution);
    end;
  end;
  if (Y = 0) then begin
    oX := 0;
  end else begin
    if (Y > 0) then begin
      oX := round(Y / resolution);
    end else begin
      oX := xySize + round(Y / resolution);
    end;
  end;

  AssignFile(Patch_File,FilePath+'\'+File_Folder+'\'+File_Name);
  Rewrite(Patch_File);
  for k := 0 to (xySize+xyExtra)-1 do begin
    BlockWrite(Patch_File, P^[(oX + (k + oY) * (xySize+xySize+xyExtra)) *dSize],
      (xySize+xyExtra)*dSize);
  end;
  CloseFile(Patch_File);
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
type
  t_DDS_DXT3_Block_mod =
    record
//      alpha  : int64;    // SHR bug - use 32 bit instead
      alpha_L  : cardinal;
      alpha_H  : cardinal;
      color0 : word;
      color1 : word;
      select : t_Select;
    end;

  t_DDS_DXT5_Block_mod =
    record
      alpha0, alpha1 : byte;
      ixAlpha_L  : Word; // 16 bits
      ixAlpha_M  : Word; // 16 bits
      ixAlpha_H  : Word; // 16 bits
      color0 : word;
      color1 : word;
      select : t_Select;
    end;

  a_dxt3 = t_DDS_DXT3_Block_mod;
  pa_dxt3 = ^a_dxt3;
  a_dxt5 = t_DDS_DXT5_Block_mod;
  pa_dxt5 = ^a_dxt5;

// what if DDS is missing? - use empty.dds?
// read DDS Header of reference, get mipmap, size and DXT type
// assumptions: DXT1,3,5, width=height, complete number of mipmaps
{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
Procedure DDS_OpenTheFile(Index, i, j : integer; FileMask, Mask : byte);
begin
  if (NOT ((FileMask AND Mask) = Mask)) then begin
    exit; // skip (only 2 files, not 4)
  end;
  File_Name_a := format('%s%s%s',[File_Prefix,MakeTileName(i, j, TileNameMode),File_Ext]);

  AssignFile(Patch_Files[Index],FilePath_a+'\'+File_Folder+'\'+File_Name_a);
  Reset(Patch_Files[Index]);
  BlockRead(Patch_Files[Index], DDS_Headers[Index], 128);
  DDS_MipMap[Index] := DDS_Headers[Index][7];
  DDS_DXT_Type[Index] := DDS_Headers[Index][21];
  if (DDS_DXT_Type[Index] = $31545844) then begin // DXT 1, 3 or 5 ?
    DDS_dSize[Index] := 8;
  end else begin
    DDS_dSize[Index] := 16;
    dSize := DDS_dSize[Index];
  end;
//dSize := 16; //for testing conversion dxt1 to dxt 3
  if (Index = ref_Index) then begin
    ref_Mipmap := DDS_MipMap[Index];
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
Procedure ExtractIndexes(IndexBitArray : Cardinal);
var
  i : integer;
begin
  // 16 groups of 2 bits
  for i := 0 to 16-1 do begin
    ExtractedIndexes[i] := IndexBitArray AND $3;
    IndexBitArray := IndexBitArray SHR 2;
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
function CreateNewIndexes(x,y : integer) : Cardinal;
var
  i : integer;
begin
  // 16 groups of 2 bits, with 2 groups the same
  for i := 0 to 8-1 do begin
    result := (result SHL 2) OR ExtractedIndexes[ MaskTable[x+2*y][i] ];
    result := (result SHL 2) OR ExtractedIndexes[ MaskTable[x+2*y][i] ];
  end;
end;

// SHR bug on int64 - use 32 bits
{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
Procedure ExtractDXT3alphas(AlphaBitArray_L, AlphaBitArray_H : Cardinal);
var
  i : integer;
begin
  // 16 groups of 4 bits
  for i := 0 to 16-1 do begin
    if (i < 8) then begin
      ExtractedAlphas[i] := AlphaBitArray_L AND $F;
      AlphaBitArray_L := AlphaBitArray_L SHR 4;
    end else begin
      ExtractedAlphas[i] := AlphaBitArray_H AND $F;
      AlphaBitArray_H := AlphaBitArray_H SHR 4;
    end;
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
Procedure CreateNewDXT3alphas(x,y : integer; var AlphaBitArray_L, AlphaBitArray_H : Cardinal);
var
  i : integer;
begin
  // 16 groups of 4 bits, with 2 groups the same
  for i := 0 to 8-1 do begin
    if (i < 4) then begin
      AlphaBitArray_H := (AlphaBitArray_H SHL 4) OR ExtractedAlphas[ MaskTable[x+2*y][i] ];
      AlphaBitArray_H := (AlphaBitArray_H SHL 4) OR ExtractedAlphas[ MaskTable[x+2*y][i] ];
    end else begin
      AlphaBitArray_L := (AlphaBitArray_L SHL 4) OR ExtractedAlphas[ MaskTable[x+2*y][i] ];
      AlphaBitArray_L := (AlphaBitArray_L SHL 4) OR ExtractedAlphas[ MaskTable[x+2*y][i] ];
    end;
  end;
end;

const
  // blue color gradient test pattern
  DXT3TestPattern : array[0..16-1] of byte = ($FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,
                                              $1F,$00,$00,$00,$78,$7A,$7F,$55);
//  // blue color alpha gradient test pattern
//  DXT3TestPattern : array[0..16-1] of byte = ($8F,$04,$88,$04,$44,$04,$00,$00,
//                                              $1F,$00,$00,$00,$00,$00,$00,$00);

  // blue color alpha gradient test pattern
  DXT5TestPattern : array[0..16-1] of byte = ($FF,$00,$D0,$58,$3F,$D0,$58,$3F,
                                              $1F,$00,$00,$00,$00,$00,$00,$00);

//  // blue color alpha gradient test pattern
//  DXT5TestPattern : array[0..16-1] of byte = ($30,$C0,$4F,$39,$C1,$4F,$39,$C1,
//                                              $00,$00,$1F,$55,$55,$55,$55,$55);

// if not all same DXT type, convert to DXT3 as a standard
{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
Procedure Convert_To_DXT3(Index, X, Y, local_xySize : integer);
var
  j, k, m : integer;
  Q : pa_dxt3;
  R : pa_dxt5;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
Procedure DXT1toDXT3alpha(var Q : a_dxt3);
var
  i : integer;
  Selector : LongWord;
  AlphaSelect : LongWord;
begin
  with Q do begin
    // check for color3 use
    for i := 0 to 16-1 do begin
      Selector := select AND $00000003;
      AlphaSelect := $0000000F; // assume opaque
      if (Selector = $00000003) then begin
        if (color0 <= color1) then begin // transparency indicator ?
          Selector := 2; // do not use color3 !
          AlphaSelect := $00000000; // transparent
          // 3 colors not converted to 4 colors - not optimum
        end;
      end;
      select := (select SHR 2) OR (Selector SHL 30); // roll data in
      // bug with shifting 64 bits; use 32 instead
//      alpha := (alpha SHR 4) OR (AlphaSelect SHL 60);
      alpha_L := (alpha_L SHR 4) OR (Alpha_H SHL 28);
      alpha_H := (alpha_H SHR 4) OR (AlphaSelect SHL 28);
    end;
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
Procedure DXT5toDXT3alpha(R : a_dxt5; var Q : a_dxt3);
var
  i : integer;
  ixExtracted : byte;
  cAlpha : cardinal;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
function ixToValue(ix : byte) : cardinal;
begin
  with R do begin
    if (alpha0 > alpha1) then begin  // 2 alpha plus 6 interpolated
      case ix of
        0: result := trunc(alpha0 / 16);
        1: result := trunc(alpha1 / 16);
        2: result := trunc((alpha0 * 6 + alpha1 * 1) /7 /16);
        3: result := trunc((alpha0 * 5 + alpha1 * 2) /7 /16);
        4: result := trunc((alpha0 * 4 + alpha1 * 3) /7 /16);
        5: result := trunc((alpha0 * 3 + alpha1 * 4) /7 /16);
        6: result := trunc((alpha0 * 2 + alpha1 * 5) /7 /16);
        7: result := trunc((alpha0 * 1 + alpha1 * 6) /7 /16);
      end;
    end else begin // 2 alpha plus 4 interpolated plus opaque and transparent
      case ix of
        0: result := trunc(alpha0 / 16);
        1: result := trunc(alpha1 / 16);
        2: result := trunc((alpha0 * 4 + alpha1 * 1) /5 /16);
        3: result := trunc((alpha0 * 3 + alpha1 * 2) /5 /16);
        4: result := trunc((alpha0 * 2 + alpha1 * 3) /5 /16);
        5: result := trunc((alpha0 * 1 + alpha1 * 4) /5 /16);
        6: result :=  0; // tranparent
        7: result := 15; // opaque
      end;
    end;
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
begin
  // 16 groups of 3 bits
  for i := 0 to 16-1 do begin
    with R do begin
      // first extract the alpha indexes
      ixExtracted := ixAlpha_L AND $7;
      ixAlpha_L := (ixAlpha_L SHR 3) OR (ixAlpha_M SHL 13);
      ixAlpha_M := (ixAlpha_M SHR 3) OR (ixAlpha_H SHL 13);
      ixAlpha_H :=  ixAlpha_H SHR 3;
    end;
    with Q do begin
      // then Convert DXT5 Alpha indexes to DXT3 Alpha values
      cAlpha := ixToValue(ixExtracted);
      alpha_L := (alpha_L SHR 4) OR (Alpha_H SHL 28);
      alpha_H := (alpha_H SHR 4) OR (cAlpha SHL 28);
    end;
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
begin
  if (DDS_DXT_Type[Index] = $31545844) then begin // DXT1 to DXT3
    // first make room for alpha data
    // 8 bytes of alpha need to be inserted before each 8 byte color block
    j := local_xySize - 1; // start at bottom
    repeat
      k := local_xySize - 1; // start on right
      repeat
        for m := 0 to DDS_dSize[Index]-1 do begin
          P^[(k*2+1)*DDS_dSize[Index] + m + (X + (j + Y) * (xySize+xySize))*dSize] :=
            P^[(k)*DDS_dSize[Index] + m + (X + (j + Y) * (xySize+xySize))*dSize];
//          P^[(k*2)*DDS_dSize[Index] + m + (X + (j + Y) * (xySize+xySize))*dSize] := $FF; // full alpha for testing
//          P^[(k*2)*DDS_dSize[Index] + m + (X + (j + Y) * (xySize+xySize))*dSize] := $5A; // for testing
        end;
        dec(k);
      until (k <0);
      dec(j);
    until (j <0);
    // now make DXT3 alpha data
    for j := 0 to local_xySize-1 do begin
      for k := 0 to local_xySize-1 do begin
        Q := @P^[(k + X + (j + Y) * (xySize+xySize))*dSize];
        DXT1toDXT3alpha(Q^);
      end;
    end;
  end else begin // DXT5 to DXT3
    // make DXT3 alpha data
    for j := 0 to local_xySize-1 do begin
      for k := 0 to local_xySize-1 do begin
        R := @P^[(k + X + (j + Y) * (xySize+xySize))*dSize];
        Q := @P^[(k + X + (j + Y) * (xySize+xySize))*dSize];
        DXT5toDXT3alpha(R^, Q^);
      end;
    end;
  end;
end;

// more complex to shuffle the pixels in 4x4 blocks - looks reasonably good
{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
Procedure dds_Expand(Index, X, Y : integer);
var
  i, j, k, m : integer;
  dds_xySize : longint;
  dds_Expansion : integer;
  Mask, Mask_H : cardinal;
  ii : cardinal;
  Index_Offset : integer;
begin
    // calculate how much expansion needed
    dds_Expansion := ref_Mipmap - DDS_Mipmap[Index];
    dds_xySize := trunc(IntPower(2,DDS_Mipmap[Index]-1) /4); // 4x4 pixel blocks
    // copy lower-res data
    for i := 0 to (dds_xySize)-1 do begin
      BlockRead(Patch_Files[Index], P^[(X + (i + Y) * (xySize+xySize))*dSize],
        (dds_xySize)*DDS_dSize[Index]);
    end;
    // backtrack the file pointer to the beginning of the mipmap
    seek(Patch_Files[Index], FilePos(Patch_Files[Index])- dds_xySize*dds_xySize *DDS_dSize[Index]);

////color or alpha pattern for testing instead of reading the file
//for i := 0 to dds_xySize-1 do begin
//  for j := 0 to dds_xySize-1 do begin
////    CopyMemory(@P^[(X + j + (i + Y) * (xySize+xySize))*dSize],@DXT3TestPattern,DDS_dSize[Index]);
//    CopyMemory(@P^[(X + j + (i + Y) * (xySize+xySize))*dSize],@DXT5TestPattern,DDS_dSize[Index]);
//  end;
//end;

    // now convert to DXT3 if not matching output type
    if (DDS_DXT_Type[Index] <> DDS_DXT_Type[ref_Index]) then begin
      convert_To_DXT3(Index, X, Y, dds_xySize);
    end;

    // calculate offset to indexes
    if (dSize = 16) then begin // DXT3
      Index_Offset := 8 + 4; // skip over alpha and color words
    end else begin // dsize = 8, DXT1 (DXT1c DXT1a)
      Index_Offset := 4; // skip over color words
    end;

    // now expand the data
    for i := 0 to dds_Expansion-1 do begin
      // copy and duplicate 4 times
      j := dds_xySize - 1; // start at bottom
      repeat
        k := dds_xySize - 1; // start on right
        repeat
          // point to color indexes & extract them
          ii := (X + k + (j + Y) * (xySize+xySize))*dSize + Index_Offset;
          ExtractIndexes(P^[ii] + P^[ii+1] SHL 8 + P^[ii+2] SHL 16 + P^[ii+3] SHL 24); // need a better way - TBD - use Q with Q = @P ???
          if (dSize = 16) then begin // DXT3
            // point to alpha values & extract them
            ii := (X + k + (j + Y) * (xySize+xySize))*dSize;
            ExtractDXT3alphas(P^[ii+0] + P^[ii+1] SHL 8 + P^[ii+2] SHL 16 + P^[ii+3] SHL 24,
                          P^[ii+4] + P^[ii+5] SHL 8 + P^[ii+6] SHL 16 + P^[ii+7] SHL 24);
          end;
          // make first copy
          for m := 0 to dSize-1 do begin
            P^[(X + k*2 + (j*2 + Y) * (xySize+xySize))*dSize + m] :=
              P^[(X + k + (j + Y) * (xySize+xySize))*dSize + m];
          end;
          // then re-generate new color indexes
          ii := (X + k*2 + (j*2 + Y) * (xySize+xySize))*dSize + Index_Offset;
          Mask := CreateNewIndexes(0, 0);
          P^[ii+0] := Mask AND $FF; P^[ii+1] := (Mask SHR 8) AND $FF;
          P^[ii+2] := (Mask SHR 16) AND $FF; P^[ii+3] := (Mask SHR 24) AND $FF; // need a better way - TBD
          if (dSize = 16) then begin // DXT3
            ii := (X + k*2 + (j*2 + Y) * (xySize+xySize))*dSize;
            CreateNewDXT3alphas(0, 0, Mask, Mask_H);
            P^[ii+0] := Mask AND $FF; P^[ii+1] := (Mask SHR 8) AND $FF;
            P^[ii+2] := (Mask SHR 16) AND $FF; P^[ii+3] := (Mask SHR 24) AND $FF; // need a better way - TBD
            P^[ii+4] := Mask_H AND $FF; P^[ii+5] := (Mask_H SHR 8) AND $FF;
            P^[ii+6] := (Mask_H SHR 16) AND $FF; P^[ii+7] := (Mask_H SHR 24) AND $FF; // need a better way - TBD
          end;

          // make second copy
          for m := 0 to dSize-1 do begin
            P^[(X + k*2+1 + (j*2 + Y) * (xySize+xySize))*dSize + m] :=
              P^[(X + k + (j + Y) * (xySize+xySize))*dSize + m];
          end;
          // then re-generate new color indexes
          ii := (X + k*2+1 + (j*2 + Y) * (xySize+xySize))*dSize + Index_Offset;
          Mask := CreateNewIndexes(1, 0);
          P^[ii+0] := Mask AND $FF; P^[ii+1] := (Mask SHR 8) AND $FF;
          P^[ii+2] := (Mask SHR 16) AND $FF; P^[ii+3] := (Mask SHR 24) AND $FF;
          if (dSize = 16) then begin // DXT3
            ii := (X + k*2+1 + (j*2 + Y) * (xySize+xySize))*dSize;
            CreateNewDXT3alphas(1, 0, Mask, Mask_H);
            P^[ii+0] := Mask AND $FF; P^[ii+1] := (Mask SHR 8) AND $FF;
            P^[ii+2] := (Mask SHR 16) AND $FF; P^[ii+3] := (Mask SHR 24) AND $FF; // need a better way - TBD
            P^[ii+4] := Mask_H AND $FF; P^[ii+5] := (Mask_H SHR 8) AND $FF;
            P^[ii+6] := (Mask_H SHR 16) AND $FF; P^[ii+7] := (Mask_H SHR 24) AND $FF; // need a better way - TBD
          end;

          // make third copy
          for m := 0 to dSize-1 do begin
            P^[(X + k*2 + (j*2+1 + Y) * (xySize+xySize))*dSize + m] :=
              P^[(X + k + (j + Y) * (xySize+xySize))*dSize + m];
          end;
          // then re-generate new color indexes
          ii := (X + k*2 + (j*2+1 + Y) * (xySize+xySize))*dSize + Index_Offset;
          Mask := CreateNewIndexes(0, 1);
          P^[ii+0] := Mask AND $FF; P^[ii+1] := (Mask SHR 8) AND $FF;
          P^[ii+2] := (Mask SHR 16) AND $FF; P^[ii+3] := (Mask SHR 24) AND $FF;
          if (dSize = 16) then begin // DXT3
            ii := (X + k*2 + (j*2+1 + Y) * (xySize+xySize))*dSize;
            CreateNewDXT3alphas(0, 1, Mask, Mask_H);
            P^[ii+0] := Mask AND $FF; P^[ii+1] := (Mask SHR 8) AND $FF;
            P^[ii+2] := (Mask SHR 16) AND $FF; P^[ii+3] := (Mask SHR 24) AND $FF; // need a better way - TBD
            P^[ii+4] := Mask_H AND $FF; P^[ii+5] := (Mask_H SHR 8) AND $FF;
            P^[ii+6] := (Mask_H SHR 16) AND $FF; P^[ii+7] := (Mask_H SHR 24) AND $FF; // need a better way - TBD
          end;

          // make fourth copy
          for m := 0 to dSize-1 do begin
            P^[(X + k*2+1 + (j*2+1 + Y) * (xySize+xySize))*dSize + m] :=
              P^[(X + k + (j + Y) * (xySize+xySize))*dSize + m];
          end;
          // then re-generate new color indexes
          ii := (X + k*2+1 + (j*2+1 + Y) * (xySize+xySize))*dSize + Index_Offset;
          Mask := CreateNewIndexes(1, 1);
          P^[ii+0] := Mask AND $FF; P^[ii+1] := (Mask SHR 8) AND $FF;
          P^[ii+2] := (Mask SHR 16) AND $FF; P^[ii+3] := (Mask SHR 24) AND $FF;
          if (dSize = 16) then begin // DXT3
            ii := (X + k*2+1 + (j*2+1 + Y) * (xySize+xySize))*dSize;
            CreateNewDXT3alphas(1, 1, Mask, Mask_H);
            P^[ii+0] := Mask AND $FF; P^[ii+1] := (Mask SHR 8) AND $FF;
            P^[ii+2] := (Mask SHR 16) AND $FF; P^[ii+3] := (Mask SHR 24) AND $FF; // need a better way - TBD
            P^[ii+4] := Mask_H AND $FF; P^[ii+5] := (Mask_H SHR 8) AND $FF;
            P^[ii+6] := (Mask_H SHR 16) AND $FF; P^[ii+7] := (Mask_H SHR 24) AND $FF; // need a better way - TBD
          end;

          dec(k);
        until (k <0);
        dec(j);
      until (j <0);
      dds_xySize := dds_xySize * 2;
    end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
Procedure DDS_ReadTheFile(Index, X, Y : integer; FileMask, Mask : byte);
var
  k : integer;
  dds_xySize : longint;
begin
  if (NOT ((FileMask AND Mask) = Mask)) then begin
    exit; // skip (only 2 files, not 4)
  end;
  // need to check mipmap level
  // - expand if smaller
  // - skip to correct mipmap if bigger
  if (ref_Mipmap <= DDS_Mipmap[Index]) then begin
    // skip over hi-res data if smaller
    while (ref_Mipmap < DDS_Mipmap[Index]) do begin
      // advance file pointer from current pos to skip over
      dds_xySize := trunc(IntPower(2,DDS_Mipmap[Index]-1) /4); // 4x4 pixel blocks
//      seek(Patch_Files[Index], FilePos(Patch_Files[Index])+ dds_xySize*dds_xySize *dSize);
      seek(Patch_Files[Index], FilePos(Patch_Files[Index])+ dds_xySize*dds_xySize * DDS_dSize[Index]);
      dec(DDS_Mipmap[Index]);
    end;
    // now copy the data
    for k := 0 to (xySize)-1 do begin
      BlockRead(Patch_Files[Index], P^[(X + (k + Y) * (xySize+xySize))*dSize],
//        (xySize)*dSize);
        (xySize)*DDS_dSize[Index]);
    end;
    // convert to DXT3 if not matching output type
    if (DDS_DXT_Type[Index] <> DDS_Headers[ref_Index][21]) then begin
      Convert_To_DXT3(Index, X, Y, xySize);
    end;
    // mipmap level done
    dec(DDS_Mipmap[Index]);
  end else begin // hi-res data missing so need to duplicate some lower-res data
    dds_Expand(Index, X, Y);
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
Procedure DDS_CloseTheFile(Index : integer; FileMask, Mask : byte);
begin
  if (NOT ((FileMask AND Mask) = Mask)) then begin
    exit; // skip (only 2 files, not 4)
  end;
  CloseFile(Patch_Files[Index]);
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
Procedure DDS_Open_WriteTheFile(i, j : integer);
begin
  File_Name := format('%s%s%s',[File_Prefix,MakeTileName(i, j, TileNameMode),File_Ext]);
  AssignFile(Patch_File,FilePath+'\'+File_Folder+'\'+File_Name);
  Rewrite(Patch_File);
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
Procedure DDS_WriteTheFile(X, Y : single);
var
  k : integer;
  oX, oY : integer;
begin
  if (X = 0) then begin
    oX := 0;
  end else begin
    if (X > 0) then begin
      oX := round(X / resolution);
    end else begin
      oX := xySize + round(X / resolution);
    end;
  end;
  if (Y = 0) then begin
    oY := xySize;
  end else begin
    if (Y > 0) then begin
      oY := xySize - round(Y / resolution);
    end else begin
      oY := - round(Y / resolution);
    end;
  end;

  for k := 0 to (xySize)-1 do begin
    BlockWrite(Patch_File, P^[(oX + (k + oY) * (xySize+xySize)) * dSize],
      (xySize)*dSize);
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
Procedure DDS_Close_WriteTheFile;
begin
  CloseFile(Patch_File);
end;

// read in each mimap
  //read in the header (128 bytes)
  // if DXT1 -> dSize := 8
  // if DXT3 -> dSize := 16
  // if DXT5 -> dSize := 16
  // in case of DXT type mix, convert all to DXT3
  // problem if change of size and/or change of DXT
    // keep mipmap size of file that has largest chunk  i.e. > or < 2880
    // if SHx >0 then largest chunk is in i+1
    // if SHy <0 then largest chunk is in j+1
      // expand mimap or skip mipmap as needed
// write each mipmap
// write a new DDS
{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
Procedure Process_DDS(i, j : integer; X, Y : single);
var
  index_X, index_Y : integer;
  FileMask : byte;
begin
  // need a reference patch
  // use the patch that has the largest data block
  index_X := 0; index_Y := 0;
  if (X > 0) then
  begin
    Inc(Index_X);
  end;
  if (Y < 0) then
  begin
    Inc(Index_Y);
  end;
  ref_Index := Index_Y * 2 + Index_X;

  // assume output file DXT1 for now
  dSize := 8;
  // open 2 or 4 files and read parameters
  FileMask := TwoOrFourFiles(X, Y);
  DDS_OpenTheFile(0, i,   j  , FileMask, 1);
  DDS_OpenTheFile(1, i+1, j  , FileMask, 2);
  DDS_OpenTheFile(2, i,   j+1, FileMask, 4);
  DDS_OpenTheFile(3, i+1, j+1, FileMask, 8);

  xySize := trunc(IntPower(2,ref_Mipmap-1) /4); // 4x4 pixel blocks
  resolution := 90.0 * 64 / xySize;

  // create a new DDS file
  DDS_Open_WriteTheFile(i, j);
  // re-use DDS header of ref_Index and set correct parameters
  if (dSize = 8) then begin
    // no change
  end else begin
    // if currently DXT1 then double the size
    if (DDS_DXT_Type[ref_Index] = $31545844) then begin
      // adjust the size
      DDS_Headers[ref_Index][5] := DDS_Headers[ref_Index][5] * 2;
    end;
    // use DXT3 as standard type (unless all 2 or 4 are DXT5 ??? then leave as DXT5 ???)
    DDS_Headers[ref_Index][21] := $33545844;
  end;
  // write the new DDS header
  BlockWrite(Patch_File, DDS_Headers[ref_Index], 128);
  // Allocate memory for top MipMap
  P := AllocMem( (xySize+xySize) * (xySize+xySize) * dSize);

  While (ref_Mipmap > 0) do begin
    //read each mipmap from 2 or 4 files
    DDS_ReadTheFile(0,  xySize, xySize, FileMask, 1);
    DDS_ReadTheFile(1,  0,      xySize, FileMask, 2);
    DDS_ReadTheFile(2,  xySize, 0,      FileMask, 4);
    DDS_ReadTheFile(3,  0,      0,      FileMask, 8);
    // write the shifted mipmap data
    // includes tweak for TR3F resolution
    DDS_WriteTheFile(X, Y);
    xySize := xySize div 2;
    if (xySize = 0) then begin
      xySize := 1; // 4x4 pixel blocks, even when 2 or 1 pixel
    end;
    resolution := resolution * 2;
    Dec(ref_Mipmap);
  end;

  // close 2 or 4 files
  DDS_CloseTheFile(3, FileMask, 1);
  DDS_CloseTheFile(2, FileMask, 2);
  DDS_CloseTheFile(1, FileMask, 4);
  DDS_CloseTheFile(0, FileMask, 8);
  DDS_Close_WriteTheFile;
  FreeMem(P);
end;

// copy the nearest file without merging (adding/removing) objects
// adjust the coordinates to new centre coordinate of tile, and copy the textures
  // read_C3D
  // for each mesh -> translate mesh
  // for each texture -> copy texture
  // write_C3D
{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
Procedure Process_C3D(i, j : integer; X, Y : single); // autogen
var
  index_X, index_Y : integer;
begin
  // need a reference patch
  // use the patch that has the largest data block
  index_X := 0; index_Y := 0;
  if (X > 0) then
  begin
    Inc(Index_X);
  end;
  if (Y < 0) then
  begin
    Inc(Index_Y);
  end;
  // check if a file is available
  File_Name_a := format('%s%s%s',[File_Prefix,MakeTileName(i+index_X, j+index_Y, TileNameMode),File_Ext]);
  if (FileExists(FilePath_a+'\'+File_Folder+'\'+File_Name_a)) then begin
    // need to shift all vertices
    Reset_FTM_Unity;
    InjectFTM := True;
    FTM[3] := -(-X);
    FTM[7] := -( Y);
    // read and shift with FTM
    ReadCondorC3Dfile(FilePath_a+'\'+File_Folder+'\'+File_Name_a, false);
    // Need to copy textures for this object
    CopyObjectTextures(FilePath+'\'+File_Folder,File_Name,
                       FilePath_a+'\'+File_Folder,File_Name_a,
                    '','');
    // now write the updated file
    File_Name := format('%s%s%s',[File_Prefix,MakeTileName(i, j, TileNameMode),File_Ext]);
    WriteCondorC3Dfile(FilePath+'\'+File_Folder+'\'+File_Name);
  end;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
begin
  case IF_Type of
    Type_TR3: begin
      File_Prefix := 'h';
      File_Ext := '.tr3';
      File_Folder := 'HeightMaps';
      Resolution := 90.0 / 3;
      xySize := 64 * 3;
      xyExtra := 1;
      dSize := 2;  // 2 byte integer
    end;
    Type_TR3f: begin // only a few TR3F files - how to deal with...
      File_Prefix := 'h';
      File_Ext := '.tr3f';
      File_Folder := 'HeightMaps\22.5m';
      Resolution := 90.0 / 4;
      xySize := 64 * 4;
      xyExtra := 1;
      dSize := 4;  // 4 byte floating-point
//      dSize := sizeof(single);  // 4 byte floating-point
    end;
    Type_FOR: begin // V2, V3
      File_Prefix := '';
      File_Ext := '.for';
      File_Folder := 'ForestMaps';
      Resolution := 90.0 / 8;
      xySize := 64 * 8;
      xyExtra := 0;
      dSize := 1;  // 1 byte containing two forest bits
    end;
    Type_DDS: begin
      File_Prefix := 't';
      File_Ext := '.dds';
      File_Folder := 'Textures';
//      Resolution := 0; // variable
//      xySize := 0;     // variable
//      xyExtra := 0;
//      dSize := 0;      // variable
    end;
    Type_C3D: begin // autogen
      File_Prefix := 'o';
      File_Ext := '.c3d';
      File_Folder := 'AutoGen';
//      Resolution := 0; // not applicable
//      xySize := 0;     // not applicable
//      xyExtra := 0;    // not applicable
//      dSize := 0;      // not applicable
    end;
    else begin
      Beep; Exit;
    end;
  end;
  // get destination file column(width) and row(height) counts
  AssignFile(TRN_File,FilePath+'\'+LandscapeName+'.trn');
  Reset(TRN_File);
  BlockRead(TRN_File,TerrainHeader,sizeof(CondorTerrainHeader));
  CloseFile(TRN_File);

  ForceDirectories(FilePath+'\'+File_Folder);
  with TerrainHeader do begin
    ProgressBar_Status.Max := tWidth div 64;
    for i := 0 to (tWidth div 64)-1 do begin // columns
      for j := 0 to (tHeight div 64)-1 do begin  //rows
        case IF_Type of
          Type_TR3, Type_TR3F, Type_FOR: begin
            FileCount := 0; // for TR3F files need at least one
            P := AllocMem( (xySize+xySize+xyExtra) * (xySize+xySize+xyExtra) * dSize);
            // read 2 or 4 files depending on offsets
            // diagonal mirror (x, y swap) for TR3, TR3f, FOR
            ReadTheFile(i,j,0,0); // file 0,0 is always needed
            if (Offset_X = 0) then begin // or within +/- 15 ?
              ReadTheFile(i,j+1,xySize,0); // file 0,1
            end else begin
              ReadTheFile(i+1,j,0,xySize); // file 1,0
              if (Offset_Y = 0) then begin
              end else begin
                ReadTheFile(i,j+1,xySize,0); // file 0,1
                ReadTheFile(i+1,j+1,xySize,xySize) // file 1,1
              end;
            end;
            // now write the file
            if (FileCount > 0) then begin
              // for TR3F, 4 files created, and most are not needed
              // need to check airport to see which to actually keep ??? TBD
              // for forest, can tweak if desired
              if ((IF_Type = Type_FOR) AND (Form_Shift.CheckBox_ApTweak.Checked) AND (t3f_Found)) then begin
                // make a shift referenced to TR3F resolution
                WriteTheFile(i, j, Offset_X - t3f_Error_X, Offset_Y - t3f_Error_Y);
              end else begin
                WriteTheFile(i, j, Offset_X, Offset_Y);
              end;
            end;
            FreeMem(P);
          end;
          Type_DDS: begin
            if ((Form_Shift.CheckBox_ApTweak.Checked) AND (t3f_Found)) then begin
              // make a shift referenced to TR3F resolution
              Process_DDS(i, j, Offset_X - t3f_Error_X, Offset_Y - t3f_Error_Y);
            end else begin
              Process_DDS(i, j, Offset_X, Offset_Y);
            end;
          end;
          Type_C3D: begin
            if ((Form_Shift.CheckBox_ApTweak.Checked) AND (t3f_Found)) then begin
              // make a shift referenced to TR3F resolution
              Process_C3D(i, j, Offset_X - t3f_Error_X, Offset_Y - t3f_Error_Y);
            end else begin
              Process_C3D(i, j, Offset_X, Offset_Y);
            end;
          end;
        end;
        ProgressBar_Status.StepIt;
        Application.ProcessMessages;
      end;
    end;
  end;
  ProgressBar_Status.Position := 0;
end;

{----------------------------------------------------------------------------}
procedure TForm_Shift.Button_ShiftClick(Sender: TObject);
var
  i, j : integer;
  FolderName : string;
  NewFolderName : string;
  SearchRec: TSearchRec;
//  Crop_Min_X, Crop_Min_Y, Crop_Max_X, Crop_Max_Y : longint;
  Crop_Min_X, Crop_Min_Y, Crop_Max_X, Crop_Max_Y : single;
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
  // Copy and shift terrain data
  MessageClear;
  MessageShow('Shifting terrain');
  u_Terrain.Memo_Message := Memo_Message;
  u_Terrain.ProgressBar_Status := ProgressBar_Status;
  // first force size
  ForceTerrainSize(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.trn');

 // set up crop range, relative to overall ??? must be a better way ???
  Crop_Min_X := -(ce.xMax-oe.xMax) / 90;
  Crop_Max_X := Crop_Min_X + TerrainHeader.tWidth;
  Crop_Min_Y := (ce.yMin-oe.yMin) / 90;
  Crop_Max_Y := Crop_Min_Y + TerrainHeader.tHeight;

  // see if there are any tr3f files
  t3f_Found := false; // assume for now
  FolderName := Condor_folder+'\Landscapes\'+
    Shift_Array[0].Name + '\HeightMaps\22.5m';
  if (FindFirst(FolderName+'\*.tr3f', faNormalFile, SearchRec)) = 0 then begin
    t3f_Found := true;
    FindClose(SearchRec);
  end;
{
// for partial testing
  // Shift the 'Textures' folder
  // need to offset XY tile indexes/names
  MessageShow('Shifting Textures');
  for i := 0 to Shift_Count-1 do begin
    with Shift_Array[i] do begin
      Copy_ReIndex_Files(Type_DDS,
        sh_E_W, sh_N_S,
        Condor_folder+'\Landscapes\'+LandscapeName,
        Condor_folder+'\Landscapes\'+Name, Name);
    end;
  end;
exit;
}
  // now Shift the landscape
  // first, terrain file
  for i := 0 to Shift_Count-1 do begin
    with Shift_Array[i] do begin
      Merge_TRN_File(round(qtX),
                     round(qtY),
                     round(Crop_Min_X),
                     round(Crop_Max_X),
                     round(Crop_Min_Y),
                     round(Crop_Max_Y),
        Condor_folder+'\Landscapes\'+LandscapeName,LandscapeName+'.trn',
        Condor_folder+'\Landscapes\'+Name,Name+'.trn');
    end;
  end;

  // 'HeightMaps' folder
  // need to offset XY tile indexes/names
  // copy and shift all .tr3 files
  MessageShow('Shifting tr3 HeightMaps');
  for i := 0 to Shift_Count-1 do begin
    with Shift_Array[i] do begin
      Copy_ReIndex_Files(Type_TR3,
        sh_E_W, sh_N_S,
        Condor_folder+'\Landscapes\'+LandscapeName,
        Condor_folder+'\Landscapes\'+Name, Name);
    end;
  end;

  // messy !
  // need to create extra TR3F for where airport is that needs the TR3F
  // this can be different than creating TR3F files where shift occurs
  if (mgVersion = 'V3') then begin
    // 'HeightMaps\22.5m' folder (extra floating-point version for higher resolution)
    MessageShow('Shifting tr3f HeightMaps');
    for i := 0 to Shift_Count-1 do begin
      with Shift_Array[i] do begin
        Copy_ReIndex_Files(Type_TR3F,
          sh_E_W, sh_N_S,
          Condor_folder+'\Landscapes\'+LandscapeName,
          Condor_folder+'\Landscapes\'+Name, Name);
      end;
    end;
  end;

{  // fix seams after to also make a good transition edge

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
//  for i := 1 to Shift_Count-1 do begin  // no need to do first one, start at 1
  for i := 0 to Shift_Count-1 do begin  // no need to do first one, start at 1
    with Shift_Array[i] do begin
      Fix_TR3_Seams(trunc(qtX) * (256 div 4),
                    trunc(qtY) * (256 div 4),
        Condor_folder+'\Landscapes\'+LandscapeName,LandscapeName+'.trn',
        Condor_folder+'\Landscapes\'+Name,Name+'.trn');
    end;
  end;
}
  // 'ForestMaps' folder
  // copy and shift all forest files
  MessageShow('Shifting ForestMaps');
  for i := 0 to Shift_Count-1 do begin
    with Shift_Array[i] do begin
      Copy_ReIndex_Files(Type_FOR,
        sh_E_W, sh_N_S,
        Condor_folder+'\Landscapes\'+LandscapeName,
        Condor_folder+'\Landscapes\'+Name, Name);
    end;
  end;

  // Shift the 'Textures' folder
  // need to offset XY tile indexes/names
  MessageShow('Shifting Textures');
  for i := 0 to Shift_Count-1 do begin
    with Shift_Array[i] do begin
      Copy_ReIndex_Files(Type_DDS,
        sh_E_W, sh_N_S,
        Condor_folder+'\Landscapes\'+LandscapeName,
        Condor_folder+'\Landscapes\'+Name, Name);
    end;
  end;
//  // then add an empty.dds file, to account for any missing tiles
//  Create_Dummy_Files(Type_DDS,
//    Condor_folder+'\Landscapes\'+LandscapeName, LandscapeName);


  // Shift flight planner map bitmaps - use 24 bit color, convert to 24 if needed
  MessageShow('Shifting FlightPlanner');
  u_BMP.Memo_Message := Memo_Message;
  u_BMP.ProgressBar_Status := ProgressBar_Status;
  // first force size
  ForceBMP24size(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.bmp');
  // now Shift
  for i := 0 to Shift_Count-1 do begin
    with Shift_Array[i] do begin
      Merge_BMP24_File(round(qtX),
                       round(qtY),
                       round(Crop_Min_X),
                       round(Crop_Max_X),
                       round(Crop_Min_Y),
                       round(Crop_Max_Y),
        Condor_folder+'\Landscapes\'+LandscapeName,LandscapeName+'.bmp',
        Condor_folder+'\Landscapes\'+Name,Name+'.bmp');
    end;
  end;
  // now convert to 32 bit - TBD ???
//  RenameFile(FileName, FileName+'.bmp');
//  Bitmap_24_To_Bitmap_32(FileName+'.bmp',FileName, false,1.0);
//  DeleteFile(FileName+'.bmp');

  // for other map bitmaps, make list of all bitmaps and then Shift by matching name
  MessageShow('Shifting alternate FlightPlanner(s)');
  // first make list, except for flightplanner, and avoid duplicates
  FileCount := 0;
  for i := 0 to Shift_Count-1 do begin
    FolderName := Condor_folder+'\Landscapes\'+
      Shift_Array[i].Name;
    if (FindFirst(FolderName+'\*.bmp', faNormalFile, SearchRec)) = 0 then begin
      FilterFile(Shift_Array[i].Name, SearchRec.Name);
      while (FindNext(SearchRec) = 0) do begin
        FilterFile(Shift_Array[i].Name, SearchRec.Name);
      end;
      FindClose(SearchRec);
    end;
  end;

  u_BMP.Memo_Message := Memo_Message;
  u_BMP.ProgressBar_Status := ProgressBar_Status;
  // then for each unique filename, Shift files with same name
  for j := 0 to FileCount-1 do begin
    // create an initial dummy .bmp file.
    WriteBMP24Header(Condor_folder+'\Landscapes\'+LandscapeName+'\'+FileList[j]);
    // now force its size
    ForceBMP24size(Condor_folder+'\Landscapes\'+LandscapeName+'\'+FileList[j]);
    for i := 0 to Shift_Count-1 do begin
      with Shift_Array[i] do begin
        Merge_BMP24_File(round(qtX),
                         round(qtY),
                         round(Crop_Min_X),
                         round(Crop_Max_X),
                         round(Crop_Min_Y),
                         round(Crop_Max_Y),
          Condor_folder+'\Landscapes\'+LandscapeName,FileList[j],
          Condor_folder+'\Landscapes\'+Name,FileList[j]);
      end;
    end;
    // now convert to 32 bit - TBD ??? or even 4, 8, 16 bit.
//  RenameFile(FileName, FileName+'.bmp');
//  Bitmap_24_To_Bitmap_32(FileName+'.bmp',FileName, false,1.0);
//  DeleteFile(FileName+'.bmp');
  end;
  MessageShow('Delete incomplete alternate flightplanner(s) as desired');

  // Shift .obj files
  MessageShow('Shifting Objects');
  u_Object.Memo_Message := Memo_Message;
  // make sure there is no old file
  DeleteFile(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.obj');
  // remove objects when cropping enabled
  // UTM relative, so Shifting needs UTM adjustment
  // also Shift object folders
  // for each object in cropped object list
  // - open the object file and search for textures and copy
  // can open file and copy blocks of 152 bytes for each object
  for i := 0 to Shift_Count-1 do begin
    with Shift_Array[i] do begin
      if ((Form_Shift.CheckBox_ApTweak.Checked) AND (t3f_Found)) then begin
        Append_OBJ_File(qtX * 90 + t3f_Error_X,
                        qtY * 90 + t3f_Error_Y,
                        Crop_Min_X * 90,
                        Crop_Max_X * 90,
                        Crop_Min_Y * 90,
                        Crop_Max_Y * 90,
          Condor_folder+'\Landscapes\'+LandscapeName,LandscapeName,
          Condor_folder+'\Landscapes\'+Name,Name);
      end else begin
        Append_OBJ_File(qtX * 90,  // needs to be UTM grid relative
                        qtY * 90,
                        Crop_Min_X * 90,
                        Crop_Max_X * 90,
                        Crop_Min_Y * 90,
                        Crop_Max_Y * 90,
          Condor_folder+'\Landscapes\'+LandscapeName,LandscapeName,
          Condor_folder+'\Landscapes\'+Name,Name);
      end;
    end;
  end;

  // Shift .apt files
  MessageShow('Copying Airports');
  // make sure there is no old file
  DeleteFile(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.apt');
  // lat and long only, no UTM, so copy is straight forward
  // remove airports when cropping enabled
  // also Shift 'Airports' folders
  // for each airport in cropped airport list
  // - open the G file and search for textures and copy
  // - open the O file and search for textures and copy
  // can open file and copy blocks of 72 bytes for each airport
  for i := 0 to Shift_Count-1 do begin
    with Shift_Array[i] do begin
      Append_APT_File(ce, {Form_Shift.}CheckBox_ApTweak.Checked, sh_E_W, sh_N_S,
        t3f_Found,
        Condor_folder+'\Landscapes\'+LandscapeName,LandscapeName,
        Condor_folder+'\Landscapes\'+{Shift_Array[i].}Name,{Shift_Array[i].}Name);
    end;
  end;

  // Shift .tdm thermal file
  MessageShow('Shifting thermal map');
  u_Thermal.Memo_Message := Memo_Message;
  u_Thermal.ProgressBar_Status := ProgressBar_Status;
  // first force size
//  ForceTDMsize(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.tdm');
  ForceTDMsize(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.tdm',128);
  // now Shift
  for i := 0 to Shift_Count-1 do begin
    with Shift_Array[i] do begin
      Merge_TDM_File(round(qtX),
                     round(qtY),
                     round(Crop_Min_X),
                     round(Crop_Max_X),
                     round(Crop_Min_Y),
                     round(Crop_Max_Y),
        Condor_folder+'\Landscapes\'+LandscapeName,LandscapeName+'.tdm',
        Condor_folder+'\Landscapes\'+Name,Name+'.tdm');
    end;
  end;

  if (mgVersion = 'V3') then begin
    // Shift .tm3 thermal file
    MessageShow('Shifting thermal tm3 map');
    u_Thermal.Memo_Message := Memo_Message;
    u_Thermal.ProgressBar_Status := ProgressBar_Status;
    // first force size
//    ForceTM3size(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.tm3');
    ForceTM3size(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.tm3',128);
    // now Shift
    for i := 0 to Shift_Count-1 do begin
      with Shift_Array[i] do begin
        Merge_TM3_File(round(qtX),
                       round(qtY),
                       round(Crop_Min_X),
                       round(Crop_Max_X),
                       round(Crop_Min_Y),
                       round(Crop_Max_Y),
          Condor_folder+'\Landscapes\'+LandscapeName,LandscapeName+'.tm3',
          Condor_folder+'\Landscapes\'+Name,Name+'.tm3');
      end;
    end;
  end;

  // Shift .cup files
  MessageShow('Copying Turnpoints');
  u_CUP.Memo_Message := Memo_Message;
  // make sure there is no old file
  DeleteFile(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.cup');
  // lat and long only, no UTM, so copy is straight forward
  // remove turnpoints when cropping enabled
  // can open file and each line for each turnpoint
  for i := 0 to Shift_Count-1 do begin
    Append_CUP_File(ce,
      Condor_folder+'\Landscapes\'+LandscapeName,LandscapeName,
      Condor_folder+'\Landscapes\'+Shift_Array[i].Name,Shift_Array[i].Name);
  end;

  if (mgVersion = 'V3') then begin
    // Shift .air files
    MessageShow('Copying Airspace');
    // make sure there is no old file
    DeleteFile(Condor_folder+'\Landscapes\'+LandscapeName+'\'+LandscapeName+'.air');
    // lat and long only, no UTM, so copy is straight forward
    // remove/crop airspace when cropping enabled
    // can open file and each line for each airspace
    for i := 0 to Shift_Count-1 do begin
      Append_AIR_File(ce,
        Condor_folder+'\Landscapes\'+LandscapeName,LandscapeName,
        Condor_folder+'\Landscapes\'+Shift_Array[i].Name,Shift_Array[i].Name);
    end;
  end;

  // Shift the 'Images' folders
  MessageShow('Copying Images');
  for i := 0 to Shift_Count-1 do begin
    // copy the Images folder if any
    FolderName := Condor_folder+'\Landscapes\'+
      Shift_Array[i].Name+'\Images';
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

  u_X_Cx.Memo_Message := Memo_Message;
  if (mgVersion = 'V3') then begin
    // Shift the 'AutoGen' folder - only copy textures that are actually used
    // need to offset XY tile indexes/names
    MessageShow('Shifting AutoGen Folder');
    for i := 0 to Shift_Count-1 do begin
      with Shift_Array[i] do begin
        Copy_ReIndex_Files(Type_C3D,
          sh_E_W, sh_N_S,
          Condor_folder+'\Landscapes\'+LandscapeName,
          Condor_folder+'\Landscapes\'+Name, Name);
      end;
    end;
  end;

  // create new .ini file
  MessageShow('Creating INI version file');
  MakeDummyINI(Condor_folder+'\Landscapes\'+LandscapeName,LandscapeName+'.ini');

  // Can't do with Condor_Tiles
  // create new .tha and .fha hash files with Landscape Editor.
  if (mgVersion = 'V3') then begin
    MessageShow('You now need to create the .FHA, .THA, .AHA, .OHA files');
  end else begin
    MessageShow('You now need to create the .FHA and .THA files');
  end;

  // use the Condor Landscape Editor to calculate the hashes
  //MessageShow('creating the .FHA and .THA files');
//  Execute_BatchFile(File_Folder, '???.bat', ''); // use batch file ?  or execute directly ?
//  C:\CST2\LandscapeEditor.exe -hash YOURVERYVERYBIGSCENERY

end;

{----------------------------------------------------------------------------}
procedure TForm_Shift.FormCreate(Sender: TObject);
begin
  Image_Grid.Picture.Bitmap.Create; // must be square for current code !
  Image_Grid.Picture.Bitmap.Width := Image_Grid.Width;
  Image_Grid.Picture.Bitmap.Height := Image_Grid.HeigHt;
  mBitmap := Image_Grid.Picture.Bitmap;
end;

{----------------------------------------------------------------------------}
procedure TForm_Shift.FormActivate(Sender: TObject);
begin
  CheckListBox_LandscapeList.Clear;  //clear the list
  CheckListBox_LandscapeList.Items.AddStrings(LandscapeList);
  if (LandscapeName <> '') then begin   // if 'Read Header' was done
    //look for Shift file and read if found
    Read_Shift_List;
  end;
end;

{----------------------------------------------------------------------------}
procedure TForm_Shift.Data_Change(Sender: TObject);
var
  i : Integer;
begin
  with CheckListBox_LandscapeList do begin
    for i := 0 to Items.Count-1 do begin
      if (Selected[i]) then begin
        Checked[i] := true;
      end else begin
        Checked[i] := false;
      end;
    end;
  end;

  Button_Create.Enabled := false;
  Button_Shift.Enabled := false;
end;

{----------------------------------------------------------------------------}
{
begin
  ce.xMin := 0;
  ce.xMax := 100;
  ce.yMin := 0;
  ce.yMax := 100;
  Append_CUP_File(ce,
    '.','Test_Out',
    '.','Test_In');
}
end.


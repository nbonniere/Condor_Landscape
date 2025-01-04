{
 * Unit_ObjectPlacer.pas
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
unit Unit_ObjectPlacer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls;

type
  // Nick - add two events to track Scrollbar movements
  TScrollBox=Class({VCL.}Forms.TScrollBox)
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
  private
    FOnScrollVert: TNotifyEvent;
    FOnScrollHorz: TNotifyEvent;
  public
   Property OnScrollVert:TNotifyEvent read FOnScrollVert Write FonScrollVert;
   Property OnScrollHorz:TNotifyEvent read FOnScrollHorz Write FonScrollHorz;
  End;

  TForm_ObjectPlacer = class(TForm)
    GroupBox_ObjectPlace: TGroupBox;
    GroupBox_Object: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Edit_FileName: TEdit;
    Edit_Easting: TEdit;
    Edit_Scale: TEdit;
    Edit_Elevation: TEdit;
    Edit_Rotation: TEdit;
    Edit_Northing: TEdit;
    ListBox_ObjectList: TListBox;
    Button_Exit: TButton;
    Button_Save: TButton;
    Button_Add: TButton;
    Button_Delete: TButton;
    Label_Tile: TLabel;
    ScrollBox_Image: TScrollBox;
    Image_Tile: TImage;
    Label_Coords: TLabel;
    Button_ZoomReset: TButton;
    Button_ZoomOut: TButton;
    Button_ZoomIn: TButton;
    UpDown_Easting: TUpDown;
    UpDown_Northing: TUpDown;
    GroupBox_Options: TGroupBox;
    RadioButton_DDS: TRadioButton;
    RadioButton_Terragen: TRadioButton;
    Label_ObjectCount: TLabel;
    OpenDialog_FileName: TOpenDialog;
    procedure ListBox_ObjectListMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Button_ExitClick(Sender: TObject);
    procedure Button_SaveClick(Sender: TObject);
    procedure Edit_ElevationExit(Sender: TObject);
    procedure Edit_EastingExit(Sender: TObject);
    procedure Edit_NorthingExit(Sender: TObject);
    procedure Edit_ScaleExit(Sender: TObject);
    procedure Edit_RotationExit(Sender: TObject);
    procedure Button_AddClick(Sender: TObject);
    procedure ShowItem(Sender: TObject);
    procedure Edit_FileNameExit(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button_DeleteClick(Sender: TObject);
    procedure ShowCoord(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image_TileMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ZoomRestore(Sender: TObject);
    procedure Button_ZoomResetClick(Sender: TObject);
    procedure Button_ZoomOutClick(Sender: TObject);
    procedure Button_ZoomInClick(Sender: TObject);
    procedure UpDown_EastingMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure UpDown_NorthingMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RadioButton_DDSMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RadioButton_TerragenMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Image_TileClick(Sender: TObject);
    procedure ScrollBox_ImageResize(Sender: TObject);
    procedure Image_TileMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListBox_ObjectListKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Edit_FileNameDblClick(Sender: TObject);
  private
    { Private declarations }
    function LoadTileBitmap(TileName : string) : boolean;
    procedure MyScrollHorz(Sender: TObject);
    procedure MyScrollVert(Sender: TObject);
  public
    { Public declarations }
    procedure Initialize(Sender: TObject);
  end;

var
  Form_ObjectPlacer : TForm_ObjectPlacer;

  Memo_Message : TMemo;  // external TMemo for messages
  CurrentLandscape : string;
  LandscapePathName : string;
  opVersion : string;
  WorkingFolder : string;

//---------------------------------------------------------------------------
implementation

{$R *.DFM}

uses
  ClipBrd,
  Unit_Main, Unit_Graphics, Unit_Coords,
  u_TileList, u_Object, u_SceneryHDR, u_VectorXY, u_BMP, u_DXT;

var
  GUI_State : (IdleScreen, SelectScreen, ScrollScreen, CancelScreen);
  ItemIndex : integer;
  ObjectsChanged : boolean;
  BitmapAvail : boolean;
  ObjectEasting, ObjectNorthing, ObjectRotation : double;
  BitMap_Save : TBitMap;
  opX, opY :double;
  opZoomScale, opRange : double;
  cX, cY : double;

// TScollBox addition
//---------------------------------------------------------------------------
procedure TScrollBox.WMHScroll(var Message: TWMHScroll);
begin
   inherited;
   if Assigned(FOnScrollHorz) then  FOnScrollHorz(Self);
end;

//---------------------------------------------------------------------------
procedure TScrollBox.WMVScroll(var Message: TWMVScroll);
begin
   inherited;
   if Assigned(FOnScrollVert) then  FOnScrollVert(Self);
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.MyScrollVert(Sender: TObject);
begin
  cY := (ScrollBox_Image.VertScrollBar.Position + (ScrollBox_Image.ClientHeight div 2))
        / (ScrollBox_Image.VertScrollBar.Range);
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.MyScrollHorz(Sender: TObject);
begin
  cX := (ScrollBox_Image.HorzScrollBar.Position + (ScrollBox_Image.ClientWidth div 2))
        / (ScrollBox_Image.HorzScrollBar.Range);
end;

{----------------------------------------------------------------------------}
Procedure MessageShow(Info : string);
begin
  if (Memo_Message <> nil) then begin
    Memo_Message.lines.add(Info);
  end;
end;

//---------------------------------------------------------------------------
Procedure Image_Tile_Clear;
begin
  // clear bitmap
  with Form_ObjectPlacer.Image_Tile do begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(rect(0,0,Width,Height));
  end;
end;

//---------------------------------------------------------------------------
Procedure Object_Change_Show(Changed, Show : Boolean);
begin
  ObjectsChanged := Changed;
  Form_ObjectPlacer.Button_Delete.Enabled := (Object_Count > 0);
  Form_ObjectPlacer.Button_Save.enabled := Changed;
  if (Show) then begin
    Form_ObjectPlacer.ShowItem(nil);
  end;
end;

//---------------------------------------------------------------------------
Procedure TForm_ObjectPlacer.Initialize(Sender: TObject);
var
  i : integer;

begin
  // load list box
  ListBox_ObjectList.clear;
  for i := 0 to Object_Count-1 do begin
    ListBox_ObjectList.Items.Append(Object_List[i].coName);
  end;
  Label_ObjectCount.Caption := IntToStr(Object_Count);
//  ItemIndex := ListBox_ObjectList.ItemIndex; // default empty is 0 ?
  ItemIndex := -1;
  opZoomScale := 1.0;
//  ObjectsChanged := false;
  Object_Change_Show(False, False);

  // blank to start
  Image_Tile_Clear;

  // also clear input boxes
  Edit_FileName.Clear;
  Edit_Easting.Clear;
  Edit_Scale.Clear;
  Edit_Elevation.Clear;
  Edit_Rotation.Clear;
  Edit_Northing.Clear;
end;

//---------------------------------------------------------------------------
var
  ObjectCorners : TCoordXY_Array;
  CentreMark : TCoordXY_Array;

//---------------------------------------------------------------------------
procedure DrawObject(TileIndex : integer);
var
  i : integer;
  Temp_CoordXY : TCoordXY;
  Object_CoordXY : TCoordXY;
  ScaleX , ScaleY : double;

begin
//  Object_CoordXY.X := 23040 - (ObjectEasting - TileList[TileIndex].TileUTMRight);
//  Object_CoordXY.Y := 23040 - (ObjectNorthing - TileList[TileIndex].TileUTMBottom);
  // for drawing need reference to top left instead of bottom right
  Object_CoordXY.X := opRange - xCoord;
  Object_CoordXY.Y := opRange - yCoord;

//  ScaleX := Form_ObjectPlacer.Image_Tile.Width/23040 * opZoomScale;
//  ScaleY := Form_ObjectPlacer.Image_Tile.Height/23040 * opZoomScale;
  with Form_ObjectPlacer do begin
    ScaleX := Image_Tile.Width/opRange * opZoomScale;
    ScaleY := Image_Tile.Height/opRange * opZoomScale;
  end;

  setlength(ObjectCorners,4);
  Temp_CoordXY.X := -20; Temp_CoordXY.Y := -20;
  ObjectCorners[0] := Temp_CoordXY;
  Temp_CoordXY.X := -20; Temp_CoordXY.Y := 20;
  ObjectCorners[1] := Temp_CoordXY;
  Temp_CoordXY.X := 20; Temp_CoordXY.Y := 20;
  ObjectCorners[2] := Temp_CoordXY;
  Temp_CoordXY.X := 20; Temp_CoordXY.Y := -20;
  ObjectCorners[3] := Temp_CoordXY;

  Rotate_Array(ObjectCorners, ObjectRotation);

//  for i := 0 to 3 do begin
//    Offset(AirportCorners[i],Airport_CoordXY);
//  end;
  Offset_Array(ObjectCorners,Object_CoordXY);

  with Form_ObjectPlacer.Image_Tile do begin
    Canvas.Pen.Mode := pmCopy; // needed for pixels[] !
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := clRed;
    Canvas.MoveTo(round(ObjectCorners[0].X * ScaleX), Round(ObjectCorners[0].Y * ScaleY));
    for i := 0 to 3 do begin
      Canvas.LineTo(round(ObjectCorners[i].X * ScaleX), Round(ObjectCorners[i].Y * ScaleY));
    end;
    Canvas.LineTo(round(ObjectCorners[0].X * ScaleX), Round(ObjectCorners[0].Y * ScaleY));
  end;

  setlength(CentreMark,4);
  Temp_CoordXY.X := -10; Temp_CoordXY.Y := 0;
  CentreMark[0] := Temp_CoordXY;
  Temp_CoordXY.X :=  10; Temp_CoordXY.Y := 0;
  CentreMark[1] := Temp_CoordXY;
//  Temp_CoordXY.X := 0; Temp_CoordXY.Y :=  10;
  Temp_CoordXY.X := 0; Temp_CoordXY.Y := 10;
  CentreMark[2] := Temp_CoordXY;
  Temp_CoordXY.X := 0; Temp_CoordXY.Y := -30; // longer to show rotation reference
  CentreMark[3] := Temp_CoordXY;

  Rotate_Array(CentreMark, ObjectRotation);

  Offset_Array(CentreMark,Object_CoordXY);

  with Form_ObjectPlacer.Image_Tile do begin
//    Canvas.Pen.Mode := pmCopy; // needed for pixels[] !
//    Canvas.Pen.Style := psSolid;
//    Canvas.Pen.Width := 1;
//    Canvas.Pen.Color := clRed;
    Canvas.MoveTo(round(CentreMark[0].X * ScaleX), Round(CentreMark[0].Y * ScaleY));
    Canvas.LineTo(round(CentreMark[1].X * ScaleX), Round(CentreMark[1].Y * ScaleY));
    Canvas.MoveTo(round(CentreMark[2].X * ScaleX), Round(CentreMark[2].Y * ScaleY));
    Canvas.LineTo(round(CentreMark[3].X * ScaleX), Round(CentreMark[3].Y * ScaleY));
  end;

end;

//---------------------------------------------------------------------------
Procedure ReCentre;
begin
  with Form_ObjectPlacer do begin
          ScrollBox_Image.HorzScrollBar.Position := trunc(cX *
            (ScrollBox_Image.HorzScrollBar.Range)-ScrollBox_Image.ClientWidth div 2);
          ScrollBox_Image.VertScrollBar.Position := trunc(cY *
            (ScrollBox_Image.VertScrollBar.Range)-ScrollBox_Image.ClientHeight div 2);
  end;
end;

//---------------------------------------------------------------------------
Procedure CentreObject;
begin
  cX := opX;
  cY := opY;
  ReCentre;
end;

// find group of 4 DDS tiles that surround the object coordinates
//---------------------------------------------------------------------------
Procedure Find_DDS_Tiles(Easting, Northing : double; var Col, Row : integer);
const
  QT_Range = Resolution * tColumns div 4;  // 23040 / 4 km
//var
//  Row, Col : integer;
begin
  // find half quarter tile to find nearest group of 4 tiles
  Col := trunc(Easting /(QT_Range/2));
  Row := trunc(Northing/(QT_Range/2));
  // airport quarter tile DDS name
//  Form_ObjectPlacer.Label_Tile.Caption := format('(q)%2.2d%2.2d',[Col div 2, Row div 2]);
  Form_ObjectPlacer.Label_Tile.Caption := format('(q)%s',[MakeTileName(Col div 2, Row div 2, TileNameMode)]);
  // find BR quarter tile
  Col := trunc((Col+1)/2)-1;      // bottom right DDS tile
  Row := trunc((Row+1)/2)-1;      // bottom right DDS tile
  // range check
  if (Col < 0) then begin
    Col := 0;
  end else begin
//    if (Col > TileColumnCount*4-2) then begin
//      Col := TileColumnCount*4-2;
    // quarter tiles - not necessarily complete tiles (4 quarter tiles)
    if (Col > ColumnCount div (tColumns div 4) -2) then begin
      Col := ColumnCount div (tColumns div 4) -2;
    end;
  end;
  if (Row < 0) then begin
    Row := 0;
  end else begin
    if (Row > RowCount div (tRows div 4) -2) then begin
      Row := RowCount div (tRows div 4) -2;
    end;
  end;

  // 4 tiles BR to B+1,R+1

  // object tile relative coords (relative to bottom right)
  {Unit_Graphics.}xReference := (Col*QT_Range);
  {Unit_Graphics.}yReference := (Row*QT_Range);
  {Unit_Graphics.}xCoord := ObjectEasting -  xReference;
  {Unit_Graphics.}yCoord := ObjectNorthing - yReference;
  // Relative fractional position (relative to top left)
  opRange := QT_Range*2; // metres, 2x2 quarter tiles
  opX := 1 - ({Unit_Graphics.}xCoord/opRange);
  opY := 1 - ({Unit_Graphics.}yCoord/opRange);
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.ShowItem(Sender: TObject);
const
  T_Range = Resolution * tColumns; // 23040 km
var
  i, j : integer;
  TileIndex : integer;
  TileRow, TileColumn : integer;

  DDS_Col, DDS_Row : integer;
  FilePicture: TPicture; // to load DDS tiles
  FileName : string;
  Temp : longint;
  DDS_Size : longint;
  OutOfResources : boolean;


begin
  if (ItemIndex <> -1) then begin
//    if (Object_Count > 1) then begin
    if (Object_Count > 0) then begin
      Button_Delete.Enabled := true;
    end else begin
      Button_Delete.Enabled := false;
    end;
    OutOfResources := false;
    BitmapAvail := false; // assume for now
    with Object_List[ItemIndex] do begin
      Edit_FileName.Text := coName;
      Edit_Easting.Text := format('%1.5f',[coEasting]);
      Edit_Northing.Text := format('%1.5f',[coNorthing]);
      Edit_Elevation.Text := format('%1.1f',[coElevation]);
      Edit_Scale.Text := format('%1.3f',[coScale]);
      Edit_Rotation.Text := format('%1.3f',[coRotation*180/PI]);

      //use lat,long to find corresponding tile
      if (TileOpen) then begin
        ObjectRotation := coRotation*180/PI;
        ObjectEasting := coEasting;                // make relative to scenery
        ObjectNorthing := coNorthing;
        TileColumn := trunc(coEasting/T_Range);
        TileRow := trunc(coNorthing/T_Range);
        TileIndex := TileRow*(TileColumnCount+1)+TileColumn;
        if (TileIndex >= TileCount) then begin
          BitmapAvail := false;
      //    Message('Object beyond scenery extents');
          exit;
        end;

        Screen.Cursor := crHourGlass;  // Let user know we're busy...
        // if DDS textures available, use 4 closest, otherwise use terragen tile
        if (RadioButton_DDS.Checked = true) then begin
          Find_DDS_Tiles(ObjectEasting, ObjectNorthing, DDS_Col, DDS_Row);
          // determine highest tile resolution
          DDS_Size := 0;
          for i := 0 to 2-1 do begin
            for j := 0 to 2-1 do begin
//              FileName := format('%s\Textures\t%2.2d%2.2d.dds',[Object_FolderName,DDS_Col+(1-i),DDS_Row+(1-j)]);
              FileName := format('%s\Textures\t%s.dds',[Object_FolderName,MakeTileName(DDS_Col+(1-i),DDS_Row+(1-j), TileNameMode)]);
              Temp := DXT_ImageWidth(FileName);
              if (Temp > DDS_Size) then begin
                DDS_Size := temp;
              end;
            end;
          end;
          // show a blank background if no files
          if (DDS_Size = 0) then begin
            DDS_Size := 512; // choose a default size
          end;

          // show a blank background if files are too large for 32 bit system
          // i.e. avoid crashing Condor_Tiles
          // could load next level Mip instead - TBD
          if (DDS_Size > 8192) then begin
            MessageShow('DDS file too large');
            OutOfResources := true;
            DDS_Size := 512; // choose a default size
          end;

          with Image_Tile.Picture.Bitmap do begin
            Image_Tile.Align := alClient;
            Image_Tile.AutoSize := true;
            Width := DDS_Size * 2;
            Height := DDS_Size * 2;
            Image_Tile.Stretch := false; // no stretch - 1:1 resolution to start
            Image_Tile_Clear;
          end;

          // show a blank background if files are too large for 32 bit system
          // i.e. avoid crashing Condor_Tiles
          // could shrink the picture as it loads instead - TBD
          if (OutOfResources) then begin
            Screen.Cursor := crDefault;  // no longer busy
            beep; exit;
          end else begin
            // load 4 dds tiles and draw onto Image_Tile
            for i := 0 to 2-1 do begin
              for j := 0 to 2-1 do begin
//                FileName := format('%s\Textures\t%2.2d%2.2d.dds',[Object_FolderName,DDS_Col+(1-i),DDS_Row+(1-j)]);
                FileName := format('%s\Textures\t%s.dds',[Object_FolderName,MakeTileName(DDS_Col+(1-i),DDS_Row+(1-j), TileNameMode)]);
                if (NOT FileExists(FileName)) then begin
//                BitmapAvail := false; // change - allow even if no files
                  // blank image
                  Screen.Cursor := crDefault;  // no longer busy
            //      Image_Tile_Clear;
            //      exit;
                  continue;
                end;
                FilePicture := TPicture.Create;
                try
                  FilePicture.LoadFromFile(FileName);
                  if (opVersion = 'V1') then begin
                    //rotate 180 deg
                    Rotate_180(FilePicture.Bitmap);
                  end;
                  try
                    with Image_Tile.Picture.Bitmap do begin
              {
                      if ((i = 0) AND (j=0)) then begin
                        Image_Tile.Align := alClient;
                        Image_Tile.AutoSize := true;
//                        Width := FilePicture.Width * 2;
                        Width := DDS_Size * 2;
//                        Height := FilePicture.Width * 2;
                        Height := DDS_Size * 2;
                        Image_Tile.Stretch := false; // no stretch - 1:1 resolution to start
                      end;
               }
//                      Image_Tile.Canvas.CopyMode := cmSrcCopy;
//                      Canvas.StretchDraw(Rect(i*FilePicture.Width, j*FilePicture.Height,
//                        (i+1)*FilePicture.Width-1, (j+1)*FilePicture.Height-1), FilePicture.Graphic);
                      Canvas.StretchDraw(Rect(i*Width div 2, j*Height div 2,
                        (i+1)*Width div 2-1, (j+1)*Height div 2-1), FilePicture.Graphic);
                    end;
                  finally
                  end;
                finally
                  FilePicture.Free;
                end;
              end;
            end;
          end;

          BitmapAvail := true;
          ZoomRestore(Sender); // after load/reload of tile
          CentreObject;
          //DrawObject(TileIndex);
          DrawObject(0);
        end else begin // try terragen tile        // Relative fractional position
//          opX := (1-(ObjectEasting - TileList[TileIndex].TileUTMRight)/T_Range);
//          opY := (1-(ObjectNorthing - TileList[TileIndex].TileUTMBottom)/T_Range);
          // object relative coords
//          xCoord := TileList[TileIndex].TileUTMRight - coEasting;
//          yCoord := TileList[TileIndex].TileUTMBottom - coNorthing;
          // scenery relative coords
//          {Unit_Graphics.}xCoord := TileList[TileIndex].TileUTMRight;
//          {Unit_Graphics.}yCoord := TileList[TileIndex].TileUTMBottom;
          xReference := TileList[TileIndex].TileUTMRight;
          yReference := TileList[TileIndex].TileUTMBottom;
          xCoord := ObjectEasting - xReference;
          yCoord := ObjectNorthing - yReference;
          // Relative fractional position (relative to top left)
          opX := 1 - ({Unit_Graphics.}xCoord/T_Range);
          opY := 1 - ({Unit_Graphics.}yCoord/T_Range);
          opRange := T_Range; // metres
//          TileName := format('%2.2d%2.2d',[trunc(coEasting/T_Range),trunc(coNorthing/T_Range)]);
          Label_Tile.Caption := TileList[TileIndex].TileName;
          // check if file are too large for 32 bit system
          // i.e. avoid crashing Condor_Tiles
          // could shrink the picture as it loads instead - TBD
          if (LoadTileBitmap(TileList[TileIndex].TileName) ) then begin
            // bitmap loaded
          end else begin
            // blank image
            Image_Tile_Clear;
            // default size
            Image_Tile.Picture.Bitmap.Width :=  512;
            Image_Tile.Picture.Bitmap.Height := 512;
          end;
          // change - allow even if no file
          BitmapAvail := true; // although it could be blank
          ZoomRestore(Sender); // after load/reload of tile
          CentreObject;
//          DrawObject(TileIndex);
          DrawObject(0);
        end;
        Screen.Cursor := crDefault;  // no longer busy
      end else begin
        // blank image
        Image_Tile_Clear;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
var
//  FPanning: Boolean;
  FMousePos: TPoint;

// mouse move
//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.ShowCoord(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Horiz, Vert : double;
begin
//  if (FPanning) then begin
  if (Gui_State = ScrollScreen) then begin
    with ScrollBox_Image do
    begin
      HorzScrollBar.Position := HorzScrollBar.Position + (FMousePos.X - X);
      MyScrollHorz(Sender);
      VertScrollBar.Position := VertScrollBar.Position + (FMousePos.Y - Y);
      MyScrollVert(Sender);
    end;
  end;

  if (BitmapAvail) then begin
    Horiz := xReference{xCoord} + opRange{Resolution*tColumns}*
//      (Image_Tile.Picture.Width-1-X)/Image_Tile.Picture.Width;
      (Image_Tile.Width-1-X)/Image_Tile.Width;
    Vert := yReference{yCoord} + opRange{Resolution*tRows}*
//      (Image_Tile.Picture.Height-1-Y)/Image_Tile.Picture.Height;
      (Image_Tile.Height-1-Y)/Image_Tile.Height;
    Label_Coords.Caption := format('%1.2f,%1.2f',[
      (Horiz),
      (Vert)
      ]);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.Image_TileMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ( (ssCtrl in Shift) AND (Button = mbLeft) ) then begin //scroll bitmap
    GUI_State := ScrollScreen;
    FMousePos.X := X;
    FMousePos.Y := Y;
  end else begin
    if ( (ssShift in Shift) AND (Button = mbLeft) ) then begin //scroll bitmap
      GUI_State := SelectScreen;
    end else begin
      GUI_State := IdleScreen;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.Image_TileMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CommaPos : integer;
  Str : string;

begin
  if (Button = mbRight) then begin
    Form_Coords.ShowModal;
  end;

  if (Gui_State = SelectScreen) then begin
    with Object_List[ItemIndex] do begin
//      Clipboard.AsText := Label_Coords.Caption;
      CommaPos := pos(',',Label_Coords.Caption);
      if (CommaPos <> 0) then begin
        Str := copy(Label_Coords.Caption,1,CommaPos-1);
        coEasting := StrtoFloat(Str);
        Edit_Easting.Text := format('%1.2f',[coEasting]);;
        Str := copy(Label_Coords.Caption, CommaPos+1, length(Label_Coords.Caption));
        coNorthing := StrtoFloat(Str);
        Edit_Northing.Text := format('%1.2f',[coNorthing]);
//        default coScale ?
//        default coRotation ?
//        default coElevation ?
        // now update the screen
        Object_Change_Show(True, True);
      end;
    end;
  end;
  GUI_State := IdleScreen;
end;

//---------------------------------------------------------------------------
function TForm_ObjectPlacer.LoadTileBitmap(TileName : string) : boolean;
begin
  result := false; // assume at first
  if (NOT Unit_Main.Form_Main.GetTileFile(TileName)) then begin
    Exit;
  end else begin
    // check if too large to load - avoid crash
    if (BMP_ImageWidth(tFileName) > 32768) then begin
      MessageShow('Terragen bitmap too large');
      beep; Exit;
    end;
    // set image to auto take its size from picture 1:1 and fit in window
    Image_Tile.Align := alClient;
    Image_Tile.AutoSize := true;
    Image_Tile.Stretch := false; // no stretch - 1:1 resolution to start
//    tFileName := Path+'\'+TileName+'.bmp';
    Image_Tile.Picture.LoadFromFile(tFileName);
//    Image_Tile.Canvas.CopyMode := cmSrcCopy; // try to make sure colors are correct - no go
// if 256 color bitmap, drawing on top of bitmap will use the 256 color palette !
// any color will use the closest color in palette -> approx color
// convert to pf24 bit for absolute color - works!
    if (Image_Tile.Picture.Bitmap.PixelFormat <> pf24bit) then begin
      Image_Tile.Picture.Bitmap.PixelFormat := pf24bit;
    end;
    ScrollBox_Image.HorzScrollBar.Range := Image_Tile.Picture.Width;
    ScrollBox_Image.VertScrollBar.Range := Image_Tile.Picture.Height;
    result := true;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.ListBox_ObjectListMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ItemIndex := ListBox_ObjectList.ItemAtPos(point(X,Y), true);
  if (ItemIndex <> -1) then begin
    opZoomScale := 1.0;
    ShowItem(Sender);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.ListBox_ObjectListKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: begin
      ItemIndex := ListBox_ObjectList.ItemIndex;
      if (ItemIndex <> -1) then begin
        opZoomScale := 1.0;
        ShowItem(Sender);
      end;
    end;
    else begin
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.Button_AddClick(Sender: TObject);
begin
  SetLength(Object_List,Object_Count+1);
  if (Object_Count >0) then begin
    Object_list[Object_Count] := Object_list[Object_Count-1]; //default values from previous
  end else begin
    Object_list[Object_Count] := CondorObject_Default; //default values
  end;
  Object_List[Object_Count].coName := 'New.X.CX.c3d';
  ListBox_ObjectList.Items.Append(Object_List[Object_Count].coName);
  INC(Object_Count);   Label_ObjectCount.Caption := IntToStr(Object_Count);
//  ObjectsChanged := true;
  ListBox_ObjectList.ItemIndex := ListBox_ObjectList.Items.Count-1;
  ItemIndex := ListBox_ObjectList.ItemIndex;
  opZoomScale := 1.0;
//  ShowItem(Sender);
  Object_Change_Show(True, True);
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.Button_DeleteClick(Sender: TObject);
var
  i : integer;
begin
  if (ItemIndex <> -1) then begin
    for i := ItemIndex to Object_Count-1-1 do begin
      Object_list[i] := Object_list[i+1];
    end;
    DEC(Object_Count); Label_ObjectCount.Caption := IntToStr(Object_Count);
    SetLength(Object_List,Object_Count);
//    ObjectsChanged := true;
    ListBox_ObjectList.Items.Delete(ItemIndex);
    if (Object_Count = 0) then begin
      ItemIndex := -1; // force not 0
    end else begin
      ItemIndex := ListBox_ObjectList.ItemIndex;
    end;  
    opZoomScale := 1.0;
//    ShowItem(Sender);
    Object_Change_Show(True, True);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if (ObjectsChanged = true) then begin
    if MessageDlg('Objets changed, Exit anyway ?', mtConfirmation,
        [mbYes, mbNo], 0) = mrNo then begin
      CanClose := False;
    end;
    CurrentLandscape := ''; // force a reload
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.Button_ExitClick(Sender: TObject);
begin
  Close;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.Button_SaveClick(Sender: TObject);
begin
  if (ObjectsChanged) then begin
    WriteObjectFile;
//    ObjectsChanged := false;
    Object_Change_Show(False, False);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.Edit_FileNameExit(Sender: TObject);
begin
  if (Edit_FileName.Modified) then begin
    if (ItemIndex <> -1) then begin
      ListBox_ObjectList.Items[ItemIndex] := Edit_FileName.Text;
      with Object_List[ItemIndex] do begin
        coName := Edit_FileName.Text;
      end;
//      ObjectsChanged := true;
      Object_Change_Show(True, False);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.Edit_EastingExit(Sender: TObject);
begin
  if (Edit_Easting.Modified) then begin
    if (ItemIndex <> -1) then begin
      with Object_List[ItemIndex] do begin
        coEasting := StrtoFloat(Edit_Easting.Text);
        Edit_Easting.Text := format('%1.5f',[coEasting]);
      end;
//      ObjectsChanged := true;
//      ShowItem(Sender);
      Object_Change_Show(True, True);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.Edit_NorthingExit(Sender: TObject);
begin
  if (Edit_Northing.Modified) then begin
    if (ItemIndex <> -1) then begin
      with Object_List[ItemIndex] do begin
        coNorthing := StrtoFloat(Edit_Northing.Text);
        Edit_Northing.Text := format('%1.5f',[coNorthing]);
      end;
//      ObjectsChanged := true;
//      ShowItem(Sender);
      Object_Change_Show(True, True);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.Edit_ScaleExit(Sender: TObject);
begin
  if (Edit_Scale.Modified) then begin
    if (ItemIndex <> -1) then begin
      with Object_List[ItemIndex] do begin
        coScale := StrtoFloat(Edit_Scale.Text);
        Edit_Scale.Text := format('%1.3f',[coScale]);
      end;
//      ObjectsChanged := true;
      Object_Change_Show(True, False);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.Edit_ElevationExit(Sender: TObject);
begin
  if (Edit_Elevation.Modified) then begin
    if (ItemIndex <> -1) then begin
      with Object_List[ItemIndex] do begin
        coElevation := StrtoFloat(Edit_Elevation.Text);
        Edit_Elevation.Text := format('%1.1f',[coElevation]);
      end;
//      ObjectsChanged := true;
      Object_Change_Show(True, False);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.Edit_RotationExit(Sender: TObject);
begin
  if (Edit_Rotation.Modified) then begin
    if (ItemIndex <> -1) then begin
      with Object_List[ItemIndex] do begin
        coRotation := StrtoFloat(Edit_Rotation.Text)*PI/180;
        Edit_Rotation.Text := format('%1.3f',[coRotation*180/PI]);
      end;
//      ObjectsChanged := true;
      Object_Change_Show(True, False);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.FormCreate(Sender: TObject);
begin
  // added scrollbar events
  ScrollBox_Image.OnScrollVert := MyScrollVert;
  ScrollBox_Image.OnScrollHorz := MyScrollHorz;

  CurrentLandscape := '';
  BitMap_Save := TBitMap.Create;
  Image_tile.Hint := 'Ctrl-Left-Mouse to Pan'#13#10'Shift-Left Mouse to select';
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.FormDestroy(Sender: TObject);
begin
  Bitmap_Save.Free;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.UpDown_EastingMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Delta : double;
begin
  if ((ItemIndex <> -1) AND (BitmapAvail)) then begin
    if (ssShift in Shift) then begin
      Delta := 10;
    end else begin
      Delta := 1;
    end;
    if (ssCtrl in Shift) then begin
      Delta := Delta * 10;
    end;
    if (X > UpDown_Easting.Height div 2) then begin // Move right
      Delta := - Delta;
    end else begin // Move left
    end;
    with Object_List[ItemIndex] do begin
      coEasting := coEasting + Delta;
    end;
//    ObjectsChanged := true;
//    ShowItem(Sender);
    Object_Change_Show(True, True);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.UpDown_NorthingMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Delta : double;
begin
  if ((ItemIndex <> -1) AND (BitmapAvail)) then begin
    if (ssShift in Shift) then begin
      Delta := 10;
    end else begin
      Delta := 1;
    end;
    if (ssCtrl in Shift) then begin
      Delta := Delta * 10;
    end;
    if (Y < UpDown_Northing.Height div 2) then begin // Move Up
    end else begin // Move Down
      Delta := - Delta;
    end;
    with Object_List[ItemIndex] do begin
      coNorthing := coNorthing + Delta;
    end;
//    ObjectsChanged := true;
//    ShowItem(Sender);
    Object_Change_Show(True, True);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.RadioButton_DDSMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  opZoomScale := 1.0; // or properly adjust the zoom due to bitmap size change
  ShowItem(Sender);
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.RadioButton_TerragenMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  opZoomScale := 1.0; // or properly adjust the zoom due to bitmap size change
  ShowItem(Sender);
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.Button_ZoomInClick(Sender: TObject);
begin
  if (BitmapAvail) then begin
    // allow re-size of image - picture stays the same
    Image_Tile.Align := alNone;
    Image_Tile.AutoSize := false;
    Image_Tile.Stretch := true;
    // scale image by 1.5
//    opZoomScale := opZoomScale / 1.5;
    opZoomScale := opZoomScale * Image_Tile.Width; // make more exact using before and after
//    Image_Tile.Width := Image_Tile.Width + Image_Tile.Width div 2;
//    Image_Tile.Height := Image_Tile.Height + Image_Tile.Height div 2;
    Image_Tile.Width := round(Image_Tile.Width * 1.5);
    Image_Tile.Height := round(Image_Tile.Height * 1.5);
    opZoomScale := opZoomScale / Image_Tile.Width;
    ScrollBox_Image.HorzScrollBar.Range := Image_Tile.Width;
    ScrollBox_Image.VertScrollBar.Range := Image_Tile.Height;
    // now refresh
//    CentreObject;
    ReCentre;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.Button_ZoomOutClick(Sender: TObject);
begin
  if (BitmapAvail) then begin
    // allow re-size of image - picture stays the same
    Image_Tile.Align := alNone;
    Image_Tile.AutoSize := false;
    Image_Tile.Stretch := true;
    // scale image by 3/4
//    opZoomScale := opZoomScale / 0.75;
    opZoomScale := opZoomScale * Image_Tile.Width; // make more exact using before and after
//    Image_Tile.Width := Image_Tile.Width - Image_Tile.Width div 4;
//    Image_Tile.Height := Image_Tile.Height - Image_Tile.Height div 4;
    Image_Tile.Width := round(Image_Tile.Width / 1.5);
    Image_Tile.Height := round(Image_Tile.Height / 1.5);
    opZoomScale :=  opZoomScale / Image_Tile.Width;
    ScrollBox_Image.HorzScrollBar.Range := Image_Tile.Width;
    ScrollBox_Image.VertScrollBar.Range := Image_Tile.Height;
    // now refresh
//    CentreObject;
    ReCentre;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.Button_ZoomResetClick(Sender: TObject);
begin
  if (BitmapAvail) then begin
    opZoomScale := 1.0;
    Image_Tile.Width := Image_Tile.Picture.Width;
    Image_Tile.Height := Image_Tile.Picture.Width;
    ScrollBox_Image.HorzScrollBar.Range := Image_Tile.Picture.Width;
    ScrollBox_Image.VertScrollBar.Range := Image_Tile.Picture.Height;
    // now refresh
    CentreObject;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.ZoomRestore(Sender: TObject);
begin
  if (BitmapAvail) then begin
    // allow re-size of image - picture stays the same
    Image_Tile.Align := alNone;
    Image_Tile.AutoSize := false;
    Image_Tile.Stretch := true;
    // restore zoom after reload of tile
    Image_Tile.Width := round(Image_Tile.Picture.Width / opZoomScale);
    Image_Tile.Height := round(Image_Tile.Picture.Height / opZoomScale);
    ScrollBox_Image.HorzScrollBar.Range := Image_Tile.Width;
    ScrollBox_Image.VertScrollBar.Range := Image_Tile.Height;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.Image_TileClick(Sender: TObject);
begin
  Clipboard.AsText := Label_Coords.Caption;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.ScrollBox_ImageResize(Sender: TObject);
begin
  Recentre;  // on current centre cX, cY
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    ord('S'), ord('s'): begin
      if (ssCtrl in Shift) then begin
        Button_SaveClick(Sender);
        key := 0; // so that other components won't see this keypress
      end;
    end;
    else begin
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.Edit_FileNameDblClick(Sender: TObject);
begin
  // dialog to select object file - must be .c3d extension
  OpenDialog_FileName.Filter :=
    'Object files (*.C3D)|*.C3D|All files (*.*)|*.*';
  OpenDialog_FileName.FileName := '';
  OpenDialog_FileName.InitialDir := Object_FolderName+'\World\Objects';
  if (OpenDialog_FileName.Execute) then begin
    // extract relative filename
    Edit_FileName.Text := ExtractRelativePath(
      Object_FolderName+'\World\Objects\',
      OpenDialog_FileName.FileName);
  end;
end;

//---------------------------------------------------------------------------

end.


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
  private
    { Private declarations }
    function LoadTileBitmap(TileName : string) : boolean;
  public
    { Public declarations }
    procedure Initialize(Sender: TObject);
  end;

var
  Form_ObjectPlacer : TForm_ObjectPlacer;

  CurrentLandscape : string;

//---------------------------------------------------------------------------
implementation

{$R *.DFM}

uses Unit_Main, Unit_Graphics, Unit_Coords,
     u_TileList, u_Object, u_SceneryHDR, u_VectorXY;

var
  ItemIndex : integer;
  ObjectsChanged : boolean;
  BitmapAvail : boolean;
  ObjectEasting, ObjectNorthing, ObjectRotation : double;
  BitMap_Save : TBitMap;
  opX, opY, opZoomScale : double;

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
  ItemIndex := ListBox_ObjectList.ItemIndex;
  ObjectsChanged := false;

  // clear bitmap
  with Image_Tile do begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(rect(0,0,Width,Height));
  end;

  // also clear input boxes...
end;

//---------------------------------------------------------------------------
var
  ObjectCorners : CoordXY_Array;
  CentreMark : CoordXY_Array;

//---------------------------------------------------------------------------
procedure DrawObject(TileIndex : integer);
var
  i : integer;
  Temp_CoordXY : CoordXY;
  Object_CoordXY : CoordXY;
  ScaleX , ScaleY : double;
begin
  Object_CoordXY.X := 23040 - (ObjectEasting - TileList[TileIndex].TileUTMRight);
  Object_CoordXY.Y := 23040 - (ObjectNorthing - TileList[TileIndex].TileUTMBottom);

  ScaleX := Form_ObjectPlacer.Image_Tile.Width/23040 * opZoomScale;
  ScaleY := Form_ObjectPlacer.Image_Tile.Height/23040 * opZoomScale;

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
Procedure CentreObject;
begin
  with Form_ObjectPlacer do begin
    with Object_List[ItemIndex] do begin
//          ScrollBox_Image.HorzScrollBar.Position := trunc((1-(coEasting -
//            TileList[TileIndex].TileUTMRight)/23040)*
//            (ScrollBox_Image.HorzScrollBar.Range-ScrollBox_Image.ClientWidth));
  //        ScrollBox_Image.HorzScrollBar.Position := trunc((1-(ObjectEasting{coEasting} -
  //          TileList[TileIndex].TileUTMRight)/23040)*
          ScrollBox_Image.HorzScrollBar.Position := trunc(opX *
            (ScrollBox_Image.HorzScrollBar.Range)-ScrollBox_Image.ClientWidth div 2);
//          ScrollBox_Image.VertScrollBar.Position := trunc((1-(coNorthing -
//            TileList[TileIndex].TileUTMBottom)/23040)*
//            (ScrollBox_Image.VertScrollBar.Range-ScrollBox_Image.ClientHeight));
  //        ScrollBox_Image.VertScrollBar.Position := trunc((1-(ObjectNorthing{coNorthing} -
  //          TileList[TileIndex].TileUTMBottom)/23040)*
          ScrollBox_Image.VertScrollBar.Position := trunc(opY *
            (ScrollBox_Image.VertScrollBar.Range)-ScrollBox_Image.ClientHeight div 2);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.ShowItem(Sender: TObject);
var
  TileIndex : integer;
  TileRow, TileColumn : integer;

begin
  begin
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
        TileColumn := trunc(coEasting/23040);
        TileRow := trunc(coNorthing/23040);
        TileIndex := TileRow*(TileColumnCount+1)+TileColumn;
        if (TileIndex >= TileCount) then begin
          BitmapAvail := false;
      //    Message('Object beyond scenery extents');
          exit;
        end;
        // Relative fractional position
        opX := (1-(ObjectEasting - TileList[TileIndex].TileUTMRight)/23040);
        opY := (1-(ObjectNorthing - TileList[TileIndex].TileUTMBottom)/23040);
        // scenery relative coords
        Unit_Graphics.xCoord := TileList[TileIndex].TileUTMRight;
        Unit_Graphics.yCoord := TileList[TileIndex].TileUTMBottom;
        // object relative coords
//        xCoord := TileList[TileIndex].TileUTMRight - coEasting;
//        yCoord := TileList[TileIndex].TileUTMBottom - coNorthing;
//        TileName := format('%2.2d%2.2d',[trunc(coEasting/23040),trunc(coNorthing/23040)]);
        Label_Tile.Caption := TileList[TileIndex].TileName;
        if (LoadTileBitmap(TileList[TileIndex].TileName)) then begin
          BitmapAvail := true;
          ZoomRestore(Sender); // after load/reload of tile
          CentreObject;
          DrawObject(TileIndex);
        end else begin
          BitmapAvail := false;
        end;
      end else begin
        BitmapAvail := false;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.ShowCoord(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Horiz, Vert : double;
begin
  if (BitmapAvail) then begin
    Horiz := xCoord + Resolution*tColumns*
//      (Image_Tile.Picture.Width-1-X)/Image_Tile.Picture.Width;
      (Image_Tile.Width-1-X)/Image_Tile.Width;
    Vert := yCoord + Resolution*tRows*
//      (Image_Tile.Picture.Height-1-Y)/Image_Tile.Picture.Height;
      (Image_Tile.Height-1-Y)/Image_Tile.Height;
    Label_Coords.Caption := format('%1.2f,%1.2f',[
      (Horiz),
      (Vert)
      ]);
  end;
end;

//---------------------------------------------------------------------------
function TForm_ObjectPlacer.LoadTileBitmap(TileName : string) : boolean;
begin
      if (NOT Unit_Main.Form_Main.GetTileFile(TileName)) then begin
        result := false;
      end else begin
//        tFileName := Path+'\'+TileName+'.bmp';
        Image_Tile.Picture.LoadFromFile(tFileName);
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
procedure TForm_ObjectPlacer.Button_AddClick(Sender: TObject);
begin
  SetLength(Object_List,Object_Count+1);
  Object_list[Object_Count] := Object_list[Object_Count-1]; //default values
  Object_List[Object_Count].coName := 'New.X.CX.c3d';
  ListBox_ObjectList.Items.Append(Object_List[Object_Count].coName);
  INC(Object_Count);
  ObjectsChanged := true;
  ListBox_ObjectList.ItemIndex := ListBox_ObjectList.Items.Count-1;
  ItemIndex := ListBox_ObjectList.ItemIndex;
  opZoomScale := 1.0;
  ShowItem(Sender);
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
    DEC(Object_Count);
    ObjectsChanged := true;
    ListBox_ObjectList.Items.Delete(ItemIndex);
    ItemIndex := ListBox_ObjectList.ItemIndex;
    opZoomScale := 1.0;
    ShowItem(Sender);
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
  end;
  ObjectsChanged := false;
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
      ObjectsChanged := true;
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
      ObjectsChanged := true;
      ShowItem(Sender);
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
      ObjectsChanged := true;
      ShowItem(Sender);
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
      ObjectsChanged := true;
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
      ObjectsChanged := true;
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
      ObjectsChanged := true;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.FormDestroy(Sender: TObject);
begin
  Bitmap_Save.Free;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.FormCreate(Sender: TObject);
begin
  CurrentLandscape := '';
  BitMap_Save := TBitMap.Create;
end;

//---------------------------------------------------------------------------
procedure TForm_ObjectPlacer.Image_TileMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) then begin
    Form_Coords.ShowModal;
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
    CentreObject;
  end;
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
    CentreObject;
  end;
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
    ObjectsChanged := true;
    ShowItem(Sender);
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
    ObjectsChanged := true;
    ShowItem(Sender);
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
end.


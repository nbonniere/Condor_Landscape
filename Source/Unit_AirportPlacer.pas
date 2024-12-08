{
 * Unit_AirportPlacer.pas
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
unit Unit_AirportPlacer;

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

  TForm_AirportPlacer = class(TForm)
    GroupBox_AirportPlace: TGroupBox;
    GroupBox_Airport: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label_Direction: TLabel;
    Label_Length: TLabel;
    Label6: TLabel;
    Edit_AirportName: TEdit;
    Edit_Latitude: TEdit;
    Edit_Altitude: TEdit;
    Edit_Direction: TEdit;
    Edit_Length: TEdit;
    Edit_Longitude: TEdit;
    Edit_Width: TEdit;
    Edit_Frequency: TEdit;
    ListBox_ObjectList: TListBox;
    Button_Exit: TButton;
    Button_Save: TButton;
    Button_Add: TButton;
    Button_Delete: TButton;
    Label_Width: TLabel;
    RadioGroup_Surface: TRadioGroup;
    RadioButton_Grass: TRadioButton;
    Label8: TLabel;
    CheckBox_Primary_Reverse: TCheckBox;
    CheckBox_Tow_Primary_Left: TCheckBox;
    CheckBox_Tow_Secondary_Left: TCheckBox;
    GroupBox1: TGroupBox;
    TreeView_G: TTreeView;
    GroupBox2: TGroupBox;
    ScrollBox_Image: TScrollBox;
    Image_Tile: TImage;
    Label_Coords: TLabel;
    Label_Tile: TLabel;
    UpDown_Longitude: TUpDown;
    UpDown_Latitude: TUpDown;
    Button_ZoomIn: TButton;
    Button_ZoomOut: TButton;
    Button_ZoomReset: TButton;
    GroupBox_Options: TGroupBox;
    Panel_View: TPanel;
    RadioButton_Terragen: TRadioButton;
    RadioButton_DDS: TRadioButton;
    Panel_Details: TPanel;
    RadioButton_APT: TRadioButton;
    Label_UTM: TLabel;
    Label_AirportCount: TLabel;
    Label_H_pos: TLabel;
    RadioButton_Elev: TRadioButton;
    RadioButton_Paved: TRadioButton;
    Panel1: TPanel;
    CheckBox_G_File: TCheckBox;
    CheckBox_O_File: TCheckBox;
    TreeView_O: TTreeView;
    Button_HiResRunway: TButton;
    Button_Help: TButton;
    procedure ListBox_ObjectListMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Button_ExitClick(Sender: TObject);
    procedure Button_SaveClick(Sender: TObject);
    procedure Edit_DirectionExit(Sender: TObject);
    procedure Edit_LatitudeExit(Sender: TObject);
    procedure Edit_LongitudeExit(Sender: TObject);
    procedure Edit_AltitudeExit(Sender: TObject);
    procedure Edit_LengthExit(Sender: TObject);
    procedure Button_AddClick(Sender: TObject);
    procedure ShowItem(Sender: TObject);
    procedure Edit_WidthExit(Sender: TObject);
    procedure Edit_FrequencyExit(Sender: TObject);
    procedure Edit_AirportNameExit(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CheckBox_Primary_ReverseMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CheckBox_Tow_Primary_LeftMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CheckBox_Tow_Secondary_LeftMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RadioButton_GrassMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RadioButton_PavedMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShowCoord(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure UpDown_LatitudeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure UpDown_LongitudeMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Button_ZoomInClick(Sender: TObject);
    procedure Button_ZoomOutClick(Sender: TObject);
    procedure Button_ZoomResetClick(Sender: TObject);
    procedure ZoomRestore(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RadioButton_TerragenMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RadioButton_DDSMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image_TileClick(Sender: TObject);
    procedure ScrollBox_ImageResize(Sender: TObject);
    procedure RadioButton_APTMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RadioButton_ElevMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Image_TileMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image_TileMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button_DeleteClick(Sender: TObject);
    procedure ListBox_ObjectListKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CheckBox_G_FileMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CheckBox_O_FileMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button_HiResRunwayClick(Sender: TObject);
    procedure Label_DirectionDblClick(Sender: TObject);
    procedure Label_LengthDblClick(Sender: TObject);
    procedure Label_WidthDblClick(Sender: TObject);
    procedure Label3DblClick(Sender: TObject);
    procedure Label2DblClick(Sender: TObject);
    procedure Button_HelpClick(Sender: TObject);
  private
    { Private declarations }
    function LoadTileBitmap(TileName : string) : boolean;
    procedure MyScrollHorz(Sender: TObject);
    procedure MyScrollVert(Sender: TObject);
    procedure Search_Airport_Details;
  public
    { Public declarations }
    procedure Initialize(Sender: TObject);
  end;

var
  Form_AirportPlacer : TForm_AirportPlacer;

  Memo_Message : TMemo;  // external TMemo for messages
  CurrentLandscape : string;
  apVersion : string;

//---------------------------------------------------------------------------
implementation

{$R *.DFM}

uses
  FileCtrl, ClipBrd, Math,
  Unit_Graphics, Unit_Main, u_MakeGDAL, u_MakeDDS, Unit_HiResRunway,
  u_Terrain, u_Airport, u_TileList, u_UTM, u_MakeGMID,
  u_SceneryHDR, u_X_CX, u_VectorXY, u_BMP, u_DXT, Unit_Help;

var
  GUI_State : (IdleScreen, DirectionSelectScreen, CentreSelectScreen,
    LengthSelectScreen, WidthSelectScreen, ScrollScreen, CancelScreen);
  ItemIndex : integer;
  AirportsChanged : boolean;
  BitmapAvail : boolean;
  AirportTileIndex : integer;
  AirportEasting, AirportNorthing, AirportDirection : double;
  AirportLength, AirportWidth : double;
  Airport_Primary_Reversed : Boolean;
  Airport_Tow_Primary_Left : Boolean;
  Airport_Tow_Secondary_Left : Boolean;
  LatDegPerM, LongDegPerM : double;
  apX, apY : double;  // airport centre relative
  apBR_X, apBR_Y : double;
  apZoomScale, apRange : double;
  cX, cY : double;  // current centre relative
//  DDS_Bitmap : TBitMap;

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
procedure TForm_AirportPlacer.MyScrollVert(Sender: TObject);
begin
  cY := (ScrollBox_Image.VertScrollBar.Position + (ScrollBox_Image.ClientHeight div 2))
        / (ScrollBox_Image.VertScrollBar.Range);
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.MyScrollHorz(Sender: TObject);
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
  with Form_AirportPlacer.Image_Tile do begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := {clBtnFace}{clGray}clSilver;
    Canvas.FillRect(rect(0,0,Width,Height));
  end;
end;

//---------------------------------------------------------------------------
Procedure TForm_AirportPlacer.Initialize(Sender: TObject);
var
  i : integer;

begin
  // load list box
  ListBox_ObjectList.clear;
  for i := 0 to Airport_Count-1 do begin
    ListBox_ObjectList.Items.Append(Airport_List[i].apName);
  end;
  Label_AirportCount.Caption := IntToStr(Airport_Count);
  ItemIndex := -1;
  apZoomScale := 1.0;
  AirportsChanged := false;

  // clear treeview
  ClearTreeView(TreeView_O);
  ClearTreeView(TreeView_G);

  // blank image to start
  Image_Tile_Clear;

  // also clear input boxes
  Edit_AirportName.Clear;
  Edit_Latitude.Clear;
  Edit_Altitude.Clear;
  Edit_Direction.Clear;
  Edit_Length.Clear;
  Edit_Longitude.Clear;
  Edit_Width.Clear;
  Edit_Frequency.Clear;
  CheckBox_Primary_Reverse.checked := False;
  CheckBox_Tow_Primary_Left.checked := False;
  CheckBox_Tow_Secondary_Left.checked := False;
end;

//---------------------------------------------------------------------------
Procedure Airport_Change_Show(Changed, Show : Boolean);
begin
  AirportsChanged := Changed;
  Form_AirportPlacer.Button_Save.enabled := Changed;
  if (Show) then begin
    Form_AirportPlacer.ShowItem(nil);
  end;
end;

//---------------------------------------------------------------------------
var
  AirportCorners : TCoordXY_Array;
  CentreMark : TCoordXY_Array;
  TowPlaneTrack : TCoordXY_Array;
  GliderTrack : TCoordXY_Array;
  WindSock : TCoordXY_Array;    // V1

  G_Object : TArray_CoordXY_Array; // G file asphalt and grass
  O_Object_Outline : TCoordXY_Array; // O file object

  TP_CR : TCoordXY;
  TP_TD : TCoordXY;
  TP_PK : TCoordXY;
  GL_TO : TCoordXY;
  WS    : TCoordXY;

// V1
// note - drawing on canvas has Y reversed !
// X is reversed if take-off is on opposite side
// Glider take-off position =    (+17,            +200 -runwy_L/2) // 14..17 - seems to move sometimes
// TowPlane touchdown position = ( -5,            +50  -runwy_L/2)
// TowPlane corner position =    (-75,            +350 -runwy_L/2)
// TowPlane parking position =   (-40,            +200 -runwy_L/2)
// Winsock position (V1) =       (+30 +runwy_W/2, +320 -runwy_L/2)
// V2
// note - drawing on canvas has Y reversed !
// reference runwy_W min is 25m
// X is reversed if take-off is on opposite side
// Glider take-off position =    ( +2,            +200 -runwy_L/2) // 14..17 - seems to move sometimes
// TowPlane touchdown position = (-12 -runwy_W/2,    0 -runwy_L/2)
// TowPlane corner position =    (-50 -runwy_W/2, +350 -runwy_L/2)
// TowPlane parking position =   (-25 -runwy_W/2, +200 -runwy_L/2)

//---------------------------------------------------------------------------
procedure Init_Positions;
var
  X_Offset : single;
begin
  if (apVersion = 'V1') then begin
    GL_TO.X :=  17;  GL_TO.Y := 200 - AirportLength/2;
    TP_TD.X :=  -5;  TP_TD.Y :=  50 - AirportLength/2;
    TP_CR.X := -75;  TP_CR.Y := 350 - AirportLength/2;
    TP_PK.X := -40;  TP_PK.Y := 200 - AirportLength/2;
    WS.X    :=  30 + AirportWidth/2;  WS.Y  := 320 - AirportLength/2;
  end else begin // V2
    if (AirportWidth < 25) then begin
      X_Offset := 12.5;
    end else begin
      X_Offset := AirportWidth/2;
    end;
    GL_TO.X :=   2;             GL_TO.Y := 200 - AirportLength/2;
    TP_TD.X := -12 - X_Offset;  TP_TD.Y :=   0 - AirportLength/2;
    TP_CR.X := -50 - X_Offset;  TP_CR.Y := 350 - AirportLength/2;
    TP_PK.X := -25 - X_Offset;  TP_PK.Y := 200 - AirportLength/2;
  end;
end;

//---------------------------------------------------------------------------
procedure DrawRunway(TileIndex : integer);
var
  i : integer;
  Temp_CoordXY : TCoordXY;
  Airport_CoordXY : TCoordXY;
  ScaleX , ScaleY : double;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure  DrawPaths(Tow_Primary, Tow_Left : boolean; useColor : TColor);
var
  i : integer;
  Tow_Side, Tow_End : double;
  End_Rotation : double;

begin
  Tow_End := -1; // condor Y reversed for Airports
  with Form_AirportPlacer.Image_Tile do begin
    if (Tow_Primary) then begin
//      Tow_End := -1;
      End_Rotation := 0;
      Canvas.Pen.Color := useColor;
    end else begin
//      Tow_End := 1;
      End_Rotation := -180;
      Canvas.Pen.Color := useColor;
    end;
  end;
  if (Tow_Left) then begin
    Tow_Side := 1;
  end else begin
    Tow_Side := -1;
  end;

  setlength(GliderTrack,2);
  Temp_CoordXY.X := GL_TO.X *Tow_Side; Temp_CoordXY.Y := (GL_TO.Y) *Tow_end;
  GliderTrack[0] := Temp_CoordXY;
  Temp_CoordXY.X := GL_TO.X *Tow_Side; Temp_CoordXY.Y := (GL_TO.Y+250) *Tow_end; // show 250m take-off
  GliderTrack[1] := Temp_CoordXY;

  Rotate_Array(GliderTrack, AirportDirection-End_Rotation);

  Offset_Array(GliderTrack, Airport_CoordXY);

  with Form_AirportPlacer.Image_Tile do begin
//    Canvas.Pen.Mode := pmCopy; // needed for pixels[] !
//    Canvas.Pen.Style := psSolid;
//    Canvas.Pen.Width := 1;
//    Canvas.Pen.Color := clBlue;
    Canvas.MoveTo(round(GliderTrack[0].X * ScaleX), Round(GliderTrack[0].Y * ScaleY));
    Canvas.LineTo(round(GliderTrack[1].X * ScaleX), Round(GliderTrack[1].Y * ScaleY));
  end;

  setlength(TowPlaneTrack,5);
  Temp_CoordXY.X := (TP_TD.X) *Tow_Side; Temp_CoordXY.Y := (TP_TD.Y) *Tow_end;
  TowPlaneTrack[0] := Temp_CoordXY;
  Temp_CoordXY.X := (TP_TD.X) *Tow_Side; Temp_CoordXY.Y := (TP_CR.Y) *Tow_end;
  TowPlaneTrack[1] := Temp_CoordXY;
  Temp_CoordXY.X := (TP_CR.X) *Tow_Side; Temp_CoordXY.Y := (TP_CR.Y) *Tow_end;
  TowPlaneTrack[2] := Temp_CoordXY;
  Temp_CoordXY.X := (TP_CR.X) *Tow_Side; Temp_CoordXY.Y := (TP_PK.Y) *Tow_end;
  TowPlaneTrack[3] := Temp_CoordXY;
  Temp_CoordXY.X := (TP_PK.X) *Tow_Side; Temp_CoordXY.Y := (TP_PK.Y) *Tow_end;
  TowPlaneTrack[4] := Temp_CoordXY;

  Rotate_Array(TowPlaneTrack, AirportDirection-End_Rotation);

  Offset_Array(TowPlaneTrack,Airport_CoordXY);

  with Form_AirportPlacer.Image_Tile do begin
//    Canvas.Pen.Mode := pmCopy; // needed for pixels[] !
//    Canvas.Pen.Style := psSolid;
//    Canvas.Pen.Width := 1;
//    Canvas.Pen.Color := clBlack;
    Canvas.MoveTo(round(TowPlaneTrack[0].X * ScaleX), Round(TowPlaneTrack[0].Y * ScaleY));
    for i := 1 to 4 do begin
      Canvas.LineTo(round(TowPlaneTrack[i].X * ScaleX), Round(TowPlaneTrack[i].Y * ScaleY));
    end;
  end;

  if (apVersion = 'V1') then begin
    setlength(WindSock,4);
    Temp_CoordXY.X := (-10 + (30 +AirportWidth/2)) *Tow_Side; Temp_CoordXY.Y := -10 + (+320 -AirportLength/2) *Tow_end;
    WindSock[0] := Temp_CoordXY;
    Temp_CoordXY.X := (-10 + (30 +AirportWidth/2)) *Tow_Side; Temp_CoordXY.Y :=  10 + (+320 -AirportLength/2) *Tow_end;
    WindSock[1] := Temp_CoordXY;
    Temp_CoordXY.X := ( 10 + (30 +AirportWidth/2)) *Tow_Side; Temp_CoordXY.Y :=  10 + (+320 -AirportLength/2) *Tow_end;
    WindSock[2] := Temp_CoordXY;
    Temp_CoordXY.X := ( 10 + (30 +AirportWidth/2)) *Tow_Side; Temp_CoordXY.Y := -10 + (+320 -AirportLength/2) *Tow_end;
    WindSock[3] := Temp_CoordXY;

    Rotate_Array(WindSock, AirportDirection-End_Rotation);

    Offset_Array(WindSock,Airport_CoordXY);

    with Form_AirportPlacer.Image_Tile do begin
//      Canvas.Pen.Mode := pmCopy; // needed for pixels[] !
//      Canvas.Pen.Style := psSolid;
//      Canvas.Pen.Width := 1;
//      Canvas.Pen.Color := clRed;
      Canvas.MoveTo(round(WindSock[3].X * ScaleX), Round(WindSock[3].Y * ScaleY));
      for i := 0 to 3 do begin
        Canvas.LineTo(round(WindSock[i].X * ScaleX), Round(WindSock[i].Y * ScaleY));
      end;
    end;
  end;

end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
begin
  // Basic positions
  Init_Positions;

  // for drawing need reference to top left instead of bottom right
  Airport_CoordXY.X := apRange - xCoord;
  Airport_CoordXY.Y := apRange - yCoord;

  with Form_AirportPlacer do begin
    ScaleX := Image_Tile.Width/apRange * apZoomScale;
    ScaleY := Image_Tile.Height/apRange * apZoomScale;
  end;

  setlength(AirportCorners,4);
  Temp_CoordXY.X := -AirportWidth/2; Temp_CoordXY.Y := -AirportLength/2;
  AirportCorners[0] := Temp_CoordXY;
  Temp_CoordXY.X := -AirportWidth/2; Temp_CoordXY.Y := AirportLength/2;
  AirportCorners[1] := Temp_CoordXY;
  Temp_CoordXY.X := AirportWidth/2; Temp_CoordXY.Y := AirportLength/2;
  AirportCorners[2] := Temp_CoordXY;
  Temp_CoordXY.X := AirportWidth/2; Temp_CoordXY.Y := -AirportLength/2;
  AirportCorners[3] := Temp_CoordXY;

  Rotate_Array(AirportCorners, AirportDirection);

  Offset_Array(AirportCorners,Airport_CoordXY);

  // prepare pen
  with Form_AirportPlacer.Image_Tile do begin
//    Canvas.Pen.Mode := pmCopy; // needed for pixels[] !
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := clRed;
  end;

  with Form_AirportPlacer.Image_Tile do begin
//    Canvas.Pen.Mode := pmCopy; // needed for pixels[] !
//    Canvas.Pen.Style := psSolid;
//    Canvas.Pen.Width := 1;
//    Canvas.Pen.Color := clRed;
    Canvas.MoveTo(round(AirportCorners[3].X * ScaleX), Round(AirportCorners[3].Y * ScaleY));
    for i := 0 to 3 do begin
      Canvas.LineTo(round(AirportCorners[i].X * ScaleX), Round(AirportCorners[i].Y * ScaleY));
    end;
  end;

  setlength(CentreMark,4);
  Temp_CoordXY.X := -10; Temp_CoordXY.Y := 0;
  CentreMark[0] := Temp_CoordXY;
  Temp_CoordXY.X :=  10; Temp_CoordXY.Y := 0;
  CentreMark[1] := Temp_CoordXY;
  Temp_CoordXY.X := 0; Temp_CoordXY.Y :=  10;
  CentreMark[2] := Temp_CoordXY;
  Temp_CoordXY.X := 0; Temp_CoordXY.Y := -10;
  CentreMark[3] := Temp_CoordXY;

  Rotate_Array(CentreMark, AirportDirection);

  Offset_Array(CentreMark,Airport_CoordXY);

  with Form_AirportPlacer.Image_Tile do begin
//    Canvas.Pen.Mode := pmCopy; // needed for pixels[] !
//    Canvas.Pen.Style := psSolid;
//    Canvas.Pen.Width := 1;
//    Canvas.Pen.Color := clRed;
    Canvas.MoveTo(round(CentreMark[0].X * ScaleX), Round(CentreMark[0].Y * ScaleY));
    Canvas.LineTo(round(CentreMark[1].X * ScaleX), Round(CentreMark[1].Y * ScaleY));
    Canvas.MoveTo(round(CentreMark[2].X * ScaleX), Round(CentreMark[2].Y * ScaleY));
    Canvas.LineTo(round(CentreMark[3].X * ScaleX), Round(CentreMark[3].Y * ScaleY));
  end;

  // draw primary direction
  DrawPaths(NOT Airport_Primary_Reversed,
//            (Airport_Tow_Primary_Left) XOR (Airport_Primary_Reversed),
            Airport_Tow_Primary_Left,
            clBlue);

  // draw secondary direction
  DrawPaths(Airport_Primary_Reversed,
//            (NOT Airport_Tow_Secondary_Left) XOR (Airport_Primary_Reversed),
            Airport_Tow_Secondary_Left,
            clYellow);
end;

//---------------------------------------------------------------------------
procedure DrawElevation(TileIndex : integer);
var
//  ScaleX , ScaleY : double;  // same for drawrunway -> move out ?
  Steps : integer;
  Increment : double;
  i, j : integer;
  x, y : integer;

//  xLeft, xRight : integer;
//  xTop, xBottom : integer;

begin
  with Form_AirportPlacer do begin
    Steps := trunc(apRange / 30); // 23040m or 11520m / 30m for V2
    Increment := Image_Tile.Width / Steps;

//    ScaleX := Image_Tile.Width/apRange * apZoomScale;
//    ScaleY := Image_Tile.Height/apRange * apZoomScale;

    //draw dots only in visible range
    // first find visible range
//    xLeft   :=
//    xRight  :=
//    xTop    :=
//    xBottom :=

    // for now, draw all
    with Form_AirportPlacer.Image_Tile do begin
      Canvas.Pen.Mode := pmCopy; // needed for pixels[] !
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Width := 1;
      Canvas.Pen.Color := clRed;
      Canvas.Brush.Color := clRed; // rectangle fill color
      for i := 0 to Steps-1 do begin
        y := trunc(Increment/2 + i*Increment);
        for j := 0 to Steps-1 do begin
          x := trunc(Increment/2 + j*Increment);
          if (Increment < 3.0) then begin
            Canvas.Pixels[x,y] := Canvas.Pen.Color; // 1 pixel
          end else begin
//            Canvas.Rectangle(x-1,y-1,x+2,y+2); // 3 pixels
            Canvas.FrameRect(rect(x-1,y-1,x+2,y+2)); // 3 pixels
          end;
        end;
      end;

    end;

  end;
end;

//---------------------------------------------------------------------------
procedure Draw_G_Objects;
var
  Index : integer;
//  Temp_CoordXY : TCoordXY;
  Airport_CoordXY : TCoordXY;
  ScaleX , ScaleY : double;
//  useColor : TColor;
  nName : string;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure DrawObject(useColor : TColor);
var
  i, j : integer;
begin
  with Form_AirportPlacer.Image_Tile do begin
//    Canvas.Pen.Mode := pmCopy; // needed for pixels[] !
//    Canvas.Pen.Style := psSolid;
//    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := useColor;
    for j := 0 to length(G_Object)-1 do begin
      // -180 because of Condor airport definition
      Rotate_Array(G_Object[j], AirportDirection-180);

      Offset_Array(G_Object[j], Airport_CoordXY);

      // start with last point to draw a closed surface
      Canvas.MoveTo(round(G_Object[j][Length(G_Object[j])-1].X * ScaleX), Round(G_Object[j][Length(G_Object[j])-1].Y * ScaleY));
      for i := 0 to Length(G_Object[j])-1 do begin
        Canvas.LineTo(round(G_Object[j][i].X * ScaleX), Round(G_Object[j][i].Y * ScaleY));
      end;
    end;
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
begin
  // for drawing need reference to top left instead of bottom right
  Airport_CoordXY.X := apRange - xCoord;
  Airport_CoordXY.Y := apRange - yCoord;

  with Form_AirportPlacer do begin
    ScaleX := Image_Tile.Width/apRange * apZoomScale;
    ScaleY := Image_Tile.Height/apRange * apZoomScale;
  end;

  // prepare pen
  with Form_AirportPlacer.Image_Tile do begin
//    Canvas.Pen.Mode := pmCopy; // needed for pixels[] !
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Width := 1;
//    Canvas.Pen.Color := clRed;
  end;

  Index := 0;
  While (Index <> -1) do begin
    // scan TreeView_G looking for mesh name = asphalt or grass
    Index := FindNodebyType(Form_AirportPlacer.TreeView_G, Index, oMesh, nName);
    if (Index <> -1) then begin
      if (upperCase(nName) = 'ASPHALT') then begin
        sExtract(Form_AirportPlacer.TreeView_G, Index, G_Object);
        DrawObject(clBlack);
      end else begin
        if (uppercase(nName) = 'GRASS') then begin
          sExtract(Form_AirportPlacer.TreeView_G, Index, G_Object);
          DrawObject(clGreen);
       	end;
      end;
      INC(Index);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure Draw_O_Objects;
var
  Index : integer;
//  Temp_CoordXY : TCoordXY;
  Airport_CoordXY : TCoordXY;
  ScaleX , ScaleY : double;
//  useColor : TColor;
  nName : string;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure DrawObject(useColor : TColor);
var
  i{, j} : integer;
begin
  with Form_AirportPlacer.Image_Tile do begin
//    Canvas.Pen.Mode := pmCopy; // needed for pixels[] !
//    Canvas.Pen.Style := psSolid;
//    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := useColor;
    // -180 because of Condor airport definition
    Rotate_Array(O_Object_Outline, AirportDirection-180);

    Offset_Array(O_Object_Outline, Airport_CoordXY);

    // start with last point to draw a closed surface
    Canvas.MoveTo(round(O_Object_Outline[Length(O_Object_Outline)-1].X * ScaleX), Round(O_Object_Outline[Length(O_Object_Outline)-1].Y * ScaleY));
    for i := 0 to Length(O_Object_Outline)-1 do begin
      Canvas.LineTo(round(O_Object_Outline[i].X * ScaleX), Round(O_Object_Outline[i].Y * ScaleY));
    end;
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
begin
  // for drawing need reference to top left instead of bottom right
  Airport_CoordXY.X := apRange - xCoord;
  Airport_CoordXY.Y := apRange - yCoord;

  with Form_AirportPlacer do begin
    ScaleX := Image_Tile.Width/apRange * apZoomScale;
    ScaleY := Image_Tile.Height/apRange * apZoomScale;
  end;

  // prepare pen
  with Form_AirportPlacer.Image_Tile do begin
//    Canvas.Pen.Mode := pmCopy; // needed for pixels[] !
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Width := 1;
//    Canvas.Pen.Color := clRed;
  end;

  Index := 0;
  While (Index <> -1) do begin
    // scan TreeView_G looking for mesh name = asphalt or grass
    Index := FindNodebyType(Form_AirportPlacer.TreeView_O, Index, oMesh, nName);
    if (Index <> -1) then begin
      vExtract(Form_AirportPlacer.TreeView_O, Index, O_Object_Outline);
      // only show outline
//      O_Object_Outline := GrahamScan(O_Object_Outline); // bug - no good
//      FindConvexHull(O_Object_Outline); // OK, but stack overflow on QuickSort sometimes
      FindConvexHull(O_Object_Outline); // OK, with ShellSort ?
      DrawObject(clBlack);
      INC(Index);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure DrawObjects(TileIndex : integer);
begin
  Screen.Cursor := crHourGlass;  // Let user know we're busy...
  try
    with Form_AirportPlacer do begin
      if (RadioButton_APT.Checked) then begin
        DrawRunway(TileIndex);
      end else begin
        if (RadioButton_Elev.Checked) then begin
          DrawElevation(TileIndex);
        end else begin
        end;
      end;
      if (CheckBox_G_File.Checked) then begin
        Draw_G_Objects;
      end;
      if (CheckBox_O_File.Checked) then begin
        Draw_O_Objects;
      end;
    end;
  finally
    Screen.Cursor := crDefault;  // no longer busy
 end;
end;

//---------------------------------------------------------------------------
function Encode_apDirection(Direction : string) : integer;
var
  decimalDirection : single;
begin
  decimalDirection := strtoFloat(Direction);
  if (apVersion = 'V1') then begin
    result := round(decimalDirection);
  end else begin // V2
    if (frac(decimalDirection) > 0.009) then begin
      result := round(decimalDirection*100+100000);
    end else begin // V1 compatible
      result := round(decimalDirection);
    end;
  end;
end;

//---------------------------------------------------------------------------
function Decode_apDirection(codedDirection : integer) : string;
begin
  if (apVersion = 'V1') then begin
    result := format('%d',[codedDirection]);
  end else begin // V2
    if (codedDirection >= 100000) then begin
      result := format('%1.2f',[(codedDirection-100000)/100.0]);
    end else begin // V1 compatible
      result := format('%1.2f',[codedDirection*1.0]);
    end;
  end;
end;

//---------------------------------------------------------------------------
Procedure ReCentre;
begin
  with Form_AirportPlacer do begin
    ScrollBox_Image.HorzScrollBar.Position := trunc(cX *
      (ScrollBox_Image.HorzScrollBar.Range)-(ScrollBox_Image.ClientWidth div 2));
    ScrollBox_Image.VertScrollBar.Position := trunc(cY *
      (ScrollBox_Image.VertScrollBar.Range)-ScrollBox_Image.ClientHeight div 2);
  end;
end;

//---------------------------------------------------------------------------
Procedure CentreAirport;
begin
  cX := apX;
  cY := apY;
  ReCentre;
end;

// find group of 4 DDS tiles that surround the airport coordinates
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
//  Form_AirportPlacer.Label_Tile.Caption := format('(q)%2.2d%2.2d',[Col div 2, Row div 2]);
  Form_AirportPlacer.Label_Tile.Caption := format('(q)%s',[MakeTIleName(Col div 2, Row div 2, TileNameMode)]);
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

  // keep track of BR reference
  apBR_X := Col*QT_Range; // UTM
  apBR_Y := Row*QT_Range;
  // airport tile relative coords (relative to bottom right)
  {Unit_Graphics.}xCoord := AirportEasting -  apBR_X;
  {Unit_Graphics.}yCoord := AirportNorthing - apBR_Y;
  // Relative fractional position (relative to top left)
  apRange := QT_Range*2; // metres, 2x2 quarter tiles
  apX := 1 - ({Unit_Graphics.}xCoord/apRange);
  apY := 1 - ({Unit_Graphics.}yCoord/apRange);
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.ShowItem(Sender: TObject);
const
  T_Range = Resolution * tColumns; // 23040 km
var
  i, j : integer;
  {TileIndex,} TileIndex2 : integer;
  TileRow, TileColumn : integer;
  apAirportDecimal : single;

  DDS_Col, DDS_Row : integer;
  FilePicture: TPicture; // to load DDS tiles
  FileName : string;
  Temp : longint;
  DDS_Size : longint;
  OutOfResources : boolean;

begin
  if (ItemIndex <> -1) then begin
    if (Airport_Count > 1) then begin
      Button_Delete.Enabled := true;
    end else begin
      Button_Delete.Enabled := false;
      Button_HiResRunway.Enabled := false;
    end;
    OutOfResources := false;
    BitmapAvail := false; // assume for now
    with Airport_List[ItemIndex] do begin
      Edit_AirportName.Text := apName;
      Edit_Latitude.Text := format('%1.7f',[apLatitude]);
      Edit_Longitude.Text := format('%1.7f',[apLongitude]);
      Edit_Altitude.Text := format('%1.3f',[apAltitude]);
      Edit_Direction.Text := Decode_apDirection(apDirection);
      apAirportDecimal := strtofloat(Edit_Direction.Text);
      Edit_Length.Text := format('%d',[apLength]);
      Edit_Width.Text := format('%d',[apWidth]);
      Edit_Frequency.Text := format('%1.3f',[apFrequency]);
      CheckBox_Primary_Reverse.checked := (apOptions AND $00000001 = $00000001);
      CheckBox_Tow_Primary_Left.checked := (apOptions AND $00000100 = $00000100);
      CheckBox_Tow_Secondary_Left.checked := (apOptions AND $00010000 = $00010000);
      if ((apAsphaltFlag AND 1) = 1) then begin
        RadioButton_Paved.Checked := true;
        Button_HiResRunway.Enabled := true;
      end else begin
        RadioButton_Grass.Checked := true;
        Button_HiResRunway.Enabled := false;
      end;
      //default to 0 if no bitmap
      LatDegPerM := 0.0; LongDegPerM := 0.0;
      //use lat,long to find corresponding tile
      if (TileOpen) then begin
        AirportDirection := apAirportDecimal;
        AirportLength := apLength;
        AirportWidth := apWidth;
        Airport_Primary_Reversed := CheckBox_Primary_Reverse.checked;
        Airport_Tow_Primary_Left := CheckBox_Tow_Primary_Left.checked;
        Airport_Tow_Secondary_Left := CheckBox_Tow_Secondary_Left.checked;
        // convert airport lat long to UTM absolute
        LatLongToUTM(apLatitude,apLongitude,UTM_Zone,UTM_ZoneNS);
        // make relative to scenery bottom right
//        AirportEasting := (UTM_Right{+Legacy_Offset}) - uEasting;
        AirportEasting := (UTM_Right+Legacy_Offset) - uEasting;          // unfortunately reference offset - how to fix ???
//        AirportNorthing := uNorthing - (UTM_Bottom{-Legacy_Offset});
        AirportNorthing := uNorthing - (UTM_Bottom-Legacy_Offset);       // unfortunately reference offset - how to fix ???
        //Apply calibration scales
        AirportEasting :=  AirportEasting  * cResolution/-cDeltaX;
        AirportNorthing := AirportNorthing * cResolution/cDeltaY;
        Label_UTM.Caption := format('%1.2f, %1.2f',[AirportEasting,AirportNorthing]);
        TileColumn := trunc(AirportEasting/T_Range);
        TileRow := trunc(AirportNorthing/T_Range);
//        if ((TileColumn < 0) or (TileColumn >= TileColumnCount) or (TileRow < 0) or (TileRow >= TileRowCount)) then begin
        if ((TileColumn < 0) or (TileColumn > TileColumnCount) or (TileRow < 0) or (TileRow > TileRowCount)) then begin
          Exit; // fault, airport coordinates beyound scenery boundaries
        end;
        AirportTileIndex := TileRow*(TileColumnCount+1)+TileColumn;
        TileIndex2 := (TileRow+1)*(TileColumnCount+1)+TileColumn+1; // other corner
        // relative ratios degrees per metre
        LatDegPerM :=  (TileList[TileIndex2].TileLatBottom - TileList[AirportTileIndex].TileLatBottom) / T_Range;
        LongDegPerM := (TileList[TileIndex2].TileLongRight - TileList[AirportTileIndex].TileLongRight) / -T_Range;
        // bug- above only works if there is a full tile, not for partial tile and not using Ceil!
        LatDegPerM := (0.001/(earthRadius*2*Pi))*360;
        LongDegPerM := LatDegPerM/cos(apLatitude*Pi/180);

        Screen.Cursor := crHourGlass;  // Let user know we're busy...
        // if DDS textures available, use 4 closest, otherwise use terragen tile
        if (RadioButton_DDS.Checked = true) then begin
          Find_DDS_Tiles(AirportEasting, AirportNorthing, DDS_Col, DDS_Row);
          // determine highest tile resolution
          DDS_Size := 0;
          for i := 0 to 2-1 do begin
            for j := 0 to 2-1 do begin
//              FileName := format('%s\Textures\t%2.2d%2.2d.dds',[Airport_FolderName,DDS_Col+(1-i),DDS_Row+(1-j)]);
              FileName := format('%s\Textures\t%s.dds',[Airport_FolderName,MakeTileName(DDS_Col+(1-i),DDS_Row+(1-j), TileNameMode)]);
              Temp := DXT_ImageWidth(FileName);
              if (Temp > DDS_Size) then begin
{ While (DDS_Size > 8192) do begin
  ReducedFileName := ???
  // if reduced exists
  if (NOT FileExist(ReducedFilaName)) then begin
    // make copy
    CopyFile(FileName,ReducedFileName,false)
  end
  // then reduce
  TextureReduce(ReducedFileName);
  Temp := DXT_ImageWidth(ReducedFileName);
end; }
                DDS_Size := temp;
              end;
            end;
          end;

          // show a blank background of default size if no files
          if (DDS_Size = 0) then begin
            DDS_Size := 1024; // choose a default size
          end;

{ tried but still no go for large DDS
          // show a blank background if files are too large for 32 bit system
          // i.e. avoid crashing Condor_Tiles
          if (DDS_Size > 8192) then begin
            // at size 8192, DDS load with load next lower level mip
            // so new threshold is 16384
            if (DDS_Size > 16384) then begin
              MessageShow('DDS file too large');
              OutOfResources := true;
              DDS_Size := 1024; // choose a default size
            end else begin
              DDS_Size := 8192;
            end;
          end;
}
          // show a blank background if files are too large for 32 bit system
          // i.e. avoid crashing Condor_Tiles
          if (DDS_Size > 8192) then begin
            MessageShow('DDS file too large');
            OutOfResources := true;
            DDS_Size := 1024; // choose a default size
          end;

          with Image_Tile.Picture.Bitmap do begin
            Image_Tile.Align := alClient;
            Image_Tile.AutoSize := true;
            Width := DDS_Size * 2;
            Height := DDS_Size * 2;
            Image_Tile.Stretch := false; // no stretch - 1:1 resolution to start
            Image_Tile_Clear;
          end;

          if (OutOfResources) then begin
            Screen.Cursor := crDefault;  // no longer busy
            beep; exit;
          end else begin
            // load 4 dds tiles and draw onto Image_Tile
            for i := 0 to 2-1 do begin
              for j := 0 to 2-1 do begin
//                FileName := format('%s\Textures\t%2.2d%2.2d.dds',[Airport_FolderName,DDS_Col+(1-i),DDS_Row+(1-j)]);
                FileName := format('%s\Textures\t%s.dds',[Airport_FolderName,MakeTileName(DDS_Col+(1-i),DDS_Row+(1-j), TileNameMode)]);
     // try ReducedFileName first
                if (NOT FileExists(FileName)) then begin
//                BitmapAvail := false; // no, change - allow even if no files
                  // blank image
                  Screen.Cursor := crDefault;  // no longer busy
            //      Image_Tile_Clear;
            //      exit;
                  continue;
                end;
                FilePicture := TPicture.Create;
                try
                  FilePicture.LoadFromFile(FileName);
                  if (apVersion = 'V1') then begin
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

          BitmapAvail := true; // although it could be blank
          ZoomRestore(Sender); // after load/reload of tile
          CentreAirport;
//          DrawRunway(AirportTileIndex);
//          DrawRunway(0);
          DrawObjects(0);
        end else begin // try terragen tile
          //keep track of BR reference
          apBR_X := TileList[AirportTileIndex].TileUTMRight;
          apBR_Y := TileList[AirportTileIndex].TileUTMBottom;
          // airport tile relative coords (relative to bottom right)
          {Unit_Graphics.}xCoord := AirportEasting -  apBR_X;
          {Unit_Graphics.}yCoord := AirportNorthing - apBR_Y;
          // Relative fractional position (relative to top left)
          apX := 1 - ({Unit_Graphics.}xCoord/T_Range);
          apY := 1 - ({Unit_Graphics.}yCoord/T_Range);
          apRange := T_Range; // metres
          Label_Tile.Caption := TileList[AirportTileIndex].TileName;
          // check if file are too large for 32 bit system
          // i.e. avoid crashing Condor_Tiles
          // could shrink the picture as it loads instead - TBD
          if (LoadTileBitmap(TileList[AirportTileIndex].TileName) ) then begin
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
          CentreAirport;
//          DrawRunway(0);
          DrawObjects(0);
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
procedure TForm_AirportPlacer.ShowCoord(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Horiz, Vert : double;
  cCOS, cSIN : double;
begin
//  if (FPanning) then begin
  if (GUI_State = ScrollScreen) then begin
    with ScrollBox_Image do
    begin
      HorzScrollBar.Position := HorzScrollBar.Position + (FMousePos.X - X);
      MyScrollHorz(Sender);
      VertScrollBar.Position := VertScrollBar.Position + (FMousePos.Y - Y);
      MyScrollVert(Sender);
    end;
  end;

  if (BitmapAvail) then begin
    cSIN := sin(-AirportDirection/180*PI);
    cCOS := cos(-AirportDirection/180*PI);
    Horiz := -xCoord + apRange{Resolution*tColumns}*
      (Image_Tile.Width-1-X)/Image_Tile.Width;
    Vert := -yCoord + apRange{Resolution*tRows}*
      (Image_Tile.Height-1-Y)/Image_Tile.Height;
    Label_Coords.Caption := format('%1.2f,%1.2f',[
      (Horiz * cCOS + Vert * -cSIN),
      (Horiz * cSIN + Vert *  cCOS)
      ]);
{
    // relative coords
    Label_H_Pos.Caption := format('%1.0f,%1.0f',[
      Horiz, Vert
      ]);
}
    // absolute coords
    Label_H_Pos.Caption := format('%1.2f,%1.2f',[
      AirportEasting+Horiz, AirportNorthing+Vert
      ]);
{
    // for testing
    Label_H_Pos.Caption := format('%d,%d,%d,%d',[X,Y,
      ScrollBox_Image.HorzScrollBar.Position,
      ScrollBox_Image.VertScrollBar.Position]);
}
{
    // for testing
    Horiz := trunc(apRange/30*
      (Image_Tile.Width-1-X)/Image_Tile.Width);
    Vert := trunc(apRange/30*
      (Image_Tile.Height-1-Y)/Image_Tile.Height);
    Label_H_Pos.Caption := format('%1.0f,%1.0f',[
      Horiz, Vert
      ]);
}
{
    // for testing
    Horiz := ScrollBox_Image.HorzScrollBar.Position;
    Vert  := ScrollBox_Image.VertScrollBar.Position;
    Label_H_Pos.Caption := format('%1.0f,%1.0f',[
      Horiz, Vert
      ]);
}
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Image_TileMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMousePos.X := X;
  FMousePos.Y := Y;
  case GUI_State of
    IdleScreen: begin
      if (ssCtrl in Shift) then begin //scroll bitmap
//        FPanning := True;
        GUI_State := ScrollScreen;
      end;
    end;
    else begin
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Image_TileMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  dx, dy : Integer;
  Angle : single;
  Value : single;

begin
  dx := X - FMousePos.X;
  dy := Y - FMousePos.Y;
  case GUI_State of
    CentreSelectScreen: begin
      xCoord := (X + FMousePos.X) /2;
      yCoord := (Y + FMousePos.Y) /2;
      // convert to absolute UTM
//      uEasting := UTM_Right - (apRange-xCoord + apBR_X);
      uEasting := UTM_Right - (apRange*(1-xCoord/Image_Tile.Width) + apBR_X);
      uNorthing := UTM_Bottom + (apRange*(1-yCoord/Image_Tile.Height) + apBR_Y);
      // convert to lat/long
      UTMtoLatLong(uNorthing, uEasting, UTM_Zone, UTM_ZoneNS);
      // save result
      Edit_Longitude.Text := floatToStr(uLongitude);
      Edit_Longitude.Modified := true;
      Edit_LongitudeExit(Sender);
      Edit_Latitude.Text := floatToStr(uLatitude);
      Edit_Latitude.Modified := true;
      Edit_LatitudeExit(Sender);
    end;
    DirectionSelectScreen: begin
      if (dy <> 0.0) then begin
        Angle := ArcTan2(dx, -dy) * 180 / PI;
        if (Angle < 0.0) then begin
          Angle := Angle + 360;
        end;
      end else begin
        if dx < 0 then begin
          Angle := 270.0;
        end else begin
          Angle := 90.0;
        end;
      end;
      Edit_Direction.Text := floatToStr(Angle);
      Edit_Direction.Modified := true;
      Edit_DirectionExit(Sender);
    end;
    LengthSelectScreen: begin
      // TBD - improve - in-line distance
      Value := sqrt(dx*dx + dy*dy) *
        apRange/Image_Tile.Width;
      Edit_Length.Text := intToStr(round(Value));
      Edit_Length.Modified := true;
      Edit_LengthExit(Sender);
    end;
    WidthSelectScreen: begin
      // TBD - improve - perpendicular distance
      Value := sqrt(dx*dx + dy*dy) *
        apRange/Image_Tile.Width;
      Edit_Width.Text := intToStr(round(Value));
      Edit_Width.Modified := true;
      Edit_WidthExit(Sender);
    end;
  end;
  Screen.Cursor := crDefault;
  Image_Tile.ShowHint := True;
  GUI_State := IdleScreen;
end;

//---------------------------------------------------------------------------
function TForm_AirportPlacer.LoadTileBitmap(TileName : string) : boolean;
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
procedure TForm_AirportPlacer.Search_Airport_Details;
var
  SearchRec: TSearchRec;
  path, mask : string;
begin
    // find objects in O file
    u_X_CX.oTreeView := TreeView_O;
    ClearTreeView(TreeView_O); // and append to not show headers

    // look for O.c3d or O.x or O.cx
    Path := Airport_FolderName+'\Airports\';
    Mask := Airport_List[ItemIndex].apName + 'O.*';
    if (FindFirst(Path+Mask, faAnyFile, SearchRec)) = 0 then begin
      if (uppercase(ExtractFileExt(SearchRec.Name)) = '.C3D') then begin
        ReadCondorC3Dfile(Path+SearchRec.Name, true);
      end else begin
        ReadCondorXfile(Path+SearchRec.Name, true);
      end;
    end else begin
      FindClose(SearchRec);
      // clear treeview
      ClearTreeView(TreeView_O);
    end;
    FindClose(SearchRec);

    // find objects in G file
    u_X_CX.oTreeView := TreeView_G;
    ClearTreeView(TreeView_G); // and append to not show headers

    // look for G.c3d or G.x or G.cx
    Path := Airport_FolderName+'\Airports\';
    Mask := Airport_List[ItemIndex].apName + 'G.*';
    if (FindFirst(Path+Mask, faAnyFile, SearchRec)) = 0 then begin
      if (uppercase(ExtractFileExt(SearchRec.Name)) = '.C3D') then begin
        ReadCondorC3Dfile(Path+SearchRec.Name, true);
      end else begin
        ReadCondorXfile(Path+SearchRec.Name, true);
      end;
    end else begin
      FindClose(SearchRec);
      // clear treeview
      ClearTreeView(TreeView_G);
    end;
    FindClose(SearchRec);

end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.ListBox_ObjectListMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ItemIndex := ListBox_ObjectList.ItemAtPos(point(X,Y), true);
  if (ItemIndex <> -1) then begin
    apZoomScale := 1.0;
    Search_Airport_Details; // get details from G and O files
    ShowItem(Sender);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Button_AddClick(Sender: TObject);
begin
  SetLength(Airport_List,Airport_Count+1);
  Airport_list[Airport_Count] := Airport_list[Airport_Count-1]; //default values
  Airport_List[Airport_Count].apName := 'New Airport';
  ListBox_ObjectList.Items.Append(Airport_List[Airport_Count].apName);
  INC(Airport_Count);
//  AirportsChanged := true;
  ListBox_ObjectList.ItemIndex := ListBox_ObjectList.Items.Count-1;
  ItemIndex := ListBox_ObjectList.ItemIndex;
  apZoomScale := 1.0;
  Search_Airport_Details;
//  ShowItem(Sender);
  Airport_Change_Show(True, True);
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Button_DeleteClick(Sender: TObject);
var
  i : integer;
begin
  if ((ItemIndex <> -1) AND (Airport_Count > 1) ) then begin
    if MessageDlg('Are you sure you want to delete '+Airport_List[ItemIndex].apName+' ?', mtConfirmation,
        [mbYes, mbNo], 0) = mrYes then begin
      // remove from list
      ListBox_ObjectList.Items.Delete(ItemIndex);
      // remove from airport list
      for i := ItemIndex to Airport_Count-1-1 do begin
        Airport_list[i] := Airport_list[i+1];
      end;
      if (ItemIndex = Airport_Count-1) then begin
        DEC(ItemIndex);
        ListBox_ObjectList.ItemIndex := ItemIndex;
      end;
      DEC(Airport_Count);
//      AirportsChanged := true;
      SetLength(Airport_List,Airport_Count);
      Search_Airport_Details;
//      ShowItem(Sender);
      Airport_Change_Show(True, True);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if (AirportsChanged = true) then begin
    if MessageDlg('Airports changed, Exit anyway ?', mtConfirmation,
        [mbYes, mbNo], 0) = mrNo then begin
      CanClose := False;
    end;
    CurrentLandscape := ''; // force a reload on re-entry
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Button_ExitClick(Sender: TObject);
begin
  Close;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Button_SaveClick(Sender: TObject);
begin
  if (AirportsChanged) then begin
    WriteAirportFile;
//    AirportsChanged := false;
    Airport_Change_Show(False, False);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Edit_AirportNameExit(Sender: TObject);
begin
  if (Edit_AirportName.Modified) then begin
    if (ItemIndex <> -1) then begin
      ListBox_ObjectList.Items[ItemIndex] := Edit_AirportName.Text;
      with Airport_List[ItemIndex] do begin
        apName := Edit_AirportName.Text;
      end;
      // search for airport details

//      AirportsChanged := true;
      Airport_Change_Show(True, False);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Edit_LatitudeExit(Sender: TObject);
begin
  if (Edit_Latitude.Modified) then begin
    if (ItemIndex <> -1) then begin
      with Airport_List[ItemIndex] do begin
        apLatitude := StrtoFloat(Edit_Latitude.Text);
        Edit_Latitude.Text := format('%1.7f',[apLatitude]);
      end;
//      AirportsChanged := true;
//      ShowItem(Sender);
      Airport_Change_Show(True, True);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Edit_LongitudeExit(Sender: TObject);
begin
  if (Edit_Longitude.Modified) then begin
    if (ItemIndex <> -1) then begin
      with Airport_List[ItemIndex] do begin
        apLongitude := StrtoFloat(Edit_Longitude.Text);
        Edit_Longitude.Text := format('%1.7f',[apLongitude]);
      end;
//      AirportsChanged := true;
//      ShowItem(Sender);
      Airport_Change_Show(True, True);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Edit_AltitudeExit(Sender: TObject);
begin
  if (Edit_Altitude.Modified) then begin
    if (ItemIndex <> -1) then begin
      with Airport_List[ItemIndex] do begin
        apAltitude := StrtoFloat(Edit_Altitude.Text);
        Edit_Altitude.Text := format('%1.3f',[apAltitude]);
      end;
//      AirportsChanged := true;
      Airport_Change_Show(True, False);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Edit_DirectionExit(Sender: TObject);
begin
  if (Edit_Direction.Modified) then begin
    if (ItemIndex <> -1) then begin
      with Airport_List[ItemIndex] do begin
        apDirection := Encode_apDirection(Edit_Direction.Text);
        Edit_Direction.Text := Decode_apDirection(apDirection);
      end;
//      AirportsChanged := true;
//      ShowItem(Sender);
      Airport_Change_Show(True, True);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Edit_LengthExit(Sender: TObject);
begin
  if (Edit_Length.Modified) then begin
    if (ItemIndex <> -1) then begin
      with Airport_List[ItemIndex] do begin
        apLength := StrtoInt(Edit_Length.Text);
        Edit_Length.Text := format('%d',[apLength]);
      end;
//      AirportsChanged := true;
//      ShowItem(Sender);
      Airport_Change_Show(True, True);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Edit_WidthExit(Sender: TObject);
begin
  if (Edit_Width.Modified) then begin
    if (ItemIndex <> -1) then begin
      with Airport_List[ItemIndex] do begin
        apWidth := StrtoInt(Edit_Width.Text);
        Edit_Width.Text := format('%d',[apWidth]);
      end;
//      AirportsChanged := true;
//      ShowItem(Sender);
      Airport_Change_Show(True, True);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Edit_FrequencyExit(Sender: TObject);
begin
  if (Edit_Frequency.Modified) then begin
    if (ItemIndex <> -1) then begin
      with Airport_List[ItemIndex] do begin
        apFrequency := StrtoFloat(Edit_Frequency.Text);
        Edit_Frequency.Text := format('%1.5f',[apFrequency]);
      end;
//      AirportsChanged := true;
      Airport_Change_Show(True, False);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.CheckBox_Primary_ReverseMouseUp(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if (ItemIndex <> -1) then begin
    with Airport_List[ItemIndex] do begin
      if (CheckBox_Primary_Reverse.checked) then begin
        apOptions := (apOptions AND $FFFFFF00) OR $00000001;
      end else begin
        apOptions := (apOptions AND $FFFFFF00) OR $00000000;
      end;
    end;
//    AirportsChanged := true;
//    ShowItem(Sender);
    Airport_Change_Show(True, True);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.CheckBox_Tow_Primary_LeftMouseUp(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if (ItemIndex <> -1) then begin
    with Airport_List[ItemIndex] do begin
      if (CheckBox_Tow_Primary_Left.checked) then begin
        apOptions := (apOptions AND $FFFF00FF) OR $00000100;
      end else begin
        apOptions := (apOptions AND $FFFF00FF) OR $00000000;
      end;
    end;
//    AirportsChanged := true;
//    ShowItem(Sender);
    Airport_Change_Show(True, True);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.CheckBox_Tow_Secondary_LeftMouseUp(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if (ItemIndex <> -1) then begin
    with Airport_List[ItemIndex] do begin
      if (CheckBox_Tow_Secondary_Left.checked) then begin
        apOptions := (apOptions AND $FF00FFFF) OR $00010000;
      end else begin
        apOptions := (apOptions AND $FF00FFFF) OR $00000000;
      end;
    end;
//    AirportsChanged := true;
//    ShowItem(Sender);
    Airport_Change_Show(True, True);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.RadioButton_GrassMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ItemIndex <> -1) then begin
    with Airport_List[ItemIndex] do begin
      if (RadioButton_Grass.checked) then begin
        apAsphaltFlag := 0;
      end else begin
        apAsphaltFlag := 1;
      end;
    end;
//    AirportsChanged := true;
    Airport_Change_Show(True, False);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.RadioButton_PavedMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (ItemIndex <> -1) then begin
    with Airport_List[ItemIndex] do begin
      if (RadioButton_Paved.checked) then begin
        apAsphaltFlag := 1;
      end else begin
        apAsphaltFlag := 0;
      end;
    end;
//    AirportsChanged := true;
    Airport_Change_Show(True, False);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.FormCreate(Sender: TObject);
begin
  // added scrollbar events
  ScrollBox_Image.OnScrollVert := MyScrollVert;
  ScrollBox_Image.OnScrollHorz := MyScrollHorz;

  CurrentLandscape := '';
//  DDS_Bitmap := TBitmap.Create;
//  with DDS_Bitmap do begin
//    Height := 512;
//    Width := 512;
  //    Transparent := True;
  //    TransParentColor := $00010101; // so that black can be used to erase
  //    TransparentMode := tmFixed;
//  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.FormDestroy(Sender: TObject);
begin
//  DDS_Bitmap.Free;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.UpDown_LongitudeMouseUp(Sender: TObject;
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
    if (X > UpDown_Longitude.Height div 2) then begin // Move right
    end else begin // Move left
      Delta := - Delta;
    end;
    with Airport_List[ItemIndex] do begin
      apLongitude := apLongitude + Delta * LongDegPerM;
    end;
//    AirportsChanged := true;
//    ShowItem(Sender);
    Airport_Change_Show(True, True);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.UpDown_LatitudeMouseUp(Sender: TObject;
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
    if (Y < UpDown_Latitude.Height div 2) then begin // Move Up
    end else begin // Move Down
      Delta := - Delta;
    end;
    with Airport_List[ItemIndex] do begin
      apLatitude := apLatitude + Delta * LatDegPerM;
    end;
//    AirportsChanged := true;
//    ShowItem(Sender);
    Airport_Change_Show(True, True);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.RadioButton_TerragenMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  apZoomScale := 1.0; // or properly adjust the zoom due to bitmap size change
  ShowItem(Sender);
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.RadioButton_DDSMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  apZoomScale := 1.0; // or properly adjust the zoom due to bitmap size change
  ShowItem(Sender);
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.RadioButton_APTMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  apZoomScale := 1.0; // or properly adjust the zoom due to bitmap size change
  ShowItem(Sender);
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.RadioButton_ElevMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  apZoomScale := 1.0; // or properly adjust the zoom due to bitmap size change
  ShowItem(Sender);
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.CheckBox_G_FileMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowItem(Sender);
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.CheckBox_O_FileMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowItem(Sender);
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Button_ZoomInClick(Sender: TObject);
begin
  if (BitmapAvail) then begin
    // allow re-size of image - picture stays the same
    Image_Tile.Align := alNone;
    Image_Tile.AutoSize := false;
    Image_Tile.Stretch := true;
    // scale image by 1.5
//    apZoomScale := apZoomScale / 1.5;
    apZoomScale := apZoomScale * Image_Tile.Width; // make more exact using before and after
//    Image_Tile.Width := Image_Tile.Width + Image_Tile.Width div 2;
//    Image_Tile.Height := Image_Tile.Height + Image_Tile.Height div 2;
    Image_Tile.Width := round(Image_Tile.Width * 1.5);
    Image_Tile.Height := round(Image_Tile.Height * 1.5);
    apZoomScale := apZoomScale / Image_Tile.Width;
    ScrollBox_Image.HorzScrollBar.Range := Image_Tile.Width;
    ScrollBox_Image.VertScrollBar.Range := Image_Tile.Height;
    // now refresh
//    CentreAirport;
    ReCentre;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Button_ZoomOutClick(Sender: TObject);
begin
  if (BitmapAvail) then begin
    // allow re-size of image - picture stays the same
    Image_Tile.Align := alNone;
    Image_Tile.AutoSize := false;
    Image_Tile.Stretch := true;
    // scale image by 3/4
//    apZoomScale := apZoomScale / 0.75;
    apZoomScale := apZoomScale * Image_Tile.Width; // make more exact using before and after
//    Image_Tile.Width := Image_Tile.Width - Image_Tile.Width div 4;
//    Image_Tile.Height := Image_Tile.Height - Image_Tile.Height div 4;
    Image_Tile.Width := round(Image_Tile.Width / 1.5);
    Image_Tile.Height := round(Image_Tile.Height / 1.5);
    apZoomScale :=  apZoomScale / Image_Tile.Width;
    ScrollBox_Image.HorzScrollBar.Range := Image_Tile.Width;
    ScrollBox_Image.VertScrollBar.Range := Image_Tile.Height;
    // now refresh
//    CentreAirport;
    ReCentre;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Button_ZoomResetClick(Sender: TObject);
begin
  if (BitmapAvail) then begin
    apZoomScale := 1.0;
    Image_Tile.Width := Image_Tile.Picture.Width;
    Image_Tile.Height := Image_Tile.Picture.Width;
    ScrollBox_Image.HorzScrollBar.Range := Image_Tile.Picture.Width;
    ScrollBox_Image.VertScrollBar.Range := Image_Tile.Picture.Height;
    // now refresh
    CentreAirport;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.ZoomRestore(Sender: TObject);
begin
  if (BitmapAvail) then begin
    // allow re-size of image - picture stays the same
    Image_Tile.Align := alNone;
    Image_Tile.AutoSize := false;
    Image_Tile.Stretch := true;
    // restore zoom after reload of tile
    Image_Tile.Width := round(Image_Tile.Picture.Width / apZoomScale);
    Image_Tile.Height := round(Image_Tile.Picture.Height / apZoomScale);
    ScrollBox_Image.HorzScrollBar.Range := Image_Tile.Width;
    ScrollBox_Image.VertScrollBar.Range := Image_Tile.Height;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Image_TileClick(Sender: TObject);
begin
  Clipboard.AsText := Label_Coords.Caption;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.ScrollBox_ImageResize(Sender: TObject);
begin
  Recentre;  // on current centre cX, cY
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.ListBox_ObjectListKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: begin
      ItemIndex := ListBox_ObjectList.ItemIndex;
      if (ItemIndex <> -1) then begin
        apZoomScale := 1.0;
        Search_Airport_Details;
        ShowItem(Sender);
      end;
    end;
    else begin
    end;
  end;
end;

// form Keypreview must be true
//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    ord('S'), ord('s'): begin
//      if (ssCtrl in Shift) then begin
      if (ssShift in Shift) then begin
        Button_SaveClick(Sender);
        key := 0; // so that other components won't see this keypress
      end;
    end;
    else begin
    end;
  end;
end;

//---------------------------------------------------------------------------
type
  tCoord = array[0..2-1] of single;

//---------------------------------------------------------------------------
Procedure Make_Px_Airport(FilePath, FileName, TextureFileName : string;
  gV, gT : array of tCoord);
var
  PXfile : text;
begin
  //open the file
  AssignFile(PXfile, FilePath +'\'+ FileName);
  Rewrite(PXfile);
  // write PX fields
  writeln(PXfile,'xof 0303txt 0032');
  writeln(PXfile,'Frame {');
  writeln(PXfile,'Mesh Asphalt {');
  writeln(PXfile,'4;');
  writeln(PXfile,format('%3.3f; %3.3f; 0.0;,',[gV[0][0],gV[0][1]]));
  writeln(PXfile,format('%3.3f; %3.3f; 0.0;,',[gV[1][0],gV[1][1]]));
  writeln(PXfile,format('%3.3f; %3.3f; 0.0;,',[gV[2][0],gV[2][1]]));
  writeln(PXfile,format('%3.3f; %3.3f; 0.0;;',[gV[3][0],gV[3][1]]));
  writeln(PXfile,'2;');
  writeln(PXfile,'3; 0, 2, 1;,');
  writeln(PXfile,'3; 2, 0, 3;;');
  writeln(PXfile,'MeshNormals {');
  writeln(PXfile,'4;');
  writeln(PXfile,' 0.0; 0.0; 1.0;,');
  writeln(PXfile,' 0.0; 0.0; 1.0;,');
  writeln(PXfile,' 0.0; 0.0; 1.0;,');
  writeln(PXfile,' 0.0; 0.0; 1.0;;');
  writeln(PXfile,'2;');
  writeln(PXfile,'3; 0, 2, 1;,');
  writeln(PXfile,'3; 2, 0, 3;;');
  writeln(PXfile,'}');
  writeln(PXfile,'MeshTextureCoords {');
  writeln(PXfile,'4;');
  writeln(PXfile,format('%1.6f; %1.6f;,',[gT[0][0],gT[0][1]]));
  writeln(PXfile,format('%1.6f; %1.6f;,',[gT[1][0],gT[1][1]]));
  writeln(PXfile,format('%1.6f; %1.6f;,',[gT[2][0],gT[2][1]]));
  writeln(PXfile,format('%1.6f; %1.6f;;',[gT[3][0],gT[3][1]]));
  writeln(PXfile,'}');
  writeln(PXfile,'MeshMaterialList {');
  writeln(PXfile,' 1;');
  writeln(PXfile,' 1;');
  writeln(PXfile,' 0;');
  writeln(PXfile,'Material  {');
  writeln(PXfile,' 1.0; 1.0; 1.0; 1.0;;');
  writeln(PXfile,' 1.0;');
  writeln(PXfile,' 0.0; 0.0; 0.0;;');
  writeln(PXfile,' 1.0; 1.0; 1.0;;');
  writeln(PXfile,'TextureFilename {');
  writeln(PXfile,'"'+TextureFileName+'";');
  writeln(PXfile,'}');
  writeln(PXfile,'}');
  writeln(PXfile,'}');
  writeln(PXfile,'}');
  writeln(PXfile,'}');
  // close the file
  Close(PXfile);
end;

//-------------------------------------------------------------------------------------
Procedure Make_Airport_All_BatchFile(epsg : integer; FilePath, FileName : string);
var
  HRR_file : TextFile;

begin
  // create path
  ForceDirectories(FilePath);

  FileName := 'MAKE_ALL';
  if (epsg = 4326) then begin
    FileName := format('%s_%d.bat',[FileName,epsg]);
  end else begin
    FileName := FileName+'.bat';
  end;

  //open the file
  AssignFile(HRR_file, FilePath +'\'+ FileName);
  Rewrite(HRR_file);

  writeln(HRR_file,'@echo off');
  // use || to execute next command if previous one failed
  writeln(HRR_file,'call Batch_Airport_Download.bat || exit /b 9');
  writeln(HRR_file,'call Batch_Airport_Combine.bat || exit /b 9');
  if (epsg = 4326) then begin
    writeln(HRR_file,'call GDAL_Airport_4326.bat || exit /b 9');
  end else begin
    writeln(HRR_file,'call GDAL_Airport.bat || exit /b 9');
  end;
  writeln(HRR_file,'call DDS_Airport.bat || exit /b 9');

  // close the file
  Close(HRR_file);
  MessageShow(FileName+' done.');
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Button_HiResRunwayClick(Sender: TObject);
const
  Margin : single = 30.0;    // meters
  EarthRadius = 6371000.0;  // metres
  Zoom_Level_Airport = '20';

var
  i : integer;
  Temp : single;
  Airport_Longitude : single;
  Airport_Latitude : single;
  Airport_Direction : single;
  Airport_Length : single;
  Airport_Width : single;
  Airport_WidthOffset : single;
  Airport_LengthOffset : single;
  Airport_Corners : array[0..4-1] of tCoord;
  Airport_R_Corners : array[0..4-1] of tCoord;
  Airport_G_Corners : array[0..4-1] of tCoord;
  Rotation_Matrix : array[0..2-1] of array[0..2-1] of single;
  X_Min, X_Max, Y_Min, Y_Max : single;
  X_Offset , Y_Offset : single;
  X_Extent , Y_Extent : single;
  Longitude_m_to_degrees, Latitude_m_to_degrees : single;
  FileName, FilePath : string;

  qTileColumn, qTileRow : integer;
  qTname :string;
  qDeltaColumn, qDeltaRow : single;

begin
  // pop-up request runway details
  // need length, width, offset in metres
//    Form_HiResRunway.Position := poDefault;
  // offset to be able to see status and progressbar
  Form_HiResRunway.Left := Self.Left + 50;
  Form_HiResRunway.Top  := Self.Top + 50;

  // if G file exists already use it as a preset - TBD
  if (false) then begin
//    // read G file and calc parameters - TBD
//    Form_HiResRunway.Edit_Length.Text := Y2-Y1;
//    Form_HiResRunway.Edit_Width.Text := X2-X1;
//    Form_HiResRunway.Edit_LengthOffset.Text := (Y2+Y1)/2;
//    Form_HiResRunway.Edit_WidthOffset.Text := (X2+X1)/2;
  end else begin
    // default to airport placer runway
    Form_HiResRunway.Edit_Length.Text := Edit_Length.Text;
    Form_HiResRunway.Edit_Width.Text := Edit_Width.Text;
    Form_HiResRunway.Edit_LengthOffset.Text := '0';
    Form_HiResRunway.Edit_WidthOffset.Text := '0';
  end;

  form_HiResRunway.ShowModal;
  Application.ProcessMessages;
  if ( NOT Unit_HiResRunway.gActionRequest) then begin
    exit;
  end;

  FilePath := u_MakeGMID.GMIDfolder+'\Airports\'+Edit_AirportName.Text;
  ForceDirectories(FilePath);

  // calculate corners in UTM metres
  Airport_Length := STRtoFloat(form_HiResRunway.Edit_Length.text);
  Airport_Width := STRtoFloat(form_HiResRunway.Edit_Width.text);
  Airport_WidthOffset := STRtoFloat(form_HiResRunway.Edit_WidthOffset.text);
  Airport_LengthOffset := STRtoFloat(form_HiResRunway.Edit_LengthOffset.text);
  Airport_Corners[0][0] :=  Airport_Width /2; Airport_Corners[0][1] :=  Airport_Length /2;
  Airport_Corners[1][0] :=  Airport_Width /2; Airport_Corners[1][1] := -Airport_Length /2;
  Airport_Corners[2][0] := -Airport_Width /2; Airport_Corners[2][1] := -Airport_Length /2;
  Airport_Corners[3][0] := -Airport_Width /2; Airport_Corners[3][1] :=  Airport_Length /2;

  // G file corners must include the offset
  for i := 0 to 4-1 do begin
    Airport_G_Corners[i][0] := Airport_Corners[i][0] + Airport_WidthOffset;
    Airport_G_Corners[i][1] := Airport_Corners[i][1] + Airport_LengthOffset;
  end;

  // get airport direction (degrees) and create rotation matrix
  Airport_Direction := strtofloat(Edit_Direction.Text);
  Rotation_Matrix[0][0] :=  COS((Airport_Direction-180)/180*Pi);
  Rotation_Matrix[0][1] := -SIN((Airport_Direction-180)/180*Pi);
  Rotation_Matrix[1][0] :=  SIN((Airport_Direction-180)/180*Pi);
  Rotation_Matrix[1][1] :=  COS((Airport_Direction-180)/180*Pi);

  // rotate airport corners
  Temp := Rotation_Matrix[0][0] * Airport_Corners[0][0] +
          Rotation_Matrix[0][1] * Airport_Corners[0][1];
  Airport_Corners[0][1] := Rotation_Matrix[1][0] * Airport_Corners[0][0] +
                           Rotation_Matrix[1][1] * Airport_Corners[0][1];
  Airport_Corners[0][0] := Temp;

  Temp := Rotation_Matrix[0][0] * Airport_Corners[1][0] +
          Rotation_Matrix[0][1] * Airport_Corners[1][1];
  Airport_Corners[1][1] := Rotation_Matrix[1][0] * Airport_Corners[1][0] +
                           Rotation_Matrix[1][1] * Airport_Corners[1][1];
  Airport_Corners[1][0] := Temp;

  Temp := Rotation_Matrix[0][0] * Airport_Corners[2][0] +
          Rotation_Matrix[0][1] * Airport_Corners[2][1];
  Airport_Corners[2][1] := Rotation_Matrix[1][0] * Airport_Corners[2][0] +
                           Rotation_Matrix[1][1] * Airport_Corners[2][1];
  Airport_Corners[2][0] := Temp;

  Temp := Rotation_Matrix[0][0] * Airport_Corners[3][0] +
          Rotation_Matrix[0][1] * Airport_Corners[3][1];
  Airport_Corners[3][1] := Rotation_Matrix[1][0] * Airport_Corners[3][0] +
                           Rotation_Matrix[1][1] * Airport_Corners[3][1];
  Airport_Corners[3][0] := Temp;

  // find min, max, extents
  X_Min := Airport_Corners[0][0];
  for i := 1 to 4-1 do begin
    if (Airport_Corners[i][0] < X_Min) then begin
      X_Min := Airport_Corners[i][0];
    end;
  end;
  Y_Min := Airport_Corners[0][1];
  for i := 1 to 4-1 do begin
    if (Airport_Corners[i][1] < Y_Min) then begin
      Y_Min := Airport_Corners[i][1];
    end;
  end;
  X_Max := Airport_Corners[0][0];
  for i := 1 to 4-1 do begin
    if (Airport_Corners[i][0] > X_Max) then begin
      X_Max := Airport_Corners[i][0];
    end;
  end;
  Y_Max := Airport_Corners[0][1];
  for i := 1 to 4-1 do begin
    if (Airport_Corners[i][1] > Y_Max) then begin
      Y_Max := Airport_Corners[i][1];
    end;
  end;
  // add margin to extents
  X_Extent := X_Max-X_min + 2*Margin;
  Y_Extent := Y_Max-Y_min + 2*Margin;
  // calculate rotated UTM offsets (rotate by - airportDirection)
  X_Offset := Airport_WidthOffset*  COS((-Airport_Direction/180*Pi))
             +Airport_LengthOffset*-SIN((-Airport_Direction/180*Pi));
  Y_Offset := Airport_WidthOffset*  SIN((-Airport_Direction/180*Pi))
             +Airport_LengthOffset* COS((-Airport_Direction/180*Pi));

  // calculate texture coords relative to extents range
  for i := 0 to 4-1 do begin
    Airport_R_Corners[i][0] := (Airport_Corners[i][0] / X_Extent) + 0.5;
    Airport_R_Corners[i][1] := (Airport_Corners[i][1] / Y_Extent) + 0.5;
  end;
  // make runway .px file
  FileName := 'Airport.px';
  Make_Px_Airport(FilePath, FileName, 'Textures/AirportName.dds',
    Airport_G_Corners,Airport_R_Corners);

  // try to use DDS tile in Textures folder
  begin
    // find tile to use
    qTileColumn := trunc(AirportEasting/(5760));
    qTileRow := trunc(AirportNorthing/(5760));
//    qTname := format('../Textures/t%2.2d%2.2d.dds',[qTileColumn,qTileRow]);
    qTname := format('../Textures/t%s.dds',[MakeTileName(qTileColumn,qTileRow, TileNameMode)]);
    qDeltaColumn := (5760) - (AirportEasting - qTileColumn * (5760));
    qDeltaRow := (5760) - (AirportNorthing - qTileRow * (5760));
    qDeltaColumn := qDeltaColumn +  // add offset
                    Rotation_Matrix[0][0] * Airport_WidthOffset +
                    Rotation_Matrix[0][1] * Airport_LengthOffset;
    qDeltaRow := qDeltaRow +  // add offset
                 Rotation_Matrix[1][0] * Airport_WidthOffset +
                 Rotation_Matrix[1][1] * Airport_LengthOffset;
    // calculate texture coords relative to tile range
    for i := 0 to 4-1 do begin
      Airport_R_Corners[i][0] := (qDeltaColumn + Airport_Corners[i][0]) / (5760);
      Airport_R_Corners[i][1] := (qDeltaRow + Airport_Corners[i][1]) / (5760);
    end;
    // make runway .px file
    FileName := 'Airport_Tile.px';
    Make_Px_Airport(FilePath, FileName, qTname,
      Airport_G_Corners,Airport_R_Corners);
  end;

  // zoom_level control - TBD
  // Y_pix/m = (256 * 2^ZoomLevel * cos(lat)) / (earthRadius*2*Pi)
  // if Y_Extent * (Y_pix/m) > 16384 pix -> ZoomLevel-=1
  // X_pix/m = (256 * 2^ZoomLevel) / (earthRadius*2*Pi)
  // if X_Extent * (X_pix/m) > 16384 pix -> ZoomLevel-=1

  // get centre of airport lat/long coords
  Airport_Longitude := strtofloat(Edit_Longitude.Text);
  Airport_Latitude := strtofloat(Edit_Latitude.Text);

  // convert extents to degrees based on earth radius and latitude
  // could also have done a UTM to lat/long conversion
  Latitude_m_to_degrees := 360/(EarthRadius*2*Pi);
  Longitude_m_to_degrees := Latitude_m_to_degrees / COS(Airport_Latitude/180*Pi);
  // create a lat/long coords for GMID download and combine batch files
  X_Min := Airport_Longitude+(X_Offset-X_Extent/2)*Longitude_m_to_degrees;
  X_Max := Airport_Longitude+(X_Offset+X_Extent/2)*Longitude_m_to_degrees;
  Y_Min := Airport_Latitude+ (Y_Offset-Y_Extent/2)* Latitude_m_to_degrees;
  Y_Max := Airport_Latitude+ (Y_Offset+Y_Extent/2)* Latitude_m_to_degrees;
  // create download and combine batch files
  // need to create a more generic make_GMID to call here which includes download and combine - TBD
  FileName := 'Batch_Airport_Download.bat';
  Make_Batch_DownloadCombine(td_D, 'Airport',
                             u_MakeGMID.GMIDMapID, u_MakeGMID.GMIDMapType,
                             FilePath, FileName,
                             Zoom_Level_Airport,
                             X_Min, X_Max, Y_Max, Y_Min);
  FileName := 'Batch_Airport_Combine.bat';
  Make_Batch_DownloadCombine(td_C, 'Airport',
                             u_MakeGMID.GMIDMapID, u_MakeGMID.GMIDMapType,
                             FilePath, FileName,
                             Zoom_Level_Airport,
                             X_Min, X_Max, Y_Max, Y_Min);

  // get landscape Bottom-Right UTM coords
  // use UTM_Right, UTM_Bottom
  // get airport relative UTM coords
  // use AirportEasting, AirportNorthing
  // calculate desired UTM for GDAL
  X_Min := UTM_Right-AirportEasting+  (X_Offset-X_Extent/2);
  X_Max := UTM_Right-AirportEasting+  (X_Offset+X_Extent/2);
  Y_Min := UTM_Bottom+AirportNorthing+(Y_Offset-Y_Extent/2);
  Y_Max := UTM_Bottom+AirportNorthing+(Y_Offset+Y_Extent/2);
  // create GDAL batch file
  FileName := 'GDAL_Airport';
  MakeAutoGDAL_Generic(3857, FileName+'.bat', FilePath,
    'Airport', Zoom_Level_Airport,
    X_Min, X_Max, Y_Min, Y_Max);
  MakeAutoGDAL_Generic(4326, FileName+'_4326.bat', FilePath,
    'Airport', Zoom_Level_Airport,
    X_Min, X_Max, Y_Min, Y_Max);
  // create DDS batch file
  FileName := 'DDS_Airport.bat';
  MakeDDS_Generic('Airport', FilePath, FileName);

  // now make a Batch ALL
  Make_Airport_All_BatchFile(3857, FilePath, FileName);
  Make_Airport_All_BatchFile(4326, FilePath, FileName); // geid as well
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Label_DirectionDblClick(Sender: TObject);
begin
  GUI_State := DirectionSelectScreen;
  Screen.Cursor := crCross;
  Image_Tile.ShowHint := False;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Label_LengthDblClick(Sender: TObject);
begin
  GUI_State := LengthSelectScreen;
  Screen.Cursor := crCross;
  Image_Tile.ShowHint := False;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Label_WidthDblClick(Sender: TObject);
begin
  GUI_State := WidthSelectScreen;
  Screen.Cursor := crCross;
  Image_Tile.ShowHint := False;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Label3DblClick(Sender: TObject);
begin
  GUI_State := CentreSelectScreen;
  Screen.Cursor := crCross;
  Image_Tile.ShowHint := False;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Label2DblClick(Sender: TObject);
begin
  GUI_State := CentreSelectScreen;
  Screen.Cursor := crCross;
  Image_Tile.ShowHint := False;
end;

//---------------------------------------------------------------------------
procedure TForm_AirportPlacer.Button_HelpClick(Sender: TObject);
begin
  Form_Help.ShowHelp('AirportPlace.hlp.txt',
    Self.Left + ScrollBox_Image.left + 8,
    Self.Top + ScrollBox_Image.Top + 30);
end;

//---------------------------------------------------------------------------
end.

{--- End of File ------------------------------------------------------------}


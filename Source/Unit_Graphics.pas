{
 * Unit_Graphics.pas
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

//selection rectangle works only for 'mask' tile !

//---------------------------------------------------------------------------
unit Unit_Graphics;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, FileCtrl, Menus,
  u_BMP;

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

  TForm_Graphic = class(TForm)
    GroupBox_Selections: TGroupBox;
    Button_Exit: TButton;
    ScrollBox_Image: TScrollBox;
    Image_Mask: TImage;
    Image_Tile: TImage;
    Image_Palette: TImage;
    Image_Alternate: TImage;
    Button_Color0: TButton;
    Button_Color1: TButton;
    Button_Color2: TButton;
    Button_Save: TButton;
    Button_Pen: TButton;
    Button_Line: TButton;
    Button_None: TButton;
    Button_Flood: TButton;
    Button_Undo: TButton;
    Shape_Pick: TShape;
    Button_PickColor: TButton;
    Shape_EditColor: TShape;
    Button_ResetFilter: TButton;
    Button_Match: TButton;
    Button_Pixelize: TButton;
    Edit_PixelizeRange: TEdit;
    Button_ApplyFilter: TButton;
    Button_Exclusion: TButton;
    Button_Brush: TButton;
    ComboBox_BrushSize: TComboBox;
    ComboBox_BrushType: TComboBox;
    Button_Options: TButton;
    Button_Window: TButton;
    Button_Rectangle: TButton;
    Button_Ellipse: TButton;
    CheckBox_Selection: TCheckBox;
    Button_Replace: TButton;
    ProgressBar_Status: TProgressBar;
    Button_Tool_0: TButton;
    Button_Swamp: TButton;
    Button_Sand: TButton;
    Button_Import: TButton;
    Label_Coords: TLabel;
    Button_Canny: TButton;
    Button_Convolve: TButton;
    Label1: TLabel;
    Button_Swell: TButton;
    Button_Shrink: TButton;
    Button_ColorReduce: TButton;
    PopupMenu_Graphics: TPopupMenu;
    ToggleMask: TMenuItem;
    ToggleGeomask: TMenuItem;
    SaveBGmap: TMenuItem;
    LoadBGmap: TMenuItem;
    Reloadmap1: TMenuItem;
    CheckBox_Alternate: TCheckBox;
    CheckBox_Overwrite: TCheckBox;
    Button_ZoomIn: TButton;
    Button_ZoomOut: TButton;
    Button_Save_TIF: TButton;
    Button_Help: TButton;
    procedure Button_ExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button_Color0Click(Sender: TObject);
    procedure Button_Color1Click(Sender: TObject);
    procedure Button_Color2Click(Sender: TObject);
    procedure Button_SaveClick(Sender: TObject);
    procedure Button_PenClick(Sender: TObject);
    procedure Button_LineClick(Sender: TObject);
    procedure Button_NoneClick(Sender: TObject);
    procedure Button_FloodClick(Sender: TObject);
//    procedure FillHoriz(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button_UndoClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button_PickColorClick(Sender: TObject);
    procedure Button_ResetFilterClick(Sender: TObject);
    procedure Button_MatchClick(Sender: TObject);
    procedure Button_PixelizeClick(Sender: TObject);
    procedure Button_ApplyFilterClick(Sender: TObject);
    procedure Button_ExclusionClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button_BrushClick(Sender: TObject);
    procedure ComboBox_BrushSizeChange(Sender: TObject);
    procedure ComboBox_BrushTypeChange(Sender: TObject);
    procedure Button_OptionsClick(Sender: TObject);
    procedure Button_WindowClick(Sender: TObject);
    procedure Button_RectangleClick(Sender: TObject);
    procedure Button_EllipseClick(Sender: TObject);
    procedure ShowSelection(Sender: TObject);
    procedure CheckBox_SelectionClick(Sender: TObject);
    procedure Button_ReplaceClick(Sender: TObject);
    procedure Button_Tool_0Click(Sender: TObject);
    procedure Button_SwampClick(Sender: TObject);
    procedure Button_SandClick(Sender: TObject);
    procedure Button_ImportClick(Sender: TObject);
    procedure ShowCoord(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure Button_CannyClick(Sender: TObject);
    procedure Button_ConvolveClick(Sender: TObject);
    procedure Edit_PixelizeRangeExit(Sender: TObject);
    procedure Button_ShrinkClick(Sender: TObject);
    procedure Button_SwellClick(Sender: TObject);
    procedure Image_TileProgress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
      const Msg: String);
    procedure Button_ColorReduceClick(Sender: TObject);
    procedure Image_PaletteClick(Sender: TObject);
    procedure ScrollBox_ImageConstrainedResize(Sender: TObject;
      var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer);
    procedure FormActivate(Sender: TObject);
    procedure BitmapSave(BitMap1 : TBitmap);
    procedure BitmapUndo(BitMap1 : TBitmap);
//    procedure IncludeLineEnd(DownXcoord,DownYcoord:integer;
//                         var LastXcoord,LastYcoord:integer);
    procedure PenInterpolate(DownXcoord,DownYcoord,x_coord,y_coord:integer);
    procedure ToggleMaskView(Sender: TObject);
    procedure ToggleAlternateView(Sender: TObject);
    procedure LoadImageView(Sender: TObject);
    procedure SaveImageView(Sender: TObject);
    procedure ScrollBox_ImageMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ScrollBox_ImageMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure ScrollBox_ImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button_ZoomInClick(Sender: TObject);
    procedure Button_ZoomOutClick(Sender: TObject);
    procedure Button_Save_TIFClick(Sender: TObject);
    procedure ScrollBox_ImageResize(Sender: TObject);
    procedure Button_HelpClick(Sender: TObject);
  private
    { Private declarations }
    procedure MyScrollHorz(Sender: TObject);
    procedure MyScrollVert(Sender: TObject);
  public
    { Public declarations }
    procedure SetBrushColor(bColor : Tcolor);
    procedure SaveMaskFile(FileName : string);
    procedure GrayScale(const Image:TImage);
    procedure MatchRange_RGB(const Image:TImage;mColor:Tcolor);
    procedure MatchRange_HSV(const Image:TImage;mColor:Tcolor);
    procedure MatchRange_CIE(const Image:TImage;mColor:Tcolor);
//    procedure Pixelize(const Image:TImage;mColor:Tcolor);
    procedure Pixelize(const Image:TImage;mColor:ColorConvert);
    procedure ApplyFilter(const Image:TImage;mColor:Tcolor);
    procedure ReplaceColor(const Image:TImage;mColor:Tcolor);
  end;

var
  Form_Graphic: TForm_Graphic;

  Graphic_Mode : (gmForest,gmThermal);
  ActivePixelColor : Tcolor;
  xScale, yScale : integer;
  zoomScale : double;
  SelectionRectangle : tRect;

  wWorkingPath : string; // added for DetectTree

  tFileName : string;  // tile file for background
  atFileName : string; // alternate modified background
  mFileName : string;  // forest file name
  thFileName : string; // thermal file name
  bFileName : string;  // alternate background
  eFileName : string;  // externally generated tile

  xCoord : double;
  yCoord : double;
  xReference : double;
  yReference : double;

//Procedure ConvertForestMask;
Procedure ConvertForestMask(hDeciduous,hConiferous,hDefault : byte);

//---------------------------------------------------------------------------
implementation

{$R *.DFM}

uses
  u_RGB_HSV, u_RGB_CIE, Unit_Filter, u_Forest, u_Thermal, u_Util, u_TIFF,
  u_SceneryHDR, u_CannyEdge, u_Convolve, u_ReduceColors, u_BitmapCursor,
  Unit_Help;

type
  ToolType = (t_None,t_Pen,t_Line,t_Brush,t_Flood,t_Replace,
              t_Pick,t_Select,t_Rectangle,t_Ellipse,
              t_Swell,t_Shrink);

var
  GUI_State : (IdleScreen, SelectScreen, ScrollScreen, CancelScreen);
  LastX, LastY : longint; // for scrollScreen
  EditTool : ToolType;
  x_coord, y_coord : longint;
  LastXcoord, LastYcoord : longint;
  DownXcoord, DownYcoord : longint;
//  DownXcoord2, DownYcoord2 : longint;
  MaskView : boolean;
  AlternateView : boolean;
  BrushBitmap : TBitMap;
  BitMap_Save : TBitMap;
  cX, cY : double;  // current centre relative

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
procedure TForm_Graphic.MyScrollVert(Sender: TObject);
begin
  cY := (ScrollBox_Image.VertScrollBar.Position + (ScrollBox_Image.ClientHeight div 2))
        / (ScrollBox_Image.VertScrollBar.Range);
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.MyScrollHorz(Sender: TObject);
begin
  cX := (ScrollBox_Image.HorzScrollBar.Position + (ScrollBox_Image.ClientWidth div 2))
        / (ScrollBox_Image.HorzScrollBar.Range);
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_ExitClick(Sender: TObject);
begin
  Close;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.SetBrushColor(bColor : Tcolor);
var
  BrushSize : integer;

begin
  with ComboBox_BrushSize do begin
    BrushSize := StrToInt(Items[ItemIndex]);
  end;
  with BrushBitmap do begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := TransParentColor;
    Canvas.FillRect(rect(0,0,Width,Height));
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Mode := pmCopy;
    Canvas.Pen.Color := bColor;
    Canvas.Pen.Width := 1;
    Canvas.Brush.Color := bColor;
    case BrushSize of
      1,2: begin
        Canvas.FillRect(rect((Width-BrushSize) div 2,(Height-BrushSize) div 2,
                     (Width-BrushSize) div 2+BrushSize,(Height-BrushSize) div 2+BrushSize));
      end;
      else begin
        if ComboBox_BrushType.ItemIndex = 1 then begin
          Canvas.Ellipse((Width-BrushSize) div 2,(Height-BrushSize) div 2,
                       (Width-BrushSize) div 2+BrushSize,(Height-BrushSize) div 2+BrushSize);
        end else begin
          Canvas.FillRect(rect((Width-BrushSize) div 2,(Height-BrushSize) div 2,
                       (Width-BrushSize) div 2+BrushSize,(Height-BrushSize) div 2+BrushSize));
        end;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.ComboBox_BrushSizeChange(Sender: TObject);
begin
  SetBrushColor(ActivePixelColor);
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.ComboBox_BrushTypeChange(Sender: TObject);
begin
  SetBrushColor(ActivePixelColor);
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.GrayScale(const Image:TImage);
var
  BytesPerScan: Integer;
  iWidth, iHeight: Integer;
  pScanLine: pByteArray;
  R,G,B: Byte;

begin
  try
    BytesPerScan := Abs(Integer(Image.Picture.Bitmap.ScanLine[1])-
    Integer(Image.Picture.Bitmap.ScanLine[0]));
  except
    raise exception.create('Error');
  end;

  for iHeight := 0 to Image.Picture.Bitmap.Height - 1 do begin
    pScanLine := Image.Picture.Bitmap.ScanLine[iHeight];
    iWidth := 0;
    while(iWidth <= BytesPerScan - 1) do begin
      R := pScanLine^[iWidth + 2];
      G := pScanLine^[iWidth + 1];
      B := pScanLine^[iWidth + 0];
      pScanLine^[iWidth + 2] := Byte(Round(0.299 * R + 0.587 * G + 0.114 * B));
      pScanLine^[iWidth + 1] := Byte(Round(0.299 * R + 0.587 * G + 0.114 * B));
      pScanLine^[iWidth + 0] := Byte(Round(0.299 * R + 0.587 * G + 0.114 * B));
      iWidth := iWidth + 3;
    end;
  end;
  Image.Refresh;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.MatchRange_RGB(const Image:TImage;mColor:Tcolor);
const
  Threshold = 255;

var
//  BytesPerScan: Integer;
  iWidth, iHeight: Integer;
//  pScanLine: pByteArray;
  pScanLine: pRGBArray;
  R,G,B: Byte;
  RR,GG,BB: Byte;
  R_Range : real;
  G_Range : real;
  B_Range : real;
//  MHC : ColorConvert;

begin
{  try
    BytesPerScan := Abs(Integer(Image.Picture.Bitmap.ScanLine[1])-
    Integer(Image.Picture.Bitmap.ScanLine[0]));
  except
    raise exception.create('Error');
  end;
}
//  MHC.ColorValue := ByteReverseOrder32(MatchHighlightColor);
  with Form_Filter do begin
    R_Range := StrToFloat(Edit_MatchRange_Red.text)/100*255;
    G_Range := StrToFloat(Edit_MatchRange_Green.text)/100*255;
    B_Range := StrToFloat(Edit_MatchRange_Blue.text)/100*255;
  end;
  RR := GetRvalue(mColor);
  GG := GetGvalue(mColor);
  BB := GetBvalue(mColor);
  ProgressBar_Status.Max := Image.Picture.Bitmap.Height;
  for iHeight := 0 to Image.Picture.Bitmap.Height - 1 do begin
    pScanLine := Image.Picture.Bitmap.ScanLine[iHeight];
//    iWidth := 0;
//    while(iWidth <= BytesPerScan - 1) do begin
    for iWidth := 0 to Image.Picture.Bitmap.Width - 1 do begin
//      R := pScanLine^[iWidth + 2];
//      G := pScanLine^[iWidth + 1];
//      B := pScanLine^[iWidth + 0];
      R := pScanLine^[iWidth].rgbtRed;
      G := pScanLine^[iWidth].rgbtGreen;
      B := pScanLine^[iWidth].rgbtBlue;
{      if (sqrt(sqr((RR-R)/R_Range)
              +sqr((GG-G)/G_Range)
              +sqr((BB-B)/B_Range))) < Threshold then begin }
      if ((ABS(RR-R)<=R_Range) AND
          (ABS(GG-G)<=G_Range) AND
          (ABS(BB-B)<=B_Range)) then begin
//        pScanLine^[iWidth + 2] := GetRValue(MatchHighlightColor);
//        pScanLine^[iWidth + 1] := GetGValue(MatchHighlightColor);
//        pScanLine^[iWidth + 0] := GetBValue(MatchHighlightColor);
//        pScanLine^[iWidth] := ColorConvert(MHC).cRGB;
        pScanLine^[iWidth] := MatchHighlightColor.cRGB;

      end;
//      iWidth := iWidth + 3;
    end;
    ProgressBar_Status.StepIt;
    Application.ProcessMessages;
  end;
  Image.Refresh;
  ProgressBar_Status.Position := 0;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.MatchRange_HSV(const Image:TImage;mColor:Tcolor);
var
//  BytesPerScan: Integer;
  iWidth, iHeight: Integer;
//  pScanLine: pByteArray;
  pScanLine: pRGBArray;
  R,G,B: Byte;
  H,S,V: single;
  HH,SS,VV: single;
  H_Range : real;
  S_Range : real;
  V_Range : real;
//  MHC : ColorConvert;

begin
{  try
    BytesPerScan := Abs(Integer(Image.Picture.Bitmap.ScanLine[1])-
    Integer(Image.Picture.Bitmap.ScanLine[0]));
  except
    raise exception.create('Error');
  end;
}
//  MHC.ColorValue := ByteReverseOrder32(MatchHighlightColor);
  with Form_Filter do begin
    H_Range := StrToFloat(Edit_MatchRange_Hue.text)/100;
    S_Range := StrToFloat(Edit_MatchRange_Saturation.text)/100;
    V_Range := StrToFloat(Edit_MatchRange_Value.text)/100*255;
  end;
  R := GetRvalue(mColor);
  G := GetGvalue(mColor);
  B := GetBvalue(mColor);
  RGBtoHSV(R,G,B,H,S,V);
  ProgressBar_Status.Max := Image.Picture.Bitmap.Height;
  for iHeight := 0 to Image.Picture.Bitmap.Height - 1 do begin
    pScanLine := Image.Picture.Bitmap.ScanLine[iHeight];
//    iWidth := 0;
//    while(iWidth <= BytesPerScan - 1) do begin
    for iWidth := 0 to Image.Picture.Bitmap.Width - 1 do begin
//      R := pScanLine^[iWidth + 2];
//      G := pScanLine^[iWidth + 1];
//      B := pScanLine^[iWidth + 0];
      R := pScanLine^[iWidth].rgbtRed;
      G := pScanLine^[iWidth].rgbtGreen;
      B := pScanLine^[iWidth].rgbtBlue;
      RGBtoHSV(R,G,B,HH,SS,VV);
//    if ( (ABS(HH-H) <= H_Range) AND
      // HUE wraps around!
      if ( ((ABS(HH-H) <= H_Range) OR (ABS(HH-(1+H)) <= H_Range)) AND
           (ABS(SS-S) <= S_Range) AND
           (ABS(VV-V) <= V_Range) ) then begin
//        pScanLine^[iWidth + 2] := GetRValue(MatchHighlightColor);
//        pScanLine^[iWidth + 1] := GetGValue(MatchHighlightColor);
//        pScanLine^[iWidth + 0] := GetBValue(MatchHighlightColor);
//        pScanLine^[iWidth] := ColorConvert(MHC).cRGB;
        pScanLine^[iWidth] := MatchHighlightColor.cRGB;
      end;
//      iWidth := iWidth + 3;
    end;
    ProgressBar_Status.StepIt;
    Application.ProcessMessages;
  end;
  Image.Refresh;
  ProgressBar_Status.Position := 0;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.MatchRange_CIE(const Image:TImage;mColor:Tcolor);
var
//  BytesPerScan: Integer;
  iWidth, iHeight: Integer;
//  pScanLine: pByteArray;
  pScanLine: pRGBArray;
  R,G,B: Byte;
  cie_L,cie_a,cie_b: single;
  cie_LL,cie_aa,cie_bb: single;
  L_Range : real;
  a_Range : real;
  b_Range : real;
//  MHC : ColorConvert;

begin
{  try
    BytesPerScan := Abs(Integer(Image.Picture.Bitmap.ScanLine[1])-
    Integer(Image.Picture.Bitmap.ScanLine[0]));
  except
    raise exception.create('Error');
  end;
}
//  MHC.ColorValue := ByteReverseOrder32(MatchHighlightColor);
  with Form_Filter do begin
    L_Range := StrToFloat(Edit_MatchRange_L.text)/100*100;
    a_Range := StrToFloat(Edit_MatchRange_a.text)/100*150;
    b_Range := StrToFloat(Edit_MatchRange_b.text)/100*150;
  end;
  R := GetRvalue(mColor);
  G := GetGvalue(mColor);
  B := GetBvalue(mColor);
  RGBtoCIElab(R,G,B,cie_L,cie_a,cie_b);
  ProgressBar_Status.Max := Image.Picture.Bitmap.Height;
  for iHeight := 0 to Image.Picture.Bitmap.Height - 1 do begin
    pScanLine := Image.Picture.Bitmap.ScanLine[iHeight];
//    iWidth := 0;
//    while(iWidth <= BytesPerScan - 1) do begin
    for iWidth := 0 to Image.Picture.Bitmap.Width - 1 do begin
//      R := pScanLine^[iWidth + 2];
//      G := pScanLine^[iWidth + 1];
//      B := pScanLine^[iWidth + 0];
      R := pScanLine^[iWidth].rgbtRed;
      G := pScanLine^[iWidth].rgbtGreen;
      B := pScanLine^[iWidth].rgbtBlue;
      RGBtoCIElab(R,G,B,cie_LL,cie_aa,cie_bb);
//    if ( (ABS(HH-H) <= H_Range) AND
      if ( (ABS(cie_LL-cie_L) <= L_Range) AND
           (ABS(cie_aa-cie_a) <= a_Range) AND
           (ABS(cie_bb-cie_b) <= b_Range) ) then begin
//        pScanLine^[iWidth + 2] := GetRValue(MatchHighlightColor);
//        pScanLine^[iWidth + 1] := GetGValue(MatchHighlightColor);
//        pScanLine^[iWidth + 0] := GetBValue(MatchHighlightColor);
//        pScanLine^[iWidth] := ColorConvert(MHC).cRGB;
        pScanLine^[iWidth] := MatchHighlightColor.cRGB;
      end;
//      iWidth := iWidth + 3;
    end;
    ProgressBar_Status.StepIt;
    Application.ProcessMessages;
  end;
  Image.Refresh;
  ProgressBar_Status.Position := 0;
end;

//---------------------------------------------------------------------------
//procedure TForm_Graphic.Pixelize(const Image:TImage;mColor:Tcolor);
procedure TForm_Graphic.Pixelize(const Image:TImage;mColor:ColorConvert);

var
//  BytesPerScan: Integer;
  iWidth, iHeight: Integer;
  iW, iH: Integer;
//  pScanLine: pByteArray;
  pScanLine: pRGBArray;
//  RR,GG,BB: Byte;
  MatchCount : integer;
  Threshold : real;
//  MC, PHC : xColorConvert;
  xScale, yScale : integer;

begin
  xScale := Image.Picture.Width div {tColumns} Image_Mask.Picture.Width;
  yScale := Image.Picture.Height div {tRows} Image_Mask.Picture.Height;
{  try
    BytesPerScan := Abs(Integer(Image.Picture.Bitmap.ScanLine[1])-
    Integer(Image.Picture.Bitmap.ScanLine[0]));
  except
    raise exception.create('Error');
  end;
}
//  PHC.ColorValue := ByteReverseOrder32(PixelizeHighlightColor);
//  MC.ColorValue := ByteReverseOrder32(mColor);
//  MC.ColorValue := ByteSwapColor(mColor);
  Threshold := StrToFloat(Edit_PixelizeRange.text)/100;
//  RR := GetRvalue(mColor);
//  GG := GetGvalue(mColor);
//  BB := GetBvalue(mColor);

  Image.Transparent := false; // !!! if transparent using scanline to overwrite a pixel will not work !!!

  ProgressBar_Status.Max := (Image.Picture.Bitmap.Height) div yScale;
  for iHeight := 0 to (Image.Picture.Bitmap.Height) div yScale - 1 do begin
//    for iWidth := 0 to (BytesPerScan div 3) div xScale - 1 do begin
    for iWidth := 0 to (Image.Picture.Bitmap.Width) div xScale - 1 do begin
      MatchCount := 0;
      for iH := iHeight*yScale to iHeight*yScale+yScale-1 do begin
        pScanLine := Image.Picture.Bitmap.ScanLine[iH];
//        iW := iWidth*3*xScale;
//        while (iW < iWidth*3*xScale+3*xScale) do begin
        for iW := iWidth*xScale to iWidth*xScale+xScale-1 do begin
//          if ((pScanLine^[iW + 2] = RR) AND
//              (pScanLine^[iW + 1] = GG) AND
//              (pScanLine^[iW + 0] = BB)) then begin
          if (
//              CompareMem(@pScanLine^[iW],@xColorConvert(MC).cRGB,3)
              CompareMem(@pScanLine^[iW],@mcolor.cRGB,3)
             ) then begin
            INC(MatchCount);
          end;
//          INC(iW,3);
        end;
      end;
      if (MatchCount >= round(xScale*yScale*Threshold)) then begin
        for iH := iHeight*yScale to iHeight*yScale+yScale-1 do begin
          pScanLine := Image.Picture.Bitmap.ScanLine[iH];
//          iW := iWidth*3*xScale;
//          while (iW < iWidth*3*xScale+3*xScale) do begin
          for iW := iWidth*xScale to iWidth*xScale+xScale-1 do begin
//            pScanLine^[iW + 2] := GetRValue(PixelizeHighlightColor);
//            pScanLine^[iW + 1] := GetGValue(PixelizeHighlightColor);
//            pScanLine^[iW + 0] := GetBValue(PixelizeHighlightColor);
//            pScanLine^[iW] := ColorConvert(PHC).cRGB;
            pScanLine^[iW] := PixelizeHighlightColor.cRGB;
//            INC(iW,3);
          end;
        end;
      end;
    end;
    ProgressBar_Status.StepIt;
    Application.ProcessMessages;
  end;
  Image.Transparent := true;
  Image.Refresh;
  ProgressBar_Status.Position := 0;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.ApplyFilter(const Image:TImage;mColor:Tcolor);

var
//  BytesPerScan: Integer;
  iWidth, iHeight: Integer;
  iW, iH: Integer;
  pScanLineTile, pScanLineMask: pRGBArray;
//  RR,GG,BB: Byte;
//  MatchCount : integer;
//  Threshold : real;
  SelectionFlag : boolean;
  sLeft, sTop, sRight, sBottom : integer;
  {PHC,}MC : ColorConvert;
  xScale, yScale : integer;

begin
  Screen.Cursor := crHourGlass;  // Let user know we're busy...
  xScale := Image.Picture.Width div {tColumns} Image_Mask.Picture.Width;
  yScale := Image.Picture.Height div {tRows} Image_Mask.Picture.Height;
{  try
    BytesPerScan := Abs(Integer(Image.Picture.Bitmap.ScanLine[1])-
    Integer(Image.Picture.Bitmap.ScanLine[0]));
  except
    raise exception.create('Error');
  end;
}
//  Threshold := StrToFloat(Edit_PixelizeRange.text)/100;
  if (CheckBox_Selection.Checked) then begin
    SelectionFlag := true;
    ShowSelection(Nil); // temporarily turn off selection box
    if (SelectionRectangle.Left > SelectionRectangle.Right) then begin
      sLeft := SelectionRectangle.Right;
      sRight := SelectionRectangle.Left;
    end else begin
      sLeft := SelectionRectangle.Left;
      sRight := SelectionRectangle.Right;
    end;
    if (SelectionRectangle.Top > SelectionRectangle.Bottom) then begin
      sTop := SelectionRectangle.Bottom;
      sBottom := SelectionRectangle.Top;
    end else begin
      sTop := SelectionRectangle.Top;
      sBottom := SelectionRectangle.Bottom;
    end;
  end else begin
    SelectionFlag := false;
    sLeft := 0;
    sTop := 0;
//    sRight := (BytesPerScan div 3) div xScale;
    sRight := Image.Picture.Bitmap.Width div xScale;
    sBottom := Image.Picture.Bitmap.Height div yScale;
  end;
//  Image_Mask.Canvas.Pen.Mode := pmCopy; //needed for pixels[] !
  ProgressBar_Status.Max := sBottom-sTop;
  Image_Mask.Visible := false; //stops display update while processing
//  PHC.ColorValue := ByteReverseOrder32(PixelizeHighlightColor);
//  MC.ColorValue := ByteReverseOrder32(mColor);
  MC.ColorValue := ByteSwapColor(mColor); //convert from Tcolor to ColorConvert
  for iHeight := sTop to sBottom - 1 do begin
    iH := iHeight*yScale;
    pScanLineTile := Image.Picture.Bitmap.ScanLine[iH];
    pScanLineMask := Image_Mask.Picture.Bitmap.ScanLine[iHeight];
    for iWidth := sLeft to sRight - 1 do begin
      iW := iWidth*xScale;
      if (
          CompareMem(@pScanLineTile^[iW],@PixelizeHighlightColor.cRGB,3)
         ) then begin
        //apply color only if currently no color unless overwrite option selected
//        if (CompareMem(@pScanLineMask^[iWidth],@tNone.cRGB,3)) then begin
        if ((Form_Graphic.CheckBox_Overwrite.checked) OR
          (CompareMem(@pScanLineMask^[iWidth],@tNone.cRGB,3))) then begin
//          pScanLineMask^[iWidth] := ColorConvert(MC).cRGB;
          pScanLineMask^[iWidth] := MC.cRGB;
        end;
      end;
    end;
    ProgressBar_Status.StepIt;
    Application.ProcessMessages;
  end;
  if (SelectionFlag) then begin
    ShowSelection(Nil);
  end;

  if (MaskView) then begin
    Image_Mask.Visible := true;
//    Image_Mask.Refresh;
  end;
  ProgressBar_Status.Position := 0;
  Screen.Cursor := crDefault;  // no longer busy
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.ReplaceColor(const Image:TImage;mColor:Tcolor);

var
//  BytesPerScan: Integer;
  iWidth, iHeight: Integer;
//  iW, iH: Integer;
  pScanLine : pRGBArray;
//  RR,GG,BB: Byte;
//  MatchCount : integer;
//  Threshold : real;
  SelectionFlag : boolean;
  sLeft, sTop, sRight, sBottom : integer;
  APC, MC : ColorConvert;

begin
  Screen.Cursor := crHourGlass;  // Let user know we're busy...
  if (CheckBox_Selection.Checked) then begin
    SelectionFlag := true;
    ShowSelection(Nil); // temporarily turn off selection box
    if (SelectionRectangle.Left > SelectionRectangle.Right) then begin
      sLeft := SelectionRectangle.Right;
      sRight := SelectionRectangle.Left;
    end else begin
      sLeft := SelectionRectangle.Left;
      sRight := SelectionRectangle.Right;
    end;
    if (SelectionRectangle.Top > SelectionRectangle.Bottom) then begin
      sTop := SelectionRectangle.Bottom;
      sBottom := SelectionRectangle.Top;
    end else begin
      sTop := SelectionRectangle.Top;
      sBottom := SelectionRectangle.Bottom;
    end;
  end else begin
    SelectionFlag := false;
    sLeft := 0;
    sTop := 0;
//    sRight := BytesPerScan div 3;
    sRight := Image.Picture.Bitmap.Width;
    sBottom := Image.Picture.Bitmap.Height;
  end;
  ProgressBar_Status.Max := sBottom-sTop;
  Image_Mask.Visible := false; //stops display update while processing
  Image_Mask.Canvas.Pen.Mode := pmCopy; //needed for pixels[] !
//  APC.ColorValue := ByteReverseOrder32(ActivePixelColor);
  APC.ColorValue := ByteSwapColor(ActivePixelColor); //convert from Tcolor to ColorConvert
//  MC.ColorValue := ByteReverseOrder32(mColor);
  MC.ColorValue := ByteSwapColor(mColor); //convert from Tcolor to ColorConvert
  for iHeight := sTop to sBottom - 1 do begin
    pScanLine := Image.Picture.Bitmap.ScanLine[iHeight];
    for iWidth := sLeft to sRight - 1 do begin
//      if (CompareMem(@pScanLine^[iWidth],@ColorConvert(MC).cRGB,3)) then begin
//        pScanLine^[iWidth] := ColorConvert(APC).cRGB;
      if (CompareMem(@pScanLine^[iWidth],@MC.cRGB,3)) then begin
        pScanLine^[iWidth] := APC.cRGB;
      end;
    end;
    ProgressBar_Status.StepIt;
    Application.ProcessMessages;
  end;
  if (SelectionFlag) then begin
    ShowSelection(Nil);
  end;

  if (MaskView) then begin
    Image_Mask.Visible := true;
//    Image_Mask.Refresh;
  end;
  ProgressBar_Status.Position := 0;
  Screen.Cursor := crDefault;  // no longer busy
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.ShowSelection(Sender: TObject);
begin
  with Image_Mask do begin
    Canvas.Pen.Mode := pmNOTXOR;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Width := 1;
//    Canvas.Pen.Color := clBlack; //'NOT' white because pen pmNOTXOR is used
    Canvas.Pen.Color := ByteSwapColor(NOT(tSelectRectColor.Colorvalue)) AND $00FFFFFF; //'NOT' white because pen pmNOTXOR is used
    Canvas.Brush.Style := bsSolid;
//    Canvas.Brush.Color := clWhite; //'NOT' black because pen pmNOTXOR is used
    Canvas.Brush.Color := ByteSwapColor(NOT(tNone.Colorvalue)) AND $00FFFFFF; //'NOT' black because pen pmNOTXOR is used
    Canvas.Rectangle(SelectionRectangle.Left,
                     SelectionRectangle.Top,
                     SelectionRectangle.Right,
                     SelectionRectangle.Bottom);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.ScrollBox_ImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  BrushSize : integer;
  xOffset, yOffset : longInt;

begin
  if (Button = mbLeft) then begin
    if (ssCtrl in Shift) then begin //scroll bitmap
      LastX := X;
      LastY := Y;
      GUI_State := ScrollScreen;
    end else begin
//      x_coord := X div xScale;
//      y_coord := Y div yScale;
      x_coord := round(X / xScale * ZoomScale);
      y_coord := round(Y / yScale * ZoomScale);
      if (ssShift in Shift) then begin
        DownXcoord := LastXcoord; // if Shift key pressed continue from last point
        DownYcoord := LastYcoord;
      end else begin
        DownXcoord := x_coord;
        DownYcoord := y_coord;
      end;
      LastXcoord := x_coord;
      LastYcoord := y_coord;
      with ComboBox_BrushSize do begin
        BrushSize := StrToInt(Items[ItemIndex]);
      end;

      //special case not applicable to Image_Mask
      if EditTool = t_Pick then begin
        if (CheckBox_Alternate.checked AND AlternateView) then begin
//   xScale := Image_Alternate.Picture.Width div tColumns;
//   yScale := Image_Alternate.Picture.Height div tRows;
//          Shape_Pick.Brush.Color := Image_Alternate.Canvas.Pixels[X,Y];
 //         Shape_Pick.Brush.Color := Image_Alternate.Canvas.Pixels[
 //           X div (Image_Alternate.Picture.Width div tColumns),
 //           Y div (Image_Alternate.Picture.Height div tRows)
 //           ];
          xOffset := round(X * zoomScale * Image_Alternate.Picture.Width/Image_Tile.Picture.Width);
          yOffset := round(Y * zoomScale * Image_Alternate.Picture.Height/Image_Tile.Picture.Height);
          Shape_Pick.Brush.Color := Image_Alternate.Canvas.Pixels[xOffset,yOffset];
        end else begin
          xOffset := round(X * zoomScale);
          yOffset := round(Y * zoomScale);
//          Shape_Pick.Brush.Color := Image_Tile.Canvas.Pixels[X,Y];
          Shape_Pick.Brush.Color := Image_Tile.Canvas.Pixels[xOffset,yOffset];
        end;
        GUI_State := SelectScreen;
      end else begin

      if (MaskView) then begin
       with Image_Mask do begin
        BitmapSave(Picture.Bitmap); //allow for undo
        case EditTool of
          t_Pen: begin
//            BitmapSave(Image_Mask.Picture.Bitmap); //allow for undo
            Canvas.Pen.Mode := pmCopy; // needed for pixels[] !
            Canvas.Pixels[x_coord,y_coord] := ActivePixelColor;
            GUI_State := SelectScreen;
          end;
          t_Select,t_Rectangle,t_Line,t_Ellipse: begin
            Canvas.Pen.Mode := pmNOTXOR;
            Canvas.Pen.Style := psSolid;
//            Canvas.Pen.Color := clBlack; //'NOT' white because pen pmNOTXOR is used
    Canvas.Pen.Color := ByteSwapColor(NOT(tSelectRectColor.Colorvalue)) AND $00FFFFFF; //'NOT' white because pen pmNOTXOR is used
            Canvas.Brush.Style := bsSolid;
//            Canvas.Brush.Color := clWhite; //'NOT' black because pen pmNOTXOR is used
    Canvas.Brush.Color := ByteSwapColor(NOT (tNone.Colorvalue)) AND $00FFFFFF; //'NOT' black because pen pmNOTXOR is used
            case EditTool of
              t_Line: begin
                Canvas.Pen.Width := BrushSize;
{
                Canvas.MoveTo(DownXcoord,DownYcoord);
                IncludeLineEnd(DownXcoord,DownYcoord,x_coord,y_coord);
                Canvas.LineTo(x_coord,y_coord);
}
                Canvas.Pixels[x_coord,y_coord] := Canvas.Pen.Color;
                Canvas.MoveTo(x_coord,y_coord);
                Canvas.LineTo(DownXcoord,DownYcoord);
              end;
              t_Select,t_Rectangle: begin
                Canvas.Pen.Width := 1;
                Canvas.Rectangle(DownXcoord,DownYcoord,x_coord,y_coord);
              end;
              t_Ellipse: begin
                Canvas.Pen.Width := 1;
                Canvas.Ellipse(DownXcoord,DownYcoord,x_coord,y_coord);
              end;
            end;
            GUI_State := SelectScreen;
          end;
          t_Flood, t_Replace, t_Swell, t_Shrink: begin
            GUI_State := SelectScreen;
          end;
          t_Brush: begin
//            BitmapSave(Image_Mask.Picture.Bitmap); //allow for undo
            Canvas.CopyMode := cmSrcCopy;
            Canvas.Draw(x_coord-3,y_coord-3,BrushBitmap);
            GUI_State := SelectScreen;
          end;
        end;
       end;
      end;
      end;
    end;
  end else begin // NOT mbLeft
    if (Button = mbRight) then begin
      x_coord := X div xScale;
      y_coord := Y div yScale;
      if (MaskView) then begin
        with Image_Mask do begin
          case EditTool of
            t_Pen: begin
              Canvas.Pen.Mode := pmCopy; // needed for pixels[] !
              Canvas.Pixels[x_coord,y_coord] := ByteSwapColor(tNone.Colorvalue){clBlack}; //erase
            end;
          end;
        end;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.ScrollBox_ImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  ShowCoord(Sender, Shift, X, Y);
  if GUI_State = SelectScreen then begin
//    x_coord := X div xScale;
//    y_coord := Y div yScale;
    x_coord := round(X / xScale * ZoomScale);
    y_coord := round(Y / yScale * ZoomScale);

    //special case not applicable to Image_Mask
    if EditTool = t_Pick then begin
      // do nothing
    end else begin

    if (MaskView) then begin
     with Image_Mask do begin
      case EditTool of
        t_Pen: begin
          PenInterpolate(DownXcoord,DownYcoord,x_coord,y_coord);
//          Canvas.Pixels[x_coord,y_coord] := ActivePixelColor;
          DownXcoord := x_coord;
          DownYcoord := y_coord;
        end;
        t_Brush: begin
          Canvas.Draw(x_coord-3,y_coord-3,BrushBitmap);
        end;
        t_Line,t_Select,t_Rectangle,t_Ellipse: begin
          case EditTool of
            t_Line: begin
{
              Canvas.MoveTo(DownXcoord,DownYcoord);
              IncludeLineEnd(DownXcoord,DownYcoord,LastXcoord,LastYcoord);
              Canvas.LineTo(LastXcoord,LastYcoord);
              LastXcoord := x_coord;
              LastYcoord := y_coord;
              Canvas.MoveTo(DownXcoord,DownYcoord);
              IncludeLineEnd(DownXcoord,DownYcoord,x_coord,y_coord);
              Canvas.LineTo(x_coord,y_coord);
}
              Canvas.MoveTo(LastXcoord,LastYcoord);
              Canvas.LineTo(DownXcoord,DownYcoord);
              LastXcoord := x_coord;
              LastYcoord := y_coord;
              Canvas.MoveTo(LastXcoord,LastYcoord);
              Canvas.LineTo(DownXcoord,DownYcoord);
            end;
            t_Select,t_Rectangle: begin
              Canvas.Rectangle(DownXcoord,DownYcoord,LastXcoord,LastYcoord);
              LastXcoord := x_coord;
              LastYcoord := y_coord;
              Canvas.Rectangle(DownXcoord,DownYcoord,LastXcoord,LastYcoord);
            end;
            t_Ellipse: begin
              Canvas.Ellipse(DownXcoord,DownYcoord,LastXcoord,LastYcoord);
              LastXcoord := x_coord;
              LastYcoord := y_coord;
              Canvas.Ellipse(DownXcoord,DownYcoord,LastXcoord,LastYcoord);
            end;
          end;
          if (X > ScrollBox_Image.HorzScrollBar.Position + ScrollBox_Image.Width-5) then begin
            ScrollBox_Image.HorzScrollBar.Position :=
              ScrollBox_Image.HorzScrollBar.Position + (ScrollBox_Image.HorzScrollBar.Range div 10);
          end else begin
            if (X < ScrollBox_Image.HorzScrollBar.Position+5) then begin
              ScrollBox_Image.HorzScrollBar.Position :=
                ScrollBox_Image.HorzScrollBar.Position - (ScrollBox_Image.HorzScrollBar.Range div 10);
            end;
          end;
          if (Y > ScrollBox_Image.VertScrollBar.Position + ScrollBox_Image.Height-5) then begin
            ScrollBox_Image.VertScrollBar.Position :=
              ScrollBox_Image.VertScrollBar.Position + (ScrollBox_Image.VertScrollBar.Range div 10);
          end else begin
            if (Y < ScrollBox_Image.VertScrollBar.Position+5) then begin
              ScrollBox_Image.VertScrollBar.Position :=
                ScrollBox_Image.VertScrollBar.Position - (ScrollBox_Image.VertScrollBar.Range div 10);
            end;
          end;
        end;
      end;
     end;
    end;
    end;
  end else begin //NOT SelectScreen
    if GUI_State = ScrollScreen then begin
      if (Y-LastY <> 0) then begin
        ScrollBox_Image.VertScrollBar.Position :=
          ScrollBox_Image.VertScrollBar.Position - (Y-LastY);
//        LastY := Y; // not needed because Y is relative to bitmap itself
        MyScrollVert(Sender);
      end;
      if (X-LastX <> 0) then begin
        ScrollBox_Image.HorzScrollBar.Position :=
          ScrollBox_Image.HorzScrollBar.Position - (X-LastX);
//        LastX := X; // not needed because Y is relative to bitmap itself
        MyScrollHorz(Sender);
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.ScrollBox_ImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  xx : Tcolor;

begin
  if GUI_State = SelectScreen then begin
//    x_coord := X div xScale;
//    y_coord := Y div yScale;
    x_coord := round(X / xScale * ZoomScale);
    y_coord := round(Y / yScale * ZoomScale);

    //special case not applicable to Image_Mask
    if EditTool = t_Pick then begin
//    GUI_State := IdleScreen;
    end else begin

    if (MaskView) then begin
     with Image_Mask do begin
      case EditTool of
        t_Pen,t_Brush: begin
//          GUI_State := IdleScreen;
        end;
        t_Replace: begin
//          SaveMaskFile('tmp.bmp');
//          BitmapSave(Image_Mask.Picture.Bitmap); //allow for undo
          Image_Mask.Transparent := false; // !!! if transparent mask doesn't get update !!!
          ReplaceColor(Image_Mask,Canvas.Pixels[x_coord, y_coord]);
          Image_Mask.Transparent := true;
//          GUI_State := IdleScreen;
        end;
        t_Line: begin
{
          Canvas.MoveTo(DownXcoord,DownYcoord);
          IncludeLineEnd(DownXcoord,DownYcoord,LastXcoord,LastYcoord);
          Canvas.LineTo(LastXcoord,LastYcoord);
//          BitmapSave(Image_Mask.Picture.Bitmap); //allow for undo
          Canvas.Pen.Mode := pmCopy;
          Canvas.Pen.Color := ActivePixelColor;
          Canvas.MoveTo(DownXcoord,DownYcoord);
          IncludeLineEnd(DownXcoord,DownYcoord,x_coord,y_coord);
          Canvas.LineTo(x_coord,y_coord);
}
          Canvas.MoveTo(LastXcoord,LastYcoord);
          Canvas.LineTo(DownXcoord,DownYcoord);
          Canvas.Pixels[DownXcoord,DownYcoord] := Canvas.Pen.Color;
//          BitmapSave(Image_Mask.Picture.Bitmap); //allow for undo
          Canvas.Pen.Mode := pmCopy;
          Canvas.Pen.Color := ActivePixelColor;
          Canvas.Pixels[DownXcoord,DownYcoord] := Canvas.Pen.Color;
          Canvas.MoveTo(x_coord,y_coord);
          Canvas.LineTo(DownXcoord,DownYcoord);
//          GUI_State := IdleScreen;
        end;
        t_Select: begin
          Canvas.Rectangle(DownXcoord,DownYcoord,x_coord,y_coord);
          if (Checkbox_Selection.Checked = true) then begin
            Checkbox_Selection.Checked := false; // erase current if showing
          end;
          if (x_Coord < 0) then x_Coord := 0;
          if (x_Coord > Picture.Width) then x_Coord := Picture.Width;
          if (y_Coord < 0) then y_Coord := 0;
          if (y_Coord > Picture.Height) then y_Coord := Picture.Height;
          SelectionRectangle := rect(DownXcoord,DownYcoord,x_coord,y_coord);
          Checkbox_Selection.Checked := true;
//          GUI_State := IdleScreen;
        end;
        t_Rectangle: begin
          Canvas.Rectangle(DownXcoord,DownYcoord,x_coord,y_coord);
//          BitmapSave(Image_Mask.Picture.Bitmap); //allow for undo
          Canvas.Pen.Mode := pmCopy;
          Canvas.Pen.Color := ActivePixelColor;
          Canvas.Brush.Color := ActivePixelColor;
          Canvas.Rectangle(DownXcoord,DownYcoord,x_coord,y_coord);
//          GUI_State := IdleScreen;
        end;
        t_Ellipse: begin
          Canvas.Ellipse(DownXcoord,DownYcoord,x_coord,y_coord);
//          BitmapSave(Image_Mask.Picture.Bitmap); //allow for undo
          Canvas.Pen.Mode := pmCopy;
          Canvas.Pen.Color := ActivePixelColor;
          Canvas.Brush.Color := ActivePixelColor;
          Canvas.Ellipse(DownXcoord,DownYcoord,x_coord,y_coord);
//          GUI_State := IdleScreen;
        end;
        t_Flood: begin
//          SaveMaskFile('tmp.bmp');
//          BitmapSave(Image_Mask.Picture.Bitmap); //allow for undo
          Canvas.Brush.Style := bsSolid;
          Canvas.Brush.Color := ActivePixelColor;
          //when selection is active, border stops fill and therefore border is not included
//          Canvas.FloodFill(x_coord, y_coord, clBlack, fsSurface); // on black only
          Canvas.FloodFill(x_coord, y_coord, Canvas.Pixels[x_coord, y_coord], fsSurface); // on matching color
//          GUI_State := IdleScreen;
        end;
        t_Swell: begin
//          BitmapSave(Image_Mask.Picture.Bitmap); //allow for undo
          // !!! can't use ColorConvert(ByteReverseOrder32(Canvas.Pixels[x_coord, y_coord])).cRGB, need to use intermeduiate xx ???
//          xx := ByteReverseOrder32(Canvas.Pixels[x_coord, y_coord]);
          xx := ByteSwapColor(Canvas.Pixels[x_coord, y_coord]);
          Image_Mask.Transparent := false; // !!! if transparent using scanline to overwrite a pixel will not work !!!
          u_Convolve.ProgressBar_Status := ProgressBar_Status;
          u_Convolve.Convolve_Swell(Image_Mask.Picture.Bitmap,
//            ColorConvert(xx).cRGB);
            ColorConvert(xx).cRGB);
          Image_Mask.Transparent := true;
//          Image_Mask.Refresh;
        end;
        t_Shrink: begin
//          BitmapSave(Image_Mask.Picture.Bitmap); //allow for undo
//          xx := ByteReverseOrder32(Canvas.Pixels[x_coord, y_coord]);
          xx := ByteSwapColor(Canvas.Pixels[x_coord, y_coord]);
          Image_Mask.Transparent := false;
          u_Convolve.ProgressBar_Status := ProgressBar_Status;
          u_Convolve.Convolve_Shrink(Image_Mask.Picture.Bitmap,
//            ColorConvert(xx).cRGB);
            ColorConvert(xx).cRGB);
          Image_Mask.Transparent := true;
//          Image_Mask.Refresh;
        end;
      end;
     end;
    end;
    end;
  end;
  GUI_State := IdleScreen;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.ShowCoord(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Horiz, Vert : double;
begin
  Horiz := xCoord + Resolution*tColumns*
//    (Image_Tile.Picture.Width-1-X)/Image_Tile.Picture.Width;
    (Image_Tile.Width-1-X)/Image_Tile.Width;
  Vert := yCoord + Resolution*tRows*
//    (Image_Tile.Picture.Height-1-Y)/Image_Tile.Picture.Height;
    (Image_Tile.Height-1-Y)/Image_Tile.Height;
  Label_Coords.Caption := format('%1.3f,%1.3f',[Horiz,Vert]);
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.BitmapSave(BitMap1 : TBitmap);
begin
  //copy a bitmap and reduce resources
  BitMap_Save.Assign(BitMap1);     // Copy BitMap1 into BitMap2
  BitMap_Save.Dormant;             // Free up GDI resources
  BitMap_Save.FreeImage;           // Free up Memory.
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.BitmapUndo(BitMap1 : TBitmap);
begin
//copy a bitmap and reduce resources
   BitMap1.Assign(BitMap_Save);     // Copy BitMap1 into BitMap2
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.PenInterpolate(DownXcoord,DownYcoord,x_coord,y_coord:integer);
var
  i : integer;
  x_Inc,y_Inc : integer;
  x_Step,y_Step : real;
  NewXcoord,NewYcoord : real;
  StepCount : integer;

begin
  x_Inc := x_coord-DownXcoord;
  y_Inc := y_coord-DownYcoord;
  if ( (ABS(x_Inc)>1) OR (ABS(y_Inc)>1) ) then begin
    if (ABS(x_Inc)>ABS(y_Inc)) then begin
      x_Step := x_Inc/ABS(x_Inc);
      y_Step := y_Inc / x_Inc * x_Step;
      StepCount := ABS(x_Inc);
    end else begin
      y_Step := y_Inc/ABS(y_Inc);
      x_step := x_Inc / y_Inc * y_Step;
      StepCount := ABS(y_Inc);
    end;
    NewXcoord := DownXCoord;
    NewYcoord := DownYCoord;
//    while ( (trunc(NewXcoord) <> x_Coord) OR (trunc(NewYcoord) <> y_Coord)) do begin
    for i := 0 to StepCount-1 do begin
      NewXcoord := NewXcoord+x_Step;
      NewYcoord := NewYcoord+y_Step;
      Image_Mask.Canvas.Pixels[trunc(NewXcoord),trunc(NewYcoord)] := ActivePixelColor;
    end;
  end else begin
    Image_Mask.Canvas.Pixels[x_Coord,y_Coord] := ActivePixelColor;
  end;
end;

//var
//  cX, cY, rX : longint;
//  ccX, rrX : longint;
//  cColor : Tcolor;
//  NoFill : boolean;
//  pScanLine: pByteArray;

//---------------------------------------------------------------------------
{procedure TForm_Graphic.IncludeLineEnd(DownXcoord,DownYcoord:integer;
                     var LastXcoord,LastYcoord:integer);
begin
  if (LastXcoord > DownXcoord) then begin
    INC(LastXcoord);
  end else begin
    if (LastXcoord < DownXcoord) then begin
      DEC(LastXcoord);
    end else begin
    end;
  end;
  if (LastYcoord > DownYcoord) then begin
    INC(LastYcoord);
  end else begin
    if (LastYcoord < DownYcoord) then begin
      DEC(LastYcoord);
    end else begin
    end;
  end;
end;
}
//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_UndoClick(Sender: TObject);
begin
{  if (FileExists('tmp.bmp')) then begin
    Image_Mask.Picture.LoadFromFile('tmp.bmp');
    Image_Mask.Picture.Bitmap.TransparentColor := ByteSwapColor(tNone.Colorvalue);
  end;
}  BitmapUndo(Image_Mask.Picture.Bitmap);
  if (CheckBox_Selection.Checked) then begin
    ShowSelection(Nil);
  end;
end;

//---------------------------------------------------------------------------
procedure ColorSelect(ColorValue : longword);
begin
//  ActivePixelColor := ByteReverseOrder32(Colorvalue);
  ActivePixelColor := ByteSwapColor(Colorvalue); //convert from ColorConvert to Tcolor
  Form_Graphic.Shape_EditColor.Brush.color := ActivePixelColor;
  Form_Graphic.SetBrushColor(ActivePixelColor);
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_NoneClick(Sender: TObject);
begin
  ColorSelect(tNone.Colorvalue);
//  ActivePixelColor := ByteReverseOrder32(tNone.Colorvalue); // black
//  Shape_EditColor.Brush.color := ActivePixelColor;
//  SetBrushColor(ActivePixelColor);
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_Color0Click(Sender: TObject);
begin
  case Graphic_mode of
    gmForest :  ColorSelect(tConiferous.Colorvalue);
    gmThermal : ColorSelect(tGreenFields.Colorvalue);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_Color1Click(Sender: TObject);
begin
  case Graphic_mode of
    gmForest :  ColorSelect(tDeciduous.Colorvalue);
    gmThermal : ColorSelect(tYellowFields.Colorvalue);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_Color2Click(Sender: TObject);
begin
  case Graphic_mode of
    gmForest :  ColorSelect(tBoth.Colorvalue);
    gmThermal : ColorSelect(tDarkFields.Colorvalue);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_ExclusionClick(Sender: TObject);
begin
  ColorSelect(tExclusion.Colorvalue);
//  ActivePixelColor := ByteReverseOrder32(tExclusion.Colorvalue);  // white
//  Shape_EditColor.Brush.color := ActivePixelColor;
//  SetBrushColor(ActivePixelColor);
end;

procedure Apply_ForestGrid_ToMask; forward;
//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_Tool_0Click(Sender: TObject);
var
  Path : string;
  FileName : string;
begin
  case Graphic_mode of
    gmForest : begin
      // import b forest file
      path := wWorkingPath+'\Terragen\ForestMaps';
      u_BMP.BMPfolder := wWorkingPath+'\Terragen\ForestMaps';
      FileName := '\b'+ExtractFileName(mFileName);
      // try reading b file
      if (ForestBitmap_To_ForestGrid(FileName, False)) then begin
        FileName := '\s'+ExtractFileName(mFileName);
        // try reading s file and combine with b file
        if (ForestBitmap_To_ForestGrid(FileName, True)) then begin
          // update the bitmap with new mask
          Apply_ForestGrid_ToMask;
        end;
      end;
    end;
    gmThermal : begin
      ColorSelect(tWater.Colorvalue);
//    ActivePixelColor := ByteReverseOrder32(tWater.Colorvalue);  // blue
//    Shape_EditColor.Brush.color := ActivePixelColor;
//    SetBrushColor(ActivePixelColor);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_SwampClick(Sender: TObject);
begin
  ColorSelect(tSwamp.Colorvalue);
//  ActivePixelColor := ByteReverseOrder32(tSwamp.Colorvalue);  // yellowish
//  Shape_EditColor.Brush.color := ActivePixelColor;
//  SetBrushColor(ActivePixelColor);
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_SandClick(Sender: TObject);
begin
  ColorSelect(tSand.Colorvalue);
//  ActivePixelColor := ByteReverseOrder32(tSand.Colorvalue);  // lightBrown
//  Shape_EditColor.Brush.color := ActivePixelColor;
//  SetBrushColor(ActivePixelColor);
end;

Procedure Extract_ForestGrid_FromMask; forward;
//---------------------------------------------------------------------------
procedure TForm_Graphic.SaveMaskFile(FileName : string);
var
  SelectionFlag : boolean;
  fName, fPath : String;

begin
  if (CheckBox_Selection.Checked) then begin
    SelectionFlag := true;
    ShowSelection(Nil); // temporarily turn off selection box
  end else begin
    SelectionFlag := false;
  end;

  Image_Mask.Picture.SaveToFile(FileName);

  // now also save V2 b and s files
  Extract_ForestGrid_FromMask;
  fName := ExtractFileName(Filename);
  fPath := ExtractFileDir(Filename);
  ForestGrid_To_256Color_Bitmap(tColumns, fDeciduous, fPath+'\b'+fName);
  ForestGrid_To_256Color_Bitmap(tColumns, fConiferous, fPath+'\s'+fName);

  if (SelectionFlag) then begin
    ShowSelection(Nil);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_SaveClick(Sender: TObject);
begin
  case Graphic_mode of
    gmForest : begin
      SaveMaskFile(mFileName);
    end;
    gmThermal : begin
      SaveMaskFile(thFileName);
    end;
  end;
end;

//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_PenClick(Sender: TObject);
begin
  EditTool := t_Pen;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_LineClick(Sender: TObject);
begin
  EditTool := t_Line;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_FloodClick(Sender: TObject);
begin
  EditTool := t_Flood;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_BrushClick(Sender: TObject);
begin
  EditTool := t_Brush;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_PickColorClick(Sender: TObject);
begin
  EditTool := t_Pick;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_WindowClick(Sender: TObject);
begin
  EditTool := t_Select;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_RectangleClick(Sender: TObject);
begin
  EditTool := t_Rectangle;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_EllipseClick(Sender: TObject);
begin
  EditTool := t_Ellipse;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_ReplaceClick(Sender: TObject);
begin
  EditTool := t_Replace;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_SwellClick(Sender: TObject);
begin
  EditTool := t_Swell;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_ShrinkClick(Sender: TObject);
begin
  EditTool := t_Shrink;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_ApplyFilterClick(Sender: TObject);
var
  Image : TImage;
begin
  Image_Mask.Transparent := false; // !!! if transparent using scanline to overwrite a pixel will not work !!!
  if (CheckBox_Alternate.checked AND AlternateView) then begin
    Image := Form_Graphic.Image_Alternate;
  end else begin
    Image := Form_Graphic.Image_Tile;
  end;
  ApplyFilter(Image,ActivePixelColor);
  Image_Mask.Transparent := true;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_MatchClick(Sender: TObject);
var
  Image : TImage;
begin
  if (CheckBox_Alternate.checked AND AlternateView) then begin
    Image := Form_Graphic.Image_Alternate;
  end else begin
    Image := Form_Graphic.Image_Tile;
  end;
  if Form_Filter.RadioButton_RGB.Checked = true then begin
    MatchRange_RGB(Image,Shape_Pick.Brush.Color);
  end else begin
    if Form_Filter.RadioButton_HSV.Checked = true then begin
      MatchRange_HSV(Image,Shape_Pick.Brush.Color);
    end else begin
      MatchRange_CIE(Image,Shape_Pick.Brush.Color);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_PixelizeClick(Sender: TObject);
var
  Image : TImage;
begin
  if (CheckBox_Alternate.checked AND AlternateView) then begin
    Image := Form_Graphic.Image_Alternate;
  end else begin
    Image := Form_Graphic.Image_Tile;
  end;
  Pixelize(Image,MatchHighlightColor);
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_ResetFilterClick(Sender: TObject);
begin
  ProgressBar_Status.Max := 100;     //On Progress doesn't trigger
  ProgressBar_Status.Position := 50; //use this for now
  Image_Tile.Picture.LoadFromFile(tFileName);
  Image_Tile.Picture.Bitmap.TransparentColor := tNone.ColorValue{clBlack};
  if CheckBox_Alternate.enabled then begin
    Image_Alternate.Picture.LoadFromFile(bFileName);
    Image_Alternate.Picture.Bitmap.TransparentColor := tNone.ColorValue{clBlack};
  end;
  ProgressBar_Status.Position := 0;
  ColorsReduced := false;
  Image_PaletteClick(Sender);
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_OptionsClick(Sender: TObject);
begin
  Form_Filter.ShowModal;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.CheckBox_SelectionClick(Sender: TObject);
begin
  ShowSelection(Sender);
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_ConvolveClick(Sender: TObject);
begin
  u_Convolve.ProgressBar_Status := ProgressBar_Status;
  u_Convolve.StringGrid1 := Unit_Filter.Form_Filter.StringGrid_Matrix;
  u_Convolve.Edit_Divisor := Unit_Filter.Form_Filter.Edit_MatrixDivisor;
  u_Convolve.Convolve_Matrix(Image_Tile.Picture.Bitmap);
  Image_Tile.Refresh;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_CannyClick(Sender: TObject);
var
  tBufr : TBitmap;
begin
//  tBufr := gaussian(Image_Tile.Picture.Bitmap, 0.2);
  Prefilter := Form_Filter.Checkbox_CannyGaussian.Checked;
  sigma := StrToFloat(Form_Filter.Edit_Sigma.Text);
  thrlow := StrToFloat(Form_Filter.Edit_LowThreshold.Text);
  thrhigh := StrToFloat(Form_Filter.Edit_HighThreshold.Text);
  tBufr := canny(Image_Tile.Picture.Bitmap);
  Image_Tile.Picture.Assign(tBufr);
  Image_Tile.Canvas.Draw(0,0,tBufr);
  tBufr.Free;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_ColorReduceClick(Sender: TObject);
begin
  {Image_Tile.Picture.Bitmap :=} ReduceColors(Image_Tile.Picture.Bitmap,
    StrToInt(Form_Filter.Edit_Maxcolors.Text));
  Image_PaletteClick(Sender);
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.ToggleMaskView(Sender: TObject);
begin
  if (MaskView) then begin
    Image_Mask.Visible := false;
    MaskView := false;
  end else begin
    Image_Mask.Visible := true;
    MaskView := true;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.ToggleAlternateView(Sender: TObject);
begin
  if CheckBox_Alternate.enabled then begin
      if (AlternateView) then begin
        Image_Alternate.Visible := false;
        AlternateView := false;
      end else begin
        Image_Alternate.Visible := true;
        AlternateView := true;
      end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.LoadImageView(Sender: TObject);
begin
        if FileExists(atFileName) then begin
          ProgressBar_Status.Max := 100;     //On Progress doesn't trigger
          ProgressBar_Status.Position := 50; //use this for now
          Image_Tile.Picture.LoadFromFile(atFileName);
          ProgressBar_Status.Position := 0;
          ColorsReduced := false;
          Image_PaletteClick(Sender);
        end;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.SaveImageView(Sender: TObject);
begin
        ProgressBar_Status.Max := 100;     //On Progress doesn't trigger
        ProgressBar_Status.Position := 50; //use this for now
        Image_Tile.Picture.SaveToFile(atFileName);
        ProgressBar_Status.Position := 0;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if (Gui_State = IdleScreen) then begin
  case Key of
    VK_ESCAPE: begin
      EditTool := t_None;
    end;

    VK_UP: begin
    end;

    VK_DOWN: begin
    end;

    VK_LEFT: begin
    end;

    VK_RIGHT: begin
    end;

    $BB: begin // +
    end;

    $BD: begin //-
    end;

    $BE: begin //>
    end;

    $BC: begin //<
    end;
{
    ord('B'), ord('b'): begin
      if (ssAlt in Shift) then begin
        if FileExists(bFileName) then begin
          ProgressBar_Status.Max := 100;     //On Progress doesn't trigger
          ProgressBar_Status.Position := 50; //use this for now
          Image_Mask.Stretch := true;
          Image_Tile.Picture.LoadFromFile(bFileName);
          ProgressBar_Status.Position := 0;
          ColorsReduced := false;
          Image_PaletteClick(Sender);
        end;
      end;
    end;
}
    ord('L'), ord('l'): begin
      if (ssAlt in Shift) then begin
        LoadImageView(nil);
      end;
    end;

    ord('S'), ord('s'): begin
      if (ssAlt in Shift) then begin
        SaveImageView(nil);
      end;
    end;

    ord('V'), ord('v'): begin
      ToggleMaskView(nil);
    end;

    ord('A'), ord('a'): begin
      ToggleAlternateView(nil);
    end;

    else begin
    end;

  end;
 end;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.FormCreate(Sender: TObject);
begin
  // added scrollbar events
  ScrollBox_Image.OnScrollVert := MyScrollVert;
  ScrollBox_Image.OnScrollHorz := MyScrollHorz;

  Image_Tile.Transparent := False;
  Image_Alternate.Transparent := True;
  Image_Mask.Transparent := True;
//  ActivePixelColor := ByteReverseOrder32(tNone.Colorvalue); // none by default
  ActivePixelColor := ByteSwapColor(tNone.Colorvalue);//convert from ColorConvert to Tcolor
  BrushBitmap := TBitmap.Create;
  with BrushBitmap do begin
    Height := 8;
    Width := 8;
    Transparent := True;
    TransParentColor := $00010101; // so that black can be used to erase
    TransparentMode := tmFixed;
    ComboBox_BrushSize.ItemIndex := 0{ComboBox_BrushSize.Items.Count-1};
    ComboBox_BrushType.ItemIndex := 1;
    SetBrushColor(ActivePixelColor);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  DeleteFile('tmp.bmp');
  Checkbox_Selection.Checked := false; //for next form open
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.FormDestroy(Sender: TObject);
begin
  BrushBitmap.Free;
  Bitmap_Save.Free;
end;

{----------------------------------------------------------------------------}
function ForestMajority(y, x : integer): integer;
var
  x_fm, y_fm : integer;
begin
  Result := 0;
  for y_fm := 0 to ForestResolution-1 do begin
    for x_fm := 0 to ForestResolution-1 do begin
      if (ForestGrid[y*ForestResolution+y_fm,x*ForestResolution+x_fm] <> 0) then INC(result);
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure ConvertForestMask(hDeciduous,hConiferous,hDefault : byte);
var
  x, y : integer;

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
function ForestAverage(y, x : integer): integer;
var
  x_fa, y_fa : integer;

begin
  Result := 0;
  for y_fa := 0 to ForestResolution-1 do begin
    for x_fa := 0 to ForestResolution-1 do begin
      case ForestGrid[y*ForestResolution+y_fa,x*ForestResolution+x_fa] of
        0: begin
          INC(Result,{200}hDefault); // default 78%
        end;
        1: begin
          INC(Result,{102}hDeciduous); //deciduous 40%
        end;
        2: begin
          INC(Result,{128}hConiferous); //coniferous 50%
        end;
        3: begin
          INC(Result,{115}(hDeciduous+hConiferous) div 2); //both 45%
        end;
      end;
    end;
  end;
end;

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
begin
  for y := 0 to tRows-1 do begin
    for x := 0 to tColumns-1 do begin
      if (ForestMajority(y,x) >= (ForestResolution*ForestResolution div 2)) then begin  // at least 50% forest ?
        ThermalGrid[y,x] := ForestAverage(y,x) div (ForestResolution*ForestResolution);
      end else begin
        ThermalGrid[y,x] := hDefault;
      end;
    end;
  end;
end;

// for thermal Mask
{----------------------------------------------------------------------------}
Procedure ApplyForestMask;
var
  NoColor : ColorConvert;
  mColor  : ColorConvert;
  x, y : integer;
  pScanLine: pRGBArray;

begin
  Screen.Cursor := crHourGlass;  // Let user know we're busy...
  Form_Graphic.Image_Mask.Visible := false; //stops display update while processing
  // !!! if transparent, mask doesn't get updated with scanline !!!
  Form_Graphic.Image_Mask.Transparent := false; // must be done
  Form_Graphic.ProgressBar_Status.Max := tRows;
  NoColor := tNone;
  mColor := tExclusion;
  for y := 0 to tRows-1 do begin
    pScanLine := Form_Graphic.Image_Mask.Picture.Bitmap.ScanLine[y];
    for x := 0 to tColumns-1 do begin
      // check for overwrite or pixel = tNone
      if ((Form_Graphic.CheckBox_Overwrite.checked) OR
//        (pScanLine^[x] = NoColor.cRGB)) then begin // can't compare like this
           CompareMem(@pScanLine^[x],@NoColor.cRGB,3) ) then begin
        if (ForestMajority(y,x) >= ForestResolution*ForestResolution div 2) then begin  // 50 %
          pScanLine^[x] := mColor.cRGB;
        end else begin
        end;
      end;
    end;
    Form_Graphic.ProgressBar_Status.StepIt;
    Application.ProcessMessages;
  end;
  if (MaskView) then begin
    Form_Graphic.Image_Mask.Visible := true;
//    Form_Graphic.Image_Mask.Refresh;
  end;
  Form_Graphic.ProgressBar_Status.Position := 0;
  Form_Graphic.Image_Mask.Transparent := true; // restore transparency
  Screen.Cursor := crDefault;  // no longer busy
end;

{----------------------------------------------------------------------------}
Procedure Apply_ForestGrid_ToMask;
var
//  NoColor : ColorConvert;
  mColor  : ColorConvert;
  x, y : integer;
  pScanLine: pRGBArray;

begin
  Screen.Cursor := crHourGlass;  // Let user know we're busy...
  Form_Graphic.Image_Mask.Visible := false; //stops display update while processing
  // !!! if transparent, mask doesn't get updated with scanline !!!
  Form_Graphic.Image_Mask.Transparent := false; // must be done
  Form_Graphic.ProgressBar_Status.Max := tRows*ForestResolution;
//  NoColor := tNone;
  for y := 0 to tRows*ForestResolution-1 do begin
    pScanLine := Form_Graphic.Image_Mask.Picture.Bitmap.ScanLine[y];
    for x := 0 to tColumns*ForestResolution-1 do begin
      // check for overwrite or pixel = tNone
      if ((Form_Graphic.CheckBox_Overwrite.checked) OR
//        (pScanLine^[x] = NoColor.cRGB)) then begin // can't compare like this
//           CompareMem(@pScanLine^[x],@NoColor.cRGB,3) ) then begin
           CompareMem(@pScanLine^[x],@tNone.cRGB,3) ) then begin
        case ForestGrid[y,x] of
          1: begin
            mColor := tDeciduous;
          end;
          2: begin
            mColor := tConiferous;
//            mColor := tConiferous_LE;
          end;
          3: begin
            mColor := tBoth;
          end;
          else begin
            mColor := tNone; // can overwrite to tNone
          end;
        end;
        pScanLine^[x] := mColor.cRGB;
      end;
    end;
    Form_Graphic.ProgressBar_Status.StepIt;
    Application.ProcessMessages;
  end;
  if (MaskView) then begin
    Form_Graphic.Image_Mask.Visible := true;
//    Form_Graphic.Image_Mask.Refresh;
  end;
  Form_Graphic.ProgressBar_Status.Position := 0;
  Form_Graphic.Image_Mask.Transparent := true; // restore transparency
  Screen.Cursor := crDefault;  // no longer busy
end;

{----------------------------------------------------------------------------}
Procedure Extract_ForestGrid_FromMask;
var
  x, y : integer;
  pScanLine: pRGBArray;

begin
  Screen.Cursor := crHourGlass;  // Let user know we're busy...
  Form_Graphic.Image_Mask.Visible := false; //stops display update while processing
  // !!! if transparent, mask doesn't get updated with scanline !!!
  Form_Graphic.Image_Mask.Transparent := false; // must be done
  Form_Graphic.ProgressBar_Status.Max := tRows*ForestResolution;
//  NoColor := tNone;
  // make sure grid is blank to start
  ClearForestGrid;
  for y := 0 to tRows*ForestResolution-1 do begin
    pScanLine := Form_Graphic.Image_Mask.Picture.Bitmap.ScanLine[y];
    for x := 0 to tColumns*ForestResolution-1 do begin
      if ( CompareMem(@pScanLine^[x],@tDeciduous.cRGB,3) ) then begin
        ForestGrid[y,x] := 1;
      end else begin
        if ( CompareMem(@pScanLine^[x],@tConiferous.cRGB,3) ) then begin
          ForestGrid[y,x] := 2;
        end else begin
          if ( CompareMem(@pScanLine^[x],@tBoth.cRGB,3) ) then begin
            ForestGrid[y,x] := 3;
          end;
        end;
      end;
    end;
    Form_Graphic.ProgressBar_Status.StepIt;
    Application.ProcessMessages;
  end;
  if (MaskView) then begin
    Form_Graphic.Image_Mask.Visible := true;
//    Form_Graphic.Image_Mask.Refresh;
  end;
  Form_Graphic.ProgressBar_Status.Position := 0;
  Form_Graphic.Image_Mask.Transparent := true; // restore transparency
  Screen.Cursor := crDefault;  // no longer busy
end;

{----------------------------------------------------------------------------}
Procedure ApplyThermalMask; // find scale. forest 512x512 or 2048x2048
var
  NoColor : ColorConvert;
  mColor  : ColorConvert;
  x, y : integer;
  pScanLine: pRGBArray;
//  k, m : integer;
  scale : integer;

begin
  Screen.Cursor := crHourGlass;  // Let user know we're busy...
  with Form_Graphic.Image_Mask do begin
    Scale := Picture.Bitmap.Width div tColumns; // ratio between thermal mask and forest mask
    Visible := false; //stops display update while processing
    // !!! if transparent, mask doesn't get updated with scanline !!!
    Transparent := false; // must be done
    Form_Graphic.ProgressBar_Status.Max := tRows*ForestResolution;
    mColor := tExclusion;
    NoColor := tNone;
    for y := 0 to tRows*Scale-1 do begin
      pScanLine := Picture.Bitmap.ScanLine[y];
      for x := 0 to tColumns*Scale-1 do begin

        // check for overwrite or pixel = tNone
        if ((Form_Graphic.CheckBox_Overwrite.checked) OR
//          (pScanLine^[x] = NoColor.cRGB)) then begin // can't compare like this
          CompareMem(@pScanLine^[x],@NoColor.cRGB,3) ) then begin
          // all but 8 which is background default i.e. no specific thermal value
          if (ThermalGrid[y div scale, x div scale] <> 8 {Heating[8]}) then begin
            pScanLine^[x] := mColor.cRGB;
          end;
        end;
      end;
      Form_Graphic.ProgressBar_Status.StepIt;
      Application.ProcessMessages;
    end;
    if (MaskView) then begin
      Visible := true;
//      Refresh;
    end;
    Form_Graphic.ProgressBar_Status.Position := 0;
    Transparent := true; // restore transparency
    Screen.Cursor := crDefault;  // no longer busy
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_ImportClick(Sender: TObject);
begin
  if (Graphic_Mode = gmThermal) then begin
    u_BMP.BMPfolder := ExtractFilepath(mFileName);
    ForestBitmap_To_ForestGrid(ExtractFilename(mFileName), False);
    ApplyForestMask;
  end else begin
    ClearThermalGrid;
    u_BMP.BMPfolder := ExtractFilepath(thFileName);
    ReadThermalBitmapTileIndexes(ExtractFilename(thFileName));
    ApplyThermalMask;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Edit_PixelizeRangeExit(Sender: TObject);
begin
  PercentRangeCheck(Edit_PixelizeRange);
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Image_TileProgress(Sender: TObject;
  Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean;
  const R: TRect; const Msg: String);
begin
  case stage of
    psStarting: begin
      ProgressBar_Status.Max := 100;
      ProgressBar_Status.Position := PercentDone;
    end;
    psRunning: begin
      ProgressBar_Status.Position := PercentDone;
    end;
    psEnding: begin
      ProgressBar_Status.Position := 0;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Image_PaletteClick(Sender: TObject);
var
  i, j : integer;
  ColorRows, ColorColumns : integer;
  x : real;

begin
  with Image_Palette do begin
    if (ColorsReduced) then begin
      ColorRows := Height div 16;
      x := NumColors / ColorRows;
      ColorColumns := trunc(x);
      if (Frac(x) <> 0) then begin
        INC(ColorColumns);
      end;
      Canvas.Brush.Color := clBlack;
      Width := ColorColumns*16+1;
      Picture.Bitmap.Height := Height;
      Picture.Bitmap.Width := Width;
      Canvas.FillRect(rect(0,0,Width,Height));
      for i := 0 to ColorRows-1 do begin
        for j := 0 to ColorColumns-1 do begin
          if (i*ColorColumns+j < NumColors) then begin
            Canvas.Brush.Color := ByteSwapColor(Tcolor(RGBQuadArray[i*ColorColumns+j]));
            Picture.Bitmap.Canvas.FillRect(rect(j*16+1,i*16,j*16+1+15,i*16+15));
          end;
        end;
      end;
      Visible := true;
    end else begin
      Visible := false;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.ScrollBox_ImageConstrainedResize(Sender: TObject;
  var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer);
begin
{  MinWidth := 256;
  MinHeight := 500;
//  MinWidth := clientWidth;
//  MinHeight := clientWidth;
  MaxWidth := ScrollBox_Image.HorzScrollBar.Range;
  MaxHeight := ScrollBox_Image.VertScrollBar.Range;
}end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.FormActivate(Sender: TObject);
begin
  Screen.Cursor := crDefault;
  EditTool := t_None;
  MaskView:= true;
  Image_Mask.Visible := true;
  Image_Mask.Cursor := crDefault;
  BitmapSave(Image_Mask.Picture.Bitmap); //allow for undo - intially
  if CheckBox_Alternate.enabled then begin
    AlternateView:= true;
    Image_Alternate.Visible := true;
  end else begin
    AlternateView:= false;
    Image_Alternate.Visible := false;
  end;
  Image_Alternate.Cursor := crDefault;
end;

//---------------------------------------------------------------------------
Procedure ReCentre;
begin
  with Form_Graphic do begin
    ScrollBox_Image.HorzScrollBar.Position := trunc(cX *
      (ScrollBox_Image.HorzScrollBar.Range)-(ScrollBox_Image.ClientWidth div 2));
    ScrollBox_Image.VertScrollBar.Position := trunc(cY *
      (ScrollBox_Image.VertScrollBar.Range)-ScrollBox_Image.ClientHeight div 2);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_ZoomInClick(Sender: TObject);
begin
  // temporarily turn off other images
  if (MaskView) then begin
    Image_Mask.Visible := false;
  end;
  if (AlternateView) then begin
    Image_Alternate.Visible := false;
  end;
  // allow re-size of image - picture stays the same
  Image_Tile.Align := alNone;
  Image_Tile.AutoSize := false;
  Image_Tile.Stretch := true;
  // scale image by 1.5
//  zoomScale := zoomScale / 1.5;
  zoomScale := zoomScale * Image_Tile.Width; // make more exact using before and after
//  Image_Tile.Width := Image_Tile.Width + Image_Tile.Width div 2;
//  Image_Tile.Height := Image_Tile.Height + Image_Tile.Height div 2;
  Image_Tile.Width := round(Image_Tile.Width * 1.5);
  Image_Tile.Height := round(Image_Tile.Height * 1.5);
  zoomScale := zoomScale / Image_Tile.Width;
  // now adjust the scrollBox accordingly
  ScrollBox_Image.HorzScrollBar.Range := Image_Tile.Width;
  ScrollBox_Image.VertScrollBar.Range := Image_Tile.Height;
  // and also adjust the mask and alternate images
  Image_Mask.Width := Image_Tile.Width;
  Image_Mask.Height := Image_Tile.Height;
  Image_Alternate.Width := Image_Tile.Width;
  Image_Alternate.Height := Image_Tile.Height;
  // now finally refresh
//  Image_Mask.Invalidate;    // does not work. visible off/on instead
//  Image_Alternate.Invalidate;
//  Image_Mask.Refresh;       // does not work. visible off/on instead
//  Image_Alternate.Refresh;
  if (MaskView) then begin
    Image_Mask.Visible := true;
  end;
  if (AlternateView) then begin
    Image_Alternate.Visible := true;
  end;
  ReCentre;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_ZoomOutClick(Sender: TObject);
begin
  // temporarily turn off other images
  if (MaskView) then begin
    Image_Mask.Visible := false;
  end;
  if (AlternateView) then begin
    Image_Alternate.Visible := false;
  end;
  // allow re-size of image - picture stays the same
  Image_Tile.Align := alNone;
  Image_Tile.AutoSize := false;
  Image_Tile.Stretch := true;
  // scale image by 3/4
//  zoomScale := zoomScale / 0.75;
  zoomScale := zoomScale * Image_Tile.Width; // make more exact using before and after
//  Image_Tile.Width := Image_Tile.Width - Image_Tile.Width div 4;
//  Image_Tile.Height := Image_Tile.Height - Image_Tile.Height div 4;
  Image_Tile.Width := round(Image_Tile.Width / 1.5);
  Image_Tile.Height := round(Image_Tile.Height / 1.5);
  zoomScale := zoomScale / Image_Tile.Width;
  // now adjust the scrollBox accordingly
  ScrollBox_Image.HorzScrollBar.Range := Image_Tile.Width;
  ScrollBox_Image.VertScrollBar.Range := Image_Tile.Height;
  // and also adjust the mask and alternate images
  Image_Mask.Width := Image_Tile.Width;
  Image_Mask.Height := Image_Tile.Height;
  Image_Alternate.Width := Image_Tile.Width;
  Image_Alternate.Height := Image_Tile.Height;
  // now finally refresh
//  Image_Mask.Invalidate;    // does not work. visible off/on instead
//  Image_Alternate.Invalidate;
//  Image_Mask.Refresh;       // does not work. visible off/on instead
//  Image_Alternate.Refresh;
  if (MaskView) then begin
    Image_Mask.Visible := true;
  end;
  if (AlternateView) then begin
    Image_Alternate.Visible := true;
  end;
  ReCentre;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_Save_TIFClick(Sender: TObject);
var
  Path : string;
  FileName : string;
begin
  Path := wWorkingPath+'\Terragen\Textures_DetectTree\response_tiles';
  ForceDirectories(Path);
  FileName := ChangeFileExt(ExtractFilename(mFileName),'.tif');
  Save_24bit_Image_To_8bit_Tiff(Image_Mask, Path+'\'+FileName);
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.ScrollBox_ImageResize(Sender: TObject);
begin
  ReCentre;
end;

//---------------------------------------------------------------------------
procedure TForm_Graphic.Button_HelpClick(Sender: TObject);
begin
  Form_Help.ShowHelp('Forest-Thermal.hlp.txt',
    Self.Left + ScrollBox_Image.left + 8,
    Self.Top + ScrollBox_Image.Top + 30);
end;

//---------------------------------------------------------------------------
begin
  GUI_State := IdleScreen;
  //create a bitmap to be able to "undo"
  BitMap_Save := TBitMap.Create;

end.

//--- End of File -----------------------------------------------------------


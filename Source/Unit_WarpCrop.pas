{
 * Unit_WarpCrop.pas
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
unit Unit_WarpCrop;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls;

//---------------------------------------------------------------------------
// for compile options
{$I Define.pas}

type
  Warp_Type = (Warp3, Warp4);
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

  TForm_WarpCrop = class(TForm)
    GroupBox_ObjectPlace: TGroupBox;
    GroupBox_Object: TGroupBox;
    Label_Select: TLabel;
    Label_File: TLabel;
    Label_FileName: TLabel;
    Label_TL: TLabel;
    Label_TR: TLabel;
    Label_BR: TLabel;
    Label_BL: TLabel;
    Label_PK: TLabel;
    Label_Coords: TLabel;
    Edit_FileName: TEdit;
    Edit_TL_X: TEdit;
    Edit_TL_Y: TEdit;
    Edit_TR_X: TEdit;
    Edit_TR_Y: TEdit;
    Edit_BR_X: TEdit;
    Edit_BR_Y: TEdit;
    Edit_BL_X: TEdit;
    Edit_BL_Y: TEdit;
    Edit_PK_X: TEdit;
    Edit_PK_Y: TEdit;
    ScrollBox_Image: TScrollBox;
    Image_Tile: TImage;
    OpenDialog_FileName: TOpenDialog;
    SaveDialog_FileName: TSaveDialog;
    Button_Open: TButton;
    Button_Exit: TButton;
    Button_ZoomIn: TButton;
    Button_ZoomOut: TButton;
    Button_ZoomReset: TButton;
    Button_MakeBatch: TButton;
    Button_ExecuteBatch: TButton;
    Button_Save_Parameters: TButton;
    Button_Load_Parameters: TButton;
    UpDown_Horiz: TUpDown;
    UpDown_Vert: TUpDown;
    PaintBox1: TPaintBox;
    procedure Button_OpenClick(Sender: TObject);
    procedure Button_Load_ParametersClick(Sender: TObject);
    procedure Button_Save_ParametersClick(Sender: TObject);
    procedure Button_ExitClick(Sender: TObject);
    procedure Button_MakeBatchClick(Sender: TObject);
    procedure Button_ZoomInClick(Sender: TObject);
    procedure Button_ZoomOutClick(Sender: TObject);
    procedure Button_ZoomResetClick(Sender: TObject);
    procedure Edit_TL_XExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure ShowCoord(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure Image_TileClick(Sender: TObject);
    procedure Image_TileMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image_TileMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ZoomRestore(Sender: TObject);
    procedure ScrollBox_ImageResize(Sender: TObject);
    procedure Edit_TL_XDblClick(Sender: TObject);
    procedure Edit_TR_XDblClick(Sender: TObject);
    procedure Edit_BR_XDblClick(Sender: TObject);
    procedure Edit_BL_XDblClick(Sender: TObject);
    procedure Edit_PK_XDblClick(Sender: TObject);
    procedure Edit_TL_XKeyPress(Sender: TObject; var Key: Char);
    procedure UpDown_HorizMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure UpDown_VertMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button_ExecuteBatchClick(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    { Private declarations }
    function LoadBitmap(FileName : string) : boolean;
    procedure MyScrollHorz(Sender: TObject);
    procedure MyScrollVert(Sender: TObject);
    procedure MakeWarpBatchFile(FilePath, FileName : string; Warp3_4 : Warp_Type);
    procedure DrawObjects(Sender: TObject);
    function  Validate(Sender: TObject) : boolean;
  public
    { Public declarations }
    procedure Initialize(Sender: TObject);
  end;

var
  Form_WarpCrop : TForm_WarpCrop;

  Memo_Message : TMemo;  // external TMemo for messages
  ObjFolder : string;
  ObjFolderOpen : boolean;

//---------------------------------------------------------------------------
implementation

{$R *.DFM}

uses
  ClipBrd,
  Unit_Main, Unit_Graphics, Unit_Coords, Unit_Utilities,
  u_Util, u_TileList, u_Object, u_SceneryHDR, u_VectorXY, u_BMP, u_DXT;

var
  GUI_State : (IdleScreen, SelectScreen, ScrollScreen, CancelScreen);
  ItemIndex : integer;
  ObjectsChanged : boolean;
  BitmapAvail : boolean;
//  opX, opY, opRange :double;
  opZoomScale : double;
  cX, cY : double;
  Parameter_FileName : string;
  rootFileName : string;
  PAR_File : TextFile;
  BMP_FileName : string;
  coordCorners : (TLX,TLY,TRX,TR_Y,BRX,BRY,BLX,BLY,PKX,PKY);
  FMousePos: TPoint;

// TScrollBox addition
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
procedure TForm_WarpCrop.MyScrollVert(Sender: TObject);
begin
  cY := (ScrollBox_Image.VertScrollBar.Position + (ScrollBox_Image.ClientHeight div 2))
        / (ScrollBox_Image.VertScrollBar.Range);
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.MyScrollHorz(Sender: TObject);
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
  with Form_WarpCrop.Image_Tile do begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(rect(0,0,Width,Height));
  end;
end;

//---------------------------------------------------------------------------
Procedure Object_Change_Show(Changed, Show : Boolean);
begin
  ObjectsChanged := Changed;
  Form_WarpCrop.Button_Save_Parameters.enabled := Changed;
  if (Show) then begin
//    Form_WarpCrop.DrawObjects(nil);
    Form_WarpCrop.PaintBox1.RePaint;
  end;
end;

//---------------------------------------------------------------------------
Procedure TForm_WarpCrop.Initialize(Sender: TObject);
begin
  opZoomScale := 1.0;
  // flag no change, no show
  Object_Change_Show(False, False);

  // blank to start
  Image_Tile_Clear;

  // also clear input boxes
  Edit_FileName.Clear;
  Edit_TL_X.Clear;
  Edit_TL_Y.Clear;
  Edit_TR_X.Clear;
  Edit_TR_Y.Clear;
  Edit_BR_X.Clear;
  Edit_BR_Y.Clear;
  Edit_BL_X.Clear;
  Edit_BL_Y.Clear;
  Edit_PK_X.Clear;
  Edit_PK_Y.Clear;

  // select first in sequence
  coordCorners := TLX;
end;

//---------------------------------------------------------------------------
var
  TriangleCorners : TCoordXY_Array;
  tVertexCount : integer;
  QuadCorners : TCoordXY_Array;
  qVertexCount : integer;
  TriangleFound, QuadFound : boolean;

//---------------------------------------------------------------------------
function Convert(Str : string; var value : integer) : boolean;
var
  ErrorCode : integer;
begin
{
  try
    value := strToInt(Str);
    result := true;
  except
    result := false;
  end;
}
  val(Str, value, ErrorCode);
  if (ErrorCode = 0) then begin
    result := true;
  end else begin
    result := false;
  end;
end;

//---------------------------------------------------------------------------
function TForm_WarpCrop.Validate(Sender: TObject) : boolean;
var
  value : Integer;

//---------------------------------------------------------------------------
begin
  result := false; // to start
  // look for quad
  QuadFound := false; // assume to start
  repeat // dummy use to be able to use 'continue'
    setlength(QuadCorners,4);
    qVertexCount := 0;

    if Convert(Edit_TL_X.Text, value) then begin
      QuadCorners[0].X := value;
    end else begin
      continue;
    end;
    if Convert(Edit_TL_Y.Text, value) then begin
      QuadCorners[0].Y := value;
      qVertexCount := 1;
      result := true; // at least one found
    end else begin
      continue;
    end;

    if Convert(Edit_TR_X.Text, value) then begin
      QuadCorners[1].X := value;
    end else begin
      continue;
    end;
    if Convert(Edit_TR_Y.Text, value) then begin
      QuadCorners[1].Y := value;
      qVertexCount := 2;
    end else begin
      continue;
    end;

    if Convert(Edit_BR_X.Text, value) then begin
      QuadCorners[2].X := value;
    end else begin
      continue;
    end;
    if Convert(Edit_BR_Y.Text, value) then begin
      QuadCorners[2].Y := value;
      qVertexCount := 3;
    end else begin
      continue;
    end;

    if Convert(Edit_BL_X.Text, value) then begin
      QuadCorners[3].X := value;
    end else begin
      continue;
    end;
    if Convert(Edit_BL_Y.Text, value) then begin
      QuadCorners[3].Y := value;
      qVertexCount := 4;
      QuadFound := true;
    end else begin
      continue;
    end;
  until (true); // end dummy repeat

  // look for triangle
  TriangleFound := false; // assume to start
  repeat // dummy use to be able to use 'continue'
    setlength(TriangleCorners,4);
    tVertexCount := 0;

    if Convert(Edit_TL_X.Text, value) then begin
      TriangleCorners[0].X := value;
    end else begin
      continue;
    end;
    if Convert(Edit_TL_Y.Text, value) then begin
      TriangleCorners[0].Y := value;
      tVertexCount := 1;
      result := true; // at least one found
    end else begin
      continue;
    end;

    if Convert(Edit_TR_X.Text, value) then begin
      TriangleCorners[1].X := value;
    end else begin
      continue;
    end;
    if Convert(Edit_TR_Y.Text, value) then begin
      TriangleCorners[1].Y := value;
      tVertexCount := 2;
    end else begin
      continue;
    end;

    if Convert(Edit_PK_X.Text, value) then begin
      TriangleCorners[2].X := value;
    end else begin
      continue;
    end;
    if Convert(Edit_PK_Y.Text, value) then begin
      TriangleCorners[2].Y := value;
      tVertexCount := 3;
      TriangleFound := true;
    end else begin
      continue;
    end;
  until (true); // end dummy repeat
end;

//---------------------------------------------------------------------------
Procedure TForm_WarpCrop.DrawObjects(Sender: TObject);
var
  i : integer;
  ScaleX , ScaleY : double;

begin
//  ScaleX := 1;               // OK for image_mask
//  ScaleY := 1;
  ScaleX := 1/opZoomScale;     // needed for PaintBox
  ScaleY := 1/opZoomScale;

  // validate, but show anyway
  if (Validate(Sender)) then begin
  end;

//  with Image_Mask do begin
//    Canvas.Brush.Color := Picture.Bitmap.TransparentColor;
//    Canvas.FillRect(rect(0,0,Picture.Width,Picture.Height));
//  end;

  if qVertexCount > 1 then begin
    with PaintBox1 do begin
//      Canvas.Pen.Mode := pmCopy; // needed for pixels[] !
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Width := 1;
      Canvas.Pen.Color := clRed;
      Canvas.MoveTo(round(QuadCorners[0].X * ScaleX), Round(QuadCorners[0].Y * ScaleY));
      for i := 1 to qVertexCount-1 do begin
        Canvas.LineTo(round(QuadCorners[i].X * ScaleX), Round(QuadCorners[i].Y * ScaleY));
      end;
      Canvas.LineTo(round(QuadCorners[0].X * ScaleX), Round(QuadCorners[0].Y * ScaleY));
    end;
  end;

  if tVertexCount > 1 then begin
    with PaintBox1 do begin
//      Canvas.Pen.Mode := pmCopy; // needed for pixels[] !
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Width := 1;
      Canvas.Pen.Color := clRed;
      Canvas.MoveTo(round(TriangleCorners[0].X * ScaleX), Round(TriangleCorners[0].Y * ScaleY));
      for i := 1 to tVertexCount-1 do begin
        Canvas.LineTo(round(TriangleCorners[i].X * ScaleX), Round(TriangleCorners[i].Y * ScaleY));
      end;
      Canvas.LineTo(round(TriangleCorners[0].X * ScaleX), Round(TriangleCorners[0].Y * ScaleY));
    end;
  end;
end;

//---------------------------------------------------------------------------
Procedure ReCentre;
begin
  with Form_WarpCrop do begin
    ScrollBox_Image.HorzScrollBar.Position := trunc(cX *
      (ScrollBox_Image.HorzScrollBar.Range)-ScrollBox_Image.ClientWidth div 2);
    ScrollBox_Image.VertScrollBar.Position := trunc(cY *
      (ScrollBox_Image.VertScrollBar.Range)-ScrollBox_Image.ClientHeight div 2);
  end;
end;

// mouse move
//---------------------------------------------------------------------------
procedure TForm_WarpCrop.ShowCoord(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Horiz, Vert : integer;
begin
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
    // need to correct based on opZoomScale
    Horiz := round(X * opZoomScale);
    Vert  := round(Y * opZoomScale);
    Label_Coords.Caption := format('%d,%d',[Horiz, Vert]);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.Image_TileMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ( (ssCtrl in Shift) AND (Button = mbLeft) ) then begin //scroll bitmap
    GUI_State := ScrollScreen;
    FMousePos.X := X;
    FMousePos.Y := Y;
  end else begin
    if ( (ssShift in Shift) AND (Button = mbLeft) ) then begin //select point
      GUI_State := SelectScreen;
    end else begin
      GUI_State := IdleScreen;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.Image_TileMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TempStr : string;

begin
  if (Button = mbRight) then begin
//    Form_Coords.ShowModal;
  end;

  if (Gui_State = SelectScreen) then begin
    with Object_List[ItemIndex] do begin
      TempStr := Label_Coords.Caption;
      // copy/paste support
      Clipboard.AsText := Label_Coords.Caption; // if needed to paste somewhere
      // update the coordinate
      case coordCorners of
        TLX: begin
          Edit_TL_X.Text := ReadCSV(TempStr);
          Edit_TL_Y.Text := ReadCSV(TempStr);
        end;
        TRX: begin
          Edit_TR_X.Text := ReadCSV(TempStr);
          Edit_TR_Y.Text := ReadCSV(TempStr);
        end;
        BRX: begin
          Edit_BR_X.Text := ReadCSV(TempStr);
          Edit_BR_Y.Text := ReadCSV(TempStr);
        end;
        BLX: begin
          Edit_BL_X.Text := ReadCSV(TempStr);
          Edit_BL_Y.Text := ReadCSV(TempStr);
        end;
        PKX: begin
          Edit_PK_X.Text := ReadCSV(TempStr);
          Edit_PK_Y.Text := ReadCSV(TempStr);
        end;
      end;
      // skip to next coordinate
      if (coordCorners < PKX) then begin
        inc(coordCorners,2);
        label_Select.Top := label_Select.Top +19;
      end else begin
        coordCorners := TLX;
        label_Select.Top := label_TL.Top;
      end;
      // flag change, show
      Object_Change_Show(True, True);
    end;
  end;
  GUI_State := IdleScreen;
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if (ObjectsChanged = true) then begin
    if MessageDlg('Objets changed, Exit anyway ?', mtConfirmation,
        [mbYes, mbNo], 0) = mrNo then begin
      CanClose := False;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.Button_ExitClick(Sender: TObject);
begin
  Close;
end;

// used for all edit boxes
//---------------------------------------------------------------------------
procedure TForm_WarpCrop.Edit_TL_XExit(Sender: TObject);
begin
  // flag change, show
  Object_Change_Show(True, True);
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.FormCreate(Sender: TObject);
begin
  // added scrollbar events
  ScrollBox_Image.OnScrollVert := MyScrollVert;
  ScrollBox_Image.OnScrollHorz := MyScrollHorz;

  Image_tile.Hint := 'Ctrl-Left-Mouse to Pan'#13#10'Shift-Left Mouse to select';

  Initialize(nil);
//  Image_Mask.Picture.Bitmap.Create;
//  Image_Mask.Picture.Bitmap.Width := 512; // for now
//  Image_Mask.Picture.Bitmap.Height := 512;
//  Image_Mask.Transparent := True;
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.FormDestroy(Sender: TObject);
begin
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.Button_ZoomInClick(Sender: TObject);
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
    // and also adjust the mask image
//    Image_Mask.AutoSize := false;
//    Image_Mask.Stretch := true;
//    Image_Mask.Width := Image_Tile.Width;
//    Image_Mask.Height := Image_Tile.Height;
    // and also adjust the PaintBox
    PaintBox1.Width  := Image_Tile.Width;
    PaintBox1.Height := Image_Tile.Height;
    // now refresh
//    CentreObject;
    ReCentre;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.Button_ZoomOutClick(Sender: TObject);
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
    // and also adjust the mask image
//    Image_Mask.AutoSize := false;
//    Image_Mask.Stretch := true;
//    Image_Mask.Width := Image_Tile.Width;
//    Image_Mask.Height := Image_Tile.Height;
    PaintBox1.Width  := Image_Tile.Width;
    PaintBox1.Height := Image_Tile.Height;
    // now refresh
//    CentreObject;
    ReCentre;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.Button_ZoomResetClick(Sender: TObject);
begin
  if (BitmapAvail) then begin
    opZoomScale := 1.0;
    Image_Tile.Width := Image_Tile.Picture.Width;
    Image_Tile.Height := Image_Tile.Picture.Height;
    ScrollBox_Image.HorzScrollBar.Range := Image_Tile.Width;
    ScrollBox_Image.VertScrollBar.Range := Image_Tile.Height;
    // and also adjust the mask image
//    Image_Mask.Width := Image_Tile.Width;
//    Image_Mask.Height := Image_Tile.Height;
    PaintBox1.Width  := Image_Tile.Width;
    PaintBox1.Height := Image_Tile.Height;
    // now refresh
//    CentreObject;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.ZoomRestore(Sender: TObject);
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
    // and also adjust the mask image
//    Image_Mask.AutoSize := false;
//    Image_Mask.Stretch := true;
//    Image_Mask.Width := Image_Tile.Width;
//    Image_Mask.Height := Image_Tile.Height;
    PaintBox1.Width  := Image_Tile.Width;
    PaintBox1.Height := Image_Tile.Height;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.Image_TileClick(Sender: TObject);
begin
  Clipboard.AsText := Label_Coords.Caption;
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.ScrollBox_ImageResize(Sender: TObject);
begin
  Recentre;  // on current centre cX, cY
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    ord('S'), ord('s'): begin
      if (ssCtrl in Shift) then begin
        Button_Save_ParametersClick(Sender);
        key := 0; // so that other components won't see this keypress
      end;
    end;
    else begin
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.Edit_TL_XDblClick(Sender: TObject);
begin
  coordCorners := TLX;
  label_Select.Top := label_TL.Top;
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.Edit_TR_XDblClick(Sender: TObject);
begin
  coordCorners := TRX;
  label_Select.Top := label_TR.Top;
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.Edit_BR_XDblClick(Sender: TObject);
begin
  coordCorners := BRX;
  label_Select.Top := label_BR.Top;
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.Edit_BL_XDblClick(Sender: TObject);
begin
  coordCorners := BLX;
  label_Select.Top := label_BL.Top;
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.Edit_PK_XDblClick(Sender: TObject);
begin
  coordCorners := PKX;
  label_Select.Top := label_PK.Top;
end;

//---------------------------------------------------------------------------
function TForm_WarpCrop.LoadBitmap(FileName : string) : boolean;
begin
  result := false; // assume at first
  if (NOT FileExists(FileName)) then begin
    Exit;
  end else begin
    // check if too large to load - avoid crash
    if (BMP_ImageWidth(tFileName) > 32768) then begin
      MessageShow('bitmap file too large');
      beep; Exit;
    end;
    Edit_Filename.Text := ExtractFileName(FileName);
    // set image to auto take its size from picture 1:1 and fit in window
    Image_Tile.AutoSize := true;
    Image_Tile.Stretch := false; // no stretch - 1:1 resolution to start
    Image_Tile.Picture.LoadFromFile(FileName);
//    Image_Tile.Canvas.CopyMode := cmSrcCopy; // try to make sure colors are correct - no go
// if 256 color bitmap, drawing on top of bitmap will use the 256 color palette !
// any color will use the closest color in palette -> approx color
// convert to pf24 bit for absolute color - works!
  //  if (Image_Tile.Picture.Bitmap.PixelFormat <> pf24bit) then begin
  //    Image_Tile.Picture.Bitmap.PixelFormat := pf24bit;
  //  end;
    // also adjust the mask image to match the size
//    Image_Mask.AutoSize := true;
//    Image_Mask.Stretch := false; // no stretch - 1:1 resolution to start
//    Image_Mask.Picture.Bitmap.Width := Image_Tile.Picture.Bitmap.Width;
//    Image_Mask.Picture.Bitmap.Height := Image_Tile.Picture.Bitmap.Height;
    // also adjust the PaintBox to match the size
    PaintBox1.Width := Image_Tile.Picture.Bitmap.Width;
    PaintBox1.Height := Image_Tile.Picture.Bitmap.Height;
    // make sure all relative sizes are correct by resetting the zoom level to 1.0
    // or could use call to ZoomRestore
    opZoomScale := 1.0;
    ScrollBox_Image.HorzScrollBar.Range := Image_Tile.{Picture.}Width;
    ScrollBox_Image.VertScrollBar.Range := Image_Tile.{Picture.}Height;
//    Image_Mask.Width := Image_Tile.Width;
//    Image_Mask.Height := Image_Tile.Height;
    // clear any previous drawings
//    with Form_WarpCrop.Image_Mask do begin
//      Canvas.Brush.Color := Picture.Bitmap.TransparentColor;
//      Canvas.FillRect(rect(0,0,Picture.Width,Picture.Height));
//    end;
    // flag change, no show
    Object_Change_Show(True, False);
    // all good
    result := true;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.Button_OpenClick(Sender: TObject);
begin
  imFileFilterString := 'Object files (*.BMP)|*.BMP|All files (*.*)|*.*';
  // dialog to select object file - must be .bmp extension
  imFileName := '';
  imInitialDir := objFolder;
  if (OpenDialog(OpenDialog_FileName,imFileName, imInitialDir, imFileFilterString)) then begin
    BMP_FileName := imFileName;
    // now try to open the file
    if (LoadBitmap(BMP_FileName)) then begin
      BitmapAvail := true;
    end;
    // flag change, show
    Object_Change_Show(True, True);
  end;
end;

// used for all edit boxes
//---------------------------------------------------------------------------
procedure TForm_WarpCrop.Edit_TL_XKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Chr(VK_RETURN)) then begin // exit ?
    {Form_WarpCrop.}SelectNext(Sender as TWinControl, True, True); // tab to next component
    Key := #0; // don't respond to key
  end;
end;

// make GDAL based warp/crop batch file, with 3 or 4 reference points
//---------------------------------------------------------------------------
Procedure TForm_WarpCrop.MakeWarpBatchFile(FilePath, FileName : string; Warp3_4 : Warp_Type);
var
  WarpFile : TextFile;
begin
  AssignFile(Warpfile, FilePath +'\'+ FileName + '.bat');
  Rewrite(Warpfile);

  writeln(Warpfile,'@echo off');
  writeln(Warpfile,'setlocal');
  writeln(Warpfile,'rem goto directory where batch file is');
  writeln(Warpfile,'cd /d %~dp0');
  writeln(Warpfile,'rem set reference points');
  writeln(Warpfile,'set TopLeftX=' + Edit_TL_X.Text);
  writeln(Warpfile,'set TopLeftY=' + Edit_TL_Y.Text);
  writeln(Warpfile,'set TopRightX=' + Edit_TR_X.Text);
  writeln(Warpfile,'set TopRightY=' + Edit_TR_Y.Text);
  if (Warp3_4 = Warp4) then begin
    writeln(Warpfile,'set BotRightX=' + Edit_BR_X.Text);
    writeln(Warpfile,'set BotRightY=' + Edit_BR_Y.Text);
    writeln(Warpfile,'set BotLeftX=' + Edit_BL_X.Text);
    writeln(Warpfile,'set BotLeftY=' + Edit_BL_Y.Text);
  end else begin // Warp3
    writeln(Warpfile,'set PeakX=' + Edit_PK_X.Text);
    writeln(Warpfile,'set PeakY=' + Edit_PK_Y.Text);
  end;
  writeln(Warpfile,'set sourcebmp=' + BMP_FileName);
  writeln(Warpfile,'set destinationbmp=' + FileName + '.bmp');
  if (Warp3_4 = Warp4) then begin
    writeln(Warpfile,'rem use warp-4 batch file for perspective correction');
    writeln(Warpfile,'call Warp-4.bat');
  end else begin // Warp3
    writeln(Warpfile,'rem use warp-3 batch file for perspective correction');
    writeln(Warpfile,'call Warp-3.bat');
  end;
  writeln(Warpfile,'endlocal');

  // close the file
  CloseFile(Warpfile);
  MessageShow(rootFileName + '.bat done.');
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.Button_Load_ParametersClick(Sender: TObject);
var
  TempStr : string;
  FileExt : string;
begin
  imFileFilterString := 'Parameter files (*.PAR)|*.PAR|All files (*.*)|*.*';
  // dialog to select object file - must be .bmp extension
  imFileName := '';
  if (objFolder = '') then begin
    imInitialDir := Object_FolderName+'\World\Objects';
  end else begin
    imInitialDir := objFolder
  end;
  if (OpenDialog(OpenDialog_FileName,imFileName, imInitialDir, imFileFilterString)) then begin
    // extract relative filename
    Parameter_FileName := ExtractFileName(OpenDialog_FileName.FileName);
    // if no extension, add extension
    FileExt := ExtractFileExt(Parameter_FileName);
    if (FileExt = '') then begin
      rootFileName := Parameter_FileName;
      Parameter_FileName := Parameter_FileName + '.par'
    end else begin
      rootFileName := copy(Parameter_FileName,1,length(Parameter_FileName)-length(FileExt));
    end;
    //remember folder for this session
    objFolder := ExtractFileDir(OpenDialog_FileName.FileName); // no trailing '\'
    // read in the Parameter file
    if (FileExists(objFolder + '\' + Parameter_FileName)) then begin
      AssignFile(PAR_file, objFolder + '\' + Parameter_FileName);
      Reset(PAR_file);
      Readln(PAR_file, TempSTR);
      Edit_FileName.text := ReadCSV(TempSTR);
      Edit_TL_X.text := ReadCSV(TempSTR);
      Edit_TL_Y.text := ReadCSV(TempSTR);
      Edit_TR_X.text := ReadCSV(TempSTR);
      Edit_TR_Y.text := ReadCSV(TempSTR);
      Edit_BR_X.text := ReadCSV(TempSTR);
      Edit_BR_Y.text := ReadCSV(TempSTR);
      Edit_BL_X.text := ReadCSV(TempSTR);
      Edit_BL_Y.text := ReadCSV(TempSTR);
      Edit_PK_X.text := ReadCSV(TempSTR);
      Edit_PK_Y.text := ReadCSV(TempSTR);
      CloseFile(PAR_file);
      Label_FileName.Caption := Parameter_FileName;
      // now try to open the Bitmap file
      BMP_FileName := Edit_FileName.text;
      if (LoadBitmap(BMP_FileName)) then begin
        BitmapAvail := true;
      end;
      // flag no change, show
      Object_Change_Show(False, True);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.Button_Save_ParametersClick(Sender: TObject);
var
  FileExt : string;
begin
  // validate ?
  // no blank items except for Peak if not used
  // or bottom of peak used
  // TBD

  if (Parameter_FileName = '') then begin
    Parameter_FileName := 'FrontView-Object'+'-1.par';
  end;
  exFileName := Parameter_FileName;
  exInitialDir := objFolder;
  exFileFilterString := 'Object files (*.PAR)|*.PAR|All files (*.*)|*.*';
  // dialog to select output file - must be .PAR extension
  if (SaveDialog(SaveDialog_FileName,exFileName, exInitialDir, exFileFilterString)) then begin
    // extract relative filename
    Parameter_FileName := ExtractFileName(exFileName);
    //remember folder for this session
    objFolder := ExtractFileDir(exFileName); // no trailing '\'
    // if no extension, add extension
    FileExt := ExtractFileExt(Parameter_FileName);
    if (FileExt = '') then begin
      rootFileName := Parameter_FileName;
      Parameter_FileName := Parameter_FileName + '.par'
    end else begin
      rootFileName := copy(Parameter_FileName,1,length(Parameter_FileName)-length(FileExt));
    end;
    // show on form
    Label_FileName.Caption := ExtractFileName(Parameter_FileName);
    // if already exists, make a backup first
    if (FileExists(objFolder + '\' + Parameter_FileName)) then begin
      CopyFile(pChar(objFolder + '\' + Parameter_FileName),
               pChar(objFolder + '\' + Parameter_FileName+'~'),false);
    end;
    // save the Parameter file
    AssignFile(PAR_file,objFolder + '\' + Parameter_FileName);
    Rewrite(PAR_file);
    writeln(PAR_File,format('%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s',[
      Edit_FileName.text,
      Edit_TL_X.text, Edit_TL_Y.text,
      Edit_TR_X.text, Edit_TR_Y.text,
      Edit_BR_X.text, Edit_BR_Y.text,
      Edit_BL_X.text, Edit_BL_Y.text,
      Edit_PK_X.text, Edit_PK_Y.text
        ]));
    CloseFile(PAR_file);
    // flag no longer changed, no show
    Object_Change_Show(False, False);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.UpDown_HorizMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Delta : Integer;
  Value : Integer;
begin
  if (BitmapAvail) then begin
    if (ssShift in Shift) then begin
      Delta := 2;
    end else begin
      Delta := 1;
    end;
    if (ssCtrl in Shift) then begin
      Delta := Delta * 2;
    end;
    if (X > UpDown_Horiz.Height div 2) then begin // Move right
    end else begin // Move left
      Delta := - Delta;
    end;
    // update the coordinate
    case coordCorners of
      TLX: begin
        if (Edit_TL_X.Text <> '') then begin
          if Convert(Edit_TL_X.Text, value) then begin
            Edit_TL_X.Text := format('%d',[value+Delta]);
            // flag change, show
            Object_Change_Show(True, True);
          end;
        end;
      end;
      TRX: begin
        if (Edit_TR_X.Text <> '') then begin
          if Convert(Edit_TR_X.Text, value) then begin
            Edit_TR_X.Text := format('%d',[value+Delta]);
            Object_Change_Show(True, True);
          end;
        end;
      end;
      BRX: begin
        if (Edit_BR_X.Text <> '') then begin
          if Convert(Edit_BR_X.Text, value) then begin
            Edit_BR_X.Text := format('%d',[value+Delta]);
            Object_Change_Show(True, True);
          end;
        end;
      end;
      BLX: begin
        if (Edit_BL_X.Text <> '') then begin
          if Convert(Edit_BL_X.Text, value) then begin
            Edit_BL_X.Text := format('%d',[value+Delta]);
            Object_Change_Show(True, True);
          end;
        end;
      end;
      PKX: begin
        if (Edit_PK_X.Text <> '') then begin
          if Convert(Edit_PK_X.Text, value) then begin
            Edit_PK_X.Text := format('%d',[value+Delta]);
            Object_Change_Show(True, True);
          end;
        end;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.UpDown_VertMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Delta : Integer;
  Value : Integer;
begin
  if (BitmapAvail) then begin
    if (ssShift in Shift) then begin
      Delta := 2;
    end else begin
      Delta := 1;
    end;
    if (ssCtrl in Shift) then begin
      Delta := Delta * 2;
    end;
    if (Y > UpDown_Vert.Height div 2) then begin // Move right
    end else begin // Move left
      Delta := - Delta;
    end;
    // update the coordinate
    case coordCorners of
      TLX: begin
        if (Edit_TL_Y.Text <> '') then begin
          if Convert(Edit_TL_Y.Text, value) then begin
            Edit_TL_Y.Text := format('%d',[value+Delta]);
            Object_Change_Show(True, True);
          end;
        end;
      end;
      TRX: begin
        if (Edit_TR_Y.Text <> '') then begin
          if Convert(Edit_TR_Y.Text, value) then begin
            Edit_TR_Y.Text := format('%d',[value+Delta]);
            Object_Change_Show(True, True);
          end;
        end;
      end;
      BRX: begin
        if (Edit_BR_Y.Text <> '') then begin
          if Convert(Edit_BR_Y.Text, value) then begin
            Edit_BR_Y.Text := format('%d',[value+Delta]);
            Object_Change_Show(True, True);
          end;
        end;
      end;
      BLX: begin
        if (Edit_BL_Y.Text <> '') then begin
          if Convert(Edit_BL_Y.Text, value) then begin
            Edit_BL_Y.Text := format('%d',[value+Delta]);
            Object_Change_Show(True, True);
          end;
        end;
      end;
      PKX: begin
        if (Edit_PK_Y.Text <> '') then begin
          if Convert(Edit_PK_Y.Text, value) then begin
            Edit_PK_Y.Text := format('%d',[value+Delta]);
            Object_Change_Show(True, True);
          end;
        end;
      end;
    end;
  end;
end;

// make GDAL based warp/crop batch file
//---------------------------------------------------------------------------
procedure TForm_WarpCrop.Button_MakeBatchClick(Sender: TObject);
begin
  validate(Sender);

  if (QuadFound) then begin
    // use parameter root file name
    MakeWarpBatchFile(objFolder, rootFileName, Warp4);
  end;

  if (TriangleFound) then begin
    // if peak not blank then do warp-3 as well
    MakeWarpBatchFile(objFolder, rootFileName+'-peak', Warp3);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.Button_ExecuteBatchClick(Sender: TObject);
var
 FileName : string;
begin
  FileName := rootFileName + '.bat';
  if (FileExists(objFolder +'\'+ FileName)) then begin
    Execute_BatchFile(objFolder, FileName, '');
  end;
  FileName := rootFileName + '-peak.bat';
  if (FileExists(objFolder +'\'+ FileName)) then begin
    Execute_BatchFile(objFolder, FileName, '');
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_WarpCrop.PaintBox1Paint(Sender: TObject);
begin
  Form_WarpCrop.DrawObjects(nil);
end;

//---------------------------------------------------------------------------
end.


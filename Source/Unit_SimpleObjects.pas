{
 * Unit_SimpleObjects.pas
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

//----------------------------------------------------------------------------
unit Unit_SimpleObjects;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Grids,
  Unit_Objects;

type
  TForm_SimpleObjects = class(TForm)
    OpenDialog_FileName: TOpenDialog;
    SaveDialog_FileName: TSaveDialog;
    PageControl_SimpleObjects: TPageControl;
    TabSheet_SimpleObject: TTabSheet;
    TabSheet_SimpleTexture: TTabSheet;
    TabSheet_CompositeObject: TTabSheet;
    GroupBox_Object: TGroupBox;
    Label_Type: TLabel;
    Label_Width: TLabel;
    Label_Height: TLabel;
    LabelPeakHeight: TLabel;
    Label_Length: TLabel;
    Edit_Width: TEdit;
    Edit_Height: TEdit;
    Button_Open: TButton;
    Button_Exit: TButton;
    Button_SaveAs: TButton;
    ComboBox_Type: TComboBox;
    Edit_Peak: TEdit;
    Edit_Length: TEdit;
    GroupBox_Texture: TGroupBox;
    Button_T_Exit: TButton;
    Image_Texture: TImage;
    Edit_TextureFileName: TEdit;
    Label_File: TLabel;
    Button_T_Open: TButton;
    Button_T_Save: TButton;
    Label_FileName: TLabel;
    Label_TextureFileName: TLabel;
    Label_CompositeFileName: TLabel;
    StringGrid_Composite: TStringGrid;
    Button_C_Exit: TButton;
    Button_C_Open: TButton;
    Button_C_Save: TButton;
    Button_Remove: TButton;
    ButtonAdd: TButton;
    Label_T_Type: TLabel;
    ComboBox_T_Type: TComboBox;
    Label_Coords: TLabel;
    ComboBox_Size: TComboBox;
    Label_Size: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button_OpenClick(Sender: TObject);
    procedure Button_ExitClick(Sender: TObject);
    procedure Button_SaveAsClick(Sender: TObject);
    procedure ComboBox_TypeChange(Sender: TObject);
    procedure Button_T_ExitClick(Sender: TObject);
    procedure Button_C_ExitClick(Sender: TObject);
    procedure Image_TextureMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure ComboBox_T_TypeChange(Sender: TObject);
    procedure Button_T_OpenClick(Sender: TObject);
    procedure Button_C_OpenClick(Sender: TObject);
    procedure Button_C_SaveClick(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure Button_RemoveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image_TextureMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button_T_SaveClick(Sender: TObject);
    procedure Image_TextureMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ComboBox_SizeChange(Sender: TObject);
  private
    { Private declarations }
    procedure InitDetailGrid;
  public
    { Public declarations }
  end;

var
  Form_SimpleObjects: TForm_SimpleObjects;

var
  ApplicationPath : string;
  Memo_Message : TMemo;  // external TMemo for messages
  CondorFolder : string; // external path for Condor program folder
  WorkingFolder : string;
  ObjectFolder : string;
  objFolder : string;

//----------------------------------------------------------------------------
implementation

uses
  FileCtrl,
  u_CalibImport, u_CalibExport, // for dialogs only
  u_X_CX{, u_Resize};

{$R *.DFM}

var
  stBitmap : TBitmap;
  BitMap_Texture : TBitMap;  // overall texture bitmap
  BitMap_Section : TBitMap;  // partial texture bitmap

  // delphi issue - mouseUp event triggered when dialog open on top double-clicked
  MouseDown_Flag : Boolean;

  soFilename : string;
  stFilename : string;
  coFilename : string;

// duplicate of function in Main.pas
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
function ReadCSV(var s: string) : string;
var
  CommaPos : Integer;
begin
  CommaPos := pos(',',S);
  if (CommaPos <> 0) then begin
    result := copy(s,1,CommaPos-1);
    s := copy(s, CommaPos+1, length(s));
  end else begin
    result := s;
    s:= '';
  end;
end;

//---------------------------------------------------------------------------
Procedure Image_Clear(iImage : TImage);
begin
  // clear bitmap
  with iImage do begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := {clBtnFace}{clGray}clSilver;
    Canvas.FillRect(rect(0,0,Width,Height));
  end;
end;

//---------------------------------------------------------------------------
Procedure Bitmap_Clear(iBitmap : TBitmap; cColor : TColor);
begin
  // clear bitmap
  with iBitmap do begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := cColor;
    Canvas.FillRect(rect(0,0,Width,Height));
  end;
end;

//----------------------------------------------------------------------------
Procedure SetDefault_Image(dSize : integer);
begin
  // set default size
  BitMap_Texture.Width := dSize;
  BitMap_Texture.Height := dSize;
  Bitmap_Texture.PixelFormat := pf24bit;
  Bitmap_Clear(BitMap_Texture, clSilver{clBlue});

  with Form_SimpleObjects do begin
    ComboBoxMatchString(ComboBox_Size, IntToStr(dSize));

    Image_Texture.Picture.Bitmap.Create;
    Image_Texture.Stretch := true;
    Image_Texture.Picture.Bitmap.Width := dSize;
    Image_Texture.Picture.Bitmap.Height := dSize;
//    Image_Texture.Picture.Bitmap.Canvas.Draw(0,0,BitMap_Texture);
    // update the bitmap and the outlines
    ComboBox_T_TypeChange(nil);
  end;
end;

//----------------------------------------------------------------------------
procedure TForm_SimpleObjects.FormCreate(Sender: TObject);
const
  dSize = 1024;
begin
  // bitmap to contain a simple texture
  BitMap_Texture := TBitMap.Create;
  // bitmap to load a view
  BitMap_Section := TBitMap.Create;

  // make sure treeView is selected; use one in Unit_Objects
  u_X_CX.oTreeView := Unit_Objects.Form_Objects.TreeView_Object;

  // set a default size for the blank inital image
  SetDefault_Image(dSize);

  // shorter reference to the image bitmap
  stBitmap := Image_Texture.Picture.Bitmap;

  InitDetailGrid;
end;

//---------------------------------------------------------------------------
procedure TForm_SimpleObjects.FormDestroy(Sender: TObject);
begin
  Bitmap_Section.Free;
  Bitmap_Texture.Free;
end;

//---------------------------------------------------------------------------
function OpenDialog() : boolean;
begin
  result := false;
  With Form_SimpleObjects do begin
    OpenDialog_FileName.Filter := imFileFilterString;
    OpenDialog_FileName.FileName := imFileName;
    OpenDialog_FileName.InitialDir := imInitialDir;
    if (OpenDialog_FileName.Execute) then begin
      imFileName := OpenDialog_FileName.FileName;
      result := true;
    end;
  end;
end;

//---------------------------------------------------------------------------
function SaveDialog() : boolean;
begin
  result := false;
  With Form_SimpleObjects do begin
    SaveDialog_FileName.Filter := exFileFilterString;
    SaveDialog_FileName.FileName := exFileName;
    SaveDialog_FileName.InitialDir := exInitialDir;
    if (SaveDialog_FileName.Execute) then begin
      exFileName := SaveDialog_FileName.FileName;
      result := true;
    end;
  end;
end;

//- TAB - Simple Object -----------------------------------------------------

//---------------------------------------------------------------------------
procedure TForm_SimpleObjects.Button_ExitClick(Sender: TObject);
begin
  Close;
end;

//---------------------------------------------------------------------------
type
  oDefaults_t = record
    od_Width : string;
    od_Length : string;
    od_Height : string;
    od_Peak : string;
    od_File : string;
  end;

  oObject_t = record
    oFile     : array[0..7-1] of string;
    oButtons  : array[0..7-1] of byte; // byte -> 8 bit mask
    oDefaults : array[0..7-1] of oDefaults_t;
  end;
const
    oObject : oObject_t =
      (oFile : ('Building_FlatRoof.px', 'Building_PeakRoof.px','Building_Domed.px',
                'pole.px', 'windsock.px', 'asphalt.px', 'grass.px');
       oButtons  : ($17, $1F, $17, $15, $06, $03, $03);
       oDefaults : (
        ( od_Width : '15.0'; od_Length : ' 25.0'; od_Height : ' 8.0'; od_Peak : '0.0'; od_File : 'Textures/H_PK_S_Red.bmp' ),
        ( od_Width : '15.0'; od_Length : ' 25.0'; od_Height : ' 8.0'; od_Peak : '2.0'; od_File : 'Textures/H_Dome_Blue.bmp' ),
        ( od_Width : '15.0'; od_Length : ' 25.0'; od_Height : ' 8.0'; od_Peak : '0.0'; od_File : 'Textures/H_Dome_Blue.bmp' ),
        ( od_Width : ' 0.1'; od_Length : '  0.0'; od_Height : '10.0'; od_Peak : '0.0'; od_File : 'Textures/Mast.dds' ),
        ( od_Width : ' 0.0'; od_Length : '  2.0'; od_Height : '10.0'; od_Peak : '0.0'; od_File : ('') ),
        ( od_Width : '20.0'; od_Length : '800.0'; od_Height : ' 0.0'; od_Peak : '0.0'; od_File : ('') ),
        ( od_Width : '20.0'; od_Length : '800.0'; od_Height : ' 0.0'; od_Peak : '0.0'; od_File : ('') )
       )
      );
var
  TC : array[0..2-1] of single =
    (0.0025,0.0025);

//---------------------------------------------------------------------------
Procedure Enable_Edits(Select : integer);
begin
  if (Select = -1) then begin
    Select := 0;
  end;
  with Form_SimpleObjects do begin
    Edit_Width.{enabled}Visible  := (Select AND 1) = 1;
    Edit_Length.{enabled}Visible := (Select AND 2) = 2;
    Edit_Height.{enabled}Visible := (Select AND 4) = 4;
    Edit_Peak.{enabled}Visible   := (Select AND 8) = 8;
    Edit_TextureFileName.{enabled}Visible   := (Select AND 16) = 16;
  end;
end;

//---------------------------------------------------------------------------
Procedure Default_Edits(Select : oDefaults_t);
begin
  with Form_SimpleObjects do begin
    Edit_Width.Text  := Select.od_Width;
    Edit_Length.Text := Select.od_Length;
    Edit_Height.Text := Select.od_Height;
    Edit_Peak.Text := Select.od_Peak;
    Edit_TextureFileName.Text := Select.od_File;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_SimpleObjects.Button_OpenClick(Sender: TObject);
var
  SO_File : TextFile;
  TempSTR : string;
begin
  // dialog to select input file - must be .SO extension
  imFileFilterString := 'Simple Object files (*.SO)|*.SO|All files (*.*)|*.*';
  imFileName := soFileName;
  imInitialDir := objFolder;
  if OpenDialog then begin
    soFileName := imFileName;
    Label_FileName.Caption := ExtractFileName(imFileName);
    ObjectFolder := ExtractFileDir(imFileName);     // no trailing '/'
    //remember folder for this session
    objFolder := ObjectFolder;
  end;

  // read in the SO file
  if (FileExists(imFileName)) then begin
    AssignFile(SO_file, imFileName);
    Reset(SO_file);
    Readln(SO_file, TempSTR);
    ComboBox_Type.text := ReadCSV(TempSTR);
    Edit_Width.text := ReadCSV(TempSTR);
    Edit_Length.text := ReadCSV(TempSTR);
    Edit_Height.text := ReadCSV(TempSTR);
    Edit_Peak.text := ReadCSV(TempSTR);
    Edit_TextureFileName.Text := ReadCSV(TempSTR);
    CloseFile(SO_file);
  end;
  ComboBoxMatchString(ComboBox_Type, ComboBox_Type.Text);
  Enable_Edits(oObject.oButtons[ComboBox_Type.ItemIndex]);
  // ???
  // now see if the texture file can be found and read it too
  //Image_Clear(Image_Texture);

end;

//---------------------------------------------------------------------------
procedure TForm_SimpleObjects.Button_SaveAsClick(Sender: TObject);
var
  SO_File : TextFile;
  oFileName : string;
  oFileExt : string;
  W, L, H, P : single;
begin
  // dialog to select output file - must be .SO or C3D extension
  exFileFilterString := 'Object files (*.SO *.C3D)|*.SO;*.C3D|All files (*.*)|*.*';
  exFileName := soFileName;
  imInitialDir := objFolder;
  if (SaveDialog) then begin
    imFileName := exFileName;
    Label_FileName.Caption := ExtractFileName(exFileName);
    ObjectFolder := ExtractFileDir(exFileName);  // not including trailing '\'
    //remember folder for this session
    objFolder := ObjectFolder;

    oFileExt := ExtractFileExt(exFileName);
    if (uppercase(oFileExt) = '.SO') then begin
      // save the SO file
      AssignFile(SO_file, exFileName);
      Rewrite(SO_file);
      writeln(SO_File,format('%s,%s,%s,%s,%s,%s',[
        ComboBox_Type.text,
        Edit_Width.text,
        Edit_Length.text,
        Edit_Height.text,
        Edit_Peak.text,
        Edit_TextureFileName.Text
          ]));
      CloseFile(SO_file);
    end else begin
      if ((uppercase(oFileExt) = '.C3D') OR
          (uppercase(oFileExt) = '.PX')) then begin
        // select the simple object
        with ComboBox_Type do begin
          ComboBoxMatchString(ComboBox_Type, ComboBox_Type.Text);
          if (ItemIndex = -1) then begin
            // MessageShow('Invalid Object type');
            exit;
          end;
          // read the simple object file
          oFileName := ApplicationPath+'\SimpleObjects\'+oObject.oFile[ComboBox_Type.ItemIndex];
          if (NOT FileExists(oFileName)) then begin
            // MessageShow('Object not available');
            Beep; Exit;
          end;
          ReadCondorXfile(oFileName, false);
          // read the parameters
          W := strtofloat(Edit_Width.text); // need validation to avoid exception !
          L := strtofloat(Edit_Length.text);
          H := strtofloat(Edit_Height.text);
          P := strtofloat(Edit_Peak.text);
          // modify the Object
          // find the FTM and set the parameters
          Reset_FTM_Unity;
          case ComboBox_Type.ItemIndex of
            0: begin // flat roof
              FTM[ 0] := W;
              FTM[ 5] := L;
              FTM[10] := H;
              //FTM[11] := 0.0;
              UpdateFTM('FTM_0',FTM);
              UpdateTF('TF_0',Edit_TextureFileName.Text);
            end;
            1: begin // peak roof
              // two FTM matrices to do!
              FTM[ 0] := W;
              FTM[ 5] := L;
              FTM[10] := H;
              //FTM[11] := 0.0;
              UpdateFTM('FTM_0',FTM);
              UpdateTF('TF_0',Edit_TextureFileName.Text);
              FTM[ 0] := W;
              FTM[ 5] := L;
              FTM[10] := P;
              FTM[11] := H;
              UpdateFTM('FTM_1',FTM);
              UpdateTF('TF_1',Edit_TextureFileName.Text);
            end;
            2: begin // dome
              FTM[ 0] := W;
              FTM[ 5] := L;
              FTM[10] := H;
              //FTM[11] := 0.0;
              UpdateFTM('FTM_0',FTM);
              UpdateTF('TF_0',Edit_TextureFileName.Text);
            end;
            3: begin // pole
              FTM[ 0] := W;
              FTM[ 5] := W;
              FTM[10] := H;
              //FTM[11] := 0.0;
              UpdateFTM('FTM_0',FTM);
            end;
            4: begin // windsock
              // calc triangle corners, and set height
              // windsock length is 100x the radius of the equilateral triangle
              // default triangle is 1m, i.e. radius=0.01
              FTM[ 0] := L;
              FTM[ 5] := L;
              FTM[10] := 0.0;
              FTM[11] := H;
              UpdateFTM('FTM_0',FTM);
            end;
            5: begin // asphalt
              FTM[ 0] := W;
              FTM[ 5] := L;
              FTM[10] := 0.0;
              //FTM[11] := 0.0;
              UpdateFTM('FTM_0',FTM);
              // adjust texture coord ratio !
              TC[0] := strtofloat(Edit_Width.text)/strtofloat(Edit_Length.text);
              TC[1] := TC[0];
              UpdateTC('tc_Asphalt', TC);
            end;
            6: begin // grass
              FTM[ 0] := W;
              FTM[ 5] := L;
              FTM[10] := 0.0;
              //FTM[11] := 0.0;
              UpdateFTM('FTM_0',FTM);
              // adjust texture coord ratio !
              TC[0] := strtofloat(Edit_Width.text)/strtofloat(Edit_Length.text);
              TC[1] := TC[0];
              UpdateTC('tc_Grass', TC);
            end;
            else begin
              // MessageShow('Invalid Object type'');
              exit;
            end;
          end;
        end;
        // now save the object
        WriteCondorObjectFile(exFileName,None,false);
      end;
    end;
  end;
end;

//- TAB - Simple Texture ----------------------------------------------------

//---------------------------------------------------------------------------
{const

 Top Left
 0.000,0.000,
 0.500,0.000,
 0.500,0.125,
 0.000,0.125
 Top Right
 0.500,0.000,
 1.000,0.000,
 1.000,0.125,
 0.000,0.125
 etc...

 sector based on mouse X,Y
 sx := (X + Image.Width) / Image.Width;
 sy := (Y + Image.Height / 2) / (Image.Height /2);

}
//---------------------------------------------------------------------------
Procedure Draw_Sectors;
var
  i, j : Integer;
  w, h : Integer;
begin
  with Form_SimpleObjects.Image_Texture do begin
    w:= Picture.Bitmap.Width;
    h:= Picture.Bitmap.Height;
    Canvas.Pen.Mode := pmCopy; // needed for pixels[] !
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Width := w div Width; // thinner-thicker if stretch-squeeze
    Canvas.Pen.Color := clRed;
    Canvas.Brush.Color := clRed; // FrameRect uses brush color
    for i := 0 to 2-1 do begin
      for j := 0 to 4-1 do begin
{        Canvas.FrameRect(rect(1+i*(w DIV 2),    1+j*(h DIV 4),
                            (  (i+1)*(w DIV 2)),((j+1)*(h DIV 4))));
}        Canvas.Polyline([point(1+i*(w DIV 2),   1+j*(h DIV 4)),
                         point(1+i*(w DIV 2),   ((j+1)*(h DIV 4))),
                         point( (i+1)*(w DIV 2),((j+1)*(h DIV 4))),
                         point( (i+1)*(w DIV 2),1+j*(h DIV 4)),
                         point(1+i*(w DIV 2),   1+j*(h DIV 4))
                        ]);
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
Procedure Draw_Halves;
var
  j : integer;
  w, h : Integer;
begin
  with Form_SimpleObjects.Image_Texture do begin
    w:= Picture.Bitmap.Width;
    h:= Picture.Bitmap.Height;
    Canvas.Pen.Mode := pmCopy; // needed for pixels[] !
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Width := w div Width; // thinner-thicker if stretch-squeeze
    Canvas.Pen.Color := clRed;
    Canvas.Brush.Color := clRed; // FrameRect uses brush color
    for j := 0 to 2-1 do begin
{      Canvas.FrameRect(rect(1, 1+j*(h DIV 2),
                           (w),((j+1)*(h DIV 2))));
}      Canvas.Polyline([point(1,1+j*(h DIV 2)),
                       point(1,  ((j+1)*(h DIV 2))),
                       point(w,  ((j+1)*(h DIV 2))),
                       point(w,1+j*(h DIV 2)),
                       point(1,1+j*(h DIV 2))
                      ]);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_SimpleObjects.ComboBox_TypeChange(Sender: TObject);
begin
  Enable_Edits(oObject.oButtons[ComboBox_Type.ItemIndex]);
  Default_Edits(oObject.oDefaults[ComboBox_Type.ItemIndex]);
end;

//---------------------------------------------------------------------------
procedure TForm_SimpleObjects.Button_T_ExitClick(Sender: TObject);
begin
  Close;
end;

//---------------------------------------------------------------------------
Procedure CalcSector(X, Y, Max_X, Max_Y : Integer; var sX, sY : Integer);
begin
  //sector based on mouse X,Y
  sX := trunc(X / (Max_X / 2));
  sY := trunc(Y / (Max_Y / 4));
end;

//---------------------------------------------------------------------------
Procedure CalcHalf(X, Y, Max_X, Max_Y : Integer; var sX, sY : Integer);
begin
  //half based on mouse X,Y
  sX := 0;
  sY := trunc(Y / (Max_Y / 2));
end;

//---------------------------------------------------------------------------
procedure TForm_SimpleObjects.Image_TextureMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  sX, sY : integer;
begin
  CalcSector(X, Y, Image_Texture.Width, Image_Texture.Height, sX, sY);
    Label_Coords.Caption := format('%d,%d',[
      (sX), (sY)
      ]);
  end;

//---------------------------------------------------------------------------
procedure TForm_SimpleObjects.ComboBox_T_TypeChange(Sender: TObject);
begin
  Image_Texture.Picture.Bitmap.Canvas.Draw(0,0,BitMap_Texture);
  with ComboBox_T_Type do begin
    ComboBoxMatchString(ComboBox_T_Type, ComboBox_T_Type.Text);
    if (ItemIndex = -1) then begin
      // MessageShow('Invalid Object type');
      exit;
    end;
    // draw the areas
    case ComboBox_T_Type.ItemIndex of
      0, 1: begin // flat roof
         Draw_Sectors;
      end;
      2: begin // flat roof
        Draw_Halves;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_SimpleObjects.ComboBox_SizeChange(Sender: TObject);
begin
  SetDefault_Image(StrToInt(ComboBox_Size.Items[ComboBox_Size.ItemIndex]));
  case ComboBox_Size.ItemIndex of
    0: begin
    end;
    else begin
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_SimpleObjects.Button_T_OpenClick(Sender: TObject);
begin
  // dialog to select input file - must be .BMP extension
  imFileFilterString := 'Bitmap files (*.BMP)|*.BMP|All files (*.*)|*.*';
  imFileName := stFileName;
  imInitialDir := objFolder;
  try
    if OpenDialog then begin
      stFileName := imFileName;
      Label_TextureFileName.Caption := ExtractFileName(imFileName);
      ObjectFolder := ExtractFileDir(imFileName);     // no trailing '/'
      //remember folder for this session
      objFolder := ObjectFolder;

      Image_Texture.Stretch := true;
//     use Objects ShowBitmap instead ? --> modify
 //     ??? fix bitmap first ???  some Condor bitmaps have size error
      Bitmap_Texture.LoadFromFile(imFileName);
      // if 256 color bitmap, drawing on top of bitmap will use the 256 color palette !
      // any color will use the closest color in palette -> approx color
      // convert to pf24 bit for absolute color - works!
      if (Bitmap_Texture.PixelFormat <> pf24bit) then begin
        Bitmap_Texture.PixelFormat := pf24bit;
      end;
      with Image_Texture do begin
//        Canvas.StretchDraw(rect(0,0,Width,Height),BitMap_Texture);
        // keep size of loaded image, will stretch/squeeze as necessary
        Picture.Bitmap.Width := Bitmap_Texture.Width;
        Picture.Bitmap.Height := Bitmap_Texture.Height;
//        Picture.Bitmap.Canvas.Draw(0,0,BitMap_Texture);
        // update the size
        ComboBoxMatchString(ComboBox_Size, IntToStr(Picture.Bitmap.Width));
      end;
      // update the outlines
      ComboBox_T_TypeChange(Sender);
    end;
  finally
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_SimpleObjects.Button_T_SaveClick(Sender: TObject);
var
  SO_File : TextFile;
  oFileName : string;
  oFileExt : string;
  W, L, H, P : single;
begin
  // dialog to select output file - must be .BMP extension
  exFileFilterString := 'Object files (*.BMP)|*.BMP|All files (*.*)|*.*';
  exFileName := stFileName;
  imInitialDir := objFolder;
  if (SaveDialog) then begin
    stFileName := exFileName;
    Label_FileName.Caption := ExtractFileName(exFileName);
    ObjectFolder := ExtractFileDir(exFileName);  // not including trailing '\'
    //remember folder for this session
    objFolder := ObjectFolder;

    oFileExt := ExtractFileExt(exFileName);
    if (uppercase(oFileExt) = '.BMP') then begin
      Bitmap_Texture.SaveToFile(exFileName);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_SimpleObjects.Image_TextureMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  sX, sY : integer;
  dX, dY : integer;
begin
  // delphi issue - mouseUp event triggered when dialog open on top double-clicked
  if (MouseDown_Flag) then begin
    MouseDown_Flag := false;
    // dialog to select input file - must be .BMP extension
    imFileFilterString := 'Bitmap files (*.BMP)|*.BMP|All files (*.*)|*.*';
    imFileName := '';
    imInitialDir := objFolder;
    if OpenDialog then begin
      Label_TextureFileName.Caption := ExtractFileName(imFileName);
      ObjectFolder := ExtractFileDir(imFileName);     // no trailing '/'
      //remember folder for this session
      objFolder := ObjectFolder;

      Image_Texture.Stretch := true;
 //     ??? fix bitmap first ???
      Bitmap_Section.LoadFromFile(imFileName);
      // if 256 color bitmap, drawing on top of bitmap will use the 256 color palette !
      // any color will use the closest color in palette -> approx color
      // convert to pf24 bit for absolute color - works!
      if (Bitmap_Section.PixelFormat <> pf24bit) then begin
        Bitmap_Section.PixelFormat := pf24bit;
      end;

      case ComboBox_T_Type.ItemIndex of
        0: begin // flat roof
          CalcSector(X, Y, Image_Texture.Width, Image_Texture.Height, sX, sY);
          dX := Bitmap_Texture.Width DIV 2; dY := Bitmap_Texture.Height DIV 4;
        end;
        1: begin // peak roof
          CalcSector(X, Y, Image_Texture.Width, Image_Texture.Height, sX, sY);
          dX := Bitmap_Texture.Width DIV 2; dY := Bitmap_Texture.Height DIV 4;
        end;
        2: begin // dome
          CalcHalf(X, Y, Image_Texture.Width, Image_Texture.Height, sX, sY);
          dX := Bitmap_Texture.Width; dY := Bitmap_Texture.Height DIV 2;
        end;
        else begin
          Exit;
        end;
      end;

//      // draw the section bitmap onto the overall bitmap
      Bitmap_Texture.Canvas.StretchDraw(rect(sX*dx,sY*dy,sX*dx+dx,sY*dy+dy),BitMap_Section);
//      Bitmap_Texture.Canvas.Draw(sX*dx,sY*dY,BitMap_Section);

    // redraw the image
{    with Image_Texture do begin
//      Canvas.StretchDraw(rect(0,0,Width,Height),BitMap_Texture);
      // keep size of loaded image, will stretch/squeeze as necessary
//      Picture.Bitmap.Width := Bitmap_Texture.Width;
//      Picture.Bitmap.Height := Bitmap_Texture.Height;
//      Picture.Bitmap.Canvas.Draw(0,0,BitMap_Texture);
    end;
}
      // update the outlines
      ComboBox_T_TypeChange(Sender);
    end;
  end;
end;

//- TAB - Composite Objects -------------------------------------------------

var
  FileCount : integer;
  FirstRow : integer; //size of grid top header

//-----------------------------------------------------------------------------
procedure TForm_SimpleObjects.InitDetailGrid;
begin
  FileCount := 0;
  with Form_SimpleObjects.StringGrid_Composite do begin
    FirstRow := FixedRows;
//    MaxRows := 25;
    RowCount := 1+FirstRow;
    ColCount := 5;
//    {Canvas.}Font.Name := 'Terminal';  // Design time selection doesn't work
//    {Canvas.}Font.Size := 6;           // 5,6,9,12,14
//    {Canvas.}Font.Style := [];
    cells[ 0,0] := '';
    cells[ 1,0] := 'File';
    cells[ 2,0] := 'Easting';
    cells[ 3,0] := 'Northing';
    cells[ 4,0] := 'Angle';
    colWidths[ 0] := 10;
    colWidths[ 1] := 200;
    colWidths[ 2] := 50;
    colWidths[ 3] := 50;
    colWidths[ 4] := 50;
//    // turn off highlight when not focused
//    Options := Options - [goDrawFocusSelected];
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_SimpleObjects.Button_C_ExitClick(Sender: TObject);
begin
  Close;
end;

//---------------------------------------------------------------------------
procedure TForm_SimpleObjects.Button_C_OpenClick(Sender: TObject);
var
  CO_File : TextFile;
  TempSTR : string;
begin
  // dialog to select input file - must be .CO extension
  imFileFilterString := 'Composite Object files (*.CO)|*.CO|All files (*.*)|*.*';
  imFileName := coFileName;
  imInitialDir := objFolder;
  if OpenDialog then begin
    coFileName := imFileName;
    Label_CompositeFileName.Caption := ExtractFileName(imFileName);
    ObjectFolder := ExtractFileDir(imFileName);     // no trailing '/'
    //remember folder for this session
    objFolder := ObjectFolder;
  end;

  // read in the CO file
  if (FileExists(imFileName)) then begin
    FileCount := 0;
    with Form_SimpleObjects.StringGrid_Composite do begin
      RowCount := 1+FirstRow;
      AssignFile(CO_file, imFileName);
      Reset(CO_file);
      While (NOT EOF(CO_file)) do begin
        Readln(CO_file, TempSTR);
        cells[ 1,FileCount+FirstRow] := ReadCSV(TempSTR);
        cells[ 2,FileCount+FirstRow] := ReadCSV(TempSTR);
        cells[ 3,FileCount+FirstRow] := ReadCSV(TempSTR);
        cells[ 4,FileCount+FirstRow] := ReadCSV(TempSTR);
        INC(FileCount);
        RowCount := FileCount+FirstRow;
      end;
      CloseFile(CO_file);
      if (FileCount > 0) then begin
        // allow editing
//        EditorMode := true;
        Options := Options + [goEditing];
      end;
    end;
  end;

end;

//---------------------------------------------------------------------------
procedure TForm_SimpleObjects.Button_C_SaveClick(Sender: TObject);
var
  CO_File : TextFile;
  oFileName : string;
  oFileExt : string;
  i : Integer;
  X, Y, A : single;
begin
  // dialog to select output file - must be .CO or C3D extension
  exFileFilterString := 'Object files (*.CO *.C3D)|*.CO;*.C3D|All files (*.*)|*.*';
  exFileName := coFileName;
  imInitialDir := objFolder;
  if (SaveDialog) then begin
    coFileName := exFileName;
    Label_CompositeFileName.Caption := ExtractFileName(exFileName);
    ObjectFolder := ExtractFileDir(exFileName);  // not including trailing '\'
    //remember folder for this session
    objFolder := ObjectFolder;

    oFileExt := ExtractFileExt(exFileName);
    if (uppercase(oFileExt) = '.CO') then begin
      with Form_SimpleObjects.StringGrid_Composite do begin
        // save the CO file
        AssignFile(CO_file, exFileName);
        Rewrite(CO_file);
        for i := 0 to FileCount-1 do begin
          writeln(CO_File,format('%s,%s,%s,%s',[
          cells[ 1,i+FirstRow],
          cells[ 2,i+FirstRow],
          cells[ 3,i+FirstRow],
          cells[ 4,i+FirstRow]
          ]));
        end;
        CloseFile(CO_file);
      end;
    end else begin
      try
        Reset_FTM_Unity;   // only once as the same values are changed
        InjectFTM := True; // for all files
        for i := 0 to FileCount-1 do begin
          if ((uppercase(oFileExt) = '.C3D') OR
              (uppercase(oFileExt) = '.PX')) then begin
            with Form_SimpleObjects.StringGrid_Composite do begin
              oFileName := cells[ 1,i+FirstRow];
              if (uppercase(ExtractFileExt(oFileName)) = '.C3D') then begin
                X := strtofloat(cells[ 2,i+FirstRow]);
                Y := strtofloat(cells[ 3,i+FirstRow]);
                A := strtofloat(cells[ 4,i+FirstRow]);
                FTM[0] := cos(A/180*PI);
                FTM[1] := -sin(A/180*PI);
                FTM[4] := -FTM[1];
                FTM[5] := FTM[0];
                FTM[3] := X;
                FTM[7] := Y;
                ReadCondorC3Dfile(oFileName, (i <> 0));
              end;
            end;
          end;
        end;
        WriteCondorObjectFile(exFileName,None,false);
      finally
        InjectFTM := False;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_SimpleObjects.ButtonAddClick(Sender: TObject);
begin
  INC(FileCount);
  with Form_SimpleObjects.StringGrid_Composite do begin
    RowCount := FileCount+FirstRow;
    // set cursor
    Row := RowCount-1;
    Col := 1;
    // allow editing
//    EditorMode := true;
    Options := Options + [goEditing];
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_SimpleObjects.Button_RemoveClick(Sender: TObject);
var
 i : Integer;
begin
  if (FileCount > 0) then begin
    with Form_SimpleObjects.StringGrid_Composite do begin
      if (RowCount >= 1) then begin
        if (row < RowCount-1) then begin// last line ?
          for i := row to RowCount-1-1 do begin
            cells[ 1,i] := cells[ 1,i+1];
            cells[ 2,i] := cells[ 2,i+1];
            cells[ 3,i] := cells[ 3,i+1];
            cells[ 4,i] := cells[ 4,i+1];
          end;
        end;
        cells[ 1,RowCount-1] := '';
        cells[ 2,RowCount-1] := '';
        cells[ 3,RowCount-1] := '';
        cells[ 4,RowCount-1] := '';
        DEC(FileCount);
        if (FileCount = 0) then begin
//          EditorMode := false;
          Options := Options - [goEditing];
        end else begin
          RowCount := FileCount+FirstRow;
        end;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_SimpleObjects.Image_TextureMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // flag this event for use with MouseUp - work-around attempt
  // cannot use onclick, because need X, Y
  // OnMouseUp by itself gets false triggers when a dialog on top gets double-cliked
  MouseDown_Flag := true;
end;

//---------------------------------------------------------------------------
end.

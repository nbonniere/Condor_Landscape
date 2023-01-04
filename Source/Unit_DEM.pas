{
 * Unit_DEM.pas
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
unit Unit_DEM;

//===========================================================================
INTERFACE

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, comctrls;

type
  TForm_DEM = class(TForm)
    GroupBox_Area: TGroupBox;
    GroupBox_ByCoord: TGroupBox;
    GroupBox_ByExtent: TGroupBox;
    RadioButton_ByCoord: TRadioButton;
    RadioButton_Extent: TRadioButton;
    GroupBox_ExtendMargin: TGroupBox;
    RadioButton_ExtendNone: TRadioButton;
    RadioButton_ExtendQuarter: TRadioButton;
    RadioButton_ExtendHalf: TRadioButton;
    RadioButton_ExtendCustom: TRadioButton;
    Edit_ExtendCustom: TEdit;
    Label_CustomKm: TLabel;
    GroupBox_Condor: TGroupBox;
    GroupBox_UTM: TGroupBox;
    GroupBoxCondorLatLong: TGroupBox;
    Edit_Zone: TEdit;
    LabelZone: TLabel;
    Label_uNorth: TLabel;
    Edit_UTMnorth: TEdit;
    Label_uSouth: TLabel;
    Edit_UTMsouth: TEdit;
    Label_uEast: TLabel;
    Edit_UTMeast: TEdit;
    Label_uWset: TLabel;
    Edit_UTMwest: TEdit;
    Button_KML: TButton;
    Button_DEM: TButton;
    Button_Make_TRN: TButton;
    Button_Make_TR3: TButton;
    Edit_ZoneNS: TEdit;
    Label_cNorth: TLabel;
    Edit_CoordLatNorth: TEdit;
    Label_cWest: TLabel;
    Label_cSouth: TLabel;
    Edit_CoordLatSouth: TEdit;
    Label_cEast: TLabel;
    Edit_CoordLongEast: TEdit;
    Label_CoordNorth: TLabel;
    Edit_ExtentNorth: TEdit;
    Label_CoordWest: TLabel;
    Edit_ExtentWest: TEdit;
    Label_CoordEast: TLabel;
    Edit_ExtentEast: TEdit;
    Label_CoordSouth: TLabel;
    Edit_ExtentSouth: TEdit;
    Label_Lat: TLabel;
    Edit_CoordLat: TEdit;
    Label_Long: TLabel;
    Edit_CoordLong: TEdit;
    Label_nKm: TLabel;
    Label_wKm: TLabel;
    Label_sKm: TLabel;
    Label_eKm: TLabel;
    Edit_CoordLongWest: TEdit;
    Label_Xkm: TLabel;
    Label_Ykm: TLabel;
    Label_sCols: TLabel;
    Label_sRows: TLabel;
    Label_TL_Lat: TLabel;
    Label_TL_Long: TLabel;
    Label_BL_Lat: TLabel;
    Label_BL_Long: TLabel;
    Label_BR_Lat: TLabel;
    Label_BR_Long: TLabel;
    Label_TR_Lat: TLabel;
    Label_TR_Long: TLabel;
    Button_CoordToUTM: TButton;
    Button_ExtentToUTM: TButton;
    Button_CheckShow: TButton;
    Image_Grid: TImage;
    RadioButton_ByTurnPoints: TRadioButton;
    GroupBox1: TGroupBox;
    Button_Browse: TButton;
    Edit_TP_Filename: TEdit;
    Label_Corners: TLabel;
    Button_Import: TButton;
    Label_rmKM: TLabel;
    GroupBox_Options: TGroupBox;
    CheckBox_TLBR: TCheckBox;
    Edit_zOffset: TEdit;
    Label_zOffset: TLabel;
    Button_Batch: TButton;
    Button_Standardize: TButton;
    procedure Button_CoordToUTMClick(Sender: TObject);
    procedure Button_CheckShowClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit_UTMnorthChange(Sender: TObject);
    procedure Button_KMLClick(Sender: TObject);
    procedure Button_DEMClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Button_Make_TR3Click(Sender: TObject);
    procedure Button_Make_TRNClick(Sender: TObject);
    procedure Button_ImportClick(Sender: TObject);
    procedure Button_ExtentToUTMClick(Sender: TObject);
    procedure RadioButton_ByCoordClick(Sender: TObject);
    procedure RadioButton_ExtentClick(Sender: TObject);
    procedure Button_BatchClick(Sender: TObject);
    procedure ShowGrid(mBitmap: Tbitmap; Columns, Rows : single);
    procedure Button_StandardizeClick(Sender: TObject);
  private
    { Private declarations }
    function  GetMargin(Sender: TObject) : double;
    procedure ClearGrid(Sender: TObject);
    function  UTM_Validate : Boolean;
  public
    { Public declarations }
  end;

type
  AreaPoints = array [0..3] of TPoint;   // desired area corners

var
  Form_DEM: TForm_DEM;

  Memo_Message : TMemo;              // external TMemo for messages
  ProgressBar_Status : TProgressBar; // external progress bar
  CurrentLandscape : string;
  File_Folder : string;              // external path for file
  library_Folder : string;           // external path for library
  Programsfolder : string;           // external path for library
  CondorVersion : string;

  Areas : array of AreaPoints;
  AreaPointsDefined : boolean;
  CropAreaPointsDefined : boolean;
  ReferencePoint: TPoint;               // reference point
  ReferencePointDefined : boolean;

  S : string[11];

//===========================================================================
IMPLEMENTATION

{$R *.DFM}

uses
  FileCtrl, Math, ShellAPI,
  u_SceneryHdr, u_TileList, u_UTM, u_MakeKML, u_Terrain, u_Exec;

const
  CondorTileSize = tColumns * Resolution;  // 23040

var
  mBitmap : TBitmap;

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
function TForm_DEM.GetMargin(Sender: TObject) : double;
begin
  if (RadioButton_ExtendQuarter.checked) then begin
    result := CondorTileSize / 4;
  end else begin
    if (RadioButton_ExtendHalf.checked) then begin
      result := CondorTileSize / 2;
    end else begin
      if (RadioButton_ExtendCustom.checked) then begin
        try
          result := 1000 * StrToFloat(Edit_ExtendCustom.text);
        except
          MessageShow('Invalid custom range margin');
          Beep; Exit;
        end;
      end else begin // none
        result := 0;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
Procedure SaveTempFile;
var
  Temp_File : TextFile;
begin
  AssignFile(Temp_file, File_folder+'\LatLong.txt');
  Rewrite(Temp_file);
  writeln(Temp_file, Form_DEM.Edit_CoordLatNorth.text);
  writeln(Temp_file, Form_DEM.Edit_CoordLatSouth.text);
  writeln(Temp_file, Form_DEM.Edit_CoordLongWest.text);
  writeln(Temp_file, Form_DEM.Edit_CoordLongEast.text);
  writeln(Temp_file, Form_DEM.Edit_Zone.Text);
  writeln(Temp_file, Form_DEM.Edit_ZoneNS.Text);
  writeln(Temp_file, Form_DEM.Edit_UTMnorth.Text);
  writeln(Temp_file, Form_DEM.Edit_UTMsouth.Text);
  writeln(Temp_file, Form_DEM.Edit_UTMwest.Text);
  writeln(Temp_file, Form_DEM.Edit_UTMeast.Text);
  writeln(Temp_file, Form_DEM.Edit_zOffset.Text);
  writeln(Temp_file, Form_DEM.Edit_CoordLat.text);
  writeln(Temp_file, Form_DEM.Edit_CoordLong.text);
  writeln(Temp_file, Form_DEM.Edit_ExtentNorth.text);
  writeln(Temp_file, Form_DEM.Edit_ExtentSouth.text);
  writeln(Temp_file, Form_DEM.Edit_ExtentWest.text);
  writeln(Temp_file, Form_DEM.Edit_ExtentEast.text);
  CloseFile(Temp_file);
end;

//---------------------------------------------------------------------------
procedure TForm_DEM.Button_CoordToUTMClick(Sender: TObject);
const
  Margin_Allowance = 100;  // don't care if 100 m over
var
  LatNorth, LatSouth, LongWest, LongEast : double;
  ZoneOffset : integer;
  UTMmidX, UTMmidY : double;
  dX, dY : double;
  Columns, Rows : integer;
  Margin : double;
  Temp_File : TextFile;
begin
  try
    LatNorth := StrToFloat(Edit_CoordLatNorth.text);
    LatSouth := StrToFloat(Edit_CoordLatSouth.text);
    LongWest := StrToFloat(Edit_CoordLongWest.text);
    LongEast := StrToFloat(Edit_CoordLongEast.text);

    ZoneOffset := StrToInt(Edit_zOffset.text);
  except
    MessageShow('DEM: Lat/long/zone entry error');
    Beep;
    exit;
  end;

  // validate
  if ( (LatNorth < LatSouth)
   OR (LatNorth > 84) OR (LatNorth < -80)
   OR (LatSouth > 84) OR (LatSouth < -80) ) then begin
    MessageShow('DEM: Latitude entry error');
    Beep;
    exit;
  end;
  if ( (LongEast < LongWest)
   OR (LongEast > 180) OR (LongEast < -180)
   OR (LongWest > 180) OR (LongWest < -180) ) then begin
    MessageShow('DEM: Longitude entry error');
    Beep;
    exit;
  end;

  // calc UTM zone
  CalcUTMzone((LatNorth+LatSouth)/2, (LongWest+LongEast)/2,ZoneOffset);
  UTM_Zone   := uZone;
  UTM_ZoneNS := uGrid;

  // calc max extents (corners)
  // start with Top-Left and Bottom-Right
  LatLongToUTM(LatNorth, LongWest, uZone, uGrid);
  // Area 0 is used for the whole grid range
  // Area 1 is used for the flying extent
  // so use Area 2
  SetLength(Areas,3);
  Areas[2][0].X := round(uEasting); Areas[2][0].Y := round(uNorthing);
  UTM_Top := uNorthing;
  UTM_Left  := uEasting;
  LatLongToUTM(LatSouth, LongEast, uZone, uGrid);
  Areas[2][2].X := round(uEasting); Areas[2][2].Y := round(uNorthing);
  UTM_Bottom := uNorthing;
  UTM_Right  := uEasting;

  // need to check all corners since it depends on the 'projection'
  LatLongToUTM(LatNorth, LongEast, uZone, uGrid);
  Areas[2][1].X := round(uEasting); Areas[2][1].Y := round(uNorthing);
  if (NOT CheckBox_TLBR.checked AND (UTM_Top < uNorthing)) then begin
    UTM_Top := uNorthing;
  end;
  if (NOT CheckBox_TLBR.checked AND (UTM_Right < uEasting)) then begin
    UTM_Right  := uEasting;
  end;
  LatLongToUTM(LatSouth, LongWest, uZone, uGrid);
  Areas[2][3].X := round(uEasting); Areas[2][3].Y := round(uNorthing);
  if (NOT CheckBox_TLBR.checked AND (UTM_Bottom > uNorthing)) then begin
    UTM_Bottom := uNorthing;
  end;
  if (NOT CheckBox_TLBR.checked AND (UTM_Left > uEasting)) then begin
    UTM_Left  := uEasting;
  end;

  // calc number of tiles
  dX := UTM_Right - UTM_Left - Margin_Allowance;
  dY := UTM_Top - UTM_Bottom - Margin_Allowance;
  Columns := round( (dX + CondorTileSize/2) / CondorTileSize);
  Rows := round( (dY + CondorTileSize/2) / CondorTileSize);

  // if not enough margin, extend the number of tiles
  Margin := 2 * GetMargin(Sender);
  if (Columns * CondorTileSize < dX + Margin - Margin_Allowance) then begin
    Columns := round( (dX + Margin + CondorTileSize/2) / CondorTileSize);
  end;
  if (Rows * CondorTileSize < dY + Margin - Margin_Allowance) then begin
    Rows := round( (dY + Margin + CondorTileSize/2) / CondorTileSize);
  end;

  //extend evenly around area
  UTMmidX := (UTM_Left+UTM_Right)/2;
  UTMmidY := (UTM_Top+UTM_Bottom)/2;
  // NOTE: use bottom-right as reference
  // since elevation points are at centre of points. the result
  // is offset by 1/2 of 90m or 30m from corner of desired area
  // and could be fixed if not TLBR
//  if (NOT CheckBox_TLBR.checked) then begin
//  offset = (-45,+45) or (-15,+15)
//  end;
  UTM_Right := round(UTMmidX + Columns * CondorTileSize / 2);
  UTM_Bottom := round(UTMmidY - Rows * CondorTileSize / 2);
  UTM_Left := UTM_Right - Columns * CondorTileSize;
  UTM_Top := UTM_Bottom + Rows * CondorTileSize;

  // show results
  Edit_Zone.Text   := UTM_Zone;
  Edit_ZoneNS.Text := UTM_ZoneNS;
  Edit_UTMnorth.Text := format('%d',[trunc(UTM_Top)]);
  Edit_UTMsouth.Text := format('%d',[trunc(UTM_Bottom)]);
  Edit_UTMwest.Text  := format('%d',[trunc(UTM_Left)]);
  Edit_UTMeast.Text  := format('%d',[trunc(UTM_Right)]);

  AreaPointsDefined:= true; // after UTM edit boxes filled

  // save in temporary file
  SaveTempFile;
end;

//---------------------------------------------------------------------------
procedure TForm_DEM.Button_ExtentToUTMClick(Sender: TObject);
const
  Margin_Allowance = 100;  // don't care if 100 m over
var
  LatRef, LongRef : double;
  ExtentNorth, ExtentSouth, ExtentWest, ExtentEast : double;
  ZoneOffset : integer;
  UTMmidX, UTMmidY : double;
  dX, dY : double;
  Columns, Rows : integer;
  Margin : double;
  Temp_File : TextFile;
begin
  try
    LatRef := StrToFloat(Edit_CoordLat.text);
    LongRef := StrToFloat(Edit_CoordLong.text);
    ExtentNorth := StrToFloat(Edit_ExtentNorth.text);
    ExtentSouth := StrToFloat(Edit_ExtentSouth.text);
    ExtentWest := StrToFloat(Edit_ExtentWest.text);
    ExtentEast := StrToFloat(Edit_ExtentEast.text);

    ZoneOffset := StrToInt(Edit_zOffset.text);
  except
    MessageShow('DEM: Lat/long/zone entry error');
    Beep;
    exit;
  end;

  // validate
  if ( (LatRef > 84) OR (LatRef < -80) ) then begin
    MessageShow('DEM: Latitude entry error');
    Beep;
    exit;
  end;
  if ( (LongRef > 180) OR (LongRef < -180) ) then begin
    MessageShow('DEM: Longitude entry error');
    Beep;
    exit;
  end;

  // calc UTM zone
  CalcUTMzone(LatRef, LongRef, ZoneOffset);
  UTM_Zone   := uZone;
  UTM_ZoneNS := uGrid;

  // calc reference point and max extents (corners)
  // start with Top-Left and Bottom-Right
  LatLongToUTM(LatRef, LongRef, uZone, uGrid);
  ReferencePoint.X := round(uEasting); ReferencePoint.Y := round(uNorthing); // reference point

  UTM_Top := uNorthing + ExtentNorth * 1000;
  UTM_Left  := uEasting - ExtentWest * 1000;
  // Area 0 is used for the whole grid range
  // Area 1 is used for the flying extent
  // so use Area 2
  SetLength(Areas,3);
  Areas[2][0].X := round(UTM_Left); Areas[2][0].Y := round(UTM_Top);
  UTM_Bottom := uNorthing - ExtentSouth * 1000;
  UTM_Right  := uEasting + ExtentEast * 1000;
  Areas[2][2].X := round(UTM_Right); Areas[2][2].Y := round(UTM_Bottom);

  // need all corners, Top-Right, Bottom-Left
  Areas[2][1].X := Areas[2][2].X; Areas[2][1].Y := Areas[2][0].Y;
  Areas[2][3].X := Areas[2][0].X; Areas[2][3].Y := Areas[2][2].Y;

  // calc number of tiles
  dX := UTM_Right - UTM_Left - Margin_Allowance;
  dY := UTM_Top - UTM_Bottom - Margin_Allowance;
  Columns := round( (dX + CondorTileSize/2) / CondorTileSize);
  Rows := round( (dY + CondorTileSize/2) / CondorTileSize);

  // if not enough margin, extend the number of tiles
  Margin := 2 * GetMargin(Sender);
  if (Columns * CondorTileSize < dX + Margin - Margin_Allowance) then begin
    Columns := round( (dX + Margin + CondorTileSize/2) / CondorTileSize);
  end;
  if (Rows * CondorTileSize < dY + Margin - Margin_Allowance) then begin
    Rows := round( (dY + Margin + CondorTileSize/2) / CondorTileSize);
  end;

  //extend evenly around area
  UTMmidX := (UTM_Left+UTM_Right)/2;
  UTMmidY := (UTM_Top+UTM_Bottom)/2;
  // NOTE: use bottom-right as reference
  // since elevation points are at centre of points. the result
  // is offset by 1/2 of 90m or 30m from corner of desired area
  // and could be fixed if not TLBR
//  if (NOT CheckBox_TLBR.checked) then begin
//  offset = (-45,+45) or (-15,+15)
//  end;
  UTM_Right := round(UTMmidX + Columns * CondorTileSize / 2);
  UTM_Bottom := round(UTMmidY - Rows * CondorTileSize / 2);
  UTM_Left := UTM_Right - Columns * CondorTileSize;
  UTM_Top := UTM_Bottom + Rows * CondorTileSize;

  // show results
  Edit_Zone.Text   := UTM_Zone;
  Edit_ZoneNS.Text := UTM_ZoneNS;
  Edit_UTMnorth.Text := format('%d',[trunc(UTM_Top)]);
  Edit_UTMsouth.Text := format('%d',[trunc(UTM_Bottom)]);
  Edit_UTMwest.Text  := format('%d',[trunc(UTM_Left)]);
  Edit_UTMeast.Text  := format('%d',[trunc(UTM_Right)]);

  AreaPointsDefined:= true; // after UTM edit boxes filled
  ReferencePointDefined:= true;

  // save in temporary file
  SaveTempFile;
end;

// allow for quarter tiles -> 0.25, 0.50, 0.75
// whole tiles from bottom right
// do differently now with area[0] as reference instead of columns and rows ???
//---------------------------------------------------------------------------
procedure TForm_DEM.ShowGrid(mBitmap: Tbitmap; Columns, Rows : single);
const
  margin = 5;
  sS = 5;
var
  MaxX, MaxY : integer;
  dX, dY : double;
  StartX, StartY : integer;
  i, j : integer;
  ScaleX, ScaleY : double;
  A: array [0..4] of TPoint;  // desired area polyline points
begin
  // determine available drawing area
  MaxX := mBitmap.Width-2*margin{-1};
  MaxY := mBitmap.Height-2*margin{-1};

  // determine size of each tile square
  if (Rows = Columns) then begin
    dX := MaxX / Columns;
    dY := dX;
    StartX := margin;
    StartY := margin;
  end else begin
    if (Rows > Columns) then begin
      dY := MaxY / Rows;
      dX := dY;
      StartX := margin + round((MaxX - dX * Columns) / 2);
      StartY := margin;
    end else begin
      dX := MaxX / Columns;
      dY := dX;
      StartX := margin;
      StartY := margin + round((MaxY - dY * Rows) / 2);
    end;
  end;

  with mBitmap do begin
    // show tile grid layout
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Mode := pmCopy;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(rect(0,0,Width,Height)); //erase first
    // start at bottom
    for i := 0 to Round(Rows+0.4)-1 do begin
      Canvas.MoveTo(StartX, StartY + round((Rows-i) * dY));
      Canvas.LineTo(StartX + round(Columns * dX), StartY + round((Rows-i) * dY));
    end;
    // last one can be fraction
    Canvas.MoveTo(StartX, StartY);
    Canvas.LineTo(StartX + round(Columns * dX), StartY);
    // start at right
    for i := 0 to Round(Columns+0.4)-1 do begin
      Canvas.MoveTo(StartX + round((Columns-i) * dX), StartY);
      Canvas.LineTo(StartX + round((Columns-i) * dX), StartY + round(Rows * dY));
    end;
    // last one can be fraction
    Canvas.MoveTo(StartX, StartY);
    Canvas.LineTo(StartX, StartY + round(Rows * dY));

    // show area(s), if defined
    Canvas.Pen.Width := 2;
    Canvas.Pen.Color := clBlue;
//    if (AreaPointsDefined) then begin  // based on desired area or UTM ?
      // Calc_XY_Scales
      ScaleX := Columns * dX / (Areas[0][2].X - Areas[0][0].X);
      ScaleY := Rows * dY / (Areas[0][0].Y - Areas[0][2].Y);
      for j := 1 to length(Areas)-1 do begin
        if (j = 1) then begin
          Canvas.Pen.Width := 1;
          Canvas.Pen.Color := clRed;
        end else begin
          if ((CropAreaPointsDefined) AND (j = length(Areas)-1)) then begin  // change color for crop area
            Canvas.Pen.Width := 2;
            Canvas.Pen.Color := clFuchsia {clAqua}{clGreen};
          end else begin
            Canvas.Pen.Width := 2;
            Canvas.Pen.Color := clBlue;
          end;
        end;
        // create scaled points
        for i := 0 to 3 do begin
          A[i].X := StartX + round( (Areas[j][i].X - Areas[0][0].X) * ScaleX );
          A[i].Y := StartY + round( (Areas[0][0].Y - Areas[j][i].Y) * ScaleY );
        end;
        A[4].X := A[0].X; A[4].Y := A[0].Y;
        Canvas.Polyline(A);
      end;
//    end;

    // show reference, if defined
    Canvas.Pen.Color := clBlue;
    if (ReferencePointDefined) then begin
      // Calc_XY_Scales
      ScaleX := Columns * dX / (Areas[0][2].X - Areas[0][0].X);
      ScaleY := Rows * dY / (Areas[0][0].Y - Areas[0][2].Y);
      // create scaled points
      A[0].X := StartX + round( (ReferencePoint.X - Areas[0][0].X) * ScaleX );
      A[0].Y := StartY + round( (Areas[0][0].Y - ReferencePoint.Y) * ScaleY );
      A[1].X := A[0].X;
      A[1].Y := A[0].Y - sS;
      A[2].X := A[0].X - sS;
      A[2].Y := A[0].y;
      A[3].X := A[0].X;
      A[3].Y := A[0].Y + sS;
      A[0].X := A[0].X + sS;
      A[0].Y := A[0].Y;
      A[4].X := A[0].X; A[4].Y := A[0].Y;
      Canvas.Polyline(A);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_DEM.ClearGrid(Sender: TObject);
begin
  // default labels
  Label_sCols.Caption  := 'Columns: xxx';
  Label_sRows.Caption  := 'Rows: xxx';
  Label_Xkm.Caption  := 'xxx Km';
  Label_Ykm.Caption  := 'xxx Km';
  Label_TL_Lat.Caption  := 'Latitude';
  Label_TL_Long.Caption := 'Longitude';
  Label_TR_Lat.Caption  := 'Latitude';
  Label_TR_Long.Caption := 'Longitude';
  Label_BL_Lat.Caption  := 'Latitude';
  Label_BL_Long.Caption := 'Longitude';
  Label_BR_Lat.Caption  := 'Latitude';
  Label_BR_Long.Caption := 'Longitude';
  // erase bitmap
  with mBitmap do begin
//    Canvas.Brush.Color := clBtnFace;
    Canvas.Brush.Color := clWhite;
    Canvas.FillRect(rect(0,0,Width,Height));
  end;
end;

//---------------------------------------------------------------------------
function TForm_DEM.UTM_Validate : Boolean;
var
  temp : integer;
begin
  result := false; // assume for now

  try
    UTM_Zone := Edit_Zone.Text;
    temp := StrToInt(UTM_Zone);
    if ( (temp <= 0) OR (temp > 60) ) then begin
      MessageShow('DEM: UTM zone not in range');
      exit;
    end;

    UTM_ZoneNS := Edit_ZoneNS.Text;
    if ( NOT ((UTM_ZoneNS = 'N') OR (UTM_ZoneNS = 'S')) ) then begin
      MessageShow('DEM: UTM zone must be ''N'' or ''S''');
      exit;
    end;

    UTM_Top := StrToFloat(Edit_UTMnorth.Text);
    UTM_Bottom := StrToFloat(Edit_UTMsouth.Text);
    UTM_Left  := StrToFloat(Edit_UTMwest.Text);
    UTM_Right  := StrToFloat(Edit_UTMeast.Text);
    if (frac((UTM_Right - UTM_Left) / CondorTileSize) <> 0) then begin
      MessageShow('DEM: UTM EW range not multiple of 23040');
      exit;
    end;
    if (frac((UTM_Top - UTM_Bottom) / CondorTileSize) <> 0) then begin
      MessageShow('DEM: UTM NS range not multiple of 23040');
      exit;
    end;
    // no errors detected
    result := true;
  except
    MessageShow('DEM: Invalid UTM data');
  end;

end;

//---------------------------------------------------------------------------
procedure TForm_DEM.Button_StandardizeClick(Sender: TObject);
var
  UTM_Width  : double;
  UTM_Height : double;
  UTM_Offset : double;
begin
  if (NOT UTM_Validate) then begin
    Beep; ClearGrid(Sender);
  end else begin
    UTM_Width  := UTM_Right - UTM_Left;
    // put on standardized Condor quarter-tile (90 m * 64)
    UTM_Offset := 500000;
    UTM_Left   := Floor( (UTM_Left - UTM_Offset + (90*64/2)) / (90*64) )*(90*64) + UTM_Offset;
    UTM_Right  := UTM_Left + UTM_Width;
    Edit_UTMwest.Text := FloatToStr(UTM_Left);
    Edit_UTMeast.Text := FloatToStr(UTM_Right);
    UTM_Height := UTM_Top - UTM_Bottom;
    // put on standardized Condor quarter-tile (90 m * 64)
    if (UTM_ZoneNS = 'N') then begin
      UTM_Offset := 0;
    end else begin
      UTM_Offset := 10000000;
    end;
    UTM_Bottom := Floor( (UTM_Bottom - UTM_Offset + (90*64/2)) / (90*64) )*(90*64) + UTM_Offset;
    UTM_Top    := UTM_Bottom + UTM_Height;
    Edit_UTMsouth.Text := FloatToStr(UTM_Bottom);
    Edit_UTMnorth.Text := FloatToStr(UTM_Top);

  end;
end;

//---------------------------------------------------------------------------
procedure TForm_DEM.Button_CheckShowClick(Sender: TObject);
var
  Columns, Rows : integer;
begin
  if (NOT UTM_Validate) then begin
    Beep; ClearGrid(Sender);
  end else begin
    // show number tile columns and rows
    Columns := round( (UTM_Right - UTM_Left) / CondorTileSize);
    Rows := round( (UTM_Top - UTM_Bottom) / CondorTileSize);
    Label_sCols.Caption  := format('Columns: %d',[Columns]);
    Label_sRows.Caption  := format('Rows: %d',[Rows]);

    //save extents
    if (Length(Areas) < 1) then begin
      SetLength(Areas,2);
    end;
  // Area 0 is used for the whole grid range
    Areas[0][0].X := round(UTM_Left); Areas[0][0].Y := round(UTM_Top);
    Areas[0][2].X := round(UTM_Right); Areas[0][2].Y := round(UTM_Bottom);

  // Area 1 is used for the flying extent
    Areas[1][0].X := round(UTM_Left + 90 * 64);
    Areas[1][0].Y := round(UTM_Top - 90 * 64);
    Areas[1][2].X := round(UTM_Right - 90 * 64);
    Areas[1][2].Y := round(UTM_Bottom + 90 * 64);
    Areas[1][1].X := Areas[1][2].X; Areas[1][1].Y := Areas[1][0].Y;
    Areas[1][3].X := Areas[1][0].X; Areas[1][3].Y := Areas[1][2].Y;

    // show distances
    Label_Xkm.Caption  := format('%0.2f Km',[Columns*CondorTileSize/1000]);
    Label_Ykm.Caption  := format('%0.2f Km',[Rows*CondorTileSize/1000]);

    // centre of elevation points !
    UTMtoLatLong(UTM_Top, UTM_Left, UTM_Zone, UTM_ZoneNS);
    Label_TL_Lat.Caption  := format('%0.5f',[uLatitude]);
    Label_TL_Long.Caption := format('%0.5f',[uLongitude]);
    UTMtoLatLong(UTM_Top, UTM_Right, UTM_Zone, UTM_ZoneNS);
    Label_TR_Lat.Caption  := format('%0.5f',[uLatitude]);
    Label_TR_Long.Caption := format('%0.5f',[uLongitude]);
    UTMtoLatLong(UTM_Bottom, UTM_Left, UTM_Zone, UTM_ZoneNS);
    Label_BL_Lat.Caption  := format('%0.5f',[uLatitude]);
    Label_BL_Long.Caption := format('%0.5f',[uLongitude]);
    UTMtoLatLong(UTM_Bottom, UTM_Right, UTM_Zone, UTM_ZoneNS);
    Label_BR_Lat.Caption  := format('%0.5f',[uLatitude]);
    Label_BR_Long.Caption := format('%0.5f',[uLongitude]);

    // show the tile grid, flyable area and desired area
    ShowGrid(mBitmap, Columns, Rows);

    // save in temporary file
    SaveTempFile;
  end;
end;

// if any UTM parameter is changed manually, defined area = false
//---------------------------------------------------------------------------
procedure TForm_DEM.Edit_UTMnorthChange(Sender: TObject);
begin
  if (Sender is TEdit) then begin
    if (TEdit(Sender).Modified) then begin
      AreaPointsDefined := false;
      CropAreaPointsDefined := false;
      ReferencePointDefined:= false;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_DEM.Button_KMLClick(Sender: TObject);
begin
  if (NOT UTM_Validate) then begin
    Beep;                                 // use scenery vars, i.e.
  end else begin                          // why make copies ? go direct ?
    TileRowCount := round( (UTM_Top - UTM_Bottom) / 256 / 90);
    TileColumnCount := round( (UTM_Right - UTM_Left) / 256 / 90);
    HeaderOpen := true;

    u_TileList.ProgressBar_Status := ProgressBar_Status;

    MakeTileList(UTM_Right, UTM_Bottom); // kludge for now - no offset

    // ADD Desired area ??? TBD

    u_MakeKML.Memo_Message := Memo_Message;
    if (NOT DirectoryExists(File_Folder)) then begin
      ForceDirectories(File_Folder);
    end;
    u_MakeKML.KMLfolder := File_Folder;
    OverallFolder := 'KML';
    MakeOverallKML(0,0,0,0);
  end;
  HeaderOpen := false;  // no longer valid on exit
  TileOpen := false;    // no longer valid on exit
end;

//---------------------------------------------------------------------------
procedure TForm_DEM.Button_DEMClick(Sender: TObject);
var
  i, j, k : integer;
  Lat_Min, Lat_Max, Long_Min, Long_Max : double;
  HGT_Lat_Min, HGT_Lat_Max, HGT_Long_Min, HGT_Long_Max : integer;
  DEM_File : TextFile;
  URL_File : TextFile;
  SS, DD : string;

//---------------------------------------------------------------------------
function HGT_Name(i,j : integer): string;
var
  S : string;
begin
        S := format('N%2.2dE%3.3d',[abs(i),abs(j)]);
        if (i < 0) then begin // if south replace N by S
          S[1] := 'S';
        end;
        if (j < 0) then begin // if west replace E by W
          S[4] := 'W';
        end;
  result := S;
end;

//---------------------------------------------------------------------------
begin
  if (NOT UTM_Validate) then begin
    Beep;
  end else begin
    // first, using lat/long extents, determine how many HGT tiles are needed
    UTMtoLatLong(UTM_Top, UTM_Left, UTM_Zone, UTM_ZoneNS);
    Lat_Max := uLatitude;
    Long_Min := uLongitude;
    UTMtoLatLong(UTM_Top, UTM_Right, UTM_Zone, UTM_ZoneNS);
    if (Lat_Max < uLatitude) then begin
      Lat_Max := uLatitude;
    end;
    Long_Max := uLongitude;
    UTMtoLatLong(UTM_Bottom, UTM_Left, UTM_Zone, UTM_ZoneNS);
    Lat_Min := uLatitude;
    if (Long_min > uLongitude) then begin
      Long_min := uLongitude;
    end;
    UTMtoLatLong(UTM_Bottom, UTM_Right, UTM_Zone, UTM_ZoneNS);
    if (Lat_Min > uLatitude) then begin
      Lat_Min := uLatitude;
    end;
    if (Long_Max < uLongitude) then begin
      Long_Max := uLongitude;
    end;

    HGT_Lat_Min  := Floor(Lat_Min);
    HGT_Lat_Max  := Floor(Lat_Max);
    HGT_Long_Min := Floor(Long_Min);
    HGT_Long_Max := Floor(Long_Max);

    // create a file to download the HGT files
    AssignFile(DEM_file, File_folder+'\DEM_WGET.bat');
    Rewrite(DEM_file);

    writeln(DEM_file,'@echo off');
    writeln(DEM_file,'setlocal');
    writeln(DEM_file,'set PATH=%PATH%;c:\programs\wget');
    writeln(DEM_file,'rem goto directory where batch file is');
    writeln(DEM_file,'cd /d %~dp0');
//    writeln(DEM_file, 'wget --http-user=uuuu --http-password=pppp -i URLs.txt');
    writeln(DEM_file, 'echo USGS SRTM data download');
    writeln(DEM_file, 'set uuuu=user');
    writeln(DEM_file, 'set pppp=password');
    writeln(DEM_file, 'if %uuuu% EQU user (set /p uuuu= User Name: )');
    writeln(DEM_file, 'if %pppp% EQU password (set /p pppp= Password: )');
    writeln(DEM_file, 'wget --http-user=%uuuu% --http-password=%pppp% -i URLs.txt');

    AssignFile(URL_file, File_folder+'\URLs.txt');
    Rewrite(URL_file);
    for i := HGT_Lat_Min to HGT_Lat_Max do begin
      for j := HGT_Long_Min to HGT_Long_Max do begin
        writeln(URL_file, 'https://e4ftl01.cr.usgs.gov/MEASURES/SRTMGL1.003/2000.02.11/'+HGT_Name(i,j)+'.SRTMGL1.hgt.zip');
      end;
    end;
    CloseFile(URL_file);

//    writeln(DEM_file,'pause');
    writeln(DEM_file,'endlocal');
    // close the file
    CloseFile(DEM_file);

    // create a file to download the HGT files
    AssignFile(DEM_file, File_folder+'\DEM_Extract.bat');
    Rewrite(DEM_file);

    writeln(DEM_file,'@echo off');
    writeln(DEM_file,'setlocal');
    writeln(DEM_file,'set PATH=%PATH%;c:\programs\7-zip');
    writeln(DEM_file,'rem set PATH=%PATH%;"'+ProgramsFolder+'\geotiff"');
    writeln(DEM_file,'rem goto directory where batch file is');
    writeln(DEM_file,'cd /d %~dp0');

    for i := HGT_Lat_Min to HGT_Lat_Max do begin
      for j := HGT_Long_Min to HGT_Long_Max do begin
        writeln(DEM_file, '7z e '+HGT_Name(i,j)+'.SRTMGL1.hgt.zip');
        // alternate Win10 has tar
        // writeln(DEM_file, 'tar -xf '+HGT_Name(i,j)+'.SRTMGL1.hgt.zip');
      end;
    end;

//  writeln(DEM_file,'pause');
//  writeln(DEM_file,'exit 0');
    writeln(DEM_file,'endlocal');
    // close the file
    CloseFile(DEM_file);

    // then create a batch file that will convert the HGT files to DEM_Tiff
    AssignFile(DEM_file, File_folder+'\DEM.bat');
    Rewrite(DEM_file);

    writeln(DEM_file,'rem @echo off');
    writeln(DEM_file,'setlocal');
    writeln(DEM_file,'set PATH=%PATH%;"'+library_Folder+'"');
    writeln(DEM_file,'set GDAL_DATA='+library_Folder+'\..\share\epsg_csv');
    writeln(DEM_file,'rem goto directory where batch file is');
    writeln(DEM_file,'cd /d %~dp0');
    writeln(DEM_file,'rem convert HGT file to GeoTiff');

    MessageClear;
    MessageShow('DEM: Will need to download the following STRM files:');
    k := 0;
    for i := HGT_Lat_Min to HGT_Lat_Max do begin
      for j := HGT_Long_Min to HGT_Long_Max do begin
        S := HGT_Name(i,j)+'.hgt';
        MessageShow(S);
        writeln(DEM_file,format('set sourceHGT=%s',[S]));
        writeln(DEM_file,format('set destinationTIFF=T%d.tif',[k]));
        writeln(DEM_file,'gdal_translate -of GTiff %sourceHGT% %destinationTIFF%');
        INC(k,1)
      end;
    end;

    // then assemble these tiff files together
    // first in rows
    k := 0;
    for i := 0 to (HGT_Lat_Max - HGT_Lat_Min) do begin
       SS := ''; DD := format('R%d.tif ',[i]);
      for j := 0 to (HGT_Long_Max - HGT_Long_Min) do begin
        SS := SS + format('T%d.tif ',[k]); INC(k,1);
      end;
      writeln(DEM_file,'gdalwarp '+ SS + DD);
    end;
    // then in columns
    SS := '';
    for i := 0 to (HGT_Lat_Max - HGT_Lat_Min) do begin
      SS := SS + format('R%d.tif ',[i]);
    end;
    writeln(DEM_file,'gdalwarp '+ SS + 'Overall.tif');
    writeln(DEM_file,'del T*.tif'); // no longer need
    writeln(DEM_file,'del R*.tif'); // no longer need

    // then warp to UTM and crop
    writeln(DEM_file,'rem create a DEM with the desired UTM easting and northing');
    writeln(DEM_file,'rem crop to desired UTM coordinates');
    writeln(DEM_file,'set utm_zone='+UTM_Zone);
    if (UTM_ZoneNS = 'N') then begin
      writeln(DEM_file,'set utm_grid=north');
    end else begin
      writeln(DEM_file,'set utm_grid=south');
    end;
    writeln(DEM_file,format('set utm_left=%1.0f',[UTM_Left]));
    writeln(DEM_file,format('set utm_bottom=%1.0f',[UTM_Bottom]));
    writeln(DEM_file,format('set utm_right=%1.0f',[UTM_Right]));
    writeln(DEM_file,format('set utm_top=%1.0f',[UTM_Top]));
    // can use resolution -tr or size -ts
    if (CondorVersion = 'V1') then begin
      writeln(DEM_file,'rem set resolution to 90 m for Condor V1');
      writeln(DEM_file,'set res= 90 90');
    end else begin
      writeln(DEM_file,'rem set resolution to 30 m for Condor V2');
      writeln(DEM_file,'set res= 30 30');
    end;
    writeln(DEM_file,'set sourceTIFF=Overall.tif');
    writeln(DEM_file,'set destinationTIFF=UTM_cropped.tif');
    writeln(DEM_file,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
    writeln(DEM_file,'gdalwarp.exe -r lanczos -of GTiff -s_srs WGS84 -tr %RES% -t_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -te %utm_left% %utm_bottom% %utm_right% %utm_top% %sourcetiff% %destinationtiff%');

    if (CondorVersion = 'V1') then begin // also keep/create 90m version
      writeln(DEM_file,'copy %destinationTIFF% UTM_cropped_90m.tif');
    end else begin
      writeln(DEM_file,'set destinationTIFF=UTM_cropped_90m.tif');
      writeln(DEM_file,'set res= 90 90');
      writeln(DEM_file,'if exist %destinationTIFF% del %destinationTIFF%'); // if already present
      writeln(DEM_file,'gdalwarp.exe -r lanczos -of GTiff -s_srs WGS84 -tr %RES% -t_srs "+proj=utm +zone=%utm_zone% +%utm_grid% +datum=WGS84" -te %utm_left% %utm_bottom% %utm_right% %utm_top% %sourcetiff% %destinationtiff%');
    end;
    writeln(DEM_file,'del %sourceTIFF%'); // no longer need

    // then convert the cropped GeoTiff file to RAW for use with Condor
    writeln(DEM_file,'rem create a DEM with the desired UTM easting and northing');
    writeln(DEM_file,'set sourceTIFF=UTM_cropped.tif');
    writeln(DEM_file,'set destinationDEM=UTM_cropped.raw');
// 2 ways to do it, ENVI or EHdr, same result but different HDR type results
//    writeln(DEM_file,'gdal_translate -of EHdr %sourceTIFF% %destinationDEM%');
//    writeln(DEM_file,'rem hdr reference tile center');
//    writeln(DEM_file,'rem will need to flip vertical');
    writeln(DEM_file,'gdal_translate -of ENVI %sourceTIFF% %destinationDEM%');
    writeln(DEM_file,'del %sourceTIFF%'); // no longer need

    writeln(DEM_file,'rem hdr reference tile corner');
    writeln(DEM_file,'rem will need to flip vertical');
//    writeln(DEM_file,'endlocal');

    // also convert the 90m cropped GeoTiff file to RAW for use with Condor
    writeln(DEM_file,'rem create a DEM with the desired UTM easting and northing');
    writeln(DEM_file,'set sourceTIFF=UTM_cropped_90m.tif');
    writeln(DEM_file,'set destinationDEM=UTM_cropped_90m.raw');
// 2 ways to do it, ENVI or EHdr, same result but different HDR type results
//    writeln(DEM_file,'gdal_translate -of EHdr %sourceTIFF% %destinationDEM%');
//    writeln(DEM_file,'rem hdr reference tile center');
//    writeln(DEM_file,'rem will need to flip vertical');
    writeln(DEM_file,'gdal_translate -of ENVI %sourceTIFF% %destinationDEM%');

    writeln(DEM_file,'rem hdr reference tile corner');
    writeln(DEM_file,'rem will need to flip vertical');
    writeln(DEM_file,'endlocal');

    // close the file
    CloseFile(DEM_file);

    // show data needed for RAW-to-Terrain
    // UTM zone, one UTM corner, rows and columns
    if (CondorVersion = 'V1') then begin
      MessageShow('DEM: Condor V1 batch file created.');
      RowCount := round((UTM_Top - UTM_Bottom) / 90);
      ColumnCount := round((UTM_Right - UTM_Left) / 90);
      MessageShow(format('DEM: Columns: %d',[ColumnCount]));
      MessageShow(format('DEM: Rows: %d',[RowCount]));
    end else begin
      MessageShow('DEM: Condor V2 batch file created.');
      RowCount := round((UTM_Top - UTM_Bottom) / 30);
      ColumnCount := round((UTM_Right - UTM_Left) / 30);
      MessageShow(format('DEM: Columns: %d',[ColumnCount]));
      MessageShow(format('DEM: Rows: %d',[RowCount]));
    end;
    MessageShow('DEM: UTM_Zone: '+UTM_Zone+' '+ UTM_ZoneNS);
    MessageShow(format('DEM: UTM_Right: %1.0f',[(UTM_Right)]));
    MessageShow(format('DEM: UTM_Bottom: %1.0f',[(UTM_Bottom)]));

    // ??? if V2. offset by 30 m to use 90 m elevation centre
    if (CondorVersion <> 'V1') then begin
      MessageShow(format('DEM: 90m Terrain UTM_Right: %1.0f',[(UTM_Right-30)]));
      MessageShow(format('DEM: 90m Terrain UTM_Bottom: %1.0f',[(UTM_Bottom+30)]));
    end;

    // also create a header file
//    Create_DEM_HeaderFile;
    NumColumns := ColumnCount;  // depends on V1 or V2 ?
    NumRows    := RowCount;
    DEM_Res := round ( (UTM_Right - UTM_Left) / (NumColumns));
    WriteSceneryHeader(File_Folder+'\scenery.hdr');
  end;
  HeaderOpen := false;  // no longer valid on exit
  TileOpen := false;    // no longer valid on exit
end;

//---------------------------------------------------------------------------
procedure TForm_DEM.Button_Make_TR3Click(Sender: TObject);
var
  Columns, Rows : integer;
begin
  if (NOT UTM_Validate) then begin
    Beep;
  end else begin
    Columns := round( (UTM_Right - UTM_Left) / 90);
    Rows := round( (UTM_Top - UTM_Bottom) / 90);
    with u_Terrain.TerrainHeader do begin
      tWidth := Columns;                // number of columns of tiles
      tHeight := Rows;                  // number of rows of tiles
      tResolution := 90;                // 90m
      tDeltaX := -90;                   // actual vertical resolution in m (calibrated)
      tDeltaY := 90;                    // actual horizontal resolution in m (calibrated)
      tRightMapEasting := UTM_Right;    // UTM absolute Easting, bottom right
      tBottomMapNorthing := UTM_Bottom; // UTM absolute Northing, bottom right
      tUTMzone := StrToInt(UTM_Zone);   // UTM zone number
      tUTMgrid[0] := UTM_ZoneNS[1];     // UTM zone grid (A..Z or N/S) (only first char used (?))
    end;
  end;
  u_Terrain.Memo_Message := Memo_Message;
  u_Terrain.ProgressBar_Status := ProgressBar_Status;
  RAW_To_TR3(File_folder+'\UTM_cropped.RAW', File_folder+'\..\..\HeightMaps');
end;

//---------------------------------------------------------------------------
procedure TForm_DEM.Button_Make_TRNClick(Sender: TObject);
var
  Columns, Rows : integer;
begin
  if (NOT UTM_Validate) then begin
    Beep;
  end else begin
    Columns := round( (UTM_Right - UTM_Left) / 90);
    Rows := round( (UTM_Top - UTM_Bottom) / 90);
    with u_Terrain.TerrainHeader do begin
      tWidth := Columns;                // number of columns of tiles
      tHeight := Rows;                  // number of rows of tiles
      tResolution := 90;                // 90m
      tDeltaX := -90;                   // actual vertical resolution in m (calibrated)
      tDeltaY := 90;                    // actual horizontal resolution in m (calibrated)
      tRightMapEasting := UTM_Right;    // UTM absolute Easting, bottom right
      tBottomMapNorthing := UTM_Bottom; // UTM absolute Northing, bottom right
      tUTMzone := StrToInt(UTM_Zone);   // UTM zone number
      tUTMgrid[0] := UTM_ZoneNS[1];     // UTM zone grid (A..Z or N/S) (only first char used (?))
    end;
  end;
  u_Terrain.Memo_Message := Memo_Message;
  u_Terrain.ProgressBar_Status := ProgressBar_Status;
  RAW_To_TRN(File_folder+'\UTM_cropped_90m.RAW', File_folder+'\..\..\'+CurrentLandscape+'.trn');
end;

// only enable button if terrain file is available
//---------------------------------------------------------------------------
procedure TForm_DEM.Button_ImportClick(Sender: TObject);
begin
  if (HeaderOpen) AND (TileOpen) then begin
    // read terrain header
    u_Terrain.Memo_Message := Memo_Message;
    u_Terrain.ProgressBar_Status := ProgressBar_Status;
    ReadTerrainHeader(File_folder+'\..\..\'+CurrentLandscape+'.trn');

    with u_Terrain.TerrainHeader do begin
      UTM_Right := tRightMapEasting;    // UTM absolute Easting, bottom right
      UTM_Left := UTM_Right - tWidth * 90;
      UTM_Bottom := tBottomMapNorthing; // UTM absolute Northing, bottom right
      UTM_Top := UTM_Bottom + tHeight * 90;
      UTM_Zone := IntToStr(tUTMzone);   // UTM zone number
      UTM_ZoneNS[1] := tUTMgrid[0];     // UTM zone grid (A..Z or N/S) (only first char used (?))
    end;

    Edit_Zone.Text   := UTM_Zone;
    Edit_ZoneNS.Text := UTM_ZoneNS;
    Edit_UTMnorth.Text := format('%d',[trunc(UTM_Top)]);
    Edit_UTMsouth.Text := format('%d',[trunc(UTM_Bottom)]);
    Edit_UTMwest.Text  := format('%d',[trunc(UTM_Left)]);
    Edit_UTMeast.Text  := format('%d',[trunc(UTM_Right)]);

    AreaPointsDefined := false;
    CropAreaPointsDefined := false;
    ReferencePointDefined := false;
  end else begin
    MessageShow('Need Header file first');
    Beep;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_DEM.RadioButton_ByCoordClick(Sender: TObject);
begin
  GroupBox_ByCoord.Enabled := true;
  GroupBox_ByExtent.Enabled := false;
end;

//---------------------------------------------------------------------------
procedure TForm_DEM.RadioButton_ExtentClick(Sender: TObject);
begin
  GroupBox_ByCoord.Enabled := false;
  GroupBox_ByExtent.Enabled := true;
end;

//---------------------------------------------------------------------------
Procedure Execute_BatchFile(FilePath, FileName, Params : String);
var
  ExitCode : DWORD;
begin
  ExitCode := Shell_Execute(FilePath, FileName, Params, true);
  case ExitCode of
    DWORD(-1): begin
      MessageShow('ERROR - Cannot execute Batch file');
    end;
    0: begin
      MessageShow('Batch file done');
    end;
    else begin
      MessageShow(format('ERROR batch file exit code= %d',[ExitCode]));
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

//---------------------------------------------------------------------------
Procedure Do_All_BatchFiles;
begin
  Execute_BatchFile(File_Folder, 'DEM_WGET.bat', '');
  Execute_BatchFile(File_Folder, 'DEM_Extract.bat', '');
  Execute_BatchFile(File_Folder, 'DEM.bat', '');
end;

//---------------------------------------------------------------------------
procedure TForm_DEM.Button_BatchClick(Sender: TObject);
begin
  Do_All_BatchFiles;
end;

//---------------------------------------------------------------------------
procedure TForm_DEM.FormCreate(Sender: TObject);
begin
  AreaPointsDefined := false;
  ReferencePointDefined := false;
  Image_Grid.Picture.Bitmap.Create; // must be square for current code !
  Image_Grid.Picture.Bitmap.Width := Image_Grid.Width;
  Image_Grid.Picture.Bitmap.Height := Image_Grid.HeigHt;
  mBitmap := Image_Grid.Picture.Bitmap;
  SetWindowLong(Button_Batch.Handle, GWL_STYLE,
  GetWindowLong(Button_Batch.Handle, GWL_STYLE) or BS_MULTILINE);
  Button_Batch.caption := 'Execute Batch Files';
end;

//---------------------------------------------------------------------------
procedure TForm_DEM.FormActivate(Sender: TObject);
var
  Temp_File : TextFile;
  TempSTR : string;
begin
  if (NOT DirectoryExists(File_Folder)) then begin
    ForceDirectories(File_Folder);
  end;
  // read in temporary file
  if (FileExists(File_folder+'\LatLong.txt')) then begin
    AssignFile(Temp_file, File_folder+'\LatLong.txt');
    Reset(Temp_file);
    Readln(Temp_file, TempSTR); Edit_CoordLatNorth.text := TempSTR;
    Readln(Temp_file, TempSTR); Edit_CoordLatSouth.text := TempSTR;
    Readln(Temp_file, TempSTR); Edit_CoordLongWest.text := TempSTR;
    Readln(Temp_file, TempSTR); Edit_CoordLongEast.text := TempSTR;
    Readln(Temp_file, TempSTR); Edit_Zone.Text := TempSTR;
    Readln(Temp_file, TempSTR); Edit_ZoneNS.Text := TempSTR;
    Readln(Temp_file, TempSTR); Edit_UTMnorth.Text := TempSTR;
    Readln(Temp_file, TempSTR); Edit_UTMsouth.Text := TempSTR;
    Readln(Temp_file, TempSTR); Edit_UTMwest.Text := TempSTR;
    Readln(Temp_file, TempSTR); Edit_UTMeast.Text := TempSTR;
    Readln(Temp_file, TempSTR); Edit_zOffset.Text := TempSTR;
    Readln(Temp_file, TempSTR); Edit_CoordLat.text := TempSTR;
    Readln(Temp_file, TempSTR); Edit_CoordLong.text := TempSTR;
    Readln(Temp_file, TempSTR); Edit_ExtentNorth.text := TempSTR;
    Readln(Temp_file, TempSTR); Edit_ExtentSouth.text := TempSTR;
    Readln(Temp_file, TempSTR); Edit_ExtentWest.text := TempSTR;
    Readln(Temp_file, TempSTR); Edit_ExtentEast.text := TempSTR;
    CloseFile(Temp_file);
  end;
  Button_Import.enabled := false;
  if ((HeaderOpen) AND (TileOpen)) then begin
    if (FileExists(File_folder+'\..\..\'+CurrentLandscape+'.trn')) then begin
      Button_Import.enabled := true;
    end;
  end;
  SetLength(Areas,0); // erase any old data
  ClearGrid(Sender);  // erase any old grid
end;

//---------------------------------------------------------------------------
begin
  Memo_Message := nil;
end.

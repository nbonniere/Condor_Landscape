{
 * u_X_CX.pas
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
UNIT u_X_CX;

{----------------------------------------------------------------------------
Condor .X and .CX coordinate encoding/decoding

see C3D at end of file
see OBJ8 at end of file
----------------------------------------------------------------------------}

//===========================================================================
INTERFACE

uses
  Graphics, Comctrls, StdCtrls,
  u_VectorXY;

//---------------------------------------------------------------------------
// for compile options
{$I Define.pas}

type
  X_CX_type = (fx, fcx, fpx);
  X_Proc = procedure(var S : String);
  Viewer_Type = (None, vSample, V1_ObjectViewer, V2_ObjectEditor);

type
  DelimiterSet = set of ','..';';

type
  oObjType = (oMagic,oHeader,oFrame,oFTM,oMesh,oMeshNormals,
              oMaterialList,oMaterial,oMaterialReference,
              oMeshTextureCoord,oFileName,
              // C3D types
              o3Dmagic,o3Dheader,
              {o3Dframe,
              o3Dmesh,o3DmeshNormals,o3DmeshTextureCoord,
              o3Dmaterial,o3DfileName,}
              // OBJ8 types
              oOBJ8magic,oOBJ8header,
              // Wavefront OBJ types
              owfOBJmagic,owfOBJheader
              );

  pIntArray = ^tIntArray;
  tIntArray = array of integer;

  pLongArray = ^tLongArray;
  tLongArray = array of LongWord;

  pFloatArray = ^tFloatArray;
//  tFloatArray = array of double;
  tFloatArray = array of single;

  pCountFloatArray = ^tCountFloatArray;
  tCountFloatArray = record
    aCount: integer;
    aArray: array of tFloatArray;
  end;

//  pCountPtrFloatArray = ^tCountPtrFloatArray;
//  tCountPtrFloatArray = record
//    aCount: integer;
//    aArray: pFloatArray;
//  end;

  pHeader = ^tHeader;
  tHeader = record
    sArray : tIntArray;
  end;

  pMagic = ^tMagic;
  tMagic = record
    tVersion: string;
    tType: string;
    tFloatType: string;
  end;

  pMaterialReference = ^tMaterialReference;
  tMaterialReference = record
    tName: string;
  end;

  pFileName = ^tpFileName;
  tpFileName = record
    tName: string;
    tqName: string;
  end;

  pMaterial = ^tMaterial;
  tMaterial = record
    tName: string;
    tRGBA: tFloatArray;  // ambient color RGBA
    tPower: Double;      // specular (reflective) exponent 0.0-1.0
    tRGBs: tFloatArray;  // specular (reflective) color RGB
    tRGBr: tFloatArray;  // emissive color RGB
//    tC3D: tFloatArray;   // Condor C3D color control
// use tRGBs instead and tPower=999 as a flag
  end;

  pMaterialList = ^tMaterialList;
  tMaterialList = record
    tName: string;
    tCount: integer;
    tsCount: integer;
    sArray : tIntArray;
  end;

  pMeshTcoord = ^tMeshTcoord;
  tMeshTcoord = record
    tName: string;
    tArray : tCountFloatArray;
    sIndex : pointer;
//    TTreeNode;  // surface array index
  end;

  pCountIntArray = ^tCountIntArray;
  tCountIntArray = record
    aCount: integer;
    aArray: array of integer;
  end;

  pCountSurfaceArray = ^tCountSurfaceArray;
  tCountSurfaceArray = record
    aCount: integer;
    aArray: array of tCountIntArray;
  end;

  pMeshNormals = ^tMeshNormals;
  tMeshNormals = record
    tName: string;
    tArray : tCountFloatArray;
    sArray : tCountSurfaceArray;
  end;

  pMesh = ^tMesh;
  tMesh = record
    tName: string;
    tMinMaxArray : array[0..9-1] of double; //Xmin, Xmax, Ymin, Ymax, Xcentre, Ycentre, Scale, Zmin, Zmax
    tRotation : double;
    tX, tY, tHeight, tScale : double;
    tArray : tCountFloatArray;
    sArray : tCountSurfaceArray;
  end;

  pFTM = ^tFTM;
  tFTM = record
    tName: string;
    ftmArray : tFloatArray;
  end;

  pFrame = ^tFrame;
  tFrame = record
    tName: string;
//    tFTMindex: integer;
  end;

  pObjectItem = ^tObjectItem;
  tObjectItem = record
    oType : oObjType;
    oPointer : pointer;
  end;

type
//  p3DfileName = ^tp3DfileName;
//  tp3DfileName = record
//    tName: string;
//    tqName: string;
//  end;

//  p3Dmaterial = ^t3Dmaterial;
//  t3Dmaterial = record
//    tRGBA: tFloatArray; // ambient color RGBA
//    tC3D: tFloatArray; // Condor Color control
//  end;

//  // option 1
//  p3DmeshTcoord = ^t3DmeshTcoord;
//  t3DmeshTcoord = record
////    tName: string;
//    tArray : tCountFloatArray;
//  end;

  // option 2
//  p3DmeshTcoord = ^t3DmeshTcoord;
//  t3DmeshTcoord = record
//    mCount : integer;
//    mArray : pointer;
//  end;

//  p3DmeshNormals = ^t3DmeshNormals;
//  t3DmeshNormals = record
////    tName: string;
//    tArray : tCountFloatArray;
//  end;

//  p3Dmesh = ^t3Dmesh;
//  t3Dmesh = record
////    tName: string;
//    tMinMaxArray : array[0..9-1] of double; //Xmin, Xmax, Ymin, Ymax, Xcentre, Ycentre, Scale, Zmin, Zmax
//    tRotation : double;
//    tX, tY, tHeight, tScale : double;
//    tArray : tCountFloatArray;
////    sArray : tCountSurfaceArray;
//  end;

//  p3Dframe = ^t3Dframe;
//  t3Dframe = record
//    tName: string;
//  end;

  p3Dheader = ^t3Dheader;
  t3Dheader = record
//    s3Darray : tLongArray;
  end;

  p3Dmagic = ^t3Dmagic;
  t3Dmagic = record
//    Rev_Major : longword;
//    Rev_Minor : longword;
  end;

var
  OBJ_Type : integer;
  Memo_Message : TMemo;  // external TMemo for messages (uses StdCtrls)
  Path : string; // external path for Condor program folder
  tvBitmap : TBitmap;
  mBitmap : TBitmap;
  oTreeView : TTreeView;

  SavedMeshData : pMesh;

  AA, BB : integer;

  InjectFTM : Boolean;
  FTM : array[0..16-1] of single =
  (1.0,0.0,0.0,0.0,
   0.0,1.0,0.0,0.0,
   0.0,0.0,1.0,0.0,
   0.0,0.0,0.0,1.0);

Procedure X_To_Normal(var S: string);
Procedure Normal_To_X(var S: string);
Procedure CX_To_Normal(var S: string);
Procedure Normal_To_CX(var S: string);

Procedure ReadCondorXfile(FileName : string; Append : boolean);
Procedure WriteCondorXfile(FileName : string; ForViewer : Viewer_Type; TC_InvertY : boolean);

Procedure PlotIt(aData : tCountFloatArray);
Procedure sPlotIt(Ptr : pointer);
//Procedure mPlotIt(aData : tCountFloatArray);
Procedure mPlotIt(Ptr, PtrMinMax : pointer);
Procedure msPlotIt(Ptr : pointer);
Procedure CalcExtents(Ptr, PtrMinMax : pointer);
Procedure NormalizeMesh(Ptr, PtrMinMax : pointer; LowestHeight : boolean);
//Procedure RotateMesh(Ptr : pointer);
Procedure RotateMesh(Node : TTreeNode);
Procedure ScaleMesh(Node : TTreeNode);
//Procedure OffsetHeight(Ptr, PtrMinMax : pointer);
Procedure OffsetHeight(Node : TTreeNode);
//Procedure TranslateMesh(Node : TTreeNode);

Procedure TranslateMesh(Node : TTreeNode; AbsoluteHeight : boolean);
Procedure ConvertForwardToBack(var S : string);
Procedure ConvertBackToForward(var S : string);
Procedure xLoadBMPfileFixAndSaveAsBMP(FileIn, FileOut : string);
Function LoadBMPfileFixAndSaveAsBMP(FileIn, FileOut : string) : Boolean;

Procedure ReadCondorC3Dfile(FileName : string; Append : boolean);
procedure Append_C3D_Details(c3dName, FileName : string);
Procedure WriteCondorC3Dfile(FileName : string);
//Procedure C3D_PlotIt(count : integer; aData : pFloatArray);
Procedure C3D_PlotIt(count : integer; aData : pointer);

Procedure ClearTreeView(oTreeView : TTreeView);
Procedure WriteCondorObjectFile(FileName : string; ForViewer : Viewer_Type; TC_InvertY : boolean);

function FindNextTexture(var NodeIndex : integer) : string;
procedure UpdateTextureFileName(NodeIndex : integer; TextureFileName : string);
procedure CopyObjectTextures (NewLandscapePath,NewLandscapeName,
                              OldLandscapePath,OldLandscapeName,
                              CurrentPath : string);

function readXplaneOBJ8file(FileName : string) : boolean;
function GetXplaneOBJ8texture : string;
procedure AdjustXplaneOBJ8texture(FileName : string);

function read_wfOBJfile(FileName : string) : boolean;

Procedure Reset_FTM_Unity;
Procedure UpdateFTM(FTMname : string ; FTM : Array of single);
Procedure UpdateTC(TCname : string; TC : Array of single);
Procedure UpdateTF(TFname : string; F : string);

function FindNodebyType(oTreeView : TTreeView;Index : integer;
                        nType : oObjType; var nName : string) : integer;
Procedure sExtract(oTreeView : TTreeView; Index : integer; var o_Object : TArray_CoordXY_Array);
Procedure vExtract(oTreeView : TTreeView; Index : integer; var o_Object : TCoordXY_Array);

//function CopyAndAddFrame(oTreeView : TTreeView; Index : integer) : integer;
//Procedure ExtractVarray(vPtr : pCountFloatArray; oTreeView : TTreeView; Index : integer);
Procedure ExtractFrame(var NewFrame : tFrame; oTreeView : TTreeView; Index : integer);
Procedure ExtractMesh(var NewMesh : tMesh; oTreeView : TTreeView; Index : integer);
Procedure ExtractNormals(var NewNormals : tMeshNormals; oTreeView : TTreeView; Index : integer);
Procedure ExtractTcoords(var NewTcoords : tMeshTcoord; oTreeView : TTreeView; Index : integer);
Procedure ExtractMaterialList(var NewMaterialList : tMaterialList; oTreeView : TTreeView; Index : integer);
Procedure ExtractMaterial(var NewMaterial : tMaterial; oTreeView : TTreeView; Index : integer);
Procedure ExtractTFileName(var NewFileName : tpFileName; oTreeView : TTreeView; Index : integer);

//===========================================================================
IMPLEMENTATION

uses Windows, FileCtrl, SysUtils, classes, extctrls,
     Unit_Objects, u_BMP, u_Util{, u_C3D}{, u_OBJ8};

const
  xMaxSigDigits = 6;   //for X - typically 5
  cxMaxSigDigits = 6;  //for CX

type
  V2_Mesh = (mNormal,mGrass,mGrassPaint,mAsphalt,mAsphlatPaint);

{----------------------------------------------------------------------------}
Procedure MessageShow(Info : string);
begin
  if assigned(Memo_Message) then begin
    Memo_Message.lines.add(Info);
  end;
end;

{----------------------------------------------------------------------------
Normally 5 significant digits plus one digit to specify the number
of digits before the decimal point
The first digit minus one indicates the number of digits before the decimal
The rest of the digits are in normal order
-312345 -> -12.345
 ----------------------------------------------------------------------------}
Procedure X_To_Normal(var S: string);
var
  i,j,lenS: integer;
  sSign : char;
  FirstDigit : integer;
  Temp : string;

begin
  lenS := length(S);
  Temp := '';
  //skip non-digits or non-sign
  i := 1;
  while ((lenS >= i) AND NOT (S[i] in ['0'..'9','-'])) do begin
    INC(i);
  end;
  if (i <= lenS) then begin
    //process sign
    j := 0;
    if (S[i] = '-') then begin
      sSign := '-';
      INC(i);
    end else begin
      sSign := ' ';
    end;
    //process first digit
    if (i <= lenS) AND (S[i] in ['0'..'9']) then begin
      FirstDigit := ord(S[i])-$30;
      INC(i);
    end else begin
      S := '0'; //error
    end;
    //process more digits
    While (i <= lenS) AND (S[i] in ['0'..'9']) do begin
      Temp := Temp + S[i];
      INC(i); INC(j);
    end;
    //insert decimal
    if (j >= FirstDigit-1) then begin
      S := sSign + copy(Temp,1,FirstDigit-1)+'.'+copy(Temp,FirstDigit-1+1,j);
    end else begin
      S := '0'; //error
    end;
  end else begin
    S := '0'; //error
  end;
end;

{----------------------------------------------------------------------------
5 significant digits possible. The range must be between 0.00001 to 99999.0
 ----------------------------------------------------------------------------}
Procedure Normal_To_X(var S: string);
var
  i,j,lenS: integer;
  sSign : char;
  {FirstDigit,} Decimal : integer;
  Temp : string;

begin
  lenS := length(S);
  Temp := '';
  //skip non-digits or non-sign or non decimal
  i := 1;
  while ((lenS >= i) AND NOT (S[i] in ['0'..'9','-','.'])) do begin
    INC(i);
  end;
  if (i <= lenS) then begin
    //process sign
    j := 0;
    if (S[i] = '-') then begin
      sSign := '-';
      INC(i);
    end else begin
      sSign := ' ';
    end;
    //skip leading 0s
    while ((lenS >= i) AND (S[i] in ['0'])) do begin
      INC(i);
    end;
    //process digits
    While ((i <= lenS) AND (S[i] in ['0'..'9']) AND (j < xMaxSigDigits)) do begin
      Temp := Temp + S[i];
      INC(i); INC(j);
    end;
    if (i > lenS) then begin // end of string
      Decimal := j;
      // extend to max digits if needed
      While j < xMaxSigDigits do begin
        Temp := Temp + '0'; INC(j);
      end;
      S := sSign+char(Decimal+1+$30)+Temp+'.0';
    end else begin // decimal or digit or invalid char
      if (S[i] = '.') then begin
        Decimal := j;
        INC(i);
        //process digits
        While ((i <= lenS) AND (S[i] in ['0'..'9']) AND (j < xMaxSigDigits)) do begin
          Temp := Temp + S[i];
          INC(i); INC(j);
        end;
        // extend to max digits if needed
        While j < xMaxSigDigits do begin
          Temp := Temp + '0'; INC(j);
        end;
        S := sSign+char(Decimal+1+$30)+Temp+'.0';
      end else begin // too many digits or invalid char
        S := '0'; //error
      end;
    end;
  end else begin
    S := '0'; //error
  end;
end;

{----------------------------------------------------------------------------
5 significant digits possible. The range must be between 0.00001 to 99999.0
 ----------------------------------------------------------------------------}
{Procedure Value_To_X(vValue:double;var S: string);
begin
  x := log(vValue);
  if x < -5.3 then 0
  if x >= 5 then error, overflow

  S := format(,%x.(5-x)f[])
  remove decimal
  add decimal count
end;
}
{----------------------------------------------------------------------------
Normally 5 significant digits plus one digit to specify the number
of digits before the decimal point
Eight minus the last digit indicates the number of digits before the decimal
The rest of the digits are in reverse order
-543216 -> -12.345
 ----------------------------------------------------------------------------}
Procedure CX_To_Normal(var S: string);
var
  i,j,lenS: integer;
  sSign : char;
  LastDigit : integer;
  Temp : string;

begin
  lenS := length(S);
  Temp := '';
  //skip non-digits or non-sign
  i := 1;
  while ((lenS >= i) AND NOT (S[i] in ['0'..'9','-'])) do begin
    INC(i);
  end;
  if (i <= lenS) then begin
    //process sign
    j := 0;
    if (S[i] = '-') then begin
      sSign := '-';
      INC(i);
    end else begin
      sSign := ' ';
    end;
    //process digits
//    While (i < lenS) AND (S[i] in ['0'..'9']) do begin
    While (i < lenS) AND (S[i] in ['0'..'9']) AND (S[i+1] in ['0'..'9']) do begin
      Temp := S[i] + Temp;
      INC(i); INC(j);
    end;
    //process last digit
    if (i <= lenS) AND (S[i] in ['0'..'9']) then begin
      LastDigit := ord(S[i])-$30;
      //INC(i);
    end else begin
      S := '0'; //error
    end;
    //insert decimal
    if (j >= 8-LastDigit) then begin
      S := sSign + copy(Temp,1,8-LastDigit)+'.'+copy(Temp,8-LastDigit+1,j);
    end else begin
      S := '0'; //error
    end;
  end else begin
    S := '0'; //error
  end;
end;

{----------------------------------------------------------------------------}
Procedure Normal_To_CX(var S: string);
var
  i,j,lenS: integer;
  sSign : char;
  {FirstDigit,} Decimal : integer;
  Temp : string;

begin
  lenS := length(S);
  Temp := '';
  //skip non-digits or non-sign or non decimal
  i := 1;
  while ((lenS >= i) AND NOT (S[i] in ['0'..'9','-','.'])) do begin
    INC(i);
  end;
  if (i <= lenS) then begin
    //process sign
    j := 0;
    if (S[i] = '-') then begin
      sSign := '-';
      INC(i);
    end else begin
      sSign := ' ';
    end;
    //skip leading 0s
    while ((lenS >= i) AND (S[i] in ['0'])) do begin
      INC(i);
    end;
    //process digits
    While ((i <= lenS) AND (S[i] in ['0'..'9']) AND (j < cxMaxSigDigits)) do begin
      Temp := S[i] + Temp;
      INC(i); INC(j);
    end;
    if (i > lenS) then begin // end of string
      Decimal := j;
      // extend to max digits if needed
      While j < cxMaxSigDigits do begin
        Temp := '0' + Temp; INC(j);
      end;
      S := sSign+Temp+char(8-Decimal+$30)+'.0';
    end else begin // decimal or digit or invalid char
      if (S[i] = '.') then begin
        Decimal := j;
        INC(i);
        //process digits
        While ((i <= lenS) AND (S[i] in ['0'..'9']) AND (j < cxMaxSigDigits)) do begin
          Temp := S[i] + Temp;
          INC(i); INC(j);
        end;
        // extend to max digits if needed
        While j < cxMaxSigDigits do begin
          Temp := '0' + Temp; INC(j);
        end;
        S := sSign+Temp+char(8-Decimal+$30)+'.0';
      end else begin // too many digits or invalid char
        S := '0'; //error
      end;
    end;
  end else begin
    S := '0'; //error
  end;
end;

{----------------------------------------------------------------------------}
Procedure Normal_To_PX(var S: string);
begin
  //do nothing
end;

{----------------------------------------------------------------------------}
Procedure PX_To_Normal(var S: string);
begin
  //do nothing
end;

{----------------------------------------------------------------------------}
var
  X_File : TextFile;
  FileError : boolean;
  xFormat : X_CX_Type;
  XCX_To_Normal : X_Proc;
  Normal_To_XCX : X_Proc;

  pObjectData : pObjectItem;
  pMagicData : pMagic;
  pHeaderData : pHeader;
  pFrameData : pFrame;
  pFTMdata : pFTM;
  pMeshData : pMesh;
  pMeshNormalsData : pMeshNormals;
  pMeshTcoordData : pMeshTcoord;
  pMaterialListData : pMaterialList;
  pMaterialData : pMaterial;
  pFileNameData : pFileName;
  pMaterialReferenceData : pMaterialReference;

{----------------------------------------------------------------------------}
Procedure xClearTreeView(oTreeView : TTreeView);
begin
  with oTreeView do begin
    while (Items.Count > 0) do begin
      Items[0].delete;  // doesn't delete allocated memory ! fix !
    end;
  end;
  oTreeView.Items.Clear; //make sure it's empty
end;

{----------------------------------------------------------------------------}
Procedure ClearTreeView(oTreeView : TTreeView);
var
  k : integer;
  Heap : THeapStatus;

begin
//// look for memory leak
//Heap := GetHeapStatus;
//MessageShow(Format('Heap: %d',[Heap.TotalAllocated]));
  with oTreeView do begin
    // walk the tree and dispose of existing memory allocations
    for k := 0 to Items.Count-1 do begin
      if (Items[k].data <> nil) then begin
        if (pObjectItem(Items[k].data)^.oPointer <> nil) then begin
          // Need to dispose by pointer type else only the pointer is disposed of.
          case pObjectItem(Items[k].data)^.oType of
            oMagic:
              dispose(pMagic(pObjectItem(Items[k].data)^.oPointer));
            oHeader:
              dispose(pHeader(pObjectItem(Items[k].data)^.oPointer));
            oFrame:
              dispose(pFrame(pObjectItem(Items[k].data)^.oPointer));
            oFTM:
              dispose(pFTM(pObjectItem(Items[k].data)^.oPointer));
            oMesh :
              dispose(pMesh(pObjectItem(Items[k].data)^.oPointer));
            oMeshNormals:
              dispose(pMeshNormals(pObjectItem(Items[k].data)^.oPointer));
            oMaterialList:
              dispose(pMaterialList(pObjectItem(Items[k].data)^.oPointer));
            oMaterial:
              dispose(pMaterial(pObjectItem(Items[k].data)^.oPointer));
            oMaterialReference:
              dispose(pMaterialReference(pObjectItem(Items[k].data)^.oPointer));
            oMeshTextureCoord:
              dispose(pMeshTcoord(pObjectItem(Items[k].data)^.oPointer));
            oFileName:
              dispose(pFileName(pObjectItem(Items[k].data)^.oPointer));
            o3Dmagic:
              dispose(p3Dmagic(pObjectItem(Items[k].data)^.oPointer));
            o3Dheader:
              dispose(p3Dheader(pObjectItem(Items[k].data)^.oPointer));
            oOBJ8magic:
              dispose(pMagic(pObjectItem(Items[k].data)^.oPointer));
            oOBJ8header:
              dispose(p3Dheader(pObjectItem(Items[k].data)^.oPointer));
            owfOBJmagic:
              dispose(pMagic(pObjectItem(Items[k].data)^.oPointer));
            owfOBJheader:
              dispose(p3Dheader(pObjectItem(Items[k].data)^.oPointer));
            else
              dispose(pObjectItem(Items[k].data)^.oPointer);
          end;
        end;
        dispose(pObjectItem(Items[k].data));
      end;
//Heap := GetHeapStatus;
//MessageShow(Format('Heap: %d',[Heap.TotalAllocated]));
    end;
  end;
//Heap := GetHeapStatus;
//MessageShow(Format('Heap: %d',[Heap.TotalAllocated]));

  oTreeView.Items.Clear; //make sure it's empty
end;

{----------------------------------------------------------------------------}
Procedure ApplyFTMtoVectors(Matrix : tFloatArray; Vector : tFloatArray; var Result : tFloatArray);
const
  mSize = 4;
var
  i : integer;
begin
  for i := 0 to (4-1)-1 do begin
    Result[i] := Matrix[i*mSize+0] * Vector[0] +
                 Matrix[i*mSize+1] * Vector[1] +
                 Matrix[i*mSize+2] * Vector[2] +
//                 Matrix[i*mSize+3] * Vector[3] ;
                 Matrix[i*mSize+3] ;                // vector[3] = 1
    // result[3] := 1
  end;
end;

{----------------------------------------------------------------------------}
Procedure ApplyFTMtoNormals(Matrix : tFloatArray; Vector : tFloatArray; var Result : tFloatArray);
const
  mSize = 4;
var
  i : integer;
  UnScale : single;
begin
  for i := 0 to (4-1)-1 do begin
    Result[i] := Matrix[i*mSize+0] * Vector[0] +
                 Matrix[i*mSize+1] * Vector[1] +
                 Matrix[i*mSize+2] * Vector[2] +
//                 Matrix[i*mSize+3] * Vector[3] ;
//                 Matrix[i*mSize+3] ;                // vector[3] = 1
                 0 ;  // no translation
    // result[3] := 1
  end;
  // now re-normalize in case of scaling
  if ((Result[0] = 0) AND (Result[1] = 0) AND (Result[2] = 0)) then begin
    Result[0] := Result[1]; // to fing bug - not bug, normals is 0,0,0 for some objects for some reason
  end else begin
    UnScale := 1.0/(sqrt(sqr(Result[0])+sqr(Result[1])+sqr(Result[2])));
    Result[0] := Result[0] * UnScale;
    Result[1] := Result[1] * UnScale;
    Result[2] := Result[2] * UnScale;
  end;
end;

{----------------------------------------------------------------------------}
Procedure ReadCondorXfile(FileName : string; Append : boolean);
var
  Ch: Char;
  InputString : string;

{----------------------------------------------------------------------------}
Procedure ReadToEOL;
begin
  while ((Ch <> char($0A)) AND (NOT Eof(X_File))) do begin // look for linefeed
    if (Ch <> char($0D)) then begin // ignore CR
    end;
    Read(X_File, Ch);
  end;
end;

{----------------------------------------------------------------------------}
Procedure ReadToken;
begin
  InputString := '';
  while (NOT Eof(X_File)) do begin
    while ((Ch in [' ',char($0A),char($0D),char($09)]) AND (NOT Eof(X_File))) do begin // look for linefeed
      Read(X_File, Ch);
    end;
    if (NOT Eof(X_File)) then begin
      case ch of
        '{','}',',',';': begin //delimiter
          InputString := ch;
          Read(X_File, Ch);
          exit;
        end;
        '/': begin //comment
          ReadToEOL;
        end;
        '"': begin //quoted string
//          InputString := InputString + Ch;
          Read(X_File, Ch);
          while ((Ch <> '"') AND (NOT Eof(X_File))) do begin
            InputString := InputString + Ch;
            Read(X_File, Ch);
          end;
          if (Eof(X_File)) then begin
            //error
            exit;
          end else begin
//            InputString := InputString + Ch;
            Read(X_File, Ch);
          end;
          exit;
        end;
        'A'..'Z','a'..'z': begin
          while ((Ch in ['A'..'Z','a'..'z','0'..'9','_','-']) AND (NOT Eof(X_File))) do begin
            InputString := InputString + Ch;
            Read(X_File, Ch);
          end;
          exit;
        end;
        '-','0'..'9','.': begin
//          while ((Ch in ['-','0'..'9','.','A'..'Z','a'..'z']) AND (NOT Eof(X_File))) do begin
          while ((Ch in ['-','0'..'9','.','e','E']) AND (NOT Eof(X_File))) do begin
            InputString := InputString + Ch;
            Read(X_File, Ch);
          end;
          exit;
        end;
        else begin
          //error
          Read(X_File, Ch);
        end;
      end;
    end;
  end;
end;

//var
//  ValueString : string;
//  ValueInt : integer;
//  ValueFloat : double;

{----------------------------------------------------------------------------}
Procedure ParseDelimiter(Delimiter : DelimiterSet);
begin
//  assert(InputString = Delimiter,'Error in File');
//  if (InputString = Delimiter) then begin
  if (InputString[1] in Delimiter) then begin
    ReadToken;
  end else begin
  end;
end;

{----------------------------------------------------------------------------}
function ParseSingleInt(Delimiter : DelimiterSet) : integer;
begin
  Result := StrToInt(InputString);
  ReadToken;
  ParseDelimiter(Delimiter);
end;

{----------------------------------------------------------------------------}
Procedure ParseIntArray(ArraySize:integer;Ptr : pointer; Delimiter : DelimiterSet);
var
  i : integer;

begin
  SetLength(pIntArray(Ptr)^,ArraySize);
  for i := 0 to ArraySize-1-1 do begin
    pIntArray(Ptr)^[i] := ParseSingleInt(Delimiter);
  end;
  pIntArray(Ptr)^[ArraySize-1] := ParseSingleInt([';']);
end;

{----------------------------------------------------------------------------}
Procedure ParseCountIntArray(Ptr : pointer; Delimiter : char);
var
  {i,} Count : integer;

begin
  Count := ParseSingleInt([';']);
  if Ptr <> nil then begin
    pCountIntArray(Ptr)^.aCount := Count;
    SetLength(pCountIntArray(Ptr)^.aArray,Count);
  end;
  ParseIntArray(Count,@pCountIntArray(Ptr)^.aArray,[',']);
end;

{----------------------------------------------------------------------------}
Procedure ParseCountSurfaceArray(Ptr : pointer; Delimiter : char);
var
  i, Count : integer;

begin
  Count := ParseSingleInt([';']);
  if Ptr <> nil then begin
    pCountSurfaceArray(Ptr)^.aCount := Count;
    SetLength(pCountSurfaceArray(Ptr)^.aArray,Count);
  end;
  for i := 0 to Count-1-1 do begin
    ParseCountIntArray(@pCountSurfaceArray(Ptr)^.aArray[i],';');
    ParseDelimiter([',']);
  end;
  ParseCountIntArray(@pCountSurfaceArray(Ptr)^.aArray[Count-1],';');
  ParseDelimiter([';']);
end;

{----------------------------------------------------------------------------}
function ParseSingleFloat(Delimiter : DelimiterSet) : double;
begin
  XCX_To_Normal(InputString);
  Result := StrToFloat(InputString);
  ReadToken;
  ParseDelimiter(Delimiter);
end;

// inconsistency between .X and .CX files due to CONV3DS
// for material, delimiter is ',' or ';' ??? - Should be ';'!
// for now add selectable delimiter
{----------------------------------------------------------------------------}
Procedure ParseFloatArray(ArraySize:integer;Ptr : pointer; Delimiter : DelimiterSet);
var
  i : integer;
//  ValueString : string;

begin
  SetLength(pFloatArray(Ptr)^,ArraySize);
  for i := 0 to ArraySize-1-1 do begin
    pFloatArray(Ptr)^[i] := ParseSingleFloat(Delimiter);
  end;
  pFloatArray(Ptr)^[ArraySize-1] := ParseSingleFloat([';']);
end;

{----------------------------------------------------------------------------}
Procedure ParseCoordsArray(ArraySize:integer;Ptr : pointer);
var
  i,Count : integer;

begin
  Count := ParseSingleInt([';']);
  if Ptr <> nil then begin
    pCountFloatArray(Ptr)^.aCount := Count;
    SetLength(pCountFloatArray(Ptr)^.aArray,Count);
  end;

  for i := 0 to Count-1-1 do begin
    ParseFloatArray(ArraySize,@pCountFloatArray(Ptr)^.aArray[i],[';']);
    ParseDelimiter([',']);
  end;

  ParseFloatArray(ArraySize,@pCountFloatArray(Ptr)^.aArray[Count-1],[';']);
  ParseDelimiter([';']);
end;

{----------------------------------------------------------------------------}
Procedure ParseFileName(mNode : TTreenode);
var
  parseState : integer;
  FileName : string;

begin
  FileName := '';
  parseState := 0;
  while (NOT EOF(X_File)) do begin
    case parseState of
      0: begin
        if (InputString = '{') then begin
          New(pObjectData); //allocate space for an object data
          pObjectData^.oType := oFileName;
          New(pFileNameData); //allocate space for data
          pFileNameData^.tName := FileName;
          pObjectData^.oPointer := pFileNameData;
          oTreeView.Items.AddChildObject(mNode, 'FileName '+FileName,pObjectData);
          parseState := 1;
          ReadToken;
        end else begin
          //must be material list name
          FileName := InputString;
          ReadToken;
        end;
      end;
      1: begin
        if (InputString = '}') then begin
          ReadToken;
          exit;
        end else begin
          //get filename - quoted string
          pFileNameData^.tqName := InputString;
          ReadToken;
          ParseDelimiter([';']);
        end;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure ParseMaterial(mNode : TTreenode; Delimiter: char);
var
  parseState : integer;
  MaterialName : string;
  fTreeNode : TTreeNode;

begin
  MaterialName := '';
  parseState := 0;
  while (NOT EOF(X_File)) do begin
    case parseState of
      0: begin
        if (InputString = '{') then begin
          New(pObjectData); //allocate space for an object data
          pObjectData^.oType := oMaterial;
          New(pMaterialData); //allocate space for data
          pMaterialData^.tName := MaterialName;
          pObjectData^.oPointer := pMaterialData;
          fTreeNode := oTreeView.Items.AddChildObject(mNode, 'Material '+MaterialName,pObjectData);
          parseState := 1;
          ReadToken;
        end else begin
          //must be material name
          MaterialName := InputString;
          ReadToken;
        end;
      end;
      1: begin
//        xParseFloatArray(4,@pMaterialData^.tRGBA,[',']);
        ParseFloatArray(4,@pMaterialData^.tRGBA,[';',',']);
        ParseDelimiter([';']);
        pMaterialData^.tPower := ParseSingleFloat([';']);
//        xParseFloatArray(3,@pMaterialData^.tRGBs,[',']);
        ParseFloatArray(3,@pMaterialData^.tRGBs,[';',',']);
        ParseDelimiter([';']);
//        xParseFloatArray(3,@pMaterialData^.tRGBr,[',']);
        ParseFloatArray(3,@pMaterialData^.tRGBr,[';',',']);
        ParseDelimiter([';']);
        parseState := 2;
      end;
      2: begin
        if (InputString = '}') then begin
          ReadToken;
          exit;
        end else begin
          if (uppercase(InputString) = 'TEXTUREFILENAME') then begin
            ReadToken;
            ParseFileName(fTreeNode);
          end else begin
            //error
            ReadToken;
            exit;
          end;
        end;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure ParseMaterialList(mNode : TTreenode);
var
  i : integer;
  MaterialCount, SurfaceCount : integer;
  parseState : integer;
  MaterialListName : string;
  fTreeNode : TTreeNode;

begin
  MaterialListName := '';
  parseState := 0;
  while (NOT EOF(X_File)) do begin
    case parseState of
      0: begin
        if (InputString = '{') then begin
          New(pObjectData); //allocate space for an object data
          pObjectData^.oType := oMaterialList;
          New(pMaterialListData); //allocate space for data
          pMaterialListData^.tName := MaterialListName;
          pObjectData^.oPointer := pMaterialListData;
          fTreeNode := oTreeView.Items.AddChildObject(mNode, 'MaterialList '+MaterialListName,pObjectData);
          parseState := 1;
          ReadToken;
        end else begin
          //must be material list name
          MaterialListName := InputString;
          ReadToken;
        end;
      end;
      1: begin
        if (InputString = '}') then begin
          ReadToken;
          exit;
        end else begin
          MaterialCount := ParseSingleInt([';']);  //get number of materials
          pMaterialListData^.tCount := MaterialCount;

          SurfaceCount := ParseSingleInt([';']);  //get number of sufaces
          pMaterialListData^.tsCount := SurfaceCount;

          //get material index for each surface
          ParseIntArray(SurfaceCount,@pMaterialListData^.sArray,[',']);

          // possible bug in CX file due to CONV3DS -> extra ';' ???
          // seems to occur when material lists '1' surface instead
          // the same surface count as the mesh (?)
          if (InputString = ';') then begin
            ReadToken;
          end;

          //get each material
          for i := 0 to MaterialCount-1 do begin
            if (uppercase(InputString) = 'MATERIAL') then begin
              ReadToken;
              ParseMaterial(fTreeNode,';');
            end else begin
              if (uppercase(InputString) = '{') then begin
                ReadToken;
                //get material name
                New(pObjectData); //allocate space for an object data
                pObjectData^.oType := oMaterialReference;
                New(pMaterialReferenceData); //allocate space for data
                pMaterialReferenceData^.tName := InputString;
                pObjectData^.oPointer := pMaterialReferenceData;
                oTreeView.Items.AddChildObject(fTreeNode, InputString, pObjectData);
                ReadToken;
                if (uppercase(InputString) = '}') then begin
                  ReadToken;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure ParseMeshNormals(mNode : TTreenode);
var
  parseState : integer;
  MeshNormalsName : string;

begin
  MeshNormalsName := '';
  parseState := 0;
  while (NOT EOF(X_File)) do begin
    case parseState of
      0: begin
        if (InputString = '{') then begin
          New(pObjectData); //allocate space for an object data
          pObjectData^.oType := oMeshNormals;
          New(pMeshNormalsData); //allocate space for data
          pMeshNormalsData^.tName := MeshNormalsName;
          pObjectData^.oPointer := pMeshNormalsData;
          oTreeView.Items.AddChildObject(mNode, 'MeshNormals '+MeshNormalsName,pObjectData);
          parseState := 1;
          ReadToken;
        end else begin
          //must be material list name
          MeshNormalsName := InputString;
          ReadToken;
        end;
      end;
      1: begin
        if (InputString = '}') then begin
          ReadToken;
          exit;
        end else begin
          ParseCoordsArray(3,@pMeshNormalsData^.tArray);
          ParseCountSurfaceArray(@pMeshNormalsData^.sArray,',');
        end;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure ParseTextureCoords(mNode : TTreenode);
var
  parseState : integer;
  MeshTextureCoordsName : string;

begin
  MeshTextureCoordsName := '';
  parseState := 0;
  while (NOT EOF(X_File)) do begin
    case parseState of
      0: begin
        if (InputString = '{') then begin
          New(pObjectData); //allocate space for an object data
          pObjectData^.oType := oMeshTextureCoord;
          New(pMeshTcoordData); //allocate space for data
          pMeshTcoordData^.tName := MeshTextureCoordsName;
          pMeshTcoordData^.sIndex := nil;
          pObjectData^.oPointer := pMeshTcoordData;
          oTreeView.Items.AddChildObject(mNode, 'MeshTextureCoords '+MeshTextureCoordsName,pObjectData);
          parseState := 1;
          ReadToken;
        end else begin
          //must be material list name
          MeshTextureCoordsName := InputString;
          ReadToken;
        end;
      end;
      1: begin
        if (InputString = '}') then begin
          ReadToken;
          exit;
        end else begin
          ParseCoordsArray(2,@pMeshTcoordData^.tArray);
          pMeshTcoordData^.sIndex := SavedMeshData;
        end;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure ParseMesh(FrameNode : TTreenode);
var
  parseState : integer;
  fTreeNode : TTreeNode;
  MeshName : string;

begin
  MeshName := '';
  parseState := 0;
  while (NOT EOF(X_File)) do begin
    case parseState of
      0: begin
        if (InputString = '{') then begin
          New(pObjectData); //allocate space for an object data
          pObjectData^.oType := oMesh;
          New(pMeshData); //allocate space for data
          pMeshData^.tName := MeshName;
          pMeshData^.tRotation := 0.0;
          pMeshData^.tX := 0.0;
          pMeshData^.tY := 0.0;
          pMeshData^.tHeight := 0.0;
          pMeshData^.tScale := 1.0;
          pObjectData^.oPointer := pMeshData;
          fTreeNode := oTreeView.Items.AddChildObject(FrameNode, 'Mesh '+MeshName,pObjectData);
          SavedMeshData := nil;
          parseState := 1;
          ReadToken;
        end else begin
          //must be mesh name
          MeshName := InputString;
          ReadToken;
        end;
      end;
      1: begin
        ParseCoordsArray(3,@pMeshData^.tArray);
        CalcExtents(@pMeshData^.tArray,@pMeshData^);
        ParseCountSurfaceArray(@pMeshData^.sArray,',');
        SavedMeshData := pMeshData;
        parseState := 2;
      end;
      2: begin
        if (InputString = '}') then begin
          ReadToken;
          exit;
        end else begin
          if (uppercase(InputString) = 'MESHMATERIALLIST') then begin
            ReadToken;
            ParseMaterialList(fTreeNode);
          end else begin
            if (uppercase(InputString) = 'MESHNORMALS') then begin
              ReadToken;
              ParseMeshNormals(fTreeNode);
            end else begin
              if (uppercase(InputString) = 'MESHTEXTURECOORDS') then begin
                ReadToken;
                ParseTextureCoords(fTreeNode);
              end else begin
                //error
                ReadToken;
                exit;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure ParseFTM(FrameNode : TTreenode);
var
  parseState : integer;
  fTreeNode : TTreeNode;
  FTMname : string;

begin
  FTMname := '';
  parseState := 0;
  while (NOT EOF(X_File)) do begin
    case parseState of
      0: begin
        if (InputString = '{') then begin
          New(pObjectData); //allocate space for an object data
          pObjectData^.oType := oFTM;
          New(pFTMdata); //allocate space for data
          pFTMdata^.tName := FTMname;
          pObjectData^.oPointer := pFTMdata;
          fTreeNode := oTreeView.Items.AddChildObject(FrameNode, 'FTM '+FTMname,pObjectData);
//       pframe(pObjectItem(oTreeview.Items[fTreeNode.AbsoluteIndex-1].data)^.oPointer)^.tFTMindex := fTreeNode.AbsoluteIndex;
          parseState := 1;
          ReadToken;
        end else begin
          //must be mesh name
          FTMname := InputString;
          ReadToken;
        end;
      end;
      1: begin
        if (InputString = '}') then begin
          ReadToken;
          exit;
        end else begin
//          ParseFloatArray(16,@pFTMdata.ftmArray,[',']);
          ParseFloatArray(16,@pFTMdata^.ftmArray,[',']);   // seems to be same as without ^
          // for SimpleObjects
          if (InjectFTM) then begin
            // with FTM and inject FTM, must combine FTMs
            // TBD ???
          end;
          ParseDelimiter([';']);
        end;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure Inject_FTM(fNode : TTreenode);
var
  j : integer;
//  fTreeNode : TTreeNode;

begin
      // FTM
      New(pObjectData); //allocate space for an object data
      pObjectData^.oType := oFTM;
      New(pFTMdata); //allocate space for data
      pFTMdata^.tName := '';
      pObjectData^.oPointer := pFTMdata;
      {fTreeNode :=} oTreeView.Items.AddChildObject(fNode, 'FTM '+pFTMdata^.tName, pObjectData);
//  ParseFloatArray(16,@pFTMdata.ftmArray,[',']);
//  SetLength(pFloatArray(pFTMdata.ftmArray)^,16);
      SetLength(tFloatArray(pFTMdata^.ftmArray),16);
      for j := 0 to 16-1 do begin
        tFloatArray(pFTMdata^.ftmArray)[j] := FTM[j];
      end;
end;

{----------------------------------------------------------------------------}
Procedure ParseFrame(fNode : TTreenode);
var
  parseState : integer;
  fTreeNode : TTreeNode;
  FrameName : string;

begin
  FrameName := '';
  parseState := 0;
  while (NOT EOF(X_File)) do begin
    case parseState of
      0: begin
        if (InputString = '{') then begin
          New(pObjectData); //allocate space for an object data
          pObjectData^.oType := oFrame;
          New(pFrameData); //allocate space for a frame data
          pObjectData^.oPointer := pFrameData;
          pFrameData^.tName := FrameName;
//          pFrameData^.tFTMindex := -1;
          fTreeNode := oTreeView.Items.AddChildObject(fNode, 'Frame '+FrameName,pObjectData);
          parseState := 1;
          ReadToken;
        end else begin
          //must be frame name
          FrameName := InputString;
          ReadToken;
        end;
      end;
      1: begin
        if (InputString = '}') then begin
          ReadToken;
          exit;
        end else begin
          if (uppercase(InputString) = 'FRAMETRANSFORMMATRIX') then begin
            ReadToken;
            ParseFTM(fTreeNode);
          end else begin
            // for SimpleObjects
            if (InjectFTM) then begin
              Inject_FTM(fTreeNode);
            end;
            if (uppercase(InputString) = 'MESH') then begin
              ReadToken;
              ParseMesh(fTreeNode);
            end else begin
              if (uppercase(InputString) = 'FRAME') then begin
                ReadToken;
                ParseFrame(fTreeNode);
              end else begin
                //error
                exit;
              end;
            end;
          end;
        end;
      end;
      else begin
        //error
        exit;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure ParseHeader(mNode : TTreenode);
begin
  New(pObjectData); //allocate space for an object data
  pObjectData^.oType := oHeader;
  New(pHeaderData); //allocate space for data
  pObjectData^.oPointer := pHeaderData;
  oTreeView.Items.AddChildObject(mNode, 'Header',pObjectData);
  if (InputString <> '{') then begin
    // error
    exit;
  end else begin
    ReadToken;
    ParseIntArray(3,@pHeaderData^.sArray,[';']);
    if (InputString = '}') then begin
      ReadToken;
    end else begin
      // error
      exit;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure ParseMagic(mNode : TTreenode);
begin
  if (Not Append) then begin
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oMagic;
    New(pMagicData); //allocate space for data
    pObjectData^.oPointer := pMagicData;
    oTreeView.Items.AddChildObject(mNode, 'XOF',pObjectData);
    pMagicData^.tVersion := InputString;
    ReadToken;
    pMagicData^.tType := InputString;
    ReadToken;
    pMagicData^.tFloatType := InputString;
  end else begin
    ReadToken; // skip
    ReadToken; // skip
  end;
  ReadToken;
end;

{----------------------------------------------------------------------------}
Procedure ParseFile;
begin
  if (NOT Append) then begin
    ClearTreeView(oTreeView);
  end;
  while (NOT EOF(X_File)) do begin
    if (uppercase(InputString) = 'XOF') then begin
      ReadToken;
      ParseMagic(nil);
    end else begin
      if (uppercase(InputString) = 'HEADER') then begin
        ReadToken;
        ParseHeader(nil);
      end else begin
        if (uppercase(InputString) = 'MATERIAL') then begin
          ReadToken;
          ParseMaterial(nil,',');
        end else begin
          if (uppercase(InputString) = 'FRAME') then begin
            ReadToken;
            ParseFrame(nil);
          end else begin // special case for files for viewing
            if (uppercase(InputString) = 'MESH') then begin
              ReadToken;
              ParseMesh(nil);
            end else begin
              exit;
            end;
          end;
        end;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
begin
  begin
    if (uppercase(ExtractFileExt(FileName)) = '.X') then begin
      xFormat := fx;
      XCX_To_Normal := X_To_Normal;
    end else begin
      if (uppercase(ExtractFileExt(FileName)) = '.CX') then begin
        xFormat := fcx;
        XCX_To_Normal := CX_To_Normal;
      end else begin // '.PX'
        xFormat := fpx;
        XCX_To_Normal := PX_To_Normal;
      end;
    end;
    AssignFile(X_File,FileName);
    Reset(X_File);
    Read(X_File, Ch);
    ReadToken;
    ParseFile;
    Close(X_File);
  end;
  if (FileError) then begin
//    MessageShow('Error in file');
  end else begin
  end;
end;

{----------------------------------------------------------------------------}
Procedure ConvertForwardToBack(var S : string);
var
  i : integer;

begin
  Repeat // convert '/' to '\'
    i := pos('/',S);
    if (i <> 0) then begin
      delete(S,i,1);
      insert('\',S,i);
    end else begin
      break;
    end;
  Until (False);
end;

{----------------------------------------------------------------------------}
Procedure ConvertBackToForward(var S : string);
var
  i : integer;

begin
  Repeat // convert '\' to '/'
    i := pos('\',S);
    if (i <> 0) then begin
      delete(S,i,1);
      insert('/',S,i);
    end else begin
      break;
    end;
  Until (False);
end;

{----------------------------------------------------------------------------}
procedure LoadGraphicFileAndSaveAsBMP(FileIn, FileOut : string);
begin
  if (FileExists(FileIn)) then begin
    Form_Objects.Image_Texture.Stretch := true;
    try
      Form_Objects.Image_Texture.picture.LoadFromFile(FileIn);
      Form_Objects.Image_Texture.picture.bitmap.PixelFormat := pf24bit; //force 24 bit color
      WriteBitmapToFile(Form_Objects.Image_Texture.picture.bitmap,FileOut);
    except
      beep;
    end;
  end;
end;

{----------------------------------------------------------------------------}
procedure xLoadBMPfileFixAndSaveAsBMP(FileIn, FileOut : string);
var
  ByteCount : longint;
  NumBytesRead : integer;
  BMP_InFile : File of byte;
  BMP_OutFile : File of byte;
  Header : BMP_V1_Header;
  P : Array[0..256-1] of byte;

begin
  if (FileExists(FileIn)) then begin
    try
      AssignFile(BMP_InFile,FileIn);
      Reset(BMP_InFile);
      AssignFile(BMP_OutFile,FileOut);
      Rewrite(BMP_OutFile);
      BlockRead(BMP_InFile,Header,sizeof(Header));

      ByteCount := Header.bDIB.bWidth*
                   Header.bDIB.bWidth*
                   Header.bDIB.bColorBits div 8;
      if (ByteCount <> Header.bDIB.bImageByteSize) then begin
        Header.bDIB.bImageByteSize := ByteCount;
      end;

      BlockWrite(BMP_OutFile,Header,sizeof(Header));
      ByteCount := Header.bH.bFileByteSize - sizeof(Header);
      While ByteCount > 0 do begin
        BlockRead(BMP_InFile,  P,sizeof(P), NumBytesRead);
        BlockWrite(BMP_OutFile,P,NumBytesRead);
        ByteCount := ByteCount - NumBytesRead;
      end;

    finally
      Close(BMP_InFile);
      Close(BMP_OutFile);
    end;
  end;
end;

{----------------------------------------------------------------------------}
function LoadBMPfileFixAndSaveAsBMP(FileIn, FileOut : string) : Boolean;
var
  ByteCount : longint;
  NumBytesRead : integer;
  BMP_InFile : File of byte;
  BMP_OutFile : File of byte;
  Header : BMP_V1_Header;
  P : Array[0..256-1] of byte;

begin
  result := false; // assume no fixing required
  if (FileExists(FileIn)) then begin
    try
      AssignFile(BMP_InFile,FileIn);
      Reset(BMP_InFile);
      BlockRead(BMP_InFile,Header,sizeof(Header));

      ByteCount := Header.bDIB.bWidth*
                   Header.bDIB.bWidth*
                   Header.bDIB.bColorBits div 8;
      // check for error in file format
      if (ByteCount <> Header.bDIB.bImageByteSize) then begin
        result := true; // fixing required
        Header.bDIB.bImageByteSize := ByteCount;
        // now write substitute file with fix
        try
          AssignFile(BMP_OutFile,FileOut);
          Rewrite(BMP_OutFile);
          BlockWrite(BMP_OutFile,Header,sizeof(Header));
          ByteCount := Header.bH.bFileByteSize - sizeof(Header);
          While ByteCount > 0 do begin
            BlockRead(BMP_InFile,  P,sizeof(P), NumBytesRead);
            BlockWrite(BMP_OutFile,P,NumBytesRead);
            ByteCount := ByteCount - NumBytesRead;
          end;
        finally
          Close(BMP_OutFile);
        end;
      end;

    finally
      Close(BMP_InFile);
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure WriteCondorXfile(FileName : string; ForViewer : Viewer_Type; TC_InvertY : boolean);
var
  NodeIndex : integer;

{----------------------------------------------------------------------------}
Procedure WriteSingleInt(cInt : integer; Delimiter : char);
begin
  write(X_File,' ',cInt:10,Delimiter);
end;

{----------------------------------------------------------------------------}
Procedure WriteIntArray(sSize : integer; cArray : tIntArray; Delimiter : char);
var
  j : integer;

begin
  for j := 0 to sSize-1-1 do begin
    WriteSingleInt(cArray[j],Delimiter);
    writeln(X_File);
  end;
  WriteSingleInt(cArray[sSize-1],';');
  writeln(X_File);
end;

{----------------------------------------------------------------------------}
Procedure WriteCountIntArray(Ptr : pointer; Delimiter : char);
var
  i, Count : integer;

begin
  //write vertex count
  Count := pCountIntArray(Ptr)^.aCount;
  write(X_File,Count,';');
  //write vertices
  for i := 0 to Count-1-1 do begin
    WriteSingleInt(pCountIntArray(Ptr)^.aArray[i],Delimiter);
//    writeln(X_File);
  end;
  WriteSingleInt(pCountIntArray(Ptr)^.aArray[Count-1],';');
//  writeln(X_File);
end;

{----------------------------------------------------------------------------}
Procedure WriteCountSurfaceArray(Ptr : pointer; Delimiter : char);
var
  i, Count : integer;

begin // pMesh(pObjectItem(Items[i].data)^.oPointer)^.sArray
  //write surface count
  Count := pCountSurfaceArray(Ptr)^.aCount;
  writeln(X_File,Count,';');
  //write surfaces
  for i := 0 to Count-1-1 do begin
    WriteCountIntArray(@pCountSurfaceArray(Ptr)^.aArray[i],Delimiter);
    writeln(X_File,Delimiter);
  end;
  WriteCountIntArray(@pCountSurfaceArray(Ptr)^.aArray[Count-1],Delimiter);
  writeln(X_File,';');
end;

{----------------------------------------------------------------------------}
Procedure WriteSingleFloat(cFloat : double; Delimiter : char);
var
  sFloat : string;

begin
  sFloat := format('%10.7f',[cFloat]);
  Normal_To_XCX(sFloat);
  write(X_File,' ',sFloat,Delimiter);
end;

{----------------------------------------------------------------------------}
Procedure xWriteFloatArray(sSize : integer; cArray : array of single;
                          Delimiter : char; InvertY : boolean;
                          Scale : double);
begin
end;
{----------------------------------------------------------------------------}
Procedure WriteFloatArray(sSize : integer; cArray : {tFloatArray}array of single;
                          Delimiter : char; InvertY : boolean;
                          Scale : double);
var
  j : integer;

begin
  for j := 0 to sSize-1-1 do begin
    if ((InvertY) AND (j=1)) then begin
      WriteSingleFloat((1-cArray[j])*Scale,Delimiter);
    end else begin
      WriteSingleFloat(cArray[j]*Scale,Delimiter);
    end;
  end;
  if ((InvertY) AND (sSize-1=1)) then begin
    WriteSingleFloat((1-cArray[sSize-1])*Scale,Delimiter);
  end else begin
    WriteSingleFloat(cArray[sSize-1]*Scale,';');
  end;
end;

{----------------------------------------------------------------------------}
Procedure WriteCountArray(sSize: integer; cArray : tCountFloatArray;
                          InvertY:boolean; Scale : double);
var
  j,Count : integer;

begin
  //write vertex count
  Count := cArray.aCount;
  writeln(X_File,Count,';');
  //write vertices
  for j := 0 to Count-1 do begin
    WriteFloatArray(sSize, cArray.aArray[j],';',InvertY,Scale);
    if (j = Count-1) then begin
      writeln(X_File,';');
    end else begin
      writeln(X_File,',');
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure WriteMeshTexture(InvertY:boolean);
//var
//  j,k,Count : integer;

begin
  //walk the tree and write the data
  with oTreeView do begin
    writeln(X_File,'MeshTextureCoords '+
      pMeshTcoord(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tName+' {');

    //invert texturecoords (or invert bitmap vertically if needed)
    WriteCountArray(2,pMeshTcoord(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tArray,InvertY,1.0);

    writeln(X_File,'}');
    INC(NodeIndex);
  end;
end;

{----------------------------------------------------------------------------}
Procedure WriteMeshNormals;
//var
//  j,Count : integer;

begin
  //walk the tree and write the data
  with oTreeView do begin
    writeln(X_File,'MeshNormals '+
      pMeshNormals(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tName+' {');

    WriteCountArray(3,pMeshNormals(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tArray,false,1.0);
    WriteCountSurfaceArray(@pMeshNormals(pObjectItem(Items[NodeIndex].data)^.oPointer)^.sArray,',');

    writeln(X_File,'}');
    INC(NodeIndex);
  end;
end;

{----------------------------------------------------------------------------}
Procedure WriteFileName;
var
  i, j : integer;
//  Position : integer;
  FileName : string;
  FileSubstitute : string;
  DesiredPath : string;

begin
  with oTreeView do begin
    writeln(X_File,'TextureFilename '+
      pFileName(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tName+' {');

    write(X_File,'"');
    FileName := pFileName(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tqName;
    DesiredPath := '';
    case ForViewer of
      vSample: begin
        // for using with Sample.exe - can only load bitmaps with correct format
        ConvertForwardToBack(FileName);
//        FileSubstitute := WorkingFolder+'\Temp\'+ChangeFileExt(ExtractFileName(FileName),'.bmp');
        FileSubstitute := ApplicationPath+'\Temp\'+ChangeFileExt(ExtractFileName(FileName),'.bmp');
        if ( NOT(pos(FileName,':') <> 0) AND
             NOT(ExtractFilePath(FileName) = '') ) then begin
          FileName := Path+'\'+FileName;
        end;
        if (uppercase(ExtractFileExt(FileName)) = '.BMP') then begin
          // check if bitmap needs fixing
          if ( NOT LoadBMPfileFixAndSaveAsBMP(FileName, FileSubstitute) ) then begin
            FileSubstitute := FileName;
          end;
        end else begin
          LoadGraphicFileAndSaveAsBMP(FileName, FileSubstitute);
        end;
        ConvertBackToForward(FileSubstitute);
        writeln(X_File, FileSubstitute+'";');
      end;
      V1_ObjectViewer: begin
        // make file path Object relative if possible
        if ( (pos('LANDSCAPES',uppercase(FileName)) = 1) OR
            (pos('WORLD',uppercase(FileName)) = 1) ) then begin
          // Condor relative, so change to object relative
          i := pos('LANDSCAPES',uppercase(oFolder));
          // first check if Object is within Condor 'Landscapes' path
          if  (i <> 0) then begin
            // now backtrack from Object folder down to Condor folder
            for j := i to length(oFolder) do begin
              if oFolder[j] = '\' then begin
                FileName := '../' + FileName;
              end;
            end;
          end;
          ConvertBackToForward(FileName); // for Slovenia2 airports (?)
          writeln(X_File, FileName+'";');
        end else begin // must be Object relative already
          writeln(X_File, FileName+'";');
        end;
      end;
      else begin
        writeln(X_File, FileName+'";');
      end;
    end; // case
    writeln(X_File,'}');
    INC(NodeIndex);
  end;
end;

{----------------------------------------------------------------------------}
Procedure WriteMaterialReference;
begin
  with oTreeView do begin
    writeln(X_File,'{'+
      pMaterialReference(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tName+'}');
  end;
  INC(NodeIndex);
end;

{----------------------------------------------------------------------------}
Procedure WriteMaterial(SpecialCase : V2_Mesh);
//var
//  j,Count : integer;

begin
  with oTreeView do begin
    writeln(X_File,'Material '+
      pMaterial(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tName+' {');

    case SpecialCase of
      mGrass : begin
        WriteFloatArray(4, [0.0,1.0,0.0,0.3],';',false,1.0); // green
      end;
      mAsphalt : begin
        WriteFloatArray(4, [0.0,0.0,1.0,0.3],';',false,1.0); // blue
      end;
      mGrassPaint,mAsphlatPaint : begin
        WriteFloatArray(4, [1.0,1.0,1.0,1.0],';',false,1.0); // White
      end;
      else begin
        WriteFloatArray(4,
          pMaterial(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tRGBA,';',false,1.0);
      end;
    end;
    writeln(X_File,';');
    WriteSingleFloat(pMaterial(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tPower,';');
    writeln(X_File);
    WriteFloatArray(3,
      pMaterial(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tRGBs,';',false,1.0);
    writeln(X_File,';');
    WriteFloatArray(3,
      pMaterial(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tRGBr,';',false,1.0);
    writeln(X_File,';');

    INC(NodeIndex);
    while ( (NodeIndex <= oTreeView.Items.Count-1) AND (Items[NodeIndex].data <> nil) AND
      (pObjectItem(Items[NodeIndex].data)^.oType in [oFilename,oMaterialReference]) ) do begin
      case pObjectItem(Items[NodeIndex].data)^.oType of
        oFileName: begin
          WriteFileName;
        end;
      end;
    end;
    writeln(X_File,'}');
  end;
end;

{----------------------------------------------------------------------------}
Procedure WriteMaterialList;
var
//  j,Count : integer;
MeshName : string;
V2_Special : V2_Mesh;

begin
  with oTreeView do begin
    MeshName := pMesh(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tName;
    V2_Special := mNormal;
    if (UpperCase(MeshName) = 'GRASS') then begin
      V2_Special := mGrass;
    end;

    writeln(X_File,'MeshMaterialList '+
      pMesh(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tName+' {');

    WriteSingleInt(pMaterialList(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tCount,';');
    writeln(X_File);
    WriteSingleInt(pMaterialList(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tsCount,';');
    writeln(X_File);
    WriteIntArray(pMaterialList(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tsCount,
      pMaterialList(pObjectItem(Items[NodeIndex].data)^.oPointer)^.sArray,',');
AA := oTreeView.Items[NodeIndex].Level;
    INC(NodeIndex);
BB := oTreeView.Items[NodeIndex].Level;
    while ( (NodeIndex <= oTreeView.Items.Count-1) AND (Items[NodeIndex].data <> nil) AND
//      (BB >= AA) AND
// bug BB = 3 but oTreeView.Items[NodeIndex].Level = 0 ???
// ??? hierarchy problem - 8 Aug 2018 - temporary fix make sure Material is at same level or higher
// in case material is at level 0 for next frame/mesh
// need to go through treeView by parent/sibling instead of index to avoid this level problem
      (oTreeView.Items[NodeIndex].Level >= oTreeView.Items[NodeIndex-1].Level) AND
      (pObjectItem(Items[NodeIndex].data)^.oType in [oMaterial,oMaterialReference]) ) do begin
      case pObjectItem(Items[NodeIndex].data)^.oType of
        oMaterial: begin
          WriteMaterial(V2_Special);
        end;
        oMaterialReference: begin
          WriteMaterialReference;
        end;
      end;
    end;
    writeln(X_File,'}');
  end;
end;

{----------------------------------------------------------------------------}
Procedure xWriteMesh;
//var
//  j,Count : integer;

begin
  //walk the tree and write the data
  with oTreeView do begin
    writeln(X_File,'Mesh '+
      pMesh(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tName+' {');

    WriteCountArray(3,pMesh(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tArray,
      false,16/pMesh(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tMinMaxArray[6]);
    WriteCountSurfaceArray(@pMesh(pObjectItem(Items[NodeIndex].data)^.oPointer)^.sArray,',');

    INC(NodeIndex);
    while ( (NodeIndex <= oTreeView.Items.Count-1) AND (Items[NodeIndex].data <> nil) AND
      (pObjectItem(Items[NodeIndex].data)^.oType in [oMeshNormals,
        oMeshTextureCoord,oMaterialList]) ) do begin
      case pObjectItem(Items[NodeIndex].data)^.oType of
        oMeshTextureCoord: begin
          if (TC_InvertY) then begin
            WriteMeshTexture(true); //invert vertical
          end else begin
            WriteMeshTexture(false);
          end;
        end;
        oMeshNormals: begin
          WriteMeshNormals;
        end;
        oMaterialList: begin
          WriteMaterialList;
        end;
      end;
    end;
    writeln(X_File,'}');
  end;
end;

{----------------------------------------------------------------------------}
Procedure WriteMesh;
//var
//  j,Count : integer;

begin
  if ( (ForViewer <> vSample) OR
       ((ForViewer = vSample) AND (NodeIndex = SelectedMeshNode.AbsoluteIndex))
     ) then begin
    //walk the tree and write the data
    with pMesh(pObjectItem(oTreeView.Items[NodeIndex].data)^.oPointer)^ do begin
      writeln(X_File,'Mesh '+ tName+' {');
      if (ForViewer = vSample) then begin
        WriteCountArray(3,tArray,false,10/tMinMaxArray[6]);
      end else begin
        WriteCountArray(3,tArray,false,1.0);
      end;
      WriteCountSurfaceArray(@sArray,',');
    end;
    INC(NodeIndex);
    while ( (NodeIndex <= oTreeView.Items.Count-1) AND
            (oTreeView.Items[NodeIndex].data <> nil) AND
            (pObjectItem(oTreeView.Items[NodeIndex].data)^.oType in
              [oMeshNormals,oMeshTextureCoord,oMaterialList])
          ) do begin
      case pObjectItem(oTreeView.Items[NodeIndex].data)^.oType of
        oMeshTextureCoord: begin
          if (TC_InvertY) then begin
            WriteMeshTexture(true); //invert vertical
          end else begin
            WriteMeshTexture(false);
          end;
        end;
        oMeshNormals: begin
          WriteMeshNormals;
        end;
        oMaterialList: begin
          WriteMaterialList;
        end;
      end;
    end;
    writeln(X_File,'}');
  end else begin
    INC(NodeIndex);
  end;
end;

{----------------------------------------------------------------------------}
Procedure WriteFTM;
begin
  if (ForViewer <> vSample) then begin
    with oTreeView do begin
      writeln(X_File,'FrameTransformMatrix '+
        pMeshTcoord(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tName+' {');

      WriteFloatArray(16,pFTM(pObjectItem(Items[NodeIndex].data)^.oPointer)^.ftmArray,',',false,1.0);
      writeln(X_File,';');
      writeln(X_File,'}');
    end;
  end;
  INC(NodeIndex);
end;

{----------------------------------------------------------------------------}
Procedure WriteFrame;
var
  TreeLevel : integer;
begin
  with oTreeView do begin
    if (ForViewer <> vSample) then begin
      writeln(X_File,'Frame '+
        pframe(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tName+' {');
    end;
    // keep track of level for recursive frames
    TreeLevel := Items[NodeIndex].Level;
    INC(NodeIndex);
    //walk the tree and write the data
    while ( (NodeIndex <= oTreeView.Items.Count-1) AND (Items[NodeIndex].data <> nil) AND
      (pObjectItem(Items[NodeIndex].data)^.oType in [oFrame,oMesh,oFTM]) ) do begin
      case pObjectItem(Items[NodeIndex].data)^.oType of
        oFrame: begin
          if (TreeLevel <> Items[NodeIndex].Level) then begin
            WriteFrame;
          end else begin
            // same level - must be new frame - close this one
            break; // exit while
          end;
        end;
        oFTM: begin
          // WriteFTM; // not used by Condor, remove it
          // INC(NodeIndex);
          WriteFTM; // still used by .px -> apply for .X and .CX ???
        end;
        oMesh: begin
          WriteMesh;
        end;
      end;
    end;
    if (ForViewer <> vSample) then begin
      writeln(X_File,'}');
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure Write3Dheader;
begin
  with oTreeView do begin
    writeln(X_File,'Header { ');
    writeln(X_File,'1;');
    writeln(X_File,'0;');
    writeln(X_File,'1;');
    writeln(X_File, ' }');
  end;
  INC(NodeIndex);
end;

{----------------------------------------------------------------------------}
Procedure WriteHeader;
begin
  with oTreeView do begin
    writeln(X_File,'Header { ');

    WriteIntArray(3,pHeader(pObjectItem(Items[NodeIndex].data)^.oPointer)^.sArray,';');

    writeln(X_File, ' }');
  end;
  INC(NodeIndex);
end;

{----------------------------------------------------------------------------}
Procedure WriteOBJ8magic;
begin
  with oTreeView do begin
    write(X_File,'xof 0303txt 0032');
    writeln(X_File);
  end;
  INC(NodeIndex);
end;

{----------------------------------------------------------------------------}
Procedure Write3Dmagic;
begin
  with oTreeView do begin
    write(X_File,'xof 0303txt 0032');
    writeln(X_File);
  end;
  INC(NodeIndex);
end;

{----------------------------------------------------------------------------}
Procedure WriteMagic;
begin
  with oTreeView do begin
    write(X_File,'xof ');

    write(X_File,pMagic(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tVersion);
    write(X_File,pMagic(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tType,' ');
    write(X_File,pMagic(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tFloatType);

    writeln(X_File);
  end;
  INC(NodeIndex);
end;

{============================================================================}
begin
  if (uppercase(ExtractFileExt(FileName)) = '.X') then begin
     xFormat := fx;
    Normal_To_XCX := Normal_To_X;
  end else begin
    if (uppercase(ExtractFileExt(FileName)) = '.CX') then begin
      xFormat := fcx;
      Normal_To_XCX := Normal_To_CX;
    end else begin
      if (uppercase(ExtractFileExt(FileName)) = '.PX') then begin
        xFormat := fpx;
        Normal_To_XCX := Normal_To_PX;
      end else begin
        Beep; Exit;
      end;
    end;
  end;

  AssignFile(X_File,FileName);

  try
    ReWrite(X_File);

    //walk the tree and write the data
    with oTreeView do begin
      NodeIndex := 0;
      while  (NodeIndex <= oTreeView.Items.Count-1) do begin
        if (oTreeView.Items[NodeIndex].data <> nil) then begin
          case pObjectItem(oTreeView.Items[NodeIndex].data)^.oType of
            owfOBJmagic: begin
              Write3Dmagic;
            end;
            oOBJ8magic: begin
              Write3Dmagic;
            end;
            o3Dmagic: begin
              Write3Dmagic;
            end;
            oMagic: begin
              WriteMagic;
            end;
       //     owfOBJheader: begin
       //       WriteWfOBJheader;  // not needed
       //     end;
       //     owfOBJheader: begin
       //       WriteWfOBJheader;  // not needed
       //     end;
       //     o3Dheader: begin
       //       Write3Dheader;  // not needed
       //     end;
            oHeader: begin
              WriteHeader;
            end;
            oFrame: begin
              WriteFrame;
              end;
            oMesh: begin
              WriteMesh;
            end;
            oMaterial: begin
              WriteMaterial(mNormal);
            end;
            else begin // skip
              INC(NodeIndex);
            end;
          end;
        end else begin // skip
          INC(NodeIndex);
        end;
      end;
    end;
  finally
    Close(X_File);
  end;
end;

{----------------------------------------------------------------------------}
Procedure PlotIt(aData : tCountFloatArray);
var
  i: integer;
//  ErrorCode :integer;
//  Xmin,Xmax,Ymin,Ymax,Scale : double;
//  Xcentre, Ycentre : double;
//  AA, COSfactor, SINfactor : double;
  iX, iY : integer;

begin
      //calc extents
//      Xmin :=  1e6;
//      Xmax := -1e6;
//      Ymin :=  1e6;
//      Ymax := -1e6;
//      if aData.aCount > 0 then begin
//        for i := 0 to aData.aCount-1 do begin
//          with aData do begin
//            if aArray[i][0] < Xmin then Xmin := aArray[i][0];
//            if aArray[i][0] > Xmax then Xmax := aArray[i][0];
//            if aArray[i][1] < Ymin then Ymin := aArray[i][1];
//            if aArray[i][1] > Ymax then Ymax := aArray[i][1];
//          end;
//        end;
//      end;
//      Xcentre := (Xmin + Xmax)/2;
//      Ycentre := (Ymin + Ymax)/2;
//      Scale := (Xmax-Xmin);
//      if (Ymax-Ymin) > Scale then begin
//        Scale := (Ymax-Ymin);
//      end;

      with tvBitmap do begin
        Canvas.Pen.Style := psSolid;
        Canvas.Pen.Mode := pmCopy;
        Canvas.Pen.Color := clRed;
        Canvas.Brush.Color := clWhite;
        Canvas.FillRect(rect(0,0,Width,Height)); //erase first
//        iX := round((aData.aArray[i][0].aArray[0]-Xcentre)*255/Scale + 127.5);
//        iY := round((aData.aArray[i][0].aArray[1]-ycentre)*255/Scale + 127.5);
        iX := round((aData.aArray[0][0])*255);
        iY := round((aData.aArray[0][1])*255);
        Canvas.MoveTo(iX,iY);
        for i := 1 to aData.aCount-1 do begin
          with aData do begin
//            iX := round((aArray[i][0]-Xcentre)*255/Scale + 127.5);
//            iY := round((aArray[i][1]-Ycentre)*255/Scale + 127.5);
            iX := round((aArray[i][0])*255);
            iY := round((aArray[i][1])*255);
            Canvas.LineTo(iX,iY);
    if (i >= StrToInt(Unit_Objects.Form_Objects.Edit_Steps.text)) then begin
      break;
    end;
          end;
        end;
      end;

end;

// plot texture coordinate surfaces
{----------------------------------------------------------------------------}
Procedure sPlotIt(Ptr : pointer);
var
  i, j : integer;
//  Xcentre, Ycentre,Scale : double;
  iX, iY : integer;
  sArray : pCountSurfaceArray;

begin
  with pMeshTcoord(Ptr)^ do begin
    sArray := @pMesh(sIndex)^.sArray;
    if (sArray <> nil) then begin
      with tvBitmap do begin
        Canvas.Pen.Style := psSolid;
        Canvas.Pen.Mode := pmCopy;
        Canvas.Pen.Color := clRed;
        Canvas.Brush.Color := clWhite;
        Canvas.FillRect(rect(0,0,Width,Height)); //erase first
        for i := 0 to sArray^.aCount-1 do begin
          iX := round((tArray.aArray[sArray.aArray[i].aArray[sArray.aArray[i].aCount-1]][0])*255);
          iY := round((tArray.aArray[sArray.aArray[i].aArray[sArray.aArray[i].aCount-1]][1])*255);
          Canvas.MoveTo(iX,iY);
          for j := 0 to sArray.aArray[i].aCount-1 do begin
            iX := round((tArray.aArray[sArray.aArray[i].aArray[j]][0])*255);
            iY := round((tArray.aArray[sArray.aArray[i].aArray[j]][1])*255);
            Canvas.LineTo(iX,iY);
          end;
        end;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure CalcExtents(Ptr, PtrMinMax : pointer);
var
  i : integer;
  Range : double;

begin
  with pMesh(PtrMinMax)^ do begin
    with pCountFloatArray(Ptr)^ do begin
      //calc extents
      {Xmin} tMinMaxArray[0] :=  1e6;
      {Xmax} tMinMaxArray[1] := -1e6;
      {Ymin} tMinMaxArray[2] :=  1e6;
      {Ymax} tMinMaxArray[3] := -1e6;
      {Zmin} tMinMaxArray[7] :=  1e6;
      {Zmax} tMinMaxArray[8] := -1e6;
      if aCount > 0 then begin
        for i := 0 to aCount-1 do begin
          if aArray[i][0] < tMinMaxArray[0] then tMinMaxArray[0] := aArray[i][0];
          if aArray[i][0] > tMinMaxArray[1] then tMinMaxArray[1] := aArray[i][0];
          if aArray[i][1] < tMinMaxArray[2] then tMinMaxArray[2] := aArray[i][1];
          if aArray[i][1] > tMinMaxArray[3] then tMinMaxArray[3] := aArray[i][1];
          if aArray[i][2] < tMinMaxArray[7] then tMinMaxArray[7] := aArray[i][2];
          if aArray[i][2] > tMinMaxArray[8] then tMinMaxArray[8] := aArray[i][2];
        end;
        {Xcentre} tMinMaxArray[4] := (tMinMaxArray[0] + tMinMaxArray[1])/2;
        {Ycentre} tMinMaxArray[5] := (tMinMaxArray[2] + tMinMaxArray[3])/2;
        tMinMaxArray[6] := 0;
        for i := 0 to aCount-1 do begin
          Range := sqrt(SQR(aArray[i][0]-tMinMaxArray[4])+
                           SQR(aArray[i][1]-tMinMaxArray[5]));
          if (Range > tMinMaxArray[6]) then begin
            tMinMaxArray[6] := Range;
          end;
        end;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure NormalizeMesh(Ptr, PtrMinMax : pointer; LowestHeight : boolean);
var
  i: integer;
  Xcentre, Ycentre, Zlowest : double;

begin
  with pMesh(PtrMinMax)^ do begin                     
//    Xcentre := (tMinMaxArray[0] + tMinMaxArray[1])/2;
//    Ycentre := (tMinMaxArray[2] + tMinMaxArray[3])/2;
    Xcentre := tMinMaxArray[4];
    Ycentre := tMinMaxArray[5];
    Zlowest := tMinMaxArray[7];
  end;

  with pCountFloatArray(Ptr)^ do begin
    for i := 0 to aCount-1 do begin
      aArray[i][0] := aArray[i][0]-Xcentre;
      aArray[i][1] := aArray[i][1]-Ycentre;
      if (LowestHeight) then begin
        aArray[i][2] := aArray[i][2]-Zlowest;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure xRotateMesh(Ptr : pointer);
var
  i: integer;
  AA, Angle : double;
  COSfactor, SINfactor : double;

begin
  with pMesh(Ptr)^ do begin
    Angle := tRotation * Pi /180.0;
    SINfactor := sin(Angle);
    COSfactor := cos(Angle);
    with tArray do begin
      for i := 0 to aCount-1 do begin
        AA := aArray[i][0] * COSfactor - aArray[i][1] * SINfactor;
        aArray[i][1] := aArray[i][0] * SINfactor + aArray[i][1] * COSfactor;
        aArray[i][0] := AA;
      end;
    end;
  end;
  //now rotate normals

end;

{----------------------------------------------------------------------------}
Procedure RotateMesh(Node : TTreeNode);
var
  i, NodeIndex: integer;
  AA, Angle : double;
  COSfactor, SINfactor : double;

begin
  with pMesh(pObjectItem(Node.data)^.oPointer)^ do begin
    Angle := tRotation * Pi /180.0;
    SINfactor := sin(Angle);
    COSfactor := cos(Angle);
    with tArray do begin
      for i := 0 to aCount-1 do begin
        AA := aArray[i][0] * COSfactor - aArray[i][1] * SINfactor;
        aArray[i][1] := aArray[i][0] * SINfactor + aArray[i][1] * COSfactor;
        aArray[i][0] := AA;
      end;
    end;
  end;
  //now look for normals and rotate
  NodeIndex := 0;
  while ( (NodeIndex <= Node.Count-1) AND (Node.Item[NodeIndex].data <> nil) AND
    (pObjectItem(Node.Item[NodeIndex].data)^.oType in [oMeshNormals,
//    (pObjectItem(Node.Item[NodeIndex].data)^.oType in [oMeshNormals, o3DmeshNormals,
      oMeshTextureCoord,oMaterialList]) ) do begin
    case pObjectItem(Node.Item[NodeIndex].data)^.oType of
      oMeshNormals{, o3DmeshNormals}: begin
        with pMeshNormals(pObjectItem(Node.Item[NodeIndex].data)^.oPointer)^ do begin
          with tArray do begin
            for i := 0 to aCount-1 do begin
              AA := aArray[i][0] * COSfactor - aArray[i][1] * SINfactor;
              aArray[i][1] := aArray[i][0] * SINfactor + aArray[i][1] * COSfactor;
              aArray[i][0] := AA;
            end;
          end;
        end;
        break;
      end;
    end;
    INC(NodeIndex);
  end;
end;

{----------------------------------------------------------------------------}
{Procedure OffsetHeight(Ptr, PtrMinMax : pointer);
var
  i: integer;
  Zmin: double;

begin
  with pMesh(PtrMinMax)^ do begin
    Zmin := tMinMaxArray[7];
  end;

  with pCountFloatArray(Ptr)^ do begin
    for i := 0 to aCount-1 do begin
      aArray[i][2] := aArray[i][2]-Zmin;
    end;
  end;
end;
}
{----------------------------------------------------------------------------}
Procedure OffsetHeight(Node : TTreeNode);
var
  i: integer;
  Zmin: double;

begin
  with pMesh(pObjectItem(Node.data)^.oPointer)^ do begin
    Zmin := tMinMaxArray[7];

    with tArray do begin
      for i := 0 to aCount-1 do begin
        aArray[i][2] := aArray[i][2]-Zmin+tHeight;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure TranslateMesh(Node : TTreeNode; AbsoluteHeight : boolean);
var
  i: integer;
  Zmin: double;

begin
  with pMesh(pObjectItem(Node.data)^.oPointer)^ do begin
    Zmin := tMinMaxArray[7];

    with tArray do begin
      for i := 0 to aCount-1 do begin
        aArray[i][0] := aArray[i][0]+tX;
        aArray[i][1] := aArray[i][1]+tY;
        if (AbsoluteHeight) then begin
          aArray[i][2] := aArray[i][2]-Zmin+tHeight;
        end else begin
          aArray[i][2] := aArray[i][2]+tHeight;
        end;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure ScaleMesh(Node : TTreeNode);
var
  i: integer;
//  Zmin: double;

begin
  with pMesh(pObjectItem(Node.data)^.oPointer)^ do begin
//    Zmin := tMinMaxArray[7];

    with tArray do begin
      for i := 0 to aCount-1 do begin
        aArray[i][0] := aArray[i][0] * tScale;
        aArray[i][1] := aArray[i][1] * tScale;
        aArray[i][2] := aArray[i][2] * tScale;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure mPlotIt(Ptr, PtrMinMax : pointer);
var
  i: integer;
  Xcentre, Ycentre,Scale : double;
  iX, iY : integer;

begin
  if (pMesh(PtrMinMax)^.tRotation <> 0) then begin
  end;
  if (pMesh(PtrMinMax)^.tHeight <> 0) then begin
  end;
  with pMesh(PtrMinMax)^ do begin
    Xcentre := tMinMaxArray[4];
    Ycentre := tMinMaxArray[5];
    Scale := tMinMaxArray[6] * 2;
    if (Scale = 0) then begin
      Scale := 1.0;  //avoid potential div 0 errors
    end;
  end;

  with pCountFloatArray(Ptr)^ do begin
    with mBitmap do begin
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Mode := pmCopy;
      Canvas.Pen.Color := clRed;
      Canvas.Brush.Color := clWhite;
      Canvas.FillRect(rect(0,0,Width,Height)); //erase first
      //iX := round((aArray[0][0]-Xcentre)*255/Scale + 127.5);
      // condor has reversed X axis
      iX := round((aArray[0][0]-Xcentre)*255/-Scale + 127.5);
      iY := round((aArray[0][1]-ycentre)*255/Scale + 127.5);
      Canvas.MoveTo(iX,iY);
      for i := 1 to aCount-1 do begin
        //iX := round((aArray[i][0]-Xcentre)*255/Scale + 127.5);
        // condor has reversed X axis
        iX := round((aArray[i][0]-Xcentre)*255/-Scale + 127.5);
        iY := round((aArray[i][1]-Ycentre)*255/Scale + 127.5);
        Canvas.LineTo(iX,iY);
//  if (i >= StrToInt(Unit_Objects.Form_Objects.Edit_Steps.text)) then begin
//    break;
//  end;
      end;
    end;
  end;
end;

// plot surfaces
{----------------------------------------------------------------------------}
Procedure msPlotIt(Ptr : pointer);
var
  i, j : integer;
  Xcentre, Ycentre,Scale : double;
  iX, iY : integer;

begin
  if (pMesh(Ptr)^.tRotation <> 0) then begin
  end;
  if (pMesh(Ptr)^.tHeight <> 0) then begin
  end;
  with pMesh(Ptr)^ do begin
    Xcentre := tMinMaxArray[4];
    Ycentre := tMinMaxArray[5];
    Scale := tMinMaxArray[6] * 2;
    if (Scale = 0) then begin
      Scale := 1.0;  //avoid potential div 0 errors
    end;
  end;

//  with pCountFloatArray(Ptr)^ do begin
  with pMesh(Ptr)^ do begin
    with mBitmap do begin
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Mode := pmCopy;
      Canvas.Pen.Color := clRed;
      Canvas.Brush.Color := clWhite;
      Canvas.FillRect(rect(0,0,Width,Height)); //erase first
      for i := 0 to sArray.aCount-1 do begin

        iX := round((tArray.aArray[sArray.aArray[i].aArray[sArray.aArray[i].aCount-1]][0]-Xcentre)*255/-Scale + 127.5);  // condor has reversed X axis
        iY := round((tArray.aArray[sArray.aArray[i].aArray[sArray.aArray[i].aCount-1]][1]-ycentre)*255/ Scale + 127.5);
        Canvas.MoveTo(iX,iY);
        for j := 0 to sArray.aArray[i].aCount-1 do begin
          iX := round((tArray.aArray[sArray.aArray[i].aArray[j]][0]-Xcentre)*255/-Scale + 127.5);
          iY := round((tArray.aArray[sArray.aArray[i].aArray[j]][1]-ycentre)*255/ Scale + 127.5);
          Canvas.LineTo(iX,iY);
        end;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure ExtractFrame(var NewFrame : tFrame; oTreeView : TTreeView; Index : integer);
var
 P : pFrame;
begin
  with oTreeView do begin
    P := pFrame(pObjectItem(Items[Index].data)^.oPointer);
    with NewFrame do begin
      tName := P^.tName;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure ExtractMesh(var NewMesh : tMesh; oTreeView : TTreeView; Index : integer);
var
 P : pMesh;
 i, j : longword;
begin
  with oTreeView do begin
    P := pMesh(pObjectItem(Items[Index].data)^.oPointer);
    with NewMesh do begin
      tName := P^.tName;
      // tMinMaxArray := P^.tMinMaxArray;
      for i := 0 to 9-1 do begin
        tMinMaxArray[i] := P^.tMinMaxArray[i];
      end;
      tRotation := P^.tRotation;
      tX:= P^.tX;
      tY:= P^.tY;
      tHeight:= P^.tHeight;
      tScale:= P^.tScale;
      // tArray := P^.tArray;
      tArray.aCount := P^.tArray.aCount;
      setlength(tArray.aArray,tArray.aCount,3);
      for i := 0 to tArray.aCount-1 do begin
        for j := 0 to 3-1 do begin
          tArray.aArray[i][j] := P^.tArray.aArray[i][j];
        end;
      end;
    //test  P^.tArray.aArray[0][1] := 9999;
      //sArray := P^.sArray;
      sArray.aCount := P^.sArray.aCount;
      setlength(sArray.aArray,sArray.aCount);
      for i := 0 to sArray.aCount-1 do begin
        sArray.aArray[i].aCount := P^.sArray.aArray[i].aCount;
        setlength(sArray.aArray[i].aArray,sArray.aArray[i].aCount);
        for j := 0 to 3-1 do begin
          sArray.aArray[i].aArray[j] := P^.sArray.aArray[i].aArray[j];
        end;
      end;
   //test  P^.sArray.aArray[0].aArray[0] := 8888;
//    CommonSurfaces := sArray;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure ExtractNormals(var NewNormals : tMeshNormals; oTreeView : TTreeView; Index : integer);
var
 P : pMeshNormals;
 i, j : longword;
begin
  with oTreeView do begin
    P := pMeshNormals(pObjectItem(Items[Index].data)^.oPointer);
    with NewNormals do begin
      tName := P^.tName;
      // tArray := P^.tArray;
      tArray.aCount := P^.tArray.aCount;
      setlength(tArray.aArray,tArray.aCount,3);
      for i := 0 to tArray.aCount-1 do begin
        for j := 0 to 3-1 do begin
          tArray.aArray[i][j] := P^.tArray.aArray[i][j];
        end;
      end;
    //test  P^.tArray.aArray[0][1] := 9999;
      //sArray := P^.sArray;
      sArray.aCount := P^.sArray.aCount;
      setlength(sArray.aArray,sArray.aCount);
      for i := 0 to sArray.aCount-1 do begin
        sArray.aArray[i].aCount := P^.sArray.aArray[i].aCount;
        setlength(sArray.aArray[i].aArray,sArray.aArray[i].aCount);
        for j := 0 to 3-1 do begin
          sArray.aArray[i].aArray[j] := P^.sArray.aArray[i].aArray[j];
        end;
      end;
   //test  P^.sArray.aArray[0].aArray[0] := 8888;
//    sArray := CommonSurfaces;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure ExtractTcoords(var NewTcoords : tMeshTCoord; oTreeView : TTreeView; Index : integer);
var
 P : pMeshTcoord;
 i, j : longword;
begin
  with oTreeView do begin
    P := pMeshTcoord(pObjectItem(Items[Index].data)^.oPointer);
    with NewTcoords do begin
      tName := P^.tName;
      // tArray := P^.tArray;
      tArray.aCount := P^.tArray.aCount;
      setlength(tArray.aArray,tArray.aCount,2);
      for i := 0 to tArray.aCount-1 do begin
        for j := 0 to 2-1 do begin
          tArray.aArray[i][j] := P^.tArray.aArray[i][j];
        end;
      end;
    //test  P^.tArray.aArray[0][1] := 7777;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure ExtractMaterialList(var NewMaterialList : tMaterialList; oTreeView : TTreeView; Index : integer);
var
 P : pMaterialList;
 i : longword;
begin
  with oTreeView do begin
    P := pMaterialList(pObjectItem(Items[Index].data)^.oPointer);
    with NewMaterialList do begin
      tName := P^.tName;
      tCount := P^.tCount;
      tsCount := P^.tsCount;
      //sArray : tIntArray;
      setlength(sArray,tsCount);
      for i := 0 to tsCount-1 do begin
        sArray[i] := P^.sArray[i];
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure ExtractMaterial(var NewMaterial : tMaterial; oTreeView : TTreeView; Index : integer);
var
 P : pMaterial;
 i, j : longword;
begin
  with oTreeView do begin
    P := pMaterial(pObjectItem(Items[Index].data)^.oPointer);
    with NewMaterial do begin
      tName := P^.tName;
      //tRGBA := P^.tRGBA;
      setlength(tRGBA,4);
      for i := 0 to 4-1 do begin
        tRGBA[i] := P^.tRGBA[i];
      end;
      tPower := P^.tPower;
      //tRGBs := P^.tRGBs;
      setlength(tRGBs,3);
      for i := 0 to 3-1 do begin
        tRGBs[i] := P^.tRGBs[i];
      end;
      //tRGBr := P^.tRGBr;
      setlength(tRGBr,3);
      for i := 0 to 3-1 do begin
        tRGBr[i] := P^.tRGBr[i];
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure ExtractTFileName(var NewFileName : tpFileName; oTreeView : TTreeView; Index : integer);
var
 P : pFileName;
begin
  with oTreeView do begin
    P := pFileName(pObjectItem(Items[Index].data)^.oPointer);
    with NewFileName do begin
      tName := P^.tName;
      tqName := P^.tqName;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure sExtract(oTreeView : TTreeView; Index : integer; var o_Object : TArray_CoordXY_Array);
var
  i, j : integer;
//  Xcentre, Ycentre,Scale : double;
//  iX, iY : integer;
  Ptr : pointer;

begin
  with oTreeView do begin
    Ptr := pMesh(pObjectItem(Items[Index].data)^.oPointer);
{
    if (pMesh(Ptr)^.tRotation <> 0) then begin
    end;
    if (pMesh(Ptr)^.tHeight <> 0) then begin
    end;
    with pMesh(Ptr)^ do begin
      Xcentre := tMinMaxArray[4];
      Ycentre := tMinMaxArray[5];
      Scale := tMinMaxArray[6] * 2;
      if (Scale = 0) then begin
        Scale := 1.0;  //avoid potential div 0 errors
      end;
    end;
}
    with pMesh(Ptr)^ do begin
      // need to set the size of the number of surfaces
      SetLength(o_Object,sArray.aCount);
      for i := 0 to sArray.aCount-1 do begin
        // need to set the size of the number of vertices
//        SetLength(o_Object[i],sArray.aArray[i].aCount+1); // one extra to close polygon
        SetLength(o_Object[i],sArray.aArray[i].aCount);
//        o_Object[i][0].X := (tArray.aArray[sArray.aArray[i].aArray[sArray.aArray[i].aCount-1]][0]{-Xcentre});
//        o_Object[i][0].Y := (tArray.aArray[sArray.aArray[i].aArray[sArray.aArray[i].aCount-1]][1]{-ycentre});
        for j := 0 to sArray.aArray[i].aCount-1 do begin
          o_Object[i][j].X := (tArray.aArray[sArray.aArray[i].aArray[j]][0]{-Xcentre});
          o_Object[i][j].Y := (tArray.aArray[sArray.aArray[i].aArray[j]][1]{-ycentre});
        end;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure vExtract(oTreeView : TTreeView; Index : integer; var o_Object : TCoordXY_Array);
var
  i{, j} : integer;
//  Xcentre, Ycentre,Scale : double;
//  iX, iY : integer;
  Ptr : pointer;

begin
  with oTreeView do begin
    Ptr := pMesh(pObjectItem(Items[Index].data)^.oPointer);
{
    if (pMesh(Ptr)^.tRotation <> 0) then begin
    end;
    if (pMesh(Ptr)^.tHeight <> 0) then begin
    end;
    with pMesh(Ptr)^ do begin
      Xcentre := tMinMaxArray[4];
      Ycentre := tMinMaxArray[5];
      Scale := tMinMaxArray[6] * 2;
      if (Scale = 0) then begin
        Scale := 1.0;  //avoid potential div 0 errors
      end;
    end;
}
    with pMesh(Ptr)^ do begin
      // need to set the size of the number of vertices
//      SetLength(o_Object,tArray.aCount+1); // one extra to close polygon
      SetLength(o_Object,tArray.aCount);
//        o_Object[0].X := (tArray.aArray[tArray.aCount-1][0]{-Xcentre});
//        o_Object[0].Y := (tArray.aArray[tArray.aCount-1][1]{-ycentre});
      for i := 0 to tArray.aCount-1 do begin
        o_Object[i].X := (tArray.aArray[i][0]{-Xcentre});
        o_Object[i].Y := (tArray.aArray[i][1]{-ycentre});
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure ExtractVarray(vPtr : pCountFloatArray; oTreeView : TTreeView; Index : integer);
begin
  setlength(vPtr^.aArray,3);
  // verify it's a frame
  if ( (oTreeView.Items[Index].data <> nil) AND
       (pObjectItem(oTreeView.Items[Index].data)^.oType in
       [oFrame]) ) then begin
    with pFrame(pObjectItem(oTreeView.Items[Index].data)^.oPointer)^ do begin
      New(pObjectData); //allocate space for an object data
      pObjectData^.oType := oFrame;
      New(pFrameData); //allocate space for a frame data
      pObjectData^.oPointer := pFrameData;
      pFrameData^.tName := tname+'3D';
//      fTreeNode := oTreeView.Items.AddChildObject(nil, 'Frame '+pFrameData^.tName, pObjectData);
    end;
  end;
end;

{----------------------------------------------------------------------------}
function CopyAndAddFrame(oTreeView : TTreeView; Index : integer) : integer;
var
 fTreeNode,fTreeNode2 : TTreeNode;
begin
  // verify it's a frame
  if ( (oTreeView.Items[Index].data <> nil) AND
       (pObjectItem(oTreeView.Items[Index].data)^.oType in
       [oFrame]) ) then begin
    with pFrame(pObjectItem(oTreeView.Items[Index].data)^.oPointer)^ do begin
      New(pObjectData); //allocate space for an object data
      pObjectData^.oType := oFrame;
      New(pFrameData); //allocate space for a frame data
      pObjectData^.oPointer := pFrameData;
      pFrameData^.tName := tname+'3D';
//      fTreeNode := oTreeView.Items.AddChildObject(nil, 'Frame '+pFrameData^.tName, pObjectData);
//      result := oTreeView.Items.Count-1;
//      fTreeNode := oTreeView.Items.AddObjectFirst(oTreeView.Items[Index], 'Frame '+pFrameData^.tName, pObjectData);
      fTreeNode := oTreeView.Items.InsertObject(oTreeView.Items[Index], 'Frame '+pFrameData^.tName, pObjectData);
      result := Index;
    end;
  end;
  INC(index,2); // 2 because just inserted and extra one above
  // mesh
  with pMesh(pObjectItem(oTreeView.Items[Index].data)^.oPointer)^ do begin
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oMesh;
    New(pMeshData); //allocate space for data
    pMeshData^.tName := tname+'3D';
    pMeshData^.tRotation := tRotation;
    pMeshData^.tMinMaxArray := tminMaxArray;
    pMeshData^.tX := tX;
    pMeshData^.tY := tY;
    pMeshData^.tHeight := tHeight;
    pMeshData^.tScale := tScale;
    // Do vertices
    pMeshData^.tArray.aCount := tArray.aCount;
    setLength(pMeshData^.tArray.aArray,pMeshData^.tArray.aCount);
    pMeshData^.tArray := tArray;
    // Do surfaces
    pMeshData^.sArray.aCount := sArray.aCount;
    setLength(pMeshData^.sArray.aArray,pMeshData^.sArray.aCount);
    pMeshData^.sArray := sArray;
    pObjectData^.oPointer := pMeshData;
    fTreeNode2 := oTreeView.Items.AddChildObject(fTreeNode, 'Mesh '+pMeshData^.tName, pObjectData);
  end;
  INC(index,2);
  // mesh normals
  with pMeshNormals(pObjectItem(oTreeView.Items[Index].data)^.oPointer)^ do begin
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oMeshNormals;
    New(pMeshNormalsData); //allocate space for data
    pMeshNormalsData^.tName := tName+'3D';
    pMeshNormalsData^.tArray.aCount := tArray.aCount;
    setLength(pMeshNormalsData^.tArray.aArray,pMeshNormalsData^.tArray.aCount);
    pMeshNormalsData^.tArray := tArray;
    // Do surfaces
    pMeshNormalsData^.sArray.aCount := sArray.aCount;
    setLength(pMeshNormalsData^.sArray.aArray,pMeshNormalsData^.sArray.aCount);
    pMeshNormalsData^.sArray := sArray;
    pObjectData^.oPointer := pMeshNormalsData;
    oTreeView.Items.AddChildObject(fTreeNode2, 'MeshNormals', pObjectData);
  end;
  INC(index,2);
  // mesh texture coordinates
  with pMeshTcoord(pObjectItem(oTreeView.Items[Index].data)^.oPointer)^ do begin
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oMeshTextureCoord;
    New(pMeshTcoordData); //allocate space for data
    pMeshTcoordData^.tName := tName+'3D';
    pMeshTcoordData^.tArray.aCount := tArray.aCount;
    setLength(pMeshTcoordData^.tArray.aArray,pMeshTcoordData^.tArray.aCount);
    pMeshTcoordData^.tArray := tArray;
    pMeshTcoordData^.sIndex := pMeshData;
    pObjectData^.oPointer := pMeshTcoordData;
    oTreeView.Items.AddChildObject(fTreeNode2, 'MeshTextureCoords', pObjectData);
  end;
  INC(index,2);
  // material list
  with pMaterialList(pObjectItem(oTreeView.Items[Index].data)^.oPointer)^ do begin
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oMaterialList;
    New(pMaterialListData); //allocate space for data
    pMaterialListData^.tName := tName;
    pMaterialListData^.tCount := tCount;
    pMaterialListData^.tsCount := tsCount;
    SetLength(pMaterialListData^.sArray,pMaterialListData^.tsCount);
    pMaterialListData^.sArray := sArray;
    pObjectData^.oPointer := pMaterialListData;
    fTreeNode2 := oTreeView.Items.AddChildObject(fTreeNode2, 'MaterialList',pObjectData);
  end;
  INC(index,2);
  // material
  with pMaterial(pObjectItem(oTreeView.Items[Index].data)^.oPointer)^ do begin
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oMaterial;
    New(pMaterialData); //allocate space for data
    setLength(pMaterialData^.tRGBA,4);
    pMaterialData^.tRGBA := tRGBA;
    pObjectData^.oPointer := pMaterialData;
    pMaterialData^.tPower := tPower;
    setLength(pMaterialData^.tRGBs,3);
    pMaterialData^.tRGBs := tRGBs;
    setLength(pMaterialData^.tRGBr,3); // condor lighting
    pMaterialData^.tRGBr := tRGBr;
    // for Condor C3D
    fTreeNode2 := oTreeView.Items.AddChildObject(fTreeNode2, 'Material', pObjectData);
  end;
  INC(index,2);
  // filename
  with pFileName(pObjectItem(oTreeView.Items[Index].data)^.oPointer)^ do begin
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oFileName;
    New(pFileNameData); //allocate space for data
    pFileNameData^.tName := tName;
    pFileNameData^.tqName := 'green_64.dds';
    pObjectData^.oPointer := pFileNameData;
    oTreeView.Items.AddChildObject(fTreeNode2, 'FileName', pObjectData);
  end;

end;

{----------------------------------------------------------------------------
Condor .C3D object coordinate encoding/decoding
----------------------------------------------------------------------------}

{----------------------------------------------------------------------------
C3D file structure
  Magic = 'C3D';
  Header_C3D = packed record
    a : longword;             // don't know what this is
    b : longword;             // could be version a.b ?
    Number_Objects : longword;
    Number_TBD : longword;    // don't know what this is
  end;
  Array[Number_Objects] of
    Object_C3D = packed record
      Name : string[255];   //  not fixed length
      Indexes_C3D = packed record
        Vertex_Offset : longword;
        NumVertices : longword;
        Surface_Offset : longword;
        NumSurfaceVertices : longword;
      end;
      TexturePath : string[255]; //  not fixed length
      Lighting_C3D = packed record
        a_Lighting : single;  // red ?
        b_Lighting : single;  // blue ?
        c_Lighting : single;  // green ?
        d_Lighting : single;  // alpha ?
        e_Lighting : single;  // specular
        f_Lighting : single;  // shiny
        g_Lighting : single;  // env
      end;
    end;
  end;
  NumVertices : longword;
  Array[NumVertices] of
    Mesh_C3D = packed record
      X, Y, Z, normalX, normalY, normalZ, TextureX, TextureY : single;
    end;
  end;
  NumSurfaceVertices : longword;
  Array[NumSurfaceVertices div 3] of
    Surface_C3D = packed record
      vertex1, vertex2, vertex3 : longword;  // triangles, 3 vertices per surface
    end;
  end;

----------------------------------------------------------------------------}

const
  C3D_Magic = 'C3D';

type
  Header_C3D = packed record
    a : longword;
    b : longword;
    Number_Objects : longword;
    Number_TBD : longword;
  end;

  Indexes_C3D = packed record
    Vertex_Offset : longword;
    NumVertices : longword;
    Surface_Offset : longword;
    NumSurfaceVertices : longword;
  end;

  Lighting_C3D = packed record
    a_Lighting : single;
    b_Lighting : single;
    c_Lighting : single;
    d_Lighting : single;
    e_Lighting : single;
    f_Lighting : single;
    g_Lighting : single;
  end;

  Object_C3D = packed record
    Name : string[255];   //  not fixed length
    Indexes : Indexes_C3D;
    TexturePath : string[255]; //  not fixed length
    Lighting : Lighting_C3D;
  end;

  Mesh_C3D = packed record
    X, Y, Z, normalX, normalY, normalZ, TextureX, TextureY : single;
  end;
  aMesh_C3D =  array[0..1] of Mesh_C3D; // cannot use 'array of' because there is overhead data to be accounted for

  Surface_C3D = packed record
    vertex1, vertex2, vertex3 : longword;
  end;
  aSurface_C3D =  array[0..1] of Surface_C3D; // cannot use 'array of' because there is overhead data to be accounted for

var
  Magic_3D : array[0..3-1] of char;
  Headers_C3D : Header_C3D;
  Objects_C3D : array of Object_C3D;
  OverallNumVertices_C3D : Longword;
  OverallNumVertices : Longword;
  Meshes_C3D : array of Mesh_C3D;
  OverallNumSurfaceVertices_C3D : Longword;
  OverallNumSurfaces : Longword;
  Surfaces_C3D : array of Surface_C3D;
  C3D_File : file of byte;
  Temp : longword;

var
  p3DmagicData : p3Dmagic;
  p3DheaderData : p3Dheader;
//  p3DframeData : p3Dframe;
//  p3DmeshData : p3Dmesh;
//  p3DmeshNormalsData : p3DmeshNormals;
//  p3DmeshTcoordData : p3DmeshTcoord;
//  p3DmaterialData : p3Dmaterial;
//  p3DfileNameData : p3DfileName;

//----------------------------------------------------------------------------
procedure CondorC3Dfile_CreateTreeViewVersion(Append : Boolean);
var
  i, j : integer;
  fTreeNode, fTreeNode2 : TTreeNode;

begin
  if (Not Append) then begin
    ClearTreeView(oTreeView);

    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := o3Dmagic;
    New(p3DmagicData); //allocate space for data
    pObjectData^.oPointer := p3DmagicData;
    oTreeView.Items.AddChildObject(nil, C3D_Magic, pObjectData);

    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := o3Dheader;
    New(p3DheaderData); //allocate space for data
    pObjectData^.oPointer := p3DheaderData;
    oTreeView.Items.AddChildObject(nil, 'Header', pObjectData);
  end;

  for i := 0 to Headers_C3D.Number_Objects-1 do begin
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oFrame;
    New(pFrameData); //allocate space for a frame data
    pObjectData^.oPointer := pFrameData;
    pFrameData^.tName := 'f_'+Objects_C3D[i].name;
    fTreeNode := oTreeView.Items.AddChildObject(nil, 'Frame '+pFrameData^.tName, pObjectData);

    if (InjectFTM) then begin
      // FTM
      New(pObjectData); //allocate space for an object data
      pObjectData^.oType := oFTM;
      New(pFTMdata); //allocate space for data
      pFTMdata^.tName := '';
      pObjectData^.oPointer := pFTMdata;
      {fTreeNode :=} oTreeView.Items.AddChildObject(fTreeNode, 'FTM '+pFTMdata^.tName, pObjectData);
//  ParseFloatArray(16,@pFTMdata.ftmArray,[',']);
//  SetLength(pFloatArray(pFTMdata.ftmArray)^,16);
      SetLength(tFloatArray(pFTMdata^.ftmArray),16);
      for j := 0 to 16-1 do begin
        tFloatArray(pFTMdata^.ftmArray)[j] := FTM[j];
      end;
    end;

    // mesh
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oMesh;
    New(pMeshData); //allocate space for data
    pMeshData^.tName := Objects_C3D[i].name;
    pMeshData^.tRotation := 0.0;
    pMeshData^.tX := 0.0;
    pMeshData^.tY := 0.0;
    pMeshData^.tHeight := 0.0;
    pMeshData^.tScale := 1.0;
    // option 1 - copy the data into existing structure - routines on structure still work
    // Do vertices
    pMeshData^.tArray.aCount := Objects_C3D[i].Indexes.NumVertices;
    setLength(pMeshData^.tArray.aArray,pMeshData^.tArray.aCount);
    for j := 0 to pMeshData^.tArray.aCount-1 do begin
      setLength(pMeshData^.tArray.aArray[j],3);
      pMeshData^.tArray.aArray[j][0] := Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset + j].X;
      pMeshData^.tArray.aArray[j][1] := Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset + j].Y;
      pMeshData^.tArray.aArray[j][2] := Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset + j].Z;
    end;
    CalcExtents(@pMeshData^.tArray,@pMeshData^);
    // Do surfaces
    // tricky - triangles in groups of 3 and absolute vertices need to be made relative
    pMeshData^.sArray.aCount := Objects_C3D[i].Indexes.NumSurfaceVertices div 3;
    setLength(pMeshData^.sArray.aArray,pMeshData^.sArray.aCount);
    for j := 0 to pMeshData^.sArray.aCount-1 do begin
      pMeshData^.sArray.aArray[j].aCount := 3;
      setLength(pMeshData^.sArray.aArray[j].aArray,pMeshData^.sArray.aArray[j].aCount);
      pMeshData^.sArray.aArray[j].aArray[0] := Surfaces_C3D[Objects_C3D[i].Indexes.Surface_Offset div 3 + j].Vertex1
                                               - Objects_C3D[i].Indexes.Vertex_Offset;
      pMeshData^.sArray.aArray[j].aArray[1] := Surfaces_C3D[Objects_C3D[i].Indexes.Surface_Offset div 3 + j].Vertex2
                                               - Objects_C3D[i].Indexes.Vertex_Offset;
      pMeshData^.sArray.aArray[j].aArray[2] := Surfaces_C3D[Objects_C3D[i].Indexes.Surface_Offset div 3 + j].Vertex3
                                               - Objects_C3D[i].Indexes.Vertex_Offset;
//      SavedMeshData := pMeshData;
    end;
//    // option 2 - no copy, just point to the data - need new routines to operate on structure
//    // Do vertices
//    //pMeshData^.mCount := Objects_C3D[i].Indexes.NumVertices;
//    //pMeshData^.mArray := @Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset{*sizeof(Mesh_C3D)}];
//    // CalcExtents(???);
//    // Do surfaces
    pObjectData^.oPointer := pMeshData;
//    fTreeNode2 := oTreeView.Items.AddChildObject(fTreeNode, 'Mesh', pObjectData);
    fTreeNode2 := oTreeView.Items.AddChildObject(fTreeNode, 'Mesh '+pMeshData^.tName, pObjectData);

    // mesh normals
    // option 1 - copy the data into existing structure - routines on structure still work
    // Do normals
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oMeshNormals;
    New(pMeshNormalsData); //allocate space for data
    pMeshNormalsData^.tName := 'mn_'+Objects_C3D[i].name;
    // option 1 - copy the data into existing structure - routines on structure still work
    pMeshNormalsData^.tArray.aCount := Objects_C3D[i].Indexes.NumVertices;
    setLength(pMeshNormalsData^.tArray.aArray,pMeshNormalsData^.tArray.aCount);
    for j := 0 to pMeshNormalsData^.tArray.aCount-1 do begin
      setLength(pMeshNormalsData^.tArray.aArray[j],3);
      pMeshNormalsData^.tArray.aArray[j][0] := Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset + j].NormalX;
      pMeshNormalsData^.tArray.aArray[j][1] := Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset + j].NormalY;
      pMeshNormalsData^.tArray.aArray[j][2] := Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset + j].NormalZ;
    end;
    // Do surfaces
    // tricky - triangles in groups of 3 and absolute vertices need to be made relative
    pMeshNormalsData^.sArray.aCount := Objects_C3D[i].Indexes.NumSurfaceVertices div 3;
    setLength(pMeshNormalsData^.sArray.aArray,pMeshNormalsData^.sArray.aCount);
    for j := 0 to pMeshNormalsData^.sArray.aCount-1 do begin
      pMeshNormalsData^.sArray.aArray[j].aCount := 3;
      setLength(pMeshNormalsData^.sArray.aArray[j].aArray,pMeshNormalsData^.sArray.aArray[j].aCount);
      pMeshNormalsData^.sArray.aArray[j].aArray[0] := Surfaces_C3D[Objects_C3D[i].Indexes.Surface_Offset div 3 + j].Vertex1
                                               - Objects_C3D[i].Indexes.Vertex_Offset;
      pMeshNormalsData^.sArray.aArray[j].aArray[1] := Surfaces_C3D[Objects_C3D[i].Indexes.Surface_Offset div 3 + j].Vertex2
                                               - Objects_C3D[i].Indexes.Vertex_Offset;
      pMeshNormalsData^.sArray.aArray[j].aArray[2] := Surfaces_C3D[Objects_C3D[i].Indexes.Surface_Offset div 3 + j].Vertex3
                                               - Objects_C3D[i].Indexes.Vertex_Offset;
    end;
//    // option 2 - point to in mesh and not really needed
//    // TBD ?
    pObjectData^.oPointer := pMeshNormalsData;
    oTreeView.Items.AddChildObject(fTreeNode2, 'MeshNormals', pObjectData);

    // mesh texture coordinates
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oMeshTextureCoord;
    New(pMeshTcoordData); //allocate space for data
    pMeshTcoordData^.tName := 'tc_'+Objects_C3D[i].name;
    // option 1 - copy the data into existing structure - routines on structure still work
    pMeshTcoordData^.tArray.aCount := Objects_C3D[i].Indexes.NumVertices;
    setLength(pMeshTcoordData^.tArray.aArray,pMeshTcoordData^.tArray.aCount);
    for j := 0 to pMeshTcoordData^.tArray.aCount-1 do begin
//      setLength(pMeshTcoordData^.tArray.aArray[j],3);
      setLength(pMeshTcoordData^.tArray.aArray[j],2);
      pMeshTcoordData^.tArray.aArray[j][0] := Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset+j].TextureX;
      pMeshTcoordData^.tArray.aArray[j][1] := Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset+j].TextureY;
    end;
//    pMeshTcoordData^.sIndex := SavedMeshData;
    pMeshTcoordData^.sIndex := pMeshData;
//    // option 2 - no copy, just point to the data - need new routines to operate on structure
//    //p3DmeshTcoordData^.mCount := Objects_C3D[i].Indexes.NumVertices;
//    //p3DmeshTcoordData^.mArray := @Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset{*sizeof(Mesh_C3D)}];
    pObjectData^.oPointer := pMeshTcoordData;
//    oTreeView.Items.AddChildObject(fTreeNode, 'MeshTextureCoords', pObjectData);
    oTreeView.Items.AddChildObject(fTreeNode2, 'MeshTextureCoords', pObjectData);

    // add dummy material List
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oMaterialList;
    New(pMaterialListData); //allocate space for data
    pMaterialListData^.tName := '';
    pMaterialListData^.tCount := 1;
    pMaterialListData^.tsCount := 1;
    SetLength(pMaterialListData^.sArray,pMaterialListData^.tsCount);
    pMaterialListData^.sArray[0] := 0;
    pObjectData^.oPointer := pMaterialListData;
    fTreeNode2 := oTreeView.Items.AddChildObject(fTreeNode2, 'MaterialList',pObjectData);

    // material
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oMaterial;
    New(pMaterialData); //allocate space for data
    //p3DmaterialData^.tName := 'mat_'+Objects_C3D[i].name;
    // option 1 - copy the data into existing structure - routines on structure still work
    setLength(pMaterialData^.tRGBA,4);
    pMaterialData^.tRGBA[0] := Objects_C3D[i].Lighting.a_Lighting;
    pMaterialData^.tRGBA[1] := Objects_C3D[i].Lighting.b_Lighting;
    pMaterialData^.tRGBA[2] := Objects_C3D[i].Lighting.c_Lighting;
    pMaterialData^.tRGBA[3] := Objects_C3D[i].Lighting.d_Lighting;
    pObjectData^.oPointer := pMaterialData;
    pMaterialData^.tPower := 999.0;  // Flag for Condor lighting
    setLength(pMaterialData^.tRGBs,3);
    pMaterialData^.tRGBs[0] := Objects_C3D[i].Lighting.e_Lighting;  // not used
    pMaterialData^.tRGBs[1] := Objects_C3D[i].Lighting.f_Lighting;
    pMaterialData^.tRGBs[2] := Objects_C3D[i].Lighting.g_Lighting;
    setLength(pMaterialData^.tRGBr,3); // condor lighting
    pMaterialData^.tRGBr[0] := 0.0;
    pMaterialData^.tRGBr[1] := 0.0;
    pMaterialData^.tRGBr[2] := 0.0;
    // for Condor C3D
//    fTreeNode2 := oTreeView.Items.AddChildObject(fTreeNode, 'Material', pObjectData);
    fTreeNode2 := oTreeView.Items.AddChildObject(fTreeNode2, 'Material', pObjectData);

    // filename
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oFileName;
    New(pFileNameData); //allocate space for data
    pFileNameData^.tName := '';
    pFileNameData^.tqName := Objects_C3D[i].TexturePath;
    pObjectData^.oPointer := pFileNameData;
    oTreeView.Items.AddChildObject(fTreeNode2, 'FileName', pObjectData);
//    oTreeView.Items.AddChildObject(fTreeNode, 'FileName', pObjectData);
  end;

end;

//----------------------------------------------------------------------------
procedure readCondorC3Dfile(FileName : string; Append : Boolean);
var
  i : integer;

begin
  FileError := false;
  AssignFile(C3D_File,FileName);
  Reset(C3D_File);

  try
    // read first 3 bytes
    blockRead (C3D_file,Magic_3D,sizeof(Magic_3D));
    if (Magic_3D <> C3D_Magic) then begin
      FileError := true;
      exit; // error
    end else begin
      with Headers_C3D do begin
        blockRead (C3D_file,Headers_C3D,sizeof(Header_C3D));
        setlength(Objects_C3D, Number_Objects); // create array of objects based on object count
        OverallNumVertices := 0;
        OverallNumSurfaces := 0;
        for i := 0 to Number_Objects-1 do begin
          blockRead (C3D_file,Objects_C3D[i].name[0],1);  // get length of object name
          blockRead (C3D_file,Objects_C3D[i].name[1],integer(Objects_C3D[i].name[0])); // get object name
// no spaces in object name !
Objects_C3D[i].name := StringReplace(Objects_C3D[i].name, ' ', '_', [rfReplaceAll]);
          blockRead (C3D_file,Objects_C3D[i].Indexes,sizeof(Indexes_C3D)); // get object mesh data
          blockRead (C3D_file,Objects_C3D[i].TexturePath[0],1);  // get length of object name
          blockRead (C3D_file,Objects_C3D[i].TexturePath[1],integer(Objects_C3D[i].TexturePath[0])); // get object nameg
          blockRead (C3D_file,Objects_C3D[i].Lighting,sizeof(Lighting_C3D)); // get object data
          OverallNumVertices := OverallNumVertices + Objects_C3D[i].Indexes.NumVertices;
          OverallNumSurfaces := OverallNumSurfaces + Objects_C3D[i].Indexes.NumSurfaceVertices div 3;  // triangles, 3 vertices per surface
        end;
      end;
      // verify size of mesh
      blockRead (C3D_file,OverallNumVertices_C3D,sizeof(OverallNumVertices_C3D)); // get size from file
      if (OverallNumVertices_C3D <> OverallNumVertices) then begin
        FileError := true;
        exit; // error
      end;
      // create Vertex array;
      setlength(Meshes_C3D, OverallNumVertices); // create array of objects based on object count
      blockRead (C3D_file,Meshes_C3D[0],OverallNumVertices*sizeof(Mesh_C3D)); // get object mesh data

      // verify size of surfaces
      blockRead (C3D_file,OverallNumSurfaceVertices_C3D,sizeof(OverallNumSurfaceVertices_C3D)); // get size from file
      if (OverallNumSurfaceVertices_C3D <> OverallNumSurfaces * 3) then begin
        FileError := true;
        exit; // error
      end;
      // create Surface array;
      setlength(Surfaces_C3D, OverallNumSurfaces); // create array of objects based on object count
      blockRead (C3D_file,Surfaces_C3D[0],OverallNumSurfaces*sizeof(Surface_C3D)); // get object surface data
    end;
  finally
   closefile(C3D_File);
  end;

  if (NOT FileError) then begin
    CondorC3Dfile_CreateTreeViewVersion(Append);
  end;
end;

//---------------------------------------------------------------------------
function FindNodeByName(NodeName : string; nType : oObjType) : integer;
var
  i : integer;

begin
  result := -1; // assume not found at first
  //search tree for material with matching name
  with oTreeView do begin
    i := 0;
    while  (i <= Items.Count-1) do begin
      if (Items[i].data <> nil) then begin
        if (pObjectItem(Items[i].data)^.oType = nType) then begin
          // use pMaterial convert to use tName even if not pMaterial for now
          if (pMaterial(pObjectItem(Items[i].data)^.oPointer)^.tName = NodeName) then begin
            result := i;
            break;
          end;
        end;
      end;
      INC(i);
    end;
  end;
end;

//---------------------------------------------------------------------------
function FindMaterialByName(matName : string) : integer;
var
  i : integer;

begin
  result := -1; // assume not found at first
  //search tree for material with matching name
  with oTreeView do begin
    i := 0;
    while  (i <= Items.Count-1) do begin
      if (Items[i].data <> nil) then begin
        case pObjectItem(Items[i].data)^.oType of
          oMaterial: begin
            with pMaterial(pObjectItem(Items[i].data)^.oPointer)^ do begin
              if (tName = matName) then begin
                result := i;
                break;
              end else begin // skip
                INC(i);
              end;
            end;
          end;
          else begin // skip
            INC(i);
          end;
        end;
      end else begin // skip
        INC(i);
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
function FindNodebyType(oTreeView : TTreeView; Index : integer;
                        nType : oObjType; var nName : string) : integer;
var
  i : integer;

begin
  result := -1; // assume not found at first
  //search tree for material with matching name
  with oTreeView do begin
    i := Index;
    while  (i <= Items.Count-1) do begin
      if (Items[i].data <> nil) then begin
        if (pObjectItem(Items[i].data)^.oType = nType) then begin
          with pMaterial(pObjectItem(Items[i].data)^.oPointer)^ do begin
            nName := tName;
          end;
          result := i;
          break;
        end else begin // skip
          INC(i);
        end;
      end else begin // skip
        INC(i);
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
function FindFTMbyName(FTMname : string) : integer;
var
  i : integer;

begin
  result := -1; // assume not found at first
  //search tree for material with matching name
  with oTreeView do begin
    i := 0;
    while  (i <= Items.Count-1) do begin
      if (Items[i].data <> nil) then begin
        case pObjectItem(Items[i].data)^.oType of
          oFTM: begin
            with pMaterial(pObjectItem(Items[i].data)^.oPointer)^ do begin
              if (tName = FTMName) then begin
                result := i;
                break;
              end else begin // skip
                INC(i);
              end;
            end;
          end;
          else begin // skip
            INC(i);
          end;
        end;
      end else begin // skip
        INC(i);
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
Procedure UpdateFTM(FTMname : string; FTM : Array of single);
var
  i : integer;
  Index : integer;
begin
//  Index := FindFTMbyName(FTMname);
  Index := FindNodeByName(FTMname, oFTM);
  if (Index <> -1) then begin
    for i := 0 to 16-1 do begin
      pFTM(pObjectItem(oTreeView.Items[Index].data)^.oPointer)^.ftmArray[i] := FTM[i];
    end;
  end;
end;

//----------------------------------------------------------------------------
Procedure Reset_FTM_Unity;
var
  i, j : Integer;
begin
  for i := 0 to 4-1 do begin
    for j := 0 to 4-1 do begin
      if (i = j) then begin
        FTM[i*4+j] := 1.0;
      end else begin
        FTM[i*4+j] := 0.0;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
Procedure UpdateTC(TCname : string; TC : Array of single);
var
//  i : integer;
  Index : integer;
begin
//  Index := FindTCbyName(TCname);
  Index := FindNodeByName(TCname, oMeshTextureCoord);
  if (Index <> -1) then begin
    pMeshTcoord(pObjectItem(oTreeView.Items[Index].data)^.oPointer)^.tArray.aArray[0][1] := TC[0];
    pMeshTcoord(pObjectItem(oTreeView.Items[Index].data)^.oPointer)^.tArray.aArray[1][1] := TC[1];
  end;
end;

//---------------------------------------------------------------------------
Procedure UpdateTF(TFname : string; F : string);
var
  Index : integer;
begin
  Index := FindNodeByName(TFname, oFileName);
  if (Index <> -1) then begin
    pFileName(pObjectItem(oTreeView.Items[Index].data)^.oPointer)^.tqName := F;
  end;
end;

//----------------------------------------------------------------------------
procedure CondorC3Dfile_CreateC3Dversion;
var
  NodeIndex : integer;
  FTMnodeIndex : integer;

{----------------------------------------------------------------------------}
Procedure WriteMaterialReference(ObjectIndex : integer);
var
  Index : integer;
begin
  with oTreeView do begin
    //default
    Objects_C3D[ObjectIndex].Lighting.a_Lighting := 1;
    Objects_C3D[ObjectIndex].Lighting.b_Lighting := 1;
    Objects_C3D[ObjectIndex].Lighting.c_Lighting := 1;
    Objects_C3D[ObjectIndex].Lighting.d_Lighting := 1;
    Objects_C3D[ObjectIndex].Lighting.e_Lighting := 0;
    Objects_C3D[ObjectIndex].Lighting.f_Lighting := 1;
    Objects_C3D[ObjectIndex].Lighting.g_Lighting := 0;
     // assume none at first
    Objects_C3D[ObjectIndex].TexturePath := '';
    // get actual filename not reference name
    Index := FindMaterialByName(pMaterialReference(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tName);
    if (Index <> -1) then begin
{      with pMaterial(pObjectItem(oTreeView.Items[Index].data)^.oPointer)^ do begin // pMaterialData
        Objects_C3D[ObjectIndex].Lighting.a_Lighting := tRGBA[0];
        Objects_C3D[ObjectIndex].Lighting.b_Lighting := tRGBA[1];
        Objects_C3D[ObjectIndex].Lighting.c_Lighting := tRGBA[2];
        Objects_C3D[ObjectIndex].Lighting.d_Lighting := tRGBA[3];
        Objects_C3D[ObjectIndex].Lighting.e_Lighting := tRGBs[0];
        Objects_C3D[ObjectIndex].Lighting.f_Lighting := tRGBs[1];
        Objects_C3D[ObjectIndex].Lighting.g_Lighting := tRGBs[2];
      end;
}
      if (oTreeView.Items[Index].Count <> 0) then begin
        Objects_C3D[ObjectIndex].TexturePath := pFileName(pObjectItem(Items[Index+1].data)^.oPointer)^.tqName;
      end;
    end;
  end;
  INC(NodeIndex);
end;

{----------------------------------------------------------------------------}
Procedure WriteMaterial(ObjectIndex : integer);
//var
//  j,Count : integer;

begin
  with oTreeView do begin
    with pMaterial(pObjectItem(oTreeView.Items[NodeIndex].data)^.oPointer)^ do begin // pMaterialData
      Objects_C3D[ObjectIndex].Lighting.a_Lighting := tRGBA[0];
      Objects_C3D[ObjectIndex].Lighting.b_Lighting := tRGBA[1];
      Objects_C3D[ObjectIndex].Lighting.c_Lighting := tRGBA[2];
      Objects_C3D[ObjectIndex].Lighting.d_Lighting := tRGBA[3];
      Objects_C3D[ObjectIndex].Lighting.e_Lighting := tRGBs[0];
      Objects_C3D[ObjectIndex].Lighting.f_Lighting := tRGBs[1];
      Objects_C3D[ObjectIndex].Lighting.g_Lighting := tRGBs[2];
      // use default
{      Objects_C3D[ObjectIndex].Lighting.a_Lighting := 1;
      Objects_C3D[ObjectIndex].Lighting.b_Lighting := 1;
      Objects_C3D[ObjectIndex].Lighting.c_Lighting := 1;
      Objects_C3D[ObjectIndex].Lighting.d_Lighting := 1;
      Objects_C3D[ObjectIndex].Lighting.e_Lighting := 0;
      Objects_C3D[ObjectIndex].Lighting.f_Lighting := 1;
      Objects_C3D[ObjectIndex].Lighting.g_Lighting := 0;
}
      INC(NodeIndex);
      Objects_C3D[ObjectIndex].TexturePath := '';  // assume no name at first
      while ( (NodeIndex <= oTreeView.Items.Count-1) AND (Items[NodeIndex].data <> nil) AND
        (pObjectItem(Items[NodeIndex].data)^.oType in [oFilename,oMaterialReference]) ) do begin
        case pObjectItem(Items[NodeIndex].data)^.oType of
          oFileName: begin
            Objects_C3D[ObjectIndex].TexturePath := pFileName(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tqName;
            INC(NodeIndex);
          end;
        end;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure WriteMaterialList(ObjectIndex : integer);
begin
  with oTreeView do begin
AA := oTreeView.Items[NodeIndex].Level;
    INC(NodeIndex);
BB := oTreeView.Items[NodeIndex].Level;
    while ( (NodeIndex <= oTreeView.Items.Count-1) AND (Items[NodeIndex].data <> nil) AND
//      (BB >= AA) AND
// bug BB = 3 but oTreeView.Items[NodeIndex].Level = 0 ???
// ??? hierarchy problem - 8 Aug 2018 - temporary fix make sure Material is at same level or higher
// in case material is at level 0 for next frame/mesh
// need to go through treeView by parent/sibling instead of index to avoid this level problem
      (oTreeView.Items[NodeIndex].Level >= oTreeView.Items[NodeIndex-1].Level) AND
      (pObjectItem(Items[NodeIndex].data)^.oType in [oMaterial,oMaterialReference]) ) do begin
      case pObjectItem(Items[NodeIndex].data)^.oType of
        oMaterial: begin
          WriteMaterial(ObjectIndex);
        end;
        oMaterialReference: begin
          WriteMaterialReference(ObjectIndex);
        end;
      end;
    end;
  end;
end;

// look at texture name and if drevesa (or tr1, tr2), flag as no shadow
{----------------------------------------------------------------------------}
function Identify_NoShadow(var O : Object_C3D) : boolean;
var
  Tn : string;

begin
//  Tn := O.Name;
//  Tn := UpperCase(Copy(O.Name,Length(O.Name)-3+1,3));
  if (UpperCase(Copy(O.Name,Length(O.Name)-3+1,3)) = '_NS') then begin
    result := false;  // already flagged as '_ns'
  end else begin
//    Tn := O.TexturePath;
    Tn := ExtractFileName( StringReplace(O.TexturePath,'/','\',[rfReplaceAll]) );
    Tn := UpperCase(copy(Tn,1,length(Tn)-length(ExtractFileExt(Tn))));
    if ( (Tn = 'DREVESA') OR
         (Tn = 'TR1') OR
         (Tn = 'TR2')
       ) then begin
      result := true;  // flag as '_ns'
    end else begin
      result := false;
    end;
  end;
end;

{----------------------------------------------------------------------------}
Procedure WriteMesh;
var
  i, j   : integer;
  rArray : tFloatArray;

begin
    SetLength(rArray,3);
    i := Headers_C3D.Number_Objects;
    setlength(Objects_C3D, i+1);
    //walk the tree and write the data
    with pMesh(pObjectItem(oTreeView.Items[NodeIndex].data)^.oPointer)^ do begin // pMeshData
      // Do vertices
      Objects_C3D[i].Indexes.Vertex_Offset := OverallNumVertices;
      Objects_C3D[i].Indexes.NumVertices := tArray.aCount;
      setlength(Meshes_C3D, OverallNumVertices + Objects_C3D[i].Indexes.NumVertices);
      // Apply scale, rotation, translation if FTM Matrix present
      if (FTMnodeIndex <> -1) then begin
        with pFTM(pObjectItem(oTreeView.Items[FTMnodeIndex].data)^.oPointer)^ do begin // pFTM
          for j := 0 to Objects_C3D[i].Indexes.NumVertices-1 do begin
            ApplyFTMtoVectors(ftmArray,tArray.aArray[j],rArray);
            Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset + j].X := rArray[0];
            Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset + j].Y := rArray[1];
            Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset + j].Z := rArray[2];
          end;
        end;
      end else begin
        for j := 0 to Objects_C3D[i].Indexes.NumVertices-1 do begin
          Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset + j].X := tArray.aArray[j][0];
          Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset + j].Y := tArray.aArray[j][1];
          Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset + j].Z := tArray.aArray[j][2];
        end;
      end;
      // Do surfaces
      Objects_C3D[i].Indexes.Surface_Offset := OverallNumSurfaces * 3;
      Objects_C3D[i].Indexes.NumSurfaceVertices := sArray.aCount * 3;
      setLength(Surfaces_C3D, OverallNumSurfaces + Objects_C3D[i].Indexes.NumSurfaceVertices div 3);
      for j := 0 to (Objects_C3D[i].Indexes.NumSurfaceVertices div 3)-1 do begin
        Surfaces_C3D[Objects_C3D[i].Indexes.Surface_Offset div 3 + j].Vertex1 :=
          sArray.aArray[j].aArray[0] + Objects_C3D[i].Indexes.Vertex_Offset;
        Surfaces_C3D[Objects_C3D[i].Indexes.Surface_Offset div 3 + j].Vertex2 :=
          sArray.aArray[j].aArray[1] + Objects_C3D[i].Indexes.Vertex_Offset;
        Surfaces_C3D[Objects_C3D[i].Indexes.Surface_Offset div 3 + j].Vertex3 :=
          sArray.aArray[j].aArray[2] + Objects_C3D[i].Indexes.Vertex_Offset;
      end;

      Objects_C3D[i].name := tName;
    end;
    INC(NodeIndex);
    while ( (NodeIndex <= oTreeView.Items.Count-1) AND
            (oTreeView.Items[NodeIndex].data <> nil) AND
            (pObjectItem(oTreeView.Items[NodeIndex].data)^.oType in
              [oMeshNormals,oMeshTextureCoord,oMaterialList])
          ) do begin
      case pObjectItem(oTreeView.Items[NodeIndex].data)^.oType of
        oMeshTextureCoord: begin
          with pMeshTcoord(pObjectItem(oTreeView.Items[NodeIndex].data)^.oPointer)^ do begin // pMeshTcoordData
            // mesh texture coordinates
            //Objects_C3D[i].Indexes.NumVertices := tArray.aCount; // done already with mesh vertices
            for j := 0 to Objects_C3D[i].Indexes.NumVertices-1 do begin
              Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset + j].TextureX := tArray.aArray[j][0];
              Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset + j].TextureY := tArray.aArray[j][1];
            end;
            INC(NodeIndex);
          end;
        end;
        oMeshNormals: begin
          with pMeshNormals(pObjectItem(oTreeView.Items[NodeIndex].data)^.oPointer)^ do begin // pMeshNormalsData
            //Objects_C3D[i].Indexes.NumVertices := tArray.aCount; // done already with mesh vertices
            // Apply scale, rotation, translation if FTM Matrix present
            if (FTMnodeIndex <> -1) then begin
              with pFTM(pObjectItem(oTreeView.Items[FTMnodeIndex].data)^.oPointer)^ do begin // pFTM
                for j := 0 to Objects_C3D[i].Indexes.NumVertices-1 do begin
                  ApplyFTMtoNormals(ftmArray,tArray.aArray[j],rArray);
                  Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset + j].NormalX := rArray[0];
                  Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset + j].NormalY := rArray[1];
                  Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset + j].NormalZ := rArray[2];
                end;
              end;
            end else begin
              for j := 0 to Objects_C3D[i].Indexes.NumVertices-1 do begin
                Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset + j].NormalX := tArray.aArray[j][0];
                Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset + j].NormalY := tArray.aArray[j][1];
                Meshes_C3D[Objects_C3D[i].Indexes.Vertex_Offset + j].NormalZ := tArray.aArray[j][2];
              end;
            end;
          end;
          INC(NodeIndex);
        end;
        oMaterialList: begin
          WriteMaterialList(i);
       end;
      end;
    end;
    // if texture is drevesa (or tr1 or tr2) turn off shadow by using mesh suffx '_ns'
//    Objects_C3D[i].TexturePath
//      Tn := StringReplace(Tn,'/','\',[rfReplaceAll]);   //could be / or \
//      FileName := ExtractFileName(coName);
//      FileExt := ExtractFileExt(coName);
//      New_coName := copy(coName,1,length(coName)-length(FileExt)) +'.c3d';

    if (Identify_NoShadow(Objects_C3D[i])) then begin  // check for tree mesh
      Objects_C3D[i].name := Objects_C3D[i].name+'_ns';
    end;

    // now check for mesh merging
    if ( (i >= 1) AND {(Enable_Merge) AND}
         (Objects_C3D[i].name = Objects_C3D[i-1].name) AND
         (Objects_C3D[i].TexturePath = Objects_C3D[i-1].TexturePath)
       ) then begin
      // keep first Indexes, but increment counts
      INC(Objects_C3D[i-1].Indexes.NumVertices,Objects_C3D[i].Indexes.NumVertices);
      // keep first surface Indexes, but increment surface counts
      INC(Objects_C3D[i-1].Indexes.NumSurfaceVertices,Objects_C3D[i].Indexes.NumSurfaceVertices);
      // do not increment object count, and do not add object header
    end else begin
      // now increment object count
      INC(Headers_C3D.Number_Objects);
      INC(Headers_C3D.Number_TBD);
    end;
    OverallNumVertices := OverallNumVertices + Objects_C3D[i].Indexes.NumVertices;
    OverallNumSurfaces := OverallNumSurfaces + Objects_C3D[i].Indexes.NumSurfaceVertices div 3;
end;

{----------------------------------------------------------------------------}
Procedure WriteFrame;
var
  TreeLevel : integer;
begin
  with oTreeView do begin
    // keep track of level for recursive frames
    TreeLevel := Items[NodeIndex].Level;
    INC(NodeIndex);
    //walk the tree and write the data
    while ( (NodeIndex <= oTreeView.Items.Count-1) AND (Items[NodeIndex].data <> nil) AND
      (pObjectItem(Items[NodeIndex].data)^.oType in [oFrame,oMesh,oFTM]) ) do begin
      case pObjectItem(Items[NodeIndex].data)^.oType of
        oFrame: begin
          if (TreeLevel <> Items[NodeIndex].Level) then begin
            FTMnodeIndex := -1;
            WriteFrame;
          end else begin
            // same level - must be new frame - close this one
            break; // exit while
          end;
        end;
        oFTM: begin
          // WriteFTM; // NA
          FTMnodeIndex := NodeIndex;
          INC(NodeIndex);
        end;
        oMesh: begin
          WriteMesh;
        end;
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
begin
  Headers_C3D.a := 1;              // ??? could be rev ?
  Headers_C3D.b := 0;              // ??? could be rev ?
  Headers_C3D.Number_Objects := 0; // to be filled in                                                           3
  Headers_C3D.Number_TBD := 0;     // ??? same as Number_Objects for now
  // need object count
  // setlength(Objects_C3D, Headers_C3D.Number_Objects);       // create array of objects based on object count
  OverallNumVertices := 0;
  setlength(Meshes_C3D, OverallNumVertices);
  OverallNumSurfaces := 0;
  setlength(Surfaces_C3D, OverallNumSurfaces * 3);

  //walk the tree and write the data
  with oTreeView do begin
    NodeIndex := 0;
    while  (NodeIndex <= oTreeView.Items.Count-1) do begin
      if (oTreeView.Items[NodeIndex].data <> nil) then begin
        case pObjectItem(oTreeView.Items[NodeIndex].data)^.oType of
    //      owfOBJmagic: begin
    //        WriteWfOBJmagic;   // do separately
    //      end;
    //      oOBJ8magic: begin
    //        WriteOBJ8magic;   // do separately
    //      end;
    //      o3Dmagic: begin
    //        Write3Dmagic;   // do separately
    //      end;
    //      oMagic: begin
    //        WriteMagic;     // NA
    //      end;
    //      oOBJ8header: begin
    //        WriteOBJ8header;  // do separately
    //      end;
    //      o3Dheader: begin
    //        Write3Dheader;  // do separately
    //      end;
    //      oHeader: begin
    //        WriteHeader;    // NA
    //      end;
          oFrame: begin
            FTMnodeIndex := -1;
            WriteFrame;
            end;
          oFTM: begin
            // WriteFTM; // NA
            FTMnodeIndex := NodeIndex;
            INC(NodeIndex);
          end;
          oMesh: begin
            WriteMesh;
          end;
     //     oMaterial: begin
     //       WriteMaterial;
  // extract filename to eliminate the reference
     //     end;
          else begin // skip
            INC(NodeIndex);
          end;
        end;
      end else begin // skip
        INC(NodeIndex);
      end;
    end;
  end;
end;

{----------------------------------------------------------------------------}
procedure Append_C3D_Details(c3dName, FileName : string);
var
  CSV_File : TextFile;
  i : integer;
begin
  AssignFile(CSV_File,FileName);
  if (NOT FileExists(FileName)) then begin
    Rewrite(CSV_File);
  end else begin
    Append(CSV_File);
  end;
  writeln(CSV_File,format('File Name:,''%s''',[c3dName]));
  writeln(CSV_File,format('Count:,''%d''',[Headers_C3D.Number_Objects]));
  for i := 0 to Headers_C3D.Number_Objects-1 do begin
    writeln(CSV_File,format('Name:,''%s''',[Objects_C3D[i].name]));
    writeln(CSV_File,format('Path:,''%s''',[Objects_C3D[i].TexturePath]));
  end;
  CloseFile(CSV_File);
end;

//----------------------------------------------------------------------------
Procedure WriteCondorC3Dfile(FileName : string);
var
  i : integer;

begin
  // first create the C3D arrays
  CondorC3Dfile_CreateC3Dversion;

  AssignFile(C3D_File,FileName);
  ReWrite(C3D_File);

  try
    // write first 3 bytes
    blockWrite (C3D_file,C3D_Magic,length(C3D_Magic));
    begin
      with Headers_C3D do begin
        blockWrite (C3D_file,Headers_C3D,sizeof(Header_C3D));
//        setlength(Objects_C3D, Number_Objects); // create array of objects based on object count
//        OverallNumVertices := 0;
//        OverallNumSurfaces := 0;
        for i := 0 to Number_Objects-1 do begin
          blockWrite (C3D_file,Objects_C3D[i].name[0],1);  // write length of object name
          blockWrite (C3D_file,Objects_C3D[i].name[1],integer(Objects_C3D[i].name[0])); // write object name
          blockWrite (C3D_file,Objects_C3D[i].Indexes,sizeof(Indexes_C3D)); // write object mesh data
          blockWrite (C3D_file,Objects_C3D[i].TexturePath[0],1);  // write length of object name
          blockWrite (C3D_file,Objects_C3D[i].TexturePath[1],integer(Objects_C3D[i].TexturePath[0])); // write object nameg
          blockWrite(C3D_file,Objects_C3D[i].Lighting,sizeof(Lighting_C3D)); // write object data
//          OverallNumVertices := OverallNumVertices + Objects_C3D[i].Indexes.NumVertices;
//          OverallNumSurfaces := OverallNumSurfaces + Objects_C3D[i].Indexes.NumSurfaceVertices div 3;  // triangles, 3 vertices per surface
        end;
      end;
      // write size of mesh
      blockWrite (C3D_file,OverallNumVertices,sizeof(OverallNumVertices));
      // create Vertex array;
//      setlength(Meshes_C3D, OverallNumVertices); // create array of objects based on object count
      blockWrite (C3D_file,Meshes_C3D[0],OverallNumVertices*sizeof(Mesh_C3D)); // get object mesh data

      // write size of surfaces
      OverallNumSurfaceVertices_C3D := OverallNumSurfaces * 3;
      blockWrite (C3D_file,OverallNumSurfaceVertices_C3D,sizeof(OverallNumSurfaceVertices_C3D));
      // create Surface array;
//      setlength(Surfaces_C3D, OverallNumSurfaces); // create array of objects based on object count
      blockWrite (C3D_file,Surfaces_C3D[0],OverallNumSurfaces*sizeof(Surface_C3D)); // get object surface data
    end;
  finally
   closefile(C3D_File);
  end;

end;

// special version if data is pointed to instead of copied (option 2)
{----------------------------------------------------------------------------}
Procedure C3D_PlotIt(count : integer; aData : pointer);
var
  i: integer;
//  ErrorCode :integer;
  iX, iY : integer;

begin
      with tvBitmap do begin
        Canvas.Pen.Style := psSolid;
        Canvas.Pen.Mode := pmCopy;
        Canvas.Pen.Color := clRed;
        Canvas.Brush.Color := clWhite;
        Canvas.FillRect(rect(0,0,Width,Height)); //erase first
        iX := round((aMesh_C3D(aData^)[0].TextureX)*255);
        iY := round((aMesh_C3D(aData^)[1].TextureY)*255);
        Canvas.MoveTo(iX,iY);
        for i := 1 to Count-1 do begin
     //     with aData^ do begin
            iX := round((aMesh_C3D(aData^)[i].TextureX)*255);
            iY := round((aMesh_C3D(aData^)[i].TextureY)*255);
            Canvas.LineTo(iX,iY);
            if (i >= StrToInt(Unit_Objects.Form_Objects.Edit_Steps.text)) then begin
              break;
            end;
     //     end;
        end;
      end;

end;

{----------------------------------------------------------------------------}
Procedure WriteCondorObjectFile(FileName : string; ForViewer : Viewer_Type; TC_InvertY : boolean);
begin
  if (uppercase(ExtractFileExt(FileName)) = '.C3D') then begin
    WriteCondorC3Dfile(FileName);
  end else begin // must be X file
    WriteCondorXfile(FileName, ForViewer, TC_InvertY);
  end;
end;

//-------------------------------------------------------------------------------------
function FindNextTexture(var NodeIndex : integer) : string;
begin
  result := '';
    //walk the tree
    with oTreeView do begin
//      NodeIndex := 0;
      while  (NodeIndex <= oTreeView.Items.Count-1) do begin
        if (oTreeView.Items[NodeIndex].data <> nil) then begin
          case pObjectItem(oTreeView.Items[NodeIndex].data)^.oType of
            owfOBJmagic: begin
//              WriteWfOBJmagic;
  INC(NodeIndex);
            end;
            oOBJ8magic: begin
//              WriteOBJ8magic;
  INC(NodeIndex);
            end;
            o3Dmagic: begin
//              Write3Dmagic;
  INC(NodeIndex);
            end;
            oMagic: begin
//              WriteMagic;
  INC(NodeIndex);
            end;
       //     oOBJ8header: begin
       //       WriteOBJ8eader;  // not needed
       //     end;
       //     o3Dheader: begin
       //       Write3Dheader;  // not needed
       //     end;
            oHeader: begin
//              WriteHeader;
  INC(NodeIndex);
            end;
            oFrame: begin
//              WriteFrame;
  INC(NodeIndex);
              end;
            oMesh: begin
//              WriteMesh;
  INC(NodeIndex);
            end;
            oMaterial: begin
//              WriteMaterial(mNormal);
    INC(NodeIndex);
    while ( (NodeIndex <= oTreeView.Items.Count-1) AND (Items[NodeIndex].data <> nil) AND
      (pObjectItem(Items[NodeIndex].data)^.oType in [oFilename,oMaterialReference]) ) do begin
      case pObjectItem(Items[NodeIndex].data)^.oType of
        oFileName: begin
          result := pFileName(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tqName;
//          break; // exit loop without inc NodeIndex
    exit;  // exit loop without inc NodeIndex
        end;
      end;
      INC(NodeIndex);
    end;
            end;
            else begin // skip
              INC(NodeIndex);
            end;
          end;
        end else begin // skip
          INC(NodeIndex);
        end;
      end;
    end;
end;

//-------------------------------------------------------------------------------------
procedure UpdateTextureFileName(NodeIndex : integer; TextureFileName : string);
begin
  with oTreeView do begin
    pFileName(pObjectItem(Items[NodeIndex].data)^.oPointer)^.tqName := TextureFileName;
  end;
end;

//-------------------------------------------------------------------------------------
procedure CopyObjectTextures (NewLandscapePath,NewLandscapeName,
                              OldLandscapePath,OldLandscapeName,
                              CurrentPath : string);
var
  NodeIndex : integer;
  Tn : string;
  Index : integer;
  Folder : string;
  Remainder : string;

//-------------------------------------------------------------------------------------
function GetFirstFolder(Tn : string) : integer;
begin
  Index := pos('\',Tn);
  if (Index <> 0) then begin
    Folder := copy(Tn,1,Index-1);
    Remainder := copy(Tn,Index+1,length(TN));
  end;
  result := Index;
end;

//-------------------------------------------------------------------------------------
function GetFirstInstance(Instance, Tn : string) : integer;
begin
  Index := pos(Instance,Tn);
  if (Index <> 0) then begin
    Folder := copy(Tn,1,Index-1);
    Remainder := copy(Tn,Index+length(Instance)+1,length(TN));
  end;
  result := Index;
end;

//-------------------------------------------------------------------------------------
Procedure CopyIt;
begin
  // create folder
  ForceDirectories(NewLandscapePath+'\'+CurrentPath+'\'+ExtractFilePath(Tn));
  // copy file
  CopyFile(pchar(OldLandscapePath+'\'+CurrentPath+'\'+Tn),
    pchar(NewLandscapePath+'\'+CurrentPath+'\'+Tn),false);
end;

//-------------------------------------------------------------------------------------
begin
  // walk the object tree and fix file paths and copy textures
  NodeIndex := 0;
  repeat
    Tn := FindNextTexture(NodeIndex);
    if (Tn <> '') then begin
//      MessageShow(Tn);
      Tn := StringReplace(Tn,'/','\',[rfReplaceAll]);   //could be / or \
      // convert to all lower case to be able to find and compare
      Tn := StringReplace(Tn,'Landscapes','landscapes',[rfReplaceAll, rfIgnoreCase]);
      Tn := StringReplace(Tn,'World','world',[rfReplaceAll, rfIgnoreCase]);
      Tn := StringReplace(Tn,'Airports','airports',[rfReplaceAll, rfIgnoreCase]);
      Tn := StringReplace(Tn,'Objects','objects',[rfReplaceAll, rfIgnoreCase]);
      if (GetFirstFolder(Tn) = 0) then begin
        CopyIt; // in same folder as object
      end else begin
        if (Folder = 'world') then begin
         // !!! Condor V2 does not try local landscape relative 'World' folder first like Condor V1
         // and goes straight to Condor2\World
         // check landscape relative world folder anyway and
         // if texture found, copy it into new local world folder
         // else do not copy as it must be in Condor2\World !
         if (FileExists(OldLandscapePath+'\'+Tn)) then begin
           CurrentPath := '.'; // make local
           CopyIt; // in landscape relative local folder
         end;
        end else begin
          if (Folder = 'landscapes') then begin
            Index := GetFirstInstance('world',Tn);
            if (Index <> 0) then begin
              if (CurrentPath = 'World\Objects') then begin
                Tn := '..\'+remainder;
              end else begin
                Tn := '..\World\'+remainder;
              end;
              CopyIt;
            end else begin
              Index := GetFirstInstance('airports',Tn);
              if (Index <> 0) then begin
                if (CurrentPath = 'Airports') then begin
                  Tn := remainder;
                end else begin
                  Tn := '..\Airports\'+remainder;
                end;
                CopyIt;
              end else begin
              end;
            end;
          end else begin // not WORLD, not LANDSCAPES
            // relative to current object
            CopyIt;
          end;
        end;
        // update the name in the file
        UpdateTextureFileName(NodeIndex, Tn);
      end;
      // look for the next one
      INC(NodeIndex);
    end;
//  until (Tn = '');  // bug ! need to continue until end
  until (NodeIndex > oTreeView.Items.Count-1)
end;

{----------------------------------------------------------------------------
Xplane .OBJ OBJ8 object coordinate encoding/decoding
----------------------------------------------------------------------------}

{----------------------------------------------------------------------------
OBJ8 file structure
  Header_OBJ8_Type = record
    CRLF_Type : string;  // 'I' -> CRLF or LF, or 'A' -> CR;
    Version   : string;  // '800' -> OBJ 8
    oType     : string;  // 'OBJ'
  end;

  TextureFileName_Type = record:
//    tID       : string;  //'TEXTURE'
	tFileName : string;
  end;

  Counts_Type = record
//    cID  : string;     // 'POINT_COUNTS'
    tris    : integer; // vertices
    lines   : integer; // lines
    lites   : integer; // lights
    indices : integer; // indexes to surface vertices
  end;

  Vertex_Array_Type = record
//    vID : string;  // 'VT'
	x   : double;  // X coordinate
	y   : double;  // Y coordinate
	z   : double;  // Z coordinate
	nx  : double;  // normal X vector
	ny  : double;  // normal Y vector
	nz  : double;  // normal Z vector
	s   : double;  // X texture coordinate
	t   : double;  // Y texture coordinate
  end;

  Vertex_List_Type = array of Vertex_array;  // count -> tris

  singleIndex_Type = record
//    siID    : string;   // 'IDX'
	siIndex : integer;  // single index
  end;

  multipleIndex_Type = record
//    miID    : string;               // 'IDX10'
	miIndex : array[10] of integer; // group of 10 indexes
  end;

  Surface_Vertex_List_Type = array of integer;

  Surface_Control_Type = record
//    scID   : string;  // 'TRIS'
 	offset : integer; // offset into Surface_Vertex_List
	count  : integer; // vertex count multiple of 3
  end;

----------------------------------------------------------------------------}

const
//  OBJ8_Magic = 'OBJ'; // dummy value
  OBJ8_Magic = 'OBJ8'; // dummy value

type
  Header_OBJ8_Type = record
    CRLF_Type : string;  // 'I' -> CRLF or LF, or 'A' -> CR;
    Version   : string;  // '800' -> OBJ 8
    oType     : string;  // 'OBJ'
  end;

  TextureFileName_Type = record
    tFileName : string;
  end;

  Counts_Type = record
    tris    : integer; // vertices
    lines   : integer; // lines
    lites   : integer; // lights
    indices : integer; // indexes to surface vertices
  end;

  Vertex_Array_Type = record
    x   : double;  // X coordinate
    y   : double;  // Y coordinate
    z   : double;  // Z coordinate
    nx  : double;  // normal X vector
    ny  : double;  // normal Y vector
    nz  : double;  // normal Z vector
    s   : double;  // X texture coordinate
    t   : double;  // Y texture coordinate
  end;

  Vertex_List_Type = array of Vertex_Array_Type;  // count -> tris

  Surface_Vertex_List_Type = array of integer;

  Surface_Control_Type = record
    offset : integer; // offset into Surface_Vertex_List
    count  : integer; // vertex count multiple of 3
  end;

//  pOBJ8header = ^tOBJ8header;
  pOBJ8header = ^t3Dheader; // for now
  tOBJ8header = record
//    s3Darray : tLongArray;
  end;

  pOBJ8magic = ^tOBJ8magic;
  tOBJ8magic = record
//    Rev_Major : longword;
//    Rev_Minor : longword;
  end;

var
//  pOBJ8magicData : pOBJ8magic;
//  pOBJ8headerData : pOBJ8header;

  Header_OBJ8 : Header_OBJ8_Type;
  TextureFileName : TextureFileName_Type;
  Counts : Counts_Type;
  Vertex_List : Vertex_List_Type;
  vtCount : integer;
  Surface_Vertex_List : Surface_Vertex_List_Type;
  svCount : integer;
  Surface_Control : array of Surface_Control_Type;
  scCount : integer;

//----------------------------------------------------------------------------
procedure XplaneOBJ8file_CreateTreeViewVersion;
var
  {i,} j, k : integer;
  prevCount : integer;
  fTreeNode, fTreeNode2 : TTreeNode;

begin
  ClearTreeView(oTreeView);

  New(pObjectData); //allocate space for an object data
  pObjectData^.oType := oOBJ8magic;
  New(p3DmagicData); //allocate space for data
  pObjectData^.oPointer := p3DmagicData;
  oTreeView.Items.AddChildObject(nil, OBJ8_Magic, pObjectData);

  New(pObjectData); //allocate space for an object data
  pObjectData^.oType := oOBJ8header;
  New(p3DheaderData); //allocate space for data
  pObjectData^.oPointer := p3DheaderData;
  oTreeView.Items.AddChildObject(nil, 'Header', pObjectData);

//  i := 0; // only one object
  begin
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oFrame;
    New(pFrameData); //allocate space for a frame data
    pObjectData^.oPointer := pFrameData;
    pFrameData^.tName := 'f_Object_1';
    fTreeNode := oTreeView.Items.AddChildObject(nil, 'Frame '+pFrameData^.tName, pObjectData);

    // mesh
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oMesh;
    New(pMeshData); //allocate space for data
    pMeshData^.tName := 'm_Object_1';
    pMeshData^.tRotation := 0.0;
    pMeshData^.tX := 0.0;
    pMeshData^.tY := 0.0;
    pMeshData^.tHeight := 0.0;
    pMeshData^.tScale := 1.0;
    // option 1 - copy the data into existing structure - routines on structure still work
    // Do vertices
    pMeshData^.tArray.aCount := Counts.tris;
    setLength(pMeshData^.tArray.aArray,pMeshData^.tArray.aCount);
    for j := 0 to pMeshData^.tArray.aCount-1 do begin
      setLength(pMeshData^.tArray.aArray[j],3);
      // reverse Z, -X, Y
      pMeshData^.tArray.aArray[j][0] :=  Vertex_List[j].z;
      pMeshData^.tArray.aArray[j][1] := -Vertex_List[j].x;
      pMeshData^.tArray.aArray[j][2] :=  Vertex_List[j].y;
    end;
    CalcExtents(@pMeshData^.tArray,@pMeshData^);
    // Do surfaces
    pMeshData^.sArray.aCount := 0;
    for k := 0 to scCount-1 do begin
      prevCount := pMeshData^.sArray.aCount;
      pMeshData^.sArray.aCount := pMeshData^.sArray.aCount + Surface_Control[k].count div 3;
      setLength(pMeshData^.sArray.aArray,pMeshData^.sArray.aCount);
//      for j := (Surface_Control[k].offset div 3) to pMeshData^.sArray.aCount-1 do begin
//      for j := prevCount to pMeshData^.sArray.aCount-1 do begin
      for j := 0 to (Surface_Control[k].count div 3)-1 do begin
        pMeshData^.sArray.aArray[j+prevCount].aCount := 3;
        setLength(pMeshData^.sArray.aArray[j+prevCount].aArray,pMeshData^.sArray.aArray[j+prevCount].aCount);
        // change order
        pMeshData^.sArray.aArray[j+prevCount].aArray[0] := Surface_Vertex_List[Surface_Control[k].offset+j*3+0];
        pMeshData^.sArray.aArray[j+prevCount].aArray[1] := Surface_Vertex_List[Surface_Control[k].offset+j*3+1];
        pMeshData^.sArray.aArray[j+prevCount].aArray[2] := Surface_Vertex_List[Surface_Control[k].offset+j*3+2];
      end;
    end;
    pObjectData^.oPointer := pMeshData;
//    fTreeNode2 := oTreeView.Items.AddChildObject(fTreeNode, 'Mesh', pObjectData);
    fTreeNode2 := oTreeView.Items.AddChildObject(fTreeNode, 'Mesh '+pMeshData^.tName, pObjectData);

    // mesh normals
    // Do normals
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oMeshNormals;
    New(pMeshNormalsData); //allocate space for data
    pMeshNormalsData^.tName := 'mn_Object_1';
    pMeshNormalsData^.tArray.aCount := Counts.tris;
    setLength(pMeshNormalsData^.tArray.aArray,pMeshNormalsData^.tArray.aCount);
    for j := 0 to pMeshNormalsData^.tArray.aCount-1 do begin
      setLength(pMeshNormalsData^.tArray.aArray[j],3);
      // reverse Z, -X, Y
      pMeshNormalsData^.tArray.aArray[j][0] :=  Vertex_List[j].nz;
      pMeshNormalsData^.tArray.aArray[j][1] := -Vertex_List[j].nx;
      pMeshNormalsData^.tArray.aArray[j][2] :=  Vertex_List[j].ny;
    end;
    // Do surfaces
    pMeshNormalsData^.sArray.aCount := 0;
    for k := 0 to scCount-1 do begin
      prevCount := pMeshNormalsData^.sArray.aCount;
      pMeshNormalsData^.sArray.aCount := pMeshNormalsData^.sArray.aCount + Surface_Control[k].count div 3;
      setLength(pMeshNormalsData^.sArray.aArray,pMeshNormalsData^.sArray.aCount);
//      for j := (Surface_Control[k].offset div 3) to pMeshNormalsData^.sArray.aCount-1 do begin
//      for j := prevCount to pMeshNormalsData^.sArray.aCount-1 do begin
      for j := 0 to (Surface_Control[k].count div 3)-1 do begin
        pMeshNormalsData^.sArray.aArray[j+prevCount].aCount := 3;
        setLength(pMeshNormalsData^.sArray.aArray[j+prevCount].aArray,pMeshNormalsData^.sArray.aArray[j+prevCount].aCount);
        // change order
        pMeshNormalsData^.sArray.aArray[j+prevCount].aArray[0] := Surface_Vertex_List[Surface_Control[k].offset+j*3+0];
        pMeshNormalsData^.sArray.aArray[j+prevCount].aArray[1] := Surface_Vertex_List[Surface_Control[k].offset+j*3+1];
        pMeshNormalsData^.sArray.aArray[j+prevCount].aArray[2] := Surface_Vertex_List[Surface_Control[k].offset+j*3+2];
      end;
    end;
    pObjectData^.oPointer := pMeshNormalsData;
    oTreeView.Items.AddChildObject(fTreeNode2, 'MeshNormals', pObjectData);

    // mesh texture coordinates
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oMeshTextureCoord;
    New(pMeshTcoordData); //allocate space for data
    pMeshTcoordData^.tName := 'tc_Object_1';
    pMeshTcoordData^.tArray.aCount := Counts.tris;
    setLength(pMeshTcoordData^.tArray.aArray,pMeshTcoordData^.tArray.aCount);
    for j := 0 to pMeshTcoordData^.tArray.aCount-1 do begin
      setLength(pMeshTcoordData^.tArray.aArray[j],3);
      // reverse/flip Y
      pMeshTcoordData^.tArray.aArray[j][0] :=       Vertex_List[j].s;
      pMeshTcoordData^.tArray.aArray[j][1] := 1.0 - Vertex_List[j].t;
    end;
    pMeshTcoordData^.sIndex := pMeshData;
    pObjectData^.oPointer := pMeshTcoordData;
//    oTreeView.Items.AddChildObject(fTreeNode, 'MeshTextureCoords', pObjectData);
    oTreeView.Items.AddChildObject(fTreeNode2, 'MeshTextureCoords', pObjectData);

    // add dummy material List
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oMaterialList;
    New(pMaterialListData); //allocate space for data
    pMaterialListData^.tName := '';
    pMaterialListData^.tCount := 1;
    pMaterialListData^.tsCount := 1;
    SetLength(pMaterialListData^.sArray,pMaterialListData^.tsCount);
    pMaterialListData^.sArray[0] := 0;
    pObjectData^.oPointer := pMaterialListData;
    fTreeNode2 := oTreeView.Items.AddChildObject(fTreeNode2, 'MaterialList',pObjectData);

    // material
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oMaterial;
    New(pMaterialData); //allocate space for data
    //p3DmaterialData^.tName := 'mat_'+Objects_C3D[i].name;
    setLength(pMaterialData^.tRGBA,4);
    pMaterialData^.tRGBA[0] := 1.0;
    pMaterialData^.tRGBA[1] := 1.0;
    pMaterialData^.tRGBA[2] := 1.0;
    pMaterialData^.tRGBA[3] := 1.0;
    pObjectData^.oPointer := pMaterialData;
    // dummy specular (?) data
    pMaterialData^.tPower := 1.0;
    setLength(pMaterialData^.tRGBs,3);
    pMaterialData^.tRGBs[0] := 0.0; // specular
    pMaterialData^.tRGBs[1] := 0.0;
    pMaterialData^.tRGBs[2] := 0.0;
    setLength(pMaterialData^.tRGBr,3);
    pMaterialData^.tRGBr[0] := 0.0; // reflective, shiny
    pMaterialData^.tRGBr[1] := 0.0;
    pMaterialData^.tRGBr[2] := 0.0;
//    fTreeNode2 := oTreeView.Items.AddChildObject(fTreeNode, 'Material', pObjectData);
    fTreeNode2 := oTreeView.Items.AddChildObject(fTreeNode2, 'Material', pObjectData);

    // filename
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oFileName;
    New(pFileNameData); //allocate space for data
    pFileNameData^.tName := '';
    pFileNameData^.tqName := TextureFileName.tFileName;
    pObjectData^.oPointer := pFileNameData;
    oTreeView.Items.AddChildObject(fTreeNode2, 'FileName', pObjectData);
//    oTreeView.Items.AddChildObject(fTreeNode, 'FileName', pObjectData);
  end;

end;

//----------------------------------------------------------------------------
function readXplaneOBJ8file(FileName : string) : boolean;
var
  i : integer;
//  File_Error : boolean;
  Temp_File : TextFile;
//  Temp_File : File_Generic_Text;
//  Temp_File : File; alternate untyped file IO
  TempSTR : string;
  pString : string;
//  ch : char;
//  TextBuffer: array[1..1024] of Char;  { 1K buffer }

// - - - - - - - - - - - - - - - - - - - - - - -
{
Procedure xReadLine(var buffer : string);
var
  Done : boolean;
begin
  Done := false;
  buffer := '';
  while ((NOT Eof(Temp_File)) AND NOT Done) do begin // look for linefeed
    Read(Temp_File, Ch);
    if (Ch = char($0A)) then begin  // LF
      Exit; // all done
    end else begin
      if (Ch = char($0D)) then begin  // CR
        if (Eof(Temp_File)) then begin
          Exit; // all done
        end else begin
          Read(Temp_File, Ch);
          if (Ch = char($0A)) then begin  // LF
            // put it back ???
          end;
        end;
      end;
    end;
    Buffer := Buffer + Ch;
  end;
end;
}
// - - - - - - - - - - - - - - - - - - - - - - -
begin
  Result := false;  // assume fault
  vtCount := 0;
  svCount := 0;
  scCount := 0;

  AssignFile(Temp_file, FileName);
  Reset(Temp_file);
//  SetTextBuf(Temp_file, TextBuffer);
//  // alternate untyped file IO
//  Reset(Temp_file, 1); // blocks of 1 byte at a time !
//  xReadLineReset;

  // read header
  with Header_OBJ8 do begin
//    Readln(Temp_file,CRLF_Type);
    Readline(@Temp_file,CRLF_Type);
//    xReadline(@Temp_file,CRLF_Type);
//    GT_ReadLine(@Temp_file,CRLF_Type);
    if (NOT ((UpperCase(CRLF_type) = 'I') or (UpperCase(CRLF_type) = 'A'))) then begin
//      ShowMessage('File type not recognized');
      beep; exit;
    end;
//    Readln(Temp_file,Version);
    Readline(@Temp_file,Version);
//    xReadline(@Temp_file,Version);
//    GT_ReadLine(@Temp_file,Version);
//    Readln(Temp_file,oType);
    Readline(@Temp_file,oType);
//    xReadline(@Temp_file,oType);
//    GT_ReadLine(@Temp_file,oType);
    if (NOT (UpperCase(oType) = 'OBJ')) then begin
//      ShowMessage('File type not recognized');
      CloseFile(Temp_file);
      beep; exit;
    end;
  end;

  try
while NOT EOF(Temp_file) do begin  // ??? eof problem with xreadline !!!
//  Readln(Temp_file,TempSTR);
  Readline(@Temp_file,TempSTR);
//  xReadline(@Temp_file,TempSTR);
//    GT_ReadLine(@Temp_file,TempSTR);
  ParseText(TempSTR,pString);
  if (UpperCase(pString) = 'TEXTURE') then begin
    with TextureFileName do begin
      ParseText(TempSTR,tFileName);
      Continue;
    end;
  end;
  if (UpperCase(pString) = 'POINT_COUNTS') then begin
    with Counts do begin
      parseInteger(Tempstr,tris);
      parseInteger(Tempstr,lines);
      parseInteger(Tempstr,lites);
      parseInteger(Tempstr,indices);
      Continue;
    end;
  end;
  if (UpperCase(pString) = 'VT') then begin
    SetLength(Vertex_List,vtCount+1);
    with Vertex_List[vtCount] do begin
      parseFloat(Tempstr,x);
      parseFloat(Tempstr,y);
      parseFloat(Tempstr,z);
      parseFloat(Tempstr,nx);
      parseFloat(Tempstr,ny);
      parseFloat(Tempstr,nz);
      parseFloat(Tempstr,s);
      parseFloat(Tempstr,t);
      INC(vtCount);
      Continue;
    end;
  end;
  if (UpperCase(pString) = 'IDX10') then begin
    SetLength(Surface_Vertex_List,svCount+10);
    for i := 0 to 10-1 do begin
      parseInteger(Tempstr,Surface_Vertex_List[svCount+i]);
    end;
    INC(svCount,10);
//    // preset in case 'TRIS' not in file
//    Surface_Control.offset := 0;
//    Surface_Control.count := svCount;
    Continue;
  end;
  if (UpperCase(pString) = 'IDX') then begin
    SetLength(Surface_Vertex_List,svCount+1);
    parseInteger(Tempstr,Surface_Vertex_List[svCount]);
    INC(svCount);
//    Surface_Control.offset := 0;
//    Surface_Control.count := svCount;
    Continue;
  end;
  if (UpperCase(pString) = 'TRIS') then begin
    SetLength(Surface_Control,scCount+1);
    with Surface_Control[scCount] do begin
      parseInteger(Tempstr,offset);
      parseInteger(Tempstr,count);
      INC(scCount);
      Continue;
    end;
  end;
end;

  except
//    ShowMessage('File error - parsing');
    CloseFile(Temp_file);
    beep; exit;
  end;

  // all done reading file
  CloseFile(Temp_file);

  // validate
  if (vtCount <> Counts.tris) then begin
//    ShowMessage('File error - vertex count');
    beep; exit;
  end;
  if (svCount <> Counts.indices) then begin
//    ShowMessage('File error - index count');
    beep; exit;
  end;
  for i := 0 to scCount-1 do begin
    if ((Surface_Control[i].count MOD 3) <> 0) then begin
//      ShowMessage('File error - vertex count (x3)');
      beep; exit;
    end;
    if ((Surface_Control[i].offset + Surface_Control[i].count) > svCount) then begin
//      ShowMessage('File error - vertex count mismatch');
      beep; exit;
    end;
  end;
  // if get here, no errors
  Result := true;

  if (Result) then begin
    XplaneOBJ8file_CreateTreeViewVersion;
  end;
end;

//----------------------------------------------------------------------------
function GetXplaneOBJ8texture : string;
begin
  Result := TextureFilename.tFileName;
end;

//----------------------------------------------------------------------------
procedure AdjustXplaneOBJ8texture(FileName : string);
begin
  TextureFilename.tFileName := FileName;
  XplaneOBJ8file_CreateTreeViewVersion;
end;

// also include Wavefront object type
{$I u_WaveFront_Obj.pas}

{----------------------------------------------------------------------------}
begin { Initialization }
end.


{--- End of File ------------------------------------------------------------}


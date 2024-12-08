{
 * Unit_Objects.pas
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
unit Unit_Objects;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  TForm_Objects = class(TForm)
    Button_Open: TButton;
    Button_Show3D: TButton;
    Button_Exit: TButton;
    Button_SaveAs: TButton;
    Image_TextureVertices: TImage;
    Image_Texture: TImage;
    Image_Mesh: TImage;
    TreeView_Object: TTreeView;
    Label_Name: TLabel;
    Label_FileName: TLabel;
    Edit_steps: TEdit;
    Button_Centre: TButton;
    Button_Rotate: TButton;
    Button_Translate: TButton;
    Edit_Degrees: TEdit;
    Edit_Height: TEdit;
    Edit_X: TEdit;
    Edit_Y: TEdit;
    Label1: TLabel;
    CheckBox_Zmin: TCheckBox;
    CheckBox_zAbsolute: TCheckBox;
    OpenDialog_FileName: TOpenDialog;
    SaveDialog_FileName: TSaveDialog;
    GroupBox_MaterialColor: TGroupBox;
    Edit_Red: TEdit;
    Edit_Green: TEdit;
    Edit_Blue: TEdit;
    Edit_Alpha: TEdit;
    Edit_Spec: TEdit;
    Edit_Shiny: TEdit;
    Edit_Env: TEdit;
    Label_Red: TLabel;
    Label_Green: TLabel;
    Label_Blue: TLabel;
    Label_Alpha: TLabel;
    Label_Spec: TLabel;
    Label_Shiny: TLabel;
    Label_Env: TLabel;
    procedure Button_ExitClick(Sender: TObject);
    procedure Button_OpenClick(Sender: TObject);
    procedure TreeView_ObjectMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure Button_Show3DClick(Sender: TObject);
    procedure TreeView_ObjectChange(Sender: TObject; Node: TTreeNode);
    procedure Button_SaveAsClick(Sender: TObject);
    procedure Button_CentreClick(Sender: TObject);
    procedure Button_RotateClick(Sender: TObject);
    procedure Button_TranslateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form_Objects: TForm_Objects;

var
  ApplicationPath : string;
  Memo_Message : TMemo;  // external TMemo for messages
  CondorFolder : string; // external path for Condor program folder
  WorkingFolder : string;
  ObjectFolder : string;
  oFolder : string;
  SelectedMeshNode: TTreeNode;

//----------------------------------------------------------------------------
implementation

uses FileCtrl,
  u_X_CX,
  u_CalibImport, u_CalibExport, // for dialogs
  u_Exec,
  pngimage,
  u_BMP, JPEG, TGA, DDS;

{$R *.DFM}

//---------------------------------------------------------------------------
procedure TForm_Objects.Button_ExitClick(Sender: TObject);
begin
  Close;
end;

//---------------------------------------------------------------------------
procedure TForm_Objects.Button_Show3DClick(Sender: TObject);
const
  TempFileName = 'TEMP.X';
begin
  u_X_CX.Path := CondorFolder;
  // only one mesh for 'sample.exe' program
  // need to invert Y (texture coordinates) for 'sample.exe' program
//  WriteCondorXfile(WorkingFolder+'\Temp\Temp.px',vSample,true);
//  WinExecAndWait32('Sample "'+WorkingFolder+'\Temp\temp.px"', 0);

////  WriteCondorXfile(WorkingFolder+'\Temp\Temp.x',true,false);
//  WriteCondorXfile(WorkingFolder+'\Temp\Temp.x',false,false);
//  // ObjectViewer needs weird combination of / and \ to display textures properly.
//  WinExecAndWait32(CondorFolder+'/Tools/ObjectViewer "'+WorkingFolder+'\Temp\temp.x"', 0);

  // write Temp object in original object folder so texture relative path will work (if relative)
//  WriteCondorXfile(oFolder+'\'+TempFileName,V1_ObjectViewer,false);
  // use ObjectFolder in case object already open
  WriteCondorXfile(ObjectFolder+'\'+TempFileName,V1_ObjectViewer,false);
  // Condor trick to find texture path - path needs \ up to where texture filename starts in object and
  // then / after that.
  // example: objectviewer "c:\program files\condor\world/objects/boc.x" 0
////  WinExecAndWait32(CondorFolder+'/Tools/Viewer/ObjectViewer "'+oFolder+'./'+TempFileName+'"', INFINITE);
////  WinExecAndWait32(CondorFolder+'/Tools/Viewer/ObjectViewer "'+oFolder+TempFileName+'"', INFINITE);
//  WinExecAndWait32(ApplicationPathName+'/Viewer/ObjectViewer "'+oFolder+TempFileName+'"', INFINITE);
  // problem - WinExec bug - does not wait, so file cannot be deleted
  //DeleteFile(oFolder+TempFileName);
  // WinExecAndWait32(ApplicationPathName+'/Viewer/ObjectViewer "'+oFolder+'\'+TempFileName+'"', INFINITE);
//  Shell_Execute(ApplicationPathName+'/Viewer', 'ObjectViewer', oFolder+'\'+TempFileName, false);
  // use ObjectFolder in case object already open
  // need "" in case there are spaces in folder or filenames
  Shell_Execute(ApplicationPath+'/Viewer', 'ObjectViewer', '"'+ObjectFolder+'\'+TempFileName+'"', false);
//  DeleteFile(oFolder+TempFileName);
  DeleteFile(ObjectFolder+'\'+TempFileName);
end;

//---------------------------------------------------------------------------
procedure TForm_Objects.Button_CentreClick(Sender: TObject);
begin
  with pObjectItem(SelectedMeshNode.data)^ do begin
    // save centre to translate back if desired
    Edit_X.text := format('%e',[pMesh(oPointer)^.tMinMaxArray[4]]);
    Edit_Y.text := format('%e',[pMesh(oPointer)^.tMinMaxArray[5]]);
    if (CheckBox_Zmin.checked) then begin
      Edit_Height.text := format('%e',[pMesh(oPointer)^.tMinMaxArray[7]]);
    end else begin
      Edit_Height.text := '0';
    end;

    NormalizeMesh(@pMesh(oPointer)^.tArray,@pMesh(oPointer)^,CheckBox_Zmin.checked);
    CalcExtents(@pMesh(oPointer)^.tArray,@pMesh(oPointer)^);
  end;
end;

//rotate object on its centre and keep offset or rotate around offset ???
// display rotated object but don't rotate original ???
//---------------------------------------------------------------------------
procedure TForm_Objects.Button_RotateClick(Sender: TObject);
begin
  with pObjectItem(SelectedMeshNode.data)^ do begin
    pMesh(oPointer)^.tRotation := StrToFloat(Edit_Degrees.text);
//    RotateMesh(@pMesh(oPointer)^);
    RotateMesh(SelectedMeshNode);
//    ScaleMesh(SelectedMeshNode);
    CalcExtents(@pMesh(oPointer)^.tArray,@pMesh(oPointer)^);
//    mPlotIt(@pMesh(oPointer)^.tArray,@pMesh(oPointer)^);
    msPlotIt(@pMesh(oPointer)^);
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Objects.Button_TranslateClick(Sender: TObject);
begin
  with pObjectItem(SelectedMeshNode.data)^ do begin
    pMesh(oPointer)^.tX := StrToFloat(Edit_X.text);
    pMesh(oPointer)^.tY := StrToFloat(Edit_Y.text);
    pMesh(oPointer)^.tHeight := StrToFloat(Edit_Height.text);
//    OffsetHeight(@pMesh(oPointer)^.tArray,@pMesh(oPointer)^);
//    OffsetHeight(SelectedMeshNode);
    TranslateMesh(SelectedMeshNode,CheckBox_zAbsolute.checked);
    CalcExtents(@pMesh(oPointer)^.tArray,@pMesh(oPointer)^);
  end;
end;

//---------------------------------------------------------------------------
function OpenDialog() : boolean;
begin
  result := false;
  Form_Objects.OpenDialog_FileName.Filter := imFileFilterString;
  Form_Objects.OpenDialog_FileName.FileName := imFileName;
  Form_Objects.OpenDialog_FileName.InitialDir := imInitialDir;
  if (Form_Objects.OpenDialog_FileName.Execute) then begin
    imFileName := Form_Objects.OpenDialog_FileName.FileName;
    result := true;
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
procedure TForm_Objects.Button_OpenClick(Sender: TObject);
var
  FileType : string;
begin
  // dialog to select input file - must be .X .CX .PX extension
  imFileFilterString := 'Object files (*.X *.CX *.PX *.C3D *.OBJ)|*.X;*.CX;*.PX;*.C3D;*.OBJ|All files (*.*)|*.*';
  imFileName := '';
  imInitialDir := oFolder;
//  form_CalibImport.ShowModal;
//  if (u_CalibImport.imActionRequest) then begin
  if OpenDialog then begin
    Label_FileName.Caption := ExtractFileName(imFileName);
//    ObjectFolder := ExtractFilePath(imFileName);
    ObjectFolder := ExtractFileDir(imFileName);     // no trailing '/'
    //remember folder for this session
    oFolder := ObjectFolder;
    //cancel mesh edit buttons
    Button_Show3D.enabled := false;
    Button_Centre.enabled := false;
    Button_Rotate.enabled := false;
    Button_Translate.enabled := false;

    // make sure treeView is selected
    u_X_CX.oTreeView := TreeView_Object;
// u_X_Cx.Memo_Message := Memo_Message;

    if (FileExists(imFileName)) then begin
      FileType := uppercase(ExtractFileExt(imFileName));
      if (FileType = '.C3D') then begin
        ReadCondorC3Dfile(imFileName, false);
      end else begin
        if (FileType = '.OBJ') then begin
          readXplaneOBJ8file(imFileName); // assume Xplane OBJ8, not Wavefront OBJ
        end else begin
          ReadCondorXfile(imFileName, false);
        end;
      end;
    end;

    Image_Clear(Image_Mesh);
    Image_Clear(Image_TextureVertices);
    Image_Clear(Image_Texture);

  end;
end;

//---------------------------------------------------------------------------
Procedure ShowColor(cArray, sArray : tFloatArray);
var
  cColor : ColorConvert;

begin
  with Form_Objects do begin
    GroupBox_MaterialColor.visible := true;
    Edit_Red.Text   := format('%1.2f',[cArray[0]]);
    Edit_Green.Text := format('%1.2f',[cArray[1]]);
    Edit_Blue.Text  := format('%1.2f',[cArray[2]]);
    Edit_Alpha.Text := format('%1.2f',[cArray[3]]);
    Edit_Spec.Text  := format('%1.2f',[sArray[0]]);
    Edit_Shiny.Text := format('%1.2f',[sArray[1]]);
    Edit_Env.Text   := format('%1.2f',[sArray[2]]);
    with {Form_Objects.}Image_Texture do begin
      cColor.ByteValue[0] := trunc(cArray[0]*255);  //Red
      cColor.ByteValue[1] := trunc(cArray[1]*255);  //Green
      cColor.ByteValue[2] := trunc(cArray[2]*255);  //Blue
      cColor.ByteValue[3] := 0; //trunc(cArray[3]*255);  //Alpha
      Canvas.Brush.Color := cColor.ColorValue;
      Canvas.FillRect(rect(0,0,Picture.Width,Picture.Height));
    end;
  end;
end;

//---------------------------------------------------------------------------
Procedure ShowBitmap(FileName:String);
var
  FileSubstitute : string;
  FilePicture: TPicture; // work around for PNG
//  Offset : integer;
  AspectRatio : real;

begin
  Form_Objects.GroupBox_MaterialColor.visible := false;
  with Form_Objects.Image_Texture do begin
    // assume size for now for TextOut to be reasonable size
    Picture.Bitmap.Width := 256;
    Picture.Bitmap.Height := 256;
    Canvas.Brush.Color := clWhite;
    Canvas.Pen.Color := clBlack;
    Canvas.FillRect(rect(0,0,Picture.Width,Picture.Height));
    if (Filename = '') then begin
      Canvas.TextOut(5,5,'No file specified');
    end else begin
      ConvertForwardToBack(FileName);
      if (pos(':',FileName) <> 0) then begin // absolute path
      end else begin
{
        if (pos('..',FileName) <> 0) then begin // relative path
          FileName := ObjectFolder+'\'+FileName;
        end else begin
          if (ExtractFilePath(FileName) = '') then begin // current path
            FileName := ObjectFolder+'\'+FileName;
          end else begin
            FileName := CondorFolder+'\'+FileName;
          end;
        end;
}
        // assume object relative path and check
        if ( FileExists(ObjectFolder+'\'+FileName) ) then begin
          FileName := ObjectFolder+'\'+FileName;
        end else begin // else try landscape relative
          if ( FileExists(WorkingFolder+'\..\'+FileName) ) then begin // if not default to Condor folder
            FileName := WorkingFolder+'\..\'+FileName;
          end else begin // else assume Condor folder realtive
            FileName := CondorFolder+'\'+FileName;
          end;
        end;
      end;
//      if (FileExists(CondorFolder+'\'+FileName)) then begin
      if (FileExists(FileName)) then begin
        if ( (Uppercase(ExtractFileExt(FileName)) = '.BMP') OR
             (Uppercase(ExtractFileExt(FileName)) = '.TGA') OR
             (Uppercase(ExtractFileExt(FileName)) = '.PNG') OR
             (Uppercase(ExtractFileExt(FileName)) = '.DDS') OR
             (Uppercase(ExtractFileExt(FileName)) = '.JPG') ) then begin
          Stretch := true;
       //   Stretch := false;
          // to load .JPG, nothing changes but must add 'uses JPEG'
          // to load .TGA, nothing changes but must add 'uses TGA'
          // to load .DDS, nothing changes but must add 'uses DDS'
          try
            if (uppercase(ExtractFileExt(FileName)) = '.BMP') then begin
//              FileSubstitute := WorkingFolder+'\Temp\'+ExtractFileName(FileName);
              FileSubstitute := ApplicationPath+'\Temp\'+ExtractFileName(FileName);
              if (LoadBMPfileFixAndSaveAsBMP(FileName,FileSubstitute)) then begin
                Picture.LoadFromFile(FileSubstitute);
              end else begin
                Picture.LoadFromFile(FileName);
              end;
            end else begin
              //Picture.LoadFromFile(FileName); // OK for BMP, JPG, DDS, TGA but not for PNG
              // work around for PNG - load as picture and then draw it onto bitmap
              FilePicture := TPicture.Create;
              try
                FilePicture.LoadFromFile(FileName);
                try
//                  with Picture.Bitmap do begin
                    SetStretchBltMode(Canvas.Handle, HALFTONE); // linear interpolation
                    // aspect ratio 1:1 for showing in relative texture coords
                    // aspect ratio correction if showing absolute bitmap
//                    AspectRatio := FilePicture.Width / FilePicture.Height;
                    AspectRatio := 1.0;
                    if (AspectRatio >= 1.0) then begin
                      Canvas.StretchDraw(Rect(0, 0, Width-1, round(Height/AspectRatio)-1), FilePicture.Graphic);
// for testing                      Canvas.Draw(0, 0, FilePicture.Graphic);
                    end else begin
                      Canvas.StretchDraw(Rect(0, 0, round(Width*AspectRatio)-1, Height-1), FilePicture.Graphic);
                    end;
//                  end;
                finally
                end;
              finally
                FilePicture.Free;
              end;
            end;
  // what is this for ?  Picture.Bitmap.PixelFormat := pf24bit; //force 24 bit color
          except
            Beep;
          end;
        end else begin
          Canvas.TextOut(5, 5,'Not Supported: '+ExtractFileExt(FileName));
          Canvas.TextOut(5,20,ExtractFileName(FileName));
        end;
      end else begin
        Canvas.TextOut(5, 5,'Not Found: '+ExtractFileName(FileName));
        Canvas.TextOut(5,20,ExtractFilePath(FileName));
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
Procedure FindMaterialAndShow(matName : string);
var
  i : integer;
  mFound : boolean;

begin
  mFound := false;
  //search tree for material with matching name
  with oTreeView do begin
    i := 0;
    while  (i <= Items.Count-1) do begin
      if (Items[i].data <> nil) then begin
        case pObjectItem(Items[i].data)^.oType of
          oMaterial: begin
            with pMaterial(pObjectItem(Items[i].data)^.oPointer)^ do begin
              if (tName = matName) then begin
                mFound := true;
                ShowColor(tRGBA,tRGBs);
                //see if it contains a filename
                // for now, if there is an object, it must be a filename
                if (oTreeView.Items[i].Count <> 0) then begin
                  ShowBitmap(pFileName(pObjectItem(Items[i+1].data)^.oPointer)^.tqName);
                end;
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
    // if not found -> error
    if (NOT mFound) then begin
      with Form_Objects.Image_Texture do begin
        Canvas.Brush.Color := clWhite;
        Canvas.Pen.Color := clBlack;
        Canvas.FillRect(rect(0,0,Picture.Width,Picture.Height));
        Canvas.TextOut(5,5,'Texture not found: '+ matName);
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Objects.TreeView_ObjectMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node : TTreeNode;
begin
    Node := TreeView_Object.GetNodeAt(X, Y);
    if (Node <> nil) then begin
      if (Node.Data <> nil) then begin
        with pObjectItem(Node.Data)^ do begin
          case oType of
            oFrame: begin
              Label_Name.Caption := pframe(oPointer)^.tName;
            end;
            oMesh: begin
              Label_Name.Caption := pMesh(oPointer)^.tName;
      //        mPlotIt(@pMesh(oPointer)^.tArray,@pMesh(oPointer)^);
              // improve by plotting surfaces instead of list of virtexes
              msPlotIt(@pMesh(oPointer)^);
            end;
            oMeshTextureCoord: begin
              Label_Name.Caption := pMeshTcoord(oPointer)^.tName;
      //        PlotIt(pMeshTcoord(oPointer)^.tArray);
              // improve by plotting surfaces instead of list of virtexes
              sPlotIt(@pMeshTcoord(oPointer)^);
            end;
            oFileName: begin
              Label_Name.Caption := pFileName(oPointer)^.tName+
                pFileName(oPointer)^.tqName;
              ShowBitmap(pFileName(oPointer)^.tqName);
            end;
            oMaterial: begin
              Label_Name.Caption := pMaterial(oPointer)^.tName;
              ShowColor(pMaterial(oPointer)^.tRGBA,pMaterial(oPointer)^.tRGBs);
            end;
            oMaterialReference: begin
              Label_Name.Caption := pMaterialReference(oPointer)^.tName;
              FindMaterialAndShow(pMaterialReference(oPointer)^.tName);
            end;
            // C3D types
//            o3DfileName: begin
//              Label_Name.Caption := p3DfileName(oPointer)^.tqName;
//              ShowBitmap(p3DfileName(oPointer)^.tqName);
//            end;
//            o3DmeshTextureCoord: begin
//              Label_Name.Caption := '';
//              C3D_PlotIt(p3DmeshTcoord(oPointer)^.mCount, p3DmeshTcoord(oPointer)^.mArray);
//              PlotIt(pMeshTcoord(oPointer)^.tArray);
//              // improve by plotting surfaces instead of list of virtexes
//            end;
//            o3Dmaterial: begin
//              Label_Name.Caption := '';
//              // show ambient color
//              ShowColor(p3Dmaterial(oPointer)^.tRGBA,p3Dmaterial(oPointer)^.tRGBs);
//            end;
//            o3Dmesh: begin
//              Label_Name.Caption := '';
////              mPlotIt(@p3Dmesh(oPointer)^.tArray,@p3Dmesh(oPointer)^);
//              mPlotIt(@pMesh(oPointer)^.tArray,@pMesh(oPointer)^);
//            end;
          end;
        end;
      end;
    end;
end;

//---------------------------------------------------------------------------
procedure TForm_Objects.FormCreate(Sender: TObject);
begin
  Image_TextureVertices.Picture.Bitmap.Create;
  Image_TextureVertices.Picture.Bitmap.Width := Image_TextureVertices.Width;
  Image_TextureVertices.Picture.Bitmap.Height := Image_TextureVertices.HeigHt;
  tvBitmap := Image_TextureVertices.Picture.Bitmap;

  Image_Mesh.Picture.Bitmap.Create;
  Image_Mesh.Picture.Bitmap.Width := Image_Mesh.Width;
  Image_Mesh.Picture.Bitmap.Height := Image_Mesh.HeigHt;
  mBitmap := Image_Mesh.Picture.Bitmap;

end;

//---------------------------------------------------------------------------
procedure TForm_Objects.TreeView_ObjectChange(Sender: TObject;
  Node: TTreeNode);
begin
//  Label_FileName.Caption := IntToStr(Node.AbsoluteIndex);
  if ((Node.Data <> nil) AND (pObjectItem(Node.Data)^.oType = oMesh)) then begin
//  if ((Node.Data <> nil) AND ((pObjectItem(Node.Data)^.oType = oMesh) OR
//                              (pObjectItem(Node.Data)^.oType = o3Dmesh)) ) then begin
    SelectedMeshNode := Node;
    Button_Show3D.enabled := true;
    Button_Centre.enabled := true;
    Button_Rotate.enabled := true;
    Button_Translate.enabled := true;
  end else begin
    Button_Show3D.enabled := false;
    Button_Centre.enabled := false;
    Button_Rotate.enabled := false;
    Button_Translate.enabled := false;
  end;
end;

//---------------------------------------------------------------------------
function SaveDialog() : boolean;
begin
  result := false;
  Form_Objects.SaveDialog_FileName.Filter := exFileFilterString;
  Form_Objects.SaveDialog_FileName.FileName := exFileName;
  Form_Objects.SaveDialog_FileName.InitialDir := exInitialDir;
  if (Form_Objects.SaveDialog_FileName.Execute) then begin
    exFileName := Form_Objects.SaveDialog_FileName.FileName;
    result := true;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Objects.Button_SaveAsClick(Sender: TObject);
begin
  // dialog to select output file - must be .X .CX .PX extension
  exFileFilterString := 'Object files (*.X *.CX *.PX *.C3D)|*.X;*.CX;*.PX;*.C3D|All files (*.*)|*.*';
  exFileName := imFileName;
  imInitialDir := oFolder;
//  form_CalibExport.ShowModal;
//  if (u_CalibExport.exActionRequest) then begin
  if (SaveDialog) then begin
    Label_FileName.Caption := ExtractFileName(exFileName);
//    ObjectFolder := ExtractFilePath(exFileName);
    ObjectFolder := ExtractFileDir(exFileName);  // not including trailing '\'
    //remember folder for this session
    oFolder := ObjectFolder;

    // if rotation <> 0 and displaying temporary rotation (not implemented yet)
    // then rotate before saving
    u_X_CX.Path := CondorFolder;
//    WriteCondorXfile(exFileName,false,false);
    WriteCondorObjectFile(exFileName,None,false);

  end;
end;

//---------------------------------------------------------------------------
end.


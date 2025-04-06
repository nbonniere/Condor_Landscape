{----------------------------------------------------------------------------
WaveFront .OBJ object coordinate encoding/decoding
----------------------------------------------------------------------------}

{----------------------------------------------------------------------------
OBJ WF file structure
  // Material file reference
  MaterialFileName_Type = record:
//    mfID       : string;  //'mtllib'
	mfFileName : string; // filename.mtl
  end;
  
  // object name
  ObjectName_Type = record:
//    onID    : string;  //'o'
	onName  : string;
  end;

  // Material/Texture selection 
  Material_Type = record:
//    mtID       : string;  //'usemtl'
	mtName : string;
  end;

  // Vertex
  Vertex_Type = record:
//    vID       : string;  //'v'
	vArray : Array[3] of float;
  end;

  // Vertex normal
  VertexNormal_Type = record:
//    vnID       : string;  //'vn'
	vnArray : Array[3] of float;
  end;

  // Vertex texture coordinates
  VertexTC_Type = record:
//    vtID       : string;  //'vt'
	vtArray : Array[2] of float;
  end;

  // Object surface definition
  // triangle with list of 3 vertices, list of 3 normals, list of 3 texture coords
  Surface_Type = record:
//    fID       : string;  //'f'
	fArray : Array[3] of Array[3] of longword;
  end;

  // Vertex group (can have own material)
  VertexTC_Type = record:
//    gID       : string;  //'g'
	gName : string;
  end;

  // Vertex smoothing group (can have own material)
  VertexTC_Type = record:
//    sgID       : string;  //'s'
	sIndex : Longint;
  end;

  // Material definition
  MaterialDEF = record:
//    mdID       : string;  //'newmtl'
	kAmbient  : Array[3] of float;
	kDiffuse  : Array[3] of float;
	kSpecular : Array[3] of float; // shinyness
	kEmissive : Array[3] of float;
        nSpecular : float; // focus 0 .. 1000 (shinyness)
        nIndex : float; // index of refraction 0.001 .. 1.0
        nDisolve : float; // opaqueness 0.0 .. 1.0
        iIllumination : integer; // illumination model
        tTextureFileName : string;  // affected by kDiffuse
  end;

OBJ WF MTL file structure example
  newmtl Wood
  Ka 1.000000 1.000000 1.000000
  Kd 0.640000 0.640000 0.640000
  Ks 0.500000 0.500000 0.500000
  Ke 0.000000 0.000000 0.000000
  Ns 96.078431
  Ni 1.000000
  d 1.000000
  illum 0
  map_Kd woodtexture.jpg

--------------------------------------------------------------------------------
// for parsing, use object structure definition
Name from o or g
Material reference
SurfaceArrayStartIndex
Surface count

Parsing procedure
case v  -> add to vertex array
case vn -> add to normal array
case vt -> add to texture-coord array
case f  -> add to surface array, update surface index count in current object
case o or g  -> save current surface index and set surface index count to 0
case usemtl -> save material reference into current object
case mtllib -> save material file name in material array

----------------------------------------------------------------------------}

const
  wfOBJ_Magic = 'WF_OBJ'; // dummy value

type
  Header_wfOBJ_Type = record
    // no file header
  end;

//  TextureFileName_Type = record
//    tFileName : string;
//  end;

  SfArray_Type = array[0..3-1] of Longint;

  Vector_Array_Type = record
    x   : double;  // X coordinate
    y   : double;  // Y coordinate
    z   : double;  // Z coordinate
  end;
  Normal_Array_Type = record
    nx  : double;  // normal X vector
    ny  : double;  // normal Y vector
    nz  : double;  // normal Z vector
  end;
  tCoord_Array_Type = record
    s   : double;  // X texture coordinate
    t   : double;  // Y texture coordinate
  end;
  Surface_Array_Type = array[0..3-1] of SfArray_Type;
//  Surface_Array_Type = record
//    i1 : SfArray_Type;
//    i2 : SfArray_Type;
//    i3 : SfArray_Type;
//  end;
  MaterialFile_Type = record
    fileName : string;
  end;

  MaterialDEF_Type = record
    name : string;
    kAmbient  : Array[0..3-1] of double;
    kDiffuse  : Array[0..3-1] of double;
    kSpecular : Array[0..3-1] of double;
    kEmissive : Array[0..3-1] of double;
    nSpecular : double;
    nIndex : double;
    nDisolve : double;
    iIllumination : integer;
    tTextureFileName : string;
  end;

  Object_Type = record
    name     : string;
    material : string;
    index    : integer; // index into Surface_List
    count    : integer; // triangular surface count
  end;

  Vector_List_Type = array of Vector_Array_Type;
  Normal_List_Type = array of Normal_Array_Type;
  tCoord_List_Type = array of tCoord_Array_Type;
  Surface_List_Type = array of Surface_Array_Type;
  MaterialFile_List_Type = array of MaterialFile_Type;
  Obj_List_Type = array of Object_Type;
  Material_List_Type = array of MaterialDEF_Type;

//  pwfOBJheader = ^twfOBJheader;
  pwfOBJheader = ^t3Dheader; // for now
  twfOBJheader = record
  end;

  pwfOBJmagic = ^twfOBJmagic;
  twfOBJmagic = record
  end;

var
//  pwfOBJmagicData : pwfOBJmagic;
//  pwfOBJheaderData : pwfOBJheader;

//  Header_wfOBJ : Header_wfOBJ_Type;

  Vector_List : Vector_List_Type;
  vCount : integer;
  Normal_List : Normal_List_Type;
  vnCount : integer;
  tCoord_List : tCoord_List_Type;
  tcCount : integer;
  Surface_List : Surface_List_Type;
  sfCount : integer;
  MaterialFile_List : MaterialFile_List_Type;
  mfCount : integer;
  Obj_List : Obj_List_Type;
  oCount : integer;
  Material_List : Material_List_Type;
  mCount : integer;

//----------------------------------------------------------------------------
procedure wfOBJfile_CreateTreeViewVersion;
var
  i, j, k : integer;
  prevCount : integer;
  fTreeNode, fTreeNode2 : TTreeNode;
  sfIndex : integer;
begin
  ClearTreeView(oTreeView);

  New(pObjectData); //allocate space for an object data
  pObjectData^.oType := owfOBJmagic;
  New(p3DmagicData); //allocate space for data
  pObjectData^.oPointer := p3DmagicData; // dummy data
  oTreeView.Items.AddChildObject(nil, wfOBJ_Magic, pObjectData);

  New(pObjectData); //allocate space for an object data
  pObjectData^.oType := owfOBJheader;
  New(p3DheaderData); //allocate space for data
  pObjectData^.oPointer := p3DheaderData; // dummy data
  oTreeView.Items.AddChildObject(nil, 'Header', pObjectData);

  for i := 0 to oCount-1 do begin
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oFrame;
    New(pFrameData); //allocate space for a frame data
    pObjectData^.oPointer := pFrameData;
    pFrameData^.tName := 'f_'+Obj_List[i].name;
    fTreeNode := oTreeView.Items.AddChildObject(nil, 'Frame '+pFrameData^.tName, pObjectData);

    // mesh
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oMesh;
    New(pMeshData); //allocate space for data
    pMeshData^.tName := Obj_List[i].name;
    pMeshData^.tRotation := 0.0;
    pMeshData^.tX := 0.0;
    pMeshData^.tY := 0.0;
    pMeshData^.tHeight := 0.0;
    pMeshData^.tScale := 1.0;
    // Do vertices
    // calc vCount for object   // ??? needs to be vcount for object not ALL!
    pMeshData^.tArray.aCount := Obj_List[i].Count*3;
    setLength(pMeshData^.tArray.aArray,pMeshData^.tArray.aCount);
    for j := 0 to Obj_List[i].Count-1 do begin // each surface per object
      for k := 0 to 3-1 do begin // each vector per surface
        setLength(pMeshData^.tArray.aArray[j*3+k],3);
        sfIndex := Surface_List[Obj_List[i].index+j][k][0]-1; // 0 for vector, -1 for 1 based array
        case OBJ_Type of
          0: begin // Wings3D
            // reverse Z, -X, Y (Wings3D)
            pMeshData^.tArray.aArray[j*3+k][0] :=  Vector_List[sfIndex].z;
            pMeshData^.tArray.aArray[j*3+k][1] := -Vector_List[sfIndex].x;
            pMeshData^.tArray.aArray[j*3+k][2] :=  Vector_List[sfIndex].y;
          end;
          1,2: begin // Blender
            // reverse Y, X, Z (Blender)
            pMeshData^.tArray.aArray[j*3+k][0] := Vector_List[sfIndex].y;
            pMeshData^.tArray.aArray[j*3+k][1] := Vector_List[sfIndex].x;
            pMeshData^.tArray.aArray[j*3+k][2] := Vector_List[sfIndex].z;
          end;
          else begin
            // should not get here
          end;
        end;
      end;
    end;
    CalcExtents(@pMeshData^.tArray,@pMeshData^);
    // Do surfaces
    pMeshData^.sArray.aCount := Obj_List[i].count;
    setLength(pMeshData^.sArray.aArray,pMeshData^.sArray.aCount);
    for j := 0 to Obj_List[i].count-1 do begin
      pMeshData^.sArray.aArray[j].aCount := 3;
      setLength(pMeshData^.sArray.aArray[j].aArray,pMeshData^.sArray.aArray[j].aCount);
{      for k := 0 to 3-1 do begin
        pMeshData^.sArray.aArray[j].aArray[k] := j*3+k; // vectors have been selected
      end; }
      // need to change order of vertices
      pMeshData^.sArray.aArray[j].aArray[0] := j*3+0; // vectors have been selected
      pMeshData^.sArray.aArray[j].aArray[1] := j*3+2;
      pMeshData^.sArray.aArray[j].aArray[2] := j*3+1;
    end;
    pObjectData^.oPointer := pMeshData;
//    fTreeNode2 := oTreeView.Items.AddChildObject(fTreeNode, 'Mesh', pObjectData);
    fTreeNode2 := oTreeView.Items.AddChildObject(fTreeNode, 'Mesh '+pMeshData^.tName, pObjectData);

    // mesh normals
    // Do normals
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oMeshNormals;
    New(pMeshNormalsData); //allocate space for data
    pMeshNormalsData^.tName := 'mn_'+Obj_List[i].name;
    // calc vnCount for object   // ??? needs to be vnCount for object not ALL!
    pMeshNormalsData^.tArray.aCount := Obj_List[i].Count*3;
    setLength(pMeshNormalsData^.tArray.aArray,pMeshNormalsData^.tArray.aCount);
    for j := 0 to Obj_List[i].Count-1 do begin // each surface per object
      for k := 0 to 3-1 do begin // each vector per surface
        setLength(pMeshNormalsData^.tArray.aArray[j*3+k],3);
        sfIndex := Surface_List[Obj_List[i].index+j][k][2]-1; // 2 for normals, -1 for 1 based array
        case OBJ_Type of
          0: begin // Wings3D
            // reverse Z, -X, Y (Wings3D)
            pMeshNormalsData^.tArray.aArray[j*3+k][0] :=  Normal_List[sfIndex].nz;
            pMeshNormalsData^.tArray.aArray[j*3+k][1] := -Normal_List[sfIndex].nx;
            pMeshNormalsData^.tArray.aArray[j*3+k][2] :=  Normal_List[sfIndex].ny;
          end;
          1,2: begin // Blender
            // reverse Y, X, Z (Blender)
            pMeshNormalsData^.tArray.aArray[j*3+k][0] := Normal_List[sfIndex].ny;
            pMeshNormalsData^.tArray.aArray[j*3+k][1] := Normal_List[sfIndex].nx;
            pMeshNormalsData^.tArray.aArray[j*3+k][2] := Normal_List[sfIndex].nz;
          end;
          else begin
            // should not get here
          end;
        end;
      end;
    end;
    // Do surfaces
    pMeshNormalsData^.sArray.aCount := Obj_List[i].count;
    setLength(pMeshNormalsData^.sArray.aArray,pMeshNormalsData^.sArray.aCount);
    for j := 0 to Obj_List[i].count-1 do begin
      pMeshNormalsData^.sArray.aArray[j].aCount := 3;
      setLength(pMeshNormalsData^.sArray.aArray[j].aArray,pMeshNormalsData^.sArray.aArray[j].aCount);
{      for k := 0 to 3-1 do begin
//        pMeshNormalsData^.sArray.aArray[j].aArray[k] := Surface_List[Obj_List[i].index+j][k][0]-1; // 0 for vectors, -1 for 1 based array
        pMeshNormalsData^.sArray.aArray[j].aArray[k] := j*3+k; // vectors have been selected
      end;}
      // need to change order of vertices
      pMeshNormalsData^.sArray.aArray[j].aArray[0] := j*3+0; // vectors have been selected
      pMeshNormalsData^.sArray.aArray[j].aArray[1] := j*3+2; // vectors have been selected
      pMeshNormalsData^.sArray.aArray[j].aArray[2] := j*3+1; // vectors have been selected
    end;
    pObjectData^.oPointer := pMeshNormalsData;
    oTreeView.Items.AddChildObject(fTreeNode2, 'MeshNormals', pObjectData);

    // mesh texture coordinates
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oMeshTextureCoord;
    New(pMeshTcoordData); //allocate space for data
    pMeshTcoordData^.tName := 'tc_'+Obj_List[i].name;
    // calc tcCount for object   // ??? needs to be tcCount for object not ALL!
    pMeshTcoordData^.tArray.aCount := Obj_List[i].Count*3;
    setLength(pMeshTcoordData^.tArray.aArray,pMeshTcoordData^.tArray.aCount);
    for j := 0 to Obj_List[i].Count-1 do begin // each surface per object
      for k := 0 to 3-1 do begin // each vector per surface
        setLength(pMeshTcoordData^.tArray.aArray[j*3+k],2);
        sfIndex := Surface_List[Obj_List[i].index+j][k][1]-1; // 1 for tCoord, -1 for 1 based array
        case OBJ_Type of
          0, 1: begin // 0.0 to 1.0 range
            pMeshTcoordData^.tArray.aArray[j*3+k][0] :=       tCoord_List[sfIndex].s;
//            pMeshTcoordData^.tArray.aArray[j*3+k][1] :=       tCoord_List[sfIndex].t;
            // reverse/flip Y
            pMeshTcoordData^.tArray.aArray[j*3+k][1] := 1.0 - tCoord_List[sfIndex].t;
          end;
          2: begin // Condor Autogen - uses the object size to tile the texture
            // applies to buildings, but not powerlines and gondolas/ski-lifts ? What a mess!
            pMeshTcoordData^.tArray.aArray[j*3+k][0] :=       tCoord_List[sfIndex].s;
            pMeshTcoordData^.tArray.aArray[j*3+k][1] :=       tCoord_List[sfIndex].t;
            // reverse/flip Y ?
//            pMeshTcoordData^.tArray.aArray[j*3+k][1] :=      -tCoord_List[sfIndex].t;
          end;
          else begin
            // should not get here
          end;
        end;
      end;
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

    // material - default
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

    // filename - default
    New(pObjectData); //allocate space for an object data
    pObjectData^.oType := oFileName;
    New(pFileNameData); //allocate space for data
    pFileNameData^.tName := '';
    pFileNameData^.tqName := '';
    pObjectData^.oPointer := pFileNameData;
    oTreeView.Items.AddChildObject(fTreeNode2, 'FileName', pObjectData);
//    oTreeView.Items.AddChildObject(fTreeNode, 'FileName', pObjectData);

    // find material reference if any
    if (Obj_List[i].material <> '') then begin // is there a material reference?
      // search for it in the material list
      for k := 0 to mCount-1 do begin
        if (Obj_List[i].material = Material_List[k].name) then begin
          // copy the data
          // ambient color
          pMaterialData^.tRGBA[0] := Material_List[k].kAmbient[0];
          pMaterialData^.tRGBA[1] := Material_List[k].kAmbient[1];
          pMaterialData^.tRGBA[2] := Material_List[k].kAmbient[2];
          // opacity
          pMaterialData^.tRGBA[3] := Material_List[k].nDisolve;
          // texture file name
          pFileNameData^.tqName := Material_List[k].tTextureFileName;
          break;
        end;
      end;
    end;

  end;

end;

//----------------------------------------------------------------------------
function read_wfMTLfile(FileName : string) : boolean;
var
  i : integer;
  Temp_File : TextFile;
  TempSTR : string;
  pString : string;
begin
  AssignFile(Temp_file, FileName);
  Reset(Temp_file);
//  SetTextBuf(Temp_file, TextBuffer);

  try
    while NOT EOF(Temp_file) do begin
      Readline(@Temp_file,TempSTR);
      ParseText(TempSTR,pString);
      if (UpperCase(pString) = 'NEWMTL') then begin
        // create new material
        INC(mCount);
        SetLength(Material_List,mCount);
        with Material_List[mCount-1] do begin
          parseText(Tempstr,name);
        end;
        Continue;
      end;
      if (UpperCase(pString) = 'KA') then begin
        if (mCount > 0) then begin
          with Material_List[mCount-1] do begin
            parseFloat(Tempstr,kAmbient[0]);
            parseFloat(Tempstr,kAmbient[1]);
            parseFloat(Tempstr,kAmbient[2]);
          end;
        end;
        Continue;
      end;
      if (UpperCase(pString) = 'KD') then begin
        if (mCount > 0) then begin
          with Material_List[mCount-1] do begin
            parseFloat(Tempstr,kDiffuse[0]);
            parseFloat(Tempstr,kDiffuse[1]);
            parseFloat(Tempstr,kDiffuse[2]);
          end;
        end;
        Continue;
      end;
      if (UpperCase(pString) = 'KS') then begin
        if (mCount > 0) then begin
          with Material_List[mCount-1] do begin
            parseFloat(Tempstr,kSpecular[0]);
            parseFloat(Tempstr,kSpecular[1]);
            parseFloat(Tempstr,kSpecular[2]);
          end;
        end;
        Continue;
      end;
      if (UpperCase(pString) = 'KE') then begin
        if (mCount > 0) then begin
          with Material_List[mCount-1] do begin
            parseFloat(Tempstr,kEmissive[0]);
            parseFloat(Tempstr,kEmissive[1]);
            parseFloat(Tempstr,kEmissive[2]);
          end;
        end;
        Continue;
      end;
      if (UpperCase(pString) = 'NS') then begin
        if (mCount > 0) then begin
          with Material_List[mCount-1] do begin
            parseFloat(Tempstr,nSpecular);
          end;
        end;
        Continue;
      end;
      if (UpperCase(pString) = 'NI') then begin
        if (mCount > 0) then begin
          with Material_List[mCount-1] do begin
            parseFloat(Tempstr,nIndex);
          end;
        end;
        Continue;
      end;
      if (UpperCase(pString) = 'D') then begin
        if (mCount > 0) then begin
          with Material_List[mCount-1] do begin
            parseFloat(Tempstr,nDisolve);
          end;
        end;
        Continue;
      end;
      if (UpperCase(pString) = 'ILLUM') then begin
        if (mCount > 0) then begin
          with Material_List[mCount-1] do begin
            parseInteger(Tempstr,iIllumination);
          end;
        end;
        Continue;
      end;
      if (UpperCase(pString) = 'MAP_KD') then begin
        if (mCount > 0) then begin
          with Material_List[mCount-1] do begin
            parseText(Tempstr,tTextureFileName);
          end;
        end;
        Continue;
      end;
      // else ignore
    end;

  except
//    ShowMessage('File error - parsing');
    CloseFile(Temp_file);
    beep; exit;
  end;

  // all done reading file
  CloseFile(Temp_file);

end;

//----------------------------------------------------------------------------
function read_wfOBJfile(FileName : string) : boolean;
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
function parseThree(var SL : SfArray_Type) : boolean;
var
  Temp : string;
begin
  parseThree := true; // assume for now

  // kludge for now
  TempStr := StringReplace(TempStr,'/',' ',[rfReplaceAll]); // replace / by SPACE

    ParseInteger(Tempstr,SL[0]);
 //	ParseText(Tempstr,Temp);
 //	if (Temp <> '/') then begin
 //	  parseThree := false;
 //	  exit;
 //	end;
    ParseInteger(Tempstr,SL[1]);
  //	ParseText(Tempstr,Temp);
  //	if (Temp <> '/') then begin
  //	  parseThree := false;
  //	  exit;
  //	end;
    ParseInteger(Tempstr,SL[2]);
end;

// - - - - - - - - - - - - - - - - - - - - - - -
begin
  Result := false;  // assume fault
  vCount  := 0;
  vnCount := 0;
  tcCount := 0;
  sfCount := 0;
  mfCount := 0;
  oCount := 0;

  AssignFile(Temp_file, FileName);
  Reset(Temp_file);
//  SetTextBuf(Temp_file, TextBuffer);

  try
    while NOT EOF(Temp_file) do begin
      Readline(@Temp_file,TempSTR);
      ParseText(TempSTR,pString);

      if (UpperCase(pString) = 'V') then begin
        SetLength(Vector_List,vCount+1);
        with Vector_List[vCount] do begin
          parseFloat(Tempstr,x);
          parseFloat(Tempstr,y);
          parseFloat(Tempstr,z);
          INC(vCount);
          Continue;
        end;
      end;
      if (UpperCase(pString) = 'VN') then begin
        SetLength(Normal_List,vnCount+1);
        with Normal_List[vnCount] do begin
          parseFloat(Tempstr,nx);
          parseFloat(Tempstr,ny);
          parseFloat(Tempstr,nz);
          INC(vnCount);
          Continue;
        end;
      end;
      if (UpperCase(pString) = 'VT') then begin
        SetLength(tCoord_List,vtCount+1);
        with tCoord_List[vtCount] do begin
          parseFloat(Tempstr,s);
          parseFloat(Tempstr,t);
          INC(vtCount);
          Continue;
        end;
      end;
      // careful, indices are 1 based, not 0 based!
      if (UpperCase(pString) = 'F') then begin
        SetLength(Surface_List,sfCount+1);
          parseThree(Surface_List[sfCount][0]);
          parseThree(Surface_List[sfCount][1]);
          parseThree(Surface_List[sfCount][2]);
          INC(sfCount);
          with Obj_List[oCount-1] do begin
            count := sfCount-index;   // triangular surface count
          end;
          Continue;
      end;
      if (UpperCase(pString) = 'MTLLIB') then begin
        SetLength(MaterialFile_List,mfCount+1);
        with MaterialFile_List[mfCount] do begin
          parseText(Tempstr,fileName);
          INC(mfCount);
          Continue;
        end;
      end;
      if (UpperCase(pString) = 'USEMTL') then begin
        with Obj_List[oCount-1] do begin
          parseText(Tempstr,Material);
          Continue;
        end;
      end;
      if ( (UpperCase(pString) = 'O') OR
           (UpperCase(pString) = 'G') ) then begin
        INC(oCount);
        SetLength(Obj_List,oCount);
        with Obj_List[oCount-1] do begin
          parseText(Tempstr,name);
          index := sfCount; // index into Surface_List
          count := 0;       // triangular surface count
        end;
      end;
      // else ignore
    end;

  except
//    ShowMessage('File error - parsing');
    CloseFile(Temp_file);
    beep; exit;
  end;

  // all done reading file
  CloseFile(Temp_file);

  // Validate
  // there must be at least one object with one surface
  if (oCount < 1) then begin
//    ShowMessage('File error - no objects');
    beep; exit;
  end;
  with Obj_List[0] do begin
    if (count < 1) then begin
//      ShowMessage('File error - no surfaces');
      beep; exit;
    end;
  end;

  // if get here, no errors
  Result := true;

  // resolve materials
  // read all material files
  for i := 0 to mfCount-1 do begin
    with MaterialFile_List[mfCount] do begin
      read_wfMTLfile(MaterialFile_List[i].filename);
    end;
  end;
  // for each object, look-up material and fill-in details
//  for i := 0 to ? begin
//  end;

  if (Result) then begin
    wfOBJfile_CreateTreeViewVersion;
  end;
end;

{
//----------------------------------------------------------------------------
function Get_wfOBJtexture : string;
begin
  Result := TextureFilename.tFileName;
end;

//----------------------------------------------------------------------------
procedure Adjust_wfOBJtexture(FileName : string);
begin
  TextureFilename.tFileName := FileName;
  wfOBJfile_CreateTreeViewVersion;
end;
}

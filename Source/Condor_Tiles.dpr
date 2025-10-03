program Condor_Tiles;

uses
  Forms,
  Unit_Main in 'Unit_Main.pas' {Form_Main},
  Unit_About in 'Unit_About.pas' {AboutBox},
  Unit_Graphics in 'Unit_Graphics.pas' {Form_Graphic},
  Unit_Filter in 'Unit_Filter.pas' {Form_Filter},
  Unit_EditPrompt in 'Unit_EditPrompt.pas' {Form_EditPrompt},
  Unit_Objects in 'Unit_Objects.pas' {Form_Objects},
  Unit_ObjectPlacer in 'Unit_ObjectPlacer.pas' {Form_ObjectPlacer},
  Unit_AirportPlacer in 'Unit_AirportPlacer.pas' {Form_AirportPlacer},
  Unit_Palette in 'Unit_Palette.pas' {Form_Palette},
  Unit_Utilities in 'Unit_Utilities.pas' {Form_Utilities},
  Unit_DEM in 'Unit_DEM.pas' {Form_DEM},
  Unit_Coords in 'Unit_Coords.pas' {Form_Coords},
  Unit_SimpleObjects in 'Unit_SimpleObjects.pas' {Form_SimpleObjects},
  Unit_HiResRunway in 'Unit_HiResRunway.pas' {Form_HiResRunway},
  Unit_Help in 'Unit_Help.pas' {Form_Help},
  u_MakeForest in 'u_MakeForest.pas' {Form_MakeForest},
  u_MakeThermal in 'u_MakeThermal.pas' {Form_MakeThermal},
  u_MakeGradient in 'u_MakeGradient.pas' {Form_Gradient},
  u_Exec in 'u_Exec.pas',
  PaletteLibrary in 'Libraries\Palette\PaletteLibrary.PAS',
  ColorQuantizationLibrary in 'Libraries\Palette\ColorQuantizationLibrary.pas',
  TGA in 'Libraries\TGA\TGA.pas',
  DDS in 'Libraries\DDS\DDS.pas',
  DXTC in 'Libraries\DDS\DXTC.pas',
  pngimage in 'Libraries\PNG\pngimage.pas',
  pnglang in 'Libraries\PNG\pnglang.pas',
  zlibpas in 'Libraries\PNG\zlibpas.pas',
  Unit_WarpCrop in 'Unit_WarpCrop.pas' {Form_WarpCrop},
  Unit_Merge in 'Unit_Merge.pas' {Form_Merge},
  Unit_Shift in 'Unit_Shift.pas' {Form_Shift};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm_Main, Form_Main);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TForm_Graphic, Form_Graphic);
  Application.CreateForm(TForm_Filter, Form_Filter);
  Application.CreateForm(TForm_MakeForest, Form_MakeForest);
  Application.CreateForm(TForm_MakeThermal, Form_MakeThermal);
  Application.CreateForm(TForm_Gradient, Form_Gradient);
  Application.CreateForm(TForm_EditPrompt, Form_EditPrompt);
  Application.CreateForm(TForm_Objects, Form_Objects);
  Application.CreateForm(TForm_ObjectPlacer, Form_ObjectPlacer);
  Application.CreateForm(TForm_AirportPlacer, Form_AirportPlacer);
  Application.CreateForm(TForm_Palette, Form_Palette);
  Application.CreateForm(TForm_Utilities, Form_Utilities);
  Application.CreateForm(TForm_DEM, Form_DEM);
  Application.CreateForm(TForm_Coords, Form_Coords);
  Application.CreateForm(TForm_SimpleObjects, Form_SimpleObjects);
  Application.CreateForm(TForm_HiResRunway, Form_HiResRunway);
  Application.CreateForm(TForm_Help, Form_Help);
  Application.CreateForm(TForm_WarpCrop, Form_WarpCrop);
  Application.CreateForm(TForm_Merge, Form_Merge);
  Application.CreateForm(TForm_Shift, Form_Shift);
  Application.Run;
end.


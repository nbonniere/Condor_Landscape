program Condor_Tiles;

uses
  Forms,
  Unit_Main in 'Unit_Main.pas' {Form_Main},
  Unit_About in 'Unit_About.pas' {AboutBox},
  Unit_Graphics in 'Unit_Graphics.pas' {Form_Graphic},
  Unit_Filter in 'Unit_Filter.pas' {Form_Filter},
  u_MakeForest in 'u_MakeForest.pas' {Form_MakeForest},
  u_MakeThermal in 'u_MakeThermal.pas' {Form_MakeThermal},
  u_CalibImport in 'u_CalibImport.pas' {Form_CalibImport},
  u_CalibExport in 'u_CalibExport.pas' {Form_CalibExport},
  u_MakeGradient in 'u_MakeGradient.pas' {Form_Gradient},
  Unit_EditPrompt in 'Unit_EditPrompt.pas' {Form_EditPrompt},
  Unit_Objects in 'Unit_Objects.pas' {Form_Objects},
  u_Exec in 'u_Exec.pas',
  Unit_ObjectPlacer in 'Unit_ObjectPlacer.pas' {Form_ObjectPlacer},
  Unit_AirportPlacer in 'Unit_AirportPlacer.pas' {Form_AirportPlacer},
  Unit_Palette in 'Unit_Palette.pas' {Form_Palette},
  TGA in 'Graphics\TGA\TGA.pas',
  DDS in 'Graphics\DDS\DDS.pas',
  DXTC in 'Graphics\DDS\DXTC.pas',
  Unit_Utilities in 'Unit_Utilities.pas' {Form_Utilities},
  Unit_DEM in 'Unit_DEM.pas' {Form_DEM},
  Unit_Coords in 'Unit_Coords.pas' {Form_Coords},
  Unit_Merge in 'Unit_Merge.pas' {Form_Merge};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm_Main, Form_Main);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TForm_Graphic, Form_Graphic);
  Application.CreateForm(TForm_Filter, Form_Filter);
  Application.CreateForm(TForm_MakeForest, Form_MakeForest);
  Application.CreateForm(TForm_MakeThermal, Form_MakeThermal);
  Application.CreateForm(TForm_CalibImport, Form_CalibImport);
  Application.CreateForm(TForm_CalibExport, Form_CalibExport);
  Application.CreateForm(TForm_Gradient, Form_Gradient);
  Application.CreateForm(TForm_EditPrompt, Form_EditPrompt);
  Application.CreateForm(TForm_Objects, Form_Objects);
  Application.CreateForm(TForm_ObjectPlacer, Form_ObjectPlacer);
  Application.CreateForm(TForm_AirportPlacer, Form_AirportPlacer);
  Application.CreateForm(TForm_Palette, Form_Palette);
  Application.CreateForm(TForm_Utilities, Form_Utilities);
  Application.CreateForm(TForm_DEM, Form_DEM);
  Application.CreateForm(TForm_Coords, Form_Coords);
  Application.CreateForm(TForm_Merge, Form_Merge);
  Application.Run;
end.


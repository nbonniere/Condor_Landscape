
//-----------------------------------------------------------------------
unit u_Condor_NaviconDLL;

{$S-,R-,I-,V-,F+,D+}

//-----------------------------------------------------------------------
interface

uses
  Windows;

type
  Condor_Navicon_Init_type = function ( Landscape : string) : boolean; stdcall;
  // 0 -> fault, 1 -> OK

  Condor_Navicon_GetMaxX_type = function : double; stdcall;

  Condor_Navicon_GetMaxY_type = function : double; stdcall;

  Condor_Navicon_XYToLon_type = function ( X:single; Y:single) : double; stdcall;
  // UTM X, Y -> result in degrees

  Condor_Navicon_XYToLat_type = function ( X:single; Y:single) : double; stdcall;
  // UTM X, Y -> result in degrees

var
  CondorFolder : string;
  Condor_Navicon_GetMaxX : Condor_Navicon_GetMaxX_type;
  Condor_Navicon_GetMaxY : Condor_Navicon_GetMaxY_type;
  Condor_Navicon_XYToLon : Condor_Navicon_XYToLon_type;
  Condor_Navicon_XYToLat : Condor_Navicon_XYToLat_type;

function Condor_Navicon_Open ( Landscape : string) : boolean;
function Condor_Navicon_Close : boolean;

//-----------------------------------------------------------------------
implementation

uses sysutils;

type
  THandle = Integer;

var
  fHandle : THandle;
  Condor_Navicon_Init : Condor_Navicon_Init_type;

//-----------------------------------------------------------------------
function Condor_Navicon_Open ( Landscape : string)  : boolean;
begin
  Condor_Navicon_Open := true; // assume for now

  fHandle := LoadLibrary(PCHAR(CondorFolder+'\'+'NAVICON.DLL'));
  if (fHandle <> 0) then begin
    @Condor_Navicon_Init := GetProcAddress(fHandle, 'NaviConInit');
    if (@Condor_Navicon_Init = nil) then begin
      Condor_Navicon_Open := false;
    end;

    @Condor_Navicon_GetMaxX := GetProcAddress(fHandle, 'GetMaxX');
    if (@Condor_Navicon_GetMaxX = nil) then begin
      Condor_Navicon_Open := false;
    end;

    @Condor_Navicon_GetMaxY := GetProcAddress(fHandle, 'GetMaxY');
    if (@Condor_Navicon_GetMaxY = nil) then begin
      Condor_Navicon_Open := false;
    end;

    @Condor_Navicon_XYToLon := GetProcAddress(fHandle, 'XYToLon');
    if (@Condor_Navicon_XYToLon = nil) then begin
      Condor_Navicon_Open := false;
    end;

    @Condor_Navicon_XYToLat := GetProcAddress(fHandle, 'XYToLat');
    if (@Condor_Navicon_XYToLat = nil) then begin
      Condor_Navicon_Open := false;
    end;

    // now open it (i.e. need to point to .trn file)
    Condor_Navicon_Open := Condor_Navicon_Init(Landscape);
  end else begin
    Condor_Navicon_Open := false;
  end;
end;

//-----------------------------------------------------------------------
function Condor_Navicon_Close : boolean;
begin
  if fHandle <> null then begin
    FreeLibrary(fHandle);
  end;
  Condor_Navicon_Close := true;
end;

//-----------------------------------------------------------------------
begin
end.

//--- End of File -------------------------------------------------------

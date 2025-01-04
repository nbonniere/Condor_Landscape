{
 * u_Exec.pas
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
Unit u_Exec;

//===========================================================================
INTERFACE

uses Windows;

//function WinExecAndWait32(FileName: string; Visibility: Integer): dWord;
function Shell_Execute(FilePath, FileName, Params : string;
                        Visibility: Boolean) : DWORD;

//===========================================================================
IMPLEMENTATION

uses
  SysUtils, ShellAPI, Forms;

//---------------------------------------------------------------------------
// for compile options
{$I Define.pas}

//-----------------------------------------------------------------------------
function WinExecAndWait32(FileName: string; Visibility: Integer): dWord;
var
  zAppName: array[0..512] of Char;
  zCurDir: array[0..255] of Char;
  WorkDir: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  StrPCopy(zAppName, FileName);
  GetDir(0, WorkDir);
  StrPCopy(zCurDir, WorkDir);
  FillChar(StartupInfo, Sizeof(StartupInfo), #0);
  StartupInfo.cb := Sizeof(StartupInfo);

  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  if not CreateProcess(nil,
           @zAppName, { pointer to command line string }
           nil, { pointer to process security attributes }
           nil, { pointer to thread security attributes }
           false, { handle inheritance flag }
           CREATE_NEW_CONSOLE or { creation flags }
           NORMAL_PRIORITY_CLASS,
           nil, { pointer to new environment block }
           nil, { pointer to current directory name }
           StartupInfo, { pointer to STARTUPINFO }
           ProcessInfo) then
    Result := 0{-1} { pointer to PROCESS_INF }
  else
  begin
{    WaitforSingleObject(ProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess, Result);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
}  end;
end;

//---------------------------------------------------------------------------
function Shell_Execute(FilePath, FileName, Params : string;
                        Visibility: Boolean) : DWORD;
var
  SEInfo: TShellExecuteInfo;
  ExitCode: DWORD;
  ExecuteFile: string;
//  StartInString: string;
begin
  ExecuteFile:=FilePath+'\'+FileName;
//  MessageShow('Executing batch file: '+FileName);

  FillChar(SEInfo, SizeOf(SEInfo), 0) ;
  SEInfo.cbSize := SizeOf(TShellExecuteInfo) ;
  with SEInfo do begin
    fMask := SEE_MASK_NOCLOSEPROCESS;
    Wnd := Application.Handle;
    lpFile := PChar(ExecuteFile) ;
    // Params can contain the application parameters
    if Params <> '' then begin
      lpParameters := PChar(Params) ;
    end;
    // StartInString specifies the name of the working directory
    // If ommited, the current directory is used
    //  lpDirectory := PChar(StartInString) ;
    if Visibility then begin
      nShow := SW_SHOWNORMAL;  // i.e. show the command window
    end else begin
      nShow := SW_HIDE;
    end;
  end;
  if ShellExecuteEx(@SEInfo) then begin
    repeat
      Application.ProcessMessages;
      GetExitCodeProcess(SEInfo.hProcess, ExitCode) ;
    until (ExitCode <> STILL_ACTIVE) or Application.Terminated;
  end else begin
    ExitCode := DWORD(-1);
  end;
  Result := ExitCode;
end;

//-----------------------------------------------------------------------------
end.

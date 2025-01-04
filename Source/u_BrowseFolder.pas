{
 * u_BrowseFolder.pas
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
unit u_BrowseFolder;

//===========================================================================
INTERFACE

//---------------------------------------------------------------------------
// for compile options
{$I Define.pas}

function BrowseForFolder(const browseTitle: String;
    const initialFolder: String = ''): String;

//===========================================================================
IMPLEMENTATION

uses
  Windows, shlobj;

var
  lg_StartFolder: String;

// Call back function used to set the initial browse directory.
//---------------------------------------------------------------------------
function BrowseForFolderCallBack(Wnd: HWND; uMsg: UINT;
           lParam, lpData: LPARAM): Integer stdcall;
begin
  if uMsg = BFFM_INITIALIZED then begin
    SendMessage(Wnd,BFFM_SETSELECTION, 1,
                Integer(@lg_StartFolder[1]));
    result := 0;
  end;	
end;
{
 This function allows the user to browse for a folder
 Arguments:-
  browseTitle   : The title to display on the browse dialog.
  initialFolder : Optional argument. Use to specify the folder initially
                  selected when the dialog opens.

 Returns: The empty string if no folder was selected (i.e. if the user
          clicked cancel), otherwise the full folder path.
}
//---------------------------------------------------------------------------
function BrowseForFolder(const browseTitle: String;
  const initialFolder: String =''): String;
  
var
  browse_info: TBrowseInfo;
  folder : array[0..MAX_PATH] of char;
  find_context: PItemIDList;

begin
  FillChar(browse_info,SizeOf(browse_info),#0);
  lg_StartFolder := initialFolder;
  browse_info.pszDisplayName := @folder[0];
  browse_info.lpszTitle := PChar(browseTitle);
  browse_info.ulFlags := BIF_RETURNONLYFSDIRS;
  browse_info.lpfn := BrowseForFolderCallBack;
  find_context := SHBrowseForFolder(browse_info);
  if (Assigned(find_context)) then begin
    if SHGetPathFromIDList(find_context,@folder) then begin
      result := folder
    end else begin
      result := '';
    end;  
  end else begin
    result := '';
  end;	
end;

//---------------------------------------------------------------------------
end.

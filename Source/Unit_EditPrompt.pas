{
 * Unit_Main.pas
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
unit Unit_EditPrompt;

//---------------------------------------------------------------------------
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm_EditPrompt = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button_OK: TButton;
    Button_Cancel: TButton;
    procedure Button_OKClick(Sender: TObject);
    procedure Button_CancelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form_EditPrompt: TForm_EditPrompt;
  pActionRequest : boolean;

//---------------------------------------------------------------------------
implementation

{$R *.DFM}

//---------------------------------------------------------------------------
procedure TForm_EditPrompt.Button_OKClick(Sender: TObject);
begin
  pActionRequest := true;
  Close;
end;

//---------------------------------------------------------------------------
procedure TForm_EditPrompt.Button_CancelClick(Sender: TObject);
begin
  pActionRequest := false;
  Close;
end;

//---------------------------------------------------------------------------
procedure TForm_EditPrompt.FormActivate(Sender: TObject);
begin
  pActionRequest := false;
end;

end.

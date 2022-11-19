{
 * u_MakeForest.pas
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
unit u_MakeForest;

//===========================================================================
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, {Dialogs,}
  StdCtrls;

type
  TForm_MakeForest = class(TForm)
    GroupBox_MakeForest: TGroupBox;
    CheckBox_Forest: TCheckBox;
    CheckBox_Deciduous: TCheckBox;
    CheckBox_Coniferous: TCheckBox;
    Button_Proceed: TButton;
    Button_Cancel: TButton;
    CheckBox_Changed: TCheckBox;
    CheckBox_Shrink: TCheckBox;
    procedure Button_CancelClick(Sender: TObject);
    procedure Button_ProceedClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form_MakeForest: TForm_MakeForest;
  ActionRequest : boolean;

//===========================================================================
implementation

{$R *.DFM}

//---------------------------------------------------------------------------
procedure TForm_MakeForest.FormActivate(Sender: TObject);
begin
  ActionRequest := false;
end;

//---------------------------------------------------------------------------
procedure TForm_MakeForest.Button_CancelClick(Sender: TObject);
begin
  ActionRequest := false;
  Close;
end;

//---------------------------------------------------------------------------
procedure TForm_MakeForest.Button_ProceedClick(Sender: TObject);
begin
  ActionRequest := true;
  Close;
end;

end.

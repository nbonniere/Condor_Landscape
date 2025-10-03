{
 * u-MakeGradient.pas
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
unit u_MakeGradient;

//===========================================================================
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Grids;

type
  TForm_Gradient = class(TForm)
    Image_Gradient: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    Button_Cancel: TButton;
    Button_Create: TButton;
    Label3: TLabel;
    DrawGrid1: TDrawGrid;
    ColorDialog1: TColorDialog;
    SaveDialog_File: TSaveDialog;
    procedure FormActivate(Sender: TObject);
    procedure Button_CancelClick(Sender: TObject);
    procedure Button_CreateClick(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure DrawGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DrawGradient(Sender: TObject);
  end;

var
  Form_Gradient: TForm_Gradient;
  gActionRequest : boolean;
  gFileName : string;
  GradientBitmap : TBitMap;
  GradientFolder : string;   // external path for file

//===========================================================================
implementation

{$R *.DFM}

uses u_BMP, {u_CalibExport,} Unit_EditPrompt, u_Util;

//---------------------------------------------------------------------------
procedure TForm_Gradient.DrawGradient(Sender: TObject);
var
  w, h : integer;
  i, j : integer;
  gColor : TColor;
  hMin, hMax : real;
  hInc : real;

begin
  hMin := GradientArray[0].fHeight;
  Label1.Caption := FloatToStr(hMin);
//  Shape1.Brush.Color := RGB(GradientArray[0].gColor[0],
//    GradientArray[0].gColor[1],GradientArray[0].gColor[2]);
  Shape1.Brush.Color := GradientArray[0].gColor.ColorValue;
  hMax := GradientArray[Levels-1].fHeight;
  Label2.Caption := FloatToStr(hMax);
//  Shape2.Brush.Color := RGB(GradientArray[Levels-1].gColor[0],
//    GradientArray[Levels-1].gColor[1],GradientArray[Levels-1].gColor[2]);
  Shape2.Brush.Color := GradientArray[Levels-1].gColor.ColorValue;
  Image_Gradient.Picture.Assign(GradientBitmap);
//  with Image_Gradient.Picture.Bitmap do begin
  with Image_Gradient do begin
    w := Picture.Bitmap.Width;
    h := Picture.Bitmap.Height;
    hInc := (hMax-hMin) / h;
    for i:=0 to h-1 do begin
      CalcGradientColor((hMax-i*hInc),gColor);
      Canvas.MoveTo(0,i);
      Canvas.Pen.Color:= gColor;
      Canvas.LineTo(w,i);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Gradient.FormActivate(Sender: TObject);
begin
  BMPfolder := GradientFolder;
  ReadGradientFile('HeightGradientColor.cl');
  gActionRequest := false;
  GradientBitmap := TBitmap.Create;
  with GradientBitmap do begin
    Height := Image_Gradient.Height;
    Width := Image_Gradient.Width;
  end;
  DrawGradient(Sender);
end;

//---------------------------------------------------------------------------
procedure TForm_Gradient.Button_CancelClick(Sender: TObject);
begin
  gActionRequest := false;
  WriteGradientFile('HeightGradientColor.cl');
  Close;
end;

//---------------------------------------------------------------------------
procedure TForm_Gradient.Button_CreateClick(Sender: TObject);
var
  exFileName : string;
  exInitialDir : string;
  exFileFilterString : string;
begin
  gActionRequest := false;
  exFileFilterString := ' (*.BMP)|*.BMP|All files (*.*)|*.*';
  exInitialDir := GradientFolder;
  exFileName := gFileName;
//  form_CalibExport.ShowModal;
  if SaveDialog(SaveDialog_File, exFileName, exInitialDir, exFileFilterString) then begin
    gFileName := exFileName;
    gActionRequest := true;
    WriteGradientFile('HeightGradientColor.cl');
    Close;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Gradient.DrawGrid1DrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
//  Ref_Rect_top := rect.top;
//  Ref_Rect_bottom := rect.bottom;
  with Sender as TDrawGrid do begin
    if (Arow = 0) then begin
      Canvas.Brush.Color := FixedColor;
      Canvas.FillRect(rect);
      case Acol of
        0: begin
          Canvas.TextOut(Rect.Left+1,Rect.Top+1,'Elevation');
        end;
        1: begin
          Canvas.TextOut(Rect.Left+1,Rect.Top+1,'Color');
        end;
      end;
    end else begin
      case Acol of
        0: begin
          Canvas.TextOut(Rect.Left+1,Rect.Top+1,
             FloatToStr(GradientArray[(Levels-1)-(Arow-1)].fHeight));
        end;
        1: begin
//          Canvas.Brush.Color := RGB(GradientArray[(Levels-1)-(Arow-1)].gcolor[0],
//                                    GradientArray[(Levels-1)-(Arow-1)].gcolor[1],
//                                    GradientArray[(Levels-1)-(Arow-1)].gcolor[2]);
          Canvas.Brush.Color := GradientArray[(Levels-1)-(Arow-1)].gcolor.ColorValue;
          Canvas.FillRect(rect);
        end;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TForm_Gradient.DrawGrid1SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  if ((ACol = 1) AND (ARow <> 0)) then begin
    ColorDialog1.Color := GradientArray[(Levels-1)-(Arow-1)].gcolor.ColorValue;
    if (ColorDialog1.Execute) then begin
      GradientArray[(Levels-1)-(Arow-1)].gcolor.ColorValue := ColorDialog1.Color;
      DrawGradient(Sender);
    end;
  end else begin
    if ((ACol = 0) AND (ARow <> 0)) then begin
      form_EditPrompt.Edit1.text := FloatToStr(GradientArray[(Levels-1)-(Arow-1)].fHeight);
      form_EditPrompt.ShowModal;
      if (pActionRequest) then begin
  // to do - check range is within heights above and below !!!
        GradientArray[(Levels-1)-(Arow-1)].fHeight := STRtoFloat(form_EditPrompt.Edit1.text);
        DrawGradient(Sender);
      end;
    end;
  end;
end;

end.

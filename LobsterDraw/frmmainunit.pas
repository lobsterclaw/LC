{
****************************************************************************
* Copyright 2014 Lobster Claw Software                                     *
*                                                                          *
* Licensed under the Apache License, Version 2.0 (the "License");          *
* you may not use this file except in compliance with the License.         *
* You may obtain a copy of the License at                                  *
*                                                                          *
*    http://www.apache.org/licenses/LICENSE-2.0                            *
*                                                                          *
* Unless required by applicable law or agreed to in writing, software      *
* distributed under the License is distributed on an "AS IS" BASIS,        *
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *
* See the License for the specific language governing permissions and      *
* limitations under the License.                                           *
*                                                                          *
****************************************************************************
}
unit frmmainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ExtCtrls, Buttons, ComCtrls, LCDrawPad;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    ActExit: TAction;
    ActFlipHorizontal: TAction;
    ActFlipVertical: TAction;
    ActRotate180: TAction;
    ActRotateClockwise: TAction;
    ActRotateCounterClockwise: TAction;
    ActResize: TAction;
    ActSave: TAction;
    ActSaveAs: TAction;
    ActNew: TAction;
    ActOpen: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    LCDrawPad1: TLCDrawPad;
    MainMenu1: TMainMenu;
    MnuFlipVertical: TMenuItem;
    MnuFlipHorizontal: TMenuItem;
    MnuRotate180: TMenuItem;
    MnuSpacer3: TMenuItem;
    MnuSpacer2: TMenuItem;
    MnuRotateClockwise: TMenuItem;
    MnuRotateCounterClockwise: TMenuItem;
    MnuResize: TMenuItem;
    MnuFile: TMenuItem;
    MnuSpacer1: TMenuItem;
    MnuEdit: TMenuItem;
    MnuSaveAs: TMenuItem;
    MnuExit: TMenuItem;
    MnuSave: TMenuItem;
    MnuNew: TMenuItem;
    MnuOpen: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ScrollBox1: TScrollBox;
    ToolBar1: TToolBar;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    procedure ActExitExecute(Sender: TObject);
    procedure ActFlipHorizontalExecute(Sender: TObject);
    procedure ActFlipVerticalExecute(Sender: TObject);
    procedure ActNewExecute(Sender: TObject);
    procedure ActOpenExecute(Sender: TObject);
    procedure ActResizeExecute(Sender: TObject);
    procedure ActRotate180Execute(Sender: TObject);
    procedure ActRotateClockwiseExecute(Sender: TObject);
    procedure ActRotateCounterClockwiseExecute(Sender: TObject);
    procedure ActSaveAsExecute(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

uses
  frmresizeunit;

{$R *.lfm}

{ TFrmMain }

procedure TFrmMain.ActExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFrmMain.ActFlipHorizontalExecute(Sender: TObject);
begin
  LCDrawPad1.Flip(fmHorizontal);
end;

procedure TFrmMain.ActFlipVerticalExecute(Sender: TObject);
begin
  LCDrawPad1.Flip(fmVertical);
end;

procedure TFrmMain.ActNewExecute(Sender: TObject);
begin
  LCDrawPad1.NewCanvas();
end;

procedure TFrmMain.ActOpenExecute(Sender: TObject);
begin
  try
    OpenDialog1.Execute;
    if (OpenDialog1.FileName <> '') And FileExistsUTF8(OpenDialog1.FileName) then
      LCDrawPad1.LoadFromFile(OpenDialog1.FileName);
  except
    ShowMessage('Error: Unable to load from file');
  end;
end;

procedure TFrmMain.ActResizeExecute(Sender: TObject);
begin
  FrmResize.EdtWidth.Value := LCDrawPad1.CanvasWidth;
  FrmResize.EdtHeight.Value := LCDrawPad1.CanvasHeight;
  if (FrmResize.ShowModal = mrOK) Then
  Begin
	  LCDrawPad1.ResizeCanvas(FrmResize.EdtWidth.Value, FrmResize.EdtHeight.Value);
  end;
end;

procedure TFrmMain.ActRotate180Execute(Sender: TObject);
begin
  LCDrawPad1.Rotate(rm180);
end;

procedure TFrmMain.ActRotateClockwiseExecute(Sender: TObject);
begin
  LCDrawPad1.Rotate(rm90Right);
end;

procedure TFrmMain.ActRotateCounterClockwiseExecute(Sender: TObject);
begin
  LCDrawPad1.Rotate(rm90Left);
end;

procedure TFrmMain.ActSaveAsExecute(Sender: TObject);
begin
  SaveDialog1.Options := SaveDialog1.Options + [ofOverwritePrompt];
  try
    SaveDialog1.Execute;
    if (SaveDialog1.FileName <> '') then
      LCDrawPad1.SaveToFile(SaveDialog1.FileName);
  except
    ShowMessage('Error: Unable to save to file');
  end;
end;

end.


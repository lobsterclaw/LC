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
    ActZoomOut: TAction;
    ActZoomIn: TAction;
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
    MnuZoomIn: TMenuItem;
    MnuZoomOut: TMenuItem;
    MnuView: TMenuItem;
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
    StatusBarMain: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
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
    procedure ActZoomInExecute(Sender: TObject);
    procedure ActZoomOutExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure SetZoomPercent(ZoomPercent: Integer);
  public
    { public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

uses
  frmresizeunit, LCLType, LCLProc;

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

procedure TFrmMain.ActZoomInExecute(Sender: TObject);
var
  NewZoom: Integer;
begin
  NewZoom := LCDrawPad1.ZoomPercent;
  If NewZoom = 1 Then
  	 NewZoom := 10
  Else If NewZoom < 100 Then
    NewZoom := NewZoom + 10
  Else If NewZoom < 200 Then
    NewZoom := NewZoom + 25
  Else If NewZoom < 500 Then
    NewZoom := NewZoom + 50
  Else
    NewZoom := NewZoom + 100;

  LCDrawPad1.ZoomPercent := NewZoom;
  SetZoomPercent(LCDrawPad1.ZoomPercent);
end;

procedure TFrmMain.ActZoomOutExecute(Sender: TObject);
var
  NewZoom: Integer;
begin
  NewZoom := LCDrawPad1.ZoomPercent;
  If NewZoom <= 10 Then
  	 NewZoom := 1
  Else If NewZoom <= 100 Then
    NewZoom := NewZoom - 10
  Else If NewZoom <= 200 Then
    NewZoom := NewZoom - 25
  Else If NewZoom <= 500 Then
    NewZoom := NewZoom - 50
  Else
    NewZoom := NewZoom - 100;

  LCDrawPad1.ZoomPercent := NewZoom;
  SetZoomPercent(LCDrawPad1.ZoomPercent);
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  SetZoomPercent(LCDrawPad1.ZoomPercent);

  {
	  There is a bug in the IDE that does not allow Ctr+- and Ctrl++ be displayed. This code
    addresses that by only applying the shortcut keys if the IDE has been patched or permentantly fixed
  }
  If ShortcutToText(KeyToShortCut(VK_SUBTRACT, [ssCtrl])) <> '' Then
  Begin
    //Ctrl+-
    ActZoomOut.ShortCut := KeyToShortCut(VK_SUBTRACT, [ssCtrl]);
    ActZoomOut.SecondaryShortCuts.AddObject('Ctrl+-', TObject(Pointer(PtrUInt(KeyToShortCut(VK_OEM_MINUS, [ssCtrl])))));
  End;

  If ShortcutToText(KeyToShortCut(VK_ADD, [ssCtrl])) <> '' Then
  Begin
    //Ctrl++
    ActZoomIn.ShortCut := KeyToShortCut(VK_ADD, [ssCtrl]);
    ActZoomIn.SecondaryShortCuts.AddObject('Ctrl++', TObject(Pointer(PtrUInt(KeyToShortCut(VK_OEM_PLUS, [ssCtrl])))));
  End;
end;

procedure TFrmMain.SetZoomPercent(ZoomPercent: Integer);
begin
  StatusBarMain.Panels[0].Text:= Format('Zoom %d%%     ', [ZoomPercent]);
end;

end.


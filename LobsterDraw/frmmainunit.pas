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
unit FrmMainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ExtCtrls, Buttons, ComCtrls, StdCtrls, ExtDlgs, LCDrawPad;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    ActExit: TAction;
    ActFlipHorizontal: TAction;
    ActFlipVertical: TAction;
    ActIncLineSize: TAction;
    ActDecLineSize: TAction;
    ActResizeImage: TAction;
    ActZoomOut: TAction;
    ActZoomIn: TAction;
    ActRotate180: TAction;
    ActRotateClockwise: TAction;
    ActRotateCounterClockwise: TAction;
    ActResizeCanvas: TAction;
    ActSave: TAction;
    ActSaveAs: TAction;
    ActNew: TAction;
    ActOpen: TAction;
    ActionList1: TActionList;
    BtnLineColor: TColorButton;
    EdtZoom: TEdit;
    ImageList1: TImageList;
    LCDrawPad1: TLCDrawPad;
    MainMenu1: TMainMenu;
    MnuResizeImage: TMenuItem;
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
    MnuResizeCanvas: TMenuItem;
    MnuFile: TMenuItem;
    MnuSpacer1: TMenuItem;
    MnuEdit: TMenuItem;
    MnuSaveAs: TMenuItem;
    MnuExit: TMenuItem;
    MnuSave: TMenuItem;
    MnuNew: TMenuItem;
    MnuOpen: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    SavePictureDialog1: TSavePictureDialog;
    ScrollBox1: TScrollBox;
    StatusBarMain: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    TbLineSize: TTrackBar;
    procedure ActDecLineSizeExecute(Sender: TObject);
    procedure ActExitExecute(Sender: TObject);
    procedure ActFlipHorizontalExecute(Sender: TObject);
    procedure ActFlipVerticalExecute(Sender: TObject);
    procedure ActIncLineSizeExecute(Sender: TObject);
    procedure ActNewExecute(Sender: TObject);
    procedure ActOpenExecute(Sender: TObject);
    procedure ActResizeCanvasExecute(Sender: TObject);
    procedure ActResizeImageExecute(Sender: TObject);
    procedure ActRotate180Execute(Sender: TObject);
    procedure ActRotateClockwiseExecute(Sender: TObject);
    procedure ActRotateCounterClockwiseExecute(Sender: TObject);
    procedure ActSaveAsExecute(Sender: TObject);
    procedure ActSaveExecute(Sender: TObject);
    procedure ActZoomInExecute(Sender: TObject);
    procedure ActZoomOutExecute(Sender: TObject);
    procedure BtnLineColorColorChanged(Sender: TObject);
    procedure EdtZoomChange(Sender: TObject);
    procedure EdtZoomKeyPress(Sender: TObject; var Key: char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure LCDrawPad1ImageChange(Sender: TObject);
    procedure StatusBarMainResize(Sender: TObject);
    procedure TbLineSizeChange(Sender: TObject);
  private
    { private declarations }
    fLastFileName: String;
    function GetDocumentTitle(): String;
    procedure UpdateCaption();
    procedure ResetControls();
    procedure UpdateZoomPercent(ZoomPercent: Integer);
    procedure UpdateLineSize(LineSize: Byte);
    function CheckNeedSave(): Boolean;
  public
    { public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

uses
  frmresizeunit, frmresampleunit, LCLType, LCLProc;

{$R *.lfm}

{ TFrmMain }

procedure TFrmMain.ActExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFrmMain.ActDecLineSizeExecute(Sender: TObject);
begin
  TbLineSize.Position := TbLineSize.Position - 1;
end;

procedure TFrmMain.ActFlipHorizontalExecute(Sender: TObject);
begin
  LCDrawPad1.Flip(fmHorizontal);
end;

procedure TFrmMain.ActFlipVerticalExecute(Sender: TObject);
begin
  LCDrawPad1.Flip(fmVertical);
end;

procedure TFrmMain.ActIncLineSizeExecute(Sender: TObject);
begin
  TbLineSize.Position := TbLineSize.Position + 1;
end;

procedure TFrmMain.ActNewExecute(Sender: TObject);
begin
  If CheckNeedSave() Then
  Begin
    LCDrawPad1.NewCanvas();
    fLastFileName := '';
    UpdateCaption;
    ResetControls;
  End;
end;

procedure TFrmMain.ActOpenExecute(Sender: TObject);
begin
  If CheckNeedSave() Then
  Begin
    if OpenPictureDialog1.Execute And (OpenPictureDialog1.FileName <> '') And FileExistsUTF8(OpenPictureDialog1.FileName) then
    Begin
      try
        LCDrawPad1.LoadFromFile(OpenPictureDialog1.FileName);
        fLastFileName := OpenPictureDialog1.FileName;
        UpdateCaption;
        ResetControls;
      except
        ShowMessage('Error: Unable to load from file ' + ExtractFileName(OpenPictureDialog1.FileName));
      end;
    end;
  End;
end;

procedure TFrmMain.ActResizeCanvasExecute(Sender: TObject);
begin
  FrmResize.EdtWidth.Value := LCDrawPad1.CanvasWidth;
  FrmResize.EdtHeight.Value := LCDrawPad1.CanvasHeight;
  if (FrmResize.ShowModal = mrOK) Then
  Begin
	  LCDrawPad1.ResizeCanvas(FrmResize.EdtWidth.Value, FrmResize.EdtHeight.Value, FrmResize.GetSelectedAnchor());
  end;
end;

procedure TFrmMain.ActResizeImageExecute(Sender: TObject);
begin
  FrmResample.RbPixels.Checked := True;
  FrmResample.EdtWidth.Value := LCDrawPad1.CanvasWidth;
  FrmResample.EdtHeight.Value := LCDrawPad1.CanvasHeight;
  if (FrmResample.ShowModal = mrOK) Then
  Begin
	  LCDrawPad1.ResizeImage(FrmResample.GetWidthAsValue, FrmResample.GetHeightAsValue);
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
var
  FileName: String;
begin
  If (fLastFileName <> '') And FileExistsUTF8(fLastFileName) Then
  Begin
    SavePictureDialog1.InitialDir := ExtractFileDir(fLastFileName);
    SavePictureDialog1.FileName := ExtractFileNameOnly(fLastFileName);
  end;

  SavePictureDialog1.Options := SavePictureDialog1.Options + [ofOverwritePrompt];
  try
    if SavePictureDialog1.Execute And (SavePictureDialog1.FileName <> '') then
    Begin
      FileName := SavePictureDialog1.FileName;
      If ExtractFileExt(FileName) = '' Then //just in case
        FileName := FileName + '.png';
      LCDrawPad1.SaveToFile(FileName);
      fLastFileName := FileName;
      ActSave.Enabled := False;
      UpdateCaption;
    End;
  except
    ShowMessage('Error: Unable to save to file');
  end;
end;

procedure TFrmMain.ActSaveExecute(Sender: TObject);
begin
  If (fLastFileName <> '') And FileExistsUTF8(fLastFileName) Then
  Begin
    LCDrawPad1.SaveToFile(fLastFileName);
    ActSave.Enabled := False;
    UpdateCaption();
  end
  Else
    ActSaveAsExecute(Sender);
end;

function IncRoundUp(Num, Factor: Integer): Integer;
var
  ModValue: Integer;
Begin
  ModValue := Num Mod Factor;
  If ModValue = 0 Then
		Result := Num + Factor
  Else
    Result := ((Num Div Factor) + 1) * Factor;
End;

function IncRoundDown(Num, Factor: Integer): Integer;
var
  ModValue: Integer;
Begin
  ModValue := Num Mod Factor;
  If ModValue = 0 Then
		Result := Num - Factor
  Else
    Result := (Num Div Factor) * Factor;
End;

procedure TFrmMain.ActZoomInExecute(Sender: TObject);
var
  NewZoom: Integer;
begin
  //TODO round numbers up
  NewZoom := LCDrawPad1.ZoomPercent;
  If NewZoom = 1 Then
  	 NewZoom := 10
  Else If NewZoom < 100 Then
    NewZoom := IncRoundUp(NewZoom, 10)
  Else If NewZoom < 200 Then
    NewZoom := IncRoundUp(NewZoom, 25)
  Else If NewZoom < 500 Then
    NewZoom := IncRoundUp(NewZoom, 50)
  Else
    NewZoom := IncRoundUp(NewZoom, 100);

  LCDrawPad1.ZoomPercent := NewZoom;
  UpdateZoomPercent(LCDrawPad1.ZoomPercent);
end;

procedure TFrmMain.ActZoomOutExecute(Sender: TObject);
var
  NewZoom: Integer;
begin
  //TODO round numbers down
  NewZoom := LCDrawPad1.ZoomPercent;
  If NewZoom <= 10 Then
  	 NewZoom := 1
  Else If NewZoom <= 100 Then
    NewZoom := IncRoundDown(NewZoom, 10)
  Else If NewZoom <= 200 Then
    NewZoom := IncRoundDown(NewZoom, 25)
  Else If NewZoom <= 500 Then
    NewZoom := IncRoundDown(NewZoom, 50)
  Else
    NewZoom := IncRoundDown(NewZoom, 100);

  LCDrawPad1.ZoomPercent := NewZoom;
  UpdateZoomPercent(LCDrawPad1.ZoomPercent);
end;

procedure TFrmMain.BtnLineColorColorChanged(Sender: TObject);
begin
  LCDrawPad1.LineColor := BtnLineColor.ButtonColor;
end;

procedure TFrmMain.EdtZoomChange(Sender: TObject);
var
  NewZoom: Integer;
begin
  If TryStrToInt(EdtZoom.Text, NewZoom) Then
  Begin
    If LCDrawPad1.ZoomPercent <> NewZoom Then
    Begin
      LCDrawPad1.ZoomPercent := NewZoom;
      UpdateZoomPercent(LCDrawPad1.ZoomPercent);
    End;
  End;
end;

procedure TFrmMain.EdtZoomKeyPress(Sender: TObject; var Key: char);
begin
  if (Not (Key in ['0'..'9',Char(VK_BACK),Char(VK_DELETE),Char(VK_TAB),Char(VK_RETURN)])) Then
  	 Key := #0;
end;

procedure TFrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CheckNeedSave();
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  fLastFileName := '';
  ActSave.Enabled := False;

  UpdateCaption;

  UpdateZoomPercent(LCDrawPad1.ZoomPercent);
  UpdateLineSize(LCDrawPad1.LineSize);
  BtnLineColor.ButtonColor := LCDrawPad1.LineColor;
  TbLineSize.Position := LCDrawPad1.LineSize;

  {
	  There is a bug in the LCL that does not allow Ctr+- and Ctrl++ be displayed. This code
    addresses that by only applying the shortcut keys if the LCL has been patched or permentantly fixed
  }

  //Ctrl+-
  If ShortcutToText(KeyToShortCut(VK_SUBTRACT, [ssCtrl])) <> '' Then
  Begin
    ActZoomOut.ShortCut := KeyToShortCut(VK_SUBTRACT, [ssCtrl]);
    ActZoomOut.SecondaryShortCuts.AddObject('Ctrl+-', TObject({%H-}Pointer(PtrUInt(KeyToShortCut(VK_OEM_MINUS, [ssCtrl])))));
  End;

  //Ctrl++
  If ShortcutToText(KeyToShortCut(VK_ADD, [ssCtrl])) <> '' Then
  Begin
    ActZoomIn.ShortCut := KeyToShortCut(VK_ADD, [ssCtrl]);
    ActZoomIn.SecondaryShortCuts.AddObject('Ctrl++', TObject({%H-}Pointer(PtrUInt(KeyToShortCut(VK_OEM_PLUS, [ssCtrl])))));
  End;

end;

procedure TFrmMain.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
  If CheckNeedSave() Then
  Begin
    if (Length(FileNames) > 0) And FileExistsUTF8(FileNames[0]) then
    Begin
      try
        LCDrawPad1.LoadFromFile(FileNames[0]);
        fLastFileName := FileNames[0];
        UpdateCaption;
        ResetControls;
      except
        ShowMessage('Error: Unable to load from file ' + ExtractFileName(FileNames[0]));
      end;
    end;
  End;
end;

procedure TFrmMain.FormKeyPress(Sender: TObject; var Key: char);
begin
  If Key = '[' Then
    TbLineSize.Position := TbLineSize.Position - 1
  Else If Key = ']' Then
    TbLineSize.Position := TbLineSize.Position + 1;
end;

procedure TFrmMain.LCDrawPad1ImageChange(Sender: TObject);
begin
  ActSave.Enabled := Not LCDrawPad1.IsFreshImage;
  UpdateCaption;
end;

procedure TFrmMain.StatusBarMainResize(Sender: TObject);
begin
  StatusBarMain.Panels[0].Width := StatusBarMain.Width - StatusBarMain.Panels[1].Width - StatusBarMain.Panels[2].Width
      - StatusBarMain.Panels[3].Width;
end;

procedure TFrmMain.TbLineSizeChange(Sender: TObject);
begin
  LCDrawPad1.LineSize := TbLineSize.Position;
  UpdateLineSize(LCDrawPad1.LineSize);
end;


function TFrmMain.GetDocumentTitle(): String;
begin
  If fLastFileName = '' Then
    Result := 'Untitled'
  Else
    Result := ExtractFileName(fLastFileName);
end;

procedure TFrmMain.UpdateCaption;
var
  Indicator: String = '';
  NewCaption: String;
begin
  If ActSave.Enabled Then Indicator := '*';
  NewCaption := GetDocumentTitle() + Indicator + ' - Lobster Draw';
  If Self.Caption <> NewCaption Then Self.Caption := NewCaption;
end;

procedure TFrmMain.ResetControls;
begin
  FrmResize.Reset();
  LCDrawPad1.ZoomPercent := 100;
  UpdateZoomPercent(LCDrawPad1.ZoomPercent);
end;

procedure TFrmMain.UpdateZoomPercent(ZoomPercent: Integer);
begin
  StatusBarMain.Panels[2].Text:= Format('Zoom %d%%', [ZoomPercent]);
  EdtZoom.Text := IntToStr(ZoomPercent);
end;

procedure TFrmMain.UpdateLineSize(LineSize: Byte);
begin
  StatusBarMain.Panels[1].Text:= Format('Line Size %dpx', [LineSize]);
end;

function TFrmMain.CheckNeedSave(): Boolean;
var
  Reply, BoxStyle: Integer;
begin
  Result := True;
  If ActSave.Enabled Then
  Begin
    BoxStyle := MB_ICONQUESTION + MB_YESNOCANCEL;
    Reply := Application.MessageBox('Do you want to save changes before continuing?', 'Save Drawing?', BoxStyle);

    If Reply = IDYES Then
      ActSaveExecute(nil);

    Result := (Reply = IDYES) Or (Reply = IDNO);
  end;
end;

end.


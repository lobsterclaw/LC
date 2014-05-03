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
unit FrmResizeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls, Buttons, LCDrawPad;

type

  { TFrmResize }

  TFrmResize = class(TForm)
    BtnOkay: TButton;
    BtnCancel: TButton;
    CbConstrain: TCheckBox;
    EdtHeight: TFloatSpinEdit;
    EdtWidth: TFloatSpinEdit;
    GrpResize: TGroupBox;
    LblEdit1: TLabel;
    LblEdit2: TLabel;
    LblHeight: TLabel;
    LblAnchor: TLabel;
    LblWidth: TLabel;
    PnlAnchors: TPanel;
    BtnUpperLeft: TSpeedButton;
    BtnUpperCenter: TSpeedButton;
    BtnUpperRight: TSpeedButton;
    BtnMiddleLeft: TSpeedButton;
    BtnMiddleCenter: TSpeedButton;
    BtnMiddleRight: TSpeedButton;
    BtnLowerLeft: TSpeedButton;
    BtnLowerCenter: TSpeedButton;
    BtnLowerRight: TSpeedButton;
    RbPercent: TRadioButton;
    RbPixels: TRadioButton;
    procedure BtnLowerCenterPaint(Sender: TObject);
    procedure BtnLowerLeftPaint(Sender: TObject);
    procedure BtnLowerRightPaint(Sender: TObject);
    procedure BtnMiddleCenterPaint(Sender: TObject);
    procedure BtnMiddleLeftPaint(Sender: TObject);
    procedure BtnMiddleRightPaint(Sender: TObject);
    procedure BtnUpperCenterPaint(Sender: TObject);
    procedure BtnUpperRightPaint(Sender: TObject);
    procedure CbConstrainChange(Sender: TObject);
    procedure EdtHeightChange(Sender: TObject);
    procedure EdtWidthChange(Sender: TObject);
    procedure RbPercentChange(Sender: TObject);
    procedure RbPixelsChange(Sender: TObject);
    procedure SpeedButtonClick(Sender: TObject);
    procedure BtnUpperLeftPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    fOrigWidth, fOrigHeight: Integer;
    fIsEditing: Boolean;
    fAspectRatio: Double;
    fPreviousPercent, fPreviousPixels: Boolean;
    procedure DrawArrow(BeginPoint, EndPoint: TPoint; ACanvas: TCanvas);
    procedure DrawCircle(Rect: TRect; ACanvas: TCanvas); overload;
    procedure DrawCircle(SpeedButton: TSpeedButton); overload;
    procedure DrawArrowN(SpeedButton: TSpeedButton);
    procedure DrawArrowNE(SpeedButton: TSpeedButton);
    procedure DrawArrowE(SpeedButton: TSpeedButton);
    procedure DrawArrowSE(SpeedButton: TSpeedButton);
    procedure DrawArrowS(SpeedButton: TSpeedButton);
    procedure DrawArrowSW(SpeedButton: TSpeedButton);
    procedure DrawArrowW(SpeedButton: TSpeedButton);
    procedure DrawArrowNW(SpeedButton: TSpeedButton);
  public
    { public declarations }
    function GetSelectedAnchor(): TCanvasPosition;
    procedure Reset();
    function GetWidthAsValue: Integer;
    function GetHeightAsValue: Integer;
  end;

var
  FrmResize: TFrmResize;

implementation

uses
  Math;

{$R *.lfm}

{ TFrmResize }

procedure TFrmResize.FormCreate(Sender: TObject);
begin
  fOrigWidth := 640;
  fOrigHeight := 480;
  fIsEditing := False;
  fAspectRatio := 1.0;
end;

procedure TFrmResize.FormShow(Sender: TObject);
begin
  fIsEditing := True;
  fOrigWidth := Round(EdtWidth.Value);
  fOrigHeight := Round(EdtHeight.Value);
  fAspectRatio := EdtWidth.Value / EdtHeight.Value;
  fIsEditing := False;
end;

procedure TFrmResize.RbPercentChange(Sender: TObject);
begin
  if (fPreviousPercent = RbPercent.Checked) or fIsEditing then Exit;
  fIsEditing := True;

  if RbPercent.Checked then
  begin
    EdtWidth.DecimalPlaces := 1;
    EdtHeight.DecimalPlaces := 1;
    EdtWidth.Value := Round((EdtWidth.Value / fOrigWidth) * 1000) / 10.0;
    EdtHeight.Value := Round((EdtHeight.Value / fOrigHeight) * 1000) / 10.0;
    EdtWidth.MaxValue := Round(4000 / fOrigWidth * 100);
    EdtHeight.MaxValue := Round(4000 / fOrigHeight * 100);
    LblEdit1.Caption := '%';
    LblEdit2.Caption := '%';
  end;

  fPreviousPercent := RbPercent.Checked;
  fIsEditing := False;
end;

procedure TFrmResize.RbPixelsChange(Sender: TObject);
begin
  if (fPreviousPixels = RbPixels.Checked) or fIsEditing then Exit;
  fIsEditing := True;

  if RbPixels.Checked then
  begin
    EdtWidth.MaxValue := 4000;
    EdtHeight.MaxValue := 4000;
    EdtWidth.Value := Round((EdtWidth.Value * fOrigWidth) / 100);
    EdtHeight.Value := Round((EdtHeight.Value * fOrigHeight) / 100);
    EdtWidth.DecimalPlaces := 0;
    EdtHeight.DecimalPlaces := 0;
    LblEdit1.Caption := 'pixels';
    LblEdit2.Caption := 'pixels';
  end;

  fPreviousPixels := RbPixels.Checked;
  fIsEditing := False;
end;

function TFrmResize.GetWidthAsValue: Integer;
begin
  if RbPixels.Checked then
    Result := Round(EdtWidth.Value)
  else
    Result := Round(fOrigWidth * (EdtWidth.Value / 100));
end;

function TFrmResize.GetHeightAsValue: Integer;
begin
  if RbPixels.Checked then
    Result := Round(EdtHeight.Value)
  else
    Result := Round(fOrigHeight * (EdtHeight.Value / 100));
end;

procedure TFrmResize.EdtWidthChange(Sender: TObject);
begin
  if fIsEditing then Exit;
  fIsEditing := True;

  if CbConstrain.Checked then
    if RbPixels.Checked then  //pixels
      EdtHeight.Value := Round(EdtWidth.Value / fAspectRatio)
    else //percent
      EdtHeight.Value := EdtWidth.Value;

  PnlAnchors.Invalidate;

  fIsEditing := False;
end;

procedure TFrmResize.EdtHeightChange(Sender: TObject);
begin
  if fIsEditing then Exit;
  fIsEditing := True;

  if CbConstrain.Checked then
    if RbPixels.Checked then  //pixels
      EdtWidth.Value := Round(EdtHeight.Value * fAspectRatio)
    else //percent
      EdtWidth.Value := EdtHeight.Value;

  PnlAnchors.Invalidate;

  fIsEditing := False;
end;

procedure TFrmResize.CbConstrainChange(Sender: TObject);
begin
  if CbConstrain.Checked then
  begin
    fIsEditing := True;

    if RbPixels.Checked then  //pixels
      EdtHeight.Value := Round(EdtWidth.Value / fAspectRatio)
    else //percent
      EdtHeight.Value := EdtWidth.Value;

    PnlAnchors.Invalidate;

    fIsEditing := False;
  end;
end;

procedure TFrmResize.SpeedButtonClick(Sender: TObject);
begin
  PnlAnchors.Invalidate;
end;

procedure TFrmResize.BtnUpperLeftPaint(Sender: TObject);
var
  SpeedButton: TSpeedButton;
begin
  SpeedButton := Sender As TSpeedButton;
  if BtnUpperLeft.Down Then
    DrawCircle(SpeedButton)
  Else if BtnUpperCenter.Down And (GetWidthAsValue >= fOrigWidth) Then
    DrawArrowW(SpeedButton)
  Else if BtnUpperCenter.Down Then
    DrawArrowE(SpeedButton)
  Else if BtnMiddleCenter.Down And (GetWidthAsValue >= fOrigWidth) And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowNW(SpeedButton)
  Else if BtnMiddleCenter.Down Then
    DrawArrowSE(SpeedButton)
  Else if BtnMiddleLeft.Down And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowN(SpeedButton)
  Else if BtnMiddleLeft.Down Then
    DrawArrowS(SpeedButton);

end;

procedure TFrmResize.BtnUpperCenterPaint(Sender: TObject);
var
  SpeedButton: TSpeedButton;
begin
  SpeedButton := Sender As TSpeedButton;
  if BtnUpperCenter.Down Then
    DrawCircle(SpeedButton)
  Else if BtnUpperRight.Down And (GetWidthAsValue >= fOrigWidth) Then
    DrawArrowW(SpeedButton)
  Else if BtnUpperRight.Down Then
    DrawArrowE(SpeedButton)
  Else if BtnMiddleRight.Down And (GetWidthAsValue >= fOrigWidth) And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowNW(SpeedButton)
  Else if BtnMiddleRight.Down Then
    DrawArrowSE(SpeedButton)
  Else if BtnMiddleCenter.Down And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowN(SpeedButton)
  Else if BtnMiddleCenter.Down Then
    DrawArrowS(SpeedButton)
  Else if BtnMiddleLeft.Down And (GetWidthAsValue >= fOrigWidth) And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowNE(SpeedButton)
  Else if BtnMiddleLeft.Down Then
    DrawArrowSW(SpeedButton)
  Else if BtnUpperLeft.Down And (GetWidthAsValue >= fOrigWidth) Then
    DrawArrowE(SpeedButton)
  Else if BtnUpperLeft.Down Then
    DrawArrowW(SpeedButton);

end;

procedure TFrmResize.BtnUpperRightPaint(Sender: TObject);
var
  SpeedButton: TSpeedButton;
begin
  SpeedButton := Sender As TSpeedButton;
  if BtnUpperRight.Down Then
    DrawCircle(SpeedButton)
  Else if BtnMiddleRight.Down And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowN(SpeedButton)
  Else if BtnMiddleRight.Down Then
    DrawArrowS(SpeedButton)
  Else if BtnMiddleCenter.Down And (GetWidthAsValue >= fOrigWidth) And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowNE(SpeedButton)
  Else if BtnMiddleCenter.Down Then
    DrawArrowSW(SpeedButton)
  Else if BtnUpperCenter.Down And (GetWidthAsValue >= fOrigWidth) Then
    DrawArrowE(SpeedButton)
  Else if BtnUpperCenter.Down Then
    DrawArrowW(SpeedButton);

end;

procedure TFrmResize.BtnMiddleLeftPaint(Sender: TObject);
var
  SpeedButton: TSpeedButton;
begin
  SpeedButton := Sender As TSpeedButton;
  if BtnMiddleLeft.Down Then
    DrawCircle(SpeedButton)
  Else if BtnUpperLeft.Down And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowS(SpeedButton)
  Else if BtnUpperLeft.Down Then
    DrawArrowN(SpeedButton)
  Else if BtnUpperCenter.Down And (GetWidthAsValue >= fOrigWidth) And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowSW(SpeedButton)
  Else if BtnUpperCenter.Down Then
    DrawArrowNE(SpeedButton)
  Else if BtnMiddleCenter.Down And (GetWidthAsValue >= fOrigWidth) Then
    DrawArrowW(SpeedButton)
  Else if BtnMiddleCenter.Down Then
    DrawArrowE(SpeedButton)
  Else if BtnLowerCenter.Down And (GetWidthAsValue >= fOrigWidth) And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowNW(SpeedButton)
  Else if BtnLowerCenter.Down Then
    DrawArrowSE(SpeedButton)
  Else if BtnLowerLeft.Down And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowN(SpeedButton)
  Else if BtnLowerLeft.Down Then
    DrawArrowS(SpeedButton);

end;

procedure TFrmResize.BtnMiddleCenterPaint(Sender: TObject);
var
  SpeedButton: TSpeedButton;
begin
  SpeedButton := Sender As TSpeedButton;
  if BtnMiddleCenter.Down Then
    DrawCircle(SpeedButton)
  Else if BtnUpperCenter.Down And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowS(SpeedButton)
  Else if BtnUpperCenter.Down Then
    DrawArrowN(SpeedButton)
  Else if BtnUpperRight.Down And (GetWidthAsValue >= fOrigWidth) And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowSW(SpeedButton)
  Else if BtnUpperRight.Down Then
    DrawArrowNE(SpeedButton)
  Else if BtnMiddleRight.Down And (GetWidthAsValue >= fOrigWidth) Then
    DrawArrowW(SpeedButton)
  Else if BtnMiddleRight.Down Then
    DrawArrowE(SpeedButton)
  Else if BtnLowerRight.Down And (GetWidthAsValue >= fOrigWidth) And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowNW(SpeedButton)
  Else if BtnLowerRight.Down Then
    DrawArrowSE(SpeedButton)
  Else if BtnLowerCenter.Down And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowN(SpeedButton)
  Else if BtnLowerCenter.Down Then
    DrawArrowS(SpeedButton)
  Else if BtnLowerLeft.Down And (GetWidthAsValue >= fOrigWidth) And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowNE(SpeedButton)
  Else if BtnLowerLeft.Down Then
    DrawArrowSW(SpeedButton)
  Else if BtnMiddleLeft.Down And (GetWidthAsValue >= fOrigWidth) Then
    DrawArrowE(SpeedButton)
  Else if BtnMiddleLeft.Down Then
    DrawArrowW(SpeedButton)
  Else if BtnUpperLeft.Down And (GetWidthAsValue >= fOrigWidth) And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowSE(SpeedButton)
  Else if BtnUpperLeft.Down Then
    DrawArrowNW(SpeedButton);
end;

procedure TFrmResize.BtnMiddleRightPaint(Sender: TObject);
var
  SpeedButton: TSpeedButton;
begin
  SpeedButton := Sender As TSpeedButton;
  if BtnMiddleRight.Down Then
    DrawCircle(SpeedButton)
  Else if BtnUpperRight.Down And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowS(SpeedButton)
  Else if BtnUpperRight.Down Then
    DrawArrowN(SpeedButton)
  Else if BtnLowerRight.Down And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowN(SpeedButton)
  Else if BtnLowerRight.Down Then
    DrawArrowS(SpeedButton)
  Else if BtnLowerCenter.Down And (GetWidthAsValue >= fOrigWidth) And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowNE(SpeedButton)
  Else if BtnLowerCenter.Down Then
    DrawArrowSW(SpeedButton)
  Else if BtnMiddleCenter.Down And (GetWidthAsValue >= fOrigWidth) Then
    DrawArrowE(SpeedButton)
  Else if BtnMiddleCenter.Down Then
    DrawArrowW(SpeedButton)
  Else if BtnUpperCenter.Down And (GetWidthAsValue >= fOrigWidth) And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowSE(SpeedButton)
  Else if BtnUpperCenter.Down Then
    DrawArrowNW(SpeedButton);
end;

procedure TFrmResize.BtnLowerLeftPaint(Sender: TObject);
var
  SpeedButton: TSpeedButton;
begin
  SpeedButton := Sender As TSpeedButton;
  if BtnLowerLeft.Down Then
    DrawCircle(SpeedButton)
  Else if BtnMiddleLeft.Down And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowS(SpeedButton)
  Else if BtnMiddleLeft.Down Then
    DrawArrowN(SpeedButton)
  Else if BtnMiddleCenter.Down And (GetWidthAsValue >= fOrigWidth) And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowSW(SpeedButton)
  Else if BtnMiddleCenter.Down Then
    DrawArrowNE(SpeedButton)
  Else if BtnLowerCenter.Down And (GetWidthAsValue >= fOrigWidth) Then
    DrawArrowW(SpeedButton)
  Else if BtnLowerCenter.Down Then
    DrawArrowE(SpeedButton);
end;

procedure TFrmResize.BtnLowerCenterPaint(Sender: TObject);
var
  SpeedButton: TSpeedButton;
begin
  SpeedButton := Sender As TSpeedButton;
  if BtnLowerCenter.Down Then
    DrawCircle(SpeedButton)
  Else if BtnMiddleCenter.Down And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowS(SpeedButton)
  Else if BtnMiddleCenter.Down Then
    DrawArrowN(SpeedButton)
  Else if BtnMiddleRight.Down And (GetWidthAsValue >= fOrigWidth) And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowSW(SpeedButton)
  Else if BtnMiddleRight.Down Then
    DrawArrowNE(SpeedButton)
  Else if BtnLowerRight.Down And (GetWidthAsValue >= fOrigWidth) Then
    DrawArrowW(SpeedButton)
  Else if BtnLowerRight.Down Then
    DrawArrowE(SpeedButton)
  Else if BtnLowerLeft.Down And (GetWidthAsValue >= fOrigWidth) Then
    DrawArrowE(SpeedButton)
  Else if BtnLowerLeft.Down Then
    DrawArrowW(SpeedButton)
  Else if BtnMiddleLeft.Down And (GetWidthAsValue >= fOrigWidth) And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowSE(SpeedButton)
  Else if BtnMiddleLeft.Down Then
    DrawArrowNW(SpeedButton);
end;

procedure TFrmResize.BtnLowerRightPaint(Sender: TObject);
var
  SpeedButton: TSpeedButton;
begin
  SpeedButton := Sender As TSpeedButton;
  if BtnLowerRight.Down Then
    DrawCircle(SpeedButton)
  Else if BtnMiddleRight.Down And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowS(SpeedButton)
  Else if BtnMiddleRight.Down Then
    DrawArrowN(SpeedButton)
  Else if BtnLowerCenter.Down And (GetWidthAsValue >= fOrigWidth) Then
    DrawArrowE(SpeedButton)
  Else if BtnLowerCenter.Down Then
    DrawArrowW(SpeedButton)
  Else if BtnMiddleCenter.Down And (GetWidthAsValue >= fOrigWidth) And (GetHeightAsValue >= fOrigHeight) Then
    DrawArrowSE(SpeedButton)
  Else if BtnMiddleCenter.Down Then
    DrawArrowNW(SpeedButton);
end;

procedure TFrmResize.DrawArrow(BeginPoint, EndPoint: TPoint; ACanvas: TCanvas);
const
  HeadLen = 10;
var
  Angle: Extended;
begin
  Angle := arctan2(EndPoint.Y - BeginPoint.Y, EndPoint.X - BeginPoint.X);

  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Width := 1;

  ACanvas.PenPos := BeginPoint;
  ACanvas.LineTo(EndPoint.X, EndPoint.Y);
  ACanvas.LineTo(EndPoint.X - Round(HeadLen * cos(Angle - (PI / 6))), EndPoint.Y - Round(HeadLen * sin(Angle - (PI / 6))));
  ACanvas.PenPos := EndPoint;
  ACanvas.LineTo(EndPoint.X - Round(HeadLen * cos(Angle + (PI / 6))), EndPoint.Y - Round(HeadLen * sin(Angle + (PI / 6))));
end;

procedure TFrmResize.DrawCircle(Rect: TRect; ACanvas: TCanvas);
begin
  ACanvas.Brush.Color := clBlack;
  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Width := 3;
  ACanvas.Ellipse(Rect);
end;

procedure TFrmResize.DrawCircle(SpeedButton: TSpeedButton);
begin
  DrawCircle(Rect(12, 12, SpeedButton.Width - 12, SpeedButton.Height - 12), SpeedButton.Canvas);
end;

procedure TFrmResize.DrawArrowN(SpeedButton: TSpeedButton);
begin
  DrawArrow(Point(SpeedButton.Width div 2, SpeedButton.Height - 6), Point(SpeedButton.Width div 2, 6), SpeedButton.Canvas);
end;

procedure TFrmResize.DrawArrowNE(SpeedButton: TSpeedButton);
begin
  DrawArrow(Point(6, SpeedButton.Height - 6), Point(SpeedButton.Width - 6, 6), SpeedButton.Canvas);
end;

procedure TFrmResize.DrawArrowE(SpeedButton: TSpeedButton);
begin
  DrawArrow(Point(6, SpeedButton.Height div 2), Point(SpeedButton.Width - 6, SpeedButton.Height div 2), SpeedButton.Canvas);
end;

procedure TFrmResize.DrawArrowSE(SpeedButton: TSpeedButton);
begin
  DrawArrow(Point(6, 6), Point(SpeedButton.Width - 6, SpeedButton.Height - 6), SpeedButton.Canvas);
end;

procedure TFrmResize.DrawArrowS(SpeedButton: TSpeedButton);
begin
  DrawArrow(Point(SpeedButton.Width div 2, 6), Point(SpeedButton.Width div 2, SpeedButton.Height - 6), SpeedButton.Canvas);
end;

procedure TFrmResize.DrawArrowSW(SpeedButton: TSpeedButton);
begin
  DrawArrow(Point(SpeedButton.Width - 6, 6), Point(6, SpeedButton.Height - 6), SpeedButton.Canvas);
end;

procedure TFrmResize.DrawArrowW(SpeedButton: TSpeedButton);
begin
  DrawArrow(Point(SpeedButton.Width - 6, SpeedButton.Height div 2), Point(6, SpeedButton.Height div 2), SpeedButton.Canvas);
end;

procedure TFrmResize.DrawArrowNW(SpeedButton: TSpeedButton);
begin
  DrawArrow(Point(SpeedButton.Width - 6, SpeedButton.Height - 6), Point(6, 6), SpeedButton.Canvas);
end;

function TFrmResize.GetSelectedAnchor: TCanvasPosition;
begin
  if BtnUpperLeft.Down Then
    Result := cpTopLeft
  Else if BtnUpperCenter.Down Then
    Result := cpTopCenter
  Else if BtnUpperRight.Down Then
    Result := cpTopRight
  Else if BtnMiddleLeft.Down Then
    Result := cpMiddleLeft
  Else if BtnMiddleCenter.Down Then
    Result := cpMiddleCenter
  Else if BtnMiddleRight.Down Then
    Result := cpMiddleRight
  Else if BtnLowerLeft.Down Then
    Result := cpBottomLeft
  Else if BtnLowerCenter.Down Then
    Result := cpBottomCenter
  Else if BtnLowerRight.Down Then
    Result := cpBottomRight;
end;

procedure TFrmResize.Reset;
begin
  BtnUpperLeft.Down := True;
end;


end.


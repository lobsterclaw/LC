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
unit FrmResampleUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, ExtCtrls, Buttons;

type

  { TFrmResample }

  TFrmResample = class(TForm)
    BtnCancel: TButton;
    BtnOkay: TButton;
    CbConstrain: TCheckBox;
    EdtHeight: TFloatSpinEdit;
    EdtWidth: TFloatSpinEdit;
    GrpResize: TGroupBox;
    LblHeight: TLabel;
    LblWidth: TLabel;
    RbPercent: TRadioButton;
    RbPixels: TRadioButton;
    procedure CbConstrainChange(Sender: TObject);
    procedure EdtHeightChange(Sender: TObject);
    procedure EdtWidthChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RbPercentChange(Sender: TObject);
    procedure RbPixelsChange(Sender: TObject);
  private
    { private declarations }
    fOrigWidth, fOrigHeight: Integer;
    fIsEditing: Boolean;
    fAspectRatio: Double;
    fPreviousPercent, fPreviousPixels: Boolean;
  public
    { public declarations }
    function GetWidthAsValue: Integer;
    function GetHeightAsValue: Integer;
  end;

var
  FrmResample: TFrmResample;

implementation

{$R *.lfm}

{ TFrmResample }

procedure TFrmResample.FormCreate(Sender: TObject);
begin
  fIsEditing := False;
  fAspectRatio := 1.0;
end;

procedure TFrmResample.FormShow(Sender: TObject);
begin
  fIsEditing := True;
  fOrigWidth := Round(EdtWidth.Value);
  fOrigHeight := Round(EdtHeight.Value);
  fAspectRatio := EdtWidth.Value / EdtHeight.Value;
  fIsEditing := False;
end;

procedure TFrmResample.RbPercentChange(Sender: TObject);
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
  end;

  fPreviousPercent := RbPercent.Checked;
  fIsEditing := False;
end;

procedure TFrmResample.RbPixelsChange(Sender: TObject);
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
  end;

  fPreviousPixels := RbPixels.Checked;
  fIsEditing := False;
end;

function TFrmResample.GetWidthAsValue: Integer;
begin
  if RbPixels.Checked then
    Result := Round(EdtWidth.Value)
  else
    Result := Round(fOrigWidth * (EdtWidth.Value / 100));
end;

function TFrmResample.GetHeightAsValue: Integer;
begin
  if RbPixels.Checked then
    Result := Round(EdtHeight.Value)
  else
    Result := Round(fOrigHeight * (EdtHeight.Value / 100));
end;

procedure TFrmResample.EdtWidthChange(Sender: TObject);
begin
  if fIsEditing then Exit;
  fIsEditing := True;

  if CbConstrain.Checked then
    if RbPixels.Checked then  //pixels
      EdtHeight.Value := Round(EdtWidth.Value / fAspectRatio)
    else //percent
      EdtHeight.Value := EdtWidth.Value;

  fIsEditing := False;
end;

procedure TFrmResample.EdtHeightChange(Sender: TObject);
begin
  if fIsEditing then Exit;
  fIsEditing := True;

  if CbConstrain.Checked then
    if RbPixels.Checked then  //pixels
      EdtWidth.Value := Round(EdtHeight.Value * fAspectRatio)
    else //percent
      EdtWidth.Value := EdtHeight.Value;

  fIsEditing := False;
end;

procedure TFrmResample.CbConstrainChange(Sender: TObject);
begin
  if CbConstrain.Checked then
  begin
    fIsEditing := True;
    EdtHeight.Value := Round(EdtWidth.Value / fAspectRatio);
    fIsEditing := False;
  end;
end;

end.


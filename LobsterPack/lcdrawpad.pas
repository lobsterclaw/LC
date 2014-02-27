{
****************************************************************************
* Copyright 2014 Jeremiah Jenkins                                          *
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
unit LCDrawPad;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes;

type
	TCanvasPosition = (csUpperLeft, csUpperCenter, csUpperRight,
  								csMiddleLeft, csMiddleCenter, csMiddleRight,
                  csLowerLeft, csLowerCenter, csLowerRight);

	TRotateMode = (rm90Right, rm90Left, rm180);
	TFlipMode = (fmHorizontal, fmVertical);

  { TLCCustomDrawPad }

  TLCCustomDrawPad = class(TGraphicControl)
  private
    { Private declarations }
    fCanvasImage: TBGRABitmap;
    fMouseDrawing: boolean;
    fMouseOrigin: TPoint;
    fForeColor: TColor;
    fLineSize: Byte;
    fZoomPercent: Integer;
    fCanvasWidth: Integer;
    fCanvasHeight: Integer;
    fCanvasColor: TColor;
    fCanvasPosition: TCanvasPosition;
    fDrawingOccurred: Boolean;
    procedure RecreateCanvas();
    procedure Draw(X, Y: Integer; Closed: boolean);
    function GetImagePos: TPoint;
    procedure PaintImage;
    procedure SetCanvasHeight(CanvasHeight: Integer);
    procedure SetCanvasWidth(CanvasWidth: Integer);
    procedure SetZoomPercent(ZoomPercent: Integer);
    procedure SetCanvasColor(CanvasColor: TColor);
    procedure SetCanvasPosition(CanvasPosition: TCanvasPosition);
  protected
    { Protected declarations }
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      {%H-}WithThemeSpace: boolean); override;
    procedure UpdateSize;
    procedure MouseDown(Button: TMouseButton; {%H-}Shift:TShiftState; X, Y:Integer); override;
    procedure MouseMove({%H-}Shift:TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; {%H-}Shift:TShiftState; {%H-}X, {%H-}Y:Integer); override;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Rotate(RotateMode: TRotateMode);
    procedure Flip(FlipMode: TFlipMode);
    procedure NewCanvas(NewWidth, NewHeight: Integer; NewColor: TColor);
    procedure ResizeCanvas(NewWidth, NewHeight: Integer);
    procedure ResampleCanvas(NewWidth, NewHeight: Integer; ResampleMode: TResampleMode=rmFineResample);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStreamAsPng(AStream: TStream);
    procedure SaveToBitmap(ABitmap: TBitmap); overload;
    function SaveToBitmap(): TBitmap; overload;
  published
    { Published declarations }
    property ForeColor: TColor read fForeColor write fForeColor;
    property LineSize: Byte read fLineSize write fLineSize;
    property ZoomPercent: Integer read fZoomPercent write SetZoomPercent;
    property CanvasWidth: Integer read fCanvasWidth write SetCanvasWidth;
    property CanvasHeight: Integer read fCanvasHeight write SetCanvasHeight;
    property CanvasColor: TColor read fCanvasColor write SetCanvasColor;
    property CanvasPosition: TCanvasPosition read fCanvasPosition write SetCanvasPosition;
  end;

  TLCDrawPad = class(TLCCustomDrawPad)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    property Align;
    property Anchors;
    property AutoSize;
  end;

procedure Register;

implementation

uses
  lcutils;

procedure Register;
begin
  {$I lcdrawpad_icon.lrs}
  RegisterComponents('Additional',[TLCDrawPad]);
end;

{ TLCCustomDrawPad }

(*procedure TLCCustomDrawPad.ResizeCanvas();
var
  NewCanvasImage: TBGRABitmap;
begin
  if (fCanvasImage = nil) OR (CanvasResizeMode = crmClearContents) then
  begin
	  NewCanvasImage := TBGRABitmap.Create(fCanvasWidth, fCanvasHeight, MapDefaultColor(fCanvasColor, clWhite));
	end
  else if CanvasResizeMode = crmResample then
  begin
    NewCanvasImage := fCanvasImage.Resample(fCanvasWidth, fCanvasHeight, rmSimpleStretch) as TBGRABitmap;
  end
  else if CanvasResizeMode = crmKeepContents then
  begin
	  NewCanvasImage := TBGRABitmap.Create(fCanvasWidth, fCanvasHeight, MapDefaultColor(fCanvasColor, clWhite));
		NewCanvasImage.PutImage(0, 0, fCanvasImage, dmSet);
  end;
  FreeAndNil(fCanvasImage);
  fCanvasImage := NewCanvasImage;
end;*)

procedure TLCCustomDrawPad.RecreateCanvas();
begin
  if Not Assigned(fCanvasImage) Then
  Begin
	  fCanvasImage := TBGRABitmap.Create(fCanvasWidth, fCanvasHeight, MapDefaultColor(fCanvasColor, clWhite));
  End
  Else If (fCanvasWidth <> fCanvasImage.Width) Or (fCanvasHeight <> fCanvasImage.Height) then
  Begin
    fCanvasImage.SetSize(fCanvasWidth, fCanvasHeight);
    fCanvasImage.Fill(MapDefaultColor(fCanvasColor, clWhite));
  End
  Else
  Begin
    fCanvasImage.Fill(MapDefaultColor(fCanvasColor, clWhite));
  End;
end;

procedure TLCCustomDrawPad.Draw(X, Y: Integer; Closed: boolean);
var
  ratio: Double;
  adjOrigin, adjDest, imagePos: TPoint;
  lForeColor: TBGRAPixel;
begin
  fDrawingOccurred := True;

  imagePos := GetImagePos();
  ratio := fZoomPercent / 100;

  adjOrigin := Point(Round((fMouseOrigin.X - imagePos.X) / ratio), Round((fMouseOrigin.Y - imagePos.Y) / ratio));
  adjDest := Point(Round((X - imagePos.X) / ratio), Round((Y - imagePos.Y) / ratio));

  lForeColor := ColorToBGRA(ColorToRGB(MapDefaultColor(fForeColor, clBlack)));
  lForeColor.alpha:= 255;

  //fCanvasImage.PenStyle := psDash;
  //fCanvasImage.DrawLineAntialias(adjOrigin.X, adjOrigin.Y, adjDest.X, adjDest.Y, BGRA(0,0,0,128), LineSize, true);
  fCanvasImage.DrawLineAntialias(adjOrigin.X, adjOrigin.Y, adjDest.X, adjDest.Y, lForeColor, fLineSize, true);
	//fCanvasImage.Canvas2D.;
  //drawCrayonLine(fCanvasImage.Canvas2D, adjOrigin.X, adjOrigin.Y, adjDest.X, adjDest.Y, BGRA(255,0,0, 255), BGRA(0,0,0, 255), '');

  fMouseOrigin := Point(X,Y);

  Invalidate;
end;

procedure TLCCustomDrawPad.PaintImage;
var
  ImagePos: TPoint;
  StretchedBmp: TBGRABitmap;
  Ratio: Double;
begin
  ImagePos := GetImagePos();

	if fZoomPercent <> 100 Then
  Begin
    Ratio := fZoomPercent / 100;
    StretchedBmp := fCanvasImage.Resample(Round(fCanvasImage.Width * Ratio), Round(fCanvasImage.Height * Ratio), rmSimpleStretch) as TBGRABitmap;
    try
      StretchedBmp.Draw(Canvas, ImagePos.X, ImagePos.Y, True);
    finally
      StretchedBmp.Free;
    end;
  end
  Else
  Begin
    fCanvasImage.Draw(Canvas, ImagePos.X, ImagePos.Y, True);
  end;
end;

function TLCCustomDrawPad.GetImagePos: TPoint;
var
  ratio: Double;
begin
  ratio := fZoomPercent / 100;

  case fCanvasPosition of
  csUpperLeft: Result := Point(0, 0);
  csUpperCenter: Result := Point(
        (self.ClientWidth - Round(fCanvasWidth * ratio)) div 2,
        0
      );
  csUpperRight: Result := Point(
			  (self.ClientWidth - Round(fCanvasWidth * ratio)),
        0
      );
  csMiddleLeft: Result := Point(
        0,
        (self.ClientHeight - Round(fCanvasHeight * ratio)) div 2
      );
  csMiddleCenter: Result := Point(
        (self.ClientWidth - Round(fCanvasWidth * ratio)) div 2,
        (self.ClientHeight - Round(fCanvasHeight * ratio)) div 2
      );
  csMiddleRight: Result := Point(
        (self.ClientWidth - Round(fCanvasWidth * ratio)),
        (self.ClientHeight - Round(fCanvasHeight * ratio)) div 2
      );
  csLowerLeft: Result := Point(
        0,
        (self.ClientHeight - Round(fCanvasHeight * ratio))
      );
  csLowerCenter: Result := Point(
        (self.ClientWidth - Round(fCanvasWidth * ratio)) div 2,
        (self.ClientHeight - Round(fCanvasHeight * ratio))
      );
  csLowerRight: Result := Point(
        (self.ClientWidth - Round(fCanvasWidth * ratio)),
        (self.ClientHeight - Round(fCanvasHeight * ratio))
      );
  else
    Result := Point(0, 0);
  end;

  // test for negative position
  if Result.X < 0 then Result.X := 0;
  if Result.Y < 0 then Result.Y := 0;

end;

procedure TLCCustomDrawPad.SetCanvasHeight(CanvasHeight: Integer);
begin
  If CanvasHeight < 0 Then CanvasHeight := 0;
  If (fCanvasHeight = CanvasHeight) Or fDrawingOccurred Then
  	Exit;

  fCanvasHeight := CanvasHeight;

  RecreateCanvas;
  UpdateSize;
  Invalidate;
end;

procedure TLCCustomDrawPad.SetCanvasWidth(CanvasWidth: Integer);
begin
  If CanvasWidth < 0 Then CanvasWidth := 0;
  If (fCanvasWidth = CanvasWidth) Or fDrawingOccurred Then
  	Exit;

  fCanvasWidth := CanvasWidth;

  RecreateCanvas;
  UpdateSize;
  Invalidate;
end;

procedure TLCCustomDrawPad.SetCanvasColor(CanvasColor: TColor);
begin
  If (CanvasColor = fCanvasColor) Or fDrawingOccurred Then
  	Exit;

  fCanvasColor := CanvasColor;

  RecreateCanvas;
  Invalidate;
end;

procedure TLCCustomDrawPad.SetCanvasPosition(CanvasPosition: TCanvasPosition);
begin
  if fCanvasPosition = CanvasPosition then Exit;

  fCanvasPosition := CanvasPosition;

  Invalidate;
end;

procedure TLCCustomDrawPad.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: boolean);
var
  ratio: Double;
begin
  ratio := fZoomPercent / 100;

  PreferredWidth := Round(fCanvasWidth * ratio);
  PreferredHeight := Round(fCanvasHeight * ratio);
end;

procedure TLCCustomDrawPad.UpdateSize;
begin
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TLCCustomDrawPad.SetZoomPercent(ZoomPercent: Integer);
begin
  if ZoomPercent < 1 Then ZoomPercent := 1;
  if ZoomPercent > 5000 Then ZoomPercent := 5000;
  if fZoomPercent = ZoomPercent then Exit;

  fZoomPercent := ZoomPercent;

  UpdateSize;
  Invalidate;
end;

procedure TLCCustomDrawPad.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    fMouseDrawing := True;
    fMouseOrigin := Point(X, Y);
    Draw(X, Y, True);
  end;
end;

procedure TLCCustomDrawPad.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if fMouseDrawing then Draw(X, Y, False);
end;

procedure TLCCustomDrawPad.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    fMouseDrawing := False;
end;

procedure TLCCustomDrawPad.Paint();
begin
  PaintImage;
end;

procedure TLCCustomDrawPad.Rotate(RotateMode: TRotateMode);
var
  TempBitmap: TBGRABitmap;
  UsesTemp: Boolean = True;
begin
  If RotateMode = rm90Left Then
  	 TempBitmap := fCanvasImage.RotateCCW() as TBGRABitmap
  Else If RotateMode = rm90Right Then
    TempBitmap := fCanvasImage.RotateCW() as TBGRABitmap
  Else If RotateMode = rm180 Then
  Begin
    Rotate180(fCanvasImage);
    UsesTemp := False;
  end;

  if UsesTemp Then
  Begin
  	fCanvasWidth := TempBitmap.Width;
  	fCanvasHeight := TempBitmap.Height;
  	fCanvasImage.Free();
  	fCanvasImage := TempBitmap;

    UpdateSize;
  end;

  Invalidate;
end;

procedure TLCCustomDrawPad.Flip(FlipMode: TFlipMode);
begin
  If FlipMode = fmHorizontal Then
  	 fCanvasImage.HorizontalFlip
  Else If FlipMode = fmVertical Then
    fCanvasImage.VerticalFlip;

  Invalidate;
end;

procedure TLCCustomDrawPad.NewCanvas(NewWidth, NewHeight: Integer; NewColor: TColor);
begin
  fDrawingOccurred := False;

  fCanvasWidth := NewWidth;
  fCanvasHeight := NewHeight;
  fCanvasColor := NewColor;

  RecreateCanvas;
  UpdateSize;
  Invalidate;
end;

procedure TLCCustomDrawPad.ResizeCanvas(NewWidth, NewHeight: Integer);
var
  NewCanvasImage: TBGRABitmap;
begin
  fCanvasWidth := NewWidth;
  fCanvasHeight := NewHeight;

  NewCanvasImage := TBGRABitmap.Create(fCanvasWidth, fCanvasHeight, MapDefaultColor(fCanvasColor, clWhite));
	NewCanvasImage.PutImage(0, 0, fCanvasImage, dmSet);
  fCanvasImage.Free();
	fCanvasImage := NewCanvasImage;

  UpdateSize;
  Invalidate;
end;

procedure TLCCustomDrawPad.ResampleCanvas(NewWidth, NewHeight: Integer; ResampleMode: TResampleMode = rmFineResample);
var
  NewCanvasImage: TBGRABitmap;
begin
  fCanvasWidth := NewWidth;
  fCanvasHeight := NewHeight;

  NewCanvasImage := fCanvasImage.Resample(fCanvasWidth, fCanvasHeight, ResampleMode) as TBGRABitmap;
  fCanvasImage.Free();
	fCanvasImage := NewCanvasImage;

  Invalidate;
end;

procedure TLCCustomDrawPad.SaveToFile(const FileName: string);
begin
  fCanvasImage.SaveToFile(FileName);
end;

procedure TLCCustomDrawPad.SaveToStreamAsPng(AStream: TStream);
begin
  fCanvasImage.SaveToStreamAsPng(AStream);
end;

procedure TLCCustomDrawPad.SaveToBitmap(ABitmap: TBitmap);
var
  opaqueCopy: TBGRACustomBitmap;
begin
  ABitmap.Width := fCanvasImage.Width;
  ABitmap.Height := fCanvasImage.Height;
  opaqueCopy := fCanvasImage.NewBitmap(fCanvasImage.Width, fCanvasImage.Height);
  opaqueCopy.Fill(ColorToRGB(MapDefaultColor(fCanvasColor, clWhite)));
  opaqueCopy.PutImage(0, 0, fCanvasImage, dmDrawWithTransparency);
  opaqueCopy.Draw(ABitmap.canvas, 0, 0, True);
  opaqueCopy.Free;
end;

function TLCCustomDrawPad.SaveToBitmap: TBitmap;
begin
  Result := fCanvasImage.MakeBitmapCopy(MapDefaultColor(fCanvasColor, clWhite));
end;

constructor TLCCustomDrawPad.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fMouseDrawing := false;
  fMouseOrigin := Point(0, 0);
  fForeColor := clBlack;
  fLineSize := 20;

  fZoomPercent := 100;

  fCanvasWidth := 640;
  fCanvasHeight := 480;
  fCanvasColor := clWhite;
  fCanvasPosition := csUpperLeft;

  fDrawingOccurred := False;

  //fCanvasImage := TBGRABitmap.Create('header_logo.gif');
  //fCanvasImage := TBGRABitmap.Create(fCanvasWidth, fCanvasHeight, MapDefaultColor(fCanvasColor, clWhite));
  RecreateCanvas;
end;

destructor TLCCustomDrawPad.Destroy;
begin
  FreeAndNil(fCanvasImage);

  inherited;
end;

end.

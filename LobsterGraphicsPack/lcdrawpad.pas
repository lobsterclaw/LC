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
unit LCDrawPad;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes;

type
	TCanvasPosition = (cpTopLeft, cpTopCenter, cpTopRight,
  								cpMiddleLeft, cpMiddleCenter, cpMiddleRight,
                  cpBottomLeft, cpBottomCenter, cpBottomRight);

	TRotateMode = (rm90Right, rm90Left, rm180);
	TFlipMode = (fmHorizontal, fmVertical);

  { TLCCustomDrawPad }

  TLCCustomDrawPad = class(TGraphicControl)
  private
    { Private declarations }
    fCanvasImage: TBGRABitmap;
    fMouseDrawing: boolean;
    fMouseOrigin: TPoint;
    fBackgroundColor1: TColor;
    fBackgroundColor2: TColor;
    fFillColor: TColor;
    fLineColor: TColor;
    fLineSize: Byte;
    fZoomPercent: Integer;
    fCanvasWidth: Integer;
    fCanvasHeight: Integer;
    fCanvasColor: TColor;
    fCanvasPosition: TCanvasPosition;
    fIsFreshImage: Boolean;
    fSafeToChangeCanvas: Boolean;
    fOnImageChange: TNotifyEvent;
    fOnZoomChange: TNotifyEvent;
    procedure ValidateSize(AWidth, AHeight: Integer);
    procedure RecreateCanvas();
    procedure Draw(X, Y: Integer; Closed: boolean);
    function GetImagePos: TPoint; overload;
    function GetImagePos(Outer, Inner: TPoint; Ratio: Double; ACanvasPosition: TCanvasPosition; AdjustNegative: Boolean): TPoint; overload;
    procedure PaintImage;
    procedure DoImageChange();
    procedure DoZoomChange();
    procedure SetLineSize(LineSize: Byte);
    procedure SetCanvasHeight(CanvasHeight: Integer);
    procedure SetCanvasWidth(CanvasWidth: Integer);
    procedure SetZoomPercent(ZoomPercent: Integer);
    procedure SetBackgroundColor1(BackgroundColor1: TColor);
    procedure SetBackgroundColor2(BackgroundColor2: TColor);
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
    procedure NewCanvas(); overload;
    procedure NewCanvas(NewWidth, NewHeight: Integer; NewColor: TColor); overload;
    procedure ResizeCanvas(NewWidth, NewHeight: Integer; Anchor: TCanvasPosition);
    procedure ResizeImage(NewWidth, NewHeight: Integer);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStreamAsPng(AStream: TStream);
    procedure SaveToBitmap(ABitmap: TBitmap); overload;
    function SaveToBitmap(): TBitmap; overload;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromBitmap(ABitmap: TBitmap);
  published
    { Published declarations }
    property BackgroundColor1: TColor read fBackgroundColor1 write SetBackgroundColor1;
    property BackgroundColor2: TColor read fBackgroundColor2 write SetBackgroundColor2;
    property FillColor: TColor read fFillColor write fFillColor;
    property LineColor: TColor read fLineColor write fLineColor;
    property LineSize: Byte read fLineSize write SetLineSize;
    property ZoomPercent: Integer read fZoomPercent write SetZoomPercent;
    property CanvasWidth: Integer read fCanvasWidth write SetCanvasWidth;
    property CanvasHeight: Integer read fCanvasHeight write SetCanvasHeight;
    property CanvasColor: TColor read fCanvasColor write SetCanvasColor;
    property CanvasPosition: TCanvasPosition read fCanvasPosition write SetCanvasPosition;
    property IsFreshImage: Boolean read fIsFreshImage;

    property OnImageChange: TNotifyEvent read fOnImageChange write fOnImageChange;
    property OnZoomChange: TNotifyEvent read fOnZoomChange write fOnZoomChange;
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
    property BorderSpacing;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Hint;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;

    property OnImageChange;
    property OnZoomChange;
  end;

procedure Register;

implementation

uses
  LCUtils, FileUtil;

const
  MAX_ZOOM = 2000;
  MAX_LINE_SIZE = 250;

procedure Register;
begin
  {$I lcdrawpad_icon.lrs}
  RegisterComponents('Additional',[TLCDrawPad]);
end;

{ TLCCustomDrawPad }

procedure TLCCustomDrawPad.ValidateSize(AWidth, AHeight: Integer);
var
  NumPixels: Integer;
begin
  NumPixels := AWidth * AHeight;
  if NumPixels < 0 then // rolled over integer limit
    raise EOutOfMemory.Create('Image too big');
end;

procedure TLCCustomDrawPad.RecreateCanvas();
begin
  Try
    If Not Assigned(fCanvasImage) Then
    Begin
	    fCanvasImage := TBGRABitmap.Create(fCanvasWidth, fCanvasHeight, MapDefaultColor(fCanvasColor, clWhite));
    End
    Else If (fCanvasWidth <> fCanvasImage.Width) Or (fCanvasHeight <> fCanvasImage.Height) Then
    Begin
      fCanvasImage.SetSize(fCanvasWidth, fCanvasHeight);
      fCanvasImage.Fill(MapDefaultColor(fCanvasColor, clWhite));
    End
    Else
    Begin
      fCanvasImage.Fill(MapDefaultColor(fCanvasColor, clWhite));
    End;
  Finally
    //safety measure
    If Assigned(fCanvasImage) Then
    Begin
      fCanvasWidth := fCanvasImage.Width;
      fCanvasHeight := fCanvasImage.Height;
    End;
  End;
end;

procedure TLCCustomDrawPad.Draw(X, Y: Integer; Closed: boolean);
var
  ratio: Double;
  adjOrigin, adjDest, imagePos: TPoint;
  lLineColor: TBGRAPixel;
begin
  imagePos := GetImagePos();
  ratio := fZoomPercent / 100;

  adjOrigin := Point(Round((fMouseOrigin.X - imagePos.X) / ratio), Round((fMouseOrigin.Y - imagePos.Y) / ratio));
  adjDest := Point(Round((X - imagePos.X) / ratio), Round((Y - imagePos.Y) / ratio));

  lLineColor := ColorToBGRA(ColorToRGB(MapDefaultColor(fLineColor, clBlack)));
  lLineColor.alpha:= 255;

  //fCanvasImage.PenStyle := psDash;
  //fCanvasImage.DrawLineAntialias(adjOrigin.X, adjOrigin.Y, adjDest.X, adjDest.Y, BGRA(0,0,0,128), LineSize, true);
  fCanvasImage.DrawLineAntialias(adjOrigin.X, adjOrigin.Y, adjDest.X, adjDest.Y, lLineColor, fLineSize, true);
	//fCanvasImage.Canvas2D.;
  //drawCrayonLine(fCanvasImage.Canvas2D, adjOrigin.X, adjOrigin.Y, adjDest.X, adjDest.Y, BGRA(255,0,0, 255), BGRA(0,0,0, 255), '');

  fSafeToChangeCanvas := False;
  fIsFreshImage := False;

  fMouseOrigin := Point(X,Y);

  Invalidate;
  DoImageChange;
end;

procedure TLCCustomDrawPad.PaintImage;
var
  ImagePos: TPoint;
  StretchedImage: TBGRABitmap;
  Ratio: Double;
begin
  ImagePos := GetImagePos();

	if fZoomPercent <> 100 Then
  Begin
    Ratio := fZoomPercent / 100;
    StretchedImage := fCanvasImage.Resample(Round(fCanvasImage.Width * Ratio), Round(fCanvasImage.Height * Ratio), rmSimpleStretch) as TBGRABitmap;
    try
      StretchedImage.Draw(Canvas, ImagePos.X, ImagePos.Y, True);
    finally
      StretchedImage.Free;
    end;
  end
  Else
  Begin
    fCanvasImage.Draw(Canvas, ImagePos.X, ImagePos.Y, True);
  end;
end;

procedure TLCCustomDrawPad.SetLineSize(LineSize: Byte);
begin
  if LineSize < 1 Then LineSize := 1;
  if LineSize > MAX_LINE_SIZE Then LineSize := MAX_LINE_SIZE;
  if fLineSize = LineSize then Exit;

  fLineSize := LineSize;
end;

function TLCCustomDrawPad.GetImagePos: TPoint;
var
  Ratio: Double;
begin
  Ratio := fZoomPercent / 100;
  Result := GetImagePos(Point(ClientWidth, ClientHeight), Point(fCanvasWidth, fCanvasHeight), Ratio, fCanvasPosition, True);
end;

function TLCCustomDrawPad.GetImagePos(Outer, Inner: TPoint; Ratio: Double; ACanvasPosition: TCanvasPosition; AdjustNegative: Boolean): TPoint; overload;
begin
  case ACanvasPosition of
    cpTopLeft: Result := Point(0, 0);
    cpTopCenter: Result := Point(
        (Outer.X - Round(Inner.X * Ratio)) div 2,
        0
      );
    cpTopRight: Result := Point(
			  (Outer.X - Round(Inner.X * Ratio)),
        0
      );
    cpMiddleLeft: Result := Point(
        0,
        (Outer.Y - Round(Inner.Y * Ratio)) div 2
      );
    cpMiddleCenter: Result := Point(
        (Outer.X - Round(Inner.X * Ratio)) div 2,
        (Outer.Y - Round(Inner.Y * Ratio)) div 2
      );
    cpMiddleRight: Result := Point(
        (Outer.X - Round(Inner.X * Ratio)),
        (Outer.Y - Round(Inner.Y * Ratio)) div 2
      );
    cpBottomLeft: Result := Point(
        0,
        (Outer.Y - Round(Inner.Y * Ratio))
      );
    cpBottomCenter: Result := Point(
        (Outer.X - Round(Inner.X * Ratio)) div 2,
        (Outer.Y - Round(Inner.Y * Ratio))
      );
    cpBottomRight: Result := Point(
        (Outer.X - Round(Inner.X * Ratio)),
        (Outer.Y - Round(Inner.Y * Ratio))
      );
  else
    Result := Point(0, 0);
  end;

  // test for negative position
  if AdjustNegative Then
  Begin
    if Result.X < 0 then Result.X := 0;
    if Result.Y < 0 then Result.Y := 0;
  End;
end;


procedure TLCCustomDrawPad.SetCanvasHeight(CanvasHeight: Integer);
begin
  If CanvasHeight < 0 Then CanvasHeight := 0;
  If (fCanvasHeight = CanvasHeight) Or (Not fSafeToChangeCanvas) Then
  	Exit;

  ValidateSize(fCanvasWidth, CanvasHeight);

  fCanvasHeight := CanvasHeight;

  RecreateCanvas;
  UpdateSize;
  Invalidate;
end;

procedure TLCCustomDrawPad.SetCanvasWidth(CanvasWidth: Integer);
begin
  If CanvasWidth < 0 Then CanvasWidth := 0;
  If (fCanvasWidth = CanvasWidth) Or (Not fSafeToChangeCanvas) Then
  	Exit;

  ValidateSize(CanvasWidth, fCanvasHeight);

  fCanvasWidth := CanvasWidth;

  RecreateCanvas;
  UpdateSize;
  Invalidate;
end;

procedure TLCCustomDrawPad.SetCanvasColor(CanvasColor: TColor);
begin
  If (CanvasColor = fCanvasColor) Or (Not fSafeToChangeCanvas) Then
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
  if ZoomPercent > MAX_ZOOM Then ZoomPercent := MAX_ZOOM;
  if fZoomPercent = ZoomPercent then Exit;

  fZoomPercent := ZoomPercent;

  UpdateSize;
  Invalidate;
  DoZoomChange;
end;

procedure TLCCustomDrawPad.SetBackgroundColor1(BackgroundColor1: TColor);
begin
  if fBackgroundColor1 = BackgroundColor1 Then
    Exit;

  fBackgroundColor1 := BackgroundColor1;
  Invalidate;
end;

procedure TLCCustomDrawPad.SetBackgroundColor2(BackgroundColor2: TColor);
begin
  if fBackgroundColor2 = BackgroundColor2 Then
    Exit;

  fBackgroundColor2 := BackgroundColor2;
  Invalidate;
end;

procedure TLCCustomDrawPad.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Button = mbLeft then
  begin
    fMouseDrawing := True;
    fMouseOrigin := Point(X, Y);
    Draw(X, Y, True);
  end;
end;

procedure TLCCustomDrawPad.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if fMouseDrawing then Draw(X, Y, False);
end;

procedure TLCCustomDrawPad.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Button = mbLeft then
    fMouseDrawing := False;
end;

procedure TLCCustomDrawPad.Paint();
begin
  if ((fBackgroundColor1 <> clNone) and (fBackgroundColor1 <> clDefault)) then
  begin
    Canvas.Brush.Color := fBackgroundColor1;
    Canvas.FillRect(0, 0, Canvas.Width, Canvas.Height);
  end;
  PaintImage;
end;

procedure TLCCustomDrawPad.DoImageChange();
begin
  if Assigned(fOnImageChange) then fOnImageChange(Self);
end;

procedure TLCCustomDrawPad.DoZoomChange();
begin
  if Assigned(fOnZoomChange) then fOnZoomChange(Self);
end;

procedure TLCCustomDrawPad.Rotate(RotateMode: TRotateMode);
var
  TempImage: TBGRABitmap;
  UsesTemp: Boolean = True;
begin
  If RotateMode = rm90Left Then
  	 TempImage := fCanvasImage.RotateCCW() as TBGRABitmap
  Else If RotateMode = rm90Right Then
    TempImage := fCanvasImage.RotateCW() as TBGRABitmap
  Else If RotateMode = rm180 Then
  Begin
    Rotate180(fCanvasImage);
    UsesTemp := False;
  end;

  if UsesTemp Then
  Begin
    try
  	  fCanvasImage.Assign(TempImage);
    	fCanvasWidth := fCanvasImage.Width;
    	fCanvasHeight := fCanvasImage.Height;
    finally
      FreeAndNil(TempImage);
    End;
    UpdateSize;
  end;

  fIsFreshImage := False;

  Invalidate;
  DoImageChange;
end;

procedure TLCCustomDrawPad.Flip(FlipMode: TFlipMode);
begin
  If FlipMode = fmHorizontal Then
  	 fCanvasImage.HorizontalFlip
  Else If FlipMode = fmVertical Then
    fCanvasImage.VerticalFlip;

  fIsFreshImage := False;

  Invalidate;
  DoImageChange;
end;

procedure TLCCustomDrawPad.NewCanvas();
begin
  fSafeToChangeCanvas := True;
  fIsFreshImage := True;

  RecreateCanvas;
  UpdateSize;
  Invalidate;
  DoImageChange;
end;

procedure TLCCustomDrawPad.NewCanvas(NewWidth, NewHeight: Integer; NewColor: TColor);
begin
  ValidateSize(NewWidth, NewHeight);

  fSafeToChangeCanvas := True;
  fIsFreshImage := True;

  fCanvasWidth := NewWidth;
  fCanvasHeight := NewHeight;
  fCanvasColor := NewColor;

  RecreateCanvas;
  UpdateSize;
  Invalidate;
  DoImageChange;
end;

procedure TLCCustomDrawPad.ResizeCanvas(NewWidth, NewHeight: Integer; Anchor: TCanvasPosition);
var
  NewCanvasImage: TBGRABitmap;
  ImagePos: TPoint;
begin
  ValidateSize(NewWidth, NewHeight);

  ImagePos := GetImagePos(Point(NewWidth, NewHeight), Point(fCanvasImage.Width, fCanvasImage.Height), 1, Anchor, False);

  NewCanvasImage := TBGRABitmap.Create(NewWidth, NewHeight, MapDefaultColor(fCanvasColor, clWhite));
  try
    NewCanvasImage.PutImage(ImagePos.X, ImagePos.Y, fCanvasImage, dmSet);
    fCanvasImage.Assign(NewCanvasImage);
    fCanvasWidth := fCanvasImage.Width;
    fCanvasHeight := fCanvasImage.Height;
  finally
    FreeAndNil(NewCanvasImage);
  end;

  fIsFreshImage := False;

  UpdateSize;
  Invalidate;
  DoImageChange;
end;

procedure TLCCustomDrawPad.ResizeImage(NewWidth, NewHeight: Integer);
var
  NewCanvasImage: TBGRABitmap;
begin
  ValidateSize(NewWidth, NewHeight);

  NewCanvasImage := fCanvasImage.Resample(NewWidth, NewHeight) as TBGRABitmap;
  fCanvasImage.Free();
	fCanvasImage := NewCanvasImage;
  NewCanvasImage := nil;

  fCanvasWidth := NewWidth;
  fCanvasHeight := NewHeight;

  fIsFreshImage := False;

  Invalidate;
  DoImageChange;
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
  OpaqueCopy: TBGRACustomBitmap;
begin
  ABitmap.Width := fCanvasImage.Width;
  ABitmap.Height := fCanvasImage.Height;

  OpaqueCopy := fCanvasImage.NewBitmap(fCanvasImage.Width, fCanvasImage.Height);
  try
    OpaqueCopy.Fill(ColorToRGB(MapDefaultColor(fCanvasColor, clWhite)));
    OpaqueCopy.PutImage(0, 0, fCanvasImage, dmDrawWithTransparency);
    OpaqueCopy.Draw(ABitmap.canvas, 0, 0, True);
  finally
    FreeAndNil(OpaqueCopy);
  end;
end;

function TLCCustomDrawPad.SaveToBitmap: TBitmap;
begin
  Result := fCanvasImage.MakeBitmapCopy(MapDefaultColor(fCanvasColor, clWhite));
end;

procedure TLCCustomDrawPad.LoadFromFile(const FileName: string);
var
  OpaqueCopy: TBGRACustomBitmap;
begin
  If Not FileExistsUTF8(FileName) Then
    Exit;

  OpaqueCopy := TBGRABitmap.Create(FileName);
  try
    fCanvasImage.SetSize(OpaqueCopy.Width, OpaqueCopy.Height);
    fCanvasImage.Fill(MapDefaultColor(fCanvasColor, clWhite));
    fCanvasImage.PutImage(0, 0, OpaqueCopy, dmDrawWithTransparency);
    fCanvasWidth := fCanvasImage.Width;
    fCanvasHeight := fCanvasImage.Height;
  finally
    FreeAndNil(OpaqueCopy);
  end;

  fSafeToChangeCanvas := False;
  fIsFreshImage := True;

  UpdateSize;
  Invalidate;
  DoImageChange;
end;

procedure TLCCustomDrawPad.LoadFromBitmap(ABitmap: TBitmap);
var
  OpaqueCopy: TBGRACustomBitmap;
begin
  if (ABitmap = nil) Or (ABitmap.Width = 0) Or (ABitmap.Height = 0) Then
  	Exit;

  OpaqueCopy := TBGRABitmap.Create(ABitmap);
  try
    fCanvasImage.SetSize(OpaqueCopy.Width, OpaqueCopy.Height);
    fCanvasImage.Fill(MapDefaultColor(fCanvasColor, clWhite));
    fCanvasImage.PutImage(0, 0, OpaqueCopy, dmDrawWithTransparency);
    fCanvasWidth := fCanvasImage.Width;
    fCanvasHeight := fCanvasImage.Height;
  finally
    FreeAndNil(OpaqueCopy);
  end;

  fSafeToChangeCanvas := False;
  fIsFreshImage := True;

  UpdateSize;
  Invalidate;
  DoImageChange;
end;

constructor TLCCustomDrawPad.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fBackgroundColor1 := clNone;
  fBackgroundColor2 := clNone;

  fMouseDrawing := false;
  fMouseOrigin := Point(0, 0);
  fLineColor := clBlack;
  fLineSize := 10;

  fZoomPercent := 100;

  fCanvasWidth := 640;
  fCanvasHeight := 480;
  fCanvasColor := clWhite;
  fCanvasPosition := cpTopLeft;

  fSafeToChangeCanvas := True;
  fIsFreshImage := True;

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

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

	TRotateMode = (rm90Right, rm90Left);

  { TLCCustomDrawPad }

  TLCCustomDrawPad = class(TGraphicControl)
  private
    { Private declarations }
    fCanvasImage: TBGRABitmap;
    fMouseDrawing: boolean;
    fMouseOrigin: TPoint;
    fForeColor: TColor;
    fBrushRadius: Byte;
    fZoomPercent: Integer;
    fCanvasWidth: Integer;
    fCanvasHeight: Integer;
    fCanvasColor: TColor;
    fCanvasPosition: TCanvasPosition;
    fDrawingOccurred: Boolean;
    procedure EnsureCanvas();
    procedure DrawBrush(X, Y: Integer; Closed: boolean);
    function getImagePos: TPoint;
    procedure PaintImage;
    procedure setCanvasHeight(CanvasHeight: Integer);
    procedure setCanvasWidth(CanvasWidth: Integer);
    procedure setZoomPercent(ZoomPercent: Integer);
    procedure setCanvasColor(CanvasColor: TColor);
    procedure setCanvasPosition(CanvasPosition: TCanvasPosition);
  protected
    { Protected declarations }
    procedure MouseDown(Button: TMouseButton; {%H-}Shift:TShiftState; X, Y:Integer); override;
    procedure MouseMove({%H-}Shift:TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; {%H-}Shift:TShiftState; {%H-}X, {%H-}Y:Integer); override;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Rotate(RotateMode: TRotateMode);
  published
    { Published declarations }
    property ForeColor: TColor read fForeColor write fForeColor;
    property BrushRadius: Byte read fBrushRadius write fBrushRadius;
    property ZoomPercent: Integer read fZoomPercent write setZoomPercent;
    property CanvasWidth: Integer read fCanvasWidth write setCanvasWidth;
    property CanvasHeight: Integer read fCanvasHeight write setCanvasHeight;
    property CanvasColor: TColor read fCanvasColor write setCanvasColor;
    property CanvasPosition: TCanvasPosition read fCanvasPosition write setCanvasPosition;
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
	  NewCanvasImage := TBGRABitmap.Create(fCanvasWidth, fCanvasHeight, mapDefaultColor(fCanvasColor, clWhite));
	end
  else if CanvasResizeMode = crmResample then
  begin
    NewCanvasImage := fCanvasImage.Resample(fCanvasWidth, fCanvasHeight, rmSimpleStretch) as TBGRABitmap;
  end
  else if CanvasResizeMode = crmKeepContents then
  begin
	  NewCanvasImage := TBGRABitmap.Create(fCanvasWidth, fCanvasHeight, mapDefaultColor(fCanvasColor, clWhite));
		NewCanvasImage.PutImage(0, 0, fCanvasImage, dmSet);
  end;
  FreeAndNil(fCanvasImage);
  fCanvasImage := NewCanvasImage;
end;*)

procedure TLCCustomDrawPad.EnsureCanvas();
var
  NewCanvasImage: TBGRABitmap;
begin
  if (fCanvasImage = nil) Then
  Begin
	  fCanvasImage := TBGRABitmap.Create(fCanvasWidth, fCanvasHeight, mapDefaultColor(fCanvasColor, clWhite));
  End
  Else If (fCanvasWidth <> fCanvasImage.Width) Or (fCanvasHeight <> fCanvasImage.Height) then
  Begin
    NewCanvasImage := TBGRABitmap.Create(fCanvasWidth, fCanvasHeight, mapDefaultColor(fCanvasColor, clWhite));
		NewCanvasImage.PutImage(0, 0, fCanvasImage, dmSet);
    fCanvasImage.Free();
  	fCanvasImage := NewCanvasImage;
  End;
end;

procedure TLCCustomDrawPad.DrawBrush(X, Y: Integer; Closed: boolean);
var
  ratio: Double;
  adjOrigin, adjDest, imagePos: TPoint;
  lForeColor: TBGRAPixel;
begin
  EnsureCanvas();

  fDrawingOccurred := True;

  imagePos := getImagePos();
  ratio := fZoomPercent / 100;

  adjOrigin := Point(Round(fMouseOrigin.X / ratio) - imagePos.X, Round(fMouseOrigin.Y / ratio) - imagePos.Y);
  adjDest := Point(Round(X / ratio) - imagePos.X, Round(Y / ratio) - imagePos.Y);

  lForeColor := ColorToBGRA(ColorToRGB(mapDefaultColor(fForeColor, clBlack)));
  lForeColor.alpha:= 255;

  //fCanvasImage.PenStyle := psDash;
  //fCanvasImage.DrawLineAntialias(adjOrigin.X, adjOrigin.Y, adjDest.X, adjDest.Y, BGRA(0,0,0,128), brushRadius, true);
  fCanvasImage.DrawLineAntialias(adjOrigin.X, adjOrigin.Y, adjDest.X, adjDest.Y, lForeColor, fBrushRadius, true);
	//fCanvasImage.Canvas2D.;
  //drawCrayonLine(fCanvasImage.Canvas2D, adjOrigin.X, adjOrigin.Y, adjDest.X, adjDest.Y, BGRA(255,0,0, 255), BGRA(0,0,0, 255), '');

  fMouseOrigin := Point(X,Y);

  self.Invalidate;
end;

procedure TLCCustomDrawPad.PaintImage;
var
  imagePos: TPoint;
  stretchedBmp: TBGRABitmap;
  ratio: Double;
begin
  EnsureCanvas();

  imagePos := getImagePos();

	if fZoomPercent <> 100 Then
  Begin
    ratio := fZoomPercent / 100;
    stretchedBmp := fCanvasImage.Resample(Round(fCanvasImage.Width * ratio), Round(fCanvasImage.Height * ratio), rmSimpleStretch) as TBGRABitmap;
    try
      stretchedBmp.Draw(Canvas, imagePos.X, imagePos.Y, True);
    finally
      stretchedBmp.Free;
    end;
  end
  Else
  Begin
    fCanvasImage.Draw(Canvas, imagePos.X, imagePos.Y, True);
  end;
end;

function TLCCustomDrawPad.getImagePos: TPoint;
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

procedure TLCCustomDrawPad.setCanvasHeight(CanvasHeight: Integer);
begin
  if CanvasHeight < 0 Then CanvasHeight := 0;
  if fCanvasHeight = CanvasHeight then Exit;

  fCanvasHeight := CanvasHeight;

  self.Invalidate;
end;

procedure TLCCustomDrawPad.setCanvasWidth(CanvasWidth: Integer);
begin
  if CanvasWidth < 0 Then CanvasWidth := 0;
  if fCanvasWidth = CanvasWidth then Exit;

  fCanvasWidth := CanvasWidth;

  self.Invalidate;
end;

procedure TLCCustomDrawPad.setCanvasColor(CanvasColor: TColor);
begin
  if CanvasColor = fCanvasColor then
  	exit;

  fCanvasColor := CanvasColor;

  If (Not fDrawingOccurred) Then FreeAndNil(fCanvasImage);

  self.Invalidate;
end;

procedure TLCCustomDrawPad.setCanvasPosition(CanvasPosition: TCanvasPosition);
begin
  if fCanvasPosition = CanvasPosition then Exit;

  fCanvasPosition := CanvasPosition;

  self.Invalidate;
end;

procedure TLCCustomDrawPad.setZoomPercent(ZoomPercent: Integer);
begin
  if ZoomPercent < 1 Then ZoomPercent := 1;
  if ZoomPercent > 5000 Then ZoomPercent := 5000;
  if fZoomPercent = ZoomPercent then Exit;

  fZoomPercent := ZoomPercent;

  self.Invalidate;
end;

procedure TLCCustomDrawPad.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    fMouseDrawing := True;
    fMouseOrigin := Point(X,Y);
    DrawBrush(X,Y,True);
  end;
end;

procedure TLCCustomDrawPad.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if fMouseDrawing then DrawBrush(X,Y,False);
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
begin
  EnsureCanvas();

  If RotateMode = rm90Left Then
  	 TempBitmap := fCanvasImage.RotateCCW() as TBGRABitmap
  Else If RotateMode = rm90Right Then
    TempBitmap := fCanvasImage.RotateCW() as TBGRABitmap;

  fCanvasWidth := TempBitmap.Width;
  fCanvasHeight := TempBitmap.Height;
  FreeAndNil(fCanvasImage);
  fCanvasImage := TempBitmap;

  self.Invalidate;
end;

constructor TLCCustomDrawPad.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fMouseDrawing := false;
  fMouseOrigin := Point(0, 0);
  fForeColor := clBlack;
  fBrushRadius := 20;

  fZoomPercent := 100;

  fCanvasWidth := 640;
  fCanvasHeight := 480;
  fCanvasColor := clWhite;
  fCanvasPosition := csUpperLeft;

  fDrawingOccurred := False;

  //fCanvasImage := TBGRABitmap.Create('header_logo.gif');
  //fCanvasImage := TBGRABitmap.Create(fCanvasWidth, fCanvasHeight, mapDefaultColor(fCanvasColor, clWhite));
end;

destructor TLCCustomDrawPad.Destroy;
begin
  FreeAndNil(fCanvasImage);

  inherited;
end;

end.

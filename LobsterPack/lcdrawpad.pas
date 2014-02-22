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

  { TLCCustomDrawPad }

  TLCCustomDrawPad = class(TGraphicControl)
  private
    { Private declarations }
    fImage: TBGRABitmap;
    fMouseDrawing: boolean;
    fMouseOrigin: TPoint;
    fForeColor: TColor;
    fBrushRadius: Byte;
    fZoomPercent: Integer;
    fCanvasWidth: Integer;
    fCanvasHeight: Integer;
    fCanvasColor: TColor;
    fCanvasPosition: TCanvasPosition;
    procedure EnsureCanvas();
    procedure DrawBrush(X, Y: Integer; Closed: boolean);
    function getImagePos: TPoint;
    procedure PaintImage;
    procedure setCanvasHeight(CanvasHeight: Integer);
    procedure setCanvasWidth(CanvasWidth: Integer);
    procedure setZoomPercent(ZoomPercent: byte);
    procedure setCanvasColor(CanvasColor: TColor);
    procedure setCanvasPosition(CanvasPosition: TCanvasPosition);
    procedure setZoomPercent(AValue: Integer);
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

procedure TLCCustomDrawPad.EnsureCanvas;
begin
  if fImage = nil then
    fImage := TBGRABitmap.Create(fCanvasWidth, fCanvasHeight, mapDefaultColor(fCanvasColor, clWhite));
end;

procedure TLCCustomDrawPad.DrawBrush(X, Y: Integer; Closed: boolean);
var
  ratio: Double;
  adjOrigin, adjDest, imagePos: TPoint;
  lForeColor: TBGRAPixel;
begin
  EnsureCanvas;

  imagePos := getImagePos();
  ratio := fZoomPercent / 100;

  adjOrigin := Point(Round(fMouseOrigin.X / ratio) - imagePos.X, Round(fMouseOrigin.Y / ratio) - imagePos.Y);
  adjDest := Point(Round(X / ratio) - imagePos.X, Round(Y / ratio) - imagePos.Y);

  lForeColor := ColorToBGRA(ColorToRGB(mapDefaultColor(fForeColor, clBlack)));
  lForeColor.alpha:= 255;

  //fImage.PenStyle := psDash;
  //fImage.DrawLineAntialias(adjOrigin.X, adjOrigin.Y, adjDest.X, adjDest.Y, BGRA(0,0,0,128), brushRadius, true);
  fImage.DrawLineAntialias(adjOrigin.X, adjOrigin.Y, adjDest.X, adjDest.Y, lForeColor, fBrushRadius, true);
	//fImage.Canvas2D.;
  //drawCrayonLine(fImage.Canvas2D, adjOrigin.X, adjOrigin.Y, adjDest.X, adjDest.Y, BGRA(255,0,0, 255), BGRA(0,0,0, 255), '');

  fMouseOrigin := Point(X,Y);

  self.Invalidate;
end;

function TLCCustomDrawPad.getImagePos: TPoint;
var
  ratio: Double;
begin
  EnsureCanvas;

  ratio := fZoomPercent / 100;

  case fCanvasPosition of
  csUpperLeft: Result := Point(0, 0);
  csUpperCenter: Result := Point(
        (self.ClientWidth - Round(fImage.Width * ratio)) div 2,
        0
      );
  csUpperRight: Result := Point(
			  (self.ClientWidth - Round(fImage.Width * ratio)),
        0
      );
  csMiddleLeft: Result := Point(
        0,
        (self.ClientHeight - Round(fImage.Height * ratio)) div 2
      );
  csMiddleCenter: Result := Point(
        (self.ClientWidth - Round(fImage.Width * ratio)) div 2,
        (self.ClientHeight - Round(fImage.Height * ratio)) div 2
      );
  csMiddleRight: Result := Point(
        (self.ClientWidth - Round(fImage.Width * ratio)),
        (self.ClientHeight - Round(fImage.Height * ratio)) div 2
      );
  csLowerLeft: Result := Point(
        0,
        (self.ClientHeight - Round(fImage.Height * ratio))
      );
  csLowerCenter: Result := Point(
        (self.ClientWidth - Round(fImage.Width * ratio)) div 2,
        (self.ClientHeight - Round(fImage.Height * ratio))
      );
  csLowerRight: Result := Point(
        (self.ClientWidth - Round(fImage.Width * ratio)),
        (self.ClientHeight - Round(fImage.Height * ratio))
      );
  else
    Result := Point(0, 0);
  end;

  // test for negative position
  if Result.X < 0 then Result.X := 0;
  if Result.Y < 0 then Result.Y := 0;

end;

procedure TLCCustomDrawPad.PaintImage;
var
  imagePos: TPoint;
  stretchedBmp: TBGRABitmap;
  ratio: Double;
begin
  EnsureCanvas;

  imagePos := getImagePos();
  If fZoomPercent <> 100 Then
  Begin
    ratio := fZoomPercent / 100;
    stretchedBmp := fImage.Resample(Round(fImage.Width * ratio), Round(fImage.Height * ratio), rmSimpleStretch) as TBGRABitmap;
    try
      stretchedBmp.Draw(Canvas, imagePos.X, imagePos.Y, True);
    finally
      stretchedBmp.Free;
    end;
  end
  Else
  Begin
    fImage.Draw(Canvas, imagePos.X, imagePos.Y, True);
  end;
end;

procedure TLCCustomDrawPad.setCanvasHeight(CanvasHeight: Integer);
begin
  if fCanvasHeight = CanvasHeight then Exit;

  fCanvasHeight := CanvasHeight;
  if fCanvasHeight < 0 Then fCanvasHeight := 0;

  self.Invalidate;
end;

procedure TLCCustomDrawPad.setCanvasWidth(CanvasWidth: Integer);
begin
  if fCanvasWidth = CanvasWidth then Exit;

  fCanvasWidth := CanvasWidth;
  if fCanvasWidth < 0 Then fCanvasWidth := 0;

  self.Invalidate;
end;

procedure TLCCustomDrawPad.setZoomPercent(ZoomPercent: byte);
begin
  if fZoomPercent = ZoomPercent then Exit;

  fZoomPercent := ZoomPercent;
  if fZoomPercent < 1 Then fZoomPercent := 1;
  if fZoomPercent > 5000 Then fZoomPercent := 5000;

  self.Invalidate;
end;

procedure TLCCustomDrawPad.setCanvasColor(CanvasColor: TColor);
begin
  if CanvasColor = fCanvasColor then
  	exit;

  fCanvasColor := CanvasColor;

  self.Invalidate;
end;

procedure TLCCustomDrawPad.setCanvasPosition(CanvasPosition: TCanvasPosition);
begin
  if fCanvasPosition = CanvasPosition then Exit;

  fCanvasPosition := CanvasPosition;

  self.Invalidate;
end;

procedure TLCCustomDrawPad.setZoomPercent(AValue: Integer);
begin
  if fZoomPercent=AValue then Exit;
  fZoomPercent:=AValue;
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

constructor TLCCustomDrawPad.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  //fImage := TBGRABitmap.Create('header_logo.gif');
  fImage := nil;

  fCanvasWidth := 640;
  fCanvasHeight := 480;

  fMouseDrawing := false;
  fMouseOrigin := Point(0, 0);
  fForeColor := clBlack;
  fCanvasColor := clWhite;
  fBrushRadius := 20;

  fCanvasPosition := csUpperLeft;
  fZoomPercent := 100;
end;

destructor TLCCustomDrawPad.Destroy;
begin
  fImage.Free;

  inherited;
end;

end.

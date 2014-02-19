unit LCDrawPad;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes, BGRACanvas2D, Math;

type

  { TLCCustomDrawPad }

  TLCCustomDrawPad = class(TGraphicControl)
  private
    { Private declarations }
    fStretched: boolean;
    fImage: TBGRABitmap;
    fMouseDrawing: boolean;
    fMouseOrigin: TPoint;
    fFillColor: TColor;
    procedure DrawBrush(X, Y: Integer; Closed: boolean);
    function getImagePos: TPoint;
    procedure PaintImage;
    procedure setStretched(Stretched: boolean);
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
    property Stretched: boolean read fStretched write setStretched;
    property FillColor: TColor read fFillColor write fFillColor;
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

procedure Register;
begin
  {$I lcdrawpad_icon.lrs}
  RegisterComponents('Additional',[TLCDrawPad]);
end;

{ TLCCustomDrawPad }

procedure TLCCustomDrawPad.DrawBrush(X, Y: Integer; Closed: boolean);
const
  brushRadius = 20;
var
  xRatio, yRatio: Double;
  adjOrigin, adjDest, imagePos: TPoint;
  lFillColor: TBGRAPixel;
begin
  If fStretched Then
  Begin
    xRatio := fImage.width / self.ClientWidth;
    yRatio := fImage.height / self.ClientHeight;

    adjOrigin := Point(Round(fMouseOrigin.X * xRatio), Round(fMouseOrigin.Y * yRatio));
    adjDest := Point(Round(X * xRatio), Round(Y * yRatio));
  end
  else
  begin
    imagePos := getImagePos();

    adjOrigin := Point(fMouseOrigin.X - imagePos.X, fMouseOrigin.Y - imagePos.Y);
    adjDest := Point(X - imagePos.X, Y - imagePos.Y);
  end;

  lFillColor := ColorToBGRA(ColorToRGB(fFillColor));
  lFillColor.alpha:= 255;

  //fImage.DrawLineAntialias(adjOrigin.X, adjOrigin.Y, adjDest.X, adjDest.Y, BGRA(0,0,0,128), brushRadius, true);
  fImage.DrawLineAntialias(adjOrigin.X, adjOrigin.Y, adjDest.X, adjDest.Y, lFillColor, brushRadius, true);
	//fImage.Canvas2D.;
  //drawCrayonLine(fImage.Canvas2D, adjOrigin.X, adjOrigin.Y, adjDest.X, adjDest.Y, BGRA(255,0,0, 255), BGRA(0,0,0, 255), '');

  fMouseOrigin := Point(X,Y);

  PaintImage;
end;

function TLCCustomDrawPad.getImagePos: TPoint;
begin
  If fStretched Then
  begin
    Result := Point(0, 0);
  end
  else
  begin
    Result := Point(
        (self.ClientWidth - fImage.Width) div 2,
        (self.ClientHeight - fImage.Height) div 2
      );

    // test for negative position
    if Result.X < 0 then Result.X := 0;
    if Result.Y < 0 then Result.Y := 0;
  end;
end;

procedure TLCCustomDrawPad.PaintImage;
var
  imagePos: TPoint;
  stretchedBmp: TBGRABitmap;
begin
  If fStretched Then
  Begin
    stretchedBmp := fImage.Resample(self.ClientWidth, self.ClientHeight, rmSimpleStretch) as TBGRABitmap;
    try
      stretchedBmp.Draw(Canvas, 0, 0, True);
    finally
      stretchedBmp.Free;
    end;
  end
  Else
  Begin
    imagePos := getImagePos();

    fImage.Draw(Canvas, imagePos.X, imagePos.Y, True);
  end;
end;

procedure TLCCustomDrawPad.setStretched(Stretched: boolean);
begin
  if fStretched = Stretched then Exit;

  fStretched := Stretched;
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

constructor TLCCustomDrawPad.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  //fImage := TBGRABitmap.Create('header_logo.gif');
  fImage := TBGRABitmap.Create(640,480,BGRAWhite);  //create a 640x480 image

	fStretched := false;
  fMouseDrawing := false;
  fMouseOrigin := Point(0, 0);
end;

destructor TLCCustomDrawPad.Destroy;
begin
  fImage.Free;

  inherited;
end;

end.

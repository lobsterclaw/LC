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

// drawLine: draw a line on the canvas in crayon
// ctx is the 2d context of the canvas to draw on
// setx1, sety1, setx2, and sety2 should be integer pixels on the canvas
// lineColor and shadeColor should be valid colors for canvas context strokeStyle
// the effect is best if lineColor is bold and shadeColor is a lighter shade of the same color
// specialOrders can be used to change the style of the line. Currently supports "big" or "dashed". If no orders, set to null

procedure drawCrayonLine(ctx: TBGRACanvas2D; setx1,sety1,setx2,sety2: Longint; lineColor,shadeColor: TBGRAPixel; specialOrders: String);
var
	x1,x2,y1,y2: Longint;
	ymax, ymin, progx, progy, engprogx, endprogy: Longint;
  dashed: Integer;
  randomizer: Double;
  fudge: Longint;
begin
	// set up the coordinates to draw all lines from left to right
	if (setx1 <= setx2) then
	begin
		x1 := setx1;
		y1 := sety1;
		x2 := setx2;
		y2 := sety2;
	end
	else
	begin
		x1 := setx2;
		y1 := sety2;
		x2 := setx1;
		y2 := sety1;
	end;

  ymax := Max(y1, y2);
	ymin := Min(y1, y2);

	// ready randomized start and end of individual pieces
	progy := y1;
  progx := x1;

	// ready alternator for dashed lines
	dashed := 5;

	// draw the line in 5 x-pixel intervals, composed of a thin strong line plus upper and lower strokes
//  for(var progx = x1; progx < x2 + 5; progx += 5)
  while progx < x2 + 5 do
	begin
		// fuzz start and end of the line segment
    fudge := x2-x1;
    if fudge = 0 then fudge := 1;
		endprogy := Min(Round((y2-y1)/fudge*(progx-x1) + y1 + 3 - 5 * Random()), ymax);
		endprogy := Max(ymin, endprogy);
		engprogx := Min(progx + 5, x2 + 2);
		randomizer := Random();

		// alternates to draw blanks for 2 / 7 of a dashed line's path
		if (specialOrders = 'dashed') then
		begin
			Dec(dashed);
			if (dashed <= 0) then
			begin
				ctx.closePath();
				if (dashed < -2) then
				begin
					dashed := 5;
				end;
				ctx.strokeStyle('#fffacd');
			end;
		end;

		// 20% chance of not drawing the central line segment
		if (randomizer < 0.95) then
		begin
			ctx.beginPath();
			if (dashed > 0)then begin // avoid resetting color during the blank parts of a dashed line
				ctx.strokeStyle(lineColor);
			end;

			if (specialOrders = 'big') then
			begin
				// draw a thicker line segment if requested
				ctx.lineWidth := 8;
			end
			else
			begin
				ctx.lineWidth := 1;
			end;
			ctx.moveTo(progx, progy);
			ctx.lineTo(engprogx, endprogy);
			ctx.stroke();
		end;
		ctx.beginPath();

		if (dashed > 0) then // avoid resetting color during the blank parts of a dashed line
		begin
			ctx.strokeStyle(shadeColor);
		end;

		// 80% chance of the upper and lower segments having width of 2
		if (randomizer > 0.2) then
		begin
			ctx.lineWidth := 2;
		end
		else
		begin
			ctx.lineWidth := 1;
		end;

		// 85% chance of drawing upper segment
		if (randomizer < 0.85) then
		begin
			ctx.moveTo(progx-Round(Random()*2)+1, progy+2);
			ctx.lineTo(engprogx-Round(Random()*2)+1, endprogy+2);
			ctx.stroke();
		end;

		// 90% chance of drawing lower segment
		if (randomizer > 0.1) then
		begin
			ctx.moveTo(progx-Round(Random()*2)+1, progy-2);
			ctx.lineTo(engprogx-Round(Random()*2)+1, endprogy-2);
			ctx.stroke();
		end;

		// random vertical jots in the segment should be met by the next segment
		progy := endprogy;
		progx := progx + 5;
	end;
end;

procedure TLCCustomDrawPad.DrawBrush(X, Y: Integer; Closed: boolean);
const
  brushRadius = 20;
var
  xRatio, yRatio: Double;
  adjOrigin, adjDest, imagePos: TPoint;
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

  //fImage.DrawLineAntialias(adjOrigin.X, adjOrigin.Y, adjDest.X, adjDest.Y, BGRA(0,0,0,128), brushRadius, true);
  fImage.DrawLineAntialias(adjOrigin.X, adjOrigin.Y, adjDest.X, adjDest.Y, BGRA(0,0,0, 255), brushRadius, true);
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

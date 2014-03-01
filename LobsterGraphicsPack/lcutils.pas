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

unit lcutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, BGRABitmap, BGRABitmapTypes, BGRACanvas2D, Math;

procedure Rotate180(Image: TBGRABitmap);
function MapDefaultColor(ActualColor, ToColor: TColor) : TColor;


implementation

procedure Rotate180(Image: TBGRABitmap);
var
  yb, xb: integer;
  PStart: PBGRAPixel;
  PEnd:   PBGRAPixel;
  temp:   TBGRAPixel;
begin
  if Image = nil then
    exit;

  for yb := 0 to (Image.Height div 2) - 1 do
  begin
    PStart := Image.Scanline[yb];
    PEnd := Image.Scanline[Image.Height - yb - 1] + Image.Width;
    for xb := 0 to Image.Width - 1 do
    begin
      Dec(PEnd);
      temp    := PStart^;
      PStart^ := PEnd^;
      PEnd^   := temp;
      Inc(PStart);
    end;
  end;
  Image.InvalidateBitmap;
end;

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

function MapDefaultColor(ActualColor, ToColor: TColor): TColor;
begin
  if ActualColor = clDefault then
    Result := ToColor
  else
    Result := ActualColor;
end;

end.


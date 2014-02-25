unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ColorBox, ComCtrls, ExtCtrls, PairSplitter, LCDrawPad;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnFlipVertical: TButton;
    BtnFlipHorizontal: TButton;
    BtnRotateLeft: TButton;
    BtnRotateRight: TButton;
    BtnRotate180: TButton;
    BtnReset: TButton;
    BtnResize: TButton;
    BtnResample: TButton;
    BtnSave: TButton;
    BtnToBitmap: TButton;
    ColorButton1: TColorButton;
    Image1: TImage;
    LblZoomPercent: TLabel;
    LCDrawPad1: TLCDrawPad;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    SaveDialog1: TSaveDialog;
    ToolBar1: TToolBar;
    SldZoomPercent: TTrackBar;
    procedure BtnFlipHorizontalClick(Sender: TObject);
    procedure BtnFlipVerticalClick(Sender: TObject);
    procedure BtnRotate180Click(Sender: TObject);
    procedure BtnRotateLeftClick(Sender: TObject);
    procedure BtnRotateRightClick(Sender: TObject);
    procedure BtnResetClick(Sender: TObject);
    procedure BtnResizeClick(Sender: TObject);
    procedure BtnResampleClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnToBitmapClick(Sender: TObject);
    procedure ColorButton1ColorChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SldZoomPercentChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BtnRotateLeftClick(Sender: TObject);
begin
  LCDrawPad1.Rotate(rm90Left);
end;

procedure TForm1.BtnRotate180Click(Sender: TObject);
begin
  LCDrawPad1.Rotate(rm180);
end;

procedure TForm1.BtnFlipVerticalClick(Sender: TObject);
begin
  LCDrawPad1.Flip(fmVertical);
end;

procedure TForm1.BtnFlipHorizontalClick(Sender: TObject);
begin
  LCDrawPad1.Flip(fmHorizontal);
end;

procedure TForm1.BtnRotateRightClick(Sender: TObject);
begin
  LCDrawPad1.Rotate(rm90Right);
end;

procedure TForm1.BtnResetClick(Sender: TObject);
begin
  LCDrawPad1.NewCanvas(500, 200, clWhite);
end;

procedure TForm1.BtnResizeClick(Sender: TObject);
begin
    LCDrawPad1.ResizeCanvas(500, 500);
end;

procedure TForm1.BtnResampleClick(Sender: TObject);
begin
  LCDrawPad1.ResampleCanvas(700, 700);
end;

procedure TForm1.BtnSaveClick(Sender: TObject);
begin
  SaveDialog1.Execute;
  if (SaveDialog1.FileName <> '') then
    LCDrawPad1.SaveToFile(SaveDialog1.FileName);
end;

procedure TForm1.BtnToBitmapClick(Sender: TObject);
begin
  LCDrawPad1.SaveToBitmap(Image1.Picture.Bitmap);
  Image1.Refresh;
end;

procedure TForm1.ColorButton1ColorChanged(Sender: TObject);
begin
  LCDrawPad1.ForeColor:=ColorButton1.ButtonColor;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ColorButton1.ButtonColor := LCDrawPad1.ForeColor;
  SldZoomPercent.Position:= LCDrawPad1.ZoomPercent;
  LblZoomPercent.Caption:= Format('%d%%', [LCDrawPad1.ZoomPercent]);
end;

procedure TForm1.SldZoomPercentChange(Sender: TObject);
begin
  LCDrawPad1.ZoomPercent := SldZoomPercent.Position;
  LblZoomPercent.Caption:= Format('%d%%', [LCDrawPad1.ZoomPercent]);
end;

end.


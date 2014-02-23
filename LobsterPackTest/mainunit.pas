unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ColorBox, LCDrawPad;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnFlipVertical: TButton;
    BtnFlipHorizontal: TButton;
    BtnRotateLeft: TButton;
    BtnRotateRight: TButton;
    BtnRotate180: TButton;
    ColorBox1: TColorBox;
    LCDrawPad1: TLCDrawPad;
    procedure BtnFlipHorizontalClick(Sender: TObject);
    procedure BtnFlipVerticalClick(Sender: TObject);
    procedure BtnRotate180Click(Sender: TObject);
    procedure BtnRotateLeftClick(Sender: TObject);
    procedure BtnRotateRightClick(Sender: TObject);
    procedure ColorBox1Change(Sender: TObject);
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

procedure TForm1.ColorBox1Change(Sender: TObject);
begin
  LCDrawPad1.ForeColor:=ColorBox1.Selected;
end;

end.


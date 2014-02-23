unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCDrawPad;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnRotateLeft: TButton;
    BtnRotateRight: TButton;
    LCDrawPad1: TLCDrawPad;
    procedure BtnRotateLeftClick(Sender: TObject);
    procedure BtnRotateRightClick(Sender: TObject);
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

procedure TForm1.BtnRotateRightClick(Sender: TObject);
begin
  LCDrawPad1.Rotate(rm90Right);
end;

end.


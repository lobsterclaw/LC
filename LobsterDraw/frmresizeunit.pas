unit frmresizeunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin;

type

  { TFrmResize }

  TFrmResize = class(TForm)
    BtnOkay: TButton;
    BtnCancel: TButton;
    EdtHeight: TSpinEdit;
    GrpResize: TGroupBox;
    EdtWidth: TSpinEdit;
    LblHeight: TLabel;
    LblWidth: TLabel;
    procedure GrpResizeClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FrmResize: TFrmResize;

implementation

{$R *.lfm}

{ TFrmResize }

procedure TFrmResize.GrpResizeClick(Sender: TObject);
begin

end;

end.


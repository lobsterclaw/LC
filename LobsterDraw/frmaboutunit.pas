unit frmaboutunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls, fpreadpng;

type

  { TFrmAbout }

  TFrmAbout = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    TabVersion: TTabSheet;
    TabAck: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label10Click(Sender: TObject);
    procedure Label11Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure Label8Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FrmAbout: TFrmAbout;

implementation

uses LCLIntf, LResources;

{$R *.lfm}

{ TFrmAbout }

procedure TFrmAbout.Button1Click(Sender: TObject);
begin
  Close();
end;

procedure TFrmAbout.FormCreate(Sender: TObject);
begin
  Image1.Picture.LoadFromLazarusResource('lobsterdrawlogo');
end;

procedure TFrmAbout.Label10Click(Sender: TObject);
begin
  OpenURL('http://famfamfam.com/lab/icons/silk/');
end;

procedure TFrmAbout.Label11Click(Sender: TObject);
begin
  OpenURL('http://tango.freedesktop.org/Tango_Icon_Library');
end;

procedure TFrmAbout.Label5Click(Sender: TObject);
begin
  OpenURL('http://lobsterclawsoftware.com');
end;

procedure TFrmAbout.Label8Click(Sender: TObject);
begin
  OpenURL('http://wiki.freepascal.org/BGRABitmap');
end;

initialization
  {$I lobsterdrawlogo.lrs}

end.


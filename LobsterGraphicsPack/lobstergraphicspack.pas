{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LobsterGraphicsPack;

interface

uses
  LCDrawPad, lcutils, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LCDrawPad', @LCDrawPad.Register);
end;

initialization
  RegisterPackage('LobsterGraphicsPack', @Register);
end.

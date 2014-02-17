{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LobsterPack;

interface

uses
  LCDrawPad, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LCDrawPad', @LCDrawPad.Register);
end;

initialization
  RegisterPackage('LobsterPack', @Register);
end.

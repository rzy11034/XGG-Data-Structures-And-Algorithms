unit X_DSA.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  StrUtils;

procedure Run;

implementation

uses
  X_DSA.HuffmanCode;

procedure Run;
var
  b: integer;
  s, c: string;

begin
  s := '10';
  c := '';
  BinToHex(pchar(s), pchar(c), 8);
  Main;
end;

end.

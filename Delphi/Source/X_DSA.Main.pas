unit X_DSA.Main;

interface

uses
  System.SysUtils,
  System.Classes;

procedure Run;

implementation

uses
  X_DSA.HuffmanCode;

procedure Run;
var
  s: string;
  i: integer;
begin
  //s:='10101000';
  s := #$30;

  i := Integer.Parse(s);
  Main;
end;

end.

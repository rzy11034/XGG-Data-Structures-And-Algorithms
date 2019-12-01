unit X_DSA.SeqSearch;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

procedure Main;
function SeqSearch(const arr: array of integer; Value: integer): integer;

implementation

procedure Main;
var
  a: array of integer;
begin
  a := [1, 2, 3, 4, 5, 6, 7, 8, 9];
  WriteLn(SeqSearch(a, 3));
end;

function SeqSearch(const arr: array of integer; Value: integer): integer;
var
  i: integer;
begin
  for i := 0 to Length(arr) - 1 do
  begin
    if Value = arr[i] then
    begin
      Result := i;
      Exit;
    end;
  end;

  Result := -1;
end;

end.

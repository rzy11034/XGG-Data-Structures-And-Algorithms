unit X_DSA.BinarySortTreeDemo;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  X_DSA.BinarySortTree;

procedure Main;

implementation

procedure Main;
var
  bst: TBinarySortTree;
  arr: array of integer;
  i: integer;
begin
  arr := [7, 3, 10, 12, 5, 1, 9, 2];

  bst := TBinarySortTree.Create;
  for i := 0 to Length(arr) - 1 do
    bst.Add(arr[i]);

  bst.InfixOrder;
  WriteLn;

  for i := 0 to Length(arr) - 1 do
  begin
    bst.Del(arr[i]);
    bst.InfixOrder;
    Write('  Count = ', bst.Count);
    writeln;
  end;
end;

end.

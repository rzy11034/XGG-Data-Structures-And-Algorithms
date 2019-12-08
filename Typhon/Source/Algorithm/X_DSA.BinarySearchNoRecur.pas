unit X_DSA.BinarySearchNoRecur;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

procedure Main;
function BinarySearchNoRecur(arr: array of integer; target: integer): integer;

implementation

procedure Main;
var
  arr: array of integer;
  index: integer;
begin
  arr := [1, 3, 8, 10, 11, 67, 100];
  index := BinarySearchNoRecur(arr, 11);
  WriteLn('index=', index);
end;

function BinarySearchNoRecur(arr: array of integer; target: integer): integer;
var
  l, r, mid: integer;
begin
  Result := -1;

  l := Low(arr);
  r := High(arr);

  while l <= r do
  begin
    mid := l + (r - l) div 2;

    if target < arr[mid] then
      r := mid - 1
    else if target > arr[mid] then
      l := mid + 1
    else
      Exit(mid);
  end;
end;

end.

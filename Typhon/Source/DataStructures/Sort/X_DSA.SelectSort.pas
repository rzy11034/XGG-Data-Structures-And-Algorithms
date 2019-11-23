unit X_DSA.SelectSort;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

procedure Main;
procedure SelectSort(var arr: array of integer);

implementation

procedure Main;
var
  a: array of integer;
  i: integer;
begin
  a := [8, 3, 2, 1];
  SelectSort(a);

  for i := 0 to Length(a) - 1 do
    Write(a[i], ' ');

  WriteLn;
end;

procedure SelectSort(var arr: array of integer);
var
  i, j, minIndex, min: integer;
begin
  for i := 0 to Length(arr) - 1 do
  begin
    minIndex := i;

    for j := i + 1 to Length(arr) - 1 do
    begin
      if arr[minIndex] > arr[j] then
        minIndex := j;
    end;

    if minIndex <> i then
    begin
      min := arr[minIndex];
      arr[minIndex] := arr[i];
      arr[i] := min;
    end;
  end;
end;

end.

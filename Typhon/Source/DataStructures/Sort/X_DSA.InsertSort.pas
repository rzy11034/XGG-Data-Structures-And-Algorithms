unit X_DSA.InsertSort;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

procedure Main;
procedure InsertSort(var arr: array of integer);

implementation

procedure Main;
var
  a: array of integer;
  i: integer;
begin
  a := [8, 3, 2, 1];
  InsertSort(a);

  for i := 0 to Length(a) - 1 do
    Write(a[i], ' ');

  WriteLn;
end;

procedure InsertSort(var arr: array of integer);
var
  i, insertindex, insertVal: integer;
begin
  for i := 1 to Length(arr) - 1 do
  begin
    insertVal := arr[i];
    insertIndex := i - 1;

    while (insertindex >= 0) and (insertval < arr[insertindex]) do
    begin
      arr[insertindex + 1] := arr[insertindex];
      insertindex -= 1;
    end;

    arr[insertindex + 1] := insertVal;
  end;
end;

end.

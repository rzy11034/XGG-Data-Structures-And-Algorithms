unit X_DSA.InsertSort;

interface

uses
  System.Generics.Defaults;

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
    write(a[i], ' ');

  WriteLn;
end;

procedure InsertSort(var arr: array of integer);
var
  i, insertindex, insertVal: integer;
begin
  for i := 1 to Length(arr) - 1 do
  begin
    insertVal := arr[i];
    insertindex := i - 1;

    while (insertindex >= 0) and (insertVal < arr[insertindex]) do
    begin
      arr[insertindex + 1] := arr[insertindex];
      insertindex := insertindex - 1;
    end;

    arr[insertindex + 1] := insertVal;
  end;
end;

end.

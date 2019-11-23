unit X_DSA.BubbleSort;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

procedure Main;
procedure BubbleSort(var arr: array of integer);

implementation

procedure Main;
var
  a: array of integer;
  i: integer;
begin
  a := [8, 8, 8, 5, 4, 3, 2, 1];
  BubbleSort(a);

  for i := 0 to Length(a) - 1 do
    Write(a[i], ' ');

  WriteLn;
end;

procedure BubbleSort(var arr: array of integer);
var
  i, j, n: integer;
  isSwap: boolean;
  tmp: integer;
begin
  n := Length(arr) - 2;
  isSwap := False;

  for i := 0 to n do
  begin
    for j := 0 to n - i do
    begin
      if arr[j] > arr[j + 1] then
      begin
        tmp := arr[j + 1];
        arr[j + 1] := arr[j];
        arr[j] := tmp;

        isSwap := True;
      end;
    end;

    if isSwap then
    begin
      isSwap := False;
    end
    else
      break;
  end;
end;

end.

unit X_DSA.ShellSort;

interface

procedure Main;
procedure ShellSort(var arr: array of integer);

implementation

procedure Main;
var
  a: array of integer;
  i: integer;
begin
  a := [8, 3, 234, 1, 7, 424, 8, 24, 9, 4, 5, 6, 4, 0];
  ShellSort(a);

  for i := 0 to Length(a) - 1 do
    Write(a[i], ' ');

  WriteLn;
end;

procedure ShellSort(var arr: array of integer);
var
  gap, i, j, tmp: integer;
begin
  // 增量 gap, 并逐步的缩小增量
  gap := Length(arr) div 2;
  while gap > 0 do
  begin
    for i := gap to Length(arr) - 1 do
    begin
      j := i;
      tmp := arr[i];

      // 从第 gap个元素，逐个对其所在的组进行直接插入排序
      if arr[j] < arr[j - gap] then
      begin
        while (j - gap >= 0) and (tmp < arr[j - gap]) do
        begin
          arr[j] := arr[j - gap];
          j := j - gap;
        end;

        arr[j] := tmp;
      end;
    end;

    gap := gap div 2;
  end;
end;

end.

unit X_DSA.QuickSort;

interface

procedure Main;
procedure QuickSort(var arr: array of integer);

implementation

procedure Main;
var
  a: array of integer;
  i: integer;
begin
  a := [8, 3, 234, 1, 7, 424, 8, 24, 9, 4, 5, 6, 4, 0];
  QuickSort(a);

  for i := 0 to Length(a) - 1 do
    Write(a[i], ' ');

  WriteLn;
end;

procedure __quickSort(var arr: array of integer; left, right: integer);
var
  pivot, tmp, r, l: integer;
begin
  l := left;
  r := right;
  // pivot 中轴值
  pivot := arr[l + (r - l) div 2];

  // while循环的目的是让比 pivot 值小放到左边, 比 pivot 值大放到右边
  while l < r do
  begin
    // 在 pivot 的左边一直找,找到大于等于 pivot值,才退出
    while arr[l] < pivot do
      Inc(l);

    // 在 pivot 的右边一直找,找到小于等于 pivot值,才退出
    while arr[r] > pivot do
      Dec(r);

    // 如果 l >= r 说明 pivot 的左右两的值，
    // 已经左边全部是小于等于 pivot 值, 右边全部是大于等于 pivot值
    if l >= r then
      Break;

    // 交换
    tmp := arr[l];
    arr[l] := arr[r];
    arr[r] := tmp;

    // 如果交换完后，发现这个 arr[l] = pivot值 相等 r--， 前移
    if arr[l] = pivot then
      Dec(r);
    // 如果交换完后，发现这 个arr[r] == pivot值 相等 l++， 后移
    if arr[r] = pivot then
      Inc(l);
  end;

  // 如果 l == r, 必须 l+1, r-1, 否则为出现栈溢出
  if l = r then
  begin
    Inc(l);
    Dec(r);
  end;

  // 向左递归
  if left < r then
    __quickSort(arr, left, r);
  // 向右递归
  if right > l then
    __quickSort(arr, l, right);
end;

procedure QuickSort(var arr: array of integer);
begin
  __quickSort(arr, 0, Length(arr) - 1);
end;

end.

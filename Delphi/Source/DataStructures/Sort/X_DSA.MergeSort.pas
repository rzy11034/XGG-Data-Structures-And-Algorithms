unit X_DSA.MergeSort;

interface

procedure Main;
procedure MergeSort(var arr: array of integer);

implementation

procedure Main;
var
  a: array of integer;
  i: integer;
begin
  a := [8, 3, 234, 1, 7, 424, 8, 24, 9, 4, 5, 6, 4, 0];
  MergeSort(a);

  for i := 0 to Length(a) - 1 do
    write(a[i], ' ');

  WriteLn;
end;

procedure __merge(var arr, tmp: array of integer; left, right: integer);
var
  mid, i, j, t, tmpLeft: integer;
begin
  mid := left + (right - left) div 2; // 中间索引
  i := left; // 初始化 i, 左边有序序列的初始索引
  j := mid + 1; //初始化 j, 右边有序序列的初始索引
  t := 0; // 指向 tmp数组的当前索引

  // (一)
  // 先把左右两边(有序)的数据按照规则填充到 tmp 数组
  // 直到左右两边的有序序列，有一边处理完毕为止
  while (i <= mid) and (j <= right) do
  begin
    // 如果左边的有序序列的当前元素，小于等于右边有序序列的当前元素
    // 即将左边的当前元素，填充到 tmp数组, 然后 t+1, i+1
    // 否则, 将右边有序序列的当前元素，填充到temp数组
    if arr[i] <= arr[j] then
    begin
      tmp[t] := arr[i];
      Inc(t);
      Inc(i);
    end
    else
    begin
      tmp[t] := arr[j];
      Inc(t);
      Inc(j);
    end;
  end;

  // (二)
  // 把有剩余数据的一边的数据依次全部填充到 tmp
  //左边的有序序列还有剩余的元素，就全部填充到 tmp
  while i <= mid do
  begin
    tmp[t] := arr[i];
    Inc(t);
    Inc(i);
  end;

  // 右边的有序序列还有剩余的元素，就全部填充到 tmp
  while j <= right do
  begin
    tmp[t] := arr[j];
    Inc(t);
    Inc(j);
  end;

  // (三)
  // 将 tmp 数组的元素拷贝到 arr
  // 注意，并不是每次都拷贝所有
  t := 0;
  tmpLeft := left;

  while (tmpLeft <= right) do
  begin
    arr[tmpLeft] := tmp[t];
    Inc(t);
    Inc(tmpLeft);
  end;
end;

// 分+合方法
procedure __mergeSort(var arr, tmp: array of integer; left, right: integer);
var
  mid: integer;
begin
  if left < right then
  begin
    mid := left + (right - left) div 2;

    __mergeSort(arr, tmp, left, mid);
    __mergeSort(arr, tmp, mid + 1, right);
    __merge(arr, tmp, left, right);
  end;
end;

procedure MergeSort(var arr: array of integer);
var
  tmp: array of integer;
begin
  SetLength(tmp, Length(arr));
  __mergeSort(arr, tmp, 0, Length(arr) - 1);
end;

end.

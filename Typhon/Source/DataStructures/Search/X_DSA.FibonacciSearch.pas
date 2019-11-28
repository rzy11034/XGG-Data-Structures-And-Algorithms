unit X_DSA.FibonacciSearch;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  TArrayOfInt = specialize TArray<integer>;

const
  MAX_SIZE = 20;

procedure Main;
function FibonacciSearch(const arr: TArrayOfInt; key: integer): integer;

implementation

procedure Main;
var
  arr: TArrayOfInt;
begin
  arr := [1, 8, 10, 89, 1000, 1234];

  WriteLn(FibonacciSearch(arr, 1234));
end;

// 因为 mid=low+F(k-1)-1，需要使用到斐波那契数列，
// 因此需要先非递归方法得到一个斐波那契数列  //
function Fib: TArrayOfInt;
var
  f: TArrayOfInt;
  i: integer;
begin
  SetLength(f, MAX_SIZE);
  f[0] := 1;
  f[1] := 1;
  for i := 2 to MAX_SIZE - 1 do
    f[i] := f[i - 1] + f[i - 2];

  Result := f;
end;

function FibonacciSearch(const arr: TArrayOfInt; key: integer): integer;
var
  low, high, k, mid, i: integer;
  f, tmp: TArrayOfInt;
begin
  low := 0;
  high := Length(arr) - 1;
  k := 0; // 表示斐波那契分割数值的下标
  mid := 0; // 存放mid值
  f := Fib; // 获取到斐波那契数列

  // 获取到斐波那契分割数值的下标
  while high > (f[k] - 1) do
    Inc(k);

  // 因为 f[k] 值 可能大于 a 的 长度，因此需要使用A一个新的数组 tmp, copy(arr)
  // 使用 arr 数组最后的数填充 tmp
  tmp := Copy(arr);
  SetLength(tmp, f[k]);
  for i := high + 1 to Length(tmp) - 1 do
    tmp[i] := arr[high];

  while low <= high do
  begin
    mid := low + f[k - 1] - 1;

    if key < tmp[mid] then
    begin
      high := mid - 1;

      // 1. 全部元素 = 前面的元素 + 后边元素
      // 2. f[k] = f[k-1] + f[k-2]
      // 因为 前面有 f[k-1]个元素,所以可以继续拆分 f[k-1] = f[k-2] + f[k-3]
      // 即 在 f[k-1] 的前面继续查找 k--
      // 即下次循环 mid = f[k-1-1]-1
      Dec(k);
    end
    else if key > tmp[mid] then
    begin
      // 继续向数组的后面查找(右边)
      low := mid + 1;

      // 1. 全部元素 = 前面的元素 + 后边元素
      // 2. f[k] = f[k-1] + f[k-2]
      // 3. 因为后面我们有f[k-2] 所以可以继续拆分 f[k-1] = f[k-3] + f[k-4]
      // 4. 即在f[k-2] 的前面进行查找 k -=2
      // 5. 即下次循环 mid = f[k - 1 - 2] - 1
      Dec(k, 2);
    end
    else
    begin
      //需要确定，返回的是哪个下标
      if mid <= high then
        Exit(mid)
      else
        Exit(high);
    end;
  end;

  Result := -1;
end;


end.

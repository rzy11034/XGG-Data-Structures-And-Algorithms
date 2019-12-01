unit X_DSA.HeapSort;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

procedure Main;
procedure HeapSort(var arr: array of integer);

implementation

procedure Main;
var
  a: array of integer;
  i: integer;
begin
  a := [8, 3, 234, 1, 7, 424, 8, 24, 9, 4, 5, 6, 4, 0];
  HeapSort(a);

  for i := 0 to Length(a) - 1 do
    Write(a[i], ' ');

  WriteLn;
end;

// 将一个数组(二叉树), 调整成一个大顶堆
procedure __shiftDwon(var arr: array of integer; i: integer; length: integer);
var
  k, tmp: integer;
begin
  //先取出当前元素的值，保存在临时变量
  tmp := arr[i];

  // k = 2*i+1. k 是 i 结点的左子结点
  while (2 * i + 1) < length do
  begin
    k := 2 * i + 1;

    // 如果左子结点的值小于右子结点的值, k+1
    if ((k + 1) < length) and (arr[k] < arr[k + 1]) then
      Inc(k);

    // 如果子结点值小于父节点值，则退出循环
    // 否则，把较大的值赋给当前结点, i 指向 k, 继续循环比较
    if (arr[k] < tmp) then
      break
    else
    begin
      arr[i] := arr[k];
      i := k;
    end;
  end;

  // 当循环结束后，已经将以 i 为父结点的树的最大值，放在了最顶(局部)
  // 将tmp值放到调整后的位置
  arr[i] := tmp;
end;

procedure HeapSort(var arr: array of integer);
var
  i, tmp: integer;
begin
  // 1.将无序序列构建成一个堆，根据升序降序需求选择大顶堆
  for i := (Length(arr) - 1 - 1) div 2 downto 0 do
    __shiftDwon(arr, i, Length(arr));

  // 2.将堆顶元素与末尾元素交换，将最大元素"沉"到数组末端;
  // 3.重新调整结构，使其满足堆定义，然后继续交换堆顶元素与当前末尾元素，反复执行调整 + 交换步骤
  for i := Length(arr) - 1 downto 1 do
  begin
    tmp := arr[0];
    arr[0] := arr[i];
    arr[i] := tmp;

    __shiftDwon(arr, 0, i);
  end;
end;

end.

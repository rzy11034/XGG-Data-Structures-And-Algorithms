unit X_DSA.RadixSort;

interface

uses
  System.SysUtils,
  System.Math,
  System.Generics.Collections;

procedure Main;
procedure RadixSort(var arr: array of integer);

implementation

type
  TQueueOfInt = TQueue<integer>;

procedure Main;
var
  a: array of integer;
  i: integer;
begin
  a := [8, 3, 234, 1, 7, 424, 8, 24, 9, 4, 5, 6, 4, 0];
  RadixSort(a);

  for i := 0 to Length(a) - 1 do
    Write(a[i], ' ');

  WriteLn;
end;

procedure RadixSort(var arr: array of integer);
var
  maxDigits, digitOfElement, indexOfArr, i, j, n: integer;
  bucketArr: array of TQueueOfInt;
begin
  // 1. 得到数组中最大的数的位数
  maxDigits := MaxIntValue(arr).ToString.Length;

  //定义一个一维数组，表示10个桶, 每个桶就是一个队列
  //说明
  // 1. 数组包含10个队列
  // 2.基数排序是使用空间换时间的经典算法
  SetLength(bucketArr, 10);
  for i := 0 to Length(bucketArr) - 1 do
    bucketArr[i] := TQueueOfInt.Create;

  // 这里我们使用循环将代码处理, 第一次是个位，第二次是十位，第三次是百位..
  n := 1;
  for i := 1 to maxDigits do
  begin
    for j := 0 to Length(arr) - 1 do
    begin
      // 取出每个元素的对应位的值
      digitOfElement := arr[j] div n mod 10;

      //放入到对应的桶中
      bucketArr[digitOfElement].Enqueue(arr[j]);
    end;

    //按照桶的顺序, 依次取出数据，放入原来数组
    indexOfArr := 0;
    for j := 0 to Length(bucketArr) - 1 do
    begin
      while bucketArr[j].Count <> 0 do
      begin
        arr[indexOfArr] := bucketArr[j].Peek;
        Inc(indexOfArr);
        bucketArr[j].Dequeue;
      end;
    end;

    n := n * 10;
  end;
end;

end.

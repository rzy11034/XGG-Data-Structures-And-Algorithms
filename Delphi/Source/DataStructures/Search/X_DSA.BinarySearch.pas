unit X_DSA.BinarySearch;

interface

uses
  System.Generics.Collections;

type
  TListOfInt = TList<integer>;

procedure Main;
function BinarySearch(const arr: array of integer; Value: integer): integer;

  {*
   * 有多个相同的数值时，如何将所有的数值都查找到，比如这里的 1000
   *
   * 思路分析
   * 1. 在找到mid 索引值，不要马上返回
   * 2. 向mid 索引值的左边扫描，将所有满足 1000， 的元素的下标，加入到集合ArrayList
   * 3. 向mid 索引值的右边扫描，将所有满足 1000， 的元素的下标，加入到集合ArrayList
   * 4. 将Arraylist返回
   *}
function BinarySearch2(const arr: array of integer; Value: integer): TListOfInt;

implementation

procedure Main;
var
  a: array of integer;
  i: integer;
begin
  a := [1, 2, 3, 4, 5, 5, 5, 5, 5, 6, 7, 8, 9];

  if BinarySearch2(a, 5) <> nil then
    for i := 0 to BinarySearch2(a, 5).Count - 1 do
    begin
      Write(BinarySearch2(a, 5)[i], ' ');
    end
  else
    Write(' ');
end;

function __binarySearch(const arr: array of integer; left, right, Value: integer): integer;
var
  mid: integer;
begin
  if left > right then
  begin
    Result := -1;
    Exit;
  end;

  mid := left + (right - left) div 2;

  if Value < arr[mid] then
    Result := __binarySearch(arr, left, mid - 1, Value)
  else if Value > arr[mid] then
    Result := __binarySearch(arr, mid + 1, right, Value)
  else
    Result := mid;
end;

function BinarySearch(const arr: array of integer; Value: integer): integer;
begin
  Result := __binarySearch(arr, 0, Length(arr) - 1, Value);
end;

function __binarySearch2(const arr: array of integer; left, right, Value: integer): TListOfInt;
var
  mid, tmpL, tmpR, i: integer;
  ret: TListOfInt;
begin
  if left > right then
  begin
    Result := nil;
    Exit;
  end;

  mid := left + (right - left) div 2;

  if Value < arr[mid] then
    Result := __binarySearch2(arr, left, mid - 1, Value)
  else if Value > arr[mid] then
    Result := __binarySearch2(arr, mid + 1, right, Value)
  else
  begin
    // 向 mid 索引值的左边扫描，标记 arr[tmpL] = arr[mid] 的元素的起始下标
    tmpL := mid;
    while (tmpL - 1 >= 0) and (arr[tmpL - 1] = arr[mid]) do
      Dec(tmpL);

    // 向 mid 索引值的右边扫描，标记 arr[tmpR] = arr[mid] 的元素的终至下标
    tmpR := mid;
    while (tmpR + 1 < Length(arr)) and (arr[tmpR + 1] = arr[mid]) do
      Inc(tmpR);

    // 将 arr[tmpL .. tmpR] 区间的元素下标加入到 TListOfInt
    ret := TListOfInt.Create;
    for i := tmpL to tmpR do
      ret.Add(i);

    Result := ret;
  end;
end;

function BinarySearch2(const arr: array of integer; Value: integer): TListOfInt;
begin
  Result := __binarySearch2(arr, 0, Length(arr) - 1, Value);
end;

end.

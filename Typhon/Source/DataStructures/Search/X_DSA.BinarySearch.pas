unit X_DSA.BinarySearch;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  gvector;

type
  TVectorOfInt = specialize TVector<integer>;

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
function BinarySearch2(const arr: array of integer; Value: integer): TVectorOfInt;

implementation

procedure Main;
var
  a: array of integer;
begin
  a := [1, 2, 3, 4, 5, 6, 7, 8, 9];
  WriteLn(BinarySearch(a, 3));
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

function __binarySearch2(const arr: array of integer; left, right, Value: integer): TVectorOfInt;
var
  mid, tmp: integer;
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
    tmp := mid;

  end;
end;

function BinarySearch2(const arr: array of integer; Value: integer): TVectorOfInt;
begin
  Result := nil;
end;

end.

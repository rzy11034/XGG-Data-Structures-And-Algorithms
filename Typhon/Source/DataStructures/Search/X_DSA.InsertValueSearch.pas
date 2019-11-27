unit X_DSA.InsertValueSearch;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

procedure Main;
function InsertValueSearch(const arr: array of integer; findVal: integer): integer;

implementation

procedure Main;
var
  a: array of integer;
  i: integer;
begin
  SetLength(a, 100);
  for i := 0 to 99 do
    a[i] := i + 1;

  WriteLn(InsertValueSearch(a, 100), ' ');
end;

function __insertValueSearch(const arr: array of integer;
  left, right, findVal: integer): integer;
var
  mid: integer;
begin
  // 注意：findVal < arr[0]  和  findVal > arr[High(arr)] 必须需要
  // 否则得到的 mid 可能越界
  if (left > right) or (findVal < arr[0]) or (findVal > arr[High(arr)]) then
    Exit(-1);

  // 求出 mid
  mid := left + (right - left) * (findVal - arr[left]) div (arr[right] - arr[left]);

  if findVal < arr[mid] then
    Result := __insertValueSearch(arr, left, mid - 1, findVal)
  else if findVal > arr[mid] then
    Result := __insertValueSearch(arr, mid + 1, right, findVal)
  else
    Result := mid;
end;

function InsertValueSearch(const arr: array of integer; findVal: integer): integer;
begin
  Result := __insertValueSearch(arr, 0, Length(arr) - 1, findVal);
end;

end.

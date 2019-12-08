unit X_DSA.ViolenceMatch;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

procedure Main;
function ViolenceMatch(str1, str2: UnicodeString): integer;

implementation

procedure Main;
var
  str1, str2: UnicodeString;
begin
  str1 := '硅硅谷 尚硅谷你尚硅 尚硅谷你尚硅谷你尚硅你好';
  str2 := '尚硅谷你尚硅你';

  WriteLn(ViolenceMatch(str1, str2));
end;

function ViolenceMatch(str1, str2: UnicodeString): integer;

  function ToUnicodeCharArray(str: UnicodeString): TUnicodeCharArray;
  var
    chrArr: TUnicodeCharArray;
    c: UnicodeChar;
    i: integer;
  begin
    SetLength(chrArr, Length(str));

    i := 0;
    for c in str do
    begin
      chrArr[i] := c;
      i += 1;
    end;

    Result := chrArr;
  end;

var
  chr1, chr2: TUnicodeCharArray;
  j, i, ret: integer;
begin
  chr1 := ToUnicodeCharArray(str1);
  chr2 := ToUnicodeCharArray(str2);

  i := 0;
  j := 0;
  while (i < Length(chr1)) and (j < Length(chr2)) do // 保证匹配时，不越界
  begin
    // 如果匹配ok
    if chr1[i] = chr2[j] then
    begin
      i += 1;
      j += 1;
    end
    else
    begin
      i := i - (j - 1);
      j := 0;
    end;
  end;

  //判断是否匹配成功
  if j = Length(chr2) then
    ret := i - j
  else
    ret := -1;

  Result := ret;
end;

end.

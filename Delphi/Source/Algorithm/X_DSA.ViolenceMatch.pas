unit X_DSA.ViolenceMatch;

interface

uses
  System.SysUtils;

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
var
  chr1, chr2: TCharArray;
  j, i, ret: integer;
begin
  chr1 := str1.ToCharArray;
  chr2 := str2.ToCharArray;

  i := 0;
  j := 0;
  while (i < Length(chr1)) and (j < Length(chr2)) do // 保证匹配时，不越界
  begin
    // 如果匹配 ok
    if chr1[i] = chr2[j] then
    begin
      i := i + 1;
      j := j + 1;
    end
    else
    begin
      i := i - (j - 1);
      j := 0;
    end;
  end;

  // 判断是否匹配成功
  if j = Length(chr2) then
    ret := i - j
  else
    ret := -1;

  Result := ret;
end;

end.

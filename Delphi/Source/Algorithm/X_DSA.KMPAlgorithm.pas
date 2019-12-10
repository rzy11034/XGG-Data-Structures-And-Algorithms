unit X_DSA.KMPAlgorithm;

interface

uses
  System.SysUtils;

type
  TArrayOfInt = TArray<Integer>;

procedure Main;
function KmpSearch(const str1, str2: string): Integer;
function KmpNext(const str: string): TArrayOfInt;

implementation

procedure Main;
var
  str1, str2: string;
  index: Integer;
begin
  str1 := 'BBC ABCDAB ABCDABCDABDE';
  str2 := 'ABCDABD';
  //str2 := 'BBC';

  index := KmpSearch(str1, str2);
  WriteLn('index=', index);
end;

function KmpSearch(const str1, str2: string): Integer;
var
  Next: TArrayOfInt;
  j, i: Integer;
begin
  Next := KmpNext(str2);

  // 遍历
  j := 0;
  for i := 0 to str1.Length - 1 do
  begin

    //需要处理 str1.Chars[i] <> str2.Chars[j], 去调整 j 的大小
    //KMP算法核心点, 可以验证...
    while (j > 0) and (str1.Chars[i] <> str2.Chars[j]) do
      j := Next[j - 1];

    if str1.Chars[i] = str2.Chars[j] then
      j := j + 1;

    if j = str2.Length then //找到了 // j = 3 i
    begin
      Result := i - j + 1;
      Exit;
    end;
  end;

  Result := -1;
end;

function KmpNext(const str: string): TArrayOfInt;
var
  Next: TArrayOfInt;
  j, i: Integer;
begin
  // 创建一个 next 数组保存部分匹配值
  SetLength(Next, str.Length);
  Next[0] := 0; // 如果字符串是长度为 1, 部分匹配值就是 0

  j := 0;
  for i := 1 to str.Length - 1 do
  begin
    // 当 dest.charAt(i) != dest.charAt(j) ，我们需要从 next[j-1] 获取新的 j
    // 直到我们发现有 dest.charAt(i) = dest.charAt(j) 成立才退出
    // 这是 kmp 算法的核心点
    while (j > 0) and (str.Chars[i] <> str.Chars[j]) do
      j := Next[j - 1];

    //当 str.Chars[i] = str.Chars[j] 满足时，部分匹配值就 +1
    if str.Chars[i] = str.Chars[j] then
      Inc(j);

    Next[i] := j;
  end;

  Result := Next;
end;

end.

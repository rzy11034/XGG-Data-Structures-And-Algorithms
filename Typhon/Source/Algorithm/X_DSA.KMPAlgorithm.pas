unit X_DSA.KMPAlgorithm;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  Classes,
  SysUtils;

type
  TArrayOfInt = array of integer;

type
  TUnicodeStringHelper = type Helper for UnicodeString
  private
    function __getChar(index: integer): UnicodeChar;
    function __getLength: integer;
  public
    function ToUnicodeCharArray: TUnicodeCharArray;
    property Chars[index: integer]: UnicodeChar read __getChar;
    property Length: integer read __getLength;
  end;

procedure Main;
function KmpSearch(const str1, str2: UnicodeString): integer;
function KmpNext(const str: UnicodeString): TArrayOfInt;

implementation

procedure Main;
var
  str1, str2: UnicodeString;
  index: integer;
begin
  str1 := 'BBC ABCDAB ABCDABCDABDE';
  str2 := 'ABCDABD';
  //str2 := 'BBC';

  index := KmpSearch(str1, str2);
  WriteLn('index=', index);
end;

function KmpSearch(const str1, str2: UnicodeString): integer;
var
  Next: TArrayOfInt;
  j, i: integer;
begin
  Next := kmpNext(str2);

  // 遍历
  j := 0;
  for i := 0 to str1.Length - 1 do
  begin

    //需要处理 str1.Chars[i] <> str2.Chars[j], 去调整 j 的大小
    //KMP算法核心点, 可以验证...
    while (j > 0) and (str1.Chars[i] <> str2.Chars[j]) do
      j := Next[j - 1];


    if str1.Chars[i] = str2.Chars[j] then
      j += 1;

    if j = str2.Length then //找到了 // j = 3 i
    begin
      Result := i - j + 1;
      Exit;
    end;
  end;

  Result := -1;
end;

function KmpNext(const str: UnicodeString): TArrayOfInt;
var
  Next: TArrayOfInt;
  j, i: integer;
begin
  // 创建一个 next 数组保存部分匹配值
  SetLength(Next, str.Length);
  Next[0] := 0; // 如果字符串是长度为 1, 部分匹配值就是 0

  j := 0;
  for i := 1 to str.Length - 1 do
  begin
    // 当 dest.charAt(i) <> dest.charAt(j) ，我们需要从 next[j-1] 获取新的 j
    // 直到我们发现有 dest.charAt(i) = dest.charAt(j) 成立才退出
    // 这是 kmp 算法的核心点
    while (j > 0) and (str.Chars[i] <> str.Chars[j]) do
      j := Next[j - 1];

    //当 str.Chars[i] = str.Chars[j] 满足时，部分匹配值就 +1
    if str.Chars[i] = str.Chars[j] then
      j += 1;

    Next[i] := j;
  end;

  Result := Next;
end;

{ TUnicodeStringHelper }

function TUnicodeStringHelper.ToUnicodeCharArray: TUnicodeCharArray;
var
  chrArr: TUnicodeCharArray;
  c: UnicodeChar;
  i: integer;
begin
  SetLength(chrArr, Self.Length);

  i := 0;
  for c in Self do
  begin
    chrArr[i] := c;
    i += 1;
  end;

  Result := chrArr;
end;

function TUnicodeStringHelper.__getChar(index: integer): UnicodeChar;
begin
  Result := Self[index + 1];
end;

function TUnicodeStringHelper.__getLength: integer;
begin
  Result := system.Length(Self);
end;

end.

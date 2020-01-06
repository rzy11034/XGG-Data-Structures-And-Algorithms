unit X_DSA.Utils;

interface

uses
  System.SysUtils,
  System.Classes;

type
  UChar = Char;
  UString = string;

  TLAUtils = class
  public
    class procedure DrawLine;
  end;

  TSortTestHelper = class
  private type
    TArrayOfInt = tArray<Integer>;
    TSorts = procedure(var arr: array of integer);

  var
    function __isSorted(arr: TArrayOfInt): boolean;

  public
    function CopyArray(const arr: TArrayOfInt): TArrayOfInt;
    procedure PrintArray(const arr: TArrayOfInt);
    /// <summary> 生成有n个元素的随机数组,每个元素的随机范围为[1, range]。 </summary>
    function GenerateRandomArray(arrSize, range: integer): TArrayOfInt;
    /// <summary> 生成有n个元素的有序数组, 并执行swapTime乱序次数。 </summary>
    function GenerateNearlyOrderedArray(arrSize, swapTime: integer): TArrayOfInt;
    procedure TestSort(sortName: string; var arr: TArrayOfInt; pSort: TSorts);
  end;

resourcestring
  END_OF_PROGRAM_EN = 'Press any key to continue...';
  END_OF_PROGRAM_CH = '按任意键继续...';

implementation

{ TLAUtils }

class procedure TLAUtils.DrawLine;
var
  i: integer;
begin
  for i := 0 to 70 do
  begin
    Write('-');
  end;
  Writeln;
end;

{ TSortTestHelper }

function TSortTestHelper.CopyArray(const arr: TArrayOfInt): TArrayOfInt;
begin
  Result := Copy(arr);
end;

function TSortTestHelper.GenerateNearlyOrderedArray(arrSize, swapTime: integer): TArrayOfInt;
var
  arr1: TArrayOfInt;
  i, tmp, posX, posY: integer;
begin
  SetLength(arr1, arrSize);
  for i := 0 to arrSize - 1 do
    arr1[i] := i;

  Randomize;
  for i := 0 to swapTime - 1 do
  begin
    posX := Random(arrSize) mod arrSize;
    posY := Random(arrSize) mod arrSize;

    tmp := arr1[posX];
    arr1[posX] := arr1[posY];
    arr1[posY] := tmp;
  end;

  Result := arr1;
end;

function TSortTestHelper.GenerateRandomArray(arrSize, range: integer): TArrayOfInt;
var
  arr: TArrayOfInt;
  i: integer;
begin
  SetLength(arr, arrSize);
  Randomize;

  for i := 0 to arrSize - 1 do
    arr[i] := Random(range);

  Result := arr;
end;

procedure TSortTestHelper.PrintArray(const arr: TArrayOfInt);
var
  i: integer;
begin
  for i := 0 to Length(arr) - 1 do
  begin
    Write(arr[i], ' ');
  end;

  Writeln;
end;

procedure TSortTestHelper.TestSort(sortName: string; var arr: TArrayOfInt; pSort: TSorts);
var
  startTime, endTime: cardinal;
begin
  startTime := TThread.GetTickCount;
  pSort(arr);
  endTime := TThread.GetTickCount;

  // --------------
  if not(__isSorted(arr)) then
    raise Exception.Create('Sort Error.');

  Write(sortName, ' Size is: ', Length(arr).ToString, '. ');
  Writeln('Total Time : ', ((endTime - startTime) / 1000).ToString, ' s');
end;

function TSortTestHelper.__isSorted(arr: TArrayOfInt): boolean;
var
  i: integer;
begin
  for i := 1 to Length(arr) - 1 do
  begin
    if arr[i - 1] > arr[i] then
    begin
      Result := False;
      Exit;
    end;
  end;

  Result := True;
end;

end.

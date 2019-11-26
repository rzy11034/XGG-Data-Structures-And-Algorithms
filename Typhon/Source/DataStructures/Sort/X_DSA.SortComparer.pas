unit X_DSA.SortComparer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  X_DSA.ShellSort,
  X_DSA.QuickSort,
  X_DSA.RadixSort,
  X_DSA.MergeSort,
  X_DSA.Utils;

procedure Main;

implementation

procedure Main;
var
  n, swapTimes: integer;
  sourceArr, targetArr: array of integer;
begin
  n := 1000000;

  WriteLn('Test for random array, size = ', n, ', random range [0 .. ', n, ']');
  with TSortTestHelper.Create do
  begin
    sourceArr := GenerateRandomArray(n, n);

    targetArr := CopyArray(sourceArr);
    TestSort('MergeSort'#9#9, targetArr, @MergeSort);

    targetArr := CopyArray(sourceArr);
    TestSort('ShellSort'#9#9, targetArr, @ShellSort);

    targetArr := CopyArray(sourceArr);
    TestSort('QuickSort'#9#9, targetArr, @QuickSort);

    targetArr := CopyArray(sourceArr);
    TestSort('RadixSort'#9#9, targetArr, @RadixSort);

    Free;
  end;

  swapTimes := 100;
  WriteLn('Test for nearly ordered array, size = ', n, ', swap time = ', swapTimes);
  with TSortTestHelper.Create do
  begin
    sourceArr := GenerateNearlyOrderedArray(n, swapTimes);

    sourceArr := GenerateRandomArray(n, n);

    targetArr := CopyArray(sourceArr);
    TestSort('MergeSort'#9#9, targetArr, @MergeSort);

    targetArr := CopyArray(sourceArr);
    TestSort('ShellSort'#9#9, targetArr, @ShellSort);

    targetArr := CopyArray(sourceArr);
    TestSort('QuickSort'#9#9, targetArr, @QuickSort);

    targetArr := CopyArray(sourceArr);
    TestSort('RadixSort'#9#9, targetArr, @RadixSort);

    Free;
  end;

  WriteLn('Test for random array, size = ', n, ', random range [0 .. ', 10, ']');
  with TSortTestHelper.Create do
  begin
    sourceArr := GenerateRandomArray(n, 10);

    sourceArr := GenerateRandomArray(n, n);

    targetArr := CopyArray(sourceArr);
    TestSort('MergeSort'#9#9, targetArr, @MergeSort);

    targetArr := CopyArray(sourceArr);
    TestSort('ShellSort'#9#9, targetArr, @ShellSort);

    targetArr := CopyArray(sourceArr);
    TestSort('QuickSort'#9#9, targetArr, @QuickSort);

    targetArr := CopyArray(sourceArr);
    TestSort('RadixSort'#9#9, targetArr, @RadixSort);

    Free;
  end;
end;

end.

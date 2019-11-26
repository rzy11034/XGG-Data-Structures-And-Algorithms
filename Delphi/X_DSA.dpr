program X_DSA;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  X_DSA.Main in 'Source\X_DSA.Main.pas',
  X_DSA.Utils in 'Source\X_DSA.Utils.pas',
  X_DSA.CircularLinkedList in 'Source\DataStructures\LinkedList\X_DSA.CircularLinkedList.pas',
  X_DSA.DoubleLinkedList in 'Source\DataStructures\LinkedList\X_DSA.DoubleLinkedList.pas',
  X_DSA.LinkedList in 'Source\DataStructures\LinkedList\X_DSA.LinkedList.pas',
  X_DSA.MiGong in 'Source\DataStructures\Recursion\X_DSA.MiGong.pas',
  X_DSA.Queen8 in 'Source\DataStructures\Recursion\X_DSA.Queen8.pas',
  X_DSA.SparseArray in 'Source\DataStructures\SparseArray\X_DSA.SparseArray.pas',
  X_DSA.InversePolishCalculator in 'Source\DataStructures\Stack\X_DSA.InversePolishCalculator.pas',
  X_DSA.BubbleSort in 'Source\DataStructures\Sort\X_DSA.BubbleSort.pas',
  X_DSA.SelectSort in 'Source\DataStructures\Sort\X_DSA.SelectSort.pas',
  X_DSA.InsertSort in 'Source\DataStructures\Sort\X_DSA.InsertSort.pas',
  X_DSA.ShellSort in 'Source\DataStructures\Sort\X_DSA.ShellSort.pas',
  X_DSA.QuickSort in 'Source\DataStructures\Sort\X_DSA.QuickSort.pas',
  X_DSA.MergeSort in 'Source\DataStructures\Sort\X_DSA.MergeSort.pas',
  X_DSA.RadixSort in 'Source\DataStructures\Sort\X_DSA.RadixSort.pas',
  X_DSA.SortComparer in 'Source\DataStructures\Sort\X_DSA.SortComparer.pas';

begin
  try
    Run;
    TLAUtils.DrawLine;
    Writeln(END_OF_PROGRAM_EN);
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

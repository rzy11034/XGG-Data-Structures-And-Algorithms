﻿program X_DSA;

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
  X_DSA.SortComparer in 'Source\DataStructures\Sort\X_DSA.SortComparer.pas',
  X_DSA.SeqSearch in 'Source\DataStructures\Search\X_DSA.SeqSearch.pas',
  X_DSA.BinarySearch in 'Source\DataStructures\Search\X_DSA.BinarySearch.pas',
  X_DSA.InsertValueSearch in 'Source\DataStructures\Search\X_DSA.InsertValueSearch.pas',
  X_DSA.FibonacciSearch in 'Source\DataStructures\Search\X_DSA.FibonacciSearch.pas',
  X_DSA.HashTableDemo in 'Source\DataStructures\HashTable\X_DSA.HashTableDemo.pas',
  X_DSA.ArrBinaryTreeDemo in 'Source\DataStructures\Tree\X_DSA.ArrBinaryTreeDemo.pas',
  X_DSA.ThreadedBinaryTreeDemo in 'Source\DataStructures\Tree\X_DSA.ThreadedBinaryTreeDemo.pas',
  X_DSA.HeapSort in 'Source\DataStructures\Sort\X_DSA.HeapSort.pas',
  X_DSA.HuffmanTree in 'Source\DataStructures\Tree\X_DSA.HuffmanTree.pas',
  X_DSA.HuffmanCode in 'Source\DataStructures\Tree\X_DSA.HuffmanCode.pas',
  X_DSA.BinarySortTreeDemo in 'Source\DataStructures\Tree\X_DSA.BinarySortTreeDemo.pas',
  X_DSA.BinarySortTree in 'Source\DataStructures\Tree\X_DSA.BinarySortTree.pas',
  X_DSA.Graph in 'Source\DataStructures\Graph\X_DSA.Graph.pas',
  X_DSA.GraphDemo in 'Source\DataStructures\Graph\X_DSA.GraphDemo.pas',
  X_DSA.BinarySearchNoRecur in 'Source\Algorithm\X_DSA.BinarySearchNoRecur.pas',
  X_DSA.HanoiTower in 'Source\Algorithm\X_DSA.HanoiTower.pas',
  X_DSA.KnapsackProblem in 'Source\Algorithm\X_DSA.KnapsackProblem.pas',
  X_DSA.ViolenceMatch in 'Source\Algorithm\X_DSA.ViolenceMatch.pas',
  X_DSA.KMPAlgorithm in 'Source\Algorithm\X_DSA.KMPAlgorithm.pas',
  X_DSA.GreedyAlgorithm in 'Source\Algorithm\X_DSA.GreedyAlgorithm.pas',
  X_DSA.PrimAlgorithm in 'Source\Algorithm\X_DSA.PrimAlgorithm.pas',
  X_DSA.KruskalCase in 'Source\Algorithm\X_DSA.KruskalCase.pas',
  X_DSA.DijkstraAlgorithm in 'Source\Algorithm\X_DSA.DijkstraAlgorithm.pas',
  X_DSA.FloydAlgorithm in 'Source\Algorithm\X_DSA.FloydAlgorithm.pas',
  X_DSA.HorseChessboard in 'Source\Algorithm\X_DSA.HorseChessboard.pas';

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

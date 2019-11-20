program X_DSA;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  X_DSA.Main in 'Source\X_DSA.Main.pas',
  X_DSA.Utils in 'Source\X_DSA.Utils.pas',
  X_DSA.SparseArray in 'Source\Code_Implementation\X_DSA.SparseArray.pas',
  X_DSA.LinkedList in 'Source\Code_Implementation\X_DSA.LinkedList.pas',
  X_DSA.DoubleLinkedList in 'Source\Code_Implementation\X_DSA.DoubleLinkedList.pas',
  X_DSA.CircularLinkedList in 'Source\Code_Implementation\X_DSA.CircularLinkedList.pas',
  X_DSA.InversePolishCalculator in 'Source\Code_Implementation\X_DSA.InversePolishCalculator.pas';

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

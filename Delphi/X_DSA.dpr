program X_DSA;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  X_DSA.Main in 'Source\X_DSA.Main.pas',
  X_DSA.Utils in 'Source\X_DSA.Utils.pas',
  X_DSA.C009_SparseArray in 'Source\Code_Implementation\X_DSA.C009_SparseArray.pas',
  X_DSA.C021_22_23 in 'Source\Code_Implementation\X_DSA.C021_22_23.pas';

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

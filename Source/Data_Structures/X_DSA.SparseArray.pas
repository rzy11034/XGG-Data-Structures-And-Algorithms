unit X_DSA.SparseArray;

interface

type
  TSparseArray<T> = class(TObject)
  type
    TData =  TArray<TArray<T>>;
  private
    __data: TData;
  public
    constructor Create(;
    destructor Destroy; override; 
  end;

procedure Main;

implementation

procedure Main;
var
  chess: TArray<TArray<Integer>>;
  i: Integer;
  j: Integer;
begin
  SetLength(chess, 11, 11);

  chess[1][2] := 1;
  chess[2][3] := 2;

  for i := 0 to Length(chess) - 1 do
  begin
    for j := 0 to Length(chess[i]) - 1 do
    begin
      write(chess[i][j], ' ');
    end;
    Writeln;
  end;

end;

end.

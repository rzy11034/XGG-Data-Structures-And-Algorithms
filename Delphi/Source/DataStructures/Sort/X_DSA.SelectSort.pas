unit X_DSA.SelectSort;

interface

uses
  System.Generics.Defaults;

type
  TSelectSort<T> = class
  private type
    TArrayOfT = TArray<T>;
  public
    class procedure Sort(var arr: TArrayOfT);
  end;

procedure Main;

implementation

type
  TSelectSorts = TSelectSort<integer>;

procedure Main;
var
  a: TArray<integer>;
  i: integer;
begin
  a := [8, 3, 2, 1];
  TSelectSorts.Sort(a);

  for i := 0 to Length(a) - 1 do
    Write(a[i], ' ');

  WriteLn;
end;

{ TSelectSort }

class procedure TSelectSort<T>.Sort(var arr: TArrayOfT);
var
  i, j, minIndex: integer;
  min: T;
  cmp: IComparer<T>;
begin
  cmp := TComparer<T>.Default;

  for i := 0 to Length(arr) - 1 do
  begin
    minIndex := i;

    for j := i + 1 to Length(arr) - 1 do
    begin
      if cmp.Compare(arr[minIndex], arr[j]) > 0 then
        minIndex := j;
    end;

    if minIndex <> i then
    begin
      min := arr[minIndex];
      arr[minIndex] := arr[i];
      arr[i] := min;
    end;
  end;
end;

end.

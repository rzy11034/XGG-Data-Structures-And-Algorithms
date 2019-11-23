unit X_DSA.BubbleSort;

interface

uses
  System.Generics.Defaults,
  System.Generics.Collections;

type
  TBubbleSort<T> = class
  private type
    TArrayOfT = TArray<T>;
  public
    class procedure Sort(var arr: TArrayOfT);
  end;

procedure Main;

implementation

type
  TBubbleSorts = TBubbleSort<integer>;

procedure Main;
var
  a: TArray<integer>;
  i: integer;
begin
  a := [8, 8, 8, 5, 4, 3, 2, 1];
  TBubbleSorts.Sort(a);

  for i := 0 to Length(a) - 1 do
    Write(a[i], ' ');

  WriteLn;
end;

{ TBubbleSort }

class procedure TBubbleSort<T>.Sort(var arr: TArrayOfT);
var
  i, j, n: integer;
  isSwap: boolean;
  tmp: T;
  cmp: IComparer<T>;
begin
  n := Length(arr) - 2;
  isSwap := False;
  cmp := TComparer<T>.Default;

  for i := 0 to n do
  begin
    for j := 0 to n - i do
    begin
      if cmp.Compare(arr[j], arr[j + 1]) > 0 then
      begin
        tmp := arr[j + 1];
        arr[j + 1] := arr[j];
        arr[j] := tmp;

        isSwap := True;
      end;
    end;

    if isSwap then
    begin
      isSwap := False;
    end
    else
      break;
  end;
end;

end.

unit X_DSA.ArrBinaryTreeDemo;

interface

type
  TArrayOfInt = TArray<integer>;

  TArrBinaryTree = class
  private
    __data: TArrayOfInt;

    procedure __preOrder(index: integer);

  public
    constructor Create(const arr: TArrayOfInt);
    destructor Destroy; override;

    procedure PreOrder;
  end;

procedure Main;

implementation

procedure Main;
var
  a: TArrayOfInt;
begin
  a := [1, 2, 3, 4, 5, 6, 7];

  with TArrBinaryTree.Create(a) do
  begin
    PreOrder;
  end;
end;

{ TArrBinaryTree }

constructor TArrBinaryTree.Create(const arr: TArrayOfInt);
begin
  __data := Copy(arr);
end;

destructor TArrBinaryTree.Destroy;
begin
  inherited Destroy;
end;

procedure TArrBinaryTree.PreOrder;
begin
  __preOrder(0);
end;

procedure TArrBinaryTree.__preOrder(index: integer);
begin
  if Length(__data) = 0 then
  begin
    WriteLn('数组为空，不能按照二叉树的前序遍历');
    Exit;
  end;

  WriteLn(__data[index], ' ');

  if (2 * index + 1) < Length(__data) then
    __preOrder(2 * index + 1);

  if (2 * index + 2) < Length(__data) then
    __preOrder(2 * index + 2);
end;

end.

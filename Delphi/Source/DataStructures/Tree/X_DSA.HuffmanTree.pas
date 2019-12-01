unit X_DSA.HuffmanTree;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Generics.Defaults;

type
  TNode = class
  public type
    TComparer = class(TComparer<TNode>)
      function Compare(const Left, Right: TNode): integer; override;
    end;

  var
    Value: integer;
    LChild: TNode;
    RChild: TNode;

    constructor Create(newValue: integer);
  end;

  THuffmanTree = class
  private type
    TArrayOfInt = TArray<integer>;
    TObjectListOfTNode = TList<TNode>;

  var
    __root: TNode;

    procedure __preOrder(node: TNode);

  public
    constructor Create(const arr: TArrayOfInt);

    procedure PreOrder;
  end;

procedure Main;

implementation

procedure Main;
var
  ht: THuffmanTree;
  arr: TArray<integer>;
begin
  arr := [13, 7, 8, 3, 29, 6, 1];
  ht := THuffmanTree.Create(arr);
  ht.PreOrder;
  Writeln;
end;

{ TNode }

constructor TNode.Create(newValue: integer);
begin
  Value := newValue;
  LChild := nil;
  RChild := nil;
end;

{ THuffmanTree }

constructor THuffmanTree.Create(const arr: TArrayOfInt);
var
  list: TObjectListOfTNode;
  i: integer;
  leftNode, rightNode, parentNode: TNode;
begin
  // 第一步为了操作方便
  // 1. 遍历 arr 数组
  // 2. 将 arr 的每个元素构成成一个 Node
  // 3. 将 Node 放入到 list 中
  list := TObjectListOfTNode.Create;
  for i := 0 to Length(arr) - 1 do
    list.Add(TNode.Create(arr[i]));

  while list.Count > 1 do
  begin
    // 排序 从小到大
    list.Sort(TNode.TComparer.Create);

    // 取出根节点权值最小的两颗二叉树
    // 1.取出权值最小的结点（二叉树）
    leftNode := list[0];
    // 2.取出权值第二小的结点（二叉树）
    rightNode := list[1];

    // 3.构建一颗新的二叉树
    parentNode := TNode.Create(leftNode.Value + rightNode.Value);
    parentNode.LChild := leftNode;
    parentNode.RChild := rightNode;

    // 4.从 List 删除处理过的二叉树
    list.Remove(leftNode);
    list.Remove(rightNode);

    // 5.将 parent 加入到 list
    list.Add(parentNode);
  end;

  __root := list[0];
end;

procedure THuffmanTree.PreOrder;
begin
  __preOrder(__root);
end;

procedure THuffmanTree.__preOrder(node: TNode);
begin
  if node = nil then
    Exit;

  Write(node.Value, ' ');
  __preOrder(node.LChild);
  __preOrder(node.RChild);
end;

{ TNode.TComparer }

function TNode.TComparer.Compare(const Left, Right: TNode): integer;
begin
  Result := Left.Value - Right.Value;
end;

end.

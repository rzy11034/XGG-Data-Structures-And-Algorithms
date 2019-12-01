unit X_DSA.ThreadedBinaryTreeDemo;

interface

uses
  System.SysUtils;

type
  TArrayOfInt = TArray<integer>;

  TNode = class
  public
    Num: integer;
    LChild: TNode;
    RChild: TNode;

    /// <summary> 如果 LType = 0 表示指向的是左子树, 如果 1 表示指向前驱结点 </summary>
    LType: shortint;
    /// <summary> 如果 RType = 0 表示指向的是右子树, 如果 1 表示指向后继结点 </summary>
    RType: shortint;

    constructor Create(n: integer);
    function ToString: string; override;
  end;

  TThreadedBinaryTreeDemo = class
  private
    __root: TNode;
    /// <summary> 指向当前结点的前驱结点的指针, 在递归进行线索化时，pre 总是保留前一个结点 </summary>
    __pre: TNode;

    function __add(node: TNode; n: integer): TNode;
    procedure __threadedNodes(node: TNode);

  public
    constructor Create(); overload;
    constructor Create(const arr: TArrayOfInt); overload;

    procedure Add(n: integer);
    /// <summary> 对二叉树进行中序线索化 </summary>
    procedure ThreadedNodes();
    /// <summary> 遍历线索化二叉树的方法 </summary>
    procedure Traversing();
  end;

procedure Main;

implementation

procedure Main;
var
  a: TArrayOfInt;
  tb: TThreadedBinaryTreeDemo;
begin
  a := [8, 3, 10, 1, 14, 6];
  tb := TThreadedBinaryTreeDemo.Create(a);
  tb.ThreadedNodes;
  tb.Traversing
end;

{ TNode }

constructor TNode.Create(n: integer);
begin
  Num := n;
  LChild := nil;
  RChild := nil;
  LType := 0;
  RType := 0;
end;

function TNode.ToString: string;
begin
  Result := IntToStr(Self.Num);
end;

{ TThreadedBinaryTreeDemo }

constructor TThreadedBinaryTreeDemo.Create();
begin
  __root := nil;
end;

constructor TThreadedBinaryTreeDemo.Create(const arr: TArrayOfInt);
var
  i: integer;
begin
  for i := 0 to Length(arr) - 1 do
    Add(arr[i]);
end;

procedure TThreadedBinaryTreeDemo.ThreadedNodes;
begin
  __threadedNodes(__root);
end;

procedure TThreadedBinaryTreeDemo.Traversing;
var
  cur: TNode;
begin
  // 定义一个变量，存储当前遍历的结点，从 __root 开始
  cur := __root;

  while cur <> nil do
  begin
    // 循环的找到 LType = 1 的结点
    // 后面随着遍历而变化,因为当 LType=1 时，说明该结点是按照线索化
    // 处理后的有效结点
    while cur.LType = 0 do
      cur := cur.LChild;

    // 输出当前这个结点
    write(cur.ToString, ' ');
    // 如果当前结点的右指针指向的是后继结点,就一直输出
    while (cur.RType = 1) do
    begin
      // 获取到当前结点的后继结点
      cur := cur.RChild;
      write(cur.ToString, ' ');
    end;

    // 替换这个遍历的结点
    cur := cur.RChild;
  end;

  Writeln;
end;

procedure TThreadedBinaryTreeDemo.__threadedNodes(node: TNode);
begin
  // 如果 node = nil, 不能线索化
  if node = nil then
    Exit;

  // 一.先线索化左子树
  __threadedNodes(node.LChild);

  // ======================================================
  // 二.线索化当前结点
  // 1.处理当前结点的前驱结点
  if node.LChild = nil then
  begin
    node.LChild := __pre;
    node.LType := 1;
  end;

  // 2.处理当前结点的后继结点
  if (__pre <> nil) and (__pre.RChild = nil) then
  begin
    __pre.RChild := node;
    __pre.RType := 1;
  end;

  __pre := node;
  // =======================================================

  // 三.线索化右子树
  __threadedNodes(node.RChild);
end;

procedure TThreadedBinaryTreeDemo.Add(n: integer);
begin
  __root := __add(__root, n);
end;

function TThreadedBinaryTreeDemo.__add(node: TNode; n: integer): TNode;
begin
  if node = nil then
  begin
    Result := TNode.Create(n);
    Exit;
  end;

  if n < node.Num then
    node.LChild := __add(node.LChild, n)
  else if n > node.Num then
    node.RChild := __add(node.RChild, n);

  Result := node;
end;

end.

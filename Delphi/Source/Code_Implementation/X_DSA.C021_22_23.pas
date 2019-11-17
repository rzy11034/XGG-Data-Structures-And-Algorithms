unit X_DSA.C021_22_23;

interface

uses
  Classes,
  SysUtils;

type
  TNode = class
  public
    Elment: integer;
    Next: TNode;
    constructor Create(newElment: integer; newNext: TNode = nil); overload;
    constructor Create(); overload;
    function ToString: string; override;
  end;

  TLinkedListDemo = class
  private
    __dummyHead: TNode;
  public
    constructor Create(arr: array of integer);

    /// <summary> 删除倒数第N个元素 </summary>
    procedure DelLastIndex(lastIndex: integer);

    /// <summary> 反转链表 </summary>
    procedure ListInversion;

    function ToString: string; override;
  end;

procedure Main;

implementation

procedure Main;
var
  a: array of integer;
  lld: TLinkedListDemo;
begin
  a := [1, 2, 3, 4, 5];
  lld := TLinkedListDemo.Create(a);
  WriteLn(lld.ToString);

  lld.DelLastIndex(3);
  WriteLn(lld.ToString);
  lld.Free;
  WriteLn;

  // ----------------------------------------------------------

  a := [1, 2, 3, 4, 5];
  lld := TLinkedListDemo.Create(a);
  lld.ListInversion;
  WriteLn(lld.ToString);
end;

{ TLinkedListDemo }

constructor TLinkedListDemo.Create(arr: array of integer);
var
  i: integer;
  head, cur: TNode;
  tail: TNode;
begin
  head := TNode.Create;
  tail := head;

  for i := 0 to Length(arr) - 1 do
  begin
    cur := TNode.Create(arr[i]);
    tail.Next := cur;
    tail := cur;
  end;

  __dummyHead := head;
end;

procedure TLinkedListDemo.DelLastIndex(lastIndex: integer);
var
  n, i: integer;
  node, del: TNode;
begin
  n := 0;
  node := __dummyHead.Next;

  while node <> nil do
  begin
    n := n + 1;
    node := node.Next;
  end;

  if (lastIndex <= 0) or (lastIndex > n) then
  begin
    WriteLn('Index Error');
    Exit;
  end;

  node := __dummyHead;
  for i := 1 to n - lastIndex do
  begin
    node := node.Next;
  end;

  if lastIndex = 1 then
  begin
    FreeAndNil(node.Next);
  end
  else
  begin
    del := node.Next;
    node.Next := del.Next;
    FreeAndNil(del);
  end;

end;

procedure TLinkedListDemo.ListInversion;
var
  cur, head, tmp: TNode;
begin
  head := TNode.Create;
  cur := __dummyHead.Next;

  while cur <> nil do
  begin
    tmp := cur.Next;
    cur.Next := head.Next;
    head.Next := cur;
    cur := tmp;
  end;

  __dummyHead.Next := head.Next;

  head.Free;
end;

function TLinkedListDemo.ToString: string;
var
  node: TNode;
begin
  Result := '';
  node := __dummyHead.Next;

  while node <> nil do
  begin
    Result := Result + node.Elment.ToString + ' ';
    node := node.Next;
  end;
end;

{ TNode }

constructor TNode.Create(newElment: integer; newNext: TNode);
begin
  Elment := newElment;
  Next := newNext;
end;

constructor TNode.Create;
begin
  Self.Create(0);
end;

function TNode.ToString: string;
begin
  Result := Elment.ToString;
end;

end.

unit X_DSA.DoubleLinkedList;

interface

uses
  System.SysUtils;

type
  TNode = class
  public
    Elment: integer;
    Prev, Next: TNode;

    constructor Create; overload;
    constructor Create(newElment: integer; newPrev: TNode = nil; newNext: TNode = nil); overload;
    function ToString: string; override;
  end;

  TDoubleLinkList = class
  private
    __dummyHead: TNode;
    __count: integer;

  public
    constructor Create(arr: array of integer);
    function ToString: string; override;

    /// <summary> 从链表中删除index位置的元素，返回删除的元素 </summary>
    function Remove(index: integer): integer;
  end;

procedure Main;

implementation

procedure Main;
var
  a: array of integer;
  dld: TDoubleLinkList;
begin
  a := [1, 2, 3, 4, 5];
  dld := TDoubleLinkList.Create(a);
  Writeln(dld.ToString);

  dld.Remove(0);
  Writeln(dld.ToString);
  Writeln;
end;

{ TDoubleLinkList }

constructor TDoubleLinkList.Create(arr: array of integer);
var
  i: integer;
  head, cur: TNode;
  tail: TNode;
begin
  head := TNode.Create;
  tail := head;

  for i := 0 to Length(arr) - 1 do
  begin
    cur := TNode.Create(arr[i], tail);
    tail.Next := cur;
    tail := cur;
  end;

  __dummyHead := head;
end;

function TDoubleLinkList.Remove(index: integer): integer;
var
  cur, Prev, Next: TNode;
  i: integer;
begin
  if (index < 0) and (index > __count) then
  begin
    Writeln('Index Error');
    Exit(-1);
  end;

  cur := __dummyHead;

  for i := 0 to index do
  begin
    cur := cur.Next;
  end;

  Prev := cur.Prev;
  Next := cur.Next;
  Prev.Next := Next;
  Next.Prev := Prev;

  Result := cur.Elment;
  FreeAndNil(cur);
end;

function TDoubleLinkList.ToString: string;
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

constructor TNode.Create;
begin
  Self.Create(0);
end;

constructor TNode.Create(newElment: integer; newPrev: TNode; newNext: TNode);
begin
  Elment := newElment;
  Prev := newPrev;
  Next := newNext;
end;

function TNode.ToString: string;
begin
  Result := Elment.ToString;
end;

end.

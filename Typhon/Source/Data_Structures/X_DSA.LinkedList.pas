unit X_DSA.LinkedList;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  generic TLinkedList<T> = class
  private  type

    TNode = class
    public
      Elment: T;
      Next: TNode;
      constructor Create(newElment: T; newNext: TNode = nil); overload;
      constructor Create(); overload;
      function ToString: string; override;
    end;

  var
    __dummyHead: TNode;
    __size: integer;
  public
    /// <summary> 获取链表中的元素个数 </summary>
    function GetSize: integer;
    /// <summary> 返回链表是否为空 </summary>
    function IsEmpty: boolean;
    /// <summary> 链表的index(0-based)位置中添加新的元素e </summary>
    procedure Add(index: integer; e: T);
    /// <summary> 在链表头添加新的元素e </summary>
    procedure AddFirst(e: T);
    /// <summary> 在链表末尾添加新的元素e </summary>
    procedure AddLast(e: T);
    /// <summary> 获取链表index(0-based)位置中的元素e </summary>
    function Get(index: integer): T;
    /// <summary> 获取链表(0-based)第一个元素 e </summary>
    function GetFirst(): T;
    /// <summary> 获取链表(0-based)最后一个元素e  </summary>
    function GetLast(): T;
    /// <summary> 修改链表所有 d 元素为 e </summary>
    procedure SetElment(d, e: T);
    /// <summary> 修改链表index(0-based)位置中的元素e </summary>
    procedure Set_(index: integer; e: T);
    /// <summary> 查找链表中是否有元素e </summary>
    function Contains(e: T): boolean;
    /// <summary> 删除链表index(0-based)位置的元素e, 返回链表删除元素 </summary>
    function Remove(index: integer): T;
    /// <summary> 删除链表(0-based)第一个位置的元素e，返回链表删除元素 </summary>
    function RemoveFirst(): T;
    /// <summary> 删除链表index(0-based)最后一个位置的元素e，返回链表删除元素 </summary>
    function RemoveLast(): T;
    /// <summary> 删除链表所有为e的元素 </summary>
    procedure RemoveElement(e: T);
    property Items[i: integer]: T read Get write Set_; default;
    function ToString: string; override;

    constructor Create;
    destructor Destroy; override;
  end;

procedure Main;

implementation

type
  TLinkedList_int = specialize TLinkedList<integer>;

procedure Main;
var
  LinkedList: TLinkedList_int;
  i: integer;
begin
  LinkedList := TLinkedList_int.Create;

  for i := 0 to 9 do
  begin
    LinkedList.AddFirst(i mod 2);
    Writeln(LinkedList.ToString);
  end;

  LinkedList.Add(2, 666);
  Writeln(LinkedList.ToString);

  LinkedList.Remove(2);
  Writeln(LinkedList.ToString);

  LinkedList.RemoveFirst;
  Writeln(LinkedList.ToString);

  LinkedList.RemoveLast;
  Writeln(LinkedList.ToString);

  LinkedList.SetElment(1, 9);
  Writeln(LinkedList.ToString);

end;

{ TLinkedList }

procedure TLinkedList.Add(index: integer; e: T);
var
  prev: TNode;
  i: integer;
begin
  if (index < 0) or (index > __size) then
    raise Exception.Create('Add failed. Index is Illegal.');

  prev := __dummyHead;
  for i := 0 to index - 1 do
    prev := prev.Next;

  // node := TNode.Create(e);
  // node.FNext = prev.FNext;
  // prev.FNext = node;
  // 以上三句等同于下面一句
  prev.Next := TNode.Create(e, prev.Next);
  Inc(Self.__size);
end;

procedure TLinkedList.AddFirst(e: T);
begin
  Add(0, e);
end;

procedure TLinkedList.AddLast(e: T);
begin
  Add(__size, e);
end;

function TLinkedList.Contains(e: T): boolean;
var
  cur: TNode;
begin
  cur := __dummyHead.Next;
  Result := False;

  while cur <> nil do
  begin
    if cur.Elment = e then
      Result := True;

    cur := cur.Next;
  end;
end;

destructor TLinkedList.Destroy;
var
  delNode, cur: TNode;
begin
  cur := __dummyHead;

  while cur <> nil do
  begin
    delNode := cur;
    cur := cur.Next;
    FreeAndNil(delNode);
  end;

  inherited Destroy;
end;

constructor TLinkedList.Create;
begin
  __dummyHead := TNode.Create();
  __size := 0;
end;

function TLinkedList.Get(index: integer): T;
var
  cur: TNode;
  i: integer;
begin
  if (index < 0) or (index >= __size) then
    raise Exception.Create('Get failed. Index is Illegal.');

  cur := __dummyHead.Next;
  for i := 0 to index - 1 do
    cur := cur.Next;

  Result := cur.Elment;
end;

function TLinkedList.GetFirst(): T;
begin
  Result := Get(0);
end;

function TLinkedList.GetLast(): T;
begin
  Result := Get(__size - 1);
end;

function TLinkedList.GetSize: integer;
begin
  Result := __size;
end;

function TLinkedList.IsEmpty: boolean;
begin
  Result := __size = 0;
end;

function TLinkedList.Remove(index: integer): T;
var
  prev: TNode;
  i: integer;
  res: TNode;
begin
  if (index < 0) or (index >= __size) then
    raise Exception.Create('Remove failed. Index is Illegal.');

  prev := __dummyHead;

  for i := 0 to index - 1 do
    prev := prev.Next;

  res := prev.Next;
  prev.Next := res.Next;
  Dec(__size);

  Result := res.Elment;
  FreeAndNil(res);
end;

procedure TLinkedList.RemoveElement(e: T);
var
  prev, del: TNode;
begin
  prev := __dummyHead;

  while prev.Next <> nil do
  begin
    if prev.Next.Elment = e then
    begin
      del := prev.Next;
      prev.Next := del.Next;
      Dec(__size);
      FreeAndNil(del);
    end
    else
    begin
      prev := prev.Next;
    end;
  end;
end;

function TLinkedList.RemoveFirst(): T;
begin
  Result := Remove(0);
end;

function TLinkedList.RemoveLast(): T;
begin
  Result := Remove(__size - 1);
end;

procedure TLinkedList.SetElment(d, e: T);
var
  cur: TNode;
begin
  cur := __dummyHead.Next;

  while cur <> nil do
  begin
    if cur.Elment = d then
      cur.Elment := e;

    cur := cur.Next;
  end;
end;

procedure TLinkedList.Set_(index: integer; e: T);
var
  cur: TNode;
  i: integer;
begin
  if (index < 0) or (index >= __size) then
    raise Exception.Create('Set failed. Index is Illegal.');

  cur := __dummyHead.Next;
  for i := 0 to index - 1 do
    cur := cur.Next;

  cur.Elment := e;
end;

function TLinkedList.ToString: string;
var
  res: TAnsiStringBuilder;
  cur: TNode;
begin
  res := TAnsiStringBuilder.Create;
  try
    cur := __dummyHead.Next;

    while cur <> nil do
    begin
      res.Append(cur.ToString + ' -> ');
      cur := cur.Next;
    end;

    res.Append('nil');

    Result := res.ToString;
  finally
    res.Free;
  end;
end;

{ TLinkedList.TNode }

constructor TLinkedList.TNode.Create(newElment: T; newNext: TNode);
begin
  Elment := newElment;
  Next := newNext;
end;

constructor TLinkedList.TNode.Create;
begin
  Self.Create(default(T));
end;

function TLinkedList.TNode.ToString: string;
begin
  Result := Elment.ToString;
end;

end.


unit X_DSA.CircularLinkedList;

interface

uses
  System.SysUtils;

type
  TNode = class
  public
    Elment: Integer;
    Next: TNode;
    constructor Create(newElment: Integer; newNext: TNode = nil);
  end;

  TCircularLinkedList = class
  private
    __head: TNode;

  public
    constructor Create(arr: array of Integer);
    function ToString: string; override;

    /// <summary> 约瑟夫问题 </summary>
    procedure JosephusProblem(k: Integer);
  end;

procedure Main;

implementation

procedure Main;
var
  a: array of Integer;
  cll: TCircularLinkedList;
  i: Integer;
begin
  SetLength(a, 41);
  for i := 0 to Length(a) - 1 do
    a[i] := i + 1;

  cll := TCircularLinkedList.Create(a);
  WriteLn(cll.ToString);

  cll.JosephusProblem(3);
end;

{ TCircularLinkedList }

constructor TCircularLinkedList.Create(arr: array of Integer);
var
  i: Integer;
  cur, tmp: TNode;
begin
  tmp := nil;
  __head := TNode.Create(arr[0], __head);
  __head.Next := __head;

  for i := 1 to Length(arr) - 1 do
  begin
    if i = 1 then
    begin
      cur := TNode.Create(arr[i]);
      __head.Next := cur;
      cur.Next := __head;
      tmp := cur;
    end
    else
    begin
      cur := TNode.Create(arr[i]);
      tmp.Next := cur;
      cur.Next := __head;
      tmp := cur;
    end;
  end;
end;

procedure TCircularLinkedList.JosephusProblem(k: Integer);
var
  cur, prev: TNode;
  i: Integer;
begin
  if __head = nil then
    Exit;

  prev := __head;

  while prev.Next <> __head do
  begin
    prev := prev.Next;
  end;

  while True do
  begin
    if prev.Next = prev then
      Break;

    cur := prev.Next;

    for i := 1 to k - 1 do
    begin
      prev := cur;
      cur := cur.Next;
    end;

    write(cur.Elment, ' ');

    if prev.Next <> prev then
    begin
      prev.Next := cur.Next;
      FreeAndNil(cur);
    end
    else
      Continue;
  end;

  WriteLn(prev.Elment);
end;

function TCircularLinkedList.ToString: string;
var
  node: TNode;
  ret: string;
begin
  if __head = nil then
    Exit('');

  node := __head;
  ret := node.Elment.ToString + ' ';

  while node.Next <> __head do
  begin
    node := node.Next;
    ret := ret + node.Elment.ToString + ' ';
  end;

  Result := ret;
end;

{ TNode }

constructor TNode.Create(newElment: Integer; newNext: TNode);
begin
  Elment := newElment;
end;

end.

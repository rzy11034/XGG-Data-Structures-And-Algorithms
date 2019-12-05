unit X_DSA.BinarySortTree;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  TNode = class
  public
    Value: integer;
    Left: TNode;
    Right: TNode;
    constructor Create(newValue: integer);
  end;

  TBinarySortTree = class
  private
    __count: integer;
    __root: TNode;

    function __add(node: TNode; val: integer): TNode;
    function __del(node: TNode; val: integer): TNode;
    function __delMin(node: TNode): TNode;
    function __minNode(node: TNode): TNode;
    procedure __destroyRoot(node: TNode);
    procedure __infixOrder(node: TNode);

  public
    constructor Create;
    destructor Destroy; override;

    function Contains(val: integer): boolean;
    procedure Add(val: integer);
    procedure Del(val: integer);
    procedure InfixOrder;
    property Count: integer read __count;
  end;

implementation

{ TBinarySortTree }

constructor TBinarySortTree.Create;
begin
  __root := nil;
  __count := 0;
end;

procedure TBinarySortTree.Add(val: integer);
begin
  __root := __add(__root, val);
end;

function TBinarySortTree.Contains(val: integer): boolean;
var
  cur: TNode;
begin
  cur := __root;
  Result := False;

  while cur <> nil do
  begin
    if val < cur.Value then
      cur := cur.Left
    else if val > cur.Value then
      cur := cur.Right
    else
      Result := True;
  end;
end;

procedure TBinarySortTree.Del(val: integer);
begin
  //if Contains(val) then
  __root := __del(__root, val);
end;

destructor TBinarySortTree.Destroy;
begin
  __destroyRoot(__root);
  inherited Destroy;
end;

procedure TBinarySortTree.InfixOrder;
begin
  __infixOrder(__root);
end;

function TBinarySortTree.__add(node: TNode; val: integer): TNode;
begin
  if node = nil then
  begin
    Result := TNode.Create(val);
    Inc(__count);
    Exit;
  end;

  if val < node.Value then
    node.Left := __add(node.Left, val)
  else if val > node.Value then
    node.Right := __add(node.Right, val);

  Result := node;
end;

function TBinarySortTree.__del(node: TNode; val: integer): TNode;
var
  succesor: TNode;
begin
  if node = nil then
    Exit(nil);

  if val < node.Value then
  begin
    node.Left := __del(node.Left, val);
    Result := node;
  end
  else if val > node.Value then
  begin
    node.Right := __del(node.Right, val);
    Result := node;
  end
  else
  begin
    if node.Left = nil then
    begin
      Result := node.Right;
      FreeAndNil(node);
      Dec(__count);
    end
    else if node.Right = nil then
    begin
      Result := node.Left;
      FreeAndNil(node);
      Dec(__count);
    end
    else
    begin
      succesor := TNode.Create(__minNode(node.Right).Value);
      succesor.Left := node.Left;
      succesor.Right := __delMin(node.Right);
      Result := succesor;
      FreeAndNil(node);
    end;
  end;
end;

function TBinarySortTree.__delMin(node: TNode): TNode;
begin
  if node.Left = nil then
  begin
    Result := node.Right;
    FreeAndNil(node);
    Dec(__count);
    Exit;
  end;

  node.Left := __delMin(node.Left);
  Result := node;
end;

procedure TBinarySortTree.__destroyRoot(node: TNode);
begin
  if node = nil then
    exit;

  __destroyRoot(node.Left);
  __destroyRoot(node.Right);

  node.Free;
end;

procedure TBinarySortTree.__infixOrder(node: TNode);
begin
  if node = nil then
    Exit;

  __infixOrder(node.Left);
  Write(node.Value, ' ');
  __infixOrder(node.Right);
end;

function TBinarySortTree.__minNode(node: TNode): TNode;
begin
  if node.Left = nil then
    Exit(node);

  Result := __minNode(node.Left);
end;

{ TNode }

constructor TNode.Create(newValue: integer);
begin
  Value := newValue;
end;

end.

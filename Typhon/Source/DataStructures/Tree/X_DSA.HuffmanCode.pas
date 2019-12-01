unit X_DSA.HuffmanCode;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Generics.Defaults,
  Generics.Collections;

type
  TNode = class
  public type
    TCmp = specialize TComparer<TNode>;
    TComparer = class(TCmp)
      function Compare(constref Left, Right: TNode): integer; override;
    end;

  var
    Data: char;
    Weight: integer;
    Left: TNode;
    Right: TNode;

    constructor Create(newData: char; newWeight: integer);
    function ToString: string; override;
  end;

  THuffmanCode = class
  private type
    TListOfNode = specialize TList<TNode>;
    TMap_Chr_Int = specialize THashMap<char, integer>;
    TMap_Chr_Str = specialize THashMap<char, string>;
    TPair_Chr_Int = specialize TPair<char, integer>;
    TPair_Chr_Str = specialize TPair<char, string>;
  var
    __root: TNode;
    __codeMap: TMap_Chr_Str;

    procedure __preOrder(node: TNode);
    /// <summary> 创建对应的赫夫曼树 </summary>
    function __createHuffmanTree(const str: string): TNode;
    /// <summary> 生成赫夫曼树对应的赫夫曼编码 </summary>
    function __getCode(node: TNode; code: string): TMap_Chr_Str;
    procedure __destroyRoot(node: TNode);
  public
    constructor Create(const str: string);
    destructor Destroy; override;
    procedure PreOrder;
    procedure PrintCode;
    procedure Zip(const str: string);
  end;

procedure Main;

implementation

procedure Main;
var
  str: string;
  hc: THuffmanCode;
begin
  str := 'i like like like java do you like a java';
  hc := THuffmanCode.Create(str);
  hc.PreOrder;
  hc.PrintCode;
  hc.Zip(str);
end;

{ THuffmanCode }

constructor THuffmanCode.Create(const str: string);
begin
  __root := __createHuffmanTree(str);
  __codeMap := TMap_Chr_Str.Create;
  __getCode(__root, '');
end;

destructor THuffmanCode.Destroy;
begin
  __destroyRoot(__root);
  __codeMap.Free;
  inherited Destroy;
end;

procedure THuffmanCode.PreOrder;
begin
  __preOrder(__root);
end;

procedure THuffmanCode.PrintCode;
var
  p: TPair_Chr_Str;
begin
  for p in __codeMap.ToArray do
    WriteLn('[', (p.Key), ' code = ', p.Value, ']');
end;

procedure THuffmanCode.Zip(const str: string);
var
  arrOfChar: array of char;
  c: char;
  sb: TAnsiStringBuilder;
begin
  arrOfChar := str.ToCharArray;
  sb := TAnsiStringBuilder.Create;

  for c in arrOfChar do
  begin
    sb.Append(__codeMap.Items[c]);
  end;

  Writeln(sb.Length);
end;

function THuffmanCode.__createHuffmanTree(const str: string): TNode;
var
  arrOfChar: array of char;
  list: TListOfNode;
  map: TMap_Chr_Int;
  i: integer;
  p: TPair_Chr_Int;
  leftNode, rightNode, parentNode: TNode;
begin
  arrOfChar := str.ToCharArray;

  map := TMap_Chr_Int.Create;
  list := TListOfNode.Create(TNode.TComparer.Create);
  try
    for i := 0 to Length(arrOfChar) - 1 do
    begin
      if not map.ContainsKey(arrOfChar[i]) then
        map.Add(arrOfChar[i], 1)
      else
        map.AddOrSetValue(arrOfChar[i], map.Items[arrOfChar[i]] + 1);
    end;

    for p in map.ToArray do
      list.Add(TNode.Create(p.Key, p.Value));

    while list.Count > 1 do
    begin
      // 排序 从小到大
      list.Sort;

      // 取出根节点权值最小的两颗二叉树
      // 1.取出权值最小的结点（二叉树）
      leftNode := list[0];
      // 2.取出权值第二小的结点（二叉树）
      rightNode := list[1];

      // 3.构建一颗新的二叉树
      parentNode := TNode.Create(chr(0), leftNode.Weight + rightNode.Weight);
      parentNode.Left := leftNode;
      parentNode.Right := rightNode;

      // 4.从 List 删除处理过的二叉树
      list.Remove(leftNode);
      list.Remove(rightNode);

      // 5.将 parent 加入到 list
      list.Add(parentNode);
    end;

    Result := list[0];
  finally
    list.Free;
    map.Free;
  end;
end;

function THuffmanCode.__getCode(node: TNode; code: string): TMap_Chr_Str;
begin
  if node = nil then
    exit;

  // 判断当前 node 是叶子结点还是非叶子结点
  // 如果是非叶子结点递归处理
  // 否则说明是一个叶子结点
  if node.Data = chr(0) then
  begin
    __getCode(node.Left, code + '0');
    __getCode(node.Right, code + '1');
  end
  else
    __codeMap.Add(node.Data, code);
end;

procedure THuffmanCode.__preOrder(node: TNode);
begin
  if node = nil then
    Exit;

  WriteLn(node.ToString, ' ');
  __preOrder(node.Left);
  __preOrder(node.Right);
end;

procedure THuffmanCode.__destroyRoot(node: TNode);
begin
  if node = nil then
    exit;

  __destroyRoot(node.Left);
  __destroyRoot(node.Right);

  node.Free;
end;

{ TNode }

constructor TNode.Create(newData: char; newWeight: integer);
begin
  Data := newData;
  Weight := newWeight;
  Left := nil;
  Right := nil;
end;

function TNode.ToString: string;
var
  ret: string;
begin
  ret := '';

  if Data = chr(0) then
    ret := Format('[D=nil W=%d]', [Weight])
  else
    ret := Format('[D=%s W=%d]', [Data, Weight]);

  Result := ret;
end;

{ TNode.TComparer }

function TNode.TComparer.Compare(constref Left, Right: TNode): integer;
begin
  Result := Left.Weight - Right.Weight;
end;

end.

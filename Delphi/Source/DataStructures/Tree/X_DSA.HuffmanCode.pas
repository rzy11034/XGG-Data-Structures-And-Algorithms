unit X_DSA.HuffmanCode;

interface

uses
  System.SysUtils,
  System.Generics.Defaults,
  System.Generics.Collections;

type
  TNode = class
  public type
    TCmp = TComparer<TNode>;

    TComparer = class(TCmp)
      function Compare(const Left, Right: TNode): Integer; override;
    end;

  var
    Data: Char;
    Weight: Integer;
    Left: TNode;
    Right: TNode;
    constructor Create(newData: Char; newWeight: Integer);
    function ToString: string; override;
  end;

  THuffmanCode = class
  private type
    TListOfNode = TList<TNode>;
    TMap_Char_Int = TDictionary<Char, Integer>;
    TMap_Char_Str = TDictionary<Char, string>;
    TPair_Char_Int = TPair<Char, Integer>;

  var
    __root: TNode;
    __huffmanCode: TMap_Char_Str;
    procedure __preOrder(node: TNode);
    /// <summary> 创建对应的赫夫曼树 </summary>
    function __createHuffmanTree(const str: string): TNode;
    function __getCode(node: TNode; code: string): TMap_Char_Str;
  public
    constructor Create(const str: string);
    destructor Destroy; override;
    procedure PreOrder;
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
end;

{ TNode.TComparer }

function TNode.TComparer.Compare(const Left, Right: TNode): Integer;
begin
  Result := Left.Weight - Right.Weight;
end;

{ TNode }

constructor TNode.Create(newData: Char; newWeight: Integer);
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
    ret := Format('[D=%d W=%d]', [Data, Weight]);

  Result := ret;
end;

{ THuffmanCode }

constructor THuffmanCode.Create(const str: string);
begin
  __root := __createHuffmanTree(str)
end;

destructor THuffmanCode.Destroy;
begin
  inherited Destroy;
end;

procedure THuffmanCode.PreOrder;
begin
  __preOrder(__root);
end;

function THuffmanCode.__createHuffmanTree(const str: string): TNode;
var
  arrOfChar: TArray<Char>;
  list: TListOfNode;
  map: TMap_Char_Int;
  i: Integer;
  p: TPair_Char_Int;
  leftNode, rightNode, parentNode: TNode;
begin
  arrOfChar := str.ToCharArray;

  map := TMap_Char_Int.Create;
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

function THuffmanCode.__getCode(node: TNode; code: string): TMap_Char_Str;
begin

end;

procedure THuffmanCode.__preOrder(node: TNode);
begin
  if node = nil then
    Exit;

  WriteLn(node.ToString, ' ');
  __preOrder(node.Left);
  __preOrder(node.Right);
end;

end.

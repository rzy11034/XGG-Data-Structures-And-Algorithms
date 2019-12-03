unit X_DSA.HuffmanCode;

interface

uses
  System.SysUtils,
  System.Math,
  System.Generics.Defaults,
  System.Generics.Collections;

type
  TNode = class
  public type
    TCmp = TComparer<TNode>;

    TComparer = class(TCmp)
    public
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
    TMap_Chr_Int = TDictionary<Char, Integer>;
    TMap_Chr_Str = TDictionary<Char, string>;
    TPair_Chr_Int = TPair<Char, Integer>;
    TPair_Chr_Str = TPair<Char, string>;

  var
    __root: TNode;
    __codeMap: TMap_Chr_Str;
    __huffmanCodes: TBytes;

    procedure __preOrder(node: TNode);
    /// <summary> 创建对应的赫夫曼树 </summary>
    function __createHuffmanTree(const str: string): TNode;
    /// <summary> 生成赫夫曼树对应的赫夫曼编码 </summary>
    procedure __getCode(node: TNode; code: string);
    procedure __destroyRoot(node: TNode);
    /// <summary> 二进制转 Byte </summary>
    function __binToByte(str: string): byte;
    /// <summary> Byte 转二进制 </summary>
    function __byteToBin(b: byte): string;
    /// <summary> 通过生成的赫夫曼编码表，返回一个赫夫曼编码压缩后的 TBytes </summary>
    function __zip(const str: string): TBytes;
    /// <summary> 通过一个赫夫曼编码压缩后的 TBytes, 返回的赫夫曼字符串 </summary>
    function __unZip(b: TBytes): string;
  public
    strZip, strUZip: string;

    constructor Create(const str: string);
    destructor Destroy; override;
    procedure PreOrder;
    procedure PrintCode;
    function ToString: string; override;
  end;

procedure Main;

implementation

procedure Main;
var
  str: string;
  hc: THuffmanCode;
begin
//  str := 'i like like like java do you like a java';
  str := 'abcdefg';
  hc := THuffmanCode.Create(str);
  WriteLn(hc.ToString);
  hc.PrintCode;

  WriteLn(hc.strZip, ' ', Length(hc.strZip));
  WriteLn(hc.strUZip, ' ', Length(hc.strUZip));
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
    ret := Format('[D=%s W=%d]', [Data, Weight]);

  Result := ret;
end;

{ THuffmanCode }

constructor THuffmanCode.Create(const str: string);
var
  s: string;
begin
  __root := __createHuffmanTree(str);
  __codeMap := TMap_Chr_Str.Create;
  __getCode(__root, '');
  __huffmanCodes := __zip(str);
  s := __unZip(__huffmanCodes)
end;

destructor THuffmanCode.Destroy;
begin
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
    WriteLn('[', (p.Key), ' = ', p.Value, ']');
end;

function THuffmanCode.ToString: string;
var
  i: integer;
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    sb.Append('[');
    for i := 0 to Length(__huffmanCodes) - 1 do
    begin
      if i <> Length(__huffmanCodes) - 1 then
        sb.Append(__huffmanCodes[i]).Append(', ')
      else
        sb.Append(__huffmanCodes[i]);
    end;
    sb.Append(']'#10);

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function THuffmanCode.__zip(const str: string): TBytes;
var
  arrOfChar: TArray<Char>;
  c: Char;
  sb: TStringBuilder;
  len, i, index: Integer;
  ret: TBytes;
  tmp: string;
begin
  arrOfChar := str.ToCharArray;
  sb := TStringBuilder.Create;

  try
    // 1.利用 __codeMap 将 str 转成赫夫曼编码对应的字符串
    for c in arrOfChar do
    begin
      sb.Append(__codeMap.Items[c]);
    end;

    // 统计返回值得的长度
    if (sb.Length mod 8) = 0 then
      len := sb.Length div 8
    else
      len := sb.Length div 8 + 1;

    SetLength(ret, len);

    index := 0; // 记录是第几个 byte
    tmp := '';
    for i := 1 to sb.Length do
    begin
      tmp := tmp + sb.Chars[i - 1];

      if (i mod 8 = 0) or (i = sb.Length) then
      begin
        ret[index] := __binToByte(tmp);
        Inc(index);
        tmp := '';
      end;
    end;

    Result := ret;
    strZip := sb.ToString;
  finally
    sb.Free;
  end;
end;

function THuffmanCode.__binToByte(str: string): byte;
var
  stack: TStack<Char>;
  tmp: byte;
  i: Integer;
begin
  if Length(str) > 8 then
  begin
    WriteLn('错误：不是有效8位二进制数。');
    Exit(0);
  end;

  stack := TStack<Char>.Create;
  try
    for i := Low(str) to High(str) do
      stack.Push(str[i]);

    tmp := 0;
    i := 0;
    while stack.Count <> 0 do
    begin
      tmp := tmp + StrToInt(stack.Pop) * Round(Power(2, i));
      Inc(i);
    end;

    Result := tmp;
  finally
    stack.Free;
  end;
end;

function THuffmanCode.__byteToBin(b: byte): string;
var
  stack: TStack<byte>;
  ret: string;
  tmp: byte;
begin
  stack := TStack<byte>.Create;
  try
    // 采用 "除2取余，逆序排列" 法
    while b > 0 do
    begin
      tmp := b mod 2;
      b := b div 2;
      stack.Push(tmp);
    end;

    ret := '';
    while stack.Count <> 0 do
      ret := ret + stack.Pop.ToString;

    Result := ret;
  finally
    stack.Free;
  end;
end;

function THuffmanCode.__createHuffmanTree(const str: string): TNode;
var
  arrOfChar: TArray<Char>;
  list: TListOfNode;
  map: TMap_Chr_Int;
  i: Integer;
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

procedure THuffmanCode.__destroyRoot(node: TNode);
begin
  if node = nil then
    Exit;

  __destroyRoot(node.Left);
  __destroyRoot(node.Right);

  node.Free;
end;

procedure THuffmanCode.__getCode(node: TNode; code: string);
begin
  if node = nil then
    Exit;

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

function THuffmanCode.__unZip(b: TBytes): string;
var
  n, i: Integer;
  sb: TStringBuilder;
  tmp: string;
begin
  n := Length(b);

  sb := TStringBuilder.Create;
  try
    for i := 0 to n - 1 do
    begin
      if i = n - 1 then
        tmp := __byteToBin(b[i])
      else
      begin
        tmp := __byteToBin(b[i]);

        while Length(tmp) <> 8 do
          Insert('0', tmp, 0);
      end;

      sb.Append(tmp);
    end;

    Result := sb.ToString;
    strUZip := sb.ToString;
  finally
    sb.Free;
  end;
end;

end.

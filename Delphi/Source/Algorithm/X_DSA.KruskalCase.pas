unit X_DSA.KruskalCase;

interface

uses
  System.SysUtils,
  X_DSA.Utils;

type
  TArr_UChar = TArray<UChar>;
  TArr2D_int = TArray<TArray<integer>>;

  TEData = class(TObject)
  public
    StartPoint: UChar; // 边的一个点
    EndPoint: UChar; // 边的另外一个点
    Weight: integer; // 边的权值

    constructor Create(newStartPoint, newEndPoint: UChar; newWeight: integer);
    destructor Destroy; override;
    function ToString: string; override;
  end;

  TArr_TEData = TArray<TEData>;
  TArr_int = TArray<integer>;

  TKruskalCase = class
  private
    __edgeNum: integer; // 边的个数
    __vertexs: TArr_UChar; // 顶点数组
    __matrix: TArr2D_int; // 邻接矩阵

    // 对边进行排序处理, 冒泡排序
    // edges: 边的集合
    procedure __sortEdges(edges: TArr_TEData);
    // 返回ch顶点对应的下标，如果找不到，返回-1
    function __getPosition(chr: UChar): integer;
    // 获取图中边，放到EData[] 数组中，后面我们需要遍历该数组
    // 是通过matrix 邻接矩阵来获取
    // EData[] 形式 [['A','B', 12], ['B','F',7], .....]
    function __getEdges: TArr_TEData;
    // 获取下标为i的顶点的终点(), 用于后面判断两个顶点的终点是否相同
    // ends ： 数组就是记录了各个顶点对应的终点是哪个,ends 数组是在遍历过程中，逐步形成
    // i : 表示传入的顶点对应的下标
    // 返回的就是下标为 i 的这个顶点对应的终点的下标
    function __getEnd(ends: TArr_int; i: integer): integer;

  public
    constructor Create(vertexs: TArr_UChar; matrix: TArr2D_int);
    destructor Destroy; override;

    procedure Print;
    procedure Kruskal;
  end;

const
  INF = MaxInt;

procedure Main;

implementation

procedure Main;
var
  vertexs: TArr_UChar;
  matrix: TArr2D_int;
  kc: TKruskalCase;
begin
  vertexs := ['A', 'B', 'C', 'D', 'E', 'F', 'G'];

  // 克鲁斯卡尔算法的邻接矩阵
  matrix := [
    [000, 012, INF, INF, INF, 016, 014],
    [012, 000, 010, INF, INF, 007, INF],
    [INF, 010, 000, 003, 005, 006, INF],
    [INF, INF, 003, 000, 004, INF, INF],
    [INF, INF, 005, 004, 000, 002, 008],
    [016, 007, 006, INF, 002, 000, 009],
    [014, INF, INF, INF, 008, 009, 000]];

  kc := TKruskalCase.Create(vertexs, matrix);
  kc.Print;
  kc.Kruskal;
end;

{ TEData }

constructor TEData.Create(newStartPoint, newEndPoint: UChar; newWeight: integer);
begin
  StartPoint := newStartPoint;
  EndPoint := newEndPoint;
  Weight := newWeight;
end;

destructor TEData.Destroy;
begin
  inherited Destroy;
end;

function TEData.ToString: string;
begin
  Result := 'EData [<' + StartPoint + ', ' + EndPoint + '>= ' + Weight.ToString + ']';
end;

{ TKruskalCase }

constructor TKruskalCase.Create(vertexs: TArr_UChar; matrix: TArr2D_int);
var
  i, j: integer;
begin
  __vertexs := Copy(vertexs);
  __matrix := Copy(matrix);

  for i := 0 to High(__matrix) do
  begin
    for j := i + 1 to High(__matrix[i]) do
    begin
      if __matrix[i, j] <> INF then
        Inc(__edgeNum);
    end;
  end;
end;

destructor TKruskalCase.Destroy;
begin
  inherited Destroy;
end;

procedure TKruskalCase.Kruskal;
var
  ends: TArr_int; // 用于保存"已有最小生成树" 中的每个顶点在最小生成树中的终点
  rets: TArr_TEData;
  edges: TArr_TEData; // 创建结果数组, 保存最后的最小生成树
  i, p1, p2, m, n: integer;
begin
  rets := nil;
  SetLength(ends, __edgeNum);

  // 获取图中 所有的边的集合 ， 一共有12边
  edges := __getEdges;
  Write('图的边的集合=');
  for i := 0 to High(edges) do
    Write(edges[i].ToString);
  writeln(' 共' + Length(edges).ToString);

  // 按照边的权值大小进行排序(从小到大)
  __sortEdges(edges);

  // 遍历edges 数组，将边添加到最小生成树中时，判断是准备加入的边否形成了回路，
  // 如果没有，就加入 rets, 否则不能加入
  for i := 0 to High(edges) do
  begin
    // 获取到第i条边的第一个顶点(起点)
    p1 := __getPosition(edges[i].StartPoint);
    // 获取到第i条边的第2个顶点
    p2 := __getPosition(edges[i].EndPoint);

    // 获取 p1这个顶点在已有最小生成树中的终点
    m := __getEnd(ends, p1);
    // 获取 p2这个顶点在已有最小生成树中的终点
    n := __getEnd(ends, p2);

    // 是否构成回路
    if m <> n then // 没有构成回路
    begin
      ends[m] := n; // 设置m 在"已有最小生成树"中的终点
      SetLength(rets, Length(rets) + 1);
      rets[High(rets)] := edges[i]; // 有
    end;
  end;

  Write('最小生成树为');
  for i := 0 to High(rets) do
    Write(rets[i].ToString);
  writeln;
end;

procedure TKruskalCase.Print;
var
  i, j: integer;
begin
  for i := 0 to Length(__matrix) - 1 do
  begin
    Write('[');

    for j := 0 to Length(__matrix[i]) - 1 do
    begin
      if j <> Length(__matrix[i]) - 1 then
        Write(Format('%12d,', [__matrix[i, j]]))
      else
        Write(Format('%12d', [__matrix[i, j]]));
    end;

    writeln(']');
  end;
end;

function TKruskalCase.__getEdges: TArr_TEData;
var
  i, j: integer;
  ret: TArr_TEData;
begin
  ret := nil;

  for i := 0 to High(__matrix) do
  begin
    for j := i + 1 to High(__matrix[i]) do
    begin
      if __matrix[i, j] <> INF then
      begin
        SetLength(ret, Length(ret) + 1);
        ret[High(ret)] := TEData.Create(__vertexs[i], __vertexs[j], __matrix[i, j]);
      end;
    end;
  end;

  Result := ret;
end;

function TKruskalCase.__getEnd(ends: TArr_int; i: integer): integer;
begin
  while ends[i] <> 0 do
    i := ends[i];

  Result := i;
end;

function TKruskalCase.__getPosition(chr: UChar): integer;
var
  i: integer;
begin
  Result := -1;

  for i := 0 to Length(__vertexs) - 1 do
  begin
    if (__vertexs[i] = chr) then
    begin
      Result := i;
    end;
  end;
end;

procedure TKruskalCase.__sortEdges(edges: TArr_TEData);
var
  i, j: integer;
  tmp: TEData;
begin
  for i := 0 to High(edges) - 1 do
  begin
    for j := 0 to High(edges) - 1 - i do
    begin
      if edges[j].Weight > edges[j + 1].Weight then
      begin
        tmp := edges[j];
        edges[j] := edges[j + 1];
        edges[j + 1] := tmp;
      end;
    end;
  end;
end;

end.

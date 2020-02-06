unit X_DSA.DijkstraAlgorithm;

interface

uses
  System.SysUtils,
  X_DSA.Utils;

type
  TArr_Int = TArray<integer>;
  TArr_UChar = TArray<UChar>;
  TArr2D_int = TArray<TArray<integer>>;

  TVisitedVertex = class
  private
    // 记录各个顶点是否访问过 1表示访问过,0未访问,会动态更新
    __already_arr: TArr_int;
    // 每个下标对应的值为前一个顶点下标, 会动态更新
    __pre_visited: TArr_int;
    // 记录出发顶点到其他所有顶点的距离,比如G为出发顶点，就会记录G到其它顶点的距离，会动态更新，求的最短距离就会存放到dis
    __dis: TArr_int;

  public
    // length :表示顶点的个数
    // index: 出发顶点对应的下标, 比如 G 顶点，下标就是 6
    constructor Create(len, index: integer);
    destructor Destroy; override;

    // 判断 index 顶点是否被访问过
    function IsVisited(index: integer): boolean;
    // 更新出发顶点到 index 顶点的距离
    procedure UpdateDis(index: integer; len: integer);
    // 更新 pre 这个顶点的前驱顶点为 index 顶点
    procedure UpdatePre(pre: integer; index: integer);
    // 返回出发顶点到 index 顶点的距离
    function GetDis(index: integer): integer;
    // 继续选择并返回新的访问顶点， 比如这里的 G 完后，
    // 就是 A 点作为新的访问顶点
    function UpdateArr: integer;
    // 显示最后的结果
    // 即将三个数组的情况输出
    procedure Show;
  end;

  TGraph = class
  private
    __vertex: TArr_UChar; // 顶点数组
    __matrix: TArr2D_int; // 邻接矩阵
    __vv: TVisitedVertex; // 已经访问的顶点的集合

  public
    constructor Create(vertex: TArr_UChar; matrix: TArr2D_int);
    destructor Destroy; override;

    procedure ShowGraph;
    // 显示结果
    procedure ShowDijkstra;
    // 迪杰斯特拉算法实现
    // index: 表示出发顶点对应的下标
    procedure Dijkstra(index: integer);
    // 更新 index 下标顶点到周围顶点的距离和周围顶点的前驱顶点
    procedure Update(index: integer);
  end;

const
  N = 65535; // 表示不可以连接

procedure Main;

implementation

procedure Main;
var
  vertex: TArr_UChar;
  matrix: TArr2D_int;
  graph: TGraph;
begin
  vertex := ['A', 'B', 'C', 'D', 'E', 'F', 'G'];
  // 邻接矩阵
  matrix := [
    [N, 5, 7, N, N, N, 2],
    [5, N, N, 9, N, N, 3],
    [7, N, N, N, 8, N, N],
    [N, 9, N, N, N, 4, N],
    [N, N, 8, N, N, 5, 4],
    [N, N, N, 4, 5, N, 6],
    [2, 3, N, N, 4, 6, N]];

  graph := TGraph.Create(vertex, matrix);
  graph.ShowGraph;
  graph.Dijkstra(2);
  graph.ShowDijkstra;
end;

{ TVisitedVertex }

constructor TVisitedVertex.Create(len, index: integer);
var
  i: integer;
begin
  SetLength(Self.__already_arr, len);
  SetLength(Self.__pre_visited, len);
  SetLength(Self.__dis, len);

  // 初始化 __dis 数组
  for i := 0 to High(__dis) do
    __dis[i] := N;

  __already_arr[index] := 1; // 设置出发顶点被访问过
  __dis[index] := 0; // 设置出发顶点的访问距离为 0
end;

destructor TVisitedVertex.Destroy;
begin
  inherited Destroy;
end;

function TVisitedVertex.GetDis(index: integer): integer;
begin
  Result := __dis[index];
end;

function TVisitedVertex.IsVisited(index: integer): boolean;
begin
  Result := __already_arr[index] = 1;
end;

procedure TVisitedVertex.Show;
var
  i, Count: integer;
  vertex: array of UChar;
begin
  WriteLn('==========================');

  // 输出 __already_arr
  for i in __already_arr do
    Write(i.ToString + ' ');
  WriteLn;

  // 输出 __pre_visited
  for i in __pre_visited do
    Write(i.ToString + ' ');
  WriteLn;

  // 输出dis
  for i in __dis do
    Write(i.ToString + ' ');
  WriteLn;

  // 为了好看最后的最短距离，我们处理
  vertex := ['A', 'B', 'C', 'D', 'E', 'F', 'G'];
  Count := 0;

  for i in __dis do
  begin
    if i <> N then
    begin
      Write(vertex[Count], '(', i, ') ');
    end
    else
    begin
      WriteLn('N ');
    end;

    Inc(Count);
  end;

  WriteLn;
end;

function TVisitedVertex.UpdateArr: integer;
var
  index: integer;
  i: integer;
  min: integer;
begin
  index := 0;
  min := N;

  for i := 0 to High(__already_arr) do
  begin
    if (__already_arr[i] = 0) and (__dis[i] < min) then
    begin
      min := __dis[i];
      index := i;
    end;
  end;

  // 更新 index 顶点被访问过
  __already_arr[index] := 1;
  Result := index;
end;

procedure TVisitedVertex.UpdateDis(index: integer; len: integer);
begin
  __dis[index] := len;
end;

procedure TVisitedVertex.UpdatePre(pre: integer; index: integer);
begin
  __pre_visited[pre] := index;
end;

{ TGraph }

constructor TGraph.Create(vertex: TArr_UChar; matrix: TArr2D_int);
begin
  __vertex := vertex;
  __matrix := matrix;
end;

destructor TGraph.Destroy;
begin
  inherited Destroy;
end;

procedure TGraph.Dijkstra(index: integer);
var
  i: integer;
begin
  __vv := TVisitedVertex.Create(Length(__vertex), index);
  Update(index); // 更新 index 顶点到周围顶点的距离和前驱顶点

  for i := 0 to High(__vertex) do
  begin
    index := __vv.UpdateArr; // 选择并返回新的访问顶点
    Update(index); // 更新 index 顶点到周围顶点的距离和前驱顶点
  end;
end;

procedure TGraph.ShowDijkstra;
begin
  __vv.Show;
end;

procedure TGraph.ShowGraph;
var
  i, j: integer;
begin
  for i := 0 to High(__matrix) do
  begin
    Write('[');

    for j := 0 to High(__matrix[i]) do
    begin
      if j <> High(__matrix[i]) then
        Write(Format('%6d,', [__matrix[i, j]]))
      else
        Write(Format('%6d', [__matrix[i, j]]));
    end;

    WriteLn(']');
  end;
end;

procedure TGraph.Update(index: integer);
var
  len, i: integer;
begin
  len := 0;
  // 根据遍历我们的邻接矩阵的  matrix[index] 行
  for i := 0 to High(__matrix[index]) do
  begin
    // len 含义是 : 出发顶点到index顶点的距离 + 从index顶点到j顶点的距离的和
    len := __vv.GetDis(index) + __matrix[index, i];
    // 如果j顶点没有被访问过，并且 len 小于出发顶点到j顶点的距离，就需要更新
    if (not __vv.IsVisited(i)) and (len < __vv.GetDis(i)) then
    begin
      __vv.UpdatePre(i, index); // 更新 j 顶点的前驱为 index 顶点
      __vv.UpdateDis(i, len); // 更新出发顶点到 j 顶点的距离
    end;
  end;
end;

end.

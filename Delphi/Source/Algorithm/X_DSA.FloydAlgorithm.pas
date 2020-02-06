unit X_DSA.FloydAlgorithm;

interface

uses
  Classes,
  SysUtils,
  X_DSA.Utils;

type
  TArr_UChar = TArray<UChar>;
  TArr2D_int = TArray<TArray<integer>>;

  TGraph = class
  private
    __vertex: TArr_UChar; // 存放顶点的数组
    __dis: TArr2D_int; // 保存，从各个顶点出发到其它顶点的距离，最后的结果，也是保留在该数组
    __pre: TArr2D_int; // 保存到达目标顶点的前驱顶点

  public
    constructor Create(len: integer; matrix: TArr2D_int; vertex: TArr_UChar);
    destructor Destroy; override;

    // 显示 pre 数组和 dis 数组
    procedure Show;
    // 弗洛伊德算法, 比较容易理解，而且容易实现
    procedure Floyd;
  end;

const
  N = 65535;

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

  graph := TGraph.Create(Length(vertex), matrix, vertex);
  graph.Floyd;
  graph.Show;
end;

{ TGraph }

constructor TGraph.Create(len: integer; matrix: TArr2D_int; vertex: TArr_UChar);
var
  i, j: integer;
begin
  Self.__vertex := vertex;
  Self.__dis := matrix;

  SetLength(__pre, len, len);
  // 对pre数组初始化, 注意存放的是前驱顶点的下标
  for i := 0 to len - 1 do
  begin
    for j := 0 to len - 1 do
      __pre[i, j] := i;
  end;
end;

destructor TGraph.Destroy;
begin
  inherited Destroy;
end;

procedure TGraph.Floyd;
var
  len, k, i, j: integer;
begin
  len := 0; // 变量保存距离

  // 对中间顶点遍历， k 就是中间顶点的下标 [A, B, C, D, E, F, G]
  for k := 0 to High(__dis) do
  begin
    // 从i顶点开始出发 [A, B, C, D, E, F, G]
    for i := 0 to High(__dis) do
    begin
      // 到达 j 顶点 // [A, B, C, D, E, F, G]
      for j := 0 to High(__dis) do
      begin
        // => 求出从 i 顶点出发，经过 k 中间顶点，到达 j 顶点距离
        len := __dis[i, k] + __dis[k, j];

        // 如果 __dis[i, j] 大于 len
        if (__dis[i, j] > len) then
        begin
          __dis[i, j] := len; // 更新距离
          __pre[i, j] := __pre[k, j]; // 更新前驱顶点
        end;
      end;
    end;
  end;
end;

procedure TGraph.Show;
var
  vertex: TArr_UChar;
  k, i: integer;
begin
  // 为了显示便于阅读，我们优化一下输出
  vertex := ['A', 'B', 'C', 'D', 'E', 'F', 'G'];

  for k := 0 to High(__dis) do
  begin
    // 先将 pre 数组输出的一行
    for i := 0 to High(__dis) do
      Write(vertex[__pre[k][i]], ' ');
    WriteLn;

    // 输出dis数组的一行数据
    for i := 0 to High(__dis) do
      Write('(', vertex[k], '到', vertex[i], '的最短路径是', __dis[k][i], ') ');

    WriteLn;
  end;
end;

end.

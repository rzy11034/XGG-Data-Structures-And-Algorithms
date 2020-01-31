unit X_DSA.DijkstraAlgorithm;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  X_DSA.Utils;

type
  TArr_int = array of integer;
  TArr_UChar = array of UChar;
  TArr2D_int = array of array of integer;

  TVisitedVertex = class

  public
    // 记录各个顶点是否访问过 1表示访问过,0未访问,会动态更新
    Already_arr: TArr_int;
    // 每个下标对应的值为前一个顶点下标, 会动态更新
    Pre_visited: TArr_int;
    // 记录出发顶点到其他所有顶点的距离,比如G为出发顶点，就会记录G到其它顶点的距离，会动态更新，求的最短距离就会存放到dis
    Dis: TArr_int;

    // length :表示顶点的个数
    // index: 出发顶点对应的下标, 比如 G 顶点，下标就是 6
    constructor Create(len, index: integer);
    destructor Destroy; override;
  end;

  TGraph = class
  private
    __vertex: TArr_UChar; // 顶点数组
    __matrix: TArr2D_int; // 邻接矩阵

  public
    constructor Create(vertex: TArr_UChar; matrix: TArr2D_int);
    destructor Destroy; override;

    procedure ShowGraph;
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
  //邻接矩阵
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
end;

{ TVisitedVertex }

constructor TVisitedVertex.Create(len, index: integer);
begin

end;

destructor TVisitedVertex.Destroy;
begin
  inherited Destroy;
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

end.

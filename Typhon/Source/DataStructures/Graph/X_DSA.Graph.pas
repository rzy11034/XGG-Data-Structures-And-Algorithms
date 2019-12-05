unit X_DSA.Graph;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections;

type
  TGraph = class
  private type
    TListOfStr = specialize TList<string>;
    TArrayOfStr = specialize TArray<string>;
    TArrayOfInt = specialize TArray<integer>;

  var
    __vertexList: TListOfStr; // 存储顶点集合
    __edges: array of array of integer; // 存储图对应的邻结矩阵
    __numOfEdges: integer; // 表示边的数目
    __isVisitedForDFS: array of boolean; // 定义给数组, 记录 DFS 某个结点是否被访问
    __isVisitedForBFS: array of boolean; // 定义给数组, 记录 BFS 某个结点是否被访问
    // 插入结点
    procedure __insertVertex(vertex: string);

  public
    constructor Create(const strArr: TArrayOfStr);
    destructor Destroy; override;

    // 返回结点的个数
    function GetNumOfVertex: integer;
    // 显示图对应的矩阵
    procedure ShowGraph;
    // 返回边的数目
    function GetNumOfEdges: integer;
    // 返回结点 i (下标)对应的数据 0->"A" 1->"B" 2->"C"
    function GetValueByIndex(i: integer): string;
    // 返回 v1 和 v2 的权值
    function GetWeight(v1, v2: integer): integer;
    // 添加边
    // v1 表示点的下标即使第几个顶点  "A"-"B" "A"->0 "B"->1
    // v2 第二个顶点对应的下标
    // weight 表示权
    procedure InsertEdge(v1, v2: integer; weight: integer);
    // 返回图中一个顶点的所有邻边
    function AdjIterator(v: integer): TArrayOfInt;
    // 图的深度优先遍历
    procedure Dfs(v: integer);
    // 图的深度优先遍历
    procedure Bfs(v: integer);
  end;

implementation

{ TGraph }

constructor TGraph.Create(const strArr: TArrayOfStr);
var
  n, i: integer;
begin
  n := Length(strArr);

  __vertexList := TListOfStr.Create;
  SetLength(__edges, n, n);
  SetLength(__isVisitedForDFS, n);
  __numOfEdges := 0;

  for i := 0 to n - 1 do
  begin
    __insertVertex(strArr[i]);
    __isVisitedForDFS[i] := False;
  end;

  __isVisitedForBFS := Copy(__isVisitedForDFS);
end;

function TGraph.AdjIterator(v: integer): TArrayOfInt;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to Length(__edges[v]) - 1 do
  begin
    if __edges[v, i] <> 0 then
    begin
      SetLength(Result, i + 1);
      Result[High(Result)] := i;
    end;
  end;
end;

procedure TGraph.Bfs(v: integer);
type
  TQueueOfInt = specialize TQueue<integer>;
var
  queue: TQueueOfInt;
  i, tmp: integer;
begin
  queue := TQueueOfInt.Create;
  try
    // 将顶点加入队列
    queue.Enqueue(v);
    // 标记为已访问
    __isVisitedForBFS[v] := True;

    while queue.Count <> 0 do
    begin
      // 取出队列的头顶点
      tmp := queue.Dequeue;

      // 查找结点 tmp 的所有邻边, 没有访问过的加入队列
      for i in AdjIterator(tmp) do
      begin
        if __isVisitedForBFS[i] = False then
        begin
          queue.Enqueue(i);
          __isVisitedForBFS[i] := True;
        end;
      end;

      // 输出该结点
      Write(GetValueByIndex(tmp), ' => ');
    end;
  finally
    FreeAndNil(queue);
  end;
end;

destructor TGraph.Destroy;
begin
  FreeAndNil(__vertexList);
  inherited Destroy;
end;

function TGraph.GetNumOfEdges: integer;
begin
  Result := __numOfEdges;
end;

function TGraph.GetNumOfVertex: integer;
begin
  Result := __vertexList.Count;
end;

function TGraph.GetValueByIndex(i: integer): string;
begin
  Result := __vertexList[i];
end;

function TGraph.GetWeight(v1, v2: integer): integer;
begin
  Result := __edges[v1, v2];
end;

procedure TGraph.InsertEdge(v1, v2: integer; weight: integer);
begin
  __edges[v1, v2] := weight;
  __edges[v2, v1] := weight;
  Inc(__numOfEdges);
end;

procedure TGraph.ShowGraph;
var
  i, j: integer;
begin
  for i := 0 to Length(__edges) - 1 do
  begin
    Write('[');

    for j := 0 to Length(__edges[0]) - 1 do
    begin
      if j <> Length(__edges[0]) - 1 then
        Write(__edges[i, j], ', ')
      else
        Write(__edges[i, j]);
    end;

    WriteLn(']');
  end;
end;

procedure TGraph.Dfs(v: integer);
var
  i: integer;
begin
  //首先我们访问该结点, 输出
  Write(GetValueByIndex(v), ' -> ');
  // 将结点设置为已经访问
  __isVisitedForDFS[v] := True;

  // 查找结点 V 的所有邻边
  for i in AdjIterator(v) do
  begin
    if __isVisitedForDFS[i] = False then
    begin
      Dfs(i);
    end;
  end;
end;

procedure TGraph.__insertVertex(vertex: string);
begin
  __vertexList.Add(vertex);
end;

end.

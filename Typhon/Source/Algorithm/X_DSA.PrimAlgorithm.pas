unit X_DSA.PrimAlgorithm;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  X_DSA.Utils;

type
  TArr_UChar = array of UChar;
  TArr2D_int = array of array of integer;

  TGraph = class
  public
    Data: TArr_UChar;
    Verxs: integer;
    Weight: TArr2D_int;
    constructor Create(newVerxs: integer);
  end;

  TMinTree = class
  public
    // 创建图的邻接矩阵
    // graph: 图对象
    // verxs: 图对应的顶点个数
    // data: 图的各个顶点的值
    // weight: 图的邻接矩阵
    procedure CreateGraph(graph: TGraph; verxs: integer; Data: TArr_UChar; weight: TArr2D_int);
    procedure ShowGraph(graph: TGraph);
    // 编写 prim 算法，得到最小生成树
    procedure Prim(graph: TGraph; v: integer);
  end;

procedure Main;

implementation

procedure Main;
var
  datas: TArr_UChar;
  verxs: integer;
  weight: TArr2D_int;
  graph: TGraph;
  minTree: TMinTree;
begin
  datas := ['A', 'B', 'C', 'D', 'E', 'F', 'G'];
  verxs := Length(datas);
  // 邻接矩阵的关系使用二维数组表示,10000这个大数，表示两个点不联通
  weight := [
    [10000, 00005, 00007, 10000, 10000, 10000, 00002],
    [00005, 10000, 10000, 00009, 10000, 10000, 00003],
    [00007, 10000, 10000, 10000, 00008, 10000, 10000],
    [10000, 00009, 10000, 10000, 10000, 00004, 10000],
    [10000, 10000, 00008, 10000, 10000, 00005, 00004],
    [10000, 10000, 10000, 00004, 00005, 10000, 00006],
    [00002, 00003, 10000, 10000, 00004, 00006, 10000]];

  graph := TGraph.Create(verxs);
  minTree := TMinTree.Create;

  minTree.CreateGraph(graph, verxs, datas, weight);
  minTree.ShowGraph(graph);
end;

{ TMinTree }

procedure TMinTree.CreateGraph(graph: TGraph; verxs: integer; Data: TArr_UChar;
  weight: TArr2D_int);
var
  i, j: integer;
begin
  for i := 0 to Verxs - 1 do
  begin
    graph.Data[i] := Data[i];

    for j := 0 to Verxs - 1 do
    begin
      graph.Weight[i, j] := weight[i, j];
    end;
  end;
end;

procedure TMinTree.ShowGraph(graph: TGraph);
var
  i, j: integer;
begin
  for i := 0 to Length(graph.Weight) - 1 do
  begin
    Write('[');

    for j := 0 to Length(graph.Weight[0]) - 1 do
    begin
      if j <> Length(graph.Weight[0]) - 1 then
        Write(graph.Weight[i, j], ', ')
      else
        Write(graph.Weight[i, j]);
    end;

    WriteLn(']');
  end;
end;

procedure TMinTree.Prim(graph: TGraph; v: integer);
begin

end;

{ TGraph }

constructor TGraph.Create(newVerxs: integer);
begin
  Verxs := newVerxs;
  SetLength(Data, Verxs);
  SetLength(Weight, Verxs, Verxs);
end;

end.

unit X_DSA.PrimAlgorithm;

interface

uses
  System.SysUtils,
  X_DSA.Utils;

type
  TArr_UChar = TArray<UChar>;
  TArr2D_int = TArray<TArray<integer>>;

  TGraph = class
  public
    Data: TArr_UChar;
    Verxs: integer;
    Weight: TArr2D_int;
    constructor Create(newVerxs: integer);
  end;

  TMinTree = class
  public
    /// <summary>创建图的邻接矩阵</summary>
    /// <param name ="graph">图对象</param>
    /// <param name="verxs">图对应的顶点个数</param>
    /// <param name="data">图的各个顶点的值</param>
    /// <param name="weight">图的邻接矩阵</param>
    procedure CreateGraph(graph: TGraph; Verxs: integer; Data: TArr_UChar; Weight: TArr2D_int);
    procedure ShowGraph(graph: TGraph);
  end;

procedure Main;

implementation

procedure Main;
var
  datas: TArr_UChar;
  Verxs: integer;
  Weight: TArr2D_int;
  graph: TGraph;
  minTree: TMinTree;
begin
  datas := ['A', 'B', 'C', 'D', 'E', 'F', 'G'];
  Verxs := Length(datas);
  // 邻接矩阵的关系使用二维数组表示,10000这个大数，表示两个点不联通
  Weight := [
    [10000, 00005, 00007, 10000, 10000, 10000, 00002],
    [00005, 10000, 10000, 00009, 10000, 10000, 00003],
    [00007, 10000, 10000, 10000, 00008, 10000, 10000],
    [10000, 00009, 10000, 10000, 10000, 00004, 10000],
    [10000, 10000, 00008, 10000, 10000, 00005, 00004],
    [10000, 10000, 10000, 00004, 00005, 10000, 00006],
    [00002, 00003, 10000, 10000, 00004, 00006, 10000]];

  graph := TGraph.Create(Verxs);
  minTree := TMinTree.Create;

  minTree.CreateGraph(graph, Verxs, datas, Weight);
  minTree.ShowGraph(graph);
end;

{ TGraph }

constructor TGraph.Create(newVerxs: integer);
begin
  Verxs := newVerxs;
  SetLength(Data, Verxs);
  SetLength(Weight, Verxs, Verxs);
end;

{ TMinTree }

procedure TMinTree.CreateGraph(graph: TGraph; Verxs: integer; Data: TArr_UChar;
  Weight: TArr2D_int);
var
  i, j: integer;
begin
  for i := 0 to Verxs - 1 do
  begin
    graph.Data[i] := Data[i];

    for j := 0 to Verxs - 1 do
    begin
      graph.Weight[i, j] := Weight[i, j];
    end;
  end;
end;

procedure TMinTree.ShowGraph(graph: TGraph);
var
  i, j: integer;
begin
  for i := 0 to Length(graph.Weight) - 1 do
  begin
    write('[');

    for j := 0 to Length(graph.Weight[0]) - 1 do
    begin
      if j <> Length(graph.Weight[0]) - 1 then
        write(graph.Weight[i, j], ', ')
      else
        write(graph.Weight[i, j]);
    end;

    WriteLn(']');
  end;
end;

end.

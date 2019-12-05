unit X_DSA.GraphDemo;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

procedure Main;

implementation

uses
  X_DSA.Graph;

procedure Main;
var
  vertexs: array of string;
  g: TGraph;
begin
  vertexs := ['A', 'B', 'C', 'D', 'E'];
  vertexs := ['1', '2', '3', '4', '5', '6', '7', '8'];
  g := TGraph.Create(vertexs);

  //g.InsertEdge(0, 1, 1);
  //g.InsertEdge(0, 2, 1);
  //g.InsertEdge(1, 2, 1);
  //g.InsertEdge(1, 3, 1);
  //g.InsertEdge(1, 4, 1);

  g.InsertEdge(0, 1, 1);
  g.InsertEdge(0, 2, 1);
  g.InsertEdge(1, 3, 1);
  g.InsertEdge(1, 4, 1);
  g.InsertEdge(3, 7, 1);
  g.InsertEdge(4, 7, 1);
  g.InsertEdge(2, 5, 1);
  g.InsertEdge(2, 6, 1);
  g.InsertEdge(5, 6, 1);

  g.ShowGraph;

  g.Dfs(0);
  WriteLn;

  g.Bfs(0);
  WriteLn;
end;

end.

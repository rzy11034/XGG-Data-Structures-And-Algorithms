unit X_DSA.KruskalCase;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  X_DSA.Utils;

type
  TArr_UChar = array of UChar;
  TArr2D_int = array of array of integer;

  { TKruskalCase }

  TKruskalCase = class
  private
    __edgeNum: integer; //边的个数
    __vertexs: TArr_UChar; //顶点数组
    __matrix: TArr2D_int; //邻接矩阵

  public
    constructor Create(vertexs: TArr_UChar; matrix: TArr2D_int);
    destructor Destroy; override;

    procedure Print;
  end;

const
  INF = MaxInt;

procedure Main;

implementation

procedure Main;
var
  vertexs: TArr_UChar;
  matrix: TArr2D_int;
begin
  vertexs := ['A', 'B', 'C', 'D', 'E', 'F', 'G'];

  //克鲁斯卡尔算法的邻接矩阵
  matrix := [
    [000, 012, INF, INF, INF, 016, 014],
    [012, 000, 010, INF, INF, 007, INF],
    [INF, 010, 000, 003, 005, 006, INF],
    [INF, INF, 003, 000, 004, INF, INF],
    [INF, INF, 005, 004, 000, 002, 008],
    [016, 007, 006, INF, 002, 000, 009],
    [014, INF, INF, INF, 008, 009, 000]];
end;

{ TKruskalCase }

constructor TKruskalCase.Create(vertexs: TArr_UChar; matrix: TArr2D_int);
begin

end;

destructor TKruskalCase.Destroy;
begin
  inherited Destroy;
end;

procedure TKruskalCase.Print;
begin

end;

end.

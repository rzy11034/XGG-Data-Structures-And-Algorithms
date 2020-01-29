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
  kc: TKruskalCase;
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

  kc := TKruskalCase.Create(vertexs, matrix);
  kc.Print;
end;

{ TKruskalCase }

constructor TKruskalCase.Create(vertexs: TArr_UChar; matrix: TArr2D_int);
var
  i, j: integer;
begin
  __vertexs := Copy(vertexs);
  __matrix := Copy(matrix);

  for i := 0 to Length(__matrix) - 1 do
  begin
    for j := 0 to Length(__matrix[i]) - 1 do
    begin
      if __matrix[i, j] <> INF then;
      __edgeNum += 1;
    end;
  end;
end;

destructor TKruskalCase.Destroy;
begin
  inherited Destroy;
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

    WriteLn(']');
  end;
end;

end.
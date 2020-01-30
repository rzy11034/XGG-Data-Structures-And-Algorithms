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

  TEData = class(TObject)
  public
    StartPoint: UChar; // 边的一个点
    EndPoint: UChar; // 边的另外一个点
    Weight: integer; // 边的权值

    constructor Create(newStartPoint, newEndPoint: UChar; newWeight: integer);
    destructor Destroy; override;
    function ToString: string; override;
  end;

  TArr_EData = array of TEData;
  TArr_int = array of integer;

  TKruskalCase = class
  private
    __edgeNum: integer; //边的个数
    __vertexs: TArr_UChar; //顶点数组
    __matrix: TArr2D_int; //邻接矩阵

    // 对边进行排序处理, 冒泡排序
    // edges: 边的集合
    procedure __sortEdges(edges: TArr_EData);
    // 返回ch顶点对应的下标，如果找不到，返回-1
    function __getPosition(chr: UChar): integer;
    //获取图中边，放到EData[] 数组中，后面我们需要遍历该数组
    // 是通过matrix 邻接矩阵来获取
    // EData[] 形式 [['A','B', 12], ['B','F',7], .....]
    function __getEdges: TArr_EData;
    // 获取下标为i的顶点的终点(), 用于后面判断两个顶点的终点是否相同
    // ends ： 数组就是记录了各个顶点对应的终点是哪个,ends 数组是在遍历过程中，逐步形成
    // i : 表示传入的顶点对应的下标
    // 返回的就是下标为 i 的这个顶点对应的终点的下标
    function __getEnd(ends: TArr_int; i: integer): integer;

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
  edges: TArr_EData;
  i: integer;
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
  Result := 'EData [<' + StartPoint + ', ' + EndPoint + '>= ' + weight.ToString + ']';
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

function TKruskalCase.__getEdges: TArr_EData;
var
  i, j: integer;
  ret: TArr_EData = nil;
begin
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

procedure TKruskalCase.__sortEdges(edges: TArr_EData);
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
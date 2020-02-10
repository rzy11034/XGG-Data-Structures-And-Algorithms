unit X_DSA.HorseChessboard;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  X_DSA.Utils;

type
  TArr_bln = array of boolean;
  TArr_int = array of integer;
  TArr2D_int = array of array of integer;
  TList_TPoint = specialize TList<TPoint>;

procedure Main;

// 根据当前位置(Point对象)，计算马儿还能走哪些位置(Point)，
// 并放入到一个集合中(ArrayList), 最多有8个位置
function Next(curPoint: TPoint): TList_TPoint;

// 完成骑士周游问题的算法
// chessboard: 棋盘
// row: 马儿当前的位置的行 从 0开始
// col: 马儿当前的位置的列  从 0开始
// step: 是第几步 ,初始位置就是第 1 步
procedure TraversalChessboard(chessboard: TArr2D_int; row, col, step: integer);

implementation

var
  X: integer = 8; // 棋盘的列数
  Y: integer = 8; // 棋盘的行数
  //创建一个数组，标记棋盘的各个位置是否被访问过
  visited: TArr_bln;
  //使用一个属性，标记是否棋盘的所有位置都被访问
  finished: boolean; // 如果为true,表示成功

procedure Main;
var
  chessboard: TArr2D_int;
  row, col, i, j: integer;
  startTime, endTime: QWord;
begin
  WriteLn('骑士周游算法，开始运行~~');

  //测试骑士周游算法是否正确
  row := 1; // 马儿初始位置的行，从 1开始编号
  col := 1; // 马儿初始位置的列，从 1开始编号

  // 创建棋盘
  SetLength(chessboard, X, Y);
  SetLength(visited, X * Y); // 初始值都是 false

  // 测试一下耗时
  startTime := TThread.GetTickCount64;
  TraversalChessboard(chessboard, row - 1, col - 1, 1);
  endTime := TThread.GetTickCount64;
  WriteLn('共耗时: ', endTime - startTime, ' 毫秒');

  // 输出棋盘的最后情况
  for i := 0 to High(chessboard) do
  begin
    for j := 0 to High(chessboard[i]) do
    begin
      if j <> High(chessboard[i]) then
        Write(chessboard[i, j], ', ', #9)
      else
        WriteLn(chessboard[i, j]);
    end;
  end;
end;

function Next(curPoint: TPoint): TList_TPoint;
var
  ps: TList_TPoint;
begin
  ps := TList_TPoint.Create;

  // 表示马儿可以走 5 这个位置
  if (curPoint.X - 2 >= 0) and (curPoint.Y - 1 >= 0) then
    ps.add(TPoint.Create(curPoint.X - 2, curPoint.Y - 1));

  // 判断马儿可以走 6 这个位置
  if (curPoint.X - 1 >= 0) and (curPoint.Y - 2 >= 0) then
    ps.add(TPoint.Create(curPoint.X - 1, curPoint.Y - 2));

  // 判断马儿可以走 7 这个位置
  if (curPoint.X + 1 < X) and (curPoint.Y - 2 >= 0) then
    ps.add(TPoint.Create(curPoint.X + 1, curPoint.Y - 2));

  // 判断马儿可以走 0 这个位置
  if (curPoint.X + 2 < X) and (curPoint.Y - 1 >= 0) then
    ps.add(TPoint.Create(curPoint.X + 2, curPoint.Y - 1));

  // 判断马儿可以走 1 这个位置
  if (curPoint.X + 2 < X) and (curPoint.Y + 1 < Y) then
    ps.add(TPoint.Create(curPoint.X + 2, curPoint.Y + 1));

  // 判断马儿可以走 2 这个位置
  if (curPoint.x + 1 < X) and (curPoint.y + 2 < Y) then
    ps.add(TPoint.Create(curPoint.x + 1, curPoint.y + 2));

  // 判断马儿可以走 3 这个位置
  if (curPoint.x - 1 >= 0) and (curPoint.y + 2 < Y) then
    ps.add(TPoint.Create(curPoint.x - 1, curPoint.y + 2));

  // 判断马儿可以走 4 这个位置
  if (curPoint.x - 2 >= 0) and (curPoint.y + 1 < Y) then
    ps.add(TPoint.Create(curPoint.x - 2, curPoint.y + 1));

  Result := ps;
end;

procedure TraversalChessboard(chessboard: TArr2D_int; row, col, step: integer);
var
  ps: TList_TPoint;
  p: TPoint;
begin
  chessboard[row, col] := step;
  // row = 4 X = 8 col = 4 = 4 * 8 + 4 = 36
  visited[row * X + col] := True; // 标记该位置已经访问
  // 获取当前位置可以走的下一个位置的集合
  ps := Next(TPoint.Create(col, row));

  // 对 ps 进行排序,排序的规则就是对 ps 的所有的 Point 对象的下一步的位置的数目，
  // 进行非递减排序
  //sort(ps);

  // 遍历 ps
  while ps.Count <> 0 do
  begin
    p := ps.ExtractIndex(0);// 取出下一个可以走的位置
    // 判断该点是否已经访问过
    if not visited[p.Y * X + p.X] then // 说明还没有访问过
      TraversalChessboard(chessboard, p.Y, p.X, step + 1);
  end;

  // 判断马儿是否完成了任务，使用   step 和应该走的步数比较 ，
  // 如果没有达到数量，则表示没有完成任务，将整个棋盘置0
  // 说明: step < X * Y  成立的情况有两种
  // 1. 棋盘到目前位置,仍然没有走完
  // 2. 棋盘处于一个回溯过程
  if (step < X * Y) and (not finished) then
  begin
    chessboard[row, col] := 0;
    visited[row * X + col] := False;
  end
  else
  begin
    finished := True;
  end;
end;

end.

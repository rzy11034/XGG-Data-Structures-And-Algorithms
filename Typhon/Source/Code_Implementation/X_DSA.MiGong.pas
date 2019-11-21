unit X_DSA.MiGong;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  TMiGong = array of array of integer;

procedure Main;

implementation

// 0表示该点没有走过
// 1为墙
// 2为表示通路可以走
// 3表示该点走过
// 策略（方法），上->右->下->左， 不通再回溯
function SetWay(map: TMiGong; i, j: integer): boolean;
begin
  if map[6, 5] = 2 then
    Exit(True);

  // 判断是否可行
  // 如果没有走过，则尝试 下->右->上->左 是否可行
  // 否则（ map[i,j] <> 0 的情况， 不通
  if map[i, j] = 0 then
  begin
    map[i, j] := 2;

    // 策略（方法），下->右->上->左
    if SetWay(map, i + 1, j) then // 向下试
      Exit(True)
    else if SetWay(map, i, j + 1) then // 向右试
      Exit(True)
    else if SetWay(map, i - 1, j) then // 向上试
      Exit(True)
    else if SetWay(map, i, j - 1) then // 向左试
      Exit(True)
    else // 不通，设为走过
    begin
      map[i, j] := 3;
      Exit(False);
    end;

  end
  else
  begin
    Exit(False);
  end;
end;

procedure Main;
var
  map: TMiGong;
  i, j: integer;
begin
  SetLength(map, 8, 7);

  for i := 0 to 6 do
  begin
    map[0, i] := 1;
    map[7, i] := 1;
  end;

  for i := 0 to 7 do
  begin
    map[i, 0] := 1;
    map[i, 6] := 1;
  end;

  map[3, 1] := 1;
  map[3, 2] := 1;
  map[1, 2] := 1;
  map[2, 2] := 1;

  for i := 0 to 7 do
  begin
    for j := 0 to 6 do
      Write(map[i, j], ' ');
    WriteLn;
  end;
  WriteLn;

  SetWay(map, 1, 1);

  for i := 0 to 7 do
  begin
    for j := 0 to 6 do
      Write(map[i, j], ' ');
    WriteLn;
  end;
end;

end.

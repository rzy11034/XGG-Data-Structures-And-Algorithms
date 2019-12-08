unit X_DSA.HanoiTower;

interface

procedure Main;
procedure HanoiTower(num: integer; a, b, c: char);

implementation

procedure Main;
begin
  HanoiTower(3, 'A', 'B', 'C');
end;

procedure HanoiTower(num: integer; a, b, c: char);
begin
  if num = 1 then
  begin
    WriteLn('第1个盘子: ', a, '->', c);
  end
  else
  begin
    // 如果我们有 n >= 2 情况，我们总是可以看做是两个盘 1.最下边的一个盘, 2.上面的所有盘
    // 1. 先把最上面的所有盘 A->B， 移动过程会使用到 c 塔
    HanoiTower(num - 1, a, c, b);
    // 2. 把最下边的盘 A->C
    WriteLn('第', num, '个盘子: ', a, '->', c);
    // 3. 把 B 塔的所有盘 从 B->C , 移动过程使用到 a
    HanoiTower(num - 1, b, a, c);
  end;
end;

end.

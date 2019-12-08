unit X_DSA.KnapsackProblem;

interface

procedure Main;

implementation

procedure Main;
type
  TArray2D = array of array of integer;

  procedure Print_Array2D(const arr: TArray2D);
  var
    i, j: integer;
  begin
    for i := 0 to Length(arr) - 1 do
    begin
      for j := 0 to Length(arr[i]) - 1 do
      begin
        Write(arr[i, j], ' ');
      end;
      WriteLn;
    end;
  end;

var
  val: array of integer;
  w: array of integer;
  m, n, i, j: integer;
  v, path: TArray2D;
begin
  val := [1500, 3000, 2000]; // 物品的价值
  w := [1, 4, 3]; // 物品的重量
  m := 4; // 背包的容量
  n := Length(val); // 物品的个数;

  //创建二维数组，
  //v[i][j] 表示在前i个物品中能够装入容量为j的背包中的最大价值
  SetLength(v, n + 1, m + 1);
  //为了记录放入商品的情况，我们定一个二维数组
  SetLength(path, n + 1, m + 1);

  // 初始化第一行和第一列, 这里在本程序中，可以不去处理，因为默认就是0
  for i := 0 to Length(v) - 1 do
    v[i][0] := 0; // 将第一列设置为 0
  for i := 0 to Length(v[0]) - 1 do
    v[0][i] := 0; // 将第一行设置 0

  // 根据前面得到公式来动态规划处理
  for i := 1 to Length(v) - 1 do
  begin
    for j := 1 to Length(v[i]) - 1 do
    begin
      if w[i - 1] > j then
        v[i][j] := v[i - 1][j]
      else
      begin
        //说明:
        //因为我们的i 从1开始的， 因此公式需要调整成
        //v[i][j]=Math.max(v[i-1][j], val[i-1]+v[i-1][j-w[i-1]]);
        //v[i][j] = Math.max(v[i - 1][j], val[i - 1] + v[i - 1][j - w[i - 1]]);
        //为了记录商品存放到背包的情况，我们不能直接的使用上面的公式，需要使用if-else来体现公式
        if v[i - 1][j] < (val[i - 1] + v[i - 1][j - w[i - 1]]) then
        begin
          v[i][j] := val[i - 1] + v[i - 1][j - w[i - 1]];
          // 把当前的情况记录到 path
          path[i][j] := 1;
        end
        else
        begin
          v[i][j] := v[i - 1][j];
        end;
      end;
    end;
  end;

  // 输出表
  Print_Array2D(v);
  WriteLn;
  //Print_Array2D(path);

  //动脑筋
  i := Length(path) - 1; //行的最大下标
  j := Length(path[0]) - 1; //列的最大下标
  while (i > 0) and (j > 0) do //从path的最后开始找
  begin
    if path[i][j] = 1 then
    begin
      WriteLn('第', i, '个商品放入到背包');
      j := j - w[i - 1]; //w[i-1]
    end;

    i := i - 1;
  end;
end;

end.

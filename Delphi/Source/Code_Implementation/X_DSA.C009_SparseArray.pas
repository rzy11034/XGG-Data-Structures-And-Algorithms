unit X_DSA.C009_SparseArray;

interface

uses
  Classes,
  SysUtils;

type
  TSparseArray = class(TObject)
  private type
    TList2D = TArray<TArray<integer>>;

  private
    __data: TList2D;
  public
    constructor Create(arr: TList2D);
    function ToString: string; override;
  end;

procedure Main;

implementation

procedure Main;
var
  chess: TArray<TArray<integer>>;
  i: integer;
  j: integer;
  sa: TSparseArray;
begin
  SetLength(chess, 11, 11);

  chess[1][2] := 1;
  chess[2][3] := 2;

  for i := 0 to Length(chess) - 1 do
  begin
    for j := 0 to Length(chess[i]) - 1 do
    begin
      Write(chess[i][j], ' ');
    end;
    Writeln;
  end;

  Writeln;

  sa := TSparseArray.Create(chess);

  Writeln(sa.ToString);

  sa.Free;
end;

{ TSparseArray }

constructor TSparseArray.Create(arr: TList2D);
var
  sum, i, j, row, col, n: integer;
begin
  sum := 0;
  row := Length(arr);
  col := Length(arr[0]);

  for i := 0 to row - 1 do
  begin
    for j := 0 to col - 1 do
    begin
      if arr[i][j] <> 0 then
        Inc(sum);
    end;
  end;

  // 生成稀疏数组
  SetLength(__data, sum + 1, 3);
  __data[0, 0] := row;
  __data[0, 1] := col;
  __data[0, 2] := sum;

  n := 1;
  for i := 0 to row - 1 do
  begin
    for j := 0 to col - 1 do
    begin
      if arr[i][j] <> 0 then
      begin
        __data[n][0] := i;
        __data[n][1] := j;
        __data[n][2] := arr[i][j];

        n := n + 1;
      end;
    end;
  end;
end;

function TSparseArray.ToString: string;
var
  sb: TStringBuilder;
  i, j: integer;
begin
  sb := TStringBuilder.Create;
  try
    for i := 0 to Length(__data) - 1 do
    begin
      for j := 0 to Length(__data) - 1 do
      begin
        sb.Append(__data[i, j]);

        if j <> (Length(__data) - 1) then
          sb.Append(', ');
      end;
      sb.AppendLine;
    end;

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

end.

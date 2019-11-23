unit X_DSA.Queen8;

interface

procedure Main;
function Judge(n: integer): boolean;
procedure Check(n: integer);
procedure Print;

implementation

const
  MAX = 8;

var
  Count: integer = 0;
  arr: array of integer;

procedure Main;
begin
  SetLength(arr, 8);
  Check(0);
end;

function Judge(n: integer): boolean;
var
  i: integer;
begin
  for i := 0 to n - 1 do
  begin
    if (arr[i] = arr[n]) or (Abs(n - i) = Abs(arr[n] - arr[i])) then
      Exit(False);
  end;

  Result := True;
end;

procedure Check(n: integer);
var
  i: integer;
begin
  if n = MAX then
  begin
    Count := Count + 1;
    Print;
    Exit;
  end;

  for i := 0 to MAX - 1 do
  begin
    arr[n] := i;

    if Judge(n) then
      Check(n + 1);
  end;
end;

procedure Print;
var
  i, j: integer;
begin
  Write('第', Count, '种解法: ');
  for i := 0 to MAX - 1 do
    Write(arr[i], ' ');
  WriteLn;

  for i := 0 to MAX - 1 do
  begin
    for j := 0 to MAX - 1 do
    begin
      if j = arr[i] then
        Write('1', ' ')
      else
        Write('0', ' ');
    end;
    WriteLn;
  end;
  WriteLn('------------------------');
end;

end.

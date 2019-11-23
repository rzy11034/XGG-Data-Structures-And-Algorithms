unit X_DSA.ShellSort;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

procedure Main;
procedure ShellSort(var arr: array of integer);

implementation

procedure Main;
begin

end;

procedure ShellSort(var arr: array of integer);
var
  gap: integer;
begin
  gap := Length(arr) - 1;
  while gap > 0 do
  begin
    gap := gap div 2;

    for i := gap to Length(arr) -1 do
    begin

    end;
  end;
end;

end.

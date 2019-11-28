unit X_DSA.HashTableDemo;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type

  TEmp = class   //表示一个雇员
  public
    ID: integer;
    Name: string;
    Next: Emp;

    constructor Create(newID: integer; newName: string);
  end;

  TEmpLinkedList = class
  private
    __head: TEmp;
  public
    // 添加雇员到链表
    // 说明
    // 假定，当添加雇员时，id 是自增长，即id的分配总是从小到大
    // 因此我们将该雇员直接加入到本链表的最后即可
    procedure Add(emp: TEmp);
  end;

implementation

{ TEmpLinkedList }

procedure TEmpLinkedList.Add(emp: TEmp);
var
  cur: TEmp;
begin
  // 如果是添加第一个雇员, 直接加入
  // 否则加入到最后
  if __head = nil then
  begin
    __head := emp;
    Exit;
  end
  else
  begin
    cur := __head;

    while cur.Next <> nil do
      cur := cur.Next;

    cur.Next := emp;
  end;
end;

{ Emp }

constructor TEmp.Create(newID: integer; newName: string);
begin
  ID := newID;
  Name := newName;
  Next := nil;
end;

end.

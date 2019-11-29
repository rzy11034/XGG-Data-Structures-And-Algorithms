unit X_DSA.HashTableDemo;

interface

uses
  System.SysUtils,
  System.TypInfo;

type

  TEmp = class //表示一个雇员
  public
    ID: integer;
    Name: string;
    Next: TEmp;
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

    // 遍历链表的雇员信息
    procedure Print(n: integer);

    // 根据 ID 查找雇员
    // 如果查找到，就返回 Emp, 如果没有找到，就返回 nil
    function FindID(ID: integer): TEmp;
  end;

  THashTable = class
  private
    __data: TArray<TEmpLinkedList>;
    __size: integer;

    function __getSize: integer;
    function __hashFun(ID: integer): integer;

  public
    constructor Create(size: integer);
    destructor Destroy; override;

    procedure Add(emp: TEmp);
    procedure Find(ID: integer);
    procedure Print;

    property size: integer read __getSize;
  end;

procedure Main;

implementation

type
  TMeun = (Add, list, Find, exit);

procedure Main;
var
  ht: THashTable;
  key, ID, Name: string;
  emp: TEmp;
  mn: TMeun;
begin
  ht := THashTable.Create(7);
  key := '';

  WriteLn('add:  添加雇员');
  WriteLn('list: 显示雇员');
  WriteLn('find: 查找雇员');
  WriteLn('exit: 退出系统');
  readLn(key);

  while True do
  begin
    mn := TMeun(GetEnumValue(TypeInfo(TMeun), key));
    case mn of
      TMeun.Add:
        begin
          WriteLn('输入ID:');
          readLn(ID);
          WriteLn('输入名字:');
          readLn(name);

          emp := TEmp.Create(ID.ToInteger, name);
          ht.Add(emp);
        end;

      TMeun.list:
        begin
          ht.Print;
        end;

      TMeun.Find:
        begin
          WriteLn('请输入要查找的id');
          readLn(ID);
          ht.Find(ID.ToInteger);
        end;

      TMeun.exit:
        begin
          Break;
        end;

    else
      WriteLn('errer');
      Break;
    end;
  end;
end;

{ THashTable }

constructor THashTable.Create(size: integer);
var
  i: integer;
begin
  __size := size;
  SetLength(__data, size);
  for i := 0 to size - 1 do
    __data[i] := TEmpLinkedList.Create;
end;

procedure THashTable.Add(emp: TEmp);
begin
  __data[__hashFun(emp.ID)].Add(emp);
end;

destructor THashTable.Destroy;
var
  i: integer;
begin
  for i := 0 to __size - 1 do
    __data[i].Free;

  inherited Destroy;
end;

procedure THashTable.Find(ID: integer);
var
  tmp: TEmp;
begin
  tmp := __data[__hashFun(ID)].FindID(ID);

  if tmp = nil then
    WriteLn('在哈希表中，没有找到该雇员~')
  else
    WriteLn(Format('在第%d条链表中找到 雇员 id = %d', [__hashFun(ID) + 1, ID]));
end;

procedure THashTable.Print;
var
  i: integer;
begin
  for i := 0 to __size - 1 do
    __data[i].Print(i);
end;

function THashTable.__hashFun(ID: integer): integer;
begin
  Result := ID mod __size;
end;

function THashTable.__getSize: integer;
begin
  Result := __size;
end;

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
  end
  else
  begin
    cur := __head;

    while cur.Next <> nil do
      cur := cur.Next;

    cur.Next := emp;
  end;
end;

function TEmpLinkedList.FindID(ID: integer): TEmp;
var
  cur, ret: TEmp;
begin
  ret := nil;

  if __head = nil then
  begin
    ret := nil;
  end
  else
  begin
    cur := __head;

    repeat
      if cur.ID = ID then
      begin
        ret := cur;
        Break;
      end;

      cur := cur.Next;
    until cur = nil;
  end;

  Result := ret;
end;

procedure TEmpLinkedList.Print(n: integer);
var
  cur: TEmp;
begin
  if __head = nil then
  begin
    WriteLn('第 ', n + 1, ' 链表为空.');
  end
  else
  begin
    write('第 ', n + 1, ' 链表的信息为:');

    cur := __head;
    repeat
      write(Format(' => id=%d name=%s ', [cur.ID, cur.Name]));
      cur := cur.Next;
    until cur = nil;

    WriteLn;
  end;
end;

{ Emp }

constructor TEmp.Create(newID: integer; newName: string);
begin
  ID := newID;
  name := newName;
  Next := nil;
end;

end.

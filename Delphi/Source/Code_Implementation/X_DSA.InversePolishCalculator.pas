unit X_DSA.InversePolishCalculator;

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type
  TStackOfString = TStack<string>;
  TStackOfChar = TStack<char>;
  TListOfString = TList<string>;

  TInversePolishCalculator = class
  const
    NUMBER_SET = ['0' .. '9', '.'];
    OPERATOR_SET = ['+', '-', '*', '/'];
    BRACKET_SET = ['(', ')', '[', ']'];
    LETTER_SET = ['a' .. 'z'];

  private
    __optStack: TStackOfString;
    __numStack: TStackOfString;

  public
    __infixExpressionList: TListOfString; // 中缀表达式元素列表
    __suffixExpressionList: TListOfString; // 后缀表达式元素列表

    /// <summary> 是否有效表达式 </summary>
    function __isValidExpression(str: string): boolean;
    /// <summary> 转换字符串为表达式列表 </summary>
    procedure __trimExpression(str: string);
    /// <summary> 返回操作符优先级 </summary>
    function __optPriority(opt: string): integer;
    /// <summary> 转换中缀表达式元素列表为后缀表达式元素列表 </summary>
    procedure __infixToSuffix;

    constructor Create;
    destructor Destroy; override;

    function Calculator(str: string): double;

  end;

procedure Main;

implementation

procedure Main;
var
  ipc: TInversePolishCalculator;
  s: string;
begin
  s := '1*(2+3)';

  ipc := TInversePolishCalculator.Create;
  //WriteLn(ipc.__isValidExpression());
  ipc.__trimExpression(s);

  for s in ipc.__infixExpressionList do
    Write(s, ' ');
  WriteLn;

  ipc.__infixToSuffix;

  for s in ipc.__suffixExpressionList do
    Write(s, ' ');
  WriteLn;


  ipc.Free;
end;

{ TInversePolishCalculator }

function TInversePolishCalculator.Calculator(str: string): double;
begin
  Result := 0.0;
end;

constructor TInversePolishCalculator.Create;
begin
  __numStack := TStackOfString.Create;
  __optStack := TStackOfString.Create;
  __infixExpressionList := TListOfString.Create;
  __suffixExpressionList := TListOfString.Create;
end;

destructor TInversePolishCalculator.Destroy;
begin
  __numStack.Free;
  __optStack.Free;
  __infixExpressionList.Free;
  __suffixExpressionList.Free;

  inherited;
end;

procedure TInversePolishCalculator.__infixToSuffix;
var
  stack: TStackOfString;
  opt: string;
  i: integer;
  d: double;
begin
  stack := TStackOfString.Create;
  try
    for i := 0 to __infixExpressionList.Count - 1 do
    begin
      // 判断是否为数字字符串，如果是直接加入后缀表达式列表，否则执行入栈操作
      if TryStrToFloat(__infixExpressionList[i], d) then
      begin
        __suffixExpressionList.Add(__infixExpressionList[i]);
      end
      else
      begin
        opt := __infixExpressionList[i];

        // 如果栈空直接入栈, 否则判断操作符优先级，
        if stack.Count = 0 then
          stack.Push(opt)
        else
        begin
          if __optPriority(stack.Peek) < __optPriority(opt) then
          begin
            stack.Push(opt);
          end
          else if __optPriority(stack.Peek) > __optPriority(opt) then
          begin
            if (opt = ')') then // 如果opt优先级低，并且 opt = ')' 出栈
            begin
              while stack.Count <> 0 do
              begin
                __suffixExpressionList.Add(stack.Peek);
                stack.Pop;

                if (stack.Peek = '(') then
                  Break;
              end;

              stack.Pop;
            end
            else if (opt = ']') then
            begin
              repeat
                __suffixExpressionList.Add(stack.Peek);
                stack.Pop;
              until stack.Peek = '[';

              stack.Pop;
            end
            else
            begin
              __suffixExpressionList.Add(stack.Peek);
              stack.Pop;
            end;
          end
          else // 优先级相等
          begin
            if opt = ')' then
            begin
              if (stack.Peek = '(') then
                stack.Pop
              else
                stack.Push(opt);
            end
            else if opt = ']' then
            begin
              if (stack.Peek = '[') then
                stack.Pop
              else
                stack.Push(opt);
            end
            else
              stack.Push(opt);
          end;
        end;
      end;
    end;

    while stack.Count <> 0 do
    begin
      __suffixExpressionList.Add(stack.Peek);
      stack.Pop;
    end;
  finally
    stack.Free;
  end;
end;

function TInversePolishCalculator.__isValidExpression(str: string): boolean;
var
  stack: TStackOfChar;
  c: char;
begin
  stack := TStackOfChar.Create;
  try
    for c in str do
    begin
      if (c = '(') or (c = '[') then
        stack.Push(c);

      if c = ')' then
      begin
        if (stack.Count <> 0) and (stack.Peek = '(') then
          stack.Pop
        else
          Exit(False);
      end;
    end;

    Result := stack.Count = 0;
  finally
    stack.Free;
  end;
end;

function TInversePolishCalculator.__optPriority(opt: string): integer;
var
  ret: integer;
begin
  if (opt = '+') or (opt = '-') then
    ret := 0
  else if (opt = '*') or (opt = '/') then
    ret := 1
  else if (opt = 'sin(') or (opt = 'cos(') or (opt = 'tan(') or (opt = 'sqr(')
    or (opt = 'sqrt(') then
    ret := 2
  else if (opt = '(') or (opt = ')') then
    ret := 3
  else
    ret := -1;

  Result := ret;
end;

procedure TInversePolishCalculator.__trimExpression(str: string);
var
  c: char;
  charArr: TArray<char>;
  tmp: string;
  i: integer;
begin
  if not __isValidExpression(str) then
  begin
    writeLn('Expression is Illegal.');
    Exit;
  end;

  // 转换字符串为字符数组，主要因为FreePascal的Debug元法调试0基字符串
  charArr := str.ToCharArray;

  tmp := '';
  i := 0;
  while i < Length(charArr) do
  begin
    c := charArr[i];

    // 判断当前字符是为数字，如果是处理数字字符情况，否则处理非数字字符情况
    if CharInSet(c, NUMBER_SET) then
    begin
      tmp := tmp + c;

      // 判断 i+1 后数组是否越界
      if i + 1 < Length(charArr) then
      begin

        // 预下一字符，判断是否为数字， 如果是 i+1，继续循环， 否则tmp加入列表
        if CharInSet(charArr[i + 1], NUMBER_SET) then
        begin
          i := i + 1;
          Continue;
        end
        else
          __infixExpressionList.Add(Trim(tmp));

      end
      else
        __infixExpressionList.Add(Trim(tmp));

    end
    else
    begin
      tmp := tmp + c;

      // 如果当前字符为 '(' 时, 直接加入列表, 继续循环
      // 如果当前字符为操作符时，直接加入列表, 继续循环
      // 否则处理为字母时情况
      if c = '(' then
      begin
        __infixExpressionList.Add(c);

        i := i + 1;
        tmp := '';
        Continue;
      end
      else if CharInSet(c, OPERATOR_SET) then
      begin
        __infixExpressionList.Add(c);

        i := i + 1;
        tmp := '';
        Continue;
      end
      else
      begin
        // 如果 i+1 后数组不越界
        if (i + 1 < Length(charArr)) then
        begin

          // 如果当前字符为字母时， 预读下一个字符
          if (CharInSet(c, LETTER_SET)) then
          begin
            // 如果下一个字符是字母时，继续循环， 否则 tmp 加入列表
            if CharInSet(charArr[i + 1], LETTER_SET) then
            begin
              i := i + 1;
              Continue;
            end
            else
              __infixExpressionList.Add(Trim(tmp));
          end;
        end
        else
          __infixExpressionList.Add(Trim(tmp));
      end;
    end;

    i := i + 1;
    tmp := '';
  end;
end;

end.

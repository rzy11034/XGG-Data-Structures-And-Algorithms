unit X_DSA.InversePolishCalculator;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  gvector,
  gstack;

type
  WString = UnicodeString;
  WChar = widechar;

  TStackOfString = specialize TStack<WString>;
  TStackOfChar = specialize TStack<WChar>;
  TVectorOfString = specialize TVector<WString>;

  TInversePolishCalculator = class
  const
    NUMBER_SET = ['0' .. '9', '.'];
    OPERATOR_SET = ['+', '-', '*', '/'];
    BRACKET_SET = ['(', ')'];
    LETTER_SET = ['a' .. 'z'];

  private
    __optStack: TStackOfString; // 操作符栈
    __numStack: TStackOfString; // 数字栈


  public
    __infixExpressionList: TVectorOfString; // 中缀表达式元素列表
    __suffixExpressionList: TVectorOfString; // 后缀表达式元素列表

    /// <summary> 是否有效表达式 </summary>
    function __isValidExpression(str: string): boolean;
    /// <summary> 转换字符串为表达式列表 </summary>
    procedure __trimExpression(str: string);
    /// <summary> 返回操作符优先级 </summary>
    function __optPriority(opt: WString): integer;
    /// <summary> 转换中缀表达式元素列表为后缀表达式元素列表 </summary>
    procedure __infixToSuffix;
    /// <summary> 计算方法 </summary>
    function __calc(a, b: double; opt: string): double;

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
  s := 'sin(1+1)*cos(2*3)';

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

constructor TInversePolishCalculator.Create;
begin
  __numStack := TStackOfString.Create;
  __optStack := TStackOfString.Create;
  __infixExpressionList := TVectorOfString.Create;
  __suffixExpressionList := TVectorOfString.Create;
end;

function TInversePolishCalculator.Calculator(str: string): double;
begin
  Result := 0.0;
end;

destructor TInversePolishCalculator.Destroy;
begin
  __numStack.Free;
  __optStack.Free;
  __infixExpressionList.Free;
  __suffixExpressionList.Free;
  inherited Destroy;
end;

function TInversePolishCalculator.__calc(a, b: double): double;
begin

end;

procedure TInversePolishCalculator.__infixToSuffix;
var
  stack: TStackOfString;
  opt: WString;
  i: integer;
  d: double;
begin
  stack := TStackOfString.Create;
  try
    stack.Push('#');

    for i := 0 to __infixExpressionList.Size - 1 do
    begin
      // 判断是否为数字字符串，如果是直接加入后缀表达式列表，否则执行入栈操作
      if TryStrToFloat(__infixExpressionList[i], d) then
      begin
        __suffixExpressionList.PushBack(__infixExpressionList[i]);
      end
      else
      begin
        opt := __infixExpressionList[i];

        // 如果opt为'('，则直接压栈
        // 如果opt为 ')', 则出栈至'(', 加入列表,
        // 否则处理优先级情况
        if opt = '(' then
          stack.Push(opt)
        else if opt = ')' then
        begin
          while stack.Top <> '(' do
          begin
            __suffixExpressionList.PushBack(stack.Top);
            stack.Pop;
          end;

          stack.Pop;
        end
        else
        begin
          if stack.Top = '(' then
            stack.Push(opt)
          else
          begin
            // 判断和opt栈顶操作符优先级
            // 如果大于等于，压栈当前opt
            // 否则弹栈至栈顶小于等于opt，opt压栈
            if __optPriority(opt) >= __optPriority(stack.Top) then
              stack.Push(opt)
            else
            begin
              while __optPriority(stack.Top) >= __optPriority(opt) do
              begin
                __suffixExpressionList.PushBack(stack.Top);
                stack.Pop;
              end;

              stack.Push(opt);
            end;
          end;
        end;
      end;
    end;

    while stack.Top <> '#' do
    begin
      __suffixExpressionList.PushBack(stack.Top);
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
      if (c = '(') then
        stack.Push(c);

      if c = ')' then
      begin
        if (not stack.IsEmpty) and (stack.Top = '(') then
        begin
          stack.Pop;
        end
        else
          Exit(False);
      end;
    end;

    Result := stack.IsEmpty;
  finally
    stack.Free;
  end;
end;

function TInversePolishCalculator.__optPriority(opt: WString): integer;
var
  ret: integer;
begin
  if opt = '#' then
    ret := 0
  else if (opt = '+') or (opt = '-') then
    ret := 1
  else if (opt = '*') or (opt = '/') then
    ret := 2
  else if (opt = 'sin') or (opt = 'cos') or (opt = 'tan') or (opt = 'sqr')
    or (opt = 'sqrt') then
    ret := 3
  else
    ret := -1;

  Result := ret;
end;

procedure TInversePolishCalculator.__trimExpression(str: string);
var
  c: WChar;
  charArr: array of WChar;
  tmp: WString;
  i: integer;
begin
  if not __isValidExpression(str) then
  begin
    WriteLn('Expression is Illegal.');
    Exit;
  end;

  // 转换字符串为字符数组，主要因为FreePascal的Debug元法调试0基字符串
  tmp := WString(str);
  SetLength(charArr, Length(tmp));
  i := 0;
  for c in tmp do
  begin
    charArr[i] := c;
    i += 1;
  end;

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
          __infixExpressionList.PushBack(Trim(tmp));

      end
      else
        __infixExpressionList.PushBack(Trim(tmp));

    end
    else
    begin
      tmp := tmp + c;

      // 如果当前字符为 '(' 时, 直接加入列表, 继续循环
      // 如果当前字符为操作符时，直接加入列表, 继续循环
      // 否则处理为字母时情况
      if (c = '(') or (c = ')') then
      begin
        __infixExpressionList.PushBack(c);

        i += 1;
        tmp := '';
        Continue;
      end
      else if CharInSet(c, OPERATOR_SET) then
      begin
        __infixExpressionList.PushBack(c);

        i += 1;
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
              i += 1;
              Continue;
            end
            else
              __infixExpressionList.PushBack(Trim(tmp));
          end;
        end
        else
          __infixExpressionList.PushBack(Trim(tmp));
      end;
    end;

    i += 1;
    tmp := '';
  end;
end;

end.

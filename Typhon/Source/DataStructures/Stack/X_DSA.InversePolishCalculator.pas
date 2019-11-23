unit X_DSA.InversePolishCalculator;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  gvector,
  gstack,
  Math;

type
  WString = UnicodeString;
  WChar = UnicodeChar;

  TStackOfString = specialize TStack<WString>;
  TStackOfDouble = specialize TStack<double>;
  TStackOfChar = specialize TStack<WChar>;
  TVectorOfString = specialize TVector<WString>;

  TInversePolishCalculator = class
  const
    NUMBER_SET = ['0' .. '9', '.'];
    OPERATOR_SET = ['+', '-', '*', '/'];
    BRACKET_SET = ['(', ')'];
    LETTER_SET = ['a' .. 'z'];

  private
    __infixExpressionList: TVectorOfString; // 中缀表达式元素列表
    __suffixExpressionList: TVectorOfString; // 后缀表达式元素列表

    /// <summary> 是否有效表达式 </summary>
    function __isValidExpression(str: string): boolean;
    /// <summary> 转换字符串为表达式列表 </summary>
    procedure __trimExpression(str: string);
    /// <summary> 返回操作符优先级 </summary>
    function __optPriority(opt: WString): integer;
    /// <summary> 计算方法 </summary>
    function __calc: double;
    /// <summary> 转换中缀表达式元素列表为后缀表达式元素列表 </summary>
    procedure __infixToSuffix;

  public
    constructor Create;
    destructor Destroy; override;

    /// <summary> 计算 </summary>
    function Calculator(str: string): double;
    /// <summary> 输出中缀表达式 </summary>
    function InfixToString: string;
    /// <summary> 输出后缀表达式 </summary>
    function SuffixToString: string;

  end;

procedure Main;

implementation

procedure Main;
var
  ipc: TInversePolishCalculator;
  s: string;
begin
  s := '3*((1+2)*3)';

  ipc := TInversePolishCalculator.Create;

  WriteLn(ipc.Calculator(s).ToString);
  WriteLn(ipc.InfixToString);
  WriteLn(ipc.SuffixToString);

  ipc.Free;
end;

{ TInversePolishCalculator }

constructor TInversePolishCalculator.Create;
begin
  __infixExpressionList := TVectorOfString.Create;
  __suffixExpressionList := TVectorOfString.Create;
end;

function TInversePolishCalculator.Calculator(str: string): double;
begin
  if not __isValidExpression(Trim(str)) then
    raise Exception.Create('It isn''t Valid Expression.')
  else
    __trimExpression(Trim(str));

  __infixToSuffix;

  Result := __calc;
end;

destructor TInversePolishCalculator.Destroy;
begin
  __infixExpressionList.Free;
  __suffixExpressionList.Free;
  inherited Destroy;
end;

function TInversePolishCalculator.InfixToString: string;
var
  sb: TAnsiStringBuilder;
  i: integer;
begin
  sb := TAnsiStringBuilder.Create;
  try
    for i := 0 to __infixExpressionList.Size - 1 do
      sb.Append(__infixExpressionList[i]).Append(' ');

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function TInversePolishCalculator.SuffixToString: string;
var
  sb: TAnsiStringBuilder;
  i: integer;
begin
  sb := TAnsiStringBuilder.Create;
  try
    for i := 0 to __suffixExpressionList.Size - 1 do
      sb.Append(__suffixExpressionList[i]).Append(' ');

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function TInversePolishCalculator.__calc: double;
var
  OptArr: array of WString;
  optList: TStringList;
  opt: WString;
  stack: TStackOfDouble;
  a, b, ret, tmp: double;
  i: integer;
begin
  // 利用 TStringList。先把备选的字符串挨个Add 进去，
  // 然后调用其 IndexOf 方法。该方法返回一个整数，
  // 表示待找字符串出现在列表中的位置
  OptArr := ['+', '-', '*', '/', 'sin', 'cos', 'tan', 'sqr', 'sqrt'];
  optList := TStringList.Create;
  stack := TStackOfDouble.Create;
  try
    for i := 0 to Length(OptArr) - 1 do
      optList.Add(OptArr[i]);

    for i := 0 to __suffixExpressionList.Size - 1 do
    begin
      if TryStrToFloat(__suffixExpressionList[i], tmp) then
        stack.Push(tmp)
      else
      begin
        opt := __suffixExpressionList[i];

        // 如果 optList.IndexOf(__optStack.Top) <= 3，操作符为二元操作符
        // 否则为一元操作符
        if optList.IndexOf(opt) <= 3 then
        begin
          b := stack.Top;  stack.Pop;
          a := stack.Top;  stack.Pop;

          case optList.IndexOf(opt) of
            0:
            begin
              ret := a + b;
              stack.Push(ret);
            end;

            1:
            begin
              ret := a - b;
              stack.Push(ret);
            end;

            2:
            begin
              ret := a * b;
              stack.Push(ret);
            end;

            3:
            begin
              try
                ret := a / b;
                stack.Push(ret);
              except
                on E: EZeroDivide do
                  Writeln(E.ClassName, ': ', E.Message);
              end;
            end;
          end;
        end
        else
        begin
          a := stack.Top;  stack.Pop;

          case optList.IndexOf(opt) of
            4:
            begin
              ret := sin(a);
              stack.Push(ret);
            end;

            5:
            begin
              ret := cos(a);
              stack.Push(ret);
            end;

            6:
            begin
              ret := Tan(a);
              stack.Push(ret);
            end;

            7:
            begin
              ret := sqr(a);
              stack.Push(ret);
            end;

            8:
            begin
              ret := sqrt(a);
              stack.Push(ret);
            end;
          end;
        end;
      end;
    end;

    Result := stack.Top;
  finally
    stack.Free;
    optList.Free;
  end;
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

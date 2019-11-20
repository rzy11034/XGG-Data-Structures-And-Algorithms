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
    BRACKET_SET = ['(', ')', '[', ']'];
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

procedure TInversePolishCalculator.__infixToSuffix;
var
  stack: TStackOfString;
  opt: WString;
  i: integer;
begin
  stack := TStackOfString.Create;
  try
    for i := 0 to __infixExpressionList.Size - 1 do
    begin
      // 如果是数字字符串
      if CharInSet(__infixExpressionList.Items[i][1], NUMBER_SET) then
      begin
        __suffixExpressionList.PushBack(__infixExpressionList[i]);
      end
      else // 操作符字符串
      begin
        opt := __infixExpressionList[i];

        if stack.IsEmpty then
          stack.Push(opt)
        else
        begin
          if __optPriority(stack.Top) < __optPriority(opt) then
          begin
            stack.Push(opt);
          end
          else if __optPriority(stack.Top) > __optPriority(opt) then
          begin
            if (opt = ')') then // 如果opt优先级低，并且 opt = ')' 出栈
            begin
              while not stack.IsEmpty do
              begin
                __suffixExpressionList.PushBack(stack.Top);
                stack.Pop;

                if (stack.Top = '(') then
                  Break;
              end;


              stack.Pop;
            end
            else if (opt = ']') then
            begin
              repeat
                __suffixExpressionList.PushBack(stack.Top);
                stack.Pop;
              until stack.Top = '[';

              stack.Pop;
            end
            else
            begin
              __suffixExpressionList.PushBack(stack.Top);
              stack.Pop;
            end;
          end
          else // 优先级相等
          begin
            if opt = ')' then
            begin
              if (stack.Top = '(') then
                stack.Pop
              else
                stack.Push(opt);
            end
            else if opt = ']' then
            begin
              if (stack.Top = '[') then
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

    while not stack.IsEmpty do
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
      if (c = '(') or (c = '[') then
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
  if (opt = ')') or (opt = ']') then
    ret := -1
  else if (opt = '+') or (opt = '-') then
    ret := 0
  else if (opt = '*') or (opt = '/') then
    ret := 1
  else if (opt = '[') or (opt = ']') then
    ret := 3
  else if (opt = '(') or (opt = ')') then
    ret := 4
  else if (opt = 'sin(') or (opt = 'cos(') or (opt = 'tan(') or (opt = 'sqr(')
    or (opt = 'sqrt(') then
    ret := 5
  else
    ret := -2;

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
  tmp := str;
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

    if CharInSet(c, NUMBER_SET) then // 数字字符
    begin
      tmp := tmp + c;

      // i+1 是否越界
      if i + 1 < Length(charArr) then
      begin

        // 下一字符是否数字
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
    else // 非数字字符
    begin
      tmp := tmp + c;

      // i+1 是否越界
      if (i + 1 < Length(charArr)) then
      begin

        // 当前字符为字母的情况
        if (CharInSet(c, LETTER_SET)) then
        begin
          // 下一个字符是字母或者括号时
          if CharInSet(charArr[i + 1], BRACKET_SET) or CharInSet(charArr[i + 1], LETTER_SET) then
          begin
            i := i + 1;
            Continue;
          end
          else
            __infixExpressionList.PushBack(Trim(tmp));
        end
        else
        begin
          // 当前字符是否括号或者操作符
          if CharInSet(c, OPERATOR_SET) or CharInSet(c, BRACKET_SET) then
            __infixExpressionList.PushBack(Trim(tmp));
        end;
      end
      else
        __infixExpressionList.PushBack(Trim(tmp));
    end;

    i := i + 1;
    tmp := '';
  end;
end;

end.
